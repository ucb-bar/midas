
package midas
package models

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import junctions._
import midas.widgets._

import Console.{UNDERLINED, RESET}
import chisel3.experimental.dontTouch
import midas.widgets.{SatUpDownCounter}

case class PCRAMModelConfig(
//    maxBanks: Int,
//    maxRanks: Int,
//    maxActBanks: Int,
//    maxWriteBanks: Int,
    pcramKey: PCRAMOrganizationParams,
    schedulerWindowSize: Int,
    transactionQueueDepth: Int,
    backendKey: PCRAMBackendKey = PCRAMBackendKey(16, 16, PCRAMModelEnums.maxPCRAMTimingBits),
    baseParams: BaseParams)
  extends PCRAMBaseConfig(baseParams) {

  def elaborate()(implicit p: Parameters): PCRAMModel= Module(new PCRAMModel(this))
}

class PCRAMModelMMRegIO(val cfg: PCRAMModelConfig) extends BasePCRAMMMRegIO(cfg) {
  val schedulerWindowSize = Input(UInt(log2Ceil(cfg.schedulerWindowSize).W))
  val transactionQueueDepth = Input(UInt(log2Ceil(cfg.transactionQueueDepth).W))

  val registers = pcramBaseRegisters ++ Seq(
    (schedulerWindowSize -> RuntimeSetting(
        default =  cfg.schedulerWindowSize,
        query   = "Reference queue depth",
        min     = 1,
        max     = Some(cfg.schedulerWindowSize))),
    transactionQueueDepth -> RuntimeSetting(
        default = cfg.transactionQueueDepth,
        query   = "Transaction queue depth",
        min     = 1,
        max     = Some(cfg.transactionQueueDepth)))

  def requestSettings() {
    Console.println(s"Configuring a PCRAM Bank-Conflict Model")
    setBasePCRAMSettings()
  }
}

class PCRAMModelIO(val cfg: PCRAMModelConfig)(implicit p: Parameters) extends TimingModelIO()(p) {
  val mmReg = new PCRAMModelMMRegIO(cfg)
  //override def clonetype = new PCRAMModelIO(cfg)(p).asInstanceOf[this.type]
}

class PCRAMModel(cfg: PCRAMModelConfig)(implicit p: Parameters) extends TimingModel(cfg)(p)
    with HasPCRAMModelConstants {

  val longName = "PCRAM Bank-Conflict Model"
  def printTimingModelGenerationConfig {}
  /**************************** CHISEL BEGINS *********************************/
  import PCRAMModelEnums._

  lazy val io = IO(new PCRAMModelIO(cfg))
  val timings = io.mmReg.pcramTimings

  //** Define all queues **//
  // 1st-stage receiving queues for read and write
  val xactionQueuesRead = Module(new Queue(new PCRAMModelEntry(cfg), cfg.transactionQueueDepth, pipe=true))
  val xactionQueuesWrite = Module(new Queue(new PCRAMModelEntry(cfg), cfg.transactionQueueDepth, pipe=true))
  // 2nd-stage buffer for scheduling with RankStateTrackers
  val newReferenceRead = Wire(Decoupled(new PCRAMModelEntry(cfg)))
  val newReferenceWrite = Wire(Decoupled(new PCRAMModelEntry(cfg)))
  val refBufferRead = CollapsingBuffer(
    enq               = newReferenceRead,
    depth             = cfg.schedulerWindowSize,
    programmableDepth = Some(io.mmReg.schedulerWindowSize))
  val refBufferWrite = CollapsingBuffer(
    enq               = newReferenceWrite,
    depth             = cfg.schedulerWindowSize,
    programmableDepth = Some(io.mmReg.schedulerWindowSize))
  val refReadUpdates = refBufferRead.io.updates       // updates = Input
  val refReadList = refBufferRead.io.entries          // entries = Output
  val refWriteUpdates = refBufferWrite.io.updates       // updates = Input
  val refWriteList = refBufferWrite.io.entries          // entries = Output
  // 3rd-stage  command arbiter
  //    : only issuable (canREAD or canWRITE) commands are applied to Arbiter from refBuffer
  val commandArbiterRead =  Module(new Arbiter(refReadList.head.bits.cloneType, refReadList.size))
  val commandArbiterWrite =  Module(new Arbiter(refWriteList.head.bits.cloneType, refWriteList.size))
  // 4th-stage backend buffer to store requests during tREAD/tWRITE, and send them to EgressUnit
  //val issuedCommand = Wire(Decoupled(new PCRAMModelEntry(cfg)))
  val backend = Module(new PCRAMBackend(cfg.backendKey))

  // ** State-machines and counters to control flow **//
  // Rank/Bank State 
  val rankStateTrackers = Seq.fill(cfg.pcramKey.maxRanks)(Module(new PCRAMRankStateTracker(cfg.pcramKey)))
  // Legality checker
  def checkRankBankLegality(getField: PCRAMCommandLegalBools => Bool)(masEntry: PCRAMModelEntry): Bool = {
    val bankFields = rankStateTrackers map { rank => VecInit(rank.io.rank.banks map getField).asUInt }
    val bankLegal = (Mux1H(masEntry.rankAddrOH, bankFields) & masEntry.bankAddrOH).orR
    val rankFields = VecInit(rankStateTrackers map { rank => getField(rank.io.rank) }).asUInt
    val rankLegal = (masEntry.rankAddrOH & rankFields).orR
    rankLegal && bankLegal }
  val canLegallyREAD = checkRankBankLegality( _.canREAD ) _
  val canLegallyWRITE = checkRankBankLegality(_.canWRITE ) _
  // Counter for pending requensts counter to prevent write-starvation 
  val selectedMode = RegInit(readMode)
  val numValidReadReqInSch = PopCount(commandArbiterRead.io.in map {_.valid})
  val numReadReqInSch = PopCount(refReadList map {_.valid})
  val numValidWriteReqInSch = PopCount(commandArbiterWrite.io.in map {_.valid})
  val numWriteReqInSch = PopCount(refWriteList map {_.valid})
  dontTouch(numValidReadReqInSch)
  dontTouch(numReadReqInSch)
  dontTouch(numValidWriteReqInSch)
  dontTouch(numWriteReqInSch)

  //val writeMode = Mux(numReadReqInSch === 0.U || numWriteReqInSch >= (refWriteList.size*2/3).U
  /*
  val numValidReadReqInSch = RegInit(0.U(log2Ceil(refReadList.size).W))
  val numReadReqInSch = RegInit(0.U(log2Ceil(refReadList.size).W))
  numValidReadReqInSch := PopCount(commandArbiterRead.io.in map {_.valid})
  numReadReqInSch := PopCount(refReadList map {_.valid})
  val numValidWriteReqInSch = RegInit(0.U(log2Ceil(refWriteList.size).W))
  val numWriteReqInSch = RegInit(0.U(log2Ceil(refWriteList.size).W))
  numValidWriteReqInSch := PopCount(commandArbiterWrite.io.in map {_.valid})
  numWriteReqInSch := PopCount(refWriteList map {_.valid})
  */
  when (selectedMode === readMode) {
    when (numWriteReqInSch >= (refWriteList.size*2/3).U || 
           (numReadReqInSch === 0.U && numWriteReqInSch =/= 0.U)){
      selectedMode := writeMode
    }
  }.elsewhen(selectedMode === writeMode) {
    when (numWriteReqInSch <= (refWriteList.size/3).U && numReadReqInSch =/= 0.U) {
      selectedMode := readMode }}
  /*
  when (!writeMode) {
    when (numWriteReqInSch >= (refWriteList.size*2/3).U || numReadReqInSch === 0.U){
      writeMode := true.B }
  }.elsewhen(writeMode) {
    when (numWriteReqInSch <= (refWriteList.size/3).U && numReadReqInSch =/= 0.U) {
      writeMode := false.B} }
  */
  // Counter for throttling : total number of ACT banks & total number of Write Banks
  // Counting # of ACT and # of WRITE bank for Power Throttling 
  val numActBanks = RegInit(0.U(log2Ceil(cfg.pcramKey.maxBanks*cfg.pcramKey.maxRanks).W))
  val actBanksPerRank = VecInit(rankStateTrackers map { perRank =>
        PopCount(perRank.io.rank.banks map {_.state =/= bank_idle})
    })
  numActBanks := actBanksPerRank.reduceLeft( _ + _ )

  val numReadBanks = RegInit(0.U(log2Ceil(cfg.pcramKey.maxBanks*cfg.pcramKey.maxRanks).W))
  val readBanksPerRank = VecInit(rankStateTrackers map { perRank =>
        PopCount(perRank.io.rank.banks map {_.state === bank_read})
    })
  numReadBanks := readBanksPerRank.reduceLeft( _ + _ )
  dontTouch(numReadBanks)

  val numWriteBanks = RegInit(0.U(log2Ceil(cfg.pcramKey.maxBanks*cfg.pcramKey.maxRanks).W))
  val writeBanksPerRank = VecInit(rankStateTrackers map { perRank =>
        val perRankWriteBanks = RegInit(0.U(log2Ceil(cfg.pcramKey.maxBanks).W))
        perRankWriteBanks := PopCount(perRank.io.rank.banks map {_.state === bank_write})
        perRankWriteBanks
    })
  numWriteBanks := writeBanksPerRank.reduceLeft( _ + _ )
  dontTouch(numWriteBanks)

  val warningPowerThrottling = RegInit(0.U(1.W))
  val warningWriteThrottling = RegInit(0.U(1.W))
  warningPowerThrottling :=  numActBanks >= cfg.pcramKey.maxActBanks.U
  warningWriteThrottling :=  numWriteBanks >= cfg.pcramKey.maxWriteBanks.U
  dontTouch(warningPowerThrottling)
  dontTouch(warningWriteThrottling)


	//** 1st-stage receiving xaction queues both for read and write **//
  xactionQueuesRead.io.enq.valid := nastiReq.ar.valid
  xactionQueuesRead.io.enq.bits.addr := nastiReq.ar.bits.addr
  xactionQueuesRead.io.enq.bits.xaction := TransactionMetaData(nastiReq.ar.bits)
  xactionQueuesRead.io.enq.bits.addr := nastiReq.ar.bits.addr
  xactionQueuesRead.io.enq.bits.latency := tCycle + timings.tTHRESHOLDREAD
  xactionQueuesRead.io.enq.bits.decode(nastiReq.ar.bits, io.mmReg)
  nastiReq.ar.ready := xactionQueuesRead.io.enq.ready

  xactionQueuesWrite.io.enq.valid := nastiReq.aw.valid
  xactionQueuesWrite.io.enq.bits.xaction := TransactionMetaData(nastiReq.aw.bits)
  xactionQueuesWrite.io.enq.bits.addr := nastiReq.aw.bits.addr
  xactionQueuesWrite.io.enq.bits.latency := tCycle + timings.tTHRESHOLDWRITE
  xactionQueuesWrite.io.enq.bits.decode(nastiReq.aw.bits, io.mmReg)
  nastiReq.aw.ready := xactionQueuesWrite.io.enq.ready
  nastiReq.w.ready := true.B

  newReferenceRead.valid := xactionQueuesRead.io.deq.valid
  newReferenceRead.bits := xactionQueuesRead.io.deq.bits
  xactionQueuesRead.io.deq.ready  := newReferenceRead.ready
  newReferenceWrite.valid := xactionQueuesWrite.io.deq.valid
  newReferenceWrite.bits := xactionQueuesWrite.io.deq.bits
  xactionQueuesWrite.io.deq.ready  := newReferenceWrite.ready

  commandArbiterRead.io.in <> refReadList.map({ entry =>
      val candidate = V2D(entry)
      val couldREAD = canLegallyREAD(entry.bits) && (numActBanks < cfg.pcramKey.maxActBanks.U) && backend.io.newRead.ready 
      candidate.valid := entry.valid && couldREAD && ( selectedMode === readMode)
      candidate
    })

  commandArbiterWrite.io.in <> refWriteList.map({ entry =>
      val candidate = V2D(entry)
      val couldWRITE = canLegallyWRITE(entry.bits) && (numWriteBanks < cfg.pcramKey.maxWriteBanks.U) && backend.io.newWrite.ready 
      candidate.valid := entry.valid && couldWRITE && (selectedMode === writeMode)
      candidate
    })

  // Take the readies from the arbiter, and kill the selected entry
  val entriesReadStillReady = refReadUpdates.zip(commandArbiterRead.io.in) map { case (ref, sel) =>
    when (sel.fire()) { ref.valid := false.B } }
  val entriesWriteStillReady = refWriteUpdates.zip(commandArbiterWrite.io.in) map { case (ref, sel) =>
    when (sel.fire()) { ref.valid := false.B } }

  backend.io.tCycle := tCycle
  backend.io.readLatency := timings.tREAD + io.mmReg.backendLatency
  backend.io.newRead.valid := (numActBanks <= cfg.pcramKey.maxActBanks.U) && commandArbiterRead.io.out.valid 
  backend.io.newRead.bits := ReadResponseMetaData(commandArbiterRead.io.out.bits.xaction) 
  commandArbiterRead.io.out.ready := (numActBanks < cfg.pcramKey.maxActBanks.U) && backend.io.newRead.ready

  backend.io.writeLatency := timings.tWRITE + io.mmReg.backendLatency
  backend.io.newWrite.valid := (numActBanks < cfg.pcramKey.maxWriteBanks.U) && commandArbiterWrite.io.out.valid 
  backend.io.newWrite.bits := WriteResponseMetaData(commandArbiterWrite.io.out.bits.xaction)
  commandArbiterWrite.io.out.ready := (numActBanks < cfg.pcramKey.maxWriteBanks.U) && backend.io.newWrite.ready

  val cmdBank = WireInit(UInt(cfg.pcramKey.bankBits.W), init = commandArbiterRead.io.out.bits.bankAddr)
  val cmdBankOH = UIntToOH(cmdBank)
  val cmdRank = WireInit(UInt(cfg.pcramKey.rankBits.W), init = commandArbiterRead.io.out.bits.rankAddr)
  val cmdRankOH = UIntToOH(cmdRank)
  val selectedCmd = WireInit(cmd_nop)
  when(numActBanks <= cfg.pcramKey.maxActBanks.U && commandArbiterWrite.io.out.valid) {
    selectedCmd := cmd_write
    cmdBank := commandArbiterWrite.io.out.bits.bankAddr
    cmdRank := commandArbiterWrite.io.out.bits.rankAddr
  }.elsewhen (numActBanks <= cfg.pcramKey.maxActBanks.U && commandArbiterRead.io.out.valid) {
    selectedCmd := cmd_read
    cmdBank := commandArbiterRead.io.out.bits.bankAddr
    cmdRank := commandArbiterRead.io.out.bits.rankAddr
  }.otherwise {
    selectedCmd :=cmd_nop
  }


  // Trackers controller-level structural hazards
  val cmdBusBusy = Module(new DownCounter((maxPCRAMTimingBits)))
  cmdBusBusy.io.decr := true.B

  // Dump the command stream
  val cmdMonitor = Module(new PCRAMCommandBusMonitor(cfg.pcramKey))
  // Biancolin: See comment above
  cmdMonitor.io.cmd := DontCare
  cmdMonitor.io.rank := DontCare
  cmdMonitor.io.bank := DontCare
    
  rankStateTrackers.zip(UIntToOH(cmdRank).toBools) foreach { case (state, cmdUsesThisRank)  =>
    state.io.selectedCmd := selectedCmd
    state.io.cmdBankOH := cmdBankOH
    state.io.cmdUsesThisRank := cmdUsesThisRank
    state.io.timings := timings
    state.io.tCycle := tCycle
  }

  // comment: for above when statements, pull out the same functions outside when-{}
	// because compiler may generates multiple same blocks and mux them.  
  wResp <> backend.io.completedWrite
  rResp <> backend.io.completedRead

//val lowestIdx = Vec(allReadQueues map { _.io.deq }).indexWhere(_.valid)
  // TODO: sensible mapping to PCRAM bus width

  cmdBusBusy.io.set.bits := timings.tCCD_PCRAM - 1.U
  cmdBusBusy.io.set.valid := (selectedCmd =/= cmd_nop)

  val powerStats = (rankStateTrackers).zip(UIntToOH(cmdRank).toBools) map {
    case (rankState, cmdUsesThisRank) =>
      val powerMonitor = Module(new PCRAMRankPowerMonitor(cfg.pcramKey))
      powerMonitor.io.selectedCmd := selectedCmd
      powerMonitor.io.cmdUsesThisRank := cmdUsesThisRank
      powerMonitor.io.rankState := rankState.io.rank
      powerMonitor.io.stats
    }
  io.mmReg.rankPower := VecInit(powerStats)


  //**** Checker: Counters to check transactions ***///
  /*
  val numCompletedReads  = RegInit(0.U(64.W))
  when ( backend.io.completedRead.fire()) {
    numCompletedReads := numCompletedReads + 1.U }
  val numCompletedWrites = RegInit(0.U(64.W))
  when ( backend.io.completedWrite.fire()) {
    numCompletedWrites := numCompletedWrites + 1.U }
  val numIssuedCommandsToSM = RegInit(0.U(64.W))
  when ( selectedCmd =/= cmd_nop) {
    numIssuedCommandsToSM := numIssuedCommandsToSM + 1.U }

  val schWriteInputValidTracker = VecInit(commandArbiterWrite.io.in map{ _.valid })
  val schWriteInputReadyTracker = VecInit(commandArbiterWrite.io.in map{ _.ready })
  val validityCheck = RegInit(0.U(log2Ceil(refWriteList.size).W))
  validityCheck := (schWriteInputValidTracker.zip(schWriteInputReadyTracker) map { case (aaa, bbb) => 
    val committed = RegInit(0.U(1.W))
    committed := Mux( bbb && aaa, 1.U, 0.U)
    committed }) .reduceLeft( _ + _ )
  assert ( validityCheck < 2.U, " Applied two inputs to the arbiter!!")
  //when ( commandArbiterWrite.io.out.valid && commandArbiterWrite.io.out.ready ) {
  //  numIssuedCommands := numIssuedCommands + 1.U }
  //when ( commandArbiterWrite.io.out.fire() ) {
  //  numIssuedCommands := numIssuedCommands + 1.U }
  //when ( newReferenceWrite.fire() ) {
  //  numIssuedCommands := numIssuedCommands + 1.U }
  //when ( xactionQueuesWrite.io.deq.fire() ) {
  //  numIssuedCommands := numIssuedCommands + 1.U }
  //when ( backend.io.newWrite.valid && backend.io.newWrite.ready) {
  //  numIssuedCommands := numIssuedCommands + 1.U }
  //when ( selectedCmd =/= cmd_nop) {
  //  numIssuedCommands := numIssuedCommands + 1.U }
  val numAppliedCommandsToModel = RegInit(0.U(64.W))
  when ( nastiReq.ar.fire() || nastiReq.aw.fire()) {
    numAppliedCommandsToModel := numAppliedCommandsToModel + 1.U }
  val numAppliedCommandsToRef = RegInit(0.U(64.W))
  when ( (xactionQueuesRead.io.deq.valid && xactionQueuesRead.io.deq.ready) || (xactionQueuesWrite.io.deq.valid && xactionQueuesWrite.io.deq.ready)) {
    numAppliedCommandsToRef := numAppliedCommandsToRef + 1.U }
  val numAppliedCommandsToBackendWrite = RegInit(0.U(64.W))
  when ( commandArbiterWrite.io.out.fire() ) {
    numAppliedCommandsToBackendWrite := numAppliedCommandsToBackendWrite + 1.U }
  dontTouch(numCompletedReads)
  dontTouch(numCompletedWrites)
  dontTouch(numIssuedCommandsToSM)
  dontTouch(numAppliedCommandsToModel)
  dontTouch(numAppliedCommandsToRef)
  dontTouch(numAppliedCommandsToBackendWrite)
  dontTouch(validityCheck)
  */


}

