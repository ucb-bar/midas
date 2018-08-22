
// tCycle is infinite??
// bit width of cycle 
package midas
package models

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import junctions._
import midas.widgets._

import Console.{UNDERLINED, RESET}

case class PCRAMModelConfig(
    maxBanks: Int,
    maxActBanks: Int,
    pcramKey: PCRAMOrganizationKey,
    transactionQueueDepth: Int,
    backendKey: PCRAMBackendKey = PCRAMBackendKey(4, 4, PCRAMModelEnums.maxPCRAMTimingBits),
    baseParams: BaseParams)
  extends PCRAMBaseConfig(baseParams) {

  def elaborate()(implicit p: Parameters): PCRAMModel= Module(new PCRAMModel(this))
}

class PCRAMModelMMRegIO(val cfg: PCRAMModelConfig) extends BasePCRAMMMRegIO(cfg) {
  val registers = pcramBaseRegisters

  def requestSettings() {
    Console.println(s"Configuring a PCRAM Bank-Conflict Model")
    setBasePCRAMSettings()
  }
}

class PCRAMModelIO(val cfg: PCRAMModelConfig)(implicit p: Parameters) extends TimingModelIO()(p) {
  val mmReg = new PCRAMModelMMRegIO(cfg)
  //override def clonetype = new PCRAMModelIO(cfg)(p).asInstanceOf[this.type]
}

/*
class PCRAMReference(cfg: PCRAMModelConfig)(implicit p: Parameters) extends Bundle {
   val reference = new PCRAMModelEntry(cfg)
   //val reference = new PCRAMBankQueueEntry(cfg)
   val latency = UInt(cfg.maxPCRAMTimingBits.W) 
   //val done = Bool() // Set high when the cycle count expires
   override def cloneType = new PCRAMReference(cfg)(p).asInstanceOf[this.type]
 }
*/

class PCRAMModel(cfg: PCRAMModelConfig)(implicit p: Parameters) extends TimingModel(cfg)(p)
    with HasPCRAMModelConstants {

  val longName = "PCRAM Bank-Conflict Model"
  def printTimingModelGenerationConfig {}
  /**************************** CHISEL BEGINS *********************************/
  import PCRAMModelEnums._

  lazy val io = IO(new PCRAMModelIO(cfg))
  val timings = io.mmReg.pcramTimings
  //val cmdRank = Reg(UInt(cfg.pcramKey.rankBits.W))


	//** 1st-stage receiving xaction queues both for read and write **//
  val xactionQueuesRead = Module(new Queue(new PCRAMModelEntry(cfg), cfg.maxReads, pipe=true))
  //xactionQueuesRead.io.enq <> nastiReq.ar
  xactionQueuesRead.io.enq.valid := nastiReq.ar.valid
  xactionQueuesRead.io.enq.bits.addr := nastiReq.ar.bits.addr
  xactionQueuesRead.io.enq.bits.xaction := TransactionMetaData(nastiReq.ar.bits)
  //xactionQueuesRead.io.enq.bits.xaction.id := nastiReq.ar.bits.id
  //xactionQueuesRead.io.enq.bits.xaction.len := nastiReq.ar.bits.len
  //xactionQueuesRead.io.enq.bits.xaction.isWrite := false.B
  xactionQueuesRead.io.enq.bits.addr := nastiReq.ar.bits.addr
  xactionQueuesRead.io.enq.bits.latency := tCycle + timings.tTHRESHOLDREAD
  xactionQueuesRead.io.enq.bits.decode(nastiReq.ar.bits, io.mmReg)
  nastiReq.ar.ready := xactionQueuesRead.io.enq.ready
  val xactionQueuesWrite = Module(new Queue(new PCRAMModelEntry(cfg), cfg.maxWrites, pipe=true))
  //xactionQueuesWrite.io.enq <> nastiReq.aw
  xactionQueuesWrite.io.enq.valid := nastiReq.aw.valid
  xactionQueuesWrite.io.enq.bits.xaction := TransactionMetaData(nastiReq.aw.bits)
  //xactionQueuesWrite.io.enq.bits.xaction.id := nastiReq.aw.bits.id
  //xactionQueuesWrite.io.enq.bits.xaction.len := nastiReq.aw.bits.len
  //xactionQueuesWrite.io.enq.bits.xaction.isWrite := true.B

  xactionQueuesWrite.io.enq.bits.addr := nastiReq.aw.bits.addr
  xactionQueuesWrite.io.enq.bits.latency := tCycle + timings.tTHRESHOLDWRITE
  //xactionQueuesWrite.io.enq.bits.urgent := false.B
  xactionQueuesWrite.io.enq.bits.decode(nastiReq.aw.bits, io.mmReg)
  nastiReq.aw.ready := xactionQueuesWrite.io.enq.ready


	//** 2nd-stage waiting queues to split the requests to the dedicated bank queues **//
  // Queue definition  
	val perBankQueuesRead = Seq.fill(cfg.maxBanks)(Module(new Queue(new PCRAMModelEntry(cfg), cfg.maxReads, pipe=true)))
	val perBankQueuesWrite = Seq.fill(cfg.maxBanks)(Module(new Queue(new PCRAMModelEntry(cfg), cfg.maxWrites, pipe=true)))
	val perBankQueuesUrgent= Seq.fill(cfg.maxBanks)(Module(new Queue(new PCRAMModelEntry(cfg), 4, pipe=true)))


  // Allocate command to the target perBankQueuesRead/Write with tCycle
  /*
  xactionQueuesRead.io.deq.ready := perBankQueuesRead.zipWithIndex map({case (bank, idx) =>
    val queueready = bank.io.enq.ready && xactionQueuesRead.io.deq.bits.bankAddr === idx.U
    queueready
  }) reduce { _ || _ }
  */
  xactionQueuesRead.io.deq.ready := Mux1H(UIntToOH(xactionQueuesRead.io.deq.bits.bankAddr), perBankQueuesRead map(_.io.enq.ready))
  perBankQueuesRead.zipWithIndex foreach { case (bank, idx)  =>  
    bank.io.enq.valid := (xactionQueuesRead.io.deq.bits.bankAddr === idx.U) && xactionQueuesRead.io.deq.valid
    bank.io.enq.bits := xactionQueuesRead.io.deq.bits
  }
  
  xactionQueuesWrite.io.deq.ready := Mux1H(UIntToOH(xactionQueuesWrite.io.deq.bits.bankAddr), perBankQueuesWrite map(_.io.enq.ready))
  perBankQueuesWrite.zipWithIndex foreach{ case (bank, idx) => 
    bank.io.enq.valid := (xactionQueuesWrite.io.deq.bits.bankAddr === idx.U) && xactionQueuesWrite.io.deq.valid
    bank.io.enq.bits := xactionQueuesWrite.io.deq.bits
  }

  // Allocate urgent command from Read/Write queues to Urgent queue

  val relocateUrgent = perBankQueuesRead.zip(perBankQueuesWrite) map({case(readqueue, writequeue ) =>
    val urgent = (tCycle >= readqueue.io.deq.bits.latency && readqueue.io.deq.valid) || (tCycle >=writequeue.io.deq.bits.latency && writequeue.io.deq.valid) 
    urgent}) reduce { _ || _}
  //val thereExistUrgent = validForUrgent map {_.valid} reduce { _ || _ }

  /*
  perBankQueuesRead.zip(perBankQueuesWrite).zip(perBankQueuesUrgent) foreach({case((readqueue,writequeue),urgentqueue) =>
    when (!readqueue.io.deq.valid && !readqueue.io.deq.valid){
      urgentqueue.io.enq.valid := false.B
    }.elsewhen (readqueue.io.deq.valid && urgentqueue.io.enq.ready && tCycle >= readqueue.io.deq.bits.latency){
      urgentqueue.io.enq.valid := readqueue.io.deq.valid
      readqueue.io.deq.ready := urgentqueue.io.enq.ready
      urgentqueue.io.enq.bits.xaction := readqueue.io.deq.bits.xaction
    }.elsewhen (!readqueue.io.deq.valid && writequeue.io.deq.valid && urgentqueue.io.enq.ready && tCycle >= writequeue.io.deq.bits.latency){
      urgentqueue.io.enq.valid := writequeue.io.deq.valid
      writequeue.io.deq.ready := urgentqueue.io.enq.ready
      urgentqueue.io.enq.bits.xaction := writequeue.io.deq.bits.xaction
    }.otherwise {
      urgentqueue.io.enq.valid := false.B
    }
  })
  */
  perBankQueuesUrgent foreach{ urgentqueue =>
    urgentqueue.io.enq.valid := false.B
  }

  /*
  perBankQueuesWrite.zip(perBankQueuesUrgent).zipWithIndex foreach({case((writequeue,urgentqueue), idx) =>
    when(writequeue.io.deq.valid && urgentqueue.io.enq.ready && tCycle >= writequeue.io.deq.bits.latency){
      urgentqueue.io.enq.valid :=true.B
      writequeue.io.deq.ready := urgentqueue.io.enq.ready
      urgentqueue.io.enq.bits.xaction := writequeue.io.deq.bits.xaction
    }
  })
  */

	//** Issue command by checking Rank/BankStateTracker, and send the request to the refBuffer **//
  val backend = Module(new PCRAMBackend(cfg.backendKey))

  // Trackers controller-level structural hazards
  val cmdBusBusy = Module(new DownCounter((maxPCRAMTimingBits)))
  cmdBusBusy.io.decr := true.B

  // Trackers for bank-level hazards and timing violations
  //val rankStateTrackers = Seq.fill(cfg.pcramKey.maxRanks)(Module(new PCRAMRankStateTracker(cfg.pcramKey)))
  val rankStateTrackers = Module(new PCRAMRankStateTracker(cfg.pcramKey))				// single-rank system
  //val currentRank = VecInit(rankStateTrackers map { _.io.rank })(currentReference.bits.rankAddr)
  //val bankMuxes = VecInit(rankStateTrackers map { tracker => tracker.io.rank.banks(currentReference.bits.bankAddr) })
  //val currentBank = WireInit(bankMuxes(currentReference.bits.rankAddr))
      

  val validForUrgent = perBankQueuesUrgent.zip(rankStateTrackers.io.rank.banks) map({case((urgentqueue, bankState)) =>
    val validForUrgent = Wire(Decoupled(new PCRAMModelEntry(cfg)))
    validForUrgent.valid := urgentqueue.io.deq.valid && bankState.canACT && rankStateTrackers.io.rank.canACT
		validForUrgent.bits  := urgentqueue.io.deq.bits
		urgentqueue.io.deq.ready := validForUrgent.ready
    validForUrgent
  })
  val thereExistsAValidUrgent = validForUrgent map {_.valid} reduce { _ || _ }
  // Actually, we don't need thereExistAValidUrgent/Read/Write, 
  // because selectorUrgent/Read/Write.io.out.valid can represent these signals!!
  // if thereExistsAValidUrgent===0, then start to check ReadQueue

    
  val validForRead = perBankQueuesRead.zip(rankStateTrackers.io.rank.banks) map({case(readqueue, bankState ) =>
    val validForRead = Wire(Decoupled(new PCRAMModelEntry(cfg)))
    validForRead.valid := readqueue.io.deq.valid && bankState.canACT && rankStateTrackers.io.rank.canACT
    validForRead.bits  := readqueue.io.deq.bits
		readqueue.io.deq.ready := validForRead.ready
    validForRead
  })
  val thereExistsAValidRead = validForRead map {_.valid} reduce { _ || _ }
  // if thereExistsAValidRead===0, then start to check WriteQueue
    
  val validForWrite = perBankQueuesWrite.zip(rankStateTrackers.io.rank.banks) map({case(writequeue, bankState) =>
    val validForWrite = Wire(Decoupled(new PCRAMModelEntry(cfg)))
    validForWrite.valid := writequeue.io.deq.valid && bankState.canACT && rankStateTrackers.io.rank.canACT
		validForWrite.bits := writequeue.io.deq.bits
		writequeue.io.deq.ready := validForWrite.ready
    validForWrite
  })
  val thereExistsAValidWrite = validForWrite map {_.valid} reduce { _ || _ }

	// RRArbiter example: val arb = Module(new RRArbiter(new PTWReq, n))
  val selectorUrgent = Module(new RRArbiter(new PCRAMModelEntry(cfg), cfg.maxBanks))
  val selectorRead = Module(new RRArbiter(new PCRAMModelEntry(cfg), cfg.maxBanks))
  val selectorWrite = Module(new RRArbiter(new PCRAMModelEntry(cfg), cfg.maxBanks))

  selectorUrgent.io.in <> validForUrgent
  selectorRead.io.in <> validForRead
  selectorWrite.io.in <> validForWrite

  // Bank/Rank address check to allocate commands to the target rank/bank
  //val cmdRankUrgent = WireInit(UInt(cfg.pcramKey.rankBits.W), init = selectorUrgent.io.out.bits.rankAddr)
  //val cmdRankRead = WireInit(UInt(cfg.pcramKey.rankBits.W), init = selectorRead.io.out.bits.rankAddr)
  //val cmdRankWrite = WireInit(UInt(cfg.pcramKey.rankBits.W), init = selectorWrite.io.out.bits.rankAddr)
  val cmdBankUrgent = WireInit(UInt(cfg.pcramKey.bankBits.W), init = selectorUrgent.io.out.bits.bankAddr)
  val cmdBankRead = WireInit(UInt(cfg.pcramKey.bankBits.W), init = selectorRead.io.out.bits.bankAddr)
  val cmdBankWrite = WireInit(UInt(cfg.pcramKey.bankBits.W), init = selectorWrite.io.out.bits.bankAddr)
  val cmdBank = WireInit(UInt(cfg.pcramKey.bankBits.W), init = selectorWrite.io.out.bits.bankAddr)
  //val cmdBank = Reg(UInt(cfg.pcramKey.bankBits.W))
  //val cmdRankOH = UIntToOH(cmdRank)
  val cmdBankOH = UIntToOH(cmdBank)
  val selectedCmd = WireInit(cmd_nop)
  val numActBanks = RegInit(0.U(log2Ceil(cfg.maxBanks).W))
  numActBanks := PopCount(rankStateTrackers.io.rank.banks map {_.state =/= bank_idle})

  // Dump the command stream
  val cmdMonitor = Module(new PCRAMCommandBusMonitor(cfg.pcramKey))
  //cmdMonitor.io.cmd := selectedCmd
  //cmdMonitor.io.rank := cmdRank
    
  /*
  backend.io.newRead.valid := thereExistsAValidRead || (thereExistsAValidUrgent && !selectorUrgent.io.out.bits.xaction.isWrite) 
  backend.io.newWrite.valid := thereExistsAValidWrite || (thereExistsAValidUrgent && selectorUrgent.io.out.bits.xaction.isWrite)
  */
  val thereExistUrgentRead = selectorUrgent.io.out.valid && !selectorUrgent.io.out.bits.xaction.isWrite
  val thereExistUrgentWrite = selectorUrgent.io.out.valid && selectorUrgent.io.out.bits.xaction.isWrite

  selectorUrgent.io.out.ready := (numActBanks <= cfg.maxActBanks.U) && selectorUrgent.io.out.valid
  selectorRead.io.out.ready := (numActBanks <= cfg.maxActBanks.U) && !thereExistUrgentRead
  selectorWrite.io.out.ready := (numActBanks <= cfg.maxActBanks.U) && !thereExistUrgentWrite
  
  backend.io.tCycle := tCycle

  backend.io.readLatency := timings.tREAD + io.mmReg.backendLatency
  backend.io.newRead.valid := (numActBanks <= cfg.maxActBanks.U) && Mux(!thereExistUrgentRead, selectorRead.io.out.valid, selectorUrgent.io.out.valid) 
  backend.io.newRead.bits := Mux(thereExistUrgentRead, ReadResponseMetaData(selectorUrgent.io.out.bits.xaction), ReadResponseMetaData(selectorRead.io.out.bits.xaction))

  backend.io.writeLatency := timings.tWRITE + io.mmReg.backendLatency
  backend.io.newWrite.valid := (numActBanks <= cfg.maxActBanks.U) && Mux(!thereExistUrgentWrite, selectorWrite.io.out.valid, selectorUrgent.io.out.valid) 
  backend.io.newWrite.bits := Mux(thereExistUrgentWrite, WriteResponseMetaData(selectorUrgent.io.out.bits.xaction), WriteResponseMetaData(selectorWrite.io.out.bits.xaction))

  when(numActBanks <= cfg.maxActBanks.U){
    when(thereExistsAValidUrgent){ 
      cmdBank := cmdBankUrgent
      when(!selectorUrgent.io.out.bits.xaction.isWrite && backend.io.newRead.ready){
        selectedCmd := cmd_read
      }.elsewhen(selectorUrgent.io.out.bits.xaction.isWrite && backend.io.newWrite.ready) {
        selectedCmd := cmd_write
      }
    }.elsewhen(thereExistsAValidRead && backend.io.newRead.ready){
      selectedCmd := cmd_read
      cmdBank := cmdBankRead
    }.elsewhen(thereExistsAValidWrite && backend.io.newWrite.ready){
      selectedCmd := cmd_write
      cmdBank := cmdBankWrite
    }
  }

  /*
  when(numActBanks <= cfg.maxActBanks.U){
    backend.io.newRead.bits.id  := selectorRead.io.out.bits.xaction.id
    //assert(false.B, "Test assertion: succeeded to enter when statement")
    when(thereExistsAValidUrgent){ 
      //assert(false.B, "Test assertion: Wrong operation due to UrgentQueue Entering")
      //rankStateTrackers.io.cmdBankOH := UIntToOH(selectorUrgent.io.out.bits.bankAddr)  // valid only for that bank-state register
      //cmdMonitor.io.bank := cmdBank(selectorUrgent.io.out.bits.bankAddr)
      //cmdRank := cmdRankUrgent
      cmdBank := cmdBankUrgent
      when(!selectorUrgent.io.out.bits.xaction.isWrite && backend.io.newRead.ready){
        //numActBanks := numActBanks + 1.U
        //assert(false.B, "Test assertion: Wrong operation due to UrgentReadQueue Entering")
        selectedCmd := cmd_read
        selectorUrgent.io.out.ready := backend.io.newRead.ready
        //backend.io.newRead.valid := !selectorUrgent.io.out.valid
        backend.io.newRead.bits  := ReadResponseMetaData(selectorUrgent.io.out.bits.xaction)
        backend.io.readLatency := timings.tREAD + io.mmReg.backendLatency
      }.elsewhen(selectorUrgent.io.out.bits.xaction.isWrite && backend.io.newWrite.ready) {
        //numActBanks := numActBanks + 1.U
        //assert(false.B, "Test assertion: Wrong operation due to UrgentWriteQueue Entering")
        selectedCmd := cmd_write
        selectorUrgent.io.out.ready := backend.io.newWrite.ready
        //backend.io.newWrite.valid := selectorUrgent.io.out.bits.xaction.isWrite
        backend.io.newWrite.bits := WriteResponseMetaData(selectorUrgent.io.out.bits.xaction)
        backend.io.writeLatency := timings.tWRITE + io.mmReg.backendLatency
      }
    }.elsewhen(thereExistsAValidRead && backend.io.newRead.ready){
      //assert(false.B, "Test assertion: succeeded to enter read-when case")
      selectedCmd := cmd_read
      //cmdRank := cmdRankRead
      cmdBank := cmdBankRead
      selectorRead.io.out.ready := backend.io.newRead.ready
      //rankStateTrackers.io.cmdBankOH := UIntToOH(selectorRead.io.out.bits.bankAddr)
      //backend.io.newRead.bits  := ReadResponseMetaData(selectorRead.io.out.bits.xaction)
      //backend.io.newRead.bits.id  := selectorRead.io.out.bits.xaction.id
      //backend.io.newRead.bits.len  := selectorRead.io.out.bits.xaction.len
      //backend.io.newRead.valid := !selectorRead.io.out.bits.xaction.isWrite
      backend.io.readLatency := timings.tREAD + io.mmReg.backendLatency
      //cmdMonitor.io.bank := cmdBank(selectorRead.io.out.bits.bankAddr)
    }.elsewhen(thereExistsAValidWrite && backend.io.newWrite.ready){
      //assert(false.B, "Test assertion: wrong enter to write operation")
      //numActBanks := numActBanks + 1.U
      selectedCmd := cmd_write
      //cmdRank :=cmdRankWrite
      cmdBank := cmdBankWrite
      selectorWrite.io.out.ready := backend.io.newWrite.ready
      //rankStateTrackers.io.cmdBankOH := UIntToOH(selectorWrite.io.out.bits.bankAddr)
      backend.io.tCycle := tCycle
      backend.io.newWrite.bits  := WriteResponseMetaData(selectorWrite.io.out.bits.xaction)
      //backend.io.newWrite.valid := selectorWrite.io.out.bits.xaction.isWrite
      backend.io.writeLatency := timings.tWRITE + io.mmReg.backendLatency
      //cmdMonitor.io.bank := cmdBank(selectorWrite.io.out.bits.bankAddr)
    }
  }
  */

  //when(thereExistsAValidRead && backend.io.newRead.ready && cmdRankRead === 0.U)
  //when(thereExistsAValidRead && backend.io.newRead.ready && cmdRankRead =/= 0.U)

  rankStateTrackers.io.selectedCmd := selectedCmd
  rankStateTrackers.io.cmdBankOH := cmdBankOH
  //rankStateTrackers.io.cmdUsesThisRank := cmdUsesThisRank
  rankStateTrackers.io.cmdUsesThisRank := true.B
  rankStateTrackers.io.timings := timings
  rankStateTrackers.io.tCycle := tCycle

  // comment: for above when statements, pull out the same functions outside when-{}
	// because compiler may generates multiple same blocks and mux them.  
  wResp <> backend.io.completedWrite
  rResp <> backend.io.completedRead

  //when(rResp.fire()) { numActBanks := numActBanks - 1.U }
  //when(wResp.fire()) { numActBanks := numActBanks - 1.U }
  
//val lowestIdx = Vec(allReadQueues map { _.io.deq }).indexWhere(_.valid)
  // TODO: sensible mapping to PCRAM bus width

  cmdBusBusy.io.set.bits := timings.tCCD_PCRAM - 1.U
  cmdBusBusy.io.set.valid := (selectedCmd =/= cmd_nop)


  //val powerStats =
  val powerMonitor = Module(new PCRAMRankPowerMonitor(cfg.pcramKey))
    powerMonitor.io.selectedCmd := selectedCmd
    powerMonitor.io.cmdUsesThisRank := true.B
    powerMonitor.io.rankState := rankStateTrackers.io.rank

  io.mmReg.rankPower := powerMonitor.io.stats
}
