package midas
package models

import chisel3._
import chisel3.util._
import junctions._
import midas.widgets._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.GenericParameterizedBundle
import freechips.rocketchip.util.{ParameterizedBundle, DecoupledHelper}

import org.json4s._
import org.json4s.native.JsonMethods._

import Console.{UNDERLINED, GREEN, RESET}
import scala.collection.mutable
import scala.io.Source


trait HasPCRAMModelConstants {
  val maxPCRAMTimingBits = 32 // width of a PCRAM timing
  //val maxPCRAMTimingBits = 63 // width of a PCRAM timing
  val numBankStates = 3     // for idle, read and write
  //val numBankStates = 2       // for idle or active
  val numRankStates = 3
}

object PCRAMModelEnums extends HasPCRAMModelConstants {
  val cmd_nop :: cmd_read :: cmd_write :: Nil = Enum(3)
  val bank_idle :: bank_read :: bank_write :: Nil = Enum(numBankStates)
  val rank_idle :: rank_read :: rank_write :: Nil = Enum(numRankStates)
  val writeMode :: readMode :: Nil = Enum(2)
}


//case class JSONField(value: BigInt, units: String)

class PCRAMProgrammableTimings extends Bundle with HasPCRAMModelConstants with HasProgrammableRegisters 
    with HasConsoleUtils {
  // The most vanilla of PCRAM timings
  val tREAD      				= UInt(maxPCRAMTimingBits.W)
  val tWRITE     				= UInt(maxPCRAMTimingBits.W)
  val tTHRESHOLDREAD    = UInt(maxPCRAMTimingBits.W)
  val tTHRESHOLDWRITE   = UInt(maxPCRAMTimingBits.W)
  val tWTR_PCRAM 				= UInt(maxPCRAMTimingBits.W)
  val tCCD_PCRAM 				= UInt(maxPCRAMTimingBits.W)
  val tRTRS_PCRAM 		  = UInt(maxPCRAMTimingBits.W)
  val tCWD_PCRAM 				= UInt(maxPCRAMTimingBits.W)
  val tCAS_PCRAM 				= UInt(maxPCRAMTimingBits.W)

  // Defaults are set to sg093, x8, 2048Mb density (1GHz clock)
  
  val registers = Seq(
		(tREAD						-> RuntimeSetting(60,
																				"Read Latency",
																				min = 1,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tWRITE						-> RuntimeSetting(300,
																				"Write Latency",
																				min = 1,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tTHRESHOLDREAD		-> RuntimeSetting(200,
																				"Waiting-Time to Set Urgent for Read Request",
																				min = 1,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tTHRESHOLDWRITE		-> RuntimeSetting(1000,
																				"Waiting-Time to Set Urgent for Write Request",
																				min = 1,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tCCD_PCRAM			-> RuntimeSetting(4,
																				"Command-to-Command delay",
																				min = 0,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tRTRS_PCRAM			-> RuntimeSetting(2,
																				"Rank-to-Rank Switching Time",
																				min = 0,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tCWD_PCRAM			-> RuntimeSetting(10,
																				"Write CAS Latency: WR to data valid on bus from MC",
																				min = 0,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tCAS_PCRAM			-> RuntimeSetting(14,
																				"CAS Latency: RD to data valid on data bus",
																				min = 0,
																				max = Some((1 << (maxPCRAMTimingBits-1))-1))),
		(tWTR_PCRAM				-> RuntimeSetting(8,
																				"Write to Read Turnaround Time",
																				max = Some((1 << (maxPCRAMTimingBits-1))-1)))
	)
																				
	// Util.scala --> use ProgrammableSubAddr to set bankMask, bankOffset, rankMask, rankOffset,... etc
  override def cloneType = new PCRAMProgrammableTimings().asInstanceOf[this.type]

}

case class PCRAMBackendKey(writeDepth: Int, readDepth: Int, latencyBits: Int)

abstract class PCRAMBaseConfig( baseParams: BaseParams) extends BaseConfig(baseParams) with HasPCRAMModelConstants {
  def pcramKey: PCRAMOrganizationParams
  def backendKey: PCRAMBackendKey
}

abstract class BasePCRAMMMRegIO(cfg: PCRAMBaseConfig) extends MMRegIO(cfg) with HasConsoleUtils {
  // Addr assignment
  val bankAddr = Input(new ProgrammableSubAddr(cfg.pcramKey.bankBits, "Bank Address", 8, 15))
  val rankAddr = Input(new ProgrammableSubAddr(cfg.pcramKey.rankBits, "Rank Address", 12, 3))
  val rowAddr = Input(new ProgrammableSubAddr(cfg.pcramKey.rowBits, "Row Address", 14, 63))

  // Additional latency added to read data beats after it's received from the devices
  val backendLatency = Input(UInt(cfg.backendKey.latencyBits.W))
  val frontendLatency = Input(UInt(cfg.backendKey.latencyBits.W))

  val pcramTimings = Input(new PCRAMProgrammableTimings())
  val rankPower = Output(Vec(cfg.pcramKey.maxRanks, new PCRAMRankPowerIO))
  //val rankPower = Output(new PCRAMRankPowerIO)


  // END CHISEL TYPES
  val pcramBaseRegisters = Seq(
    (backendLatency -> RuntimeSetting(1,
                                      "Backend Latency",
                                      min = 1,
                                      max = Some(1 << (cfg.backendKey.latencyBits - 1)))),
    (frontendLatency -> RuntimeSetting(2,
                                      "Frontend Latency",
                                      min = 1,
                                      max = Some(1 << (cfg.backendKey.latencyBits - 1))))
  )

  // A list of DDR3 speed grades provided by micron.
  // _1 = is used as a key to look up a device,  _2 = long name
  val speedGrades = Seq(
    ("psg125" -> "DDR4-1600 (11-11-11)  Minimum Clock Period: 1250 ps"),
    ("psg25E" -> "DDR3-800E (5-5-5)  Minimum Clock Period: 2500 ps")
  )

  // Prompt the user for an address assignment scheme. TODO: Channel bits.
  def getAddressScheme(
      numRanks: BigInt,
      numBanks: BigInt,
      numRows: BigInt,
      numBytesPerLine: BigInt,
      pageSize: BigInt) {

    case class SubAddr(
        shortName: String,
        longName: String,
        field: Option[ProgrammableSubAddr],
        count: BigInt) {
      require(isPow2(count))
      val bits = log2Ceil(count)
      def set(offset: Int) { field.foreach( _.forceSettings(offset, count - 1) ) }
      def legendEntry = s"  ${shortName} -> ${longName}"
    }

    //val ranks       = SubAddr("L", "Rank Address Bits", Some(rankAddr), numRanks)
    val banks       = SubAddr("B", "Bank Address Bits", Some(bankAddr), numBanks)
    val rows        = SubAddr("R", "Row Address Bits", Some(rowAddr),  numRows)
    val linesPerRow = SubAddr("N", "log2(Lines Per Row)", None, pageSize/numBytesPerLine)
    val bytesPerLine= SubAddr("Z", "log2(Bytes Per Line)", None, numBytesPerLine)

    // Address schemes
    // _1 = long name, _2 = A seq of subfields from address MSBs to LSBs
    val addressSchemes = Seq(
      //"Baseline Open   " -> Seq(rows, ranks, banks, linesPerRow, bytesPerLine),
      "Baseline Open   " -> Seq(rows,  banks, linesPerRow, bytesPerLine),
      "Baseline Closed " -> Seq(rows, linesPerRow, banks, bytesPerLine)
    )

    val legendHeader = s"${UNDERLINED}Legend${RESET}\n"
    val legendBody   = (addressSchemes.head._2 map {_.legendEntry}).mkString("\n")

    val schemeStrings = addressSchemes map { case (name, addrOrder) =>
      val shortNameOrder = (addrOrder map { _.shortName }).mkString(" | ")
      s"${name} -> ( ${shortNameOrder} ) "
    }

    val scheme = addressSchemes(requestSeqSelection(
      "Select an address assignment scheme:",
      schemeStrings,
      legendHeader + legendBody + "\nAddress scheme number"))._2

    def setSubAddresses(ranges: Seq[SubAddr], offset: Int = 0): Unit = ranges match {
      case current :: moreSigFields =>
        current.set(offset)
        setSubAddresses(moreSigFields, offset + current.bits)
      case Nil => None
    }
    setSubAddresses(scheme.reverse)
  }

  // Prompt the user for a speedgrade selection. TODO: illegalize SGs based on frequency
  def getSpeedGrade(): String = {
    speedGrades(requestSeqSelection("Select a speed grade:", speedGrades.unzip._2))._1
  }

  // Get the parameters (timings, bitwidths etc..) for a paticular device from jsons in resources/
  def lookupPart(density: BigInt, dqWidth: BigInt, speedGrade: String): Map[String, JSONField] = {
    val dqKey = "x" + dqWidth.toString
    val stream = getClass.getResourceAsStream(s"/midas/models/dram/${density}Mb_ddr3.json")
    val lines = Source.fromInputStream(stream).getLines
    implicit val formats = org.json4s.DefaultFormats
    val json = parse(lines.mkString).extract[Map[String, Map[String, Map[String, JSONField]]]]
    json(speedGrade)(dqKey)
  }

  def setBasePCRAMSettings(): Unit = {

    // Prompt the user for overall memory organization of this channel
    Console.println(s"${UNDERLINED}PCRAM system organization${RESET}")
    val memorySize = requestInput("PCRAM system size in GiB", 2)
    val numRanks =   requestInput("Number of PCRAM ranks", 1)
    val busWidth =   requestInput("PCRAM data bus width in bits", 64)
    val dqWidth =    requestInput("Device DQ width", 8)

    val devicesPerRank = busWidth / dqWidth
    val deviceDensityMib = ((memorySize << 30) * 8 / numRanks / devicesPerRank) >> 20
    Console.println(s"${GREEN}Selected Device density (Mib) -> ${deviceDensityMib}${RESET}")

    // Select the appropriate device, and look up it's parameters in resource jsons
    Console.println(s"\n${UNDERLINED}Device Selection${RESET}")
    val freqMHz       = requestInput("Clock Frequency in MHz", 1000)
    val speedGradeKey = getSpeedGrade()

    val lut = lookupPart(deviceDensityMib, dqWidth, speedGradeKey)
    //val pcramTimingSettings = pcramTimings.setDependentRegisters(lut, freqMHz)

    // Determine the address assignment scheme
    Console.println(s"\n${UNDERLINED}Address assignment${RESET}")
    val lineSize = requestInput("Line size in Bytes", 64)

    val numBanks = 16  // DDR3 Mandated
    //val numRanks = 4  // DDR3 Mandated
    val pageSize = ((BigInt(1) << lut("COL_BITS").value.toInt) * devicesPerRank * dqWidth ) / 8
    val numRows = BigInt(1) << lut("ROW_BITS").value.toInt
    getAddressScheme(numRanks, numBanks,  numRows, lineSize, pageSize)
  }
}

case class PCRAMOrganizationParams(maxBanks: Int, maxRanks: Int, maxActBanks: Int, maxWriteBanks: Int, pcramSize: BigInt, lineBits: Int = 8) {
  require(isPow2(maxBanks))
  require(isPow2(maxRanks))
  require(isPow2(pcramSize))
  require(isPow2(lineBits))
  //def bankBits = log2Up(maxBanks)
  //def rankBits = log2Up(maxRanks)
  def bankBits = log2Ceil(maxBanks)
  def rankBits = log2Ceil(maxRanks)
  def rowBits  = log2Ceil(pcramSize) - lineBits
  def maxRows  = 1 << rowBits
}

trait PCRAMCommandLegalBools {
  val canACT  = Output(Bool())
  val canREAD = Output(Bool())
  val canWRITE = Output(Bool())
}

trait HasPCRAMLegalityUpdateIO {
  val key: PCRAMOrganizationParams
  import PCRAMModelEnums._
  val timings = Input(new PCRAMProgrammableTimings)
  val selectedCmd = Input(cmd_nop.cloneType)
}

// Add some scheduler specific metadata to a reference
// TODO factor out different PCRAM metadata into a mixin
class PCRAMXactionSchedulerEntry(key: PCRAMBaseConfig)(implicit p: Parameters) extends NastiBundle()(p) {
  val xaction = new TransactionMetaData
  val addr = UInt(nastiXAddrBits.W)
}

class PCRAMModelEntry(key: PCRAMBaseConfig)(implicit p: Parameters) extends NastiBundle with HasPCRAMModelConstants {
  val addr = UInt(nastiXAddrBits.W)
  val xaction = new TransactionMetaData
  val latency = UInt(maxPCRAMTimingBits.W)
  val bankAddrOH = UInt(key.pcramKey.maxBanks.W)
  val bankAddr = UInt(key.pcramKey.bankBits.W)
  val rankAddrOH = UInt(key.pcramKey.maxRanks.W)
  val rankAddr = UInt(key.pcramKey.rankBits.W)

  def decode(from: NastiAddressChannel, mmReg: BasePCRAMMMRegIO) {
    bankAddr := mmReg.bankAddr.getSubAddr(addr)
    bankAddrOH := UIntToOH(bankAddr)
    rankAddr := mmReg.rankAddr.getSubAddr(addr)
    rankAddrOH := UIntToOH(rankAddr)
  }
  //val bankAddr = WireInit(UInt(key.pcramKey.bankBits.W), init=mmReg.bankAddr.getSubAddr(addr))
  //val bankAddrOH = WireInit(UInt(key.pcramKey.maxBanks.W), init=UIntToOH(bankAddr))
  //val rankAddr = WireInit(UInt(key.pcramKey.rankBits.W), init=mmReg.rankAddr.getsubAddr(addr))
  //val rankAddrOH = WireInit(UInt(key.pcramKey.maxRanks.W), init=UIntToOh(rankAddr))


  override def cloneType = new PCRAMModelEntry(key)(p).asInstanceOf[this.type]
}

class PCRAMModelLegalEntry(key: PCRAMBaseConfig)(implicit p: Parameters) extends PCRAMModelEntry(key)(p) {
  val canACT = Bool()

  override def cloneType = new PCRAMModelLegalEntry(key)(p).asInstanceOf[this.type]
}

// Tracks the state of a bank, including:
//   - Whether it's active/idle
//   - Whether READ, WRITE, and ACT commands can be legally issued
//
// A PCRAMModel model uses these trackers to filte out illegal instructions for this bank
//
// A necessary condition for the controller to issue a CMD that uses this bank
// is that the can{CMD} bit be high. The controller of course all extra-bank
// timing and resource constraints are met. The controller must also ensure CAS
// commands use the open ROW. 

class PCRAMBankStateTrackerO(key: PCRAMOrganizationParams) extends GenericParameterizedBundle(key)
    with PCRAMCommandLegalBools {

  import PCRAMModelEnums._
  //val state = Output(Bool())
  val state = Output(bank_idle.cloneType)
}

class PCRAMBankStateTrackerIO(val key: PCRAMOrganizationParams) extends GenericParameterizedBundle(key)
  with HasPCRAMLegalityUpdateIO {
  val out = new PCRAMBankStateTrackerO(key)
  val cmdUsesThisBank = Input(Bool())
}

class PCRAMBankStateTracker(key: PCRAMOrganizationParams) extends Module with HasPCRAMModelConstants {
  import PCRAMModelEnums._
  val io = IO(new PCRAMBankStateTrackerIO(key))

  val state = RegInit(bank_idle)
  val internalState = RegInit(0.U(4.W))

  val nextLegalACT = Module(new DownCounter(maxPCRAMTimingBits))
  nextLegalACT.io.decr := true.B
  nextLegalACT.io.set.valid := false.B
  nextLegalACT.io.set.bits :=DontCare

  when (io.cmdUsesThisBank) {
    switch(io.selectedCmd) {
      is(cmd_read) {
        assert(io.out.canACT, "Bank Timing Violation: Controller issued read command illegally")
        //state := bank_active
        state := bank_read
        internalState := 1.U
        nextLegalACT.io.set.valid := true.B
        nextLegalACT.io.set.bits := io.timings.tREAD - 1.U
        //nextLegalACT.io.set.bits := 100.U
      }
      is(cmd_write) {
        assert(io.out.canACT, "Bank Timing Violation: Controller issued write command illegally")
        //state := bank_active
        state := bank_write
        internalState := 2.U
        nextLegalACT.io.set.valid := true.B
        nextLegalACT.io.set.bits := io.timings.tWRITE - 1.U
      }
    }
  }
  
  when(nextLegalACT.io.current ===1.U) {
    state := bank_idle
    internalState := 0.U
  }

  io.out.canACT := state === bank_idle
  io.out.canREAD := state === bank_idle
  io.out.canWRITE := state === bank_idle
  io.out.state := state 
}


// Tracks the state of a rank, including:
//   - Whether READ and WRITE commands can be legally issued
//

class PCRAMRankStateTrackerO(key: PCRAMOrganizationParams) extends GenericParameterizedBundle(key)
    with PCRAMCommandLegalBools {
  import PCRAMModelEnums._
  val banks = Vec(key.maxBanks, Output(new PCRAMBankStateTrackerO(key)))
  val state = Output(rank_idle.cloneType)
}

class PCRAMRankStateTrackerIO(val key: PCRAMOrganizationParams) extends GenericParameterizedBundle(key)
    with HasPCRAMLegalityUpdateIO with HasPCRAMModelConstants {
  val rank = new PCRAMRankStateTrackerO(key)
  val tCycle = Input(UInt(maxPCRAMTimingBits.W))
  val cmdUsesThisRank = Input(Bool())
  val cmdBankOH = Input(UInt(key.maxBanks.W))
}

class PCRAMRankStateTracker(key: PCRAMOrganizationParams) extends Module with HasPCRAMModelConstants {
  import PCRAMModelEnums._

  val io = IO(new PCRAMRankStateTrackerIO(key))

  val state = RegInit(rank_idle)
  /*
  val nextLegalACT = Module(new DownCounter(maxPCRAMTimingBits))
  nextLegalACT.io.decr := true.B
  nextLegalACT.io.set.valid := false.B
  nextLegalACT.io.set.bits := DontCare
  */
  //val wasRead = Mux(io.selectedCmd === cmd_read,  
  //val cmdChecker = Module(new Queue( Bool(), 1, pipe=true))

  //val nextLegalACT = Module(new DownCounter(maxPCRAMTimingBits))
  val nextLegalREAD = Module(new DownCounter(maxPCRAMTimingBits))
  val nextLegalWRITE = Module(new DownCounter(maxPCRAMTimingBits))

  Seq( nextLegalREAD, nextLegalWRITE) foreach { mod =>
    mod.io.decr := true.B
    mod.io.set.valid := false.B
    mod.io.set.bits := DontCare
  }

  // need to modify for signle counter

  //when (io.cmdUsesThisRank) {
    switch(io.selectedCmd) {
      is(cmd_read) {
        assert(!io.cmdUsesThisRank || io.rank.canREAD, "Bank Timing Violation: Controller issued read command illegally: need to keep tCMD/tCCD")
        state := rank_read
        nextLegalREAD.io.set.valid := true.B
        nextLegalREAD.io.set.bits := io.timings.tCCD_PCRAM - 1.U + 
          Mux(io.cmdUsesThisRank, 0.U, io.timings.tRTRS_PCRAM)
        nextLegalWRITE.io.set.valid := true.B
        nextLegalWRITE.io.set.bits := io.timings.tCCD_PCRAM - 1.U + io.timings.tCAS_PCRAM - io.timings.tCWD_PCRAM
      }
      is(cmd_write) {
        assert(!io.cmdUsesThisRank || io.rank.canWRITE, "Bank Timing Violation: Controller issued write command illegally: need to keep tCMD/tCCD")
        state := rank_write
        nextLegalREAD.io.set.valid := true.B
        nextLegalREAD.io.set.bits := Mux(io.cmdUsesThisRank,
          io.timings.tCCD_PCRAM + io.timings.tWTR_PCRAM + io.timings.tCWD_PCRAM - 1.U,
          io.timings.tCCD_PCRAM + io.timings.tWTR_PCRAM + io.timings.tCWD_PCRAM + io.timings.tRTRS_PCRAM - 1.U)
        /*
        nextLegalREAD.io.set.bits := Mux(io.cmdUsesThisRank,
          io.timings.tCCD_PCRAM + io.timings.tWTR_PCRAM + io.timings.tCWD_PCRAM - 1.U,
          io.timings.tCCD_PCRAM + io.timings.tCWD_PCRAM + io.timings.tRTRS_PCRAM - io.timings.tCAS_PCRAM - 1.U)
          */
        nextLegalWRITE.io.set.valid := true.B
        nextLegalWRITE.io.set.bits := io.timings.tCCD_PCRAM - 1.U
      }
    }
  //}

  /*
  when(nextLegalACT.io.current === 1.U) {
    state := rank_idle
  }
  */

  val bankTrackers = Seq.fill(key.maxBanks)(Module(new PCRAMBankStateTracker(key)).io)

  io.rank.banks.zip(bankTrackers) foreach { case (out, bank) => out := bank.out }

  bankTrackers.zip(io.cmdBankOH.toBools) foreach { case (bank, cmdUsesThisBank)  =>
    bank.timings := io.timings
    bank.selectedCmd := io.selectedCmd
    bank.cmdUsesThisBank := cmdUsesThisBank && io.cmdUsesThisRank
  }

  io.rank.state := state
	//io.rank.canACT := nextLegalACT.io.idle
	io.rank.canACT := state === rank_idle 
  io.rank.canREAD := nextLegalREAD.io.idle
  io.rank.canWRITE := nextLegalWRITE.io.idle
}


class PCRAMCommandBusMonitor(val key: PCRAMOrganizationParams) extends Module {
  import PCRAMModelEnums._

	val rankBits = log2Ceil(key.maxRanks)
	val bankBits = log2Ceil(key.maxBanks)
  val io = IO( new Bundle {
    val cmd = Input(cmd_nop.cloneType)
    val rank = Input(UInt(rankBits.W))
    val bank = Input(UInt(bankBits.W))
  })

  val cycleCounter = RegInit(1.U(32.W))
  val lastCommand = RegInit(0.U(32.W))
  cycleCounter := cycleCounter + 1.U
  when (io.cmd =/= cmd_nop) {
    lastCommand := cycleCounter
    when (lastCommand + 1.U =/= cycleCounter) { printf("nop(%d);\n", cycleCounter - lastCommand - 1.U) }
  }

  switch (io.cmd) {
    //is(cmd_act) {
    //  printf("activate(%d, %d, %d); // %d\n", io.rank, io.bank, io.row, cycleCounter)
    //}
    is(cmd_read) {
      //val autoPRE = io.autoPRE
      //val burstChop = false.B
      val column = 0.U // Don't care since we aren't checking data
      //printf("read(%d, %d, %d); // \n",
      //  io.rank, io.bank, column)
    }
    is(cmd_write) {
      //val autoPRE = io.autoPRE
      //val burstChop = false.B
      val column = 0.U // Don't care since we aren't checking data
      val mask = 0.U // Don't care since we aren't checking data
      val data = 0.U // Don't care since we aren't checking data
      //printf("write(%d, %d, %d, %d, %d); // %d\n",
      //  io.rank, io.bank, column, mask, data, cycleCounter)
    }
  }
}

// Outputs for counters used to feed to micron's power calculator
// # CASR, CASW is a proxy for cycles of read and write data (assuming fixed burst length)
// 1 -  (ACT/(CASR + CASW)) = rank row buffer hit rate
class PCRAMRankPowerIO extends Bundle {
  val allPreCycles = UInt(32.W) // # of cycles the rank has all banks precharged
  val numREAD = UInt(32.W) // Assume no burst-chop
  val numWRITE = UInt(32.W) // Ditto above

  // TODO
  // CKE low & all banks pre
  // CKE low & at least one bank active
}

object PCRAMRankPowerIO {
  def apply(): PCRAMRankPowerIO = {
    val w = Wire(new PCRAMRankPowerIO)
    w.numREAD := 0.U
    w.numWRITE := 0.U
		w.allPreCycles := 0.U
    w
  }
}

class PCRAMRankPowerMonitor(key: PCRAMOrganizationParams) extends Module with HasPCRAMModelConstants {
  import PCRAMModelEnums._
  val io = IO(new Bundle {
    val stats = Output(new PCRAMRankPowerIO)
    val rankState = Input(new PCRAMRankStateTrackerO(key))
    val selectedCmd = Input(cmd_nop.cloneType)
    val cmdUsesThisRank = Input(Bool())
  })
  val stats = RegInit(PCRAMRankPowerIO())

  when (io.cmdUsesThisRank) {
    switch(io.selectedCmd) {
      is(cmd_read) {
        stats.numREAD := stats.numREAD + 1.U
      }
      is(cmd_write) {
        stats.numWRITE := stats.numWRITE + 1.U
      }
    }
  }

  io.stats := stats
}

class PCRAMBackendIO(val latencyBits: Int)(implicit val p: Parameters) extends Bundle {
  val newRead = Flipped(Decoupled(new ReadResponseMetaData))
  val newWrite = Flipped(Decoupled(new WriteResponseMetaData))
  val completedRead = Decoupled(new ReadResponseMetaData)
  val completedWrite = Decoupled(new WriteResponseMetaData)
  val readLatency = Input(UInt(latencyBits.W))
  val writeLatency = Input(UInt(latencyBits.W))
  val tCycle = Input(UInt(latencyBits.W))
}

class PCRAMBackend(key: PCRAMBackendKey)(implicit p: Parameters) extends Module {
  val io = IO(new PCRAMBackendIO(key.latencyBits))
  val rQueue = Module(new DynamicLatencyPipe(new ReadResponseMetaData, key.readDepth, key.latencyBits))
  val wQueue = Module(new DynamicLatencyPipe(new WriteResponseMetaData, key.writeDepth, key.latencyBits))

  io.completedRead <> rQueue.io.deq
  io.completedWrite <> wQueue.io.deq
  rQueue.io.enq <> io.newRead
  rQueue.io.latency := io.readLatency
  wQueue.io.enq <> io.newWrite
  wQueue.io.latency := io.writeLatency
  Seq(rQueue, wQueue) foreach { _.io.tCycle := io.tCycle }
}
