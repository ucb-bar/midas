package strober

import Chisel._

case object HostLen extends Field[Int]
case object AddrLen extends Field[Int]
case object TagLen extends Field[Int]
case object MemLen extends Field[Int]
case object DaisyLen extends Field[Int]
case object CmdLen extends Field[Int]
case object TraceLen extends Field[Int]
case object HTIFLen extends Field[Int]

object StroberParams {
  val hostlen = 32
  val addrlen = 26
  val memlen = 32
  val taglen = 5
  val cmdlen = 4
  val tracelen = 16  
  val htiflen = 16

  val mask = (key: Any, site: View, here: View, up: View) => key match {
    case HostLen => Dump("HOST_LEN", hostlen)
    case AddrLen => Dump("MIF_ADDR_BITS", addrlen)
    case MemLen => Dump("MIF_DATA_BITS", memlen)
    case TagLen => Dump("MIF_TAG_BITS", taglen)
    case CmdLen => Dump("CMD_LEN", cmdlen)
    case TraceLen => Dump("TRACE_LEN", tracelen)
    case HTIFLen => htiflen
    case DaisyLen => here(HostLen)
  }
}

// Enum type for step response
object StepResp extends Enumeration {
  val FIN, TRACE, PEEKQ = Value
}

// import daisyParams._

abstract trait StroberParams extends UsesParameters {
  val hostLen = params(HostLen)
  val addrLen = params(AddrLen)
  val tagLen  = params(TagLen)
  val memLen  = params(MemLen) 
  val daisyLen = params(DaisyLen)
  val traceLen = params(TraceLen)
  val tagNum = math.pow(2, tagLen).toInt
  val step_FIN   = UInt(StepResp.FIN.id)
  val step_TRACE = UInt(StepResp.TRACE.id)
  val step_PEEKQ = UInt(StepResp.PEEKQ.id)
}

// Enum type for debug commands
object Cmd extends Enumeration {
  val STEP, POKE, PEEK, POKEQ, PEEKQ, TRACE, MEM = Value
}

abstract trait Commands extends UsesParameters {
  val cmdLen = params(CmdLen)
  val STEP  = UInt(Cmd.STEP.id, cmdLen)
  val POKE  = UInt(Cmd.POKE.id, cmdLen)
  val PEEK  = UInt(Cmd.PEEK.id, cmdLen)
  val POKEQ = UInt(Cmd.POKEQ.id, cmdLen)
  val PEEKQ = UInt(Cmd.PEEKQ.id, cmdLen)
  val TRACE = UInt(Cmd.TRACE.id, cmdLen)
  val MEM   = UInt(Cmd.MEM.id, cmdLen)
}
