package midas
package platform

import util.ParameterizedBundle // from rocketchip
import widgets._
import chisel3._
import chisel3.util._
import config.{Parameters, Field}
import junctions._

case object PCIeWidth extends Field[Int]
case object SoftRegKey extends Field[SoftRegParam]
case class SoftRegParam(addrBits: Int, dataBits: Int)

class SoftRegReq(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val addr = UInt(width=p(SoftRegKey).addrBits)
  val wdata = UInt(width=p(SoftRegKey).dataBits)
  val wr = Bool()
}

class SoftRegResp(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val rdata = UInt(width=p(SoftRegKey).dataBits)
}

class SoftRegBundle(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req = Flipped(Decoupled(new SoftRegReq))
  val resp = Decoupled(new SoftRegResp)
}

class CatapultShimIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val pcie = new SerialIO(p(PCIeWidth))
  val softreg = new SoftRegBundle
  val umireq = Decoupled(new CatapultMemReq)
  val umiresp = Flipped(Decoupled(new CatapultMemResp))
}

trait CatapultIFParams {
  val PCIE_DATA_WIDTH = 128
  val PCIE_SLOT_WIDTH = 16
  val PCIE_PAD_WIDTH = 4

  // the document says 32, but the code says 64...
  val UMI_ADDR_WIDTH = 64

  // looks like use_ecc_ddr is hardcoded to 1? in
  // Shells/Academic/shell_parameters.tcl
  //
  // the document says 512..
  val UMI_DATA_WIDTH = 576
  val UMI_DATA_BITS = log2Ceil(512/8)

  val SOFTREG_ADDR_WIDTH = 32
  val SOFTREG_DATA_WIDTH = 64
}

class CatapultMemReq extends Bundle with CatapultIFParams {
  // OMIT VALID HERE
  // should wrap with ready/valid
  val isWrite = Bool()
  val addr = UInt(width=UMI_ADDR_WIDTH)
  val data = UInt(width=UMI_DATA_WIDTH)
}

class CatapultMemResp extends Bundle with CatapultIFParams {
  // OMIT VALID HERE
  // should wrap with ready/valid
  val data = UInt(width=UMI_DATA_WIDTH)
}

class SimUMIMem(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new CatapultMemReq))
    val resp = Decoupled(new CatapultMemResp)
  })

  val umiDataWidth = 512/8
  val umiOffsetBits = log2Ceil(umiDataWidth)
  val size = 2L << 28L
  val depth = size >> umiOffsetBits

  val mem = Mem(depth.intValue, UInt(width=umiDataWidth*8))

  val req = Queue(io.req, 1)
  val idx = req.bits.addr >> umiOffsetBits

  req.ready := req.bits.isWrite || io.resp.ready
  io.resp.valid := req.valid && !req.bits.isWrite
  io.resp.bits.data := mem(idx)

  when (req.fire() && req.bits.isWrite) {
    mem(idx) := req.bits.data
  }
}

/* todo move to firesim */
class NastiUMIAdapter(implicit p: Parameters) extends NastiModule with CatapultIFParams {
  val io = IO(new Bundle {
    val nastimem = Flipped(new NastiIO)
    val umireq = Decoupled(new CatapultMemReq)
    val umiresp = Flipped(Decoupled(new CatapultMemResp))
  })

  // hide everything behind queues
  val awQ = Module(new Queue(new NastiWriteAddressChannel, 2))
  val wQ = Module(new Queue(new NastiWriteDataChannel, 8))
  val bQ = Module(new Queue(new NastiWriteResponseChannel, 2))
  val arQ = Module(new Queue(new NastiReadAddressChannel, 2))
  val rQ = Module(new Queue(new NastiReadDataChannel, 8))

  val umireqQ = Module(new Queue(new CatapultMemReq, 2))
  val umirespQ = Module(new Queue(new CatapultMemResp, 2))
 
  awQ.io.enq <> io.nastimem.aw
  wQ.io.enq <> io.nastimem.w
  io.nastimem.b <> bQ.io.deq
  arQ.io.enq <> io.nastimem.ar
  io.nastimem.r <> rQ.io.deq

  io.umireq <> umireqQ.io.deq
  umirespQ.io.enq <> io.umiresp

  val (sIdle :: sReadReq :: sWait :: sRead ::
       sWrite :: sWriteReq :: sWriteDone :: Nil) = Enum(UInt(), 7)
  val state = RegInit(sIdle)
  val isWr = Reg(Bool())
  val addr = Reg(UInt(nastiXAddrBits.W))
  val id = Reg(UInt(nastiXIdBits.W))
  val len = Reg(UInt(nastiXLenBits.W))
  val data = Reg(UInt(512.W))

  val DRAM_BASE = UInt(BigInt("80000000", 16))
  val numChunks = 512 / nastiXDataBits
  val count = Reg(UInt(log2Ceil(numChunks).W))
  assert(512 % nastiXDataBits == 0)
  assert(numChunks > 0)
  assert(isPow2(numChunks))

  val dataSeq = (0 until numChunks) map { i =>
    val chunk = data((i + 1) * nastiXDataBits - 1, i * nastiXDataBits)
    val wdata = (0 until nastiWStrobeBits) map { i =>
      val hi = 8 * (i + 1) - 1
      val lo = 8 * i
      Mux(isWr && wQ.io.deq.bits.strb(i), wQ.io.deq.bits.data(hi, lo), chunk(hi, lo))
    }
    Mux(count === i.U, Cat(wdata.reverse), chunk)
  }

  arQ.io.deq.ready := state === sIdle
  awQ.io.deq.ready := state === sIdle
  wQ.io.deq.ready  := state === sWrite
  rQ.io.enq.bits := NastiReadDataChannel(id, Vec(dataSeq)(count), !len.orR)
  rQ.io.enq.valid := state === sRead
  bQ.io.enq.bits := NastiWriteResponseChannel(id)
  bQ.io.enq.valid := state === sWriteDone

  umireqQ.io.enq.bits.addr := (addr >> UMI_DATA_BITS.U) << UMI_DATA_BITS.U
  umireqQ.io.enq.bits.data := data
  umireqQ.io.enq.bits.isWrite := state === sWriteReq
  umireqQ.io.enq.valid := state === sReadReq || state === sWriteReq
  umirespQ.io.deq.ready := state === sWait

  switch(state) {
    is(sIdle) {
      when(awQ.io.deq.valid) {
        printf("[nasti aw] addr: 0x%x, id: %d, len: %d\n",
               awQ.io.deq.bits.addr, awQ.io.deq.bits.id, awQ.io.deq.bits.len)
        addr  := awQ.io.deq.bits.addr - DRAM_BASE
        id    := awQ.io.deq.bits.id
        len   := awQ.io.deq.bits.len
        isWr  := true.B
        state := sReadReq
      }.elsewhen(arQ.io.deq.valid) {
        printf("[nasti ar] addr: 0x%x, id: %d, len: %d\n",
               arQ.io.deq.bits.addr, arQ.io.deq.bits.id, arQ.io.deq.bits.len)
        addr  := arQ.io.deq.bits.addr - DRAM_BASE
        id    := arQ.io.deq.bits.id
        len   := arQ.io.deq.bits.len
        isWr  := false.B
        state := sReadReq
      }
    }
    is(sReadReq) {
      when(umireqQ.io.enq.ready) {
        printf("[umi read req] addr: 0x%x\n", umireqQ.io.enq.bits.addr)
        state := sWait
      }
    }
    is(sWait) {
      when(umirespQ.io.deq.valid) {
        printf("[umi read resp] data: 0x%x\n", umirespQ.io.deq.bits.data)
        data  := umirespQ.io.deq.bits.data
        count := addr(log2Ceil(512/8), log2Ceil(nastiXDataBits/8))
        state := Mux(isWr, sWrite, sRead)
      }
    }
    is(sRead) {
      when(rQ.io.deq.ready) {
        printf("[nasti r] id: %d, data: 0x%x, last: %d\n",
               rQ.io.deq.bits.id, rQ.io.deq.bits.data, rQ.io.deq.bits.last)
        len := len - 1.U
        count := count + 1.U
        when(!len.orR) {
          state := sIdle
        }.elsewhen(count === (numChunks - 1).U) {
          addr  := ((addr >> UMI_DATA_BITS.U) + 1.U) << UMI_DATA_BITS.U
          state := sReadReq
        }
      } 
    }
    is(sWrite) {
      when(wQ.io.deq.valid) {
        printf("[nasti w] data: 0x%x, strb: 0x%x, last: %d\n",
               wQ.io.deq.bits.data, wQ.io.deq.bits.strb, wQ.io.deq.bits.last)
        when(!wQ.io.deq.bits.last) {
          len := len - 1.U
        }
        count := count + 1.U
        data  := Cat(dataSeq.reverse)
        when(wQ.io.deq.bits.last || count === (numChunks - 1).U) {
          state := sWriteReq
        }
      } 
    }
    is(sWriteReq) {
      when(umireqQ.io.enq.ready) {
        printf("[umi write req] addr: 0x%x, data: 0x%x\n",
               umireqQ.io.enq.bits.addr, umireqQ.io.enq.bits.data)
        addr  := ((addr >> UMI_DATA_BITS.U) + 1.U) << UMI_DATA_BITS.U
        state := Mux(len.orR, sReadReq, sWriteDone)
      } 
    }
    is(sWriteDone) {
      when(bQ.io.deq.ready) {
        state := sIdle
      }
    }
  }
}

class CatapultShim(simIo: midas.core.SimWrapperIO)
                  (implicit p: Parameters) extends PlatformShim with CatapultIFParams {
  val ctrlKey = p(widgets.CtrlNastiKey)
  val io = IO(new CatapultShimIO)
  val top = Module(new midas.core.FPGATop(simIo))
  val headerConsts = List(
    "PCIE_WIDTH"         -> p(PCIeWidth),
    "SOFTREG_ADDR_WIDTH" -> p(SoftRegKey).addrBits,
    "SOFTREG_DATA_WIDTH" -> p(SoftRegKey).dataBits,
    "SERIAL_WIDTH"       -> p(PCIeWidth) / 8,
    "UMI_ADDR_WIDTH"     -> UMI_ADDR_WIDTH,
    "UMI_DATA_WIDTH"     -> UMI_DATA_WIDTH,
    "MMIO_WIDTH"         -> p(SoftRegKey).dataBits / 8,
    "MEM_WIDTH"          -> 512 / 8
  ) ++ top.headerConsts

  val nastiumi = Module(new NastiUMIAdapter()(p alterPartial ({ case NastiKey => p(SlaveNastiKey) })))
  nastiumi.io.nastimem <> top.io.mem

  val SIMULATED = false
  // connect to simumimem for software simulation
  if (SIMULATED) {
    val simumimem = Module(new SimUMIMem)
    simumimem.io.req <> nastiumi.io.umireq
    nastiumi.io.umiresp <> simumimem.io.resp 
    // tie off top level
    io.umireq.valid := UInt(0)
    io.umiresp.ready := UInt(0)
  } else {
    io.umireq <> nastiumi.io.umireq
    nastiumi.io.umiresp <> io.umiresp
  }

  val sIdle :: sRead :: sWrite :: sWrAck:: Nil = Enum(UInt(), 4)
  val state = RegInit(sIdle)
  val dataSizeBits = UInt(log2Up(ctrlKey.dataBits/8))
  top.io.ctrl.aw.bits := NastiWriteAddressChannel(
    UInt(0), io.softreg.req.bits.addr << dataSizeBits, dataSizeBits)
  top.io.ctrl.aw.valid := io.softreg.req.valid && io.softreg.req.bits.wr && state === sIdle
  top.io.ctrl.ar.bits := NastiReadAddressChannel(
    UInt(0), io.softreg.req.bits.addr << dataSizeBits, dataSizeBits)
  top.io.ctrl.ar.valid := io.softreg.req.valid && !io.softreg.req.bits.wr && state === sIdle
  top.io.ctrl.w.bits := NastiWriteDataChannel(io.softreg.req.bits.wdata)
  top.io.ctrl.w.valid := state === sWrite
  io.softreg.req.ready := top.io.ctrl.ar.fire() || top.io.ctrl.w.fire()

  io.softreg.resp.bits.rdata := top.io.ctrl.r.bits.data
  io.softreg.resp.valid := top.io.ctrl.r.valid
  top.io.ctrl.r.ready := state === sRead && io.softreg.resp.ready
  top.io.ctrl.b.ready := state === sWrAck

  switch(state) {
    is(sIdle) {
      when(top.io.ctrl.ar.fire()) {
        state := sRead
      }.elsewhen(top.io.ctrl.aw.fire()) {
        state := sWrite
      }
    }
    is(sRead) {
      when(top.io.ctrl.r.fire()) {
        state := sIdle
      }
    }
    is(sWrite) {
      when(top.io.ctrl.w.fire()) {
        state := sWrAck
      }
    }
    is(sWrAck) {
      when(top.io.ctrl.b.fire()) {
        state := sIdle
      }
    }
  }

  // Turn off PCIe
  io.pcie.in.ready := Bool(false)
  io.pcie.out.valid := Bool(false)
}
