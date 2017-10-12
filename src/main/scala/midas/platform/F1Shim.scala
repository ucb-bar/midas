package midas
package platform

import freechips.rocketchip.util.ParameterizedBundle // from rocketchip

import chisel3._
import chisel3.util._
import junctions._
import freechips.rocketchip.config.{Parameters, Field}

class F1ShimIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val master = Flipped(new NastiIO()(p alterPartial ({ case NastiKey => p(MasterNastiKey) })))
  val NICmaster = Flipped(new NastiIO()(p alterPartial ({ case NastiKey => p(NICMasterNastiKey) })))
  val slave  = Vec(4, new NastiIO()(p alterPartial ({ case NastiKey => p(SlaveNastiKey) })))
}

class F1Shim(simIo: midas.core.SimWrapperIO)
              (implicit p: Parameters) extends PlatformShim {
  val io = IO(new F1ShimIO)
  val top = Module(new midas.core.FPGATop(simIo))
  val headerConsts = List(
    "MMIO_WIDTH" -> p(MasterNastiKey).dataBits / 8,
    "MEM_WIDTH"  -> p(SlaveNastiKey).dataBits / 8
  ) ++ top.headerConsts

  val cyclecount = Reg(init = UInt(0, width=64.W))
  cyclecount := cyclecount + UInt(1)

  // print all transactions
  when (io.master.aw.fire()) {
    printf("[master,awfire,%x] addr %x, len %x, size %x, burst %x, lock %x, cache %x, prot %x, qos %x, region %x, id %x, user %x\n",
      cyclecount,
      io.master.aw.bits.addr,
      io.master.aw.bits.len,
      io.master.aw.bits.size,
      io.master.aw.bits.burst,
      io.master.aw.bits.lock,
      io.master.aw.bits.cache,
      io.master.aw.bits.prot,
      io.master.aw.bits.qos,
      io.master.aw.bits.region,
      io.master.aw.bits.id,
      io.master.aw.bits.user
      )
  }

  when (io.master.w.fire()) {
    printf("[master,wfire,%x] data %x, last %x, id %x, strb %x, user %x\n",
      cyclecount,
      io.master.w.bits.data,
      io.master.w.bits.last,
      io.master.w.bits.id,
      io.master.w.bits.strb,
      io.master.w.bits.user
      )
  }

  when (io.master.b.fire()) {
    printf("[master,bfire,%x] resp %x, id %x, user %x\n",
      cyclecount,
      io.master.b.bits.resp,
      io.master.b.bits.id,
      io.master.b.bits.user
      )
  }

  when (io.master.ar.fire()) {
    printf("[master,arfire,%x] addr %x, len %x, size %x, burst %x, lock %x, cache %x, prot %x, qos %x, region %x, id %x, user %x\n",
      cyclecount,
      io.master.ar.bits.addr,
      io.master.ar.bits.len,
      io.master.ar.bits.size,
      io.master.ar.bits.burst,
      io.master.ar.bits.lock,
      io.master.ar.bits.cache,
      io.master.ar.bits.prot,
      io.master.ar.bits.qos,
      io.master.ar.bits.region,
      io.master.ar.bits.id,
      io.master.ar.bits.user
      )
  }

  when (io.master.r.fire()) {
    printf("[master,rfire,%x] resp %x, data %x, last %x, id %x, user %x\n",
      cyclecount,
      io.master.r.bits.resp,
      io.master.r.bits.data,
      io.master.r.bits.last,
      io.master.r.bits.id,
      io.master.r.bits.user
      )
  }

  when (io.slave(0).aw.fire()) {
    printf("[slave,awfire,%x] addr %x, len %x, size %x, burst %x, lock %x, cache %x, prot %x, qos %x, region %x, id %x, user %x\n",
      cyclecount,

      io.slave(0).aw.bits.addr,
      io.slave(0).aw.bits.len,
      io.slave(0).aw.bits.size,
      io.slave(0).aw.bits.burst,
      io.slave(0).aw.bits.lock,
      io.slave(0).aw.bits.cache,
      io.slave(0).aw.bits.prot,
      io.slave(0).aw.bits.qos,
      io.slave(0).aw.bits.region,
      io.slave(0).aw.bits.id,
      io.slave(0).aw.bits.user
      )
  }

  when (io.slave(0).w.fire()) {
    printf("[slave(0),wfire,%x] data %x, last %x, id %x, strb %x, user %x\n",
      cyclecount,

      io.slave(0).w.bits.data,
      io.slave(0).w.bits.last,
      io.slave(0).w.bits.id,
      io.slave(0).w.bits.strb,
      io.slave(0).w.bits.user
      )
  }

  when (io.slave(0).b.fire()) {
    printf("[slave(0),bfire,%x] resp %x, id %x, user %x\n",
      cyclecount,

      io.slave(0).b.bits.resp,
      io.slave(0).b.bits.id,
      io.slave(0).b.bits.user
      )
  }

  when (io.slave(0).ar.fire()) {
    printf("[slave(0),arfire,%x] addr %x, len %x, size %x, burst %x, lock %x, cache %x, prot %x, qos %x, region %x, id %x, user %x\n",
      cyclecount,

      io.slave(0).ar.bits.addr,
      io.slave(0).ar.bits.len,
      io.slave(0).ar.bits.size,
      io.slave(0).ar.bits.burst,
      io.slave(0).ar.bits.lock,
      io.slave(0).ar.bits.cache,
      io.slave(0).ar.bits.prot,
      io.slave(0).ar.bits.qos,
      io.slave(0).ar.bits.region,
      io.slave(0).ar.bits.id,
      io.slave(0).ar.bits.user
      )
  }

  when (io.slave(0).r.fire()) {
    printf("[slave(0),rfire,%x] resp %x, data %x, last %x, id %x, user %x\n",
      cyclecount,

      io.slave(0).r.bits.resp,
      io.slave(0).r.bits.data,
      io.slave(0).r.bits.last,
      io.slave(0).r.bits.id,
      io.slave(0).r.bits.user
      )
  }

  top.io.ctrl <> io.master
  //io.slave <> top.io.mem
  io.slave.zip(top.io.mem).foreach {
    case (slave_i, top_mem_i) => slave_i <> top_mem_i
  }


  val (wCounterValue, wCounterWrap) = Counter(io.master.aw.fire(), 4097)
  top.io.ctrl.aw.bits.id := wCounterValue

  val (rCounterValue, rCounterWrap) = Counter(io.master.ar.fire(), 4097)
  top.io.ctrl.ar.bits.id := rCounterValue

  // benchmark register reads over 512b interface
  val LATENCY_IN_NS = 1000 // 1 us
  val NET_Gb = 64 // 64 Gbps network
  val PCIE_IF_WIDTH = 512
  val DEPTH = LATENCY_IN_NS * NET_Gb / PCIE_IF_WIDTH
  val dataBank = Reg(Vec(DEPTH, UInt(PCIE_IF_WIDTH.W)))

  val aw_queue = Queue(io.NICmaster.aw, 10)
  val w_queue = Queue(io.NICmaster.w, 10)
  val ar_queue = Queue(io.NICmaster.ar, 10)

  def fire_write(exclude: Bool, includes: Bool*) = {
    val rvs = Array (
      aw_queue.valid,
      w_queue.valid,
      io.NICmaster.b.ready
    )
    (rvs.filter(_ ne exclude) ++ includes).reduce(_ && _)
  }

  def fire_read(exclude: Bool, includes: Bool*) = {
    val rvs = Array (
      ar_queue.valid,
      io.NICmaster.r.ready
    )
    (rvs.filter(_ ne exclude) ++ includes).reduce(_ && _)
  }

  val writeBeatCounter = Reg(init = UInt(0, 9.W))
  when (fire_write(Bool(true), writeBeatCounter === aw_queue.bits.len)) {
    printf("resetting writeBeatCounter\n")
    writeBeatCounter := UInt(0)
  } .elsewhen(fire_write(Bool(true))) {
    printf("incrementing writeBeatCounter\n")
    writeBeatCounter := writeBeatCounter + UInt(1)
  } .otherwise {
    writeBeatCounter := writeBeatCounter
  }

  val readBeatCounter = Reg(init = UInt(0, 9.W))
  when (fire_read(Bool(true), readBeatCounter === ar_queue.bits.len)) {
    printf("resetting readBeatCounter\n")
    readBeatCounter := UInt(0)
  } .elsewhen(fire_read(Bool(true))) {
    printf("incrementing readBeatCounter\n")
    readBeatCounter := readBeatCounter + UInt(1)
  } .otherwise {
    readBeatCounter := readBeatCounter
  }

  // TODO is single write resp sufficient?
  io.NICmaster.b.bits.resp := UInt(0, width=2)
  io.NICmaster.b.bits.id := aw_queue.bits.id
  io.NICmaster.b.bits.user := aw_queue.bits.user
  io.NICmaster.b.valid := fire_write(io.NICmaster.b.ready, writeBeatCounter === aw_queue.bits.len)
  aw_queue.ready := fire_write(aw_queue.valid, writeBeatCounter === aw_queue.bits.len)
  w_queue.ready := fire_write(w_queue.valid)

  when (fire_write(Bool(false))) {
    printf("firing write\n")
    dataBank((aw_queue.bits.addr >> UInt(6)) + writeBeatCounter) := w_queue.bits.data
  }

  when (fire_read(Bool(false))) {
    printf("firing read\n")
  }

  io.NICmaster.r.valid := fire_read(io.NICmaster.r.ready)
  io.NICmaster.r.bits.data := dataBank((ar_queue.bits.addr >> UInt(6)) + readBeatCounter)
  io.NICmaster.r.bits.resp := UInt(0, width=2)
  io.NICmaster.r.bits.last := readBeatCounter === ar_queue.bits.len
  io.NICmaster.r.bits.id := ar_queue.bits.id
  io.NICmaster.r.bits.user := ar_queue.bits.user
  ar_queue.ready := fire_read(ar_queue.valid, readBeatCounter === ar_queue.bits.len)

}
