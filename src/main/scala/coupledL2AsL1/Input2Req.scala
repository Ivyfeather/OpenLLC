package coupledL2AsL1

import chisel3._
import coupledL2.prefetch._
import org.chipsalliance.cde.config.Parameters

case class InputAsPrefectchParam() extends PrefetchParameters {
  override val hasPrefetchBit: Boolean = true
  override val hasPrefetchSrc: Boolean = true
  override val inflightEntries: Int = 16
}

class Input2Req(implicit p: Parameters) extends Prefetcher {
  val io_inputAddr = IO(Input(UInt(fullAddressBits.W)))
  val io_inputNeedT = IO(Input(new Bool()))

  println("--------------------------------")
  println(" Modify Prefetcher as Input2Req ")
  println("--------------------------------")

  io.req.valid := true.B
  io.req.bits.tag := parseFullAddress(io_inputAddr)._1
  io.req.bits.set := parseFullAddress(io_inputAddr)._2
  io.req.bits.vaddr.foreach(_ := 0.U)
  io.req.bits.needT := io_inputNeedT
  io.req.bits.source := {
    // for Core 0, it's 0,2,4...; for Core 1, it's 1,3,5...
    val reqSource = RegInit(cacheParams.hartId.U(sourceIdBits.W))
    when(io.req.valid && io.req.ready) {
      reqSource := reqSource + 2.U
    }
    reqSource
  }
  io.req.bits.pfSource := PfSource.NoWhere.id.U

  // train, resp, tlb_req are not used
  io.train.ready := true.B
  io.resp.ready := true.B

  io.tlb_req.req.valid := false.B
  io.tlb_req.req.bits := DontCare
  io.tlb_req.req_kill := DontCare
  io.tlb_req.resp.ready := true.B
}