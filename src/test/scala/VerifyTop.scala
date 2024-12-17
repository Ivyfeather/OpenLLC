package openLLC

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import coupledL2._
import coupledL2.tl2tl._
import coupledL2.tl2chi._
import cc.xiangshan.openncb._
import cc.xiangshan.openncb.chi._
import utility._

class VerifyTop(numCores: Int = 1, numULAgents: Int = 0, banks: Int = 1, issue: String = Issue.Eb)(implicit p: Parameters) extends LazyModule
  with HasCHIMsgParameters {
  
  /*   L1D(L1I)* L1D(L1I)* ... L1D(L1I)*
   *       \         |          /
   *       L2        L2   ...  L2
   *         \       |        /
   *          \      |       /
   *                 L3
   */
  override lazy val desiredName: String = "VerifyTop"
  val delayFactor = 0.5
  val cacheParams = p(L2ParamKey)

  // ******* Instantiate L0s *******
  def createClientNode(name: String, sources: Int) = {
    val masterNode = TLClientNode(Seq(
      TLMasterPortParameters.v2(
        masters = Seq(
          TLMasterParameters.v1(
            name = name,
            sourceId = IdRange(0, sources),
            supportsProbe = TransferSizes(cacheParams.blockBytes)
          )
        ),
        channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
        minLatency = 1,
        echoFields = Nil,
        requestFields = Seq(huancun.AliasField(2)),
        responseKeys = cacheParams.respKey
      )
    ))
    masterNode
  }
  val l0_nodes = (0 until numCores).map(i => createClientNode(s"L0_$i", 32))

  // ******* Instantiate L1s *******
  val l1d_nodes = (0 until numCores).map(i => LazyModule(new TL2TLCoupledL2()(new Config((site, here, up) => {
    case L2ParamKey => cacheParams.copy(
      name                = s"L1d_$i",
      hartId              = i,
    )
    case EnableCHI => false
    case huancun.BankBitsKey => 1 // FV: 1 bank for L1s
    case MaxHartIdBits => log2Up(numCores)
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      i
    )
  }))))
  // val l1i_nodes = (0 until numCores).map {i =>
  //   (0 until numULAgents).map { j =>
  //     TLClientNode(Seq(
  //       TLMasterPortParameters.v1(
  //         clients = Seq(TLMasterParameters.v1(
  //           name = s"L1i_${i}_${j}",
  //           sourceId = IdRange(0, 32)
  //         ))
  //       )
  //     ))
  //   }
  // }

  // ******* Instantiate L2s *******
  val l2_nodes = (0 until numCores).map(i => LazyModule(new TL2CHICoupledL2()(new Config((site, here, up) => {
    case L2ParamKey => cacheParams.copy(
      name                = s"L2_$i",
      hartId              = i,
    )
    case EnableCHI => true
    case CHIIssue => issue
    case huancun.BankBitsKey => log2Ceil(banks)
    case MaxHartIdBits => log2Up(numCores)
    case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      i
    )
  }))))

  // L3 is defined as Module instead of LazyModule, so it is defined in LazyModuleImp below
  // ******* Instantiate L3Bridge *******
  val l3Bridge = LazyModule(new OpenNCB()(new Config((site, here, up) => {
    case CHIIssue => issue
    case NCBParametersKey => new NCBParameters(
      axiMasterOrder      = EnumAXIMasterOrder.WriteAddress,
      readCompDMT         = false,
      writeCancelable     = false,
      writeNoError        = true,
      axiBurstAlwaysIncr  = true
    )
  })))

  // ******* Instantiate Miscs *******
  val ram = LazyModule(new AXI4RAM(AddressSet(0, 0xff_ffffL), beatBytes = 32))
  val bankBinders = (0 until numCores).map(_ => BankBinder(banks, 64))

  // ******* Connect L0s to L1s *******
  l0_nodes.zip(l1d_nodes).foreach { case (l0, l1) => l1.node := l0 }

  // ******* Connect L1s to L2s *******
  l1d_nodes.zip(l2_nodes).zipWithIndex.foreach { case ((l1d, l2), i) =>
    val l1xbar = TLXbar()
    l1xbar := 
      TLBuffer() :=
      TLLogger(s"L2_L1[${i}].C[0]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) := 
      l1d.node

    // l1i_nodes(i).zipWithIndex.foreach { case (l1i, j) =>
    //   l1xbar :=
    //     TLBuffer() :=
    //     TLLogger(s"L2_L1[${i}].UL[${j}]", !cacheParams.FPGAPlatform && cacheParams.enableTLLog) :=
    //     l1i
    // }
    
    l2.managerNode :=
      TLXbar() :=*
      bankBinders(i) :*=
      l2.node :*=
      l1xbar
    /**
      * MMIO: make diplomacy happy
      */
    val mmioClientNode = TLClientNode(Seq(
      TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          "uncache"
        ))
      )
    ))
    l2.mmioBridge.mmioNode := mmioClientNode
  }

  ram.node := 
    AXI4Xbar() :=
    AXI4Fragmenter() :=
    l3Bridge.axi4node

  lazy val module = new LazyModuleImp(this) {
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    // l1d_nodes.zipWithIndex.foreach{
    //   case (node, i) =>
    //     node.makeIOs()(ValName(s"master_port_$i"))
    // }
    // if (numULAgents != 0) {
    //   l1i_nodes.zipWithIndex.foreach { case (core, i) =>
    //     core.zipWithIndex.foreach { case (node, j) =>
    //       node.makeIOs()(ValName(s"master_ul_port_${i}_${j}"))
    //     }
    //   }
    // }

    val l3 = Module(new OpenLLC()(new Config((site, here, up) => {
      case CHIIssue => issue
      case OpenLLCParamKey => OpenLLCParam(
        clientCaches = Seq.fill(numCores)(cacheParams.copy(
          ways = 2,
          sets = 2,
        )),
        fullAddressBits = ADDR_WIDTH,
        banks = 1,  // TODO: can be modified
        ways = 2,
        sets = 2
      )
    })))

    l1d_nodes.zipWithIndex.foreach { case (l1d, i) =>
      dontTouch(l1d.module.io)
      l1d.module.io.hartId := i.U
      l1d.module.io.debugTopDown := DontCare
      l1d.module.io.l2_tlb_req <> DontCare
    }

    l2_nodes.zipWithIndex.foreach { case (l2, i) =>
      /*
      val chilogger = CHILogger(s"L3_L2[${i}]", true)
      chilogger.io.up <> l2.module.io_chi
      l3.io.rn(i) <> chilogger.io.down
      */
      l2.module.io_chi <> l3.io.rn(i)
      dontTouch(l2.module.io)

      l2.module.io.hartId := i.U
      l2.module.io_nodeID := i.U(NODEID_WIDTH.W)
      l2.module.io.debugTopDown := DontCare
      l2.module.io.l2_tlb_req <> DontCare
    }

    /*
    val chilogger = CHILogger(s"MEM_L3", true)
    l3.io.sn.connect(chilogger.io.up)
    l3Bridge.module.io.chi.connect(chilogger.io.down)
    */
    l3.io.sn <> l3Bridge.module.io.chi
    l3.io.nodeID := numCores.U(NODEID_WIDTH.W)
  }
}

object VerifyTopCHIHelper {
  def gen(fTop: Parameters => VerifyTop)(args: Array[String]) = {
    val FPGAPlatform    = false
    val enableChiselDB  = !FPGAPlatform && true
    
    val config = new Config((_, _, _) => {
      case L2ParamKey => L2Param(
        ways                = 4,
        sets                = 128,
        clientCaches        = Seq(L1Param(aliasBitsOpt = Some(2))),
        // echoField        = Seq(DirtyField),
        enablePerf          = false,
        enableRollingDB     = enableChiselDB && true,
        enableMonitor       = enableChiselDB && true,
        enableTLLog         = enableChiselDB && true,
        elaboratedTopDown   = false,
        FPGAPlatform        = FPGAPlatform,

        // OddParity Data Check
        dataCheck           = Some("oddparity"),

        // SAM for tester ICN: Home Node ID = 33
        sam                 = Seq(AddressSet.everything -> 33)
      )
    })

    ChiselDB.init(enableChiselDB)

    val top = DisableMonitors(p => LazyModule(fTop(p)))(config)

    (new ChiselStage).execute(args, Seq(
      ChiselGeneratorAnnotation(() => top.module)
    ))

    ChiselDB.addToFileRegisters
    FileRegisters.write("./build")
  }
}

object VerifyTop_CHI_DualCore_0UL extends App {
  VerifyTopCHIHelper.gen(p => new VerifyTop(
    numCores = 2,
    numULAgents = 0,
    banks = 1
  )(p))(args)
}
