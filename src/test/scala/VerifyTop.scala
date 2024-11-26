package openLLC
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import coupledL2._
import coupledL2.tl2chi._
import coupledL2.tl2tl.TL2TLCoupledL2
import utility._
//import coupledL2AsL1.prefetch.CoupledL2AsL1PrefParam
//import coupledL2AsL1.tl2tl.{TL2TLCoupledL2 => TLCoupledL2AsL1}
//import coupledL2AsL1.tl2chi.{TL2CHICoupledL2 => CHICoupledL2AsL1}
class VerifyTop_CHIL2L3(numCores: Int = 2, numULAgents: Int = 0, banks: Int = 1)(implicit p: Parameters) extends LazyModule
  with HasCHIMsgParameters {
  /*   L1D(L1I)* L1D(L1I)* ... L1D(L1I)*
   *       \         |          /
   *       L2        L2   ...  L2
   *         \       |        /
   *          \      |       /
   *                L3
   */
  override lazy val desiredName: String = "VerifyTop"
  val delayFactor = 0.5
  val cacheParams = p(L2ParamKey)

  // ******* Instantiate L0s *******
  def createMMIOClientNode(name: String) = {
    val mmioClientNode = TLClientNode(Seq(
      TLMasterPortParameters.v1(
        clients = Seq(TLMasterParameters.v1(
          name = name
        ))
      )
    ))
    mmioClientNode
  }
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
  val l0_nodes = (0 until numCores).map(i => createClientNode(s"l0$i", 32))
  // ******* Instantiate L1s *******
  val l1s = (0 until numCores).map(i => LazyModule(new TL2TLCoupledL2()(new Config((_, here, _) => {
    case L2ParamKey => L2Param(
      name = s"l1$i",
      ways = 2,
      sets = 2,
      blockBytes = 2,
      channelBytes = TLChannelBeatBytes(1),
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
      echoField = Seq(),
      prefetch = Nil,
      mshrs = 4,
      hartId = i
    )
    case EnableCHI => false
    case huancun.BankBitsKey => log2Ceil(banks)
    case MaxHartIdBits => log2Up(numCores)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false) // Customize for FV, disable all Log
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, i) // Customize for FV, disable all perf
  }))))
  val l1_nodes = l1s.map(_.node)

  // ******* Instantiate L2s *******
  val l2s = (0 until numCores).map(i => LazyModule(new TL2CHICoupledL2()(new Config((_, here, _) => {
    case L2ParamKey =>
      L2Param(
        name = s"l2$i",
        ways = 2,
        sets = 4,
        blockBytes = 2, // Customize for FV
        mmioBridgeSize = 1, // Customize for FV
        channelBytes = TLChannelBeatBytes(1), // Customize for FV
        clientCaches = Seq(L1Param(aliasBitsOpt = Some(2))),
        echoField = Seq(huancun.DirtyField()),
        mshrs = 4, // Customize for FV
        hartId = i
      )
    case EnableCHI => true
    case CHIIssue => "B"
    case huancun.BankBitsKey => log2Ceil(banks)
    case MaxHartIdBits => log2Up(numCores)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false) // Customize for FV, disable all Log
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, i)// Customize for FV, disable all perf
  }))))
  val l2_nodes = l2s.map(_.node)

  // ******* Instantiate L3 *******
  val l3 = LazyModule(new DummyLLC(numCores)(new Config((_, _, _) => {
    case OpenLLCParamKey => OpenLLCParam(
      ways = 2,
      sets = 4,
      blockBytes = 2,
      beatBytes = 1,
      fullAddressBits = 5
    )
  })))

  // ******* Instantiate Miscs *******
  val ram = LazyModule(new AXI4RAM(AddressSet(0, 0x1fL), beatBytes = 1))
  val bankBinders = (0 until numCores).map(_ => BankBinder(banks, 2))

  // ******* Connecting all parts *******
  l0_nodes.zip(l1_nodes).foreach { case (l0, l1) => l1 := l0 }
  l1s.zip(l2s).zipWithIndex.foreach { case ((l1d, l2), i) =>
    val l1xbar = TLXbar()
    l1xbar :=
      TLBuffer() :=
      l1d.node
    l2.managerNode :=
      TLXbar() :=*
      bankBinders(i) :*=
      l2.node :*=
      l1xbar
    l2.mmioBridge.mmioNode := createMMIOClientNode(s"mmio_$i")
  }

  ram.node :=
    AXI4Xbar() :=
    AXI4Fragmenter() :=
    l3.axi4node

  lazy val module = new LazyModuleImp(this){
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)
    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)
    val io = IO(Vec(numCores, new Bundle() {
      // Input signals for formal verification
      val topInputRandomAddrs = Input(UInt(5.W))
      val topInputNeedT = Input(Bool())
    }))
    l1s.zipWithIndex.foreach { case (l2AsL1, i) =>
//      l2AsL1.module.io.prefetcherInputRandomAddr := io(i).topInputRandomAddrs
//      l2AsL1.module.io.prefetcherNeedT := io(i).topInputNeedT
      l2AsL1.module.io.l2_tlb_req <> DontCare
      l2AsL1.module.io.debugTopDown <> DontCare
      l2AsL1.module.io.hartId <> DontCare
      dontTouch(l2AsL1.module.io)
    }
    l2s.zipWithIndex.foreach { case (l2, i) =>
      l3.module.io.rn(i) <> l2.module.io_chi
      dontTouch(l2.module.io)
      l2.module.io.hartId := i.U
      l2.module.io_nodeID := i.U(NODEID_WIDTH.W)
      l2.module.io.debugTopDown := DontCare
      l2.module.io.l2_tlb_req <> DontCare
    }

/*
    val verify_timer = RegInit(0.U(50.W))
    verify_timer := verify_timer + 1.U
    val dir_resetFinish = WireDefault(false.B)
    BoringUtils.addSink(dir_resetFinish, "coupledL2_0_dir")
    assume(verify_timer < 100.U || dir_resetFinish)
    val offsetBits = 1
    val bankBits = 0
    val setBits = 2
    val tagBits = 2
    def parseAddress(x: UInt): (UInt, UInt, UInt) = {
      val offset = x
      val set = offset >> (offsetBits + bankBits)
      val tag = set >> setBits
      (tag(tagBits - 1, 0), set(setBits - 1, 0), offset(offsetBits - 1, 0))
    }
    val stateArray = Seq.fill(numCores)(WireDefault(VecInit.fill(4, 2)(0.U(2.W))))
    val tagArray = Seq.fill(numCores)(WireDefault(VecInit.fill(4, 2)(0.U(2.W))))
    for (i <- 0 until numCores) {
      BoringUtils.addSink(stateArray(i), s"stateArray_${i}")
      BoringUtils.addSink(tagArray(i), s"tagArray_${i}")
    }
    def generateAssert(addr: UInt, state1: UInt, state2: UInt): Unit = {
      val (tag, set, offset) = parseAddress(addr)
      val match_tag0 = tagArray(0)(set).map(_ === tag)
      val match_tag1 = tagArray(1)(set).map(_ === tag)
      val hit0 = match_tag0.reduce(_ || _)
      val hit1 = match_tag1.reduce(_ || _)
      val way0 = MuxCase(0.U, match_tag0.zipWithIndex.map {
        case (matched, indx) =>
          matched -> indx.U
      })
      val way1 = MuxCase(0.U, match_tag1.zipWithIndex.map {
        case (matched, indx) =>
          matched -> indx.U
      })
      assert(!(hit0 && stateArray(0)(set)(way0) === state1 &&
        hit1 && stateArray(1)(set)(way1) === state2))
    }
    generateAssert(0.U(32.W), MetaData.TIP, MetaData.TIP)
    generateAssert(0.U(32.W), MetaData.TIP, MetaData.TRUNK)
    generateAssert(0.U(32.W), MetaData.INVALID, MetaData.TIP)
    generateAssert(0.U(32.W), MetaData.BRANCH, MetaData.BRANCH)
    generateAssert(0.U(32.W), MetaData.TIP, MetaData.BRANCH)
    generateAssert(0.U(32.W), MetaData.TRUNK, MetaData.INVALID)
 */
  }
}
object VerifyTopCHIHelper {
  def gen(fTop: Parameters => VerifyTop_CHIL2L3)(args: Array[String]) = {
    val FPGAPlatform    = false
    val enableChiselDB  = !FPGAPlatform
    val config = new Config((_, _, _) => {
      case L2ParamKey => L2Param(
        clientCaches        = Seq(L1Param(aliasBitsOpt = Some(2))),
        // echoField        = Seq(DirtyField),
        enablePerf          = false,
        enableRollingDB     = enableChiselDB,
        enableMonitor       = enableChiselDB,
        enableTLLog         = enableChiselDB,
        elaboratedTopDown   = false,
        FPGAPlatform        = FPGAPlatform,
        // SAM for tester ICN: Home Node ID = 33
        sam                 = Seq(AddressSet.everything -> 33)
      )
    })
    ChiselDB.init(enableChiselDB)
    val top = DisableMonitors(p => LazyModule(fTop(p)))(config)
    (new ChiselStage).emitSystemVerilog(
      top.module,
      Array("--target-dir", "Verilog/CHI-L2L3")
    )
  }
}
object VerifyTop_CHI_DualCore_0UL extends App {
  VerifyTopCHIHelper.gen(p => new VerifyTop_CHIL2L3(
    numCores = 2,
    numULAgents = 0,
    banks = 1)(p)
  )(args)
}