/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package chiL2FV

import chisel3._
import chisel3.util._
import coupledL2._
import coupledL2.tl2chi._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters

class TL2CHICoupledL2FV(implicit p: Parameters) extends TL2CHICoupledL2 {

  class CoupledL2FVImp(wrapper: LazyModule) extends CoupledL2Imp(wrapper) {
    override def createSlice(enableCHI: Boolean,
                             sliceId: Int,
                             edgeIn: TLEdgeIn,
                             edgeOut: TLEdgeOut): BaseSlice[_ <: BaseOuterBundle] = {
      if (enableCHI) {
        Module(new SliceFV()(p.alterPartial {
          case EdgeInKey => edgeIn
          case EdgeOutKey => edgeOut
          case BankBitsKey => bankBits
          case SliceIdKey => sliceId
        }))
      } else {
        require(false, "CHI-L2 used in TL system")
        ??? // = throw NotImplementedError
      }
    }
  }

  override lazy val module = new CoupledL2FVImp(this)
}