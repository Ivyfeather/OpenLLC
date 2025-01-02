/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
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
import chisel3.util.random.LFSR
import utility._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLMessages._
import coupledL2.prefetch.PrefetchTrain
import coupledL2._
import coupledL2.tl2chi._

class MSHRCtlFV(implicit p: Parameters) extends MSHRCtl {
  val timers = RegInit(VecInit(Seq.fill(mshrsAll)(0.U(64.W))))
  for (((timer, m), i) <- timers.zip(mshrs).zipWithIndex) {
    when(m.io.alloc.valid) {
      timer := 1.U
    }.elsewhen(m.io.status.valid) {
      timer := timer + 1.U
    }

    assert(timer <= 1000.U, "TimeOut")
  }
}
