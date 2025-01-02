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
import coupledL2._
import coupledL2.tl2chi._
import org.chipsalliance.cde.config.Parameters

class SliceFV()(implicit p: Parameters) extends Slice {
//  val directoryTest = Module(new DirectoryTest)
//  directoryTest.io.metaWReq <> mainPipe.io.metaWReq
//  directoryTest.io.tagWReq <> mainPipe.io.tagWReq

  override lazy val mshrCtl = Module(new MSHRCtlFV())
}
