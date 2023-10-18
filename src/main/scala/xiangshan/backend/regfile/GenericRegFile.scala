/***************************************************************************************
 * Copyright (c) 2020-2023 Institute of Computing Technology, Chinese Academy of Sciences
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
 ***************************************************************************************/
/***************************************************************************************
 * Author: Liang Sen
 * E-mail: liangsen20z@ict.ac.cn
 * Date: 2023-06-19
 ****************************************************************************************/
package xiangshan.backend.regfile

import chisel3._
import chisel3.experimental.prefix
import chisel3.util._
import xiangshan.{ExuInput, Redirect, SrcType, XSBundle, XSModule}
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.issue.RsIdx

class WritePort(dataWidth:Int)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val data = Input(UInt(dataWidth.W))
  val en = Input(Bool())
}

class ReadPort(dataWidth:Int)(implicit p: Parameters) extends XSBundle {
  val addr = Input(UInt(PhyRegIdxWidth.W))
  val en = Input(Bool())
  val data = Output(UInt(dataWidth.W))
}

class GenericRegFile(entriesNum:Int, writeBackNum:Int, bypassNum:Int, readPortNum:Int, readNumNoBypass:Int, dataWidth:Int, moduleName:String, hasZero:Boolean)(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle{
    val read = Vec(readPortNum, new ReadPort(dataWidth))
    val readNoBypass = Vec(readNumNoBypass, new ReadPort(dataWidth))
    val write = Vec(writeBackNum, new WritePort(dataWidth))
    val bypassWrite = Vec(bypassNum, new WritePort(dataWidth))
  })
  override val desiredName = moduleName
  println(s"${moduleName} read ports: $readPortNum regular write ports: $writeBackNum bypass write ports $bypassNum")
  private val writeDelay = Wire(Vec(writeBackNum, new WritePort(dataWidth)))
  private val bypassWriteDelay = Wire(Vec(bypassNum, new WritePort(dataWidth)))
  (writeDelay ++ bypassWriteDelay).zip(io.write ++ io.bypassWrite).foreach({case(wd, w) =>
    wd.en := RegNext(w.en, false.B)
    wd.addr := RegEnable(w.addr, w.en)
    wd.data := RegEnable(w.data, w.en)
  })
  private val mem = Reg(Vec(entriesNum, UInt(dataWidth.W)))
  private val writes = writeDelay ++ bypassWriteDelay
  mem.zipWithIndex.foreach({case(m, i) =>
    val hitVec = writes.map(w => w.en && w.addr === i.U)
    val dataSel = Mux1H(hitVec, writes.map(_.data))
    val wen = Cat(hitVec).orR
    when(wen){
      m := dataSel
    }
  })

  if(hasZero) mem(0) := 0.U

  io.read.foreach(r => {
    val addrOH = mem.indices.map(_.U === r.addr)
    val memReadData = Mux1H(addrOH, mem)
    val writeSrcs = io.bypassWrite ++ writeDelay ++ bypassWriteDelay
    val bypassHits = writeSrcs.map(w => w.en && w.addr === r.addr)
    val bypassData = Mux1H(bypassHits, writeSrcs.map(_.data))
    val bypassValid = bypassHits.reduce(_ | _)
    val data = Mux(bypassValid, bypassData, memReadData)
    r.data := RegEnable(data, r.en)
  })
  io.readNoBypass.foreach(r => {
    val addrOH = mem.indices.map(_.U === r.addr)
    val memReadData = Mux1H(addrOH, mem)
    val writeSrcs = writeDelay ++ bypassWriteDelay
    val bypassHits = writeSrcs.map(w => w.en && w.addr === r.addr)
    val bypassData = Mux1H(bypassHits, writeSrcs.map(_.data))
    val bypassValid = bypassHits.reduce(_ | _)
    val data = Mux(bypassValid, bypassData, memReadData)
    r.data := RegEnable(data, r.en)
  })
}