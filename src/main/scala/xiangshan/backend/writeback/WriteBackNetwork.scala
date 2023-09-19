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
package xiangshan.backend.writeback

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan.backend.execute.exu.ExuType
import freechips.rocketchip.diplomacy._
import xiangshan.{ExuOutput, HasXSParameter, MemPredUpdateReq, Redirect}
import xiangshan.frontend.Ftq_RF_Components

class WriteBackNetwork(implicit p:Parameters) extends LazyModule{
  val node = new WriteBackNetworkNode

  lazy val module = new LazyModuleImp(this) with HasXSParameter {
    private val wbSources = node.in.map(i => (i._1, i._2._1))
    private val wbSourcesMap = node.in.map(elm => elm._2._1 -> (elm._1, elm._2._1)).toMap
    private val wbSink = node.out

    println("\n\nWriteback Network Info:")
    println(s"Writeback Num: ${wbSources.length}")
    wbSources.foreach(w => print(w._2))
    val io = IO(new Bundle{
      val pcReadAddr = Output(Vec(2, UInt(log2Ceil(FtqSize).W)))
      val pcReadData = Input(Vec(2, new Ftq_RF_Components))
      val redirectOut = Output(Valid(new Redirect))
      val memPredUpdate = Output(Valid(new MemPredUpdateReq))
    })
    private val jmpNum = wbSources.count(_._2.exuType == ExuType.jmp)
    private val aluNum = wbSources.count(_._2.exuType == ExuType.alu)
    private val lduNum = wbSources.count(w => (w._2.exuType == ExuType.ldu || w._2.exuType == ExuType.sta) && w._2.writebackToRob)
    private val redirectGen = Module(new RedirectGen(jmpNum, aluNum, lduNum))
    io.pcReadAddr := redirectGen.io.pcReadAddr
    redirectGen.io.pcReadData := io.pcReadData
    private val localRedirectReg = Pipe(redirectGen.io.redirectOut)

    private def PipeWithRedirect(in: Valid[ExuOutput], latency: Int, p: Parameters): Valid[ExuOutput] = {
      require(latency > 0)
      val res = Wire(Valid(new ExuOutput()(p)))
      val realIn = if (latency == 1) in else PipeWithRedirect(in, latency - 1, p)
      val validCond = realIn.valid && !realIn.bits.uop.robIdx.needFlush(localRedirectReg)
      res.valid := RegNext(validCond, false.B)
      res.bits := RegEnable(realIn.bits, validCond)
      res
    }

    private var jmpRedirectIdx = 0
    private var aluRedirectIdx = 0
    private var memRedirectIdx = 0
    print("\n\nRedirect Info:")
    wbSources.filter(e => e._2.hasRedirectOut && e._2.writebackToRob).foreach(source => {
      if(source._2.exuType == ExuType.jmp){
        print(source._2)
        redirectGen.io.jmpWbIn(jmpRedirectIdx) := source._1
        jmpRedirectIdx = jmpRedirectIdx + 1
      } else if(source._2.exuType == ExuType.alu){
        print(source._2)
        redirectGen.io.aluWbIn(aluRedirectIdx) := source._1
        aluRedirectIdx = aluRedirectIdx + 1
      } else if (source._2.exuType == ExuType.sta || source._2.exuType == ExuType.ldu) {
        print(source._2)
        redirectGen.io.memWbIn(memRedirectIdx) := source._1
        memRedirectIdx = memRedirectIdx + 1
      } else {
        require(false, "Unexpected redirect out exu!")
      }
    })

    redirectGen.io.redirectIn := localRedirectReg
    io.redirectOut := redirectGen.io.redirectOut
    io.memPredUpdate := redirectGen.io.memPredUpdate

    for((s, i) <- wbSink.zipWithIndex){
      val sinkParam = s._2._2
      val source = sinkParam.map(elm => wbSourcesMap(elm))
      val sink = s._1
      print(s"\n\n${s._2._1.name} sinkId #${i}")
      sink.zip(source).foreach({case(dst, (src,cfg)) =>
        print(cfg)
        val realSrc = WireInit(src)
        if(s._2._1.needWriteback && cfg.speculativeWakeup){
          val realValid = src.valid
          realSrc.bits.uop := RegEnable(src.bits.uop, realValid)
          realSrc.valid := RegNext(realValid, false.B)
        }
        if(s._2._1.isRob || s._2._1.isVrs || s._2._1.isVprs || s._2._1.isVms || s._2._1.isMemRs && cfg.throughVectorRf){
          dst := PipeWithRedirect(realSrc, 2, p)
        } else if(s._2._1.isIntRs){
          if (cfg.isIntType || cfg.isMemType) {
            dst := Pipe(realSrc)
          } else {
            dst := realSrc
          }
        } else if(s._2._1.isFpRs){
          if (cfg.isFpType || cfg.isMemType) {
            dst := Pipe(realSrc)
          } else {
            dst := realSrc
          }
        } else if(s._2._1.isMemRs){
          dst := Pipe(realSrc)
        } else if(s._2._1.isVprs || s._2._1.isVrs) {
          if (cfg.isFpType || cfg.isIntType || cfg.isMemType) {
            dst := Pipe(realSrc, 2)
          } else {
            dst := realSrc
          }
        } else {
          dst := realSrc
        }
      })
    }
    println("")
  }
}
