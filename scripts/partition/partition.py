import json
import re
import argparse
import os
import copy
from json import JSONEncoder

no_split = ["pred_taken"]

def match(line, pattern_c):
  res = pattern_c.search(line.strip())
  return not (res == None)

def io_parser(io_str:str):
  raw_words = io_str.split("_")
  words = []
  i = 0
  while True:
    if(i == len(raw_words) - 1):
      words.append(raw_words[i])
      break
    elif(f"{raw_words[i]}_{raw_words[i+1]}" in no_split):
      words.append(f"{raw_words[i]}_{raw_words[i+1]}")
      i = i + 1
    else:
      words.append(raw_words[i])

    if(i == len(raw_words) - 1):break
    i = i + 1

  num_re = re.compile("^[0-9]*$")
  if(words[0] == "auto"):
    bundle_end = [words.index(x) for x in words if (x == "in" or x == "out")][0]
    bundle_string = words[1]
    for i in range(2, bundle_end + 1):
      bundle_string = bundle_string + "_" + words[i]
    res = f".auto.elements(\"{bundle_string}\")"

    i = bundle_end + 1
    while True:
      this_is_num = match(words[i], num_re)
      if(this_is_num):
        res = res + f".asInstanceOf[Vec[Data]]({words[i]})"
      else:
        res = res + f".asInstanceOf[Bundle].elements(\"{words[i]}\")"

      i = i + 1
      if(i == len(words)):
        break

  else:
    res = ""
    i = 0
    while True:
      this_is_num = match(words[i], num_re)
      if(this_is_num):
        res = res + f"({words[i]})"
      else:
        res = res + f".{words[i]}"

      i = i + 1
      if(i == len(words)):
        break

  return res

class ModuleMeta:
  top_name = ""
  module_name = ""
  instance_path = ""
  import_path = ""
  comp_args = ""
  regular_io = []
  timestamp_io = []
  special_io = []
  func_num = 0

  def __init__(self, name, io:list[tuple]):
    self.module_name = name
    clock_pat = re.compile("clock")
    reset = re.compile("reset")
    timestamp_pat = re.compile("logTimestamp")
    special_pat = re.compile("(DISPLAY_LOG_ENABLE|XSPERF_CLEAN|XSPERF_DUMP)")
    def IsTimeStamp(x):return match(x[2], timestamp_pat)
    def IsSpecial(x):return match(x[2], special_pat)
    def IsRegualr(x):return not (match(x[2], timestamp_pat) or match(x[2], special_pat) or match(x[2], clock_pat) or match(x[2], reset))
    self.timestamp_io = list(filter(IsTimeStamp, io))
    self.special_io = list(filter(IsSpecial, io))
    self.regular_io = list(filter(IsRegualr, io))

  def gen_top(self, odir:str):
    ofile = os.path.join(odir, self.top_name + ".scala")
    with open(ofile, "w") as f:
      self.gen_prefix(f)
      self.gen_helpers(f)
      self.gen_io(f)
      if(len(self.special_io) != 0 or len(self.timestamp_io) != 0):
        self.gen_perflog_conn(f)
      self.gen_body(f)
      self.gen_tail(f)
      self.gen_main(f)

  def gen_prefix(self, f):
    pfx = f"""
package top
import chisel3._
import xiangshan._
import chisel3.stage.ChiselGeneratorAnnotation
import chipsalliance.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.HasRocketChipStageUtils
import xstransforms.ModulePrefixAnnotation
import {self.top_name}._
import {self.import_path}

"""
    print(pfx, file=f, end="")

  def gen_helpers(self, f):
    io_num = len(self.regular_io)
    slice_num = 200
    print(f"object {self.top_name} {{", file=f)
    func_idx = 0
    for i in range(0, io_num, slice_num):
      print(f"  def conn{func_idx}(wrp:{self.top_name}, mod:{self.module_name}):Unit = {{", file=f)
      for p in self.regular_io[i: i + slice_num]:
        signal_path = io_parser(p[2])
        if(p[0] == "input"):
          print(f"    mod.module{signal_path} := wrp.{p[2]}", file = f)
        else:
          print(f"    wrp.{p[2]} := mod.module{signal_path}", file = f)
      print("  }", file=f)
      func_idx = func_idx + 1
    print("}", file=f)
    self.func_num = func_idx

  def gen_io(self, f):
    class_head = f"""
class {self.top_name}(mod: {self.module_name})(implicit p: Parameters) extends Module {{
  val debugOpts = p(DebugOptionsKey)\n
"""
    print(class_head, file=f, end="")
    for p in (self.regular_io + self.special_io + self.timestamp_io):
      des = f"  val {p[2]:<60s} = IO({p[0].capitalize()}(UInt({p[1]}.W)))\n"
      print(des, file=f, end="")

  def gen_perflog_conn(self, f):
    special_conn = f"""
  if (!debugOpts.FPGAPlatform && (debugOpts.EnableDebug || debugOpts.EnablePerfDebug)) {{
    val timer = Wire(UInt(64.W))
    val logEnable = Wire(Bool())
    ExcitingUtils.addSource(logEnable, "DISPLAY_LOG_ENABLE")
    ExcitingUtils.addSource(timer, "logTimestamp")
    timer := {self.timestamp_io[0][2]}
    logEnable := DISPLAY_LOG_ENABLE
  }}

  if (!debugOpts.FPGAPlatform && debugOpts.EnablePerfDebug) {{
    val clean = Wire(Bool())
    val dump = Wire(Bool())
    ExcitingUtils.addSource(clean, "XSPERF_CLEAN")
    ExcitingUtils.addSource(dump, "XSPERF_DUMP")
    clean := XSPERF_CLEAN
    dump := XSPERF_DUMP
  }}
"""
    print(special_conn, file=f, end="")

  def gen_body(self, f):
    print("\n  private val inst = Module(mod.module)\n", file=f, end = "")
    def GetBundle(x):return x[2].split("_")[0]
    bundles = list(set(map(GetBundle, self.regular_io)))
    for b in bundles:
      des = f"  inst.{b} := DontCare\n"
      print(des, file=f, end="")
    print("", file=f)
    for i in range(self.func_num):
      print(f"  conn{i}(this, mod)", file=f)

  def gen_tail(self, f):
    pfx = f"""
}}
"""
    print(pfx, file=f, end="")

  def gen_main(self, f):
    main_str = f"""
object {self.top_name}Main extends App with HasRocketChipStageUtils {{
  override def main(args: Array[String]): Unit = {{
    val (config, firrtlOpts) = ArgParser.parse(args)
    val soc = DisableMonitors(p => LazyModule(new XSTop()(p)))(config)
    XiangShanStage.execute(firrtlOpts, Seq(
      ModulePrefixAnnotation(\"{self.top_name}_\"),
      ChiselGeneratorAnnotation(() => {{
        new {self.top_name}(soc.{self.instance_path})(config)
      }})
    ))
  }}
}}
"""
    print(main_str, file = f, end = "")

def module_rtl_parser(lines:list[str], module_names:list[str], module_meta_list):
  mod_decl_re = re.compile("^module ")
  mod_in_re = re.compile("^ *input ")
  mod_out_re = re.compile("^ *output ")
  word_pat = '[a-zA-Z0-9_\[\]:]+'
  name = ""
  io_list = []
  for l in lines:
    if(match(l, mod_decl_re)):
      name = l.rstrip('(\n').split(" ")[-1]
      if (name not in module_names):
        return
    if(match(l, mod_in_re) or match(l, mod_out_re)):
      words = re.findall(word_pat, l)
      if(len(words) == 2):
        io_list.append((words[0], "1", words[1]))
      elif(len(words) == 3):
        width = int(words[1].strip('[]').split(":")[0]) + 1
        io_list.append((words[0], str(width), words[2]))
      else:
        assert False, "Wrong io pattern!"
  module_meta_list.append(ModuleMeta(name, copy.deepcopy(io_list)))

def replace(line:str, re_list:list[re.Pattern[str]], target_list:list[str], top_list:list[str]):
  for idx in range(len(target_list)):
    if(match(line, re_list[idx])):
      return line.replace(target_list[idx], f"{top_list[idx]}_{top_list[idx]}")
  return line


if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='RTL module io parser')
  parser.add_argument('infile', type=str, help='RTL target file')
  parser.add_argument('-o','--out', type=str, required = True, help='RTL target file')

  args = parser.parse_args()

  curdir = os.path.dirname(__file__)
  work_dir = os.getcwd()
  src_path = os.path.join(work_dir,args.infile)
  cfg_path = os.path.join(curdir,"config/partition.json")
  artf_path = os.path.abspath(args.out)
  out_top_path = os.path.join(artf_path, os.path.basename(src_path))
  out_mk_path = os.path.join(artf_path, "makefile")
  target_modules_list = []
  target_modules_re_list = []
  target_top_module_list = []
  meta_dict = {}

  with open(cfg_path) as cf:
    meta_dict = json.load(cf)
    for m in meta_dict:
      target_modules_list.append(m['module_name'])
      target_top_module_list.append(m['top_name'])
      target_modules_re_list.append(re.compile(f"^ *{m['module_name'] }"))

  module_meta_list = []
  mod_decl_re = re.compile("^module ")
  mod_end_re = re.compile("^ *endmodule")

  of = open(out_top_path, "w")
  with open(src_path, "r") as f:
    lines = f.readlines()
    decl_founded = False
    decl_end = False
    module_lines = []
    for l in lines:
      if(match(l, mod_decl_re)):
        decl_founded = True
      if(match(l, mod_end_re)):
        decl_end = True
        decl_founded = False

      if(decl_founded):
        module_lines.append(copy.deepcopy(l))
      elif(decl_end):
        module_lines.append(copy.deepcopy(l))
        meta = module_rtl_parser(module_lines, target_modules_list, module_meta_list)
        module_lines = []
        decl_end = False

      new_line = replace(l, target_modules_re_list, target_modules_list, target_top_module_list)
      print(new_line, file = of, end = "")
  of.close()


  for m in module_meta_list:
    for md in meta_dict:
      if(md['module_name'] == m.module_name):
        m.instance_path = md['instance_path']
        m.top_name = md['top_name']
        m.import_path = md['import_path']
        m.comp_args = md['compile_args']

    m.gen_top(artf_path)

  with open(out_mk_path, "w") as mk:
    for m in module_meta_list:
      print(f"{m.top_name}:{cfg_path}", file=mk)
      print(f"\tmill -i XiangShan.runMain top.{m.top_name}Main \\", file=mk)
      print(f"\t{m.comp_args} \\", file=mk)
      print(f"\t-td {artf_path} --output-file {m.top_name}\n", file=mk)