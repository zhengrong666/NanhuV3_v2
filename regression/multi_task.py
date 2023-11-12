import os
import time
import sys
import getopt
from multiprocessing import Process, Pool
import regression_config as rcfg

def run_case(name, simulator):
  print('Run Case: %s' % name)
  vcs = 'make simv-run'
  emu = 'make emu_rtl-run'
  shell = ''
  if simulator == 'vcs':
    shell = vcs + " RUN_BIN=" + name
  elif simulator == 'verilator':
    shell = emu + " RUN_BIN=" + name
  print(shell)
  os.system(shell)

def multi_task_run(task_num, cfg, case_file_list):
  path = cfg['config']['case_abs_path']
  case_prefix = cfg['config']['case_prefix']
  simulator = cfg['config']['simulator']

  test_case_list = rcfg.get_case_list('./regression', case_file_list)
  print(test_case_list)

  pool = Pool(task_num)

  for list_name in test_case_list.keys():
    case_list = test_case_list[list_name]
    for case in case_list:
      case_name = case_prefix + '/' + case
      print(case_name)
      pool.apply_async(func = run_case, args = (case_name, simulator))
  pool.close()
  pool.join()