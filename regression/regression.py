import os
import time
import sys
import getopt
from multiprocessing import Process, Pool
import regression_config as rcfg
import multi_task
import record

def parse_case_list(file_name):
	with open(file_name, 'r') as f:
		case_list = f.readlines()
		return case_list
	return []

def main():
	simulator = ""
	cfg = rcfg.parse_config('./regression')
	test_list = []
	if cfg['config']['sim_mode'] == 'part':
		test_list = cfg['config']['part_list']
	elif cfg['config']['sim_mode'] == 'all':
		test_list = cfg['case']['list']
	print(test_list)
	multi_task.multi_task_run(5, cfg, test_list)
	record.sim_result_statistic(cfg, test_list)
	


if __name__ == '__main__':
	main()