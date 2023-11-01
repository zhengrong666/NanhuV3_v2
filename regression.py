import os
import time

from multiprocessing import Process, Pool

def parse_case_list(file_name):
	with open(file_name, 'r') as f:
		case_list = f.readlines()
		return case_list
	return []

def parse_sim_result(file_name):
	print('parse result file: %s' % file_name)
	with open(file_name, 'r') as f:
		for line in f:
			if 'unit test pass!' in line:
				return [file_name, "PASS"]
	return [file_name, "Faild!!!!!!!!"]

def run_case(name, fail_list):
	print('Run Case: %s' % name)
	shell = "make simv-run RUN_BIN=" + name
	os.system(shell)

def result_record(file_name, fail_list):
	with open(file_name, 'a+') as f:
		for case in fail_list:
			f.write(case[0] + "\t\t\t\t" + case[1] + '\n')
		f.close()

if __name__ == '__main__':
	case_list = parse_case_list('./case_list.txt')												#modified it to case list file
	for case in case_list:
		case = case.replace('\n', '').replace('\r', '')
	fail_list = []
	print(case_list)
	pool = Pool(5)
	for case in case_list:
		case_name = "vcase/" + case																					#modified it to case's path
		pool.apply_async(func = run_case, args = (case_name, fail_list))
	pool.close()
	pool.join()

	for case in case_list:
		case_current = case.replace('\n', '').replace('\r', '')
		case_name = './sim/rtl/vcase/' + case_current + '/sim.log'
		result = parse_sim_result(case_name)
		fail_list.append([case_current, result[1]])

	result_record('./regression_result.txt', fail_list)
	print(fail_list)
