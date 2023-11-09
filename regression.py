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
			if 'HIT GOOD TRAP' in line:
				return [file_name, "PASS"]
			elif 'different at pc' in line:
				return [file_name, "difftest Failed! Please debug it!"]
			elif 'Error-[FGP_AFFINITY_FAILED] cpu_affinity/auto_affinity failed' in line:
				return [file_name, "EDA Tools has no core, Please run it again!"]
		return [file_name, "Unknown Error!!!!!!"]

def run_case(name, fail_list):
	print('Run Case: %s' % name)
	vcs = 'make simv-run'
	emu = 'make emu_rtl-run'
	shell = vcs + " RUN_BIN=" + name
	os.system(shell)

def result_record(file_name, case_list):
	pass_list = []
	fail_list = []
	unknown_list = []
	for case in case_list:
		result = case[1]
		if 'PASS' in result:
			pass_list.append(case)
		elif 'difftest Failed! Please debug it!' in result:
			fail_list.append(case)
		else:
			unknown_list.append(case)
	with open(file_name, 'a+') as f:
		f.write('This regression passed %d cases:' % len(pass_list) + '\n')
		for case in pass_list:
			f.write(case[0] + "\t\t\t\t" + case[1] + '\n')
		
		f.write('\n\n\nThis regression failed %d cases:' % len(fail_list) + '\n')
		for case in fail_list:
			f.write(case[0] + "\t\t\t\t" + case[1] + '\n')
		
		f.write('\n\n\n%d cases had an unknown error:' % len(unknown_list) + '\n')
		for case in unknown_list:
			f.write(case[0] + "\t\t\t\t" + case[1] + '\n')

		f.write('\n\n\nThis regression runs a total of %d cases\n' % len(case_list))
		f.write('passing rate: %.2f' % (len(pass_list) / len(case_list)))
		
		f.close()

	should_rerun_case = './rerun_case.txt'
	with open(should_rerun_case, 'w+') as f:
		for case in unknown_list:
			f.write(case[0] + '\n')
		f.close()

if __name__ == '__main__':
	case_list = parse_case_list('./case_list.txt')												#modified it to case list file
	for case in case_list:
		case = case.replace('\n', '').replace('\r', '')
	file_list = []
	print(case_list)
	pool = Pool(5)
	for case in case_list:
		case_name = "vcase/" + case																					#modified it to case's path
		pool.apply_async(func = run_case, args = (case_name, file_list))
	pool.close()
	pool.join()

	for case in case_list:
		case_current = case.replace('\n', '').replace('\r', '')
		case_name = './sim/rtl/vcase/' + case_current + '/sim.log'
		result = parse_sim_result(case_name)
		file_list.append([case_current, result[1]])

	result_record('./regression_result.txt', file_list)
	print(file_list)
