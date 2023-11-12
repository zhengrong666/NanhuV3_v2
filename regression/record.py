import regression_config as rcfg
import os

def parse_sim_result(file_name):
	print('parse result file: %s' % file_name)
	try:
		with open(file_name, 'r') as f:
			for line in f:
				if 'HIT GOOD TRAP' in line:
					return [file_name, "PASS"]
				elif 'different at pc' in line:
					return [file_name, "difftest Failed! Please debug it!"]
				elif 'Error-[FGP_AFFINITY_FAILED] cpu_affinity/auto_affinity failed' in line:
					return [file_name, "EDA Tools has no core, Please run it again!"]
	except EnvironmentError:	
		return [file_name, "This case has not sim_file"]
	return [file_name, "Unknown Error!!!!!!"]

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
	# should_rerun_case = './rerun_case.txt'
	# with open(should_rerun_case, 'w+') as f:
	# 	for case in unknown_list:
	# 		f.write(case[0] + '\n')
	# 	f.close()

def sim_result_statistic(cfg, test_list):
	sim_prefix = ''
	if cfg['config']['simulator'] == "verilator":
		sim_prefix = cfg['config']['sim_path'] + '/emu/' + cfg['config']['case_prefix']
	elif cfg['config']['simulator'] == "vcs":
		sim_prefix = cfg['config']['sim_path'] + '/rtl/' + cfg['config']['case_prefix']
	
	results = []
	test_case_list = rcfg.get_case_list('./regression', test_list)
	for list_name in test_case_list.keys():
		case_list = test_case_list[list_name]
		for case in case_list:
			sim_name = sim_prefix + '/' + case
			result = parse_sim_result(sim_name + '/sim.log')
			results.append(result)
	
	result_record('regression_result.txt', results)