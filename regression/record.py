import regression_config as rcfg
import os
import toml
from pprint import pprint

def get_analysis_info():
	cfg_file = os.path.abspath("./regression/analysis_info.toml")
	cfg = toml.load(cfg_file)
	pprint(cfg)

	info_keyword = cfg['analysis']['line_keyword']
	info_res = cfg['analysis']['analysis_res']

	return info_keyword, info_res

def parse_sim_result(file_name):
	print('parse result file: %s' % file_name)
	analysis_keyword, analysis_res = get_analysis_info()
	try:
		with open(file_name, 'r') as f:
			for line in f:
				for key, res in zip(analysis_keyword, analysis_res):
					if key in line:
						return [file_name, res]
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

def res_analysis(cfg, test_list):
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
	
	analysis_res = {}
	for result in results:
		res = result[1]
		keyword = result[0]
		if res in analysis_res.keys():
			analysis_res[res].append(keyword)
		else:
			analysis_res[res] = []
			analysis_res[res].append(keyword)

	cfg_file = os.path.abspath("./regression/analysis_info.toml")
	cfg = toml.load(cfg_file)

	result_file = cfg['result']['file']
	print(analysis_res)
	with open(result_file, 'a+') as f:
		for res_type in analysis_res.keys():
			res_list = analysis_res[res_type]
			f.write(res_type + (': %d\n'%len(res_list)))
			for case in res_list:
				f.write(case + '\n')
			f.write("---------------------------------------------------------------------------------------------\n\n")
		f.close()

def only_fault_link(cfg, test_list):
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
	
	analysis_res = {}
	for result in results:
		res = result[1]
		keyword = result[0]
		if res in analysis_res.keys():
			analysis_res[res].append(keyword)
		else:
			analysis_res[res] = []
			analysis_res[res].append(keyword)

	cfg_file = os.path.abspath("./regression/analysis_info.toml")
	cfg = toml.load(cfg_file)
	os.system('rm -rf ./only_failed/*')
	for res_type in analysis_res.keys():
		if res_type == 'Unknown Error!!!!!!' or res_type == 'difftest failed':
			res_list = analysis_res[res_type]
			for case in res_list:
				bin_name = case.split('/')
				print(bin_name[-2])
				link = 'ln -s /home/simulation_path/' + bin_name[-2] +  ' /home/simulation_path/only_failed'
				os.system(link)