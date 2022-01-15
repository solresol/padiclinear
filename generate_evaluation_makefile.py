#!/usr/bin/env python3

import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--tgz-with-extracts",
                    help="Directory with tgz file containing by-language/languagecode/tokenmethod/translationname.csv files",
                    required=True)
parser.add_argument("--makefile", help="Where to create the Makefile.", required=True)
parser.add_argument("--output-zip-folder", help="Where to put the zipped files from the process", required=True)
parser.add_argument("--local-counts-min",
                    type=int,
                    default=3)
parser.add_argument("--local-folder", help="Local node staging area, usually $PBS_JOBFS", required=True)
parser.add_argument("--local-counts-max",
                    type=int,
                    default=20)
parser.add_argument("--path-to-binary",
                    help="How to invoke singular2plural",
                    required=True)
parser.add_argument("--version", default="0.002")
parser.add_argument("--progress", help="Show progress bar", action="store_true")
parser.add_argument("--limit", help="Only create this many targets", default=None, type=int)
parser.add_argument("--token", help="e.g. 'unigram,uni_token' or 'unigram,bigram,trigram'")
args = parser.parse_args()

import glob
import os
import tqdm
import tarfile
import sys

source_directory = tarfile.open(args.tgz_with_extracts)
members = source_directory.getmembers()
everyfilename = [mem.name for mem in members if mem.isfile()]

if not all([x.startswith('by-language/') for x in everyfilename]):
    print([x  for x in everyfilename if not x.startswith('by-language/')])
    sys.exit("Not all filenames in the archive begin with 'by-language/'")

if not all([len(x.split('/')) == 4 for x in everyfilename]):
    sys.exit("Not all filenames in the archive have four components'")

if not all([x.split('/')[2] in ['unigram', 'bigram', 'trigram', 'uni_token', 'bi_token', 'tri_token',
                                     'quad_token'] for x in everyfilename]):
    sys.exit("Not all filenames in the archive used known extraction methods'")

if not all([x.endswith('.csv') for x in everyfilename]):
    sys.exit("Not all filenames in the archive end with .csv")
    

def metric_option_generator():
    yield (['--global-padic-linear'], 'global_padic_linear')
    yield (['--global-siegel'], 'global_siegel')
    for i in range(args.local_counts_min, args.local_counts_max+1):
        yield (['--local-padic', str(i)], f"local_padic.{i}")
        yield (['--local-siegel', str(i)], f"local_siegel.{i}")
        yield (['--hybrid-siegel', str(i)], f"local_hybrid.{i}")

        
def option_generator():
    for (a, d) in metric_option_generator():
        yield (['--show-answers'] + a, f"answers/{d}")
    for (a, d) in metric_option_generator():
        yield (['--show-rule-summary'] + a, f"rules/{d}")
    for (a, d) in metric_option_generator():
        yield (['--show-failed-rule-detail'] + a, f"rule-detail/{d}")

macro_target_list = []
micro_target_list = {}
micro_source_list = {}
build_rule_list = []
extract_rule_list = []

iterator = everyfilename
if args.progress:
    iterator = tqdm.tqdm(iterator)
    
for filename in iterator:
    fulltail = "/".join(filename.split('/')[-3:])
    language_code = filename.split('/')[-3]
    token_method = filename.split('/')[-2]
    if args.token is not None and token_method not in args.token.split(','):
        continue
    micro_target = f"{args.output_zip_folder}/{language_code}_{token_method}_{args.version}.tgz"
    if micro_target not in micro_target_list:
        micro_target_list[micro_target] = []
        micro_source_list[micro_target] = []
        macro_target_list.append(micro_target)
    source_filename = f"{args.local_folder}/extracts/by-language/{fulltail}"
    extract_rule_list.append(source_filename)
    name, ext = os.path.splitext(fulltail)
    for (a, d) in option_generator():
        output_filename = os.path.join(args.local_folder, 'results', d, name) + '.txt'
        relative_output_filename = os.path.join('results', d, name) + '.txt'
        micro_target_list[micro_target].append(output_filename)
        micro_source_list[micro_target].append(relative_output_filename)
        build_rule = f"""
{output_filename}: {args.local_folder}/extracts/by-language/{fulltail}
	mkdir -p {os.path.dirname(output_filename)}
	{args.path_to_binary} {' '.join(a)} {source_filename} > {output_filename}.tmp
	mv {output_filename}.tmp {output_filename}

"""
        build_rule_list.append(build_rule)
        if args.limit is not None and len(build_rule_list) > args.limit:
            break
    if args.limit is not None and len(build_rule_list) > args.limit:
        break        

with open(args.makefile, 'w') as f:
    f.write(f"""results-{args.version}.flag: {' '.join(macro_target_list)}
	touch results-{args.version}.flag
	echo Complete
""")
    f.write("\n")
    for micro_target in micro_target_list:
        print(micro_target, micro_target_list[micro_target])
        f.write(f"""{micro_target}: {' '.join(micro_target_list[micro_target])}
	tar --create --file {micro_target} --directory {args.local_folder} --gzip {' '.join(micro_source_list[micro_target])}

""")
    f.write(f"""{' '.join(extract_rule_list)}: {args.tgz_with_extracts}
	mkdir -p {args.local_folder}/extracts/
	tar --extract --directory {args.local_folder}/extracts/ --gunzip --file {args.tgz_with_extracts}

""")
    for build_rule in build_rule_list:
        f.write(build_rule)




                 


                    
