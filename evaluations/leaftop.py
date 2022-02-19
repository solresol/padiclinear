#!/usr/bin/env python3

import argparse
import configparser
import sqlalchemy
import pandas
import psycopg2
import tempfile
import os
import platform
import re
import subprocess
import sys
import time

parser = argparse.ArgumentParser(description="""
This program connects to a database with the LEAFTOP dataset in it, extracts out the
vocabulary from a bible version, runs singular2plural on it, and stores the results
back in the database.
""")

parser.add_argument("--path-to-singular2plural",
                    default="singular2plural",
                    help="Where the compiled singular2plural lives")
parser.add_argument("--bible-version-id", type=int,
                    required=True,
                    help="Bible version ID to work with")
parser.add_argument("--global-padic-linear", action="store_true",
                    help="Perform a global padic linear evaluation")
parser.add_argument("--global-siegel", action="store_true",
                    help="Perform a global Siegel evaluation")
parser.add_argument("--local-padic-linear", type=int, metavar="COUNT",
                    help="Perform a p-adic linear regressor based on the p-adically nearest COUNT points")
parser.add_argument("--local-siegel", type=int, metavar="COUNT",
                    help="Perform a siegel-based linear regressor based on the euclideanly nearest COUNT points")
parser.add_argument("--local-hybrid", type=int, metavar="COUNT",
                    help="Perform a Euclidean-metric siegel-based linear regressor based on the p-adically nearest COUNT points")
parser.add_argument("--show-answers", action="store_true",
                    help="Instead of storing something in the database, show the constructed plurals")
parser.add_argument("--show-rule-summary", action="store_true",
                    help="Instead of storing something in the database, show the rules that were used")
parser.add_argument("--show-failed-rule-detail", action="store_true",
                    help="Show the rules that were used in detail and with examples of failures")

parser.add_argument("--pgconf", default="db.conf",
                    help="Database configuration file: .ini format, with a section called database and fields dbname, user, password, host ")
parser.add_argument("--replace", action="store_true",
                    help="Instead of giving an error if the answer has already been stored, replace whatever is there")
parser.add_argument("--tokenisation-method-id", default="unigram",
                    choices=['unigram', 'bigram', 'trigram', 'uni_token', 'bi_token',
                             'tri_token', 'quad_token'])

args = parser.parse_args()

config = configparser.ConfigParser()
config.read(args.pgconf)
dbname = config['database']['dbname']
user = config['database']['user']
password = config['database']['password']
host = config['database']['host']
port = config['database']['port']
conn = psycopg2.connect(f'dbname={dbname} user={user} password={password} host={host} port={port}')
read_cursor = conn.cursor()
write_cursor = conn.cursor()
engine = sqlalchemy.create_engine(
    f"postgresql+psycopg2://{user}:{password}@{host}:5432/{dbname}")

df = pandas.read_sql(f"""SELECT singular.bible_version_id,
    singular.tokenisation_method_id,
    singular.lemma,
    singular.gender,
    singular.noun_case,
    singular.translation_in_target_language AS singular,
    plural.translation_in_target_language AS plural
   FROM vocabulary_extractions singular
   JOIN vocabulary_extractions plural USING (bible_version_id, tokenisation_method_id, lemma, gender, noun_case)
    where bible_version_id = {args.bible_version_id} 
      and tokenisation_method_id = '{args.tokenisation_method_id}'
      and singular.noun_number = 'singular' AND plural.noun_number = 'plural'
""", engine)

intermediary_file = tempfile.NamedTemporaryFile(mode='w', dir='.', prefix='extract_from_leaftop_',
                                                suffix='.csv', delete=False)
df.to_csv(intermediary_file, index=False, sep='\t')

subprocess_args = [ args.path_to_singular2plural ]

algo_name = None
algo_chosen = False
parametric_algorithm = False
region_size = None

if args.global_padic_linear:
    subprocess_args += ["--global-padic-linear"]
    algo_chosen = True
    algo_name = 'GlobalPadicLinear'

if args.global_siegel:
    if algo_chosen:
        sys.exit("Multiple algorithms chosen.")
    subprocess_args += ["--global-siegel"]
    algo_chosen = True
    algo_name = 'GlobalSiegel'
    
if args.local_padic_linear:
    if algo_chosen:
        sys.exit("Multiple algorithms chosen.")
    if args.local_padic_linear < 3:
        sys.exit("Cannot do anything meaningful with less than 3 neighbours in a dataset")
    subprocess_args += ["--local-padic", str(args.local_padic_linear)]
    algo_chosen = True
    algo_name = 'LocalPadicLinear'
    parametric_algorithm = True
    region_size = args.local_padic_linear

if args.local_siegel:
    if algo_chosen:
        sys.exit("Multiple algorithms chosen.")
    if args.local_siegel < 3:
        sys.exit("Cannot do anything meaningful with less than 3 neighbours in a dataset")
    subprocess_args += ["--local-siegel", str(args.local_siegel)]
    algo_chosen = True
    algo_name = 'LocalEuclideanSiegel'
    parametric_algorithm = True
    region_size = args.local_siegel

if args.local_hybrid:
    if algo_chosen:
        sys.exit("Multiple algorithms chosen.")
    if args.local_hybrid < 3:
        sys.exit("Cannot do anything meaningful with less than 3 neighbours in a dataset")
    subprocess_args += ["--hybrid-siegel-padic", str(args.local_hybrid)]
    algo_chosen = True
    algo_name = 'HybridSiegel'
    parametric_algorithm = True
    region_size = args.local_hybrid


if not algo_chosen:
    sys.exit("No algorithm chosen")

display_only = False
if args.show_answers:
    display_only = True
    subprocess_args += ["--show-answers"]

if args.show_rule_summary:
    display_only = True    
    subprocess_args += ["--show-rule-summary"]

if args.show_failed_rule_detail:
    display_only = True    
    subprocess_args += ["--show-failed-rule-detail"]

subprocess_args += [ intermediary_file.name ]


version_proc = subprocess.run([args.path_to_singular2plural, '--version'],
                              check=True, capture_output=True)
version = version_proc.stdout.decode('utf-8').strip()

if not display_only:
    if parametric_algorithm:
        read_cursor.execute("""
      select count(*) from machine_learning_morphology_scoring 
       where bible_version_id = %s
         and tokenisation_method_id = %s
         and calculation_algorithm = %s
         and algorithm_region_size_parameter = %s
         and result_version = %s""",
                          [args.bible_version_id,
                           args.tokenisation_method_id,
                           algo_name,
                           region_size,
                           version])
    else:
        read_cursor.execute("""
      select count(*) from machine_learning_morphology_scoring 
       where bible_version_id = %s
         and tokenisation_method_id = %s
         and calculation_algorithm = %s
         and algorithm_region_size_parameter is null
         and result_version = %s""",
                          [args.bible_version_id,
                           args.tokenisation_method_id,
                           algo_name,
                           version])
    existing_count = read_cursor.fetchone()
    if existing_count is None:
        sys.exit("Could not read from machine_learning_morphology_scoring table")
    existing_count = existing_count[0]
    if existing_count > 0 and not args.replace:
        sys.exit("Record already exists in machine_learning_morphology. No point in running")

start_time = time.time()        
proc = subprocess.run(subprocess_args, check=True, capture_output=True)
end_time = time.time()
computation_time = end_time - start_time
computation_hostname = platform.node()
text_output = proc.stdout.decode('utf-8')

if display_only:
    print(text_output)
    sys.exit(0)

print(text_output)
rules_summary_re = re.compile('^(.*): (\d+) right, (\d+) wrong, out of a total of (\d+)$')
m = rules_summary_re.match(text_output)
if m is None:
    sys.exit(f"Could not process {text_output}")
correct_count = int(m.group(2))
wrong_count = int(m.group(3))
total = int(m.group(4))

if existing_count == 0:
    if parametric_algorithm:
        write_cursor.execute("""
   insert into machine_learning_morphology_scoring ( 
     bible_version_id,
     tokenisation_method_id,
     calculation_algorithm,
     algorithm_region_size_parameter,
     result_version,
     answers_correct,
     answers_wrong,
     total_vocab_size_checked,
     computation_time,
     computation_hostname
   ) values (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)""",
                             [args.bible_version_id,
                             args.tokenisation_method_id,
                             algo_name,
                             region_size,
                             version,
                             correct_count,
                             wrong_count,
                              total,
                              computation_time,
                              computation_hostname]
                             )
    else:
        write_cursor.execute("""
   insert into machine_learning_morphology_scoring ( 
     bible_version_id,
     tokenisation_method_id,
     calculation_algorithm,
     result_version,
     answers_correct,
     answers_wrong,
     total_vocab_size_checked,
     computation_time,
     computation_hostname     
   ) values (%s, %s, %s, %s, %s, %s, %s, %s)""",
                             [args.bible_version_id,
                             args.tokenisation_method_id,
                             algo_name,
                             version,
                             correct_count,
                             wrong_count,
                              total,
                              computation_time,
                              computation_hostname]
                             )        
elif args.replace:
    if parametric_algorithm:
        write_cursor.execute("""
   update machine_learning_morphology_scoring set 
     answers_correct = %s,
     answers_wrong = %s,
     total_vocab_size_checked = %s,
     computation_time = %s,
     computation_hostname = %s
   where bible_version_id = %s
     and tokenisation_method_id = %s
     and calculation_algorithm = %s
     and algorithm_region_size_parameter = %s
     and result_version = %s""",
                             [correct_count,
                             wrong_count,
                             total,
                              computation_time,
                              computation_hostname,
                             args.bible_version_id,
                             args.tokenisation_method_id,
                             algo_name,
                             region_size,
                             version]
                             )
    else:
        write_cursor.execute("""
   update machine_learning_morphology_scoring set 
     answers_correct = %s,
     answers_wrong = %s,
     total_vocab_size_checked = %s,
     computation_time = %s,
     computation_hostname = %s
   where bible_version_id = %s
     and tokenisation_method_id = %s
     and calculation_algorithm = %s
     and result_version = %s
     and algorithm_region_size_parameter is null""",
                             [correct_count,
                             wrong_count,
                             total,
                              computation_time,
                              computation_hostname,
                             args.bible_version_id,
                             args.tokenisation_method_id,
                             algo_name,
                             version]
                             )            
                             
conn.commit()
    
os.remove(intermediary_file.name)
