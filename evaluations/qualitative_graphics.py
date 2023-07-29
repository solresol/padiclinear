#!/usr/bin/env python3

import matplotlib.pyplot
import numpy
import os
import math
import string
import json
import pandas
import scipy
import scipy.spatial
import scipy.stats
import tqdm
import random
import sklearn.linear_model
import psycopg2
import configparser
import sqlalchemy
import argparse


parser = argparse.ArgumentParser(description="""
This program connects to a database with the LEAFTOP dataset in it, extracts out the
morphology results and then creates graphics of the results.""")

parser.add_argument("--bible-version-id", type=int,
                    required=True,
                    help="Bible version ID to work with")
parser.add_argument("--tokenisation-method-id", default="unigram",
                    choices=['unigram', 'bigram', 'trigram', 'uni_token', 'bi_token',
                             'tri_token', 'quad_token'])
parser.add_argument("--verbose", action="store_true", help="Show output results and the command being run")
parser.add_argument("--pgconf", default="db.conf",
                    help="Database configuration file: .ini format, with a section called database and fields dbname, user, password, host")
parser.add_argument("--result-version", default="0.03",
                    help="There can be different versions of the morphology results in the database, based on what singular2plural reported as its algorithm version. Select which version you want.")
parser.add_argument("--output", required=True,
                    help="Filename to write the figure to")
args = parser.parse_args()


config = configparser.ConfigParser()
config.read(args.pgconf)
dbname = config['database']['dbname']
user = config['database']['user']
password = config['database']['password']
host = config['database']['host']
port = config['database']['port']
conn = psycopg2.connect(f'dbname={dbname} user={user} password={password} host={host} port={port}')
cursor = conn.cursor()

# What's the name of the language and the bible version?

cursor.execute("""
select version_name, language_name
from bible_versions 
left join wikidata_iso639_codes 
on (bible_versions.language = wikidata_iso639_codes.iso_639_3_code)
where version_id = %s""", [args.bible_version_id])
row = cursor.fetchone()
version_name = row[0].title()
language_name =row[1]


engine = sqlalchemy.create_engine(
    f"postgresql+psycopg2://{user}:{password}@{host}:5432/{dbname}")

data = pandas.read_sql(f"""
 select * 
 from machine_learning_morphology_scoring
 where bible_version_id = {args.bible_version_id}
and tokenisation_method_id = '{args.tokenisation_method_id}'
and result_version = '{args.result_version}'
""", engine)


fig, ax = matplotlib.pyplot.subplots(figsize=(9,5))
data[data.calculation_algorithm == 'LocalPadicLinear'
              ].set_index('algorithm_region_size_parameter'
                         ).sort_index().proportion_correct.plot(ax=ax, color='green',
                                                               label="Local P-adic Linear")

data[data.calculation_algorithm == 'LocalEuclideanSiegel'
              ].set_index('algorithm_region_size_parameter'
                         ).sort_index().proportion_correct.plot(ax=ax, color='lightblue',
                                                               label="Local Euclidean Linear")

data[data.calculation_algorithm == 'HybridSiegel'
              ].set_index('algorithm_region_size_parameter'
                         ).sort_index().proportion_correct.plot(ax=ax, color='red',
                                                               label="Siegel with P-adic neighbourhood")
pandas.Series( index=range(0,21), data=
data[
    data.calculation_algorithm == 'GlobalPadicLinear'
].iloc[0].proportion_correct).plot(ax=ax, linestyle=":", color="gold",
                                  label="Global P-adic linear")


pandas.Series( index=range(0,21), data=
data[
    data.calculation_algorithm == 'GlobalSiegel'
].iloc[0].proportion_correct).plot(ax=ax, linestyle=":", color="brown",
                                  label="Global Siegel")


#ax.legend()
if args.tokenisation_method_id == 'unigram':
    ax.set_title(f"{language_name} ({version_name})")
else:
    ax.set_title(f"{language_name} ({version_name}) -- {args.tokenisation_method_id}")
ax.set_xlim((0,20))
ax.set_xticks([0,2,4,6,8,10,12,14,16,18,20])
ax.set_xlabel("Region size hyperparameter")
ax.set_ylabel("Proportion correct")
fig.legend(loc=5)
fig.tight_layout()
fig.subplots_adjust(right=0.66)
fig.savefig(args.output)
