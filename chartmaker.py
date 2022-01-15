#!/usr/bin/env python3

import argparse
import glob
import re
import os
import sys
import pandas
import matplotlib.pyplot

parser = argparse.ArgumentParser()
parser.add_argument(

r = re.compile('NearestByCount (\d+): (\d+) right, (\d+) wrong, out of a total of (\d+)$')
records = []
for filename in glob.glob('results/*.txt'):
    basename = os.path.basename(filename)
    version_name = basename.split('.')[0]
    f = open(filename)
    for line in f:
        m = r.match(line)
        if m is None:
            sys.exit(f"Bad line in {filename}: {line}")
        records.append({
            'filename': filename,
            'version_name': version_name,
            'neighbourhood': int(m.group(1)),
            'correct': int(m.group(2)),
            'incorrect': int(m.group(3)),
            'total': int(m.group(4)),
            'method': 'padic'
            })
df = pandas.DataFrame.from_records(records).set_index('neighbourhood')

version_counts = df.version_name.nunique()

fig, axes = matplotlib.pyplot.subplots(nrows=version_counts,
                                       figsize=(10, version_counts*6))
if version_counts == 1:
    axes = [axes]
    
for i, version in enumerate(df.version_name.unique()):
    temp_df = df[df.version_name == version].sort_index()
    temp_df.correct.plot(ax=axes[i], label="Correct",
                                               color="green")
    temp_df.incorrect.plot(ax=axes[i], label="Incorrect",
                                                 color="red")
    temp_df.total.plot(ax=axes[i], label="Total",
                                             color="blue")
    axes[i].set_title(version)
    axes[i].set_ylim((0, temp_df.total.max()+1))

fig.savefig('results/padic.png')
print(df)
            
    
    
