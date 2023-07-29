#!/bin/sh

cd $(dirname $0)
mkdir -p figures/

./qualitative_graphics.py --bible 553 --output figures/Esperanto.png --result 0.002
./qualitative_graphics.py --bible 1046 --output figures/latin-vulgate.png --result 0.002
./qualitative_graphics.py --bible 779 --output figures/Hiligaynon.png --result 0.002
./ngram-comparison.py  --bible 779 --output figures/hiligaynon-tokenisation.png --result 0.002
