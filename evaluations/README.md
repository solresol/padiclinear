Evaluating padic linear
=======================



Requirements
------------

To reproduce the result from my (2022) thesis, you need a copy of the
`thousand_language` morphology database from the LEAFTOP
project. Procedures and schema for this are in

`git@github.com:solresol/thousand-language-morphology.git`

The resulting database will have many `machine_learning_morphology_*` tables 
and views which are initially empty.

There is a view `machine_learning_morphology_scoring_absentees` which
shows all the singular-plural morphological forms and algorithm
combinations which have not yet been calculated.

If `select * from machine_learning_morphology_scoring_absentees where
tokenisation_method_id = 'unigram'` is not empty, then run the
`./leaftop.py` command for each row that is present. After running it,
that row should disappear.

The columns in the output correspond to the command-line flags to give
to `./leaftop.py`:

- `bible_version_id` -> `--bible-version-id

- `tokenisation_method_id` -> `--tokenisation-method-id`

- `calculation_algorithm` -> see table below

- `algorithm_region_size_parameter` is the [COUNT] field for algorithms that
  operate over a small region.



| Algorithm Name         | Command-line flag             |
|----------------------- | ----------------------------- |
| `GlobalPadicLinear`    |  --global-padic-linear        |
| `GlobalSiegel`         |  --global-siegel              |
| `LocalPadicLinear`     |  --local-padic-linear [COUNT] |
| `LocalEuclideanSiegel` |  --local-siegel [COUNT]       |
| `HybridSiegel`         |  --local-hybrid [COUNT]       |
| `Y_Equals_X`           |  --y-equals-x                 |


When there are no more absentees, run

`refresh materialized view machine_learning_morphology_best_scores;`

`refresh materialized view machine_learning_morphology_summary_by_version`

`refresh materialized view machine_learning_morphology_summary_by_language`
