Source directory

data/
  scripts/  Data preparation scripts, numbered in their intended run order.
  clean/    Processed .fst datasets used by the research scripts.

factor/
  Shared factor utilities for scaling, residualisation, and information-coefficient analysis.

risk/
  Risk-model functions, including factor covariance estimation.

research/
  Research workflows for value, momentum, alpha construction, and portfolio optimisation.
  output/   Charts produced by the research workflows.

Typical workflow

1. Run the scripts in data/scripts to create the cleaned datasets.
2. Run the factor and research workflows as required.
3. Review generated charts in research/output.
