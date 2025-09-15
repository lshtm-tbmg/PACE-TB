#------------------------------------------------------------------------------#
# PACE-TB - Epidemiological impact and cost-effectiveness plots                #
# Runs all calculations and plots                                              #
# Code by Katherine C. Horton (katherine.horton@lshtm.ac.uk)                   #
# Last updated 2025-06-11 by ASC                                               #
#------------------------------------------------------------------------------#

# Packages
suppressPackageStartupMessages({
  library(here)
})

# Run cost probabilities
source(here('scripts', 'prob_costs.R'))

# Run all epidemiological impacts
source(here('scripts', 'epi01_outputs.R'))
source(here('scripts', 'epi02_plotprep.R'))
#source(here('scripts', 'epi03_plots.R'))

# Run all cost-effectiveness analysis
source(here('scripts', 'outs_cea.R'))
source(here('scripts', 'cea01_pipeline.R'))
#source(here('scripts', 'cea02_plots.R'))