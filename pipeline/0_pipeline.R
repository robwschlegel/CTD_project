#############################################################################
###"pipeline/load_CTD.R"
## This script does:
# 1. Load CTD data and save
# 2. Interpolate CTD data and save
# 3.
## DEPENDS ON:
# Nothing. All of the scripts this sources load their own dependencies
## USED BY:
# This is the top most script in the pipeline
## CREATES:
# All of the outputs throughout the pipeline
#############################################################################


# 1. Load CTD data and save -----------------------------------------------

system.time(source("pipeline/load_CTD.R")) # 35 seconds


# 2. Interpolate CTD data and save ----------------------------------------

## NB: This is currently not advisable to run in one go
  ## The interpolation is dreadfully slow
# source("pipeline/2_interp_CTD.R")