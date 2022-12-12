# About

This repository contains publicly-available data sourced from the Wisconsin Department of Public Instruction (DPI), along with R scripts to perform example analyses on the data.

These scripts require an R package developed by [City Forward Collective](https://www.cityforwardcollective.org/) called [`wisconsink12`](https://github.com/cityforwardcollective/wisconsink12), which can be installed with this command: `remotes::install_github("cityforwardcollective/wisconsink12")`.

The first step you will want to take is to run the script in `R/data_script.R`. This script will read in all the data and create an rda file for local use. Once you have completed this step, you won't have to run this script again.