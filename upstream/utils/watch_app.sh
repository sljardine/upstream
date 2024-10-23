#!/bin/bash -eu

# This script watches for changes in the "R" directory. When changes are found, it kills any running processes on port 8000 and then re-runs the run_dev.R script to restart the app.
find ../R | entr sh -c 'lsof -i:8000 | awk '"'"'NR!=1 {print $2}'"'"' | xargs kill ; (cd .. && Rscript ./dev/run_dev.R &)'