#!/usr/bin/env bash
# This script kills all processes on port 8000 when watch_app is killed, and runs watch_app.
trap watch_app.sh kill $( lsof -i:8000 -t)
bash watch_app.sh