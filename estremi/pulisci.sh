#!/bin/bash

find .  -name ".RData" -exec rm -rf {} \;
find .  -name "*.Rout" -exec rm -rf {} \;
find .  -name "*.csv" -exec rm -rf {} \;
find .  -name "*.html" -exec rm -rf {} \;
find .  -name "*.pdf" -exec rm -rf {} \;
