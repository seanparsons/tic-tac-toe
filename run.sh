#!/usr/bin/env bash

set -e

./build.sh
cd ./project-builder/_build
./ttt-server
