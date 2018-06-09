#!/usr/bin/env bash
ag -l | entr -d -s -r './run.sh'
