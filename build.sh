#!/usr/bin/env bash

set -e

cd server
cabal new-build all
cd ../frontend
psc-package build
purs bundle "output/**/*.js" --module Main
