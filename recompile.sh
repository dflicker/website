#! /bin/sh

ghc -O3 -Wall -o dtf dtf.hs && ./dtf rebuild && ./dtf watch
