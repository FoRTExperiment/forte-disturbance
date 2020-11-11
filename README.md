# forte-disturbance

This repository contains the materials and results from a series of ED runs that apply different girdling treatments to a forest stand to explore resilience. 

## A.inputs 

The materials and scripts used to set up the ED runs on pic. See the A.inputs/READ-ME for more details. 

## B.ed 

The scripts that set up the various ED experiments on pic. 

## C.analysis 

This is a [drake workflow](https://github.com/ropensci/drake) workflow that uses `drake` for reproducible data processing, manipulation and visualization. To launch this analysis run `source(make.R)` This is the only porition of the project that can easily be run on a local machine but it requires that the ED-outputs were downloaded properly,TODO figure out how archive/distribute the results.

