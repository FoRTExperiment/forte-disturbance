# forte-disturbance

This repository contains the materials and results from a series of ED runs that apply different girdling treatments to a forest stand to explore resilience. 

## A.inputs 

The materials and scripts used to set up the ED runs on pic. See the A.inputs/READ-ME for more details. 

## B.ed 

The scripts that set up the various ED experiments on pic. 

## C.analysis 

This is a [drake workflow](https://github.com/ropensci/drake) workflow that uses `drake` for reproducible data processing, manipulation and visualization. To launch this analysis run `source(make.R)` This is the only porition of the project that can easily be run on a local machine. This is where most of the action takes place. 

## ED-outputs

The directory where the processed ED results should live and is also where the final processed ED results and 
calculated ecosystem stability metrics will be stored. 
