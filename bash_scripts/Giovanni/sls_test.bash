#!/bin/bash
#SBATCH --time=00:57:58 --partition=gelifes
project=sls
my_email=glaudanno@gmail.com

lambda_m=$1
mu_m=$2
lambda_s=$3
mu_s=$4
cond=$5
crown_age=$6
shift_time=$7
min_sims=$8
max_sims=$9
chosen_partition=${10}

echo $lambda_m
echo $mu_m
echo $lambda_s
echo $mu_s
echo $cond
echo $crown_age
echo $shift_time
echo $min_sims
echo $max_sims
echo $chosen_partition
echo ${chosen_partition}_${lambda_m}