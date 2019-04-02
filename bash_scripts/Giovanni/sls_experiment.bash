#!/bin/bash
#SBATCH --time=00:57:58 --partition=gelifes
project=sls
my_email=glaudanno@gmail.com
cd /home/$USER/
mkdir -p $project
cd /home/$USER/$project/
mkdir -p results
mkdir -p data
mkdir -p logs

sim_min=1
sim_max=1000
crown_age=15

shift_time_vec=(5 10)
lambda_m_vec=(0.2 0.3 0.4 0.5)
mu_m_vec=(0.0 0.05 0.1 0.2)
lambda_s_vec=(0.6)
mu_s_vec=(0.1)

for shift_time in ${shift_time_vec[@]}; do
for lambda_m in ${lambda_m_vec[@]}; do
for mu_m in ${mu_m_vec[@]}; do
for lambda_s in ${lambda_s_vec[@]}; do
for mu_s in ${mu_s_vec[@]}; do

cond=0
experiment_name=exp-${project}-${lambda_m}-${mu_m}-${lambda_s}-${mu_s}-${cond}-${crown_age}-${shift_time}
sbatch --job-name=$experiment_name --mail-type=FAIL,TIME_LIMIT --mail-user=$my_email --output=logs/$experiment_name.log sls_main.bash $lambda_m $mu_m $lambda_s $mu_s $cond $crown_age $shift_time $sim_min $sim_max gelifes

cond=1
experiment_name=exp-${project}-${lambda_m}-${mu_m}-${lambda_s}-${mu_s}-${cond}-${crown_age}-${shift_time}
sbatch --job-name=$experiment_name --mail-type=FAIL,TIME_LIMIT --mail-user=$my_email --output=logs/$experiment_name.log sls_main.bash $lambda_m $mu_m $lambda_s $mu_s $cond $crown_age $shift_time $sim_min $sim_max regular

cond=2
experiment_name=exp-${project}-${lambda_m}-${mu_m}-${lambda_s}-${mu_s}-${cond}-${crown_age}-${shift_time}
sbatch --job-name=$experiment_name --mail-type=FAIL,TIME_LIMIT --mail-user=$my_email --output=logs/$experiment_name.log sls_main.bash $lambda_m $mu_m $lambda_s $mu_s $cond $crown_age $shift_time $sim_min $sim_max gelifes

cond=3
experiment_name=exp-${project}-${lambda_m}-${mu_m}-${lambda_s}-${mu_s}-${cond}-${crown_age}-${shift_time}
sbatch --job-name=$experiment_name --mail-type=FAIL,TIME_LIMIT --mail-user=$my_email --output=logs/$experiment_name.log sls_main.bash $lambda_m $mu_m $lambda_s $mu_s $cond $crown_age $shift_time $sim_min $sim_max regular

done
done
done
done
done
