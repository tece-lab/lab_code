#!/bin/bash
#SBATCH --time=00:57:58 --partition=gelifes
project=sls
my_email=glaudanno@gmail.com
cd /home/$USER/$project/
mkdir -p results
mkdir -p data
mkdir -p logs

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

R_file_name=R-${project}-${lambda_m}-${mu_m}-${lambda_s}-${mu_s}-${cond}-${crown_age}-${shift_time}-${chosen_partition}.R
bash_file_name=bash-${project}-${s}-${lambda_m}-${mu_m}-${lambda_s}-${mu_s}-${cond}-${crown_age}-${shift_time}.bash
ml_name=ml-${project}-${s}-${lambda_m}-${mu_m}-${lambda_s}-${mu_s}-${cond}-${crown_age}-${shift_time}

chmod +x install_packages.bash
./install_packages.bash "Giappo/$project"

sleep 30

echo ".libPaths(new = file.path(substring(getwd(),1,13), 'Rlibrary')); library(\"$project\"); args <- as.numeric(commandArgs(TRUE))" > $R_file_name
echo "sls:::sls_main(seed=args[1],sim_pars=c(args[2],args[3],args[4],args[5]),cond=args[6],l_2 = sim_get_standard_l_2(crown_age = args[7],shift_time = args[8]),loglik_functions=sls_logliks_experiment())" >> $R_file_name

#args:
#1: $s = seed
#2: $lambda_m = Main clade speciation rate
#3: $mu_m = Main clade extinction rate
#4: $lambda_s = Subclade speciation rate
#5: $mu_s = Subclade extinction rate
#6: $cond = Conditioning
#7: $crown_age = Main clade starting time
#8: $shift_time = Subclade starting time

for((s = min_sims; s <= max_sims; s++)); do

echo "#!/bin/bash" > $bash_file_name
echo "#SBATCH --time=71:59:00" >> $bash_file_name
echo "module load R" >> $bash_file_name
echo "Rscript $R_file_name $s $lambda_m $mu_m $lambda_s $mu_s $cond $crown_age $shift_time" >> $bash_file_name
echo "rm $R_file_name" >> $bash_file_name
echo "rm $bash_file_name" >> $bash_file_name

#NEVER ASK FOR MORE THAN 9GB OF MEMORY!
sbatch --partition=$chosen_partition --mem=9GB --job-name=$ml_name --mail-type=FAIL,TIME_LIMIT --mail-user=$my_email --output=logs/$ml_name.log $bash_file_name

done

