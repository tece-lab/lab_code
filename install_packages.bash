# Code by Giovanni Laudanno @Giappo

#!/bin/bash

github_address=$1

chmod +x /home/$USER/Rlibrary/

echo "github_address = commandArgs(TRUE)" > Rsetup.R
echo "path = getwd()" >> Rsetup.R
echo "home_dir = substring(path,1,13)" >> Rsetup.R
echo "lib_dir = file.path(home_dir, 'Rlibrary')" >> Rsetup.R
echo "print(lib_dir)" >> Rsetup.R
echo ".libPaths(new = lib_dir)" >> Rsetup.R
echo "devtools::install_github(github_address)" >> Rsetup.R

echo "#!/bin/bash" > Rsetup2
echo "#SBATCH --time=00:59:00" >> Rsetup2
echo "path='pwd'" >> Rsetup2
echo "module load R/3.5.0-foss-2018a-X11-20180131" >> Rsetup2
echo "Rscript Rsetup.R $github_address" >> Rsetup2
echo "rm Rsetup.R" >> Rsetup2
echo "rm Rsetup2" >> Rsetup2

chmod +x Rsetup2
./Rsetup2
