This model is structured into five separate R scripts (.R files), which are meant to be executed in the following order:

    1. MLE Theta Estimation Full Constraints
    2. State Definition Viterbi
    3. Beta Estimation
    4. MVOPT No States Monthly
    5. MVOPT States Monthly

Each script will create .rds files to store the results for later use. 
Additionally, the .rds files generated throughout this five-step process can be found in the "RDS Files" subfolder.

Please note that the paths in the read_xlsx() functions contain a placeholder labeled "ENTERPATH". 
To obtain a fully functioning program, you will need to adjust the paths according to the local file location.

All scripts contain successive duplications of the code, one for each sample period (20 and 30 years).
