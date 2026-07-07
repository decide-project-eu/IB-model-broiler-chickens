import os

# This is a sensitivity analysis of changing one variable at a time in the default scenario.
# Assumption that the default scenario is already run in 'run_scenarios' so this will not be repeated.


# -----------------------------------------------------
# USER SETTINGS
# -----------------------------------------------------

yaml_file = "work-version.yaml"        # EMULSION model file
output_base = "outputs"   # where runs will be stored
folder_name = "sens_analysis"

n_rounds = 100

# Default scenario
vaccination_scenario = 0
introduction_time = 14
prop_bact_factor = 1
early_slaughter = 0

# Sensitivity analysis parameters (each 2x lower and higher)
beta_factor_list = [0.5, 2]
recovery_factor_list = [0.5, 2]
prop_clinical_list = [0.8, 0.95]   # this is strictly testing 2x difference in proportion subclinical: 5% or 20%
mortality_I_list = [0.000725, 0.001885]   # = 1x (!) and 2.6x normal mort
mortality_B_list = [0.0142, 0.0571] # = 20x or 80x normal mort
time_before_reduced_growth_list = [1, 4]
duration_reduced_growth_list = [4, 16]
t_until_bact_inf_list = [3, 10]
length_B_list = [7, 28]

# Note: growth factor and condemnation should be changed in R script.


# -----------------------------------------------------

os.makedirs(output_base, exist_ok=True)


model_n = 0

# default model
for beta_factor in beta_factor_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p beta_factor={beta_factor} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command

for recovery_factor in recovery_factor_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p recovery_factor={recovery_factor} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command

for prop_clinical in prop_clinical_list:
    # ! this changes prop_clinical_base in the model

    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p prop_clinical_base={prop_clinical} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command

for mortality_I in mortality_I_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p mortality_I={mortality_I} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command
    
for mortality_B in mortality_B_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p mortality_B={mortality_B} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command

for time_before_reduced_growth in time_before_reduced_growth_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p time_before_reduced_growth={time_before_reduced_growth} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command

for duration_reduced_growth in duration_reduced_growth_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p duration_reduced_growth={duration_reduced_growth} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command

for t_until_bact_inf in t_until_bact_inf_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p t_until_bact_inf={t_until_bact_inf} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command

for length_B in length_B_list:
   
    # Create output folder name: each scenario has its own numbered folder
    model_n += 1
    folder_name_model = f"{model_n}"
    output_dir = os.path.join(output_base, folder_name, folder_name_model)
    os.makedirs(output_dir, exist_ok=True)

    # Build the EMULSION command
    cmd = f'emulsion run {yaml_file} -r {n_rounds} -p length_B={length_B} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

    print("model:",model_n)
    os.system(cmd) # runs the emulsion command


print("\nAll runs complete!")
