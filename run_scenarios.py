import os

# Also for multiple scenario's, but without sensitivity analysis


# -----------------------------------------------------
# USER SETTINGS
# -----------------------------------------------------

yaml_file = "work-version.yaml"        # EMULSION model file
output_base = "outputs"   # where runs will be stored
folder_name = "all_scenarios"

n_rounds = 100

# Parameter values you want to test:
vaccination_scenario_list = [0, 0.5, 1]
introduction_time_list = [7, 14, 21, 28]
prop_bact_factor_list = [0.5, 1, 2]
early_slaughter_list = [0, 1]


# -----------------------------------------------------

os.makedirs(output_base, exist_ok=True)


n_models = len(vaccination_scenario_list) * len(introduction_time_list) * len(prop_bact_factor_list) * len(early_slaughter_list)
print("running", n_models, "models with", n_rounds, "rounds each")

model_n = 0

# default model
for vaccination_scenario in vaccination_scenario_list:
    for introduction_time in introduction_time_list:
        for prop_bact_factor in prop_bact_factor_list:
            for early_slaughter in early_slaughter_list:
                # Create output folder name: each scenario has its own numbered folder
                model_n += 1
                folder_name_model = f"{model_n}"
                output_dir = os.path.join(output_base, folder_name, folder_name_model)
                os.makedirs(output_dir, exist_ok=True)

                # Build the EMULSION command
                cmd = f'emulsion run {yaml_file} -r {n_rounds} -p vaccination_scenario={vaccination_scenario} -p introduction_time={introduction_time} -p prop_bact_factor={prop_bact_factor} -p early_slaughter={early_slaughter} --silent --output-dir {output_dir}'                   

                print("model:",model_n)
                os.system(cmd) # runs the emulsion command



print("\nAll runs complete!")
