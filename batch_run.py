import os

# -----------------------------------------------------
# USER SETTINGS
# -----------------------------------------------------

yaml_file = "work-version.yaml"        # EMULSION model file
output_base = "batch_results_23_12_25"   # where runs will be stored

n_rounds = 20

# Parameter values you want to test:
introduction_time_list = [14, 21, 28]

beta_list = [0.5, 0.8, 1.5]
recovery_list = [0.2, 0.0833, 0.053]  # 5, 12, 19 days
t_until_bact_inf_list = [2, 5, 8]
#mortality_B_list = ['0.10/14', '0.40/14', '0.70/14']  # expected between 8-41%
prop_bact_factor_list = [0.5, 1, 2]
duration_reduced_growth_list = [7, 10]
bacterial_recovery_rate_list = ['1/6', '1/14'] # expert opinion 14 but literature 'fast recovery'??

treatment_trigger_list = [100]   # so without treatment. Threshold still needs to be determined!


# -----------------------------------------------------

os.makedirs(output_base, exist_ok=True)

n_models = len(introduction_time_list) * len(beta_list) *  len(recovery_list) * len(t_until_bact_inf_list) * len(prop_bact_factor_list) * len(duration_reduced_growth_list) * len(bacterial_recovery_rate_list)
print("running", n_models, "models with", n_rounds, "rounds each")

model_n = 0
for introduction_time in introduction_time_list:
    for beta in beta_list:
        for recovery in recovery_list:
            for t_until_bact_inf in t_until_bact_inf_list:
                for prop_bact_factor in prop_bact_factor_list:
                    for duration_reduced_growth in duration_reduced_growth_list:
                        for bacterial_recovery_rate in bacterial_recovery_rate_list:

                            # Create output folder name
                            model_n += 1
                            folder_name = f"{model_n}"
                            output_dir = os.path.join(output_base, folder_name)
                            os.makedirs(output_dir, exist_ok=True)
                            print("made output dir:", output_dir)

                            # Build the EMULSION command
                            cmd = f'emulsion run {yaml_file} -r {n_rounds} -p beta={beta} -p introduction_time={introduction_time} -p recovery={recovery} -p t_until_bact_inf={t_until_bact_inf} -p prop_bact_factor={prop_bact_factor} -p duration_reduced_growth={duration_reduced_growth} -p bacterial_recovery_rate={bacterial_recovery_rate} --silent --output-dir {output_dir}'                   

                            print("emulsion command built:",cmd)
                            os.system(cmd) # runs the emulsion command

print("\nAll runs complete!")
