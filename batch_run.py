import os

# -----------------------------------------------------
# USER SETTINGS
# -----------------------------------------------------

yaml_file = "sep-bact-state.yaml"        # your EMULSION model file
output_base = "batch_results"   # where runs will be stored

n_rounds = 10

# Parameter values you want to test:
beta_list = [0.7, 0.8, 0.9]
introduction_time_list = [21, 28]
recovery_list = [0.0625, 0.0714, 0.0833]
t_until_bact_inf_list = [3, 5, 7]
mortality_I2_list = [0.00213, 0.0071]
treatment_trigger_list = [4]

# -----------------------------------------------------

os.makedirs(output_base, exist_ok=True)

n_models = len(beta_list) * len(introduction_time_list) * len(recovery_list) * len(t_until_bact_inf_list) * len(treatment_trigger_list)
print("running", n_models, "models with", n_rounds, "rounds each")

model_n = 0
for beta in beta_list:
    for introduction_time in introduction_time_list:
        for recovery in recovery_list:
            for t_until_bact_inf in t_until_bact_inf_list:
                for mortality_I2 in mortality_I2_list:
                    for treatment_trigger in treatment_trigger_list:                

                        # Create output folder name
                        model_n += 1
                        folder_name = f"{model_n}"
                        output_dir = os.path.join(output_base, folder_name)
                        os.makedirs(output_dir, exist_ok=True)
                        print("made output dir:", output_dir)

                        # Build the EMULSION command
                        cmd = f'emulsion run {yaml_file} -r {n_rounds} -p beta={beta} -p introduction_time={introduction_time} -p recovery={recovery} -p t_until_bact_inf={t_until_bact_inf} -p mortality_I2={mortality_I2} -p treatment_trigger={treatment_trigger} --silent --output-dir {output_dir}'                   

                        print("emulsion command built:",cmd)
                        os.system(cmd) # runs the emulsion command

print("\nAll runs complete!")