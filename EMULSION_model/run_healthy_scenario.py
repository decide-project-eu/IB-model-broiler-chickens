import os

# -----------------------------------------------------
# USER SETTINGS
# -----------------------------------------------------

yaml_file = "work-version.yaml"
output_base = "outputs"

n_rounds = 100

os.makedirs(output_base, exist_ok=True)

folder_name = "healthy_scenario"
output_dir = os.path.join(output_base, folder_name)
os.makedirs(output_dir, exist_ok=True)

# Build the EMULSION command
cmd = f'emulsion run {yaml_file} -r {n_rounds} -p initial_infected=0 --silent --output-dir {output_dir}'
os.system(cmd) # runs the emulsion command

