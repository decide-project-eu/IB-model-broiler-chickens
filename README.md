# Model of infectious bronchitis (IB) in a broiler flock

This model is part of Work Package 4 of the DECIDE Project.
The model is based on the EMULSION framework (Picault et al., 2019).

## Authors

- Yara Slegers, Utrecht University, The Netherlands
- Sébastien Picault, INRAE, France

## Description

The model simulates an outbreak of IB within a broiler flock. It is based on a typical infection in Dutch conventional broiler flocks. Secondary bacterial infections are also taken into account.
Different scenarios will be compared, including no vaccination and vaccination against the field strain. Currently, only the default model without any vaccination is presented.

## How to run the models

First, you need to install EMULSION. For more information see [here](https://sourcesup.renater.fr/www/emulsion-public/pages/Install.html).

The EMULSION model as specified in the [YAML file](sep-bact-state.yaml) can be run directly from the command prompt. Alternatively, the [batch_run.py](batch_run.py) script can be used to run multiple scenarios at once.

Broiler growth and feed intake is not implemented within the EMULSION model; instead, an [R script](output_processed-G.R) is used to calculate daily growth, flock weight and feed use afterwards, based on the number of chickens with reduced growth at each time step (day).

## References

S. Picault, Y.-L. Huang, V. Sicard, S. Arnoux, G. Beaunée, P. Ezanno (2019). “EMULSION: Transparent and flexible multiscale stochastic models in human, animal and plant epidemiology”, PLoS Computational Biology 15(9): e1007342.




