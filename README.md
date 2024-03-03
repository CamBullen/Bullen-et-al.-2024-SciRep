# Bullen-et-al.-2024---SciRep
Code and data to accompany Bullen et al. 2024

Please direct any questions to the corresponding author Dr. Edward Gregr at ed[at]scitechconsulting.com

Description of files:

- Code
	- model_funs_and_params.R - R script to load data and define functions needed to run scenarios and sensitivity analyses.
	- scenario_and_sensitivity_analysis.R - R script to run scenario and sensitivity analyses.

- Inputs
	- SeaweedCarbon_model_inputs.csv - file containing the parameter values (including minimum, maximum, standard deviation, and uncertainty distribution where relevant).
	- economic_params.csv - file containing the additional parameters related to the economic analysis.

- Simulation Results
	- scenario_results.parquet - primary model results for simulations and scenarios reported in the manuscript
	- scenario_results_params.parquet - the model parameter values used for each Monte Carlo simulation of the scenario models.
	- model_sensitivity_results.parquet - results of sensitivity analysis at the scale of sub-models.
	- model_sensitivity_result_params.parquet - the model parameter values used for each Monte Carlo simulation of the sub-model sensitivity analysis.
	- individual_parameter_sensitivity_results.parquet - results of sensitivity analysis at the scale of individual parameters.
	- individual_parameter_sensitivity_result_params.parquet - the model parameter values used for each Monte Carlo simulation of the individual parameter sensitivity analysis.