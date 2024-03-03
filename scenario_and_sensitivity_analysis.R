# This file contains code to run scenarios and sensitivity analysis for the Seaweed Carbon model
# Author - C Bullen
# Sep 2022

# Packages
library(here)
library(ggplot2)
library(ggdist)
library(patchwork)
library(assertthat)
library(readxl)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggallin)
library(stringr)

# Sensitivity Analysis Toggles ----------


# Output units
area_u <- "km2"
mass_u <- "Mt"

# number of simulations
nsim = 10


# seeds for simulations
# One seed per simulation, so that simulations across scenarios and sensitivity analyses use the same values
set.seed(1)
seeds <- sample(1:9999999, size = nsim, replace = FALSE)

# Source functions and reading of input files -----
source(here("model_funs_and_params.R"))


# rename columns to match the processing that Shiny has to do ----
param_key <- default_param_key %>%
  rename(Original_Value = value,
         Original_SD = sd) %>%
  mutate(Value_Used = Original_Value,
         SD_Used = Original_SD,
         `Accessible to User` = "No",
         `Modified by User` = "No") %>%
  select(module, Parameter = parameter, `Parameter Description` = parameter_description, Units = units, distribution,
         Original_Value, Value_Used, Original_SD, SD_Used, `Minimum Value` = min, `Maximum Value` = max,
         `Accessible to User`, `Modified by User`, source, notes)

# testing <- iterate_model(nsim = 10, scenario = "Expanded", input = input)

# Run model by Scenario (Primary Results) ----------------------

production_scenarios <- c("Pioneer", "Pioneer", "Expanded", "Expanded", "Build Out")
fate_scenarios <- c("Pioneer", "Local Sequestration", "Realistic Future", "Optimistic Production", "All-in Techno Farming")


scenarios_out <- list()
model_params <- list()

for (i in 1:length(production_scenarios)) {

    scenario <- production_scenarios[i]
    fate_scenario <- fate_scenarios[i]
    
    if(fate_scenario == "Pioneer"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.889
      input$Value_Used[input$Parameter == "Feed%"] <- 0.111
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    } else if(fate_scenario == "Local Sequestration"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 1
      input$Value_Used[input$Parameter == "Proc%"] <- 0
      
    } else if(fate_scenario == "Realistic Future"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.667
      input$Value_Used[input$Parameter == "Feed%"] <- 0.222
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.111
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    } else if(fate_scenario == "Optimistic Production"){
      
      input <- param_key
      
      input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
      input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
      
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.667
      input$Value_Used[input$Parameter == "Feed%"] <- 0.222
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.111
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    }  else if(fate_scenario == "All-in Techno Farming"){
      input <- param_key
      input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
      input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
      
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0.5 
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0 
      input$Value_Used[input$Parameter == "Proc%"] <- 0.5  
      input$Value_Used[input$Parameter == "Food%"] <- 0.6
      input$Value_Used[input$Parameter == "Feed%"] <- 0.2
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.2
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    }
    
    
    model_out <- iterate_model_scenario(nsim = nsim,
                               scenario = scenario,
                               seed = seeds,
                               uncertainty = TRUE,
                               input = input)
    
    model_out_long <- combine_out(model_out) %>%
      convert_units(df = .,
                    unit_out = mass_u,
                    area = model_out$Total_Area,
                    area_unit = area_u)
    
    model_out_long$Production_Scenario <- scenario
    model_out_long$Fate_Scenario <- fate_scenario
    
    run_params <- model_out$Model_run_inputs %>%
      pivot_longer(4:ncol(model_out$Model_run_inputs))%>%
      mutate(Production_Scenario = scenario,
             Fate_Scenario = fate_scenario)
    
    
    scenarios_out[[i]] <- model_out_long
    model_params[[i]] <- run_params
    
    print(paste("Scenario ", i))
  

  
}

scenarios_results <- bind_rows(scenarios_out)
model_params <- bind_rows(model_params)



# Results for equal amounts of each possible fate (Table S2) -----------------------

fate_tests <- c("Deep Sink", "Shallow Sink", "Food", "Animal Feed", "Fuel",
                "Deep Sink - Optimized", "Shallow Sink - Optimized", "Food - Optimized",
                "Animal Feed - Optimized", "Fuel - Optimized")


scenarios_out <- list()
model_params <- list()

for (i in 1:length(fate_tests)) {
  
  scenario <- "Expanded"
  fate_scenario <- fate_tests[i]
  
  if(fate_scenario == "Deep Sink"){
    input <- param_key
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 1
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0
    input$Value_Used[input$Parameter == "Proc%"] <- 0
    input$Value_Used[input$Parameter == "Food%"] <- 0
    input$Value_Used[input$Parameter == "Feed%"] <- 0
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 0
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  } else if(fate_scenario == "Shallow Sink"){
    input <- param_key
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 1
    input$Value_Used[input$Parameter == "Proc%"] <- 0
    
  } else if(fate_scenario == "Food"){
    input <- param_key
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0
    input$Value_Used[input$Parameter == "Proc%"] <- 1
    input$Value_Used[input$Parameter == "Food%"] <- 1
    input$Value_Used[input$Parameter == "Feed%"] <- 0
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 0
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  } else if(fate_scenario == "Animal Feed"){
    
    input <- param_key
    
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0
    input$Value_Used[input$Parameter == "Proc%"] <- 1
    input$Value_Used[input$Parameter == "Food%"] <- 0
    input$Value_Used[input$Parameter == "Feed%"] <- 1
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 0
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  }  else if(fate_scenario == "Fuel"){
    input <- param_key
    
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0 
    input$Value_Used[input$Parameter == "Proc%"] <- 1
    input$Value_Used[input$Parameter == "Food%"] <- 0
    input$Value_Used[input$Parameter == "Feed%"] <- 0
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 1
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  } else if(fate_scenario == "Deep Sink - Optimized"){
    input <- param_key
    input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
    input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 1
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0
    input$Value_Used[input$Parameter == "Proc%"] <- 0
    input$Value_Used[input$Parameter == "Food%"] <- 0
    input$Value_Used[input$Parameter == "Feed%"] <- 0
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 0
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  } else if(fate_scenario == "Shallow Sink - Optimized"){
    input <- param_key
    input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
    input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 1
    input$Value_Used[input$Parameter == "Proc%"] <- 0
    
  } else if(fate_scenario == "Food - Optimized"){
    input <- param_key
    input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
    input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0
    input$Value_Used[input$Parameter == "Proc%"] <- 1
    input$Value_Used[input$Parameter == "Food%"] <- 1
    input$Value_Used[input$Parameter == "Feed%"] <- 0
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 0
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  } else if(fate_scenario == "Animal Feed - Optimized"){
    
    input <- param_key
    input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
    input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0
    input$Value_Used[input$Parameter == "Proc%"] <- 1
    input$Value_Used[input$Parameter == "Food%"] <- 0
    input$Value_Used[input$Parameter == "Feed%"] <- 1
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 0
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  }  else if(fate_scenario == "Fuel - Optimized"){
    input <- param_key
    input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
    input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
    input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
    input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
    input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
    input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0 
    input$Value_Used[input$Parameter == "Proc%"] <- 1
    input$Value_Used[input$Parameter == "Food%"] <- 0
    input$Value_Used[input$Parameter == "Feed%"] <- 0
    input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
    input$Value_Used[input$Parameter == "Fuel%"] <- 1
    input$Value_Used[input$Parameter == "Biochar%"] <- 0
    
  }
  
  model_out <- iterate_model_scenario(nsim = nsim,
                                      scenario = scenario,
                                      seed = seeds,
                                      uncertainty = TRUE,
                                      input = input)
  
  model_out_long <- combine_out(model_out) %>%
    convert_units(df = .,
                  unit_out = mass_u,
                  area = model_out$Total_Area,
                  area_unit = area_u)
  
  model_out_long$Production_Scenario <- scenario
  model_out_long$Fate_Scenario <- fate_scenario
  
  run_params <- model_out$Model_run_inputs %>%
    pivot_longer(4:ncol(model_out$Model_run_inputs))%>%
    mutate(Production_Scenario = scenario,
           Fate_Scenario = fate_scenario)
  
  
  scenarios_out[[i]] <- model_out_long
  model_params[[i]] <- run_params
  
  print(paste("Scenario ", i))
  
  
  
}

fate_test_results <- bind_rows(scenarios_out)
fate_test_model_params <- bind_rows(model_params)

# Results per area
fate_values_per_area <- fate_test_results %>% 
  filter(name %in% c("Net_CO2_reduction", "Total_emissions", "Total_CO2_sequestered")) %>%
  mutate(value = value / Area_farmed,
         value = value * 1000000,
         Unit = "T CO2 / km2 / year") %>%
  group_by(Fate_Scenario, Unit, Species, name, Model, Area_farmed_Unit)%>%
  summarise(N = length(value),
            median = median(value),
            mean = mean(value),
            min = min(value),
            max = max(value),
            Pct_2.5 = quantile(value, 0.025),
            Pct_25 = quantile(value, 0.25),
            Pct_75 = quantile(value, 0.75),
            Pct_97.5 = quantile(value, 0.975)) %>%
  pivot_longer(cols = N:Pct_97.5, names_to = "sum_stat") %>%
  pivot_wider(names_from = c(Fate_Scenario),
              values_from = value)

write_csv(fate_values_per_area, file = here("outputs","fate_specific_results_per_area_May2023.csv"))


#
# Sensitivity Analysis -----

sensitivity_groups <- c("All", "Production", "Sequestration", "Emissions", "None")
sensitivity_production_scenarios <- c("Pioneer", "Pioneer", "Expanded", "Expanded", "Build Out")
sensitivity_scenarios <-  c("Pioneer", "Local Sequestration", "Realistic Future", "Optimistic Production", "All-in Techno Farming")

sensitivity_out_interim <- list()
sensitivity_inputs_interim <- list()
sensitivity_out <- list()
sensitivity_inputs <- list()

# iterate over the modules
for(j in 1:length(sensitivity_scenarios)){
  for (i in 1:length(sensitivity_groups)) {

    scenario <- sensitivity_production_scenarios[j]
    fate_scenario <- sensitivity_scenarios[j]
    
    if(fate_scenario == "Pioneer"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.889
      input$Value_Used[input$Parameter == "Feed%"] <- 0.111
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    } else if(fate_scenario == "Local Sequestration"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 1
      input$Value_Used[input$Parameter == "Proc%"] <- 0
      
    } else if(fate_scenario == "Realistic Future"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.667
      input$Value_Used[input$Parameter == "Feed%"] <- 0.222
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.111
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    } else if(fate_scenario == "Optimistic Production"){
      
      input <- param_key
      
      input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
      input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
      
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.667
      input$Value_Used[input$Parameter == "Feed%"] <- 0.222
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.111
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    }  else if(fate_scenario == "All-in Techno Farming"){
      input <- param_key
      input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
      input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
      
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0.5 
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0 
      input$Value_Used[input$Parameter == "Proc%"] <- 0.5  
      input$Value_Used[input$Parameter == "Food%"] <- 0.6
      input$Value_Used[input$Parameter == "Feed%"] <- 0.2
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.2
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    }
    
      
  cur_module <- sensitivity_groups[i]
 
  model_out <- iterate_model_sensitivity(nsim = nsim,
                                         scenario = scenario,
                                         sensitivity_group = cur_module,
                                         seed = seeds,
                                         uncertainty = TRUE,
                                         input = input)
  
  model_out_long <- combine_out(model_out) %>%
    convert_units(df = .,
                  unit_out = mass_u,
                  area = model_out$Total_Area,
                  area_unit = area_u)
  
  model_out_long$Sensitivity_module <- cur_module
  model_out_long$Production_Scenario <- scenario
  model_out_long$Fate_Scenario <- fate_scenario
  
  sensitivity_out_interim[[i]] <- model_out_long
  
  
  run_params <- model_out$Model_run_inputs %>%
    pivot_longer(4:ncol(model_out$Model_run_inputs)) %>%
    mutate(Production_Scenario = scenario,
           Fate_Scenario = fate_scenario)
  
  sensitivity_inputs_interim[[i]] <- run_params
  
  
  print(paste("Sensitivity ", i, "-", j))
  
  
  }
  
  sensitivity_out[[j]] <- sensitivity_out_interim
  sensitivity_inputs[[j]] <- sensitivity_inputs_interim
}

sensitivity_results <- bind_rows(sensitivity_out)
sensitivity_inputs_all <- bind_rows(sensitivity_inputs)

# Standardize to "no" sensitivity
sensitivity_results <- sensitivity_results %>%
  filter(Sensitivity_module == "None") %>%
  select(Species, Run, Fate_Scenario, name, No_U_value = value) %>%
  right_join(sensitivity_results) %>%
  mutate(standardized_result = value / No_U_value)


# SINGLE PARAMETER Sensitivity Analysis -----

sensitivity_params <- input%>%
  filter(distribution != "NA",
         !Parameter %in% c("CC_m", "F_rep_fertilizer", "F_seq_biochar"),
         module %in% c("Production", "Sequestration", "Emissions")) %>% 
  pull(Parameter)


sensitivity_params <- c("All", "None", sensitivity_params)
sensitivity_production_scenarios <- c("Pioneer", "Pioneer", "Expanded", "Expanded", "Build Out")
sensitivity_scenarios <-  c("Pioneer", "Local Sequestration", "Realistic Future", "Optimistic Production", "All-in Techno Farming")


sensitivity_out_interim <- list()
sensitivity_inputs_interim <- list()
sensitivity_out <- list()
sensitivity_inputs <- list()


# iterate over the modules
for(j in 1:length(sensitivity_scenarios)){
  for (i in 1:length(sensitivity_params)) {
  
    scenario <- sensitivity_production_scenarios[j]
    fate_scenario <- sensitivity_scenarios[j]
    
    if(fate_scenario == "Pioneer"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.889
      input$Value_Used[input$Parameter == "Feed%"] <- 0.111
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    } else if(fate_scenario == "Local Sequestration"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 1
      input$Value_Used[input$Parameter == "Proc%"] <- 0
      
    } else if(fate_scenario == "Realistic Future"){
      input <- param_key
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.667
      input$Value_Used[input$Parameter == "Feed%"] <- 0.222
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.111
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    } else if(fate_scenario == "Optimistic Production"){
      
      input <- param_key
      
      input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
      input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
      
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0.1
      input$Value_Used[input$Parameter == "Proc%"] <- 0.9
      input$Value_Used[input$Parameter == "Food%"] <- 0.667
      input$Value_Used[input$Parameter == "Feed%"] <- 0.222
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.111
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    }  else if(fate_scenario == "All-in Techno Farming"){
      input <- param_key
      input$Value_Used[input$Parameter == "Ba_s_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_a_h"] <- 8.3
      input$Value_Used[input$Parameter == "Ba_n_h"] <- 8.3
      input$SD_Used[input$Parameter == "Ba_s_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_a_h"] <- 5.9
      input$SD_Used[input$Parameter == "Ba_n_h"] <- 5.9
      
      input$Value_Used[input$Parameter == "Sink_deep%"] <- 0.5 
      input$Value_Used[input$Parameter == "Sink_shallow%"] <- 0 
      input$Value_Used[input$Parameter == "Proc%"] <- 0.5  
      input$Value_Used[input$Parameter == "Food%"] <- 0.6
      input$Value_Used[input$Parameter == "Feed%"] <- 0.2
      input$Value_Used[input$Parameter == "Fertilizer%"] <- 0
      input$Value_Used[input$Parameter == "Fuel%"] <- 0.2
      input$Value_Used[input$Parameter == "Biochar%"] <- 0
      
    }
  
  cur_module <- sensitivity_params[i]
  
  model_out <- iterate_model_sensitivity_ind(nsim = nsim,
                                             scenario = scenario,
                                             sensitivity_group = cur_module,
                                             seed = seeds,
                                             uncertainty = TRUE,
                                             input = input)
  
  model_out_long <- combine_out(model_out) %>%
    convert_units(df = .,
                  unit_out = mass_u,
                  area = model_out$Total_Area,
                  area_unit = area_u)
  
  model_out_long$Sensitivity_param <- cur_module
  model_out_long$Sensitivity_module <- ifelse(cur_module == "All", "All",
                                               ifelse(cur_module == "None", "None",
                                                       input %>% filter(Parameter == cur_module) %>% pull(module)))
  model_out_long$Production_Scenario <- scenario
  model_out_long$Fate_Scenario <- fate_scenario
  
  sensitivity_out_interim[[i]] <- model_out_long
  
  
  
  run_params <- model_out$Model_run_inputs %>%
    pivot_longer(4:ncol(model_out$Model_run_inputs)) %>%
    mutate(Production_Scenario = scenario,
           Fate_Scenario = fate_scenario)
  
  sensitivity_inputs_interim[[i]] <- run_params
  
  
  print(paste("Sensitivity ", i, "-", j))
  
  
  }
  
  sensitivity_out[[j]] <- sensitivity_out_interim
  sensitivity_inputs[[j]] <- sensitivity_inputs_interim
}


sensitivity_results_ind <- bind_rows(sensitivity_out)
sensitivity_inputs_ind_all <- bind_rows(sensitivity_inputs)

# Standardize to "no" sensitivity
sensitivity_results_ind <- sensitivity_results_ind %>%
  filter(Sensitivity_module == "None") %>%
  select(Species, Run, Fate_Scenario, name, No_U_value = value) %>%
  right_join(sensitivity_results_ind) %>%
  mutate(standardized_result = value / No_U_value)


#

# Rename parameters and Scenarios for plotting ------

scenarios_results <- scenarios_results %>%
  mutate(name = case_when(name == "CO2_sequestered_in_place" ~ "On-shelf disposal",
                          name == "CO2_sequestered_sinking" ~ "Deep disposal",
                          name == "CO2_sequestered_POC_DOC" ~ "Passive sequestration",
                          name == "CO2_avoided_food" ~ "Food products",
                          name == "CO2_avoided_feed" ~ "Animal feed",
                          name == "CO2_avoided_fertilizer" ~ "Fertilizer",
                          name == "CO2_avoided_fuel" ~ "Fuel products",
                          name == "CO2_sequestered_biochar" ~ "Biochar ",
                          name == "Total_CO2_sequestered" ~ "Total CO2 sequestered or avoided",
                          
                          name == "E_cap" ~ "Equipment",
                          name == "E_maint" ~ "Maintenance",
                          name == "E_mat_trans" ~ "Material transport",
                          name == "E_nurs" ~ "Nursery",
                          name == "E_proc" ~ "Product processing",
                          name == "E_seq" ~ "Deep sequestration",
                          name == "E_sw_trans" ~ "Seaweed transport",
                          name == "Total_emissions" ~ "Total emissions",
                          name == "Net_CO2_reduction" ~ "Net reduction in CO2",
                          TRUE ~ name))

sensitivity_results <- sensitivity_results %>%
  mutate(name = case_when(name == "CO2_sequestered_in_place" ~ "On-shelf disposal",
                          name == "CO2_sequestered_sinking" ~ "Deep disposal",
                          name == "CO2_sequestered_POC_DOC" ~ "Passive sequestration",
                          name == "CO2_avoided_food" ~ "Food products",
                          name == "CO2_avoided_feed" ~ "Animal feed",
                          name == "CO2_avoided_fertilizer" ~ "Fertilizer",
                          name == "CO2_avoided_fuel" ~ "Fuel products",
                          name == "CO2_sequestered_biochar" ~ "Biochar ",
                          name == "Total_CO2_sequestered" ~ "Total CO2 sequestered or avoided",
                          
                          name == "E_cap" ~ "Equipment",
                          name == "E_maint" ~ "Maintenance",
                          name == "E_mat_trans" ~ "Material transport",
                          name == "E_nurs" ~ "Nursery",
                          name == "E_proc" ~ "Product processing",
                          name == "E_seq" ~ "Deep sequestration",
                          name == "E_sw_trans" ~ "Seaweed transport",
                          name == "Total_emissions" ~ "Total emissions",
                          name == "Net_CO2_reduction" ~ "Net reduction in CO2",
                          TRUE ~ name))

# Rename Scenarios
scenarios_results <- scenarios_results %>%
  mutate(Fate_Scenario = case_when(Fate_Scenario == "Local Sequestration" ~ "Local - No Harvest",
                                   Fate_Scenario == "Pioneer" ~ "Local - Products",
                                   Fate_Scenario == "Realistic Future" ~ "Expanded",
                                   Fate_Scenario == "Optimistic Production" ~ "Expanded - Optimized",
                                   Fate_Scenario == "All-in Techno Farming" ~ "Techno Industrial"),
         Fate_Scenario = factor(Fate_Scenario, levels = c("Local - No Harvest", "Local - Products",
                                                          "Expanded", "Expanded - Optimized",
                                                          "Techno Industrial")))
           
sensitivity_results <- sensitivity_results %>%
  mutate(Fate_Scenario = case_when(Fate_Scenario == "Local Sequestration" ~ "Local - No Harvest",
                                   Fate_Scenario == "Pioneer" ~ "Local - Products",
                                   Fate_Scenario == "Realistic Future" ~ "Expanded",
                                   Fate_Scenario == "Optimistic Production" ~ "Expanded - Optimized",
                                   Fate_Scenario == "All-in Techno Farming" ~ "Techno Industrial"),
         Fate_Scenario = factor(Fate_Scenario, levels = c("Local - No Harvest", "Local - Products",
                                                          "Expanded", "Expanded - Optimized",
                                                          "Techno Industrial")))

sensitivity_results_ind <- sensitivity_results_ind %>%
  mutate(Fate_Scenario = case_when(Fate_Scenario == "Local Sequestration" ~ "Local - No Harvest",
                                   Fate_Scenario == "Pioneer" ~ "Local - Products",
                                   Fate_Scenario == "Realistic Future" ~ "Expanded",
                                   Fate_Scenario == "Optimistic Production" ~ "Expanded - Optimized",
                                   Fate_Scenario == "All-in Techno Farming" ~ "Techno Industrial"),
         Fate_Scenario = factor(Fate_Scenario, levels = c("Local - No Harvest", "Local - Products",
                                                          "Expanded", "Expanded - Optimized",
                                                          "Techno Industrial")))

