# This file contains the functions used (sourced) by the main model script
# This file also reads in the parameters
# Author - C Bullen
# May 2022

# Packages
library(assertthat)
library(here)
library(tidyverse)
library(readxl)
library(dplyr)
library(EnvStats)
library(msm)
library(ggdist) # for raincloud plot

# Parameters -----
default_param_key <- read_excel(here("inputs", "model_params_v9.xlsx"),
                        skip = 1)

variables <- default_param_key %>% filter(parameter_type == "Variable") %>% pull(parameter_description)


# Monte Carlo sampler ----
mc_samp0 <- function(mu, sd, min, max, dist, uncertainty = TRUE) {
  assertthat::assert_that(all(dist %in% c("unif", "norm", "tri","tnorm", "NA") | is.na(dist)),
                          msg = "Error: Distribution must be one of 'unif', 'norm', 'tnorm', 'tri', or 'NA'.")
  
  # get sd
  sd <- ifelse(!is.na(sd), # use sd if available, otherwise calculate from max and min
               sd,
               (max - min) / 2 / 2)
  
  # get min
  mi <- ifelse(!is.na(min), # use available min, or calculate ~95% CI from sd
               min,
               mu - (2 * sd))
  
  # get max
  ma <- ifelse(!is.na(max), # use available max, or calculate ~95% CI from sd
               max,
               mu + (2 * sd))
  
  if(is.na(dist) | uncertainty == FALSE) {
    mu 
  }  else if(dist == "norm") {
    rnorm(mean = mu, sd = sd, n = 1)
  }  else if(dist == "tnorm") {
    rtnorm(mean = mu, sd = sd, n = 1, lower = 0)
  } else if (dist == "unif") {
    runif(min = mi, max = ma, n = 1)
  } else if (dist == "tri") {
    EnvStats::rtri(n = 1, min = mi, max = ma, mode = mu)
  } else {
    mu
  }
  
}

# Area-Weighted Spatial Parameters Function ========================

# This function uses the area for small, medium, and large farms to create an area-weighted average value for use in the model

A_wt_avg <- function(scenario, df = run_inputs) {
  

  # Depednding on scenario, calculate areas differently  
  if(scenario == "Build Out") {
    
    # Change area to only substrate and human use suitable
    df$A_shallow_near <- df$A_shallow_near * 0.1
    df$A_shallow_distant <- df$A_shallow_distant * 0.1
    df$A_deep_near <- df$A_deep_near  * 0.1
    df$A_deep_distant <- df$A_deep_distant * 0.1
    
  } else if(scenario == "Expanded") {
    
    # Change area to only substrate and human use suitable
    df$A_shallow_near <- df$A_shallow_near * 0.1
    df$A_shallow_distant <- 0
    df$A_deep_near <- df$A_deep_near * 0.1
    df$A_deep_distant <- 0

  } else if(scenario == "Pioneer") {
    
    # substrate and distance limit
    df$A_shallow_near <- df$A_shallow_near * 0.1
    df$A_shallow_distant <- 0
    df$A_deep_near <- 0
    df$A_deep_distant <- 0

  }
  
  # Calculate total area and weighted averages
  
  df$A <- df$A_shallow_near + df$A_shallow_distant + df$A_deep_near + df$A_deep_distant
  df$D_port <- (df$D_port_near * df$A_shallow_near +
                  df$D_port_near * df$A_deep_near +
                  df$D_port_distant * df$A_shallow_distant +
                  df$D_port_distant * df$A_deep_distant)/df$A
  df$D_sink <- (df$D_sink_shallow * df$A_shallow_near +
                  df$D_sink_deep * df$A_deep_near +
                  df$D_sink_shallow * df$A_shallow_distant +
                  df$D_sink_deep * df$A_deep_distant)/df$A
  df$Seq_mult <- (df$Seq_mult_shallow * df$A_shallow_near +
                  df$Seq_mult_deep * df$A_deep_near +
                  df$Seq_mult_shallow * df$A_shallow_distant +
                  df$Seq_mult_deep * df$A_deep_distant)/df$A
  df$Mat_life <- (df$Mat_life_shallow * df$A_shallow_near +
                    df$Mat_life_deep * df$A_deep_near +
                    df$Mat_life_shallow * df$A_shallow_distant +
                    df$Mat_life_deep * df$A_deep_distant)/df$A
  
  df

}

# Seaweed Production Model ------
# Model that calculates amount of biomass grown for a given area
seaweed_production <- function(df = run_inputs) {
  
  # Function
  area_h <- df$A * df$`C%_h` * 1000000 # horizontal area, convert km2 to m2
  area_v <- df$A * df$`C%_v` * 1000000 # vertical area, convert km2 to m2
  
  m_h <- area_h * df$`S%_m` * df$Ba_m_h * df$Hn
  m_v <- area_v * df$`S%_m` * df$Ba_m_v * df$Hn
  m <- m_h + m_v
  
  a_h <- area_h * df$`S%_a` * df$Ba_a_h * df$Hn
  a_v <- area_v * df$`S%_a` * df$Ba_a_v * df$Hn
  a <- a_h + a_v
  
  s_h <- area_h * df$`S%_s` * df$Ba_s_h * df$Hn
  s_v <- area_v * df$`S%_s` * df$Ba_s_v * df$Hn
  s <- s_h + s_v
  
  n_h <- area_h * df$`S%_n` * df$Ba_n_h * df$Hn
  n_v <- area_v * df$`S%_n` * df$Ba_n_v * df$Hn
  n <- n_h + n_v
  
  t_h <- m_h + a_h + s_h + n_h
  t_v <- m_v + a_v + s_v + n_v
  t <- t_h + t_v
  
  Biomass_df <- data.frame(Biomass = c(m, a, s, n, t), 
                           B_horizontal = c(m_h, a_h, s_h, n_h, t_h),
                           B_vertical = c(m_v, a_v, s_v, n_v, t_v),
                           Unit = "kg ww / year", 
                           Species = c("Macro", "Alaria", "Sacch", "Nereo", "Total"))
  return(Biomass_df)
  
}


# Seaweed Fate Model ------
seaweed_fate <- function(biomass_df, df = run_inputs) {
  
  
  # Calculate Total Biomass Produced (Total and by Species)
  Carbon_produced <- biomass_df |> 
    dplyr::filter(Species != "Total") |>
    dplyr::mutate(DryWeight_Conversion = dplyr::case_when(Species == "Macro" ~  df$W_D_m,
                                                          Species == "Sacch" ~  df$W_D_s,
                                                          Species == "Alaria" ~ df$W_D_a,
                                                          Species == "Nereo" ~  df$W_D_n),
                  carbon_content = dplyr::case_when(Species == "Macro" ~  df$CC_m,
                                                    Species == "Sacch" ~  df$CC_s,
                                                    Species == "Alaria" ~ df$CC_a,
                                                    Species == "Nereo" ~  df$CC_n),
                  Harvested_biomass_carbon = Biomass * DryWeight_Conversion * carbon_content,
                  NPP = Harvested_biomass_carbon / (1 - (df$POC_FL + df$DOC_FL)),
                  POC = NPP * df$POC_FL,
                  DOC = NPP * df$DOC_FL,
                  Unit = "kg C / year") %>%
    dplyr::select(-Biomass, - B_horizontal, - B_vertical)
  
  total <- data.frame(POC = sum(Carbon_produced$POC),
                      DOC = sum(Carbon_produced$DOC),
                      Harvested_biomass_carbon = sum(Carbon_produced$Harvested_biomass_carbon),
                      NPP = sum(Carbon_produced$NPP),
                      Unit = "kg C / year",
                      Species = "Total")
  
  Carbon_produced <- dplyr::bind_rows(Carbon_produced, total)
  
  return(Carbon_produced)
  
}

# Sequestration model -----
sequester <- function(biomass_df, Carbon_produced_df, df = run_inputs) {
  
  # Calculate CO2 sequestration / avoided from sinking and processing
  active_seq <-  biomass_df %>%
    dplyr::filter(Species != "Total") %>%
    dplyr::mutate(DryWeight_Conversion = dplyr::case_when(Species == "Macro" ~  df$W_D_m,
                                                          Species == "Sacch" ~  df$W_D_s,
                                                          Species == "Alaria" ~ df$W_D_a,
                                                          Species == "Nereo" ~  df$W_D_n),
                  carbon_content = dplyr::case_when(Species == "Macro" ~  df$CC_m,
                                                    Species == "Sacch" ~  df$CC_s,
                                                    Species == "Alaria" ~ df$CC_a,
                                                    Species == "Nereo" ~  df$CC_n),
                  
                  CO2_sequestered_sinking =  Biomass * DryWeight_Conversion * carbon_content * df$`Sink_deep%` *
                    df$F_seq_sink_deep * df$K_c_CO2 * df$K_c,
                  
                  CO2_sequestered_in_place = Biomass * DryWeight_Conversion * carbon_content *df$`Sink_shallow%` *
                    df$F_seq_sink_shallow * df$Seq_mult * df$K_c_CO2 * df$K_c,
                  
                  
                  CO2_avoided_food = Biomass * DryWeight_Conversion * df$`Proc%` * df$`Food%` * df$F_rep_food,
                  CO2_avoided_feed = Biomass * DryWeight_Conversion * df$`Proc%` * df$`Feed%` * df$F_rep_feed,
                  CO2_avoided_fertilizer = Biomass * DryWeight_Conversion * df$`Proc%` * df$`Fertilizer%` * df$F_rep_fertilizer,
                  CO2_avoided_fuel = Biomass * DryWeight_Conversion * df$`Proc%` * df$`Fuel%` * df$F_rep_fuel,
                  CO2_sequestered_biochar = Biomass * DryWeight_Conversion * df$K_c *
                    df$`Proc%` * df$`Biochar%` * df$F_seq_biochar,
                  
                  Unit = "kg CO2 / year") %>%
    select(- c(Biomass, B_horizontal, B_vertical))
  
  # Calculate totals
  total <- data.frame(CO2_sequestered_sinking = sum(active_seq$CO2_sequestered_sinking),
                      CO2_sequestered_in_place = sum(active_seq$CO2_sequestered_in_place),
                      CO2_avoided_food = sum(active_seq$CO2_avoided_food),
                      CO2_avoided_feed = sum(active_seq$CO2_avoided_feed),
                      CO2_avoided_fertilizer = sum(active_seq$CO2_avoided_fertilizer),
                      CO2_avoided_fuel = sum(active_seq$CO2_avoided_fuel),
                      CO2_sequestered_biochar = sum(active_seq$CO2_sequestered_biochar),
                      Unit = "kg CO2 / year",
                      Species = "Total")
  
  # Calculate CO2 sequestration from POC and DOC
  passive_seq <- Carbon_produced_df %>%
    dplyr::mutate(Carbon_sequestered_POC_DOC = POC * df$POC_seq * df$Seq_mult + DOC * df$DOC_seq * df$Seq_mult,
                  CO2_sequestered_POC_DOC = Carbon_sequestered_POC_DOC * df$K_c_CO2 * df$K_c) %>%
    select(Species, CO2_sequestered_POC_DOC)
  
  # Combine all into single DF
  Sequestration <- dplyr::bind_rows(active_seq, total) %>%
    full_join(passive_seq, by = "Species")
  
  return(Sequestration)
}

# Emissions model ----
emissions <- function(biomass_df, df = run_inputs){
  
  total_biomass <- biomass_df %>% filter(Species == "Total") %>% pull(Biomass)
  
  E_nurs <- (df$Nurs_nrg * df$A) * df$E_nrg
  E_cap <- df$E_mat * df$A / df$Mat_life
  E_mat_trans <- (df$M_mat * df$A) * df$E_barge * 2 * df$D_port
  E_maint <- (2 * df$D_port * df$E_ves * (df$N_maint/(df$A_maint / df$A))) + (df$A * df$D_maint * df$E_ves)
  E_seq <- (total_biomass * df$`Sink_deep%` * df$E_barge * df$D_sink) + (total_biomass * df$`Sink_deep%` * df$E_sink) # emissions from active sinking + transport
  E_sw_trans <- total_biomass * df$`Proc%` * df$D_port * df$E_barge
  E_proc <-  total_biomass * df$`Proc%` * df$E_conv
  Total_emissions <- E_nurs + E_cap + E_mat_trans + E_maint + E_seq + E_sw_trans + E_proc
  Unit <- "kg CO2 / year"
  
  
  data.frame(E_nurs = E_nurs,
             E_cap = E_cap,
             E_mat_trans = E_mat_trans,
             E_maint = E_maint,
             E_seq = E_seq,
             E_sw_trans = E_sw_trans,
             E_proc = E_proc,
             Total_emissions = Total_emissions,
             Unit = Unit)
  
  
}

# Net CO2 reduction model ----
net_co2 <- function(seq_df, emis_df, df = run_inputs) {
  
  # Get the total CO2 sequestered, accounting for non-equilibrium with atmosphere
  seq <- seq_df %>% filter(Species == "Total")
  seq_tot = ((seq$CO2_sequestered_sinking + seq$CO2_sequestered_in_place + seq$CO2_sequestered_POC_DOC)) +
    (seq$CO2_avoided_food + seq$CO2_avoided_feed + seq$CO2_avoided_fertilizer + seq$CO2_avoided_fuel + seq$CO2_sequestered_biochar)
  
  
  
  emi <- emis_df[["Total_emissions"]]
  
  net <- seq_tot - emi
  Unit <- "kg CO2 / year"
  
  data.frame(Total_CO2_sequestered = seq_tot,
             Net_CO2_reduction = net,
             Unit = Unit)
  
}

# Function to iterate over the model --------
iterate_model <- function(nsim = 1, scenario, input, ...) {
  
  assertthat::assert_that(nsim%%1 == 0)
  
  # Create the dataframes for the outputs
  # Model_run_inputs <- input[c("Parameter", "Parameter Description", "Units")]
  # Harvested_Biomass <- data.frame()
  # Carbon_Produced <- data.frame()
  # CO2_Sequestered <- data.frame()
  # CO2_Emissions <- data.frame()
  # Net_CO2_Reduction <- data.frame()
  
  # Create the dataframes for the outputs
  Model_run_inputs <- list() #
  Harvested_Biomass <- list()
  Carbon_Produced <- list()
  CO2_Sequestered <- list()
  CO2_Emissions <- list()
  Net_CO2_Reduction <- list()
  
  # For loop to iterate over the model
  for(i in 1:nsim){
    run <- paste("Run", i)
    
    run_inputs <- input %>%
      rowwise()%>%
      mutate(run_value = mc_samp0(mu = Value_Used, sd = SD_Used, min = `Minimum Value`,
                                  max = `Maximum Value`, dist = distribution)) %>% 
      select(Parameter, run_value) %>%
      pivot_wider(names_from = Parameter, values_from = run_value)
    
    run_inputs <- A_wt_avg(scenario = scenario, df = run_inputs)
    a <- seaweed_production(df = run_inputs)
    b <- seaweed_fate(biomass_df = a, df = run_inputs)
    c_out <- sequester(biomass_df = a, Carbon_produced_df = b, df = run_inputs)
    d <- emissions(biomass_df = a, df = run_inputs)
    e <- net_co2(seq_df = c, emis_df = d, df = run_inputs)
    
    Total_Area <- run_inputs$A
    
    
    run_inputs <- data.frame(as.numeric(run_inputs[1,]))
    names(run_inputs) <- run    
    a$Run = run
    b$Run = run
    c$Run = run
    d$Run = run
    e$Run = run
    
    Model_run_inputs[[i]] <- run_inputs
    Harvested_Biomass[[i]] <- a
    Carbon_Produced[[i]] <- b
    CO2_Sequestered[[i]] <- c
    CO2_Emissions[[i]] <- d
    Net_CO2_Reduction[[i]] <- e


}
  
  # Combine list items into dataframes
  Model_run_inputs <- cbind(input[c("Parameter", "Parameter Description", "Units")],
                            bind_cols(Model_run_inputs))
  Harvested_Biomass <- bind_rows(Harvested_Biomass)
  Carbon_Produced <- bind_rows(Carbon_Produced)
  CO2_Sequestered <- bind_rows(CO2_Sequestered)
  CO2_Emissions <- bind_rows(CO2_Emissions)
  Net_CO2_Reduction <- bind_rows(Net_CO2_Reduction)
  
  
  out_list <- list(Model_run_inputs, Harvested_Biomass, Carbon_Produced,
                   CO2_Sequestered, CO2_Emissions, Net_CO2_Reduction, Total_Area)
  names(out_list) <- c("Model_run_inputs", "Harvested_Biomass", "Carbon_Produced",
                       "CO2_Sequestered", "CO2_Emissions", "Net_CO2_Reduction", "Total_Area")
  return(out_list)
  }

# Function to iterate over the model FOR SENSITIVITY ANALYSIS --------
    # For sensitivity analysis, we allow the seed to be set within the for loop so that the same
    # input parameters are used for each sensitivity test
    # sensitivity_group argument is expected tobe one of the modules (e.g., production, emissions, sequestration)

iterate_model_sensitivity <- function(nsim = 1, scenario = scenario, sensitivity_group,
                                      input, seed, ...) {
  
  assertthat::assert_that(nsim%%1 == 0)
  assertthat::assert_that(nsim == length(seed))
  
  # Create the dataframes for the outputs
  Model_run_inputs <- input[c("Parameter", "Parameter Description", "Units")]
  Harvested_Biomass <- data.frame()
  Carbon_Produced <- data.frame()
  CO2_Sequestered <- data.frame()
  CO2_Emissions <- data.frame()
  Net_CO2_Reduction <- data.frame()
  
  # For loop to iterate over the model
  for(i in 1:nsim){
    run <- paste("Run", i)
    
    set.seed(seed[i])
    run_inputs <- input %>%
      rowwise()%>%
      mutate(run_value = mc_samp0(mu = Value_Used, sd = SD_Used, min = `Minimum Value`,
                                  max = `Maximum Value`, dist = distribution, ...)) %>% 
      # Here we set whether to use the randomised run_value or the default Value_Used, for each module
      mutate(run_value = case_when(sensitivity_group == "All" ~ run_value,
                                   sensitivity_group == "None" ~ Value_Used,
                                   sensitivity_group == module ~ run_value,
                                   sensitivity_group != module ~ Value_Used))%>%
      select(Parameter, run_value) %>%
      pivot_wider(names_from = Parameter, values_from = run_value)
    
    

    run_inputs <- A_wt_avg(scenario = scenario, df = run_inputs)
    a <- seaweed_production(df = run_inputs)
    b <- seaweed_fate(biomass_df = a, df = run_inputs)
    c <- sequester(biomass_df = a, Carbon_produced_df = b, df = run_inputs)
    d <- emissions(biomass_df = a, df = run_inputs)
    e <- net_co2(seq_df = c, emis_df = d, df = run_inputs)
    
    Total_Area <- run_inputs$A
    
    run_inputs <- data.frame(as.numeric(run_inputs[1,]))
    names(run_inputs) <- run    
    a$Run = run
    b$Run = run
    c$Run = run
    d$Run = run
    e$Run = run
    
    Model_run_inputs <- cbind(Model_run_inputs, run_inputs)
    Harvested_Biomass <- rbind(Harvested_Biomass, a)
    Carbon_Produced <- rbind(Carbon_Produced, b)
    CO2_Sequestered <- rbind(CO2_Sequestered, c)
    CO2_Emissions <- rbind(CO2_Emissions, d)
    Net_CO2_Reduction <- rbind(Net_CO2_Reduction, e)
    
  }
  
  
  
  out_list <- list(Model_run_inputs, Harvested_Biomass, Carbon_Produced, CO2_Sequestered, CO2_Emissions, Net_CO2_Reduction, Total_Area)
  names(out_list) <- c("Model_run_inputs", "Harvested_Biomass", "Carbon_Produced", "CO2_Sequestered", "CO2_Emissions", "Net_CO2_Reduction", "Total_Area")
  return(out_list)
}


# Function to iterate over the model FOR SENSITIVITY ANALYSIS OF SINGLE PARAMETERS --------
# For sensitivity analysis, we allow the seed to be set within the for loop so that the same
# input parameters are used for each sensitivity test

iterate_model_sensitivity_ind <- function(nsim = 1, scenario = scenario, sensitivity_group,
                                      input, seed, ...) {
  
  assertthat::assert_that(nsim%%1 == 0)
  assertthat::assert_that(nsim == length(seed))
  
  # Create the dataframes for the outputs
  Model_run_inputs <- input[c("Parameter", "Parameter Description", "Units")]
  Harvested_Biomass <- data.frame()
  Carbon_Produced <- data.frame()
  CO2_Sequestered <- data.frame()
  CO2_Emissions <- data.frame()
  Net_CO2_Reduction <- data.frame()
  
  # For loop to iterate over the model
  for(i in 1:nsim){
    run <- paste("Run", i)
    
    set.seed(seed[i])
    run_inputs <- input %>%
      rowwise()%>%
      mutate(run_value = mc_samp0(mu = Value_Used, sd = SD_Used, min = `Minimum Value`,
                                  max = `Maximum Value`, dist = distribution, ...)) %>%
      # Here we set whether to use the randomised run_value or the default Value_Used, for each module
      mutate(run_value = case_when(sensitivity_group == "All" ~ run_value,
                                   sensitivity_group == "None" ~ Value_Used,
                                   sensitivity_group == Parameter ~ run_value,
                                   sensitivity_group != Parameter ~ Value_Used))%>%
      select(Parameter, run_value) %>%
      pivot_wider(names_from = Parameter, values_from = run_value)
    
    
    
    run_inputs <- A_wt_avg(scenario = scenario, df = run_inputs)
    a <- seaweed_production(df = run_inputs)
    b <- seaweed_fate(biomass_df = a, df = run_inputs)
    c <- sequester(biomass_df = a, Carbon_produced_df = b, df = run_inputs)
    d <- emissions(biomass_df = a, df = run_inputs)
    e <- net_co2(seq_df = c, emis_df = d, df = run_inputs)
    
    Total_Area <- run_inputs$A
    
    run_inputs <- data.frame(as.numeric(run_inputs[1,]))
    names(run_inputs) <- run    
    a$Run = run
    b$Run = run
    c$Run = run
    d$Run = run
    e$Run = run
    
    Model_run_inputs <- cbind(Model_run_inputs, run_inputs)
    Harvested_Biomass <- rbind(Harvested_Biomass, a)
    Carbon_Produced <- rbind(Carbon_Produced, b)
    CO2_Sequestered <- rbind(CO2_Sequestered, c)
    CO2_Emissions <- rbind(CO2_Emissions, d)
    Net_CO2_Reduction <- rbind(Net_CO2_Reduction, e)
    
  }
  
  
  
  out_list <- list(Model_run_inputs, Harvested_Biomass, Carbon_Produced, CO2_Sequestered, CO2_Emissions, Net_CO2_Reduction, Total_Area)
  names(out_list) <- c("Model_run_inputs", "Harvested_Biomass", "Carbon_Produced", "CO2_Sequestered", "CO2_Emissions", "Net_CO2_Reduction", "Total_Area")
  return(out_list)
}



# Function to iterate over the model FOR SCENARIOS --------
# For scenario analysis, we allow the seed to be set within the for loop so that the same
# input parameters are used for each scenario

iterate_model_scenario <- function(nsim = 1, scenario = scenario,
                                      input, seed, ...) {
  
  assertthat::assert_that(nsim%%1 == 0)
  assertthat::assert_that(nsim == length(seed))
  
  # Create the dataframes for the outputs
  Model_run_inputs <- input[c("Parameter", "Parameter Description", "Units")]
  Harvested_Biomass <- data.frame()
  Carbon_Produced <- data.frame()
  CO2_Sequestered <- data.frame()
  CO2_Emissions <- data.frame()
  Net_CO2_Reduction <- data.frame()
  
  # For loop to iterate over the model
  for(i in 1:nsim){
    run <- paste("Run", i)
    
    set.seed(seed[i])
    run_inputs <- input %>%
      rowwise()%>%
      mutate(run_value = mc_samp0(mu = Value_Used, sd = SD_Used, min = `Minimum Value`,
                                  max = `Maximum Value`, dist = distribution, ...)) %>% 
      select(Parameter, run_value) %>%
      pivot_wider(names_from = Parameter, values_from = run_value)
    
    
    
    run_inputs <- A_wt_avg(scenario = scenario, df = run_inputs)
    a <- seaweed_production(df = run_inputs)
    b <- seaweed_fate(biomass_df = a, df = run_inputs)
    c <- sequester(biomass_df = a, Carbon_produced_df = b, df = run_inputs)
    d <- emissions(biomass_df = a, df = run_inputs)
    e <- net_co2(seq_df = c, emis_df = d, df = run_inputs)
    
    Total_Area <- run_inputs$A
    
    run_inputs <- data.frame(as.numeric(run_inputs[1,]))
    names(run_inputs) <- run    
    a$Run = run
    b$Run = run
    c$Run = run
    d$Run = run
    e$Run = run
    
    Model_run_inputs <- cbind(Model_run_inputs, run_inputs)
    Harvested_Biomass <- rbind(Harvested_Biomass, a)
    Carbon_Produced <- rbind(Carbon_Produced, b)
    CO2_Sequestered <- rbind(CO2_Sequestered, c)
    CO2_Emissions <- rbind(CO2_Emissions, d)
    Net_CO2_Reduction <- rbind(Net_CO2_Reduction, e)
    
  }
  
  
  
  out_list <- list(Model_run_inputs, Harvested_Biomass, Carbon_Produced, CO2_Sequestered, CO2_Emissions, Net_CO2_Reduction, Total_Area)
  names(out_list) <- c("Model_run_inputs", "Harvested_Biomass", "Carbon_Produced", "CO2_Sequestered", "CO2_Emissions", "Net_CO2_Reduction", "Total_Area")
  return(out_list)
}



# Function to combine results into version appropriate for plotting ----
combine_out <- function(out_df) {
  a <- out_df$Harvested_Biomass %>%
    pivot_longer(Biomass:B_vertical) %>%
    mutate(Model = "Production")
  b <- out_df$Carbon_Produced %>%
    pivot_longer(DryWeight_Conversion:DOC)%>%
    mutate(Model = "Production")
  c <- out_df$CO2_Sequestered %>%
    pivot_longer(DryWeight_Conversion:CO2_sequestered_POC_DOC)%>%
    mutate(Model = "Sequestration")
  d <- out_df$CO2_Emissions %>%
    pivot_longer(E_nurs:Total_emissions)%>%
    mutate(Model = "Emissions",
           Species = "Total")
  e <- out_df$Net_CO2_Reduction %>%
    pivot_longer(Total_CO2_sequestered: Net_CO2_reduction)%>%
    mutate(Model = "Sequestration",
           Species = "Total")
  
  rbind(a,b) %>% rbind(c) %>% rbind(d) %>% rbind(e)
  
}

# Function for unit conversions ----
convert_units <- function(df, unit_out, area, area_unit){
  assertthat::assert_that(all(unit_out %in% c("kg", "Tonne", "t", "Megatonne", "Mt", "Gigatonne", "Gt")),
                          msg = "Units must be one of kg, Tonne, Megatonne, or Gigatonne")
  assertthat::assert_that(all(area_unit %in% c("km2", "m2", "Hectare", "Ha", "Acre")),
                          msg = "Area Units must be one of km2, m2, Hectare, or Acre")
  
  # First, convert mass based units
  if(unit_out %in% c("Tonne", "t")) {
    df$value <- df$value * 0.001
    df$Unit <- gsub(x = df$Unit, pattern = "kg", replacement = "t")
  } else if(unit_out %in% c("Megatonne", "Mt")) {
    df$value <- df$value / 1000000000
    df$Unit <- gsub(x = df$Unit, pattern = "kg", replacement = "Mt")
  } else if(unit_out %in% c("Gigatonne", "Gt")) {
    df$value <- df$value / 1000000000000
    df$Unit <- gsub(x = df$Unit, pattern = "kg", replacement = "Gt")
  } else {
    # no change to value or unit
    df
  }
  
  #Then add columns for harvested biomass and area (for per biomass and per area plots)
  df <- df %>%
    filter(name == "Biomass" & Species == "Total") %>%
    select(Run, Biomass_Harvested = value, Biomass_Harvested_Unit = Unit) %>%
    mutate(Area_farmed = area, Area_farmed_Unit = "km2") %>%
    left_join(df, ., by = "Run")
  
  # Then convert area units if needed
  if(area_unit == "m2") {
    df$Area_farmed <- df$Area_farmed * 1000000
    df$Area_farmed_Unit <- "m2"
  } else if(area_unit %in% c("Hectare", "Ha")) {
    df$Area_farmed <- df$Area_farmed * 100
    df$Area_farmed_Unit <- "Hectare"
  } else if(area_unit == "Acre") {
    df$Area_farmed <- df$Area_farmed * 247.105
    df$Area_farmed_Unit <- "Acre"
  } else {
    # no change to value or unit
    df
  }
  
  return(df)
}



