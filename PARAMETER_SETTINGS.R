#-------------------------------------------------------------------------------
# Name Variables in baseline VAR model
#-------------------------------------------------------------------------------

#1. Reduced-form model:
y_names <- c("BD","CPI","IP","UNEMP","EBP","TREAS")
N_vars <- length(y_names) # Number of endogenous variables

#2. Structural model:
N_m <- 1                  # Number of forecast variables.
N_c <- (N_vars-N_m)  #8#5 # Number of contemporaneous variables.

#3. Monetary policy shocks:
z_names <- c("FFR","FG","LSAP") # Get variable names.

#-------------------------------------------------------------------------------
## Set parameters for baseline results:
#-------------------------------------------------------------------------------

irf_periods <- 1000     # Select number of periods to produce IRF. (can be greater than IRF periods on plots).
welf_horizon <- 60      # Set Welfare horizon (ie, set truncation length for welfare objective function).
slct_ci <- 0.95         # Select confidence intervals to show up on plots.
slct_ci_2 <- 0.90       # Select a second choice of confidence intervals to show up on plots.
slct_bootruns <- 100    # Select number of bootstrap replications.

# IMPORTANT: Un-select one of the following to replicate exactly the paper's results:
# slct_bootruns <- 10000 # Iterations for Baseline Results
# slct_bootruns <- 1000 # Iterations for Robustness Results

N_lags <- 7             # Set number of lags.
N_lags_rat_reg <- 4     # Set number of lags on Rational Expectations regression.
slct_lambda <- 1/2      # Select lambda coefficient (weights on unemployment).

# Monetary Policy shocks:
slct_mpshocktypes <- c("bauer_swanson")

# Set seed to generate random numbers:
set.seed(1000)          # Set seed to 1000 to replicate the paper's results.

#-------------------------------------------------------------------------------
# Bootstrap Settings
#-------------------------------------------------------------------------------

bootstrap_list = c("omega_fixed","irf_mp_fixed")
bootstrap_method = bootstrap_list[1]

#-------------------------------------------------------------------------------
## Adjust parameters for robustness tests (if needed):
#-------------------------------------------------------------------------------

if( slct_robust=="lambda_0"){
  slct_lambda <- 0                      # Robustness Test #2 of Table 2.
}

if( slct_robust=="lambda_1"){
  slct_lambda <- 1                      # Robustness Test #3 of Table 2.
}

if( slct_robust=="lags_3"){
  N_lags <- 3                           # Robustness Test #4 of Table 2.
}

if( slct_robust=="lags_12"){ 
  N_lags <- 12                          # Robustness Test #5 of Table 2.
}

if( slct_robust=="lags_rat_12"){
  N_lags_rat_reg <- 12                  # Robustness Test #6 of Table 2.
}

if( slct_robust=="welf_horizon_24"){
  welf_horizon <- 24                    # Robustness Test #8 of Table 2.
}

if( slct_robust=="welf_horizon_120"){
  welf_horizon <- 120                   # Robustness Test #9 of Table 2.
}

if( slct_robust=="aruoba_mps"){
  slct_mpshocktypes <- c("aruoba_mps")  # Robustness Test #11 of Table 2.
}

if( slct_robust=="small_var"){          # Robustness Test #10 of Table 2.
  y_names <- c("BD","CPI","UNEMP","TREAS")  # Parsimonious VAR model.
  N_vars <- length(y_names)                 # Number of endogenous variables.
  N_c <- (N_vars-N_m)                       # Number of contemporaneous variables.
}

#-------------------------------------------------------------------------------
# Cosmetics:
#-------------------------------------------------------------------------------

slct_dcml <- 3          #Select decimal digits to round results on output tables.
irf_periods_plot <- 48  #irf_periods - 12 
#> Note: On structural methodology the IRF for BD on last 12 periods are unidentified ...
#> ... because we need the Rational Expectations 1Y ahead.

## Convert shortcut names to full names:
#1. Endogenous VAR variables:
y_dnames <- y_names # Assign names to the endogenous variables.
y_dnames[which((y_dnames)=="IP")] <- "Industrial Production"
y_dnames[which((y_dnames)=="CPI")] <- "Consumer Price Index"
y_dnames[which((y_dnames)=="PPI")] <- "Producer Price Index"
y_dnames[which((y_dnames)=="TREAS")] <- "2Y Treasury"
y_dnames[which((y_dnames)=="EBP")] <- "Excess Bond Premium"
y_dnames[which((y_dnames)=="UNEMP")] <- "Unemployment"
y_dnames[which((y_dnames)=="FCST_CPI")] <- "Forecast CPI"
y_dnames[which((y_dnames)=="BD")] <- "Belief Distortions" #"Sentiment"
y_dnames[which((y_dnames)=="TREAS_10Y")] <- "10Y Treasury"
y_dnames[which((y_dnames)=="SHADOW_FFR")] <- "Shadow FFR"

#2. Exogenous MP shocks:
z_dnames <- z_names                     # Baseline Results: Assign names to the exogenous MP variables.
z_dnames[which((z_dnames)=="FFR")] <- "Target Rate"
z_dnames[which((z_dnames)=="FG")] <- "Forward Guidance"
z_dnames[which((z_dnames)=="LSAP")] <- "Large Scale Asset Purchases"

if( slct_robust=="aruoba_mps"){         # Robustness Test #11 of Table 2: Assign name to the exogenous MP variable.
  z_names <- c("FFR")                   # Give shortcut name to the exogenous variable.
  z_dnames <- "Aruoba-Drechsel Shock"   # Give full name to the exogenous variable.
}
