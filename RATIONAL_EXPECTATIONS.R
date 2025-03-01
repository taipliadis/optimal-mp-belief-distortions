
## Merge realized CPI data with macro variables based on forecast announcement date:

controls_rat <- y_names[2:length(y_names)]
controls_rat <- c(controls_rat,"TREAS_10Y","SHADOW_FFR","PPI")

dt_forecast <- merge(dt_cpi_actual,y_data[,mget( c("ANNDATE","FCST_CPI",controls_rat))],by=c("ANNDATE"),all=FALSE)  # Merge forecasts with macro variables.
setorder(dt_forecast,ANNDATE)   # Sort dates in ascending order.

# Notes on "dt_forecast":
#> CPI_ACTUAL is the year-ahead realized cpi (in log form x 100): CPI_{t+h}.
#> FCST_CPI is the time-t implied forecast of t+h cpi (in log form x 100).
#> CPI is the time-t reported cpi (in log form x 100).

rm(dt_cpi_actual)               # Remove unnecessary data.

## Estimate reduced-form Belief Distortion:
lag_vars_names <- c("FCST_CPI",controls_rat)

# Create lags of all explanatory variables:

for (var_name in lag_vars_names) {
  lag_vars <- as.data.table(shift(dt_forecast[[var_name]], n = 1:N_lags_rat_reg, type = "lag")) # Create N=4 lags of variable "var_name".
  setnames(lag_vars, paste0(var_name, "_lag", 1:N_lags_rat_reg)) # Name the new lagged columns
  dt_forecast <- cbind(dt_forecast, lag_vars) # Add lagged variables to "dt_forecast".
  rm(lag_vars)
}

dt_forecast <- na.omit(dt_forecast) # Remove observations with NA values introduced by lags.
init_date <- dt_forecast[,.SD[1]]$ANNDATE # Get the initial date from the time series.

## Prepare data as time-series to run the OLS regression:

ts_forecast <- ts(dt_forecast,start = init_date ,frequency = 12) # Convert data table to time series.

## Define all explanatory variables:
explanatory_vars <- colnames(ts_forecast)[!colnames(ts_forecast) %in% c("ANNDATE","CPI_ACTUAL")]

## Run OLS regression:

formula_reg <- paste("CPI_ACTUAL ~ ",paste( explanatory_vars, collapse = " + ")) # Type the regression formula.

reg_rat <- lm(formula_reg,data = ts_forecast) # Run the (OLS) Rational-Expectations regression.

summary(reg_rat) # Print regression results. # Adjusted R-squared: 99.88%

#-------------------------------------------------------------------------------
## Estimate Rational Expectations and Belief Distortions:
#-------------------------------------------------------------------------------

fitted_vals <- reg_rat$fitted.values          # Get the fitted values (proxy for "Rational Expectations").
dt_forecast <- cbind(dt_forecast,fitted_vals) # Add Ex-post Rational Expectations to the data.
setnames(dt_forecast,"fitted_vals","RE")      # Name the rational expectations variable.
dt_forecast[,BD:=FCST_CPI-RE]                 # Estimate belief distortions.

# Notes on "dt_forecast":
#> RE: (Ex-post) Rational Expectations of 1-year-ahead CPI.
#> BD: (Ex-post) Belief Distortions of 1-year-ahead (implied) CPI forecasts.

rm(fitted_vals,init_date,formula_reg,N_lags_rat_reg,lag_vars_names,var_name,explanatory_vars,ts_forecast) # Remove unnecesary variables.

# Keep the relevant data:
y_data <- merge(y_data[,mget( c("ANNDATE","FCST_CPI",controls_rat))],dt_forecast[,.(ANNDATE,BD)],by=c("ANNDATE"),all=TRUE)
y_data <- y_data[,mget( c("ANNDATE","FCST_CPI",y_names[2:length(y_names)],y_names[1]) )]