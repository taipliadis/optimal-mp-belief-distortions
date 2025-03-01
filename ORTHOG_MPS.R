################################################################################
# ORTHOGONALIZE MONETARY POLICY INSTRUMENTS
################################################################################
# Info:
#> Regress each MP shock on 1 lag of macro variables.
#> The residuals of these regressions are the orthogonalized instruments.
#> We do not include any constant and we only regress time-series on months when
#> there were monetary policy instruments available ... 
#> (i.e., shocks with 0 value were N/A values and we need to exclude them).
#-------------------------------------------------------------------------------

temp_exp_vars <- c("FCST_CPI",y_names[2:length(y_names)]) # Name the control variables in orthogonalization regression.

z_data_temp <- merge(z_data,y_data[,mget( c("ANNDATE","FCST_CPI",y_names[2:length(y_names)]))],by="ANNDATE",all = TRUE)

# Create lags for explanatory variables:
for (var_name in temp_exp_vars){
  lag_vars <- as.data.table(shift(z_data_temp[[var_name]], n = 1, type = "lag"))
  setnames(lag_vars, paste0(var_name, "_lag", 1)) # Name the lagged variables.
  z_data_temp <- cbind(z_data_temp, lag_vars) # Add lagged variables to "z_data_temp".
  rm(lag_vars)
}

z_data_temp <- z_data_temp[is.na(FFR)!=TRUE] # Remove observations when there is no MP instrument.

# In this for-loop we regress each Monetary Policy instrument separately:
for (var_name in z_names){
  formula_z <- paste0(var_name," ~ 0 + ", paste( paste0(temp_exp_vars,"_lag1"), collapse = " + "))
  reg_z <- lm(formula_z,data = z_data_temp[round(get(var_name),4)!=0])
  temp_resid <- residuals(reg_z)
  # Save orthogonalized instruments on observations when MP is not exactly equal to 0 (which was an NA value):
  z_data_temp[round(get(var_name),4)!=0,(paste0(var_name,"_RESID")):=temp_resid] 
  rm(temp_resid,reg_z,formula_z)
  z_data_temp[,(var_name):=ifelse( round(get(var_name),4)==0,0,get(paste0(var_name,"_RESID")) )] # Replace MPS with Orthogonalized MPS.
  z_data_temp[,(paste0(var_name,"_RESID")):=NULL] # Remove this series.
}

# Replace z_data series with orthogonalized shocks:
z_data <- z_data_temp[,mget(c("ANNDATE",z_names))]
rm(temp_exp_vars,z_data_temp)