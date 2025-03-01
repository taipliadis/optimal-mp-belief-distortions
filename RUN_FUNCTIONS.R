#-------------------------------------------------------------------------------
# Run pre.ts.var function to convert data into time series with p lags for VAR(p).
#-------------------------------------------------------------------------------

pre.ts.var <- function(data_set,N_lags,date_freq=12,y_names,slct_method){
  ## Description: This function gets the "data_set" a data.table that includes all variables of interest
  #> and creates lags for all variables mentioned in "y_names". It creates lags up to "N_lags" (e.g., N_lags=12).
  #> If "reduced_form" is selected on "slct_method" then the function will include belief distortions (BD).
  #> If "structural" form is selected instead, then the function will include Forecast CPI (FCST_CPI) instead of BD.
  
  ## Inputs Description:
  #> data_set: Data table that includes all variables of the VAR model.
  #> N_lags: Number of lags in VAR(p) model.
  #> date_freq: Number of periods per year. Default: 12 months.
  #> y_names: Response Variables for IRFs. Default: All variables in y_data.
  #> slct_method: "reduced_form" or "structural".
  
  ## Outputs Description:
  #> ts_var: time-series data to be used in VAR.
  #> formula_reg: Prints the formula of OLS regressions of the VAR model.
  #> y_date: All dates in the time-series of endogenous variables.
  
  ## Step 1: Create the lagged variables and include them in the data table.
  for (i in 1:N_lags){
    # Create lag of order i for all variables in y_names:
    for (j in y_names){
      lag_vars <- as.data.table(shift(data_set[[j]], n = i, type = "lag"))
      setnames(lag_vars, paste0(j, "_lag", i)) # Name the new lagged columns.
      data_set <- cbind(data_set, lag_vars) # Add lagged variables to the data.table
      rm(lag_vars) # Remove unnecessary element.
    }
  }
  rm(i,j) # Remove unnecessary elements.
  
  # Remove observations with NA which are created after taking the lags:
  data_set[,REMOVE:=ifelse(is.na( get(y_names[1]) )|is.na( get( paste0(y_names[1],"_lag",N_lags) ) ),1,0)]
  data_set <- data_set[REMOVE==0]
  data_set$REMOVE <- NULL # Remove unnecessary variable.
  y_date <- data_set$ANNDATE # Create a column of all dates of the VAR model.
  
  # Find the first and last columns of explanatory variables in the VAR model:
  init_ctrl <- which(colnames(data_set)==paste0(y_names[1],"_lag",1))
  last_ctrl <- which(colnames(data_set)==paste0(y_names[length(y_names)],"_lag",N_lags))
  
  # Create a vector with names of all control variables:
  explanatory_vars <- colnames(data_set)[init_ctrl:last_ctrl] # Select names of control variables.
  
  # Type all regression lines and save them in "formula_reg" vector:
  formula_reg <- paste0(y_names[1:length(y_names)]," ~ ",paste( explanatory_vars, collapse = " + "))
  
  init_date <- y_date[1] # Find initial date to label time-series.
  
  ## Step 2: Remove unnecessary variables in the time-series data.
  
  if (slct_method=="reduced_form"){
    # In the reduced-form method, we keep BD and do not need FCST_CPI:
    ts_var <- ts(data_set[,!c("ANNDATE","FCST_CPI","FFR","FG","LSAP")],start = init_date ,frequency = 12) # Convert data table to time series.
  } else if (slct_method=="structural"){
    # In the structural method, we keep FCST_CPI and do not need BD:
    ts_var <- ts(data_set[,!c("ANNDATE","BD","FFR","FG","LSAP")],start = init_date ,frequency = 12) # Convert data table to time series.
  }
  
  y_date <- as.data.table(y_date) # Convert the column of dates to data.table format.
  colnames(y_date) <- "ANNDATE"   # Name this column ANNDATE.
  rm(init_ctrl,last_ctrl)         # Remove unnecessary elements.
  return(list(ts_var = ts_var, formula_reg = formula_reg, y_date = y_date ))
}

#-------------------------------------------------------------------------------
# Run run.var function to run the VAR(p) model.
#-------------------------------------------------------------------------------

run.var <- function(ts_var,formula_reg,y_names,N_lags){
  ## Description: This function takes the time series saved in "ts_var" and runs
  #> the VAR model. The model is represented by a series of OLS regression formulas 
  #> included in the "formula_reg". The VAR model is of order "N_lags". 
  #> The endogenous variables of the VAR are named in the "y_names" vector.
  
  # Description of input variables:
  #> ts_var: The time series data obtained from pre.ts.var() function.
  #> formula_reg: A vector of all regression formulas in the VAR obtained from pre.ts.var() function.
  #> N_lags: Number of lags of the VAR(p) model.

  # Description of output variables:
  #> B_mat: list of lag coefficient matrices.
  #> C_mat: matrix of constant coefficients.
  #> w_mat: matrix of reduced-form residuals.
  #> y_hat_mat: matrix of fitted values.

  N_vars <- length(y_names)  # Find the number of variables in the vector.
  
  ## Step 1: Run Regressions from VAR(p) model.
  
  var_result <- list()  # Save regression results from VAR(p) model in this list.
  
  # For each response variable i, run an OLS regression: 
  for (i in 1:N_vars){
    var_result[[i]] <- lm(formula_reg[i],data = ts_var) # Save the regression result.
  }
  
  ## Step 2: Save Coefficients, fitted values and residuals.
  
  B_mat <- list()                                      # Save slope coefficients here.
  C_mat <- matrix(0,nrow=N_vars,ncol=1)                # Save intercepts here.
  w_mat <- matrix(0,nrow=nrow(ts_var),ncol=N_vars)     # Save residuals here.
  y_hat_mat <- matrix(0,nrow=nrow(ts_var),ncol=N_vars) # Save fitted Values here.
  
  # Save beta coefficients:
  j = 1
  for (p in 1:N_lags){  # For each lag of order p ...
    temp_coef <- matrix(0,ncol = N_vars,nrow = N_vars)  # ... create a temporary matrix ...
    for (k in 1:N_vars){
      temp_coef[k,] <- coef(var_result[[k]])[(j+1):(j+N_vars)] # ... and save the associated coefficients.
    }
    B_mat[[p]] <- temp_coef # B_mat[[p]] corresponds to B_p (ie, coefficients of lag-p variables).
    colnames(B_mat[[p]]) <- names( coef(var_result[[k]])[(j+1):(j+N_vars)] ) # Label columns to associate the coefficients with the variables.
    rownames(B_mat[[p]]) <- y_names # Label rows according to the response variables of the VAR.
    j <- j + N_vars
  }
  rm(p,j,k) # Remove unnecessary elements.
  
  # Save intercepts, residuals and fitted values:
  for (k in 1:N_vars){ # For each response variable ...
    C_mat[k] <- coef(var_result[[k]])[1]            # ... get the intercept.
    w_mat[,k] <- residuals(var_result[[k]])         # ... get the reduced-form residuals.
    y_hat_mat[,k] <- fitted.values(var_result[[k]]) # ... get the fitted values.
  }
  rm(k) # Remove unnecessary element.
  
  # Label rows and columns:
  rownames(w_mat) <- 1:nrow(ts_var)                 # Assign row numbers on residuals matrix.
  colnames(w_mat) <- paste0("RESIDUAL_",y_names)    # Assign column labels on residuals matrix.
  
  rownames(y_hat_mat) <- 1:nrow(ts_var)             # Assign row numbers on fitted-values matrix.
  colnames(y_hat_mat) <- paste0("FIT_",y_names)     # Assign column labels on fitted-values matrix.
  
  C_mat <- as.matrix(C_mat)                         # Convert to matrix form.
  rownames(C_mat) <- y_names                        # Label row names.
  colnames(C_mat) <- "INTERCEPT"                    # Label column names.
  
  w_mat <- as.matrix(w_mat)                         # Convert to matrix form.
  
  y_hat_mat <- as.matrix(y_hat_mat)                 # Convert to matrix form.
  
  return(list(B_mat = B_mat, C_mat = C_mat ,w_mat = w_mat, y_hat_mat = y_hat_mat))
}

#-------------------------------------------------------------------------------
# Run run.PHI function to get Impulse Response function (IRF).
#-------------------------------------------------------------------------------

run.PHI <- function(y_names,N_lags,B_mat,irf_periods=24){
  ## Description: This function creates the \phi(k) that describe the IRFs for horizons from 0 to "irf_periods" periods onward.
  
  ## Description of inputs:
  #> y_names: Vector with the name of variables in order of the VAR(p) model.
  #> N_lags: Number of lags of the VAR(p) model.
  #> B_mat: Slope coefficient matrices (stack them in list form).
  #> irf_periods: Number of periods to study IRF. Default: 24 periods.
   
  ## Description of outputs:
  #> phi_function: matrix containing the impulse response functions.
  
  ## Run impulse responses:
  phi_function <- list()    # Save here the IRF for every shock variable j.
  phi_function[[1]] <- B_mat[[1]]%^%0
  colnames(phi_function[[1]]) <- y_names # Each column corresponds to different response variable.
  for (t in 1:irf_periods){
    IRF_t <- phi_function[[1]] * 0
    if( (t-N_lags) <0){ # As long as t is less than p from VAR(p), not all coefficients from the B matrix are needed ...
      j=1
      while (t-j>=0) { # ... add only as many coefficients from the B matrix as needed.
        IRF_t <- IRF_t + B_mat[[j]] %*% phi_function[[t-j+1]]
        j <- j+1
      }
      rm(j)
    } else { # If time t is at least as much as p from VAR(p), all coefficients are needed from the B matrix ...
      for (k in 1:N_lags){ # ... add all coefficients on lagged variables.
        IRF_t <- IRF_t + B_mat[[k]] %*% phi_function[[t-k+1]] 
      }
      rm(k)
    }
    phi_function[[t+1]] <- IRF_t
    colnames(phi_function[[t+1]]) <- y_names
  }
  return(list(phi_function = phi_function))
}

#-------------------------------------------------------------------------------
# Run run.PHI2 function to get phi_x and A matrix.
#-------------------------------------------------------------------------------

run.PHI2 <- function(y_names,N_m,N_c,B_mat,irf_periods=24,phi_function,Sigma_mat){
  ## Description: This function estimates the impact matrix from structural shocks
  #> when the structural methodology is selected.
  #> It is needed to identify the structural belief distortion shock.
  
  ## Description of Inputs:
  #> y_names: Vector of all names of the response variables in order.
  #> N_m: The number of belief distortion shocks. Here N_m=1 (ie, belief distortions of CPI).
  #> N_c: The number of contemporaneous macro shocks.
  #> irf_periods: Define the horizon to create IRFs (i.e., periods onward).
  #> phi_function: IRFs.
  #> Sigma_mat: Variance-covariance matrix of reduced-form residuals.

  ## Description of outputs:  Read Adams and Barrett (2024).
  
  N_vars <- N_m + N_c # Number of response variables in the VAR model.
  
  ## Find blocks of Sigma matrix:
  Sigma_11 <- Sigma_mat[1:N_m,1:N_m] # See Appendix A of Adams and Barrett (2024).
  Sigma_12 <- Sigma_mat[1:N_m,(N_m+1):N_vars] # See Appendix A of Adams and Barrett (2024).
  Sigma_22 <- Sigma_mat[(N_m+1):N_vars,(N_m+1):N_vars] # See Appendix A of Adams and Barrett (2024).
  
  ## Find phi_x^h:
  phi_x <-  matrix(0,nrow=(irf_periods+1),ncol = N_vars)
  colnames(phi_x) <- y_names
  rownames(phi_x) <- 0:irf_periods
  
  for (t in 1:(irf_periods+1)){
    #col_order <- which(colnames(phi_function[[1]])=="CPI")
    phi_x[t,] <- phi_function[[t]][2,] #I get the 2nd row because this is where CPI responds to shocks.
  }
  rm(t)
  
  ## Estimate V^S:
  phi_x_f <- phi_x[13,1:N_m] #h=12 months ahead.
  phi_x_c <- phi_x[13,(N_m+1):N_vars] #h=12 months ahead.
  I_mat <- diag(N_m)
  temp_vector <- matrix( c(I_mat - phi_x_f, - phi_x_c), nrow = 1)
  colnames(temp_vector) <- y_names
  VS <- temp_vector %*% Sigma_mat %*% t(temp_vector)
  
  xi_VS <- sqrt(VS)
  
  function_ASc <- solve(xi_VS) %*% (I_mat - phi_x_f) %*% (Sigma_12 - solve(I_mat-phi_x_f) %*% phi_x_c %*% Sigma_22)
  
  ASc <- t(function_ASc)
  colnames(ASc) <- "SENTIMENT"
  rownames(ASc) <- y_names[(1+N_m):N_vars]
  
  ASf <- solve(I_mat-phi_x_f) %*% xi_VS + solve(I_mat-phi_x_f) %*% phi_x_c %*% ASc
  rownames(ASf) <- y_names[1:N_m]
  
  function_AFc <- Sigma_22 - ASc %*% t(ASc) # See Equation (29) of Appendix A in Adams and Barrett (2024).
  AFc <- t( chol(function_AFc) ) # Arbitrarily assume lower triangular from Cholesky decomposition.
  
  AFf <- solve(I_mat - phi_x_f) %*% phi_x_c %*% AFc # See Equation (6) in Adams and Barrett (2024).
  A_mat <- matrix(c(ASf,ASc),ncol=1)
  A_mat <- cbind(A_mat, rbind(AFf,AFc) )
  colnames(A_mat) <- y_names
  rownames(A_mat) <- y_names
  
  return(list(phi_x=phi_x,phi_x_f=phi_x_f,phi_x_c=phi_x_c,VS=VS,xi_VS=xi_VS,ASc=ASc,ASf=ASf,AFc=AFc,AFf=AFf,A_mat=A_mat))
}

# continue here ...
#-------------------------------------------------------------------------------
# Run run.MPS function to estimate impact of MP shocks on endogenous variables.
#-------------------------------------------------------------------------------

run.MPS <- function(y_names,z_names,y_date,w_mat,z_data){
  
  N_vars <- length(y_names) # Number of endogenous variables.
  M_vars <- length(z_names) # Number of exogenous MP shocks.
  
  ## Merge reduced-form VAR residuals with external shocks data:
  tsls_dt <- cbind(y_date, as.data.table(w_mat) ) # Convert residuals matrix to data table.
  tsls_dt <- merge(tsls_dt,z_data,by=c("ANNDATE"),all=TRUE)
  tsls_dt <- na.omit(tsls_dt)
  
  omega_mat <- matrix(0,nrow = N_vars,ncol = M_vars) # 3 MP shocks.
  rownames(omega_mat) <- y_names
  colnames(omega_mat) <- z_names
  
  # formula_reg_2 <- paste0("RESIDUAL_",y_names[1:length(y_names)]," ~ ",paste( z_names , collapse = " + "))
  formula_reg_2 <- paste0("RESIDUAL_",y_names[1:length(y_names)]," ~ ",paste( z_names , collapse = " + "))

  for (i in 1:N_vars) {
    second_stage <- lm( formula_reg_2[i] , data = tsls_dt) # Regress residuals on first-stage fitted values.
    omega_mat[i,1:M_vars] <- as.numeric( coef(second_stage)[2:(M_vars+1)] ) # Save estimated beta coefficients.
  }
  
  return(list(omega_mat=omega_mat,formula_reg_2=formula_reg_2))
}

#-------------------------------------------------------------------------------
# Run run.IRF function to get Impulse Responses of a BD/Sentiment shock
#-------------------------------------------------------------------------------

run.IRF <- function(phi_function,A_mat,y_names,scale_var="BD",scale_var_mag=1,irf_periods=24,slct_method){
  # Info: Find the IRF of a belief distortion shock that causes 
  #> forecasts to depart from rational expectations of CPI_{t+12} by 1 p.p.
  
  # Input variables:
  #> phi_function: impulse responses from VAR model.
  #> A_mat: Impact matrix of VAR shocks.
  #> scale_var: Scale the shock to have an impact of scale_var_mag on scale_var on impact. Example: "BD".
  #> scale_var_mag: The size of shock as measured on the scaled variable on impact. Example: 1.
  #> irf_periods: Number of IRF periods considered.
  #> slct_method: "reduced_form" or "structural".
  
  # Output variables:
  #> irf_mat: TxN matrix containing all impulse responses to the shock.
  
  N_vars <- length(y_names) # Number of endogenous variables in the VAR.
  
  epsilon_vector <- matrix(0,ncol=1,nrow=N_vars)
  colnames(epsilon_vector) <- "SHOCK"
  rownames(epsilon_vector) <- y_names
  
  if (slct_method=="reduced_form"){
    epsilon_vector[which(rownames(epsilon_vector)==scale_var)] <- 1/A_mat[ which(rownames(A_mat)==scale_var) , which(colnames(A_mat)==scale_var) ] * scale_var_mag
  } else if (slct_method=="structural"){
    
    if (scale_var=="BD"){
      scale_var <- "FCST_CPI"   # In the structural methodology we infer Belief Distortion by the difference between FCST_CPI and Rational Expectations of CPI_{t+12}.
      scale_rational <- "CPI"
      }
    
    epsilon_vector[which(rownames(epsilon_vector)==scale_var)] <- solve(A_mat[ which(rownames(A_mat)==scale_var) , which(colnames(A_mat)==scale_var) ] - phi_function[[13]][ which( colnames(phi_function[[13]])==scale_rational ) ,]  %*% A_mat[,which(colnames(A_mat)==scale_var) ]) * scale_var_mag # Set shock to have a 1 p.p. increase of BD on impact.
  }
  
  
  N_vars <- nrow(phi_function[[1]])
  
  if (slct_method=="reduced_form"){
    # In the reduced-form methodology, the IRF are straightforward ...
    irf_mat <- matrix(0, ncol = N_vars, nrow = irf_periods + 1) # Initialize the matrix to store IRF results.
    colnames(irf_mat) <- c(rownames(phi_function[[1]]))
    rownames(irf_mat) <- 0:irf_periods
    
    for (t in 1:(irf_periods+1)){
      for (j in 1:N_vars){
        irf_mat[t,j] <- phi_function[[t]][j,] %*% A_mat %*% epsilon_vector
      }
    }
    
  } else if (slct_method=="structural"){
    # In the structural-form methodology, we need to include the IRF of two more variables
    #> not explicitly mentioned in the VAR system ... these are:
    #> (a) Rational Expectations.
    #> (b) Sentiment: the difference between reported forecasts and rational expectations.
    irf_mat <- matrix(0, ncol = N_vars+2, nrow = irf_periods + 1) # Initialize the matrix to store IRF results.
    colnames(irf_mat) <- c(rownames(phi_function[[1]]) , "RE_CPI", "SENTIMENT" )
    rownames(irf_mat) <- 0:irf_periods
    
    for (t in 1:(irf_periods+1)){
      for (j in 1:N_vars){
        irf_mat[t,j] <- phi_function[[t]][j,] %*% A_mat %*% epsilon_vector
      }
    }
    for (t in 1:(irf_periods+1-12)){
      irf_mat[t,(N_vars+1)] <- irf_mat[(t+12),2] # Rational expectations of inflation at t is equal to 12M ahead E[CPI].
      irf_mat[t,(N_vars+2)] <- irf_mat[t,1] - irf_mat[t,(N_vars+1)] # Sentiment is equal to FCST_CPI - E[CPI].
    }
    
  }
  
  return( list(irf_mat = irf_mat) )
}


#-------------------------------------------------------------------------------
# Run run.IRF function to get Impulse Responses of a MP shock
#-------------------------------------------------------------------------------

run.IRF.MPS <- function(phi_function,omega_mat,y_names,z_names,scale_var="TREAS",scale_var_mag=1,irf_periods=24,slct_method){
  # Info: Find the IRF of a monetary policy shock that causes 
  #> unemployment to increase by 1 p.p. on impact.
  
  # Input variables:
  #> phi_function: impulse responses from VAR model.
  #> omega_mat: Impact matrix of MP shocks to endogenous VAR variables.
  #> scale_var: Scale the shock to have an impact of scale_var_mag on scale_var on impact.
  #> scale_var_mag: The size of shock as measured on the scaled variable on impact.
  #> irf_periods: Number of IRF periods considered.
  #> slct_method: "reduced_form" or "structural".
  
  # Output variables:
  #> irf_mat_mp: A list that stores all IRFs.
  #> For each shock, e.g., irf_mat_mp$FFR, it creates a TxN matrix containing all impulse responses to the shock.
  
  irf_mat_mp <- list() # Save all IRF here.
  
  N_vars <- length(y_names) # Number of endogenous variables in the VAR.
  M_vars <- length(z_names) # Number of exogenous MP shocks.
  
  if (length(scale_var)<M_vars){
    scale_var <- matrix(data=c(rep(scale_var,M_vars)),nrow=1,ncol=M_vars)
  }
  if (length(scale_var_mag)<M_vars){
    scale_var_mag <- matrix(data=c(rep(scale_var_mag,M_vars)),nrow=1,ncol=M_vars)
  }
  
  for (mj in 1:M_vars){
    epsilon_mp <- matrix(0,ncol=1,nrow=M_vars)
    colnames(epsilon_mp) <- "SHOCK"
    rownames(epsilon_mp) <- z_names
    
    epsilon_mp[mj] <- 1/omega_mat[ which(rownames(omega_mat)==scale_var[mj]) ,mj] * scale_var_mag[mj]
    
    var_mj <- z_names[mj]
    
    if (slct_method=="reduced_form"){
    irf_temp <- matrix(0, ncol = N_vars, nrow = irf_periods + 1) # Initialize the matrix to store IRF results.
    colnames(irf_temp) <- c(rownames(phi_function[[1]]))
    rownames(irf_temp) <- 0:irf_periods
    
    for (t in 1:(irf_periods+1)){
      for (j in 1:N_vars){
        irf_temp[t,j] <- phi_function[[t]][j,] %*% omega_mat %*% epsilon_mp
      }
    }
    irf_mat_mp[[var_mj]] <- irf_temp
    
    } else if (slct_method=="structural") {
      irf_temp <- matrix(0, ncol = N_vars+2, nrow = irf_periods + 1) # Initialize the matrix to store IRF results.
      colnames(irf_temp) <- c(rownames(phi_function[[1]]), "RE_CPI", "BD")
      rownames(irf_temp) <- 0:irf_periods
      
      for (t in 1:(irf_periods+1)){
        for (j in 1:N_vars){
          irf_temp[t,j] <- phi_function[[t]][j,] %*% omega_mat %*% epsilon_mp
        }
      }
      for (t in 1:(irf_periods+1-12)){
        irf_temp[t,(N_vars+1)] <- irf_temp[(t+12),2] # Rational expectations of inflation at t is equal to 12M ahead E[CPI].
        irf_temp[t,(N_vars+2)] <- irf_temp[t,1] - irf_temp[t,(N_vars+1)] # Sentiment is equal to FCST_CPI - E[CPI].
      }
      irf_mat_mp[[var_mj]] <- irf_temp
      
      
    }
    
  }
  return(list( irf_mat_mp = irf_mat_mp))
}

#-------------------------------------------------------------------------------
# Run run.PHIM function to vectorize responses to CPI and UNEMP into the phi_m
#-------------------------------------------------------------------------------

run.PHIM <- function(y_names,z_names,phi_function,welf_horizon,A_mat,omega_mat,
                     slct_method,irf_mat,irf_mat_mp){
  
  N_vars <- length(y_names) # Number of endogenous variables in the VAR.
  M_vars <- length(z_names) # Number of exogenous MP shocks.
  
  ## Create a basis vector that selects CPI variable:
  e_inf <- matrix(0,ncol = N_vars,nrow = 1)
  e_inf[which(y_names=="CPI")] <- 1
  colnames(e_inf) <- y_names
  rownames(e_inf) <- "CPI"
  
  ## Create a basis vector that selects UNEMP variable:
  e_unemp <- matrix(0,ncol = N_vars,nrow = 1)
  e_unemp[which(y_names=="UNEMP")] <- 1
  colnames(e_unemp) <- y_names
  rownames(e_unemp) <- "UNEMP"
  
  ## Vectorize impact of a BD shock on CPI and UNEMP:
  
  epsilon_vector <- matrix(0,ncol=1,nrow=N_vars)
  colnames(epsilon_vector) <- "SHOCK"
  rownames(epsilon_vector) <- y_names
  if (slct_method=="reduced_form"){
    epsilon_vector[which(rownames(epsilon_vector)=="BD")] <-  1/A_mat[1,1] * 1
  } else if (slct_method=="structural"){
    epsilon_vector[which(rownames(epsilon_vector)=="FCST_CPI")] <- solve(A_mat[which(rownames(A_mat)=="FCST_CPI"),which(colnames(A_mat)=="FCST_CPI")] - phi_function[[13]][ which( colnames(phi_function[[13]])=="CPI" ),]  %*% A_mat[,which(colnames(A_mat)=="FCST_CPI")]) * 1 # Set shock to have a 1 p.p. increase of BD on impact.
  }
  
  phi_r_u <- matrix(0,ncol = 1,nrow = (welf_horizon+1) )  # Vectorize unemployment response to BD shocks for all horizons.
  phi_r_pi <- matrix(0,ncol = 1,nrow = (welf_horizon+1)  ) # Vectorize cpi response to BD shocks for all horizons.
  for (j in 1:(welf_horizon+1) ){
    phi_r_u[j] <- e_unemp %*% phi_function[[j]] %*% A_mat %*% epsilon_vector
    # Estimate welfare with Inflation (not CPI):
    if (j==1){
      phi_r_pi[j] <- e_inf %*% (phi_function[[j]]) %*% A_mat %*% epsilon_vector
    } else {
      phi_r_pi[j] <- e_inf %*% (phi_function[[j]]-phi_function[[(j-1)]]) %*% A_mat %*% epsilon_vector
    }
  }
  
  phi_r <- rbind(phi_r_u,phi_r_pi)  # Stack these vectors in one.
  length(phi_r) #2002
  
  ## Vectorize impact of a MP shock on CPI and UNEMP:
  phi_m_u <- matrix(0,ncol = M_vars,nrow = (welf_horizon+1) )  # Vectorize unemployment response to MP shocks for all horizons.
  colnames(phi_m_u) <- z_names
  phi_m_pi <- matrix(0,ncol = M_vars,nrow = (welf_horizon+1) ) # Vectorize cpi response to MP shocks for all horizons.
  colnames(phi_m_pi) <- z_names
  
  scale_var_mp <- c("TREAS","TREAS","TREAS") # c("SHADOW_FFR","TREAS","TREAS_10Y")
  scale_var_mp_mag <- c(1,1,1)
  
  for (m in 1:M_vars){
    
    scale_temp <- scale_var_mp_mag[m] / irf_mat_mp[[m]][j,which(colnames(irf_mat_mp[[m]])==scale_var_mp[m])]
    for (j in 1:(welf_horizon+1) ){
      phi_m_u[j,m] <- irf_mat_mp[[m]][j,which(colnames(irf_mat_mp[[m]])=="UNEMP")]
      # Estimate welfare with Inflation (not CPI):
      if (j==1){
        phi_m_pi[j,m] <- irf_mat_mp[[m]][j,which(colnames(irf_mat_mp[[m]])=="CPI")]
      } else {
        phi_m_pi[j,m] <- irf_mat_mp[[m]][j,which(colnames(irf_mat_mp[[m]])=="CPI")] - irf_mat_mp[[m]][(j-1),which(colnames(irf_mat_mp[[m]])=="CPI")]
      }
    }
  }
  
  phi_m <- rbind(phi_m_u,phi_m_pi)  # Stack these vectors in one.
  nrow(phi_m) #2002
  ncol(phi_m) #3
  
  return(list( phi_r = phi_r, phi_m = phi_m ))
}

#-------------------------------------------------------------------------------
# Run COUNTERFACTUALS
#-------------------------------------------------------------------------------

run.COUNTERF <- function(lambda_coef,welf_horizon,phi_r,phi_m,z_names,slct_mpshocktypes){
  
  block_1 <- lambda_coef * diag( (welf_horizon+1) )
  block_2 <- matrix(0,ncol=(welf_horizon+1),nrow=(welf_horizon+1) )
  block_3 <- (1-lambda_coef) * diag( (welf_horizon+1) )
  Welf_mat <- rbind( cbind(block_1,block_2) , cbind(block_2,block_3) )
  
  dep_var <- (Welf_mat)^(1/2) %*% phi_r
  colnames(dep_var) <- "BD"
  indep_var <- - (Welf_mat)^(1/2) %*% phi_m
  M_vars <- ncol(indep_var)
  
  counterf_dt <- as.data.table( cbind(dep_var,indep_var) )
  
  if (slct_mpshocktypes=="bauer_swanson"){
    formula_count_indep <- c("BD~0+FFR","BD~0+FG","BD~0+LSAP")
    formula_count_pair <- c("BD~0+FFR+FG","BD~0+FG+LSAP","BD~0+FFR+LSAP")
    formula_count_all <- c("BD~0+FFR+FG+LSAP")
    formula_count_reg <- c(formula_count_indep, formula_count_pair, formula_count_all)
  }
  
  if (slct_mpshocktypes=="aruoba_mps"){
    formula_count_reg <- c("BD~0+FFR")
  }
  
  ## Create a matrix to save all estimated psi coefficients:
  psi_coef_mat <- matrix(as.numeric(NA), nrow = length(formula_count_reg), ncol = M_vars )
  
  # Label rows and columns of the matrix:
  colnames(psi_coef_mat) <- z_names
  if (slct_mpshocktypes=="bauer_swanson"){
  rownames(psi_coef_mat) <- c( rep("Independent Tools", length(formula_count_indep)),
                               rep("Pair Tools", length(formula_count_pair)),
                               rep("All Tools", length(formula_count_all)) )
  }
  if (slct_mpshocktypes=="aruoba_mps"){
    rownames(psi_coef_mat) <- c("Single Tool")
  }
  
  ## Create a matrix to save all Adjusted R^2:
  r_sq_mat <- matrix(as.numeric(NA), nrow = length(formula_count_reg), ncol = 1 )
  
  # Label rows and columns of the matrix:
  colnames(r_sq_mat) <- "R_Squared"
  if (slct_mpshocktypes=="bauer_swanson"){
  rownames(r_sq_mat) <- c( rep("Independent Tools", length(formula_count_indep)),
                               rep("Pair Tools", length(formula_count_pair)),
                               rep("All Tools", length(formula_count_all)) )
  }
  if (slct_mpshocktypes=="aruoba_mps"){
    rownames(r_sq_mat) <- c("Single Tool")
  }
  
  ## Run counterfactuals and save estimated coefficients:
  for (i in 1:length(formula_count_reg) ){
    temp_reg <- lm(formula = formula_count_reg[i],data = counterf_dt)
    
    for (j in 1:length(coefficients(temp_reg))){
      factor_name <- names(coefficients(temp_reg))[j]
      psi_coef_mat[i, which(colnames(psi_coef_mat)==factor_name) ] <- round( as.numeric( coefficients(temp_reg)[j] ), slct_dcml)
      r_sq_mat[i] <- round( summary(temp_reg)$r.squared , slct_dcml)
    }
    rm(temp_reg)
  }
  
  return(list( psi_coef_mat = psi_coef_mat, r_sq_mat = r_sq_mat ))
}

#-------------------------------------------------------------------------------
# Run Bootstrap function for Baseline VAR.
#-------------------------------------------------------------------------------

run.BOOTSTRAP <- function(y_names,w_mat,y_hat_mat,ts_var,data_set,y_date,N_lags,N_m,N_c,formula_reg,
                          shock_vector,omega_mat,lambda_coef,
                          irf_periods=24,welf_horizon=24,slct_method,slct_bootruns=100,slct_mpshocktypes,bootstrap_method,irf_mat,irf_mat_mp){
  
  N_vars <- length(y_names)
  N_obs <- nrow(w_mat)            # Find number of observations.
  
  IRF_BOOT <- list()              # Save here bootstrap IRF of BD shocks.
  IRF_MP_BOOT <- list()           # Save here bootstrap IRF of MP shocks.
  PSI_BOOT <- list()              # Save here bootstrap optimal policy responses.
  
  PHI_R_BOOT <- list()            # Save here bootstrap phi_r vector.
  PHI_M_BOOT <- list()            # Save here bootstrap phi_m vector.
  
  y_hat_dt <- cbind(y_date,as.data.table(ts_var),as.data.table(y_hat_mat)) # Convert fitted values to matrix form.
  
  for (b in 1:slct_bootruns){
    
    rand_idx <- sample(1:N_obs, size = N_obs, replace = TRUE)  # Generate a vector of random indices to re-arrange the residual data.
    samplew_mat <- w_mat[rand_idx,]
    
    
    sample_dt <- cbind(y_hat_dt, as.data.table(samplew_mat) )  # Add the randomly arranged residuals to the data table.
    
    # Add the randomly assigned residuals to fitted values and generate bootstrapped data for y_t:
    for (var in y_names) {
      fvar <- paste0("FIT_", var)
      rvar <- paste0("RESIDUAL_", var)
      sample_dt[, (var) := get(fvar) + get(rvar)]
      sample_dt[, (fvar) := NULL]
      sample_dt[, (rvar) := NULL]
    }
    
    ## Run VAR model:
    
    bootresult <- run.var(ts_var=sample_dt,formula_reg=formula_reg,
                          y_names=y_names,N_lags=N_lags) # Run VAR(p) model.
    B_mat_boot <- bootresult$B_mat          # Save slope coefficients here.
    C_mat_boot <- bootresult$C_mat          # Save intercepts here.
    w_mat_boot <- bootresult$w_mat          # Save residuals here.
    y_hat_mat_boot <- bootresult$y_hat_mat  # Save fitted Values here.
    rm(bootresult)
    
    Sigma_mat_boot <- cov(w_mat_boot)   # Variance-covariance matrix of residuals.
    
    ## Run impulse response functions:
    bootresult <- run.PHI(y_names=y_names,N_lags=N_lags,B_mat=B_mat_boot,irf_periods=irf_periods)
    phi_function_boot <- bootresult$phi_function # phi_function[[1]]: IRF of t=0; phi_function[[k]]: IRF of k-1.
    rm(bootresult)
    
    ## Impact matrix estimation:
    if (slct_method=="reduced_form"){
      A_mat_boot <- diag(N_vars)           # In the reduced-form methodology, A_mat is just a diagonal matrix.
    } else if (slct_method=="structural"){
      result <- run.PHI2(y_names=y_names,N_m=N_m,N_c=N_c,B_mat=B_mat_boot,irf_periods=irf_periods,phi_function=phi_function_boot,Sigma_mat=Sigma_mat_boot)
      A_mat_boot <- result$A_mat
    }
    colnames(A_mat_boot) <- y_names
    rownames(A_mat_boot) <- y_names
    
    #---------------------------------------------------------------------------
    #Important: We take omega_mat from original sample because the 2-stage nature introduces a lot of noise in the data on bootstrap.
    omega_mat_boot <- omega_mat
    
    ## IRF of a 1p.p. positive sentiment shock:
    bootresult <- run.IRF(phi_function=phi_function_boot,A_mat=A_mat_boot,
                      y_names=y_names,scale_var="BD",scale_var_mag=1,
                      irf_periods=irf_periods,slct_method=slct_method)
    irf_mat_boot <- bootresult$irf_mat
    
    ## IRF of a MP shock that causes 2Y Treasury yields to increase by 1p.p. on impact:
    if (bootstrap_method=="omega_fixed"){
      bootresult <- run.IRF.MPS(phi_function=phi_function_boot,omega_mat=omega_mat_boot,
                                y_names=y_names,z_names=z_names,scale_var=c("TREAS","TREAS","TREAS"),
                                scale_var_mag=c(1,1,1),irf_periods=irf_periods,slct_method=slct_method)
      irf_mat_mp_boot <- bootresult$irf_mat_mp
    } else if (bootstrap_method=="irf_mp_fixed"){
      irf_mat_mp_boot <- irf_mat_mp
    }
    
    ## Run counterfactual policies:
    bootresult <- run.PHIM(y_names=y_names,z_names=z_names,phi_function=phi_function_boot,
                           welf_horizon=welf_horizon,A_mat=A_mat_boot,omega_mat=omega_mat_boot,slct_method=slct_method,irf_mat_boot,irf_mat_mp_boot)
    phi_r_boot <- bootresult$phi_r
    phi_m_boot <- bootresult$phi_m
    
    PHI_R_BOOT[[b]] <- bootresult$phi_r
    PHI_M_BOOT[[b]] <- bootresult$phi_m
    
    bootresult <- run.COUNTERF(lambda_coef=lambda_coef,welf_horizon=welf_horizon,phi_r_boot,phi_m_boot,z_names=z_names,slct_mpshocktypes=slct_mpshocktypes)
    psi_coef_mat_boot <- bootresult$psi_coef_mat
    
    IRF_BOOT[[b]] <- irf_mat_boot
    IRF_MP_BOOT[[b]] <- irf_mat_mp_boot
    PSI_BOOT[[b]] <- psi_coef_mat_boot
    
  }
  
  return( list(irf_boot=IRF_BOOT,irf_mp_boot=IRF_MP_BOOT,psi_boot=PSI_BOOT,PHI_R_BOOT=PHI_R_BOOT,PHI_M_BOOT=PHI_M_BOOT) )
}

#-------------------------------------------------------------------------------
# Run IRF with optimal MP response:
#-------------------------------------------------------------------------------

run.IRF.COUNT <- function(phi_function,A_mat,omega_mat,y_names,z_names,psi_coef_mat,scale_var="BD",scale_var_mp="TREAS",scale_var_mag=1,scale_var_mp_mag=1,irf_periods=24,slct_method,slct_mpshocktypes){
  # Info: Find the IRF of a belief distortion shock that causes 
  #> forecasts to depart from rational expectations of CPI_{t+12} by 1 p.p.
  
  # Input variables:
  #> phi_function: impulse responses from VAR model.
  #> A_mat: Impact matrix of VAR shocks.
  #> scale_var: Scale the shock to have an impact of scale_var_mag on scale_var on impact. Example: "BD".
  #> scale_var_mag: The size of shock as measured on the scaled variable on impact. Example: 1.
  #> irf_periods: Number of IRF periods considered.
  #> slct_method: "reduced_form" or "structural".
  
  # Output variables:
  #> irf_mat: TxN matrix containing all impulse responses to the shock.
  
  N_vars <- length(y_names) # Number of endogenous variables in the VAR.
  M_vars <- length(z_names) # Number of exogenous MP shocks.
  
  if (length(scale_var_mp)<M_vars){
    scale_var_mp <- matrix(data=c(rep(scale_var_mp,M_vars)),nrow=1,ncol=M_vars)
  }
  if (length(scale_var_mp_mag)<M_vars){
    scale_var_mp_mag <- matrix(data=c(rep(scale_var_mp_mag,M_vars)),nrow=1,ncol=M_vars)
  }
  
  epsilon_vector <- matrix(0,ncol=1,nrow=N_vars)
  colnames(epsilon_vector) <- "SHOCK"
  rownames(epsilon_vector) <- y_names
  
  if (slct_method=="reduced_form"){
    epsilon_vector[which(rownames(epsilon_vector)==scale_var)] <- 1/A_mat[ which(rownames(A_mat)==scale_var) , which(colnames(A_mat)==scale_var) ] * scale_var_mag
  } else if (slct_method=="structural"){
    
    if (scale_var=="BD"){
      scale_var <- "FCST_CPI"   # In the structural methodology we infer Belief Distortion by the difference between FCST_CPI and Rational Expectations of CPI_{t+12}.
      scale_rational <- "CPI"
    }
    
    epsilon_vector[which(rownames(epsilon_vector)==scale_var)] <- solve(A_mat[ which(rownames(A_mat)==scale_var) , which(colnames(A_mat)==scale_var) ] - phi_function[[13]][ which( colnames(phi_function[[13]])==scale_rational ) ,]  %*% A_mat[,which(colnames(A_mat)==scale_var) ]) * scale_var_mag # Set shock to have a 1 p.p. increase of BD on impact.
  }
  
  irf_count <- list()
  
  psi_vector <- psi_coef_mat
  psi_vector[is.na(psi_vector)] <- 0
  
  if (slct_mpshocktypes=="bauer_swanson"){
    N_count <- 7 # 7 Counterfactuals: 3 Individual Tools, 3 Pair-Tools, 1 All-3-Tools.
  }
  if (slct_mpshocktypes=="aruoba_mps"){
    N_count <- 1 # 1 Counterfactual because there is only one MP series.
  }
  
  # Run the 7 counterfactuals ...
  for (i in 1:N_count){
    # Define the counterfactual case we run (eg, pair tools) ...
    count_case <- c("indep_ffr","indep_fg","indep_lsap","pair_ffr_fg","pair_fg_lsap","pair_ffr_lsap","all_tools")[i]
    # Construct MP shock matrix depending on available MP tools of the counterfactual and optimal response coefficient (psi):
    epsilon_mp <- matrix(0,ncol=1,nrow=M_vars)
    colnames(epsilon_mp) <- "SHOCK"
    rownames(epsilon_mp) <- z_names
    
    for (mj in 1:M_vars){
      epsilon_mp[mj] <- 1/omega_mat[ which(rownames(omega_mat)==scale_var_mp[mj]) ,mj] * scale_var_mp_mag[mj] * psi_vector[i,mj]
    }
    
    # Run IRF of the BD shock accompanied by the simultaneous response from Fed:
    if (slct_method=="reduced_form"){
      # In the reduced-form methodology, the IRF are straightforward ...
      irf_mat <- matrix(0, ncol = N_vars, nrow = irf_periods + 1) # Initialize the matrix to store IRF results.
      colnames(irf_mat) <- c(rownames(phi_function[[1]]))
      rownames(irf_mat) <- 0:irf_periods
      
      for (t in 1:(irf_periods+1)){
          irf_mat[t,] <- (phi_function[[t]] %*% A_mat %*% epsilon_vector + phi_function[[t]] %*% omega_mat %*% epsilon_mp)
      }
      
    } else if (slct_method=="structural"){
      # In the structural-form methodology, we need to include the IRF of two more variables
      #> not explicitly mentioned in the VAR system ... these are:
      #> (a) Rational Expectations.
      #> (b) Sentiment: the difference between reported forecasts and rational expectations.
      irf_mat <- matrix(0, ncol = N_vars+2, nrow = irf_periods + 1) # Initialize the matrix to store IRF results.
      colnames(irf_mat) <- c(rownames(phi_function[[1]]) , "RE_CPI", "SENTIMENT" )
      rownames(irf_mat) <- 0:irf_periods
      
      for (t in 1:(irf_periods+1)){
        for (j in 1:N_vars){
          irf_mat[t,j] <- phi_function[[t]][j,] %*% A_mat %*% epsilon_vector + (phi_function[[t]][j,] %*% omega_mat %*% epsilon_mp)
        }
      }
      for (t in 1:(irf_periods+1-12)){
        irf_mat[t,(N_vars+1)] <- irf_mat[(t+12),2] # Rational expectations of inflation at t is equal to 12M ahead E[CPI].
        irf_mat[t,(N_vars+2)] <- irf_mat[t,1] - irf_mat[t,(N_vars+1)] # Sentiment is equal to FCST_CPI - E[CPI].
      }
      
    }
    # Save counterfactual results:
    irf_count[[count_case]] <- irf_mat # Save Counterfactual Case
  }
  return( list(irf_count = irf_count) )
}
