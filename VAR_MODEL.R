## Merge macro data with monetary policy instruments:
data_set <- merge(y_data,z_data,by=c("ANNDATE"),all=TRUE)

# For the following robustness test only, drop the COVID-19 era off the sample:
if (slct_robust=="drop_covid"){
  data_set <- data_set[ANNDATE<="Dec 2019"]     # Robustness Test #7 of Table 2.
}

# In the "structural" methodology we use Forecast CPI as the first variable (vs. Belief Distortions as in the "reduced-form" methodology):
if (slct_method=="structural"){
  y_names[which(y_names=="BD")] <- "FCST_CPI"
}

## Identify time periods (in year-month form):

# Time periods related to endogenous macro variables:
if (slct_method=="reduced_form"){
  init_date <- unique(data_set[is.na(BD)!=TRUE]$ANNDATE)[1]
  end_date <- unique(data_set[is.na(BD)!=TRUE]$ANNDATE)[length( unique(data_set[is.na(BD)!=TRUE]$ANNDATE) )]
  y_date <- data_set[ANNDATE>=init_date & ANNDATE<=end_date,.(ANNDATE)]
  } else if (slct_method=="structural"){
    init_date <- unique(data_set[is.na(FCST_CPI)!=TRUE]$ANNDATE)[1]
    end_date <- unique(data_set[is.na(FCST_CPI)!=TRUE]$ANNDATE)[length( unique(data_set[is.na(FCST_CPI)!=TRUE]$ANNDATE) )]
    y_date <- data_set[ANNDATE>=init_date & ANNDATE<=end_date,.(ANNDATE)]
}
rm(init_date,end_date)

# Time periods related to exogenous monetary policy instruments:
init_date <- unique(data_set[is.na(FFR)!=TRUE]$ANNDATE)[1]
end_date <- unique(data_set[is.na(FFR)!=TRUE]$ANNDATE)[length( unique(data_set[is.na(FFR)!=TRUE]$ANNDATE) )]
z_date <- data_set[ANNDATE>=init_date & ANNDATE<=end_date,.(ANNDATE)] # Get dates of exogenous MP series.
rm(init_date,end_date)

## Unselect to Run Akaiki Information Criterion on optimal lag selection:
#------------------------------------------------------------------------
# N_lag_max = 12
# Run Akaiki for Reduced Form model: 
# data_test <- data_set[,!c("FCST_CPI","FFR","FG","LSAP")]
# data_test <- na.omit(data_test)
# init_date <- data_test[,.SD[1]]$ANNDATE
# data_test$ANNDATE <- NULL
# ts_var <- ts(data_test,start = init_date ,frequency = 12)
# opt_lag <- VARselect(ts_var,lag.max = N_lag_max,type = "trend")
# opt_lag$selection #12 Lags according to AIC, HQ and FPE.
# rm(N_lag_max,ts_var,data_test,opt_lag,init_date)
#------------------------------------------------------------------------

## Prepare data as time-series before we run the VAR model:

result <- pre.ts.var(data_set=data_set,N_lags=N_lags,date_freq=12,y_names=y_names,slct_method = slct_method) # Run function to get data and formulas for VAR(p).
ts_var <- result$ts_var             # Get the time series data.
formula_reg <- result$formula_reg   # Get all OLS regression formulas related to the VAR(p).
y_date <- result$y_date             # Update the vector of time-periods after creating lagged variables in the sample.
rm(result)

## Run the VAR model:

varesult <- run.var(ts_var=ts_var,formula_reg=formula_reg,y_names=y_names,N_lags=N_lags) # Run VAR(p) model.
B_mat <- varesult$B_mat         # Save slope coefficients here.
C_mat <- varesult$C_mat         # Save intercepts here.
w_mat <- varesult$w_mat         # Save reduced-form residuals here.
y_hat_mat <- varesult$y_hat_mat # Save fitted-values here.
Sigma_mat <- cov(w_mat)         # Variance-covariance matrix of reduced-form residuals.

## Run impulse response functions:

result <- run.PHI(y_names=y_names,N_lags=N_lags,B_mat=B_mat,irf_periods=irf_periods)
phi_function <- result$phi_function # phi_function[[1]]: IRF of t=0; phi_function[[k]]: IRF of k-1.
rm(result)

## Impact matrix estimation:

if (slct_method=="reduced_form"){
  A_mat <- diag(N_vars)           # In the reduced-form methodology, A_mat is just a diagonal matrix.
} else if (slct_method=="structural"){
  result <- run.PHI2(y_names=y_names,N_m=N_m,N_c=N_c,B_mat=B_mat,irf_periods=irf_periods,phi_function=phi_function,Sigma_mat=Sigma_mat)
  A_mat <- result$A_mat
  # run.PHI2 here for structural methodology.
  # ...
}
colnames(A_mat) <- y_names
rownames(A_mat) <- y_names

## Impact matrix of MP shocks estimation:
# Important: We restrict regressions with MP shocks to series before February 2020 (also see Swanson).

shock_series <- z_data[ANNDATE<="Feb 2020"]

result <- run.MPS(y_names,z_names,y_date,w_mat,z_data=shock_series)

omega_mat <- result$omega_mat # Save impact matrix of MP shocks.

## IRF of a 1p.p. positive sentiment shock:

result <- run.IRF(phi_function,A_mat,y_names,scale_var="BD",scale_var_mag=1,irf_periods,slct_method)
irf_mat <- result$irf_mat

SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_mat <- result$irf_mat

## IRF of a MP shock that causes unemployment to increase by 1p.p. on impact:
# result <- run.IRF.MPS(phi_function,omega_mat,y_names,z_names,scale_var="UNEMP",scale_var_mag=1,irf_periods,slct_method)

## IRF of a MP shock that causes CPI to fall by 1p.p. on impact:

result <- run.IRF.MPS(phi_function,omega_mat,y_names,z_names,scale_var=c("TREAS","TREAS","TREAS"),scale_var_mag=c(1,1,1),irf_periods,slct_method)

irf_mat_mp <- result$irf_mat_mp

SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_mat_mp <- result$irf_mat_mp

## Run counterfactual policies:

result <- run.PHIM(y_names,z_names,phi_function,welf_horizon=welf_horizon,A_mat,omega_mat,slct_method,irf_mat,irf_mat_mp)
phi_r <- result$phi_r
phi_m <- result$phi_m

# slct_lambda <- 1/2

result <- run.COUNTERF(lambda_coef=slct_lambda,welf_horizon=welf_horizon,phi_r,phi_m,z_names,slct_mpshocktypes)
psi_coef_mat <- result$psi_coef_mat
r_sq_mat <- result$r_sq_mat

SAVE_RESULTS[[slct_robust]][[slct_method]]$psi_coef_mat <- result$psi_coef_mat
SAVE_RESULTS[[slct_robust]][[slct_method]]$r_sq_mat <- result$r_sq_mat

## Run Bootstrap algorithm:

result <- run.BOOTSTRAP(y_names,w_mat,y_hat_mat,ts_var,data_set,y_date,N_lags,N_m,N_c,formula_reg,
                        shock_vector,omega_mat,lambda_coef=slct_lambda,
                        irf_periods=irf_periods,welf_horizon=welf_horizon,slct_method,
                        slct_bootruns=slct_bootruns,slct_mpshocktypes=slct_mpshocktypes,
                        bootstrap_method,irf_mat,irf_mat_mp) #seed=500

irf_boot <- result$irf_boot
irf_mp_boot <- result$irf_mp_boot
psi_boot <- result$psi_boot

SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_boot <- result$irf_boot
SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_mp_boot <- result$irf_mp_boot
SAVE_RESULTS[[slct_robust]][[slct_method]]$psi_boot <- result$psi_boot
PHI_R_BOOT <- result$PHI_R_BOOT
PHI_M_BOOT <- result$PHI_M_BOOT

## Create a matrix to save all bootstrap standard errors for optimal psi:
SAVE_RESULTS[[slct_robust]][[slct_method]]$se <- SAVE_RESULTS[[slct_robust]][[slct_method]]$psi_coef_mat
SAVE_RESULTS[[slct_robust]][[slct_method]]$se[,] <- as.numeric(NA)


## Save standard-error results from bootstrap iterations:
for (i in 1:nrow(SAVE_RESULTS[[slct_robust]][[slct_method]]$se) ){
  for (j in 1:ncol(SAVE_RESULTS[[slct_robust]][[slct_method]]$se) ){
    SAVE_RESULTS[[slct_robust]][[slct_method]]$se[i,j] <- round(  sd(sapply(1:slct_bootruns, function(k) SAVE_RESULTS[[slct_robust]][[slct_method]]$psi_boot[[k]][i,j]),na.rm = TRUE) , slct_dcml)
    }
}

result <- run.IRF.COUNT(phi_function,A_mat,omega_mat,y_names,z_names,psi_coef_mat=SAVE_RESULTS[[slct_robust]][[slct_method]]$psi_coef_mat
                ,scale_var="BD",scale_var_mp="TREAS",scale_var_mag=1,scale_var_mp_mag=1,irf_periods=irf_periods,slct_method,slct_mpshocktypes)

SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_count <- result$irf_count
