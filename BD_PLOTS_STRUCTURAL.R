################################################################################
# PLOT IRF TO BELIEF DISTORTION SHOCKS (structural methodology)
################################################################################

N_vars_plot <- N_vars + 2

y_names_plot <- c(y_names,"RE_CPI","BD")
y_dnames_plot <- c(y_dnames,"Rational Expectation","Belief Distortions")

irf_vals <- list()

# Iterate over each column index:
for (xc in 1:N_vars_plot) {
  columns_list <- list() # Store impulse responses here.
  for (bc in 1:slct_bootruns) {
    columns_list[[bc]] <- irf_boot[[bc]][, xc] # Extract column bc corresponding to bc-th bootstrap iteration.
  }
  irf_vals[[xc]] <- do.call(cbind, columns_list) # Combine columns into a matrix.
}
rm(xc,bc,columns_list)

IRF_CI <- list() # Store confidence intervals here.
IRF_CI_10 <- list() # Store confidence intervals here.

alpha <- 1-slct_ci # Select significance level.
alpha_2 <- (1-slct_ci)*2 # Select significance level.

for (xc in 1:N_vars_plot) {
  IRF_CI[[xc]] <- matrix(0,nrow =(irf_periods+1),ncol = 2 ) # Create a matrix inside the list to store confidence intervals.
  colnames(IRF_CI[[xc]]) <- c("UP","LO") # Name columns corresponding to upper and lower bounds of CI.
  IRF_CI_10[[xc]] <- matrix(0,nrow =(irf_periods+1),ncol = 2 ) # Create a matrix inside the list to store confidence intervals.
  colnames(IRF_CI_10[[xc]]) <- c("UP","LO") # Name columns corresponding to upper and lower bounds of CI.
  for (tc in 1:(irf_periods+1) ) {
    IRF_CI[[xc]][tc,1] <-  as.numeric( quantile(irf_vals[[xc]][tc,], 1-alpha/2 ) )  # Find Upper-Bound CI (ie, 0.975).
    IRF_CI[[xc]][tc,2] <- as.numeric( quantile(irf_vals[[xc]][tc,], alpha/2 ) )   # Find Lower-Bound CI (ie, 0.025).
    IRF_CI_10[[xc]][tc,1] <-  as.numeric( quantile(irf_vals[[xc]][tc,], 1-alpha_2/2 ) )  
    IRF_CI_10[[xc]][tc,2] <- as.numeric( quantile(irf_vals[[xc]][tc,], alpha_2/2 ) )   
  }
}
rm(xc,tc)

dt_irf <- lapply(1:N_vars_plot, function(var_idx) {
  data.table(
    PERIOD = 0:irf_periods,
    UPPER = as.data.frame(IRF_CI[[var_idx]])[,1],
    LOWER = as.data.frame(IRF_CI[[var_idx]])[,2],
    UPPER_10 = as.data.frame(IRF_CI_10[[var_idx]])[,1],
    LOWER_10 = as.data.frame(IRF_CI_10[[var_idx]])[,2],
    # IRF = as.data.frame(irf_mat)[, var_idx],
    IRF = as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_mat)[, var_idx],
    ORDER = var_idx,
    VARIABLE = y_names_plot[var_idx],
    NAME = y_dnames_plot[var_idx]
  )
})

dt_irf <- do.call(rbind, dt_irf) # Combine all data frames into one.

#-------------------------------------------------------------------------------
combined_plot <- list() # Create an empty list to save all plots.

# The following loop reports IRF with CI for all response variables:
for (i in 1:N_vars_plot){
  temp_plot <- ggplot(data = dt_irf[ORDER==i & PERIOD<=irf_periods_plot]) +
    geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = UPPER_10), fill = "blue", alpha = 0.20) +
    geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = LOWER), fill = "blue", alpha = 0.10) +
    geom_ribbon(aes(x = PERIOD, ymin = UPPER, ymax = UPPER_10), fill = "blue", alpha = 0.10) +
    geom_line(aes(x = PERIOD, y = IRF ), color="blue",size = 1 ,linetype = "solid") + 
    theme_classic() + #theme_minimal() +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max( nrow(dt_irf) ),by=12)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + # Format y-axis labels
    
    labs(title = dt_irf[ORDER==i]$NAME, y = "", x = "")  + 
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 14, hjust = 0.5))
  plot_name <- y_names_plot[i]
  combined_plot[[plot_name]] <- temp_plot
  rm(temp_plot)
}
rm(i)

bd_position <- length(combined_plot)
combined_plot[[1]] <- combined_plot[[bd_position]]
combined_plot[[bd_position]] <- NULL
combined_plot[[(bd_position-1)]] <- NULL

SAVE_PLOTS[[slct_method]][["BD"]] <- do.call(ggarrange, c(combined_plot, list(ncol = 3, nrow=2) )  )  # Combine all plots in one figure.
SAVE_PLOTS[[slct_method]][["BD"]]

#-------------------------------------------------------------------------------
# Run again the plots and change the position of the titles to add in the paper:
#-------------------------------------------------------------------------------

combined_plot_2 <- list() # Create an empty list to save all plots.

# The following loop reports IRF with CI for all response variables:
for (i in 1:N_vars_plot){
  temp_plot <- ggplot(data = dt_irf[ORDER==i & PERIOD<=irf_periods_plot]) +
    geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = UPPER_10), fill = "blue", alpha = 0.20) +
    geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = LOWER), fill = "blue", alpha = 0.10) +
    geom_ribbon(aes(x = PERIOD, ymin = UPPER, ymax = UPPER_10), fill = "blue", alpha = 0.10) +
    geom_line(aes(x = PERIOD, y = IRF ), color="blue",size = 1 ,linetype = "solid") + 
    theme_classic() + #theme_minimal() +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max( nrow(dt_irf) ),by=6)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                       limits = c(ifelse(i==N_vars_plot,-0.4, ifelse(i==2, -3 , ifelse(i==4, -0.8, NA ) ) ),
                                  ifelse(i==N_vars_plot, 1.15, ifelse(i==2, 0.1 , ifelse(i==4, 1.3, NA) ) ) ) ) + # Format y-axis labels
    labs(title = ifelse(i==N_vars_plot,"Structural"," "), y = dt_irf[ORDER==i]$NAME, x = "")  +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 14, hjust = 0.5))
  plot_name <- y_names_plot[i]
  combined_plot_2[[plot_name]] <- temp_plot
  rm(temp_plot)
}
rm(i)

SAVE_PLOTS[[slct_method]][["BD_SELECT"]] <- combined_plot_2
