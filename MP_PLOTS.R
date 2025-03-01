################################################################################
# PLOT IRF TO MONETARY POLICY SHOCKS (reduced-form methodology)
################################################################################

z_dpnames <- c("Target Rate","Forward Guidance","LSAP") #Use these names on plots.

y_dpnames <- y_dnames

combined_superplot <- list() # Create an empty list to save all plots.
combined_superplot_b <- list() # Create an empty list to save all plots.
plot_number = 0
plot_number_b = 0

for (mj in 1:3){
  N_vars_plot <- N_vars
  
  y_names_plot <- c(y_names)
  y_dnames_plot <- c(y_dpnames)
  
  irf_vals <- list()
  
  # Iterate over each column index:
  for (xc in 1:N_vars_plot) {
    columns_list <- list() # Store impulse responses here.
    for (bc in 1:slct_bootruns) {
      columns_list[[bc]] <- irf_mp_boot[[bc]][[mj]][, xc] # Extract column bc corresponding to bc-th bootstrap iteration.
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
      
      IRF_CI_10[[xc]][tc,1] <-  as.numeric( quantile(irf_vals[[xc]][tc,], 1-alpha_2/2 ) )  # Find Upper-Bound CI (ie, 0.975).
      IRF_CI_10[[xc]][tc,2] <- as.numeric( quantile(irf_vals[[xc]][tc,], alpha_2/2 ) )   # Find Lower-Bound CI (ie, 0.025).
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
      IRF = as.data.frame(irf_mat_mp[[mj]])[, var_idx],
      ORDER = var_idx,
      VARIABLE = y_names_plot[var_idx],
      NAME = y_dnames_plot[var_idx]
    )
  })
  
  dt_irf <- do.call(rbind, dt_irf) # Combine all data frames into one.
  
  #-----------------------------------------------------------------------------
  # Print selected plots for the paper.
  #-----------------------------------------------------------------------------
  
  title_posit <- which(y_names=="TREAS")
  second_posit <- which(y_names=="CPI")
  bottom_posit <- which(y_names=="UNEMP")

  show_plots <- c(title_posit,second_posit,bottom_posit)
  for (i in show_plots){
    plot_number <- plot_number + 1
    temp_plot <- ggplot(data = dt_irf[ORDER==i & PERIOD<=irf_periods_plot]) +
      geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = UPPER_10), fill = "blue", alpha = 0.20) +
      geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = LOWER), fill = "blue", alpha = 0.10) +
      geom_ribbon(aes(x = PERIOD, ymin = UPPER, ymax = UPPER_10), fill = "blue", alpha = 0.10) +
      geom_line(aes(x = PERIOD, y = IRF ), color="blue",size = 1 ,linetype = "solid") + 
      theme_classic() + #theme_minimal() +
      geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, max( nrow(dt_irf) ),by=12)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + # Format y-axis labels
      labs(title = ifelse(i==title_posit,z_dpnames[mj],""), y = ifelse(mj==1,dt_irf[ORDER==i]$NAME,""), x = "")  + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 14, hjust = 0.5))
    combined_superplot[[plot_number]] <- temp_plot
    rm(temp_plot)
  }
  rm(i)
  
  #-----------------------------------------------------------------------------
  # Run again the plots: This time, print all plots.
  #-----------------------------------------------------------------------------
  
  title_posit <- 1
  show_plots_b <- c(1:length(y_names))
  for (i in show_plots_b){
    plot_number_b <- plot_number_b + 1
    temp_plot <- ggplot(data = dt_irf[ORDER==i & PERIOD<=irf_periods_plot]) +
      geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = UPPER_10), fill = "blue", alpha = 0.20) +
      geom_ribbon(aes(x = PERIOD, ymin = LOWER_10, ymax = LOWER), fill = "blue", alpha = 0.10) +
      geom_ribbon(aes(x = PERIOD, ymin = UPPER, ymax = UPPER_10), fill = "blue", alpha = 0.10) +
      geom_line(aes(x = PERIOD, y = IRF ), color="blue",size = 1 ,linetype = "solid") + 
      theme_classic() + #theme_minimal() +
      geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
      scale_x_continuous(breaks = seq(0, max( nrow(dt_irf) ),by=12)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + # Format y-axis labels
      labs(title = ifelse(i==title_posit,z_dpnames[mj],""), y = ifelse(mj==1,dt_irf[ORDER==i]$VARIABLE,""), x = "")  + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 14, hjust = 0.5))
    combined_superplot_b[[plot_number_b]] <- temp_plot
    rm(temp_plot)
  }
  rm(i)
}

combined_superplot_matrix <- matrix(combined_superplot, nrow = length(show_plots), ncol = 3, byrow = FALSE)
combined_superplot_column_major <- as.vector(t(combined_superplot_matrix))
figure_comb <- do.call(ggarrange, c(combined_superplot_column_major, list(ncol = 3, nrow = 3 )))
SAVE_PLOTS[[slct_method]][["MP_SELECT"]] <- figure_comb

combined_superplot_matrix <- matrix(combined_superplot_b, nrow = length(show_plots_b), ncol = 3, byrow = FALSE)
combined_superplot_column_major <- as.vector(t(combined_superplot_matrix))
figure_comb <- do.call(ggarrange, c(combined_superplot_column_major, list(ncol = 3, nrow = length(show_plots_b) )))
SAVE_PLOTS[[slct_method]][["MP"]] <- figure_comb
