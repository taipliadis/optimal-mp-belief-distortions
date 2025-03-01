################################################################################
# PLOT IRF FROM COUNTERFACTUAL POLICIES (structural methodology)
################################################################################
# Note: Ignore warnings(), this is just because we add a shape on each line but with steps to make the graph readable. 
#> For most observations we give NA on shape and it pops up as warnings.
#-------------------------------------------------------------------------------

slct_color <- c("No Response" = "blue","Target Rate" = "red","Forward Guidance" = "darkblue","LSAP" = "grey","All 3 Tools" = "green")
slct_shape <- c("No Response"=4,"Target Rate"=24,"Forward Guidance"=21,"LSAP"=22,"All 3 Tools"=23)
slct_linetype <- c("No Response" = "solid","Target Rate" = "longdash","Forward Guidance" = "longdash","LSAP" = "longdash","All 3 Tools" = "solid")

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
    IRF = as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_mat)[, var_idx],
    COUNT_FFR = as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_count$indep_ffr)[, var_idx], # Optimal Policy (FFR)
    COUNT_FG = as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_count$indep_fg)[, var_idx], # Optimal Policy (FG)
    COUNT_LSAP = as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_count$indep_lsap)[, var_idx], # Optimal Policy (LSAP)
    COUNT_ALL = as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_count$all_tools)[, var_idx], # Optimal Policy (All 3 Tools)
    ORDER = var_idx,
    VARIABLE = y_names_plot[var_idx],
    NAME = y_dnames_plot[var_idx]
  )
})

dt_irf <- do.call(rbind, dt_irf) # Combine all data frames into one.

#-------------------------------------------------------------------------------
# NEW: Add INFLATION IRFs not CPI:
SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_mat
save_temp_table <- as.data.table( SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_mat )
init_val <- save_temp_table[1,]$CPI
save_temp_table[,INF:=CPI-shift(CPI,n=1)]
save_temp_table[,INF:=ifelse(is.na(INF),init_val,INF)]

save_temp_table_2 <- as.data.table( as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_count$indep_ffr) )
init_val <- save_temp_table_2[1,]$CPI
save_temp_table_2[,INF:=CPI-shift(CPI,n=1)]
save_temp_table_2[,INF:=ifelse(is.na(INF),init_val,INF)]

save_temp_table_3 <- as.data.table( as.data.frame(SAVE_RESULTS[[slct_robust]][[slct_method]]$irf_count$all_tools) )
init_val <- save_temp_table_3[1,]$CPI
save_temp_table_3[,INF:=CPI-shift(CPI,n=1)]
save_temp_table_3[,INF:=ifelse(is.na(INF),init_val,INF)]

dt_irf_2 <- dt_irf # IMPORTANT: in dt_irf_2 just print INF instead of CPI.
dt_irf_2[VARIABLE=="CPI"]$IRF <- save_temp_table$INF
dt_irf_2[VARIABLE=="CPI"]$NAME <- "Inflation"
dt_irf_2[VARIABLE=="CPI"]$COUNT_FFR <- save_temp_table_2$INF
dt_irf_2[VARIABLE=="CPI"]$COUNT_ALL <- save_temp_table_3$INF
#-------------------------------------------------------------------------------

combined_plot <- list() # Create an empty list to save all plots.

# The following loop reports IRF with CI for all response variables:
for (i in 1:N_vars_plot){
    temp_plot <- ggplot(data = dt_irf[ORDER==i & PERIOD<=irf_periods_plot]) +
    geom_line(aes(x = PERIOD, y = IRF, color = "No Response", linetype = "No Response"), size = 1.2) + 
    geom_line(aes(x = PERIOD, y = COUNT_FFR, color = "Target Rate", linetype = "Target Rate"), size = 1.2) +
    geom_line(aes(x = PERIOD, y = COUNT_FG, color = "Forward Guidance", linetype = "Forward Guidance"), size = 1.2) +
    geom_line(aes(x = PERIOD, y = COUNT_LSAP, color = "LSAP", linetype = "LSAP"), size = 1.2) +
    geom_line(aes(x = PERIOD, y = COUNT_ALL, color = "All 3 Tools", linetype = "All 3 Tools"), size = 1.2) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(0,48,12) ,IRF,NA), color = "No Response", shape = "No Response", fill = "No Response"), size = 3) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(1,48,9) ,COUNT_FFR,NA), color = "Target Rate", shape = "Target Rate", fill = "Target Rate"), size = 3) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(2,48,9) ,COUNT_FG,NA), color = "Forward Guidance", shape = "Forward Guidance", fill = "Forward Guidance"), size = 3) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(3,48,9) ,COUNT_LSAP,NA), color = "LSAP", shape = "LSAP", fill = "LSAP"), size = 3) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(1,48,6) ,COUNT_ALL,NA), color = "All 3 Tools", shape = "All 3 Tools", fill = "All 3 Tools"), size = 3) +
    theme_classic() +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(nrow(dt_irf)), by = 12)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + # Format y-axis labels
    labs(title = dt_irf[ORDER==i]$NAME, y = "", x = "", color = "Counterfactuals:",linetype = "Counterfactuals:", shape = "Counterfactuals:", fill = "Counterfactuals:") +
    scale_shape_manual(values = slct_shape)+
    scale_color_manual(values = slct_color) +
    scale_fill_manual(values = slct_color) +
    scale_linetype_manual(values = slct_linetype) +
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

#-------------------------------------------------------------------------------
# Run again the plots and change the position of the titles to add in the paper:
#-------------------------------------------------------------------------------

combined_plot_2 <- list() # Create an empty list to save all plots.
show_plots <- c(N_vars+2,which(y_names=="CPI"),which(y_names=="UNEMP"))
for (i in show_plots){
  temp_plot <- ggplot(data = dt_irf_2[ORDER==i & PERIOD<=irf_periods_plot]) +
    geom_line(aes(x = PERIOD, y = IRF, color = "No Response", linetype = "No Response"), size = 1.2) + 
    geom_line(aes(x = PERIOD, y = COUNT_FFR, color = "Target Rate", linetype = "Target Rate"), size = 1.2) +
    geom_line(aes(x = PERIOD, y = COUNT_ALL, color = "All 3 Tools", linetype = "All 3 Tools"), size = 1.2) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(0,48,12) ,IRF,NA), color = "No Response", shape = "No Response", fill = "No Response"), size = 3) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(1,48,9) ,COUNT_FFR,NA), color = "Target Rate", shape = "Target Rate", fill = "Target Rate"), size = 3) +
    geom_point(aes(x = PERIOD, y = ifelse( PERIOD %in% seq(1,48,6) ,COUNT_ALL,NA), color = "All 3 Tools", shape = "All 3 Tools", fill = "All 3 Tools"), size = 3) +
    theme_classic() +
    geom_hline(yintercept = 0, size = 1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, max(nrow(dt_irf_2)), by = 6)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1),
                       limits = c(ifelse(i==N_vars_plot,-0.5, ifelse(i==2, -0.4 , ifelse(i==4, -0.2, NA ) ) ),
                                  ifelse(i==N_vars_plot, 1.15, ifelse(i==2, 0.1 , ifelse(i==4, 0.7, NA) ) ) ) ) + # Format y-axis labels
    labs(title = ifelse(i==(N_vars+2),"Structural"," "), y = dt_irf_2[ORDER==i]$NAME, x = "", color = "Counterfactuals:",linetype = "Counterfactuals:", shape = "Counterfactuals:", fill = "Counterfactuals:") +
    scale_shape_manual(values = slct_shape)+
    scale_color_manual(values = slct_color) +
    scale_fill_manual(values = slct_color) +
    scale_linetype_manual(values = slct_linetype) +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 14, hjust = 0.5))
  plot_name <- y_names_plot[i]
  combined_plot_2[[plot_name]] <- temp_plot
  rm(temp_plot)
}
rm(i)
SAVE_PLOTS[[slct_method]][["COUNT_SELECT"]] <- combined_plot_2

SAVE_PLOTS[[slct_method]][["COUNT_SELECT"]] <- combined_plot_2

SAVE_PLOTS[[slct_method]][["COUNT"]] <- do.call(ggarrange, c( combined_plot , list(ncol = 3,nrow = 2, common.legend = TRUE,legend = "bottom") ) )

# Note: Ignore warnings(), this is just because we add a shape on each line but with steps to make the graph readable. 
#> For most observations we give NA on shape and it pops up as warnings.