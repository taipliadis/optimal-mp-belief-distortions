#===============================================================================
# Read readme.txt
#===============================================================================

rm(list = ls())                     # Empty Environment.
cat("\014")                         # Empty Console.
Project_directory <- getwd()        # Set working directory.

start_time <- Sys.time()

source("LOAD_LIBRARIES.R")          # Load Libraries.
source("RUN_FUNCTIONS.R")           # Run Functions.

################################################################################
# 1. BASELINE RESULTS
################################################################################

slct_robust <- "baseline"   # Start with the baseline results first.

SAVE_RESULTS <- list()      # Save all results for table 1 in this list.
SAVE_PLOTS <- list()        # Save all plots in this list.

for ( slct_method in c("reduced_form","structural") ){
    source("PARAMETER_SETTINGS.R")        # Set parameters.
    
    load("baseline_data.RData")           # Load data.
    
    source("RATIONAL_EXPECTATIONS.R")     # Identify reduced-form belief distortions.
    
    source("ORTHOG_MPS.R")                # Orthogonalize MP shocks.
    
    source("VAR_MODEL.R")                 # Run VAR model.
  
    if (slct_method=="reduced_form"){     # Generate Reduced-form methodology plots:
      source("BD_PLOTS.R")                # Figure 1.
      source("MP_PLOTS.R")                # Figure 3.
      source("COUNT_PLOTS.R")
    } else if (slct_method=="structural"){# Generate structural methodology plots:
      source("BD_PLOTS_STRUCTURAL.R")     # Figure 2.
      source("MP_PLOTS_STRUCTURAL.R")
      source("COUNT_PLOTS_STRUCTURAL.R")
    }
}
print(paste(slct_robust,"done!"))

# Estimated time required to run baseline results: 
#> 100 bootstrap iterations in less than 1 min; 
#> 1,000 bootstrap iterations in about 4 min; 
#> 10,000 bootstrap iterations in about 55 mins.

#-------------------------------------------------------------------------------
# Print Plots
#-------------------------------------------------------------------------------

## Figure 1:
combined_plot <- list()
combined_plot[[1]] <- SAVE_PLOTS$structural$BD_SELECT$BD
combined_plot[[2]] <- SAVE_PLOTS$reduced_form$BD_SELECT$BD
combined_plot[[3]] <- SAVE_PLOTS$structural$BD_SELECT$CPI
combined_plot[[4]] <- SAVE_PLOTS$reduced_form$BD_SELECT$CPI
combined_plot[[5]] <- SAVE_PLOTS$structural$BD_SELECT$UNEMP
combined_plot[[6]] <- SAVE_PLOTS$reduced_form$BD_SELECT$UNEMP
figure_1 <- do.call(ggarrange, c( combined_plot , list(ncol = 2, nrow=3, common.legend = TRUE,legend = "bottom") ) )

## Figure 2:
figure_2 <- SAVE_PLOTS$structural$MP_SELECT

## Figure 3:
combined_plot <- list()
combined_plot[[1]] <- SAVE_PLOTS$structural$COUNT_SELECT$BD
combined_plot[[2]] <- SAVE_PLOTS$reduced_form$COUNT_SELECT$BD
combined_plot[[3]] <- SAVE_PLOTS$structural$COUNT_SELECT$CPI
combined_plot[[4]] <- SAVE_PLOTS$reduced_form$COUNT_SELECT$CPI
combined_plot[[5]] <- SAVE_PLOTS$structural$COUNT_SELECT$UNEMP
combined_plot[[6]] <- SAVE_PLOTS$reduced_form$COUNT_SELECT$UNEMP
figure_3 <- do.call(ggarrange, c( combined_plot , list(ncol = 2, nrow=3, common.legend = TRUE,legend = "bottom") ) )

## Plots at Online Appendix:
figure_online_1 <- SAVE_PLOTS$reduced_form$BD # Appendix.
figure_online_2 <- SAVE_PLOTS$structural$BD # Appendix.
figure_online_3 <- SAVE_PLOTS$reduced_form$COUNT # Appendix.
figure_online_4 <- SAVE_PLOTS$structural$COUNT # Appendix.
figure_online_5 <- SAVE_PLOTS$reduced_form$MP # Appendix.
figure_online_6 <- SAVE_PLOTS$structural$MP # Appendix.

## Save all plots:
setwd(paste0(Project_directory,"/PLOTS")) # Save plots at PLOTS folder.
ggsave( paste0("figure_1_boot_",slct_bootruns,".pdf"), plot = figure_1, width = 8, height = 8, units = "in")
ggsave( paste0("figure_2_boot_",slct_bootruns,".pdf"), plot = figure_2, width = 8, height = 8, units = "in")
ggsave( paste0("figure_3_boot_",slct_bootruns,".pdf"), plot = figure_3, width = 8, height = 8, units = "in")
ggsave( paste0("figure_O1_reduced_form_boot_",slct_bootruns,".pdf"), plot = figure_online_1, width = 8, height = 6, units = "in")
ggsave( paste0("figure_O2_structural_boot_",slct_bootruns,".pdf"), plot = figure_online_2, width = 8, height = 6, units = "in")
ggsave( paste0("figure_O3_reduced_form_boot_",slct_bootruns,".pdf"), plot = figure_online_3, width = 8, height = 6, units = "in")
ggsave( paste0("figure_O4_structural_boot_",slct_bootruns,".pdf"), plot = figure_online_4, width = 8, height = 6, units = "in")
ggsave( paste0("figure_O5_reduced_form_boot_",slct_bootruns,".pdf"), plot = figure_online_5, width = 8, height = 12, units = "in")
ggsave( paste0("figure_O6_structural_boot_",slct_bootruns,".pdf"), plot = figure_online_6, width = 8, height = 12, units = "in")
setwd(paste0(Project_directory)) # Return to working directory.

print(figure_1)
print(figure_2)
print(figure_3)

#-----------------------------------------------------------------------------
# Print Table 1 (in latex form)
#-----------------------------------------------------------------------------

source("TABLE_1.R")

################################################################################
# 2. ROBUSTNESS TESTS
################################################################################

# name list of robustness tests:
robust_list <- c("lambda_0","lambda_1","lags_3","lags_12","lags_rat_12",
                 "drop_covid","welf_horizon_24","welf_horizon_120",
                 "small_var","aruoba_mps")

start_time_robust <- Sys.time()
for (slct_robust in robust_list){
  source("PARAMETER_SETTINGS.R")      # Reset parameters.
  load("baseline_data.RData")         # Load data.
  
  if( slct_robust=="aruoba_mps"){
  load("aruoba_data.RData")           # Load data for Robustness Test #11 of Table 2.
  }
  
  source("RATIONAL_EXPECTATIONS.R")   # Identify reduced-form belief distortions.
  source("ORTHOG_MPS.R")              # Orthogonalize MP shocks.
  
  for ( slct_method in c("reduced_form","structural") ){
    source("VAR_MODEL.R")                 # Run VAR model.
  }
  print(paste(slct_robust,"done!"))
}

#-----------------------------------------------------------------------------
# Print Table 2 (in latex form)
#-----------------------------------------------------------------------------

source("TABLE_2.R")

end_time <- Sys.time()
total_time <- end_time - start_time

print(paste("Replication code has finished."))
print(total_time)
