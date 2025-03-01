################################################################################
# TABLE 2
################################################################################

cat(paste0(
  "\\begin{table}[] \n",
  "\\centering \n",
  "\\caption{Robustness Tests} \n",
  "\\label{tab_robustness_tests} \n",
  "\\resizebox{1","\\hsize}{!}{%","\\columnwidth","\\linewidth\n",
  "\\begin{adjustbox}{angle=00}\n",
  "\\begin{tabular}{l  c c  c c }\n",
  "\\hline\n",
  "\\hline","\\\\[-1.8ex]\n",
  " & ","\\multicolumn{2}{c}{Structural}  &"," \\multicolumn{2}{c}{Reduced-form}","\\\\ \n",
  "\\hline","\\\\[-1.8ex] \n",
  " & Target & ","$R^2$ & Target & ","$R^2$ ","\\\\ \n",
  
  " Baseline Model & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$baseline$structural$r_sq_mat[1]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " & (",SAVE_RESULTS$baseline$structural$se[1,1],") &  & (",SAVE_RESULTS$baseline$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " Inflation Targeting ($\\lambda=0$) & ",SAVE_RESULTS$lambda_0$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lambda_0$structural$r_sq_mat[1]," & ",SAVE_RESULTS$lambda_0$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lambda_0$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " & (",SAVE_RESULTS$lambda_0$structural$se[1,1],") &  & (",SAVE_RESULTS$lambda_0$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " Employment Targeting ($\\lambda=1$) & ",SAVE_RESULTS$lambda_1$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lambda_1$structural$r_sq_mat[1]," & ",SAVE_RESULTS$lambda_1$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lambda_1$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " & (",SAVE_RESULTS$lambda_1$structural$se[1,1],") &  & (",SAVE_RESULTS$lambda_1$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " VAR with 3 lags & ",SAVE_RESULTS$lags_3$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lags_3$structural$r_sq_mat[1]," & ",SAVE_RESULTS$lags_3$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lags_3$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " & (",SAVE_RESULTS$lags_3$structural$se[1,1],") &  & (",SAVE_RESULTS$lags_3$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " VAR with 12 lags & ",SAVE_RESULTS$lags_12$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lags_12$structural$r_sq_mat[1]," & ",SAVE_RESULTS$lags_12$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lags_12$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " & (",SAVE_RESULTS$lags_12$structural$se[1,1],") &  & (",SAVE_RESULTS$lags_12$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " Belief Distortion estimation  & - & - & ",SAVE_RESULTS$lags_rat_12$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$lags_rat_12$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " with 12 lags & - &  & (",SAVE_RESULTS$lags_rat_12$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " Excl. COVID-19 Era & ",SAVE_RESULTS$drop_covid$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$drop_covid$structural$r_sq_mat[1]," & ",SAVE_RESULTS$drop_covid$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$drop_covid$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " & (",SAVE_RESULTS$drop_covid$structural$se[1,1],") &  & (",SAVE_RESULTS$drop_covid$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " 24-Month Truncation of & ",SAVE_RESULTS$welf_horizon_24$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$welf_horizon_24$structural$r_sq_mat[1]," & ",SAVE_RESULTS$welf_horizon_24$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$welf_horizon_24$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " Welfare Objective & (",SAVE_RESULTS$welf_horizon_24$structural$se[1,1],") &  & (",SAVE_RESULTS$welf_horizon_24$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " 120-Month Truncation of & ",SAVE_RESULTS$welf_horizon_120$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$welf_horizon_120$structural$r_sq_mat[1]," & ",SAVE_RESULTS$welf_horizon_120$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$welf_horizon_120$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " Welfare Objective & (",SAVE_RESULTS$welf_horizon_120$structural$se[1,1],") &  & (",SAVE_RESULTS$welf_horizon_120$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " Small VAR & ",SAVE_RESULTS$small_var$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$small_var$structural$r_sq_mat[1]," & ",SAVE_RESULTS$small_var$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$small_var$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " & (",SAVE_RESULTS$small_var$structural$se[1,1],") &  & (",SAVE_RESULTS$small_var$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  " Aruoba-Dreschel Monetary & ",SAVE_RESULTS$aruoba_mps$structural$psi_coef_mat[1,1]," & ",SAVE_RESULTS$aruoba_mps$structural$r_sq_mat[1]," & ",SAVE_RESULTS$aruoba_mps$reduced_form$psi_coef_mat[1,1]," & ",SAVE_RESULTS$aruoba_mps$reduced_form$r_sq_mat[1]," ","\\\\ \n",
  " Policy Shock & (",SAVE_RESULTS$aruoba_mps$structural$se[1,1],") &  & (",SAVE_RESULTS$aruoba_mps$reduced_form$se[1,1],") & ","\\\\[1.8ex] \n",
  
  "\\hline \n",
  "\\hline ","\\\\[-1.8ex] \n",
  
  "\\end{tabular} \n",
  "\\end{adjustbox} \n",
  "} \n",
  "\\end{table} \n"
))












