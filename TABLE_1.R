################################################################################
# TABLE 1
################################################################################

cat(paste0(
"\\begin{table}[] \n",
"\\centering \n",
"\\caption{Optimal Monetary Policy Response} \n",
"\\label{tab_optimal_policies} \n",
"\\resizebox{1","\\hsize}{!}{%","\\columnwidth","\\linewidth\n",
"\\begin{adjustbox}{angle=00}\n",
"\\begin{tabular}{l  c c c c  c c c c}\n",
"\\hline\n",
"\\hline","\\\\[-1.8ex]\n",
" & ","\\multicolumn{4}{c}{Structural Methodology}  &"," \\multicolumn{4}{c}{Reduced-form Methodology}","\\\\ \n",
"\\hline","\\\\[-1.8ex] \n",
" & Target & FG & LSAP & ","$R^2$ & Target & FG & LSAP & ","$R^2$ ","\\\\ \n",

"Independent Tools & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[1,1]," &  &  & ",SAVE_RESULTS$baseline$structural$r_sq_mat[1]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[1,1]," &  &  & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[1]," ","\\\\ \n",
" & (",SAVE_RESULTS$baseline$structural$se[1,1],") &  &  &  & (",SAVE_RESULTS$baseline$reduced_form$se[1,1],") &  &  & ","\\\\[1.8ex] \n",

" &  & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[2,2]," &  & ",SAVE_RESULTS$baseline$structural$r_sq_mat[2]," &  & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[2,2]," &  & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[2]," ","\\\\ \n",
" &  & (",SAVE_RESULTS$baseline$structural$se[2,2],") &  &  &  & (",SAVE_RESULTS$baseline$reduced_form$se[2,2],") &  & ","\\\\[1.8ex]  \n",

" &  &  & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[3,3]," & ",SAVE_RESULTS$baseline$structural$r_sq_mat[3]," &  &  & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[3,3]," & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[3]," ","\\\\ \n",
" &  &  & (",SAVE_RESULTS$baseline$structural$se[3,3],") &  &  &  & (",SAVE_RESULTS$baseline$reduced_form$se[3,3],") & ","\\\\[1.8ex] \n",

"\\hline ","\\\\[-1.8ex] \n",

"Pairwise Tools   &  ",SAVE_RESULTS$baseline$structural$psi_coef_mat[4,1],"  & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[4,2]," &  & ",SAVE_RESULTS$baseline$structural$r_sq_mat[4]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[4,1]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[4,2]," &  & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[4]," ","\\\\ \n",
" & (",SAVE_RESULTS$baseline$structural$se[4,1],") & (",SAVE_RESULTS$baseline$structural$se[4,2],") &  &  & (",SAVE_RESULTS$baseline$reduced_form$se[4,1],") & (",SAVE_RESULTS$baseline$reduced_form$se[4,2],") &  & ","\\\\[1.8ex]  \n",

" &  & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[5,2]," & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[5,3]," & ",SAVE_RESULTS$baseline$structural$r_sq_mat[5]," & & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[5,2]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[5,3]," & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[5]," ","\\\\ \n",
" &  & (",SAVE_RESULTS$baseline$structural$se[5,2],") & (",SAVE_RESULTS$baseline$structural$se[5,3],") &  &  & (",SAVE_RESULTS$baseline$reduced_form$se[5,2],") & (",SAVE_RESULTS$baseline$reduced_form$se[5,3],") & ","\\\\[1.8ex] \n",

" & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[6,1]," &  & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[6,3]," & ",SAVE_RESULTS$baseline$structural$r_sq_mat[6]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[6,1]," &  & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[6,3]," & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[6]," ","\\\\ \n",
" & (",SAVE_RESULTS$baseline$structural$se[6,1],") &  & (",SAVE_RESULTS$baseline$structural$se[6,3],") &  & (",SAVE_RESULTS$baseline$reduced_form$se[6,1],") &  & (",SAVE_RESULTS$baseline$reduced_form$se[6,3],") & ","\\\\[1.8ex] \n",

"\\hline ","\\\\[-1.8ex] \n",

"All Tools   & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[7,1],"  & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[7,2]," & ",SAVE_RESULTS$baseline$structural$psi_coef_mat[7,3]," & ",SAVE_RESULTS$baseline$structural$r_sq_mat[7]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[7,1]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[7,2]," & ",SAVE_RESULTS$baseline$reduced_form$psi_coef_mat[7,3]," & ",SAVE_RESULTS$baseline$reduced_form$r_sq_mat[7]," ","\\\\ \n",
"&  (",SAVE_RESULTS$baseline$structural$se[7,1],") & (",SAVE_RESULTS$baseline$structural$se[7,2],") & (",SAVE_RESULTS$baseline$structural$se[7,3],") & & (",SAVE_RESULTS$baseline$reduced_form$se[7,1],") & (",SAVE_RESULTS$baseline$reduced_form$se[7,2],") & (",SAVE_RESULTS$baseline$reduced_form$se[7,3],") & ","\\\\[1.8ex] \n",

"\\hline \n",
"\\hline ","\\\\[-1.8ex] \n",

"\\end{tabular} \n",
"\\end{adjustbox} \n",
"} \n",
"\\end{table} \n"
))

  

  
 
  
                    
  
  
  
  

