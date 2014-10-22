examples.translate.old.sol = function() {
  setwd("D:/libraries/RTutor/examples")
  in.file = "The Impact of Shrouded Fees 3_sol.Rmd"
  out.file = "The Impact of Shrouded Fees - 3_sol.Rmd"
  
  in.file = "Problem_Set_Sol.Rmd"
  out.file = "Bank Runs 2_sol.Rmd"
  
  translate.old.rtutor.sol(in.file=in.file, out.file=out.file)
}

translate.old.rtutor.sol = function(txt = readLines(in.file), in.file=NULL, out.file=rmd.file) {
  txt = gsub("#> task","#>",txt)
  txt = gsub("#> notest","#>",txt)
  txt = gsub("#> settings","#>",txt)
  txt = gsub("#< add to hint","#< add_to_hint",txt)
  txt = gsub("#< test.arg","#< test_arg",txt)
  txt = gsub("#< test.hint.arg","#< test_hint_arg",txt)
  txt = gsub("#< task notest","#< task_notest",txt) 
  
  rows = str.trim(txt=="#<")
  txt[rows] = "#< test"
  
  if (!is.null(out.file)) {
    writeLines(txt, out.file)
  }
  invisible(txt)
}