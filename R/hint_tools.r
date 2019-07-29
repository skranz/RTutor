
# Works so far only for simple formulas using lm
compare.regression.formula = function(form.stud, form.sol, from="lm") {
  restore.point("compare.regression.formula")
  
  descr = NULL
  
  if (!is.character(form.stud))
    form.stud = deparse1(form.stud)
  if (!is.character(form.sol))
    form.sol = deparse1(form.sol)
  
  if (identical(form.stud, form.sol)) {
    return(list(same=TRUE))
  }
  
  if (has.substr(form.sol,"~") & ! has.substr(form.stud, "~")) {
    descr = paste0(descr, "You formula must contain a '~' (tilde). You write the dependent variable left of ~ and the explanatory variables right of the ~ sign. You combine the different explanatory variabes with the + sign.")
    return(list(same=FALSE, descr=descr))
  }
  
  dep.stud = trimws(str.left.of(form.stud, "~"))
  dep.sol = trimws(str.left.of(form.sol, "~"))
  
  if (dep.stud != dep.sol) {
    if (dep.sol == "") {
      descr = c(descr,paste0("The sample solution specifies no dependent variable left of ~"))
    } else if (dep.stud =="") {
      descr = c(descr,paste0("You have not specified a dependent variable left of ~"))
    } else {
      descr = c(descr, paste0("Your dependent variable ", dep.stud, " left of ~ is not right."))
    }
    return(list(same=FALSE, descr=descr))
  }
  
  rhs.stud = trimws(str.right.of(form.stud,"~"))
  rhs.sol = trimws(str.right.of(form.sol,"~"))
  
  # More complex multipart formula
  if (has.substr(rhs.sol, "|")) {
    scramble = scramble.text(form.sol,"?",0.5, keep.char=c(" ","~","|"))
    return(same=FALSE, descr = scramble)
  }
  
  if (has.substr(rhs.stud,"|")) {
    descr = "Your formula should not contain a |"
    return(list(same=FALSE, descr=descr))
  }
  terms.stud = trimws(strsplit(rhs.stud,"+",fixed = TRUE)[[1]])
  terms.sol = trimws(strsplit(rhs.sol,"+",fixed = TRUE)[[1]])
  
  excess = setdiff(terms.stud, terms.sol)
  if (length(excess)>0) {
    descr = paste0("Don't use in your formula the explanatory variable(s) ", paste0(excess, collapse = ", "),".")
    return(list(same=FALSE, descr=descr))
  }
    
  missing = setdiff(terms.sol, terms.stud)
  if (length(missing)==1) {
    descr = paste0("You are missing the explanatory variable ", missing,".")
    if (length(terms.stud)==1) {
      descr = paste0(descr," To combine different explanatory variables use +.")
    }
    return(list(same=FALSE, descr=descr))
  }
  if (length(missing)>1) {
    descr = paste0("You are missing ", length(missing), " explanatory variables on the right hand side. One of the missing variables is ", sample(missing, 1),".")  
    if (length(terms.stud)==1) {
      descr = paste0(descr," Combine different explanatory variables with +.")
    }
    return(list(same=FALSE, descr=descr))
  }
  if (setequal(terms.stud, terms.sol)) {
    descr = paste0("You shall arrange your rhs variables in a different order: ", paste0(terms.sol, collapse=" + "))
    return(list(same=FALSE, descr=descr))
  }
  
  return(list(same=FALSE, descr=""))
}

descibe.vector.diff = function(v.stud, v.sol) {
  list(
    same = identical(v.stud == v.sol),
    missing = setdiff(v.sol, v.stud),
    excess = setdiff(v.stud, v.sol),
    only.order.wrong = setequal(v.stud, v.sol) 
  )
}