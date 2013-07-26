

#' Test: Compare students variables with those from the given solutions
#' @export 
check.var = function(vars,exists=FALSE, length=FALSE, class=FALSE, values=FALSE, verbose=FALSE, student.env = get.student.env(),  sol.env = get.sol.env()) {
  
  if (verbose) {
    str = paste(
      ifelse(exists!=FALSE,"existence,",""),
      ifelse(length!=FALSE,"length,",""),
      ifelse(class!=FALSE,"type,",""),
      ifelse(values!=FALSE,"values,",""),
      sep=""
      )
    
    cat(paste0("\nCheck for ", paste0(vars, collapse=",")," ", str,"..."))
  }
  
  
  if (exists != FALSE) {
    for (var in vars) {
      if (!exists(var,student.env, inherits=FALSE)) {
        message(paste0("Uups, you have not even generated the variable ",var, "."))
        return(FALSE)
      }
    }
    if (verbose)
      cat(" ok!")
  }
  if (length != FALSE) {
    for (var in vars) {
      var.user = get(var,student.env)
      var.sol = get(var,sol.env)
      
      if (!length(var.user)==length(var.sol)) {
        message(paste0("Your variable ", var, " has the wrong length. Length should be ", length(var.sol), " but it is ",length(var.user)))
        return(FALSE)
      }
    }
    if (verbose)
      cat(" ok!")
  }  
  if (class != FALSE) {
    for (var in vars) {
      var.user = get(var,student.env)
      var.sol = get(var,sol.env)
      
      if (!class(var.user)==class(var.sol)) {
        message(paste0("Tricky, your variable ", var, " has a wrong type. It should be ", class(var.sol), " but it is ",class(var.user)))
        return(FALSE)
      }
    }
    if (verbose)
      cat(" ok!")
  }  
  if (values != FALSE) {
    for (var in vars) {
      var.user = get(var,student.env)
      var.sol = get(var,sol.env)
      
      if (! all(var.user==var.sol)) {
        message(paste0("Sorry, but the values of ", var, " are not correct."))
        return(FALSE)
      }
    }
    if (verbose)
      cat(" ok!")
  }  
  return(TRUE)
}



#' A helper function for hypothesis test about whether student solution is correct
#' @export 
hypothesis.test.result = function(p.value, alpha.warning=0.05, alpha.fail = 0.0001, verbose=FALSE) {
  if (p.value < alpha.fail) {
    if (verbose)
      message("  H0 is highly significantly rejected... check fails!")
    return(FALSE)
  } else if (p.value < alpha.warning) {
    if (verbose)
      message(paste0("  H0 signicantly rejected... warning!"))
    return("warning")
  } else {
    if (verbose)
      cat(paste0("... ok!"))
    return(TRUE)    
  }
  
} 

#' Test for variance (copied from TeachingDemos)
#' @export
sigma.test = function (x, sigma = 1, sigmasq = sigma^2,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95,  ...) {
  alternative <- match.arg(alternative)
  sigma <- sqrt(sigmasq)
  n <- length(x)
  xs <- var(x)*(n-1)/sigma^2
  out <- list(statistic = c("X-squared" = xs))
  class(out) <- "htest"
  out$parameter <- c(df = n-1)
  minxs <- min(c(xs, 1/xs))
  maxxs <- max(c(xs, 1/xs))
  PVAL <- pchisq(xs, df = n - 1)
  
  out$p.value <- switch(alternative,
                        two.sided = 2*min(PVAL, 1 - PVAL),
                        less = PVAL,
                        greater = 1 - PVAL)
  out$conf.int <- switch(alternative,
                         two.sided = xs * sigma^2 *
                           1/c(qchisq(1-(1-conf.level)/2, df = n-1), qchisq((1-conf.level)/2, df
                                                                            = n-1)),
                         less = c(0, xs * sigma^2 /
                                    qchisq(1-conf.level, df = n-1)),
                         greater = c(xs * sigma^2 /
                                       qchisq(conf.level, df = n-1), Inf))
  attr(out$conf.int, "conf.level") <- conf.level
  out$estimate <- c("var of x" = var(x))
  out$null.value <- c(variance = sigma^2)
  out$alternative <- alternative
  out$method <- "One sample Chi-squared test for variance"
  out$data.name <- deparse(substitute(x))
  names(out$estimate) <- paste("var of", out$data.name)
  return(out)}


#' Check whether a certain H0 could be significantly rejected with a specified statistical test
#' @export 
check.H0.rejected = function(test.expr, alpha =0.05, failure.message, student.env=get.student.env()) {
  test.expr = substitute(test.expr)
  
  test.res = eval(test.expr, student.env)
  p.value = test.res$p.value
  
  if (missing(failure.message)) {
    expr.str = deparse(text.expr)
  
    failure.message = paste0("Failure: I couldn't significantly reject the null hypothesis from the test '", expr.str, "', p.value = ", round(p.value*100,4), "%")
  } else{
    
  }
  if (p.value > alpha) {
    message(failure.message)
    return(FALSE)
  }
  return(TRUE)
}


#' Test: The variance of the distribution from which a vector of random numbers has been drawn
#' @export
test.variance = function(vec, true.val,alpha.warning=0.05,alpha.fail=0.0001, test = "t.test",student.env=get.student.env(),..., digits=7,verbose=FALSE) {
  call.str = as.character(match.call())
  var.name = call.str[2]
  
  if (verbose)
    cat(paste0("\nTest variance of ",call.str[2],  " with chi-square test:"))
  res = sigma.test(vec,sigmasq=true.val)
  p.value = res$p.value
  if (verbose)
    cat(paste0("  H0: Var(",call.str[2],")=",true.val,
               ", Var.obs(",call.str[2],")=", round(var(vec),digits),
               ", p.value = ", round(p.value*100,4),"% "))
  
  res = hypothesis.test.result(p.value=p.value,alpha.warning=alpha.warning, alpha.fail=alpha.fail, verbose=verbose)
  if (res =="warning") {
    if (!verbose)
      message(paste0("Warning: ", var.name, " has suspicious variance! \n I am not sure if your random variable '", var.name, "'' really with sample variance ",round(var(vec),5)," comes from a distribution with variance ", true.val,". A chi-square test tells me that if that null hypthesis were true, the probability would be just around ",round(p.value*100,4),"% to get such extreme values in your sample."))
    
  } else if (res==FALSE) {
    message(paste0("Failure: ", var.name, " has wrong variance! \nI really don't believe that your random variable '", var.name, "' with sample variance ", round(var(vec),5)," comes from a distribution with variance ", true.val,". A chi-square test tells me that if that really were true, the probability would be just around ",round(p.value*100,15),"% to get such an extreme sample."))
    
  }
  return(res)
  
} 

#' Test: The mean of the distribution from which a vector of random numbers has been drawn
#' @export
test.mean = function(vec, true.val, alpha.warning=0.05,alpha.fail=0.0001, test = "t.test",student.env=get.student.env(),..., digits=7, verbose=FALSE) {
  call.str = as.character(match.call())
  restore.point("test.mean")
  stopifnot(test=="t.test")
  var.name = call.str[2]
  
  if (verbose)
    cat(paste0("\nTest mean of ",call.str[2],  " with t-test:"))
  res = t.test(vec,mu=true.val)
  p.value = res$p.value
  if (verbose)
    cat(paste0("  H0: E(",call.str[2],")=",true.val,
             ", E.obs(",call.str[2],")=", round(mean(vec),digits),
             ", p.value = ", round(p.value*100,4),"% "))
  
  res = hypothesis.test.result(p.value=p.value,alpha.warning=alpha.warning, alpha.fail=alpha.fail, verbose=verbose)
  
  if (res =="warning") {
    if (!verbose)
      message(paste0("Warning: I am not sure if your random variable '", var.name, "'' really comes from a distribution with mean ", true.val,". A t-test tells me that if that null hypothesis were really true, the probability would be just around ",round(p.value*100,4),"% to get such extreme values in your sample."))
    
  } else if (res==FALSE) {
    message(paste0("Failure: I really don't believe that your random variable '", var.name, "' with sample average ", round(mean(vec),5)," comes from a distribution with mean ", true.val,". A t-test tells me that if that really were true, the probability would be just around ",round(p.value*100,15),"% to get such an extreme sample."))
    
  }
  return(res)
  
  
  return(hypothesis.test.result(p.value=p.value,alpha.warning=alpha.warning, alpha.fail=alpha.fail))
}

#' Test: Has a vector of random numbers been drawn from a normal distribution?
#' @export
test.normality = function(vec, alpha.warning=0.05,alpha.fail=0.0001, student.env=get.student.env(),..., digits=7, verbose=FALSE) {
  call.str = as.character(match.call())
  restore.point("test.normality")
  var.name=call.str[2]
  if (verbose)
    cat(paste0("\nTest normality of ",call.str[2],  " with Shapiro-Wilk test"))
  res = shapiro.test(vec)
  p.value = res$p.value
  if (verbose)
    cat(paste0("  H0: ",call.str[2]," is normally distributed",
             ", p.value = ", round(p.value*100,4),"% "))
  
  res = hypothesis.test.result(p.value=p.value,alpha.warning=alpha.warning, alpha.fail=alpha.fail, verbose=verbose)
  if (res =="warning") {
    if (!verbose)
      message(paste0("Warning: ", var.name, " looks not very normally distributed.\n I am not sure if '", var.name, "'' is really drawn from a normal distribution. A Shapiro-Wilk test tells me that if that null hypthesis were true, the probability would be just around ",round(p.value*100,4),"% to get such a non-normally looking sample."))
    
  } else if (res==FALSE) {
    message(paste0("Failure: ", var.name, " is by far to unnormal! \n A Shapiro-Wilk test tells me that if ", var.name," were really drawn from a normal distribution, the probability would be just around ",round(p.value*100,15),"% to get such an non-normal sample."))
  }
  return(res)
}

#' Test: Does a certain condition on the user's generated variables hold true
#' @export
holds.true = function(cond, failure.message="Failure in holds.true", student.env=get.student.env()) {
  restore.point("holds.true")
  cond = substitute(cond)
  if (!all(eval(cond,student.env))) {
    message(failure.message)
    return(FALSE)
  }
  #cat(paste0("\n",message, "... ok!"))
  return(TRUE)
}
