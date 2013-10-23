# Direct testing functions


#' Simply shows a success message when this test is reached for the first time!
#' @export 
show.success.message = function(success.message,..., ex=get.ex()) {
  add.success(ex,success.message,...)
  return(TRUE)
}



#' Test: Compare students variables with either the values from the given solutions or with the result of an expression that is evaluated in the students solution 
#' @param vars a variable name or vector of variable names
#' @param exists shall existence be checked (similar length, class, values)
#' @param failure.exists a message that is shown if the variable does not exists (similar the other failure.??? variables)
#' @param failure.message.add a text that will be added to all failure messages
#' @param expr
#' @export 
check.var = function(vars, expr=NULL,exists=FALSE, length=FALSE, class=FALSE, values=FALSE,
  failure.exists="You have not generated the variable {{var}}.",
  failure.length="Your variable {{var}} has length {{length_stud}} but it shall have length {{length_sol}}.",
  failure.class = "Your variable {{var}} has a wrong class. It should be {{class_sol}} but it is {{class_stud}}.",
  failure.values = "Your variable {{var}} has wrong values.",
  failure.message.add = NULL,
  success.message = "Great, {{vars}} has correct {{tests}}.", hint.name = NULL,
  ex=get.ex(),stud.env = ex$stud.env, verbose=FALSE,   sol.env = ex$sol.env) {
    
  set.current.hint(hint.name)
  
  if (!is.null(failure.message.add)) {
    failure.exists = paste0(failure.exists,"\n", failure.message.add)
    failure.length = paste0(failure.length,"\n", failure.message.add)
    failure.class = paste0(failure.class,"\n", failure.message.add)
    failure.values = paste0(failure.values,"\n", failure.message.add)
  }
  
  expr = substitute(expr)
  if (!is.null(expr)) {
    if (length(vars)>1)
      stop("Error in check.var: if you provide expr, you can only check a single variable!")
    vars.sol = list(eval(expr,stud.env))
  } else {
    vars.sol = lapply(vars, function(var) get(var,sol.env)) 
  }
  names(vars.sol) = vars
  
  if (exists != FALSE) {
    short.message = paste0("{{var}} does not exist")
    for (var in vars) {
      if (!exists(var,stud.env, inherits=FALSE)) {
        add.failure(ex,short.message,failure.exists, var = var)
        return(FALSE)
      }
    }
  }
  
  
  if (length != FALSE) {
    short.message = paste0("wrong length {{var}}: is {{length_stud}} must {{length_sol}}")
    for (var in vars) {
      var.stud = get(var,stud.env)
      var.sol = vars.sol[[var]]
      if (!length(var.stud)==length(var.sol)) {
        add.failure(ex,short.message, failure.length, var=var, length_stud = length(var.stud), length_sol=length(var.sol))
        return(FALSE)
      }
    }
  }  
  if (class != FALSE) {
    for (var in vars) {
      var.stud = get(var,stud.env)
      var.sol = vars.sol[[var]]
      short.message = "wrong class {{var}}: is {{class_stud}} must {{class_sol}}"
      class.stud = class(var.stud)[1]
      class.sol = class(var.sol)[1]
      if (class.stud == "integer") class.stud = "numeric"
      if (class.sol == "integer") class.sol = "numeric"
      
      if (class.stud!=class.sol) {
        add.failure(ex,short.message, failure.class, var=var, class_stud=class.stud, class_sol = class.sol)
        return(FALSE)
      }
    }
  }  
  if (values != FALSE) {
    for (var in vars) {
      var.stud = get(var,stud.env)
      var.sol = vars.sol[[var]]

      
      if (! all(var.stud==var.sol)) {
        add.failure(ex,"wrong values of {{var}}", failure.values, var=var)
        return(FALSE)
      }
    }
  }
  
  tests.str = flags.to.string(length=length,class=class,values=values)
  add.success(ex,success.message, vars=paste0(vars,collapse=","), tests=tests.str)
  return(TRUE)
}

flags.to.string = function(..., sep=", ", last.sep = " and ") {
  args = list(...)
  
  args = args[unlist(args)]
  if (length(args)==1)
    return(names(args))
  if (length(args)==2)
    return(paste0(names(args),collapse=last.sep))
  
  return(paste0(paste0(names(args[-length(args)]),collapse=last.sep),
         last.sep,names(args[length(args)])))                  
  
}


#' A helper function for hypothesis test about whether student solution is correct
#' @export 
hypothesis.test.result = function(p.value, alpha.warning=0.05, alpha.failure = 0.0001, verbose=FALSE) {
  if (p.value < alpha.failure) {
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
  return(out)
}


#' Test whether a certain H0 can be significantly rejected
#' 
#' @param test.expr an expression that calls a test which will be evaluated in stud.env. The test must return a list that contains a field "p.value"
#' @param p.value Instead of providing test.expr, one can directly provide a p.value from a previously run test
#' @param test.name an optional test.name that can be used to fill the {{test_name}} whiskers in warning or failure messages.
#' @param alpha.failure default=0.001 the critical p.value below which the stud code is considered wrong
#' @param alpha.warning default=0.05 a p.value below a warning is printed that the code may be wrong
#' @param short.message,failure.messages, warning.messages Messages in case of a failure and warning and  short message for the log.file
#' @param check.warning if FALSE don't check for a warning 
#' @return TRUE if H0 can be rejected, FALSE if not and "warning" if it can be weakly rejected
#' @export 
test.H0.rejected = function(test.expr,p.value,test.name="",
  alpha.warning = 0.01,alpha.failure =0.05,
  short.message="Fail to reject '{{test_name}}', p.value = {{p_value}}",
  warning.message="The null hypothesis from the test '{{test_name}}', should not be rejcected, but I get a fairly low p.value of {{p_value}}.",
  failure.message="I couldn't significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}",
  success.message = "Great, I could significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}!",
  check.warning=TRUE, ex=get.ex(),stud.env = ex$stud.env, hint.name=NULL,...)
{
  set.current.hint(hint.name)
  
  if (!missing(test.expr)) {
    test.expr = substitute(test.expr)
    if (test.name=="") {
      test.name = deparse(test.expr)
    }
  }
  if (missing(p.value)) {
    test.res = eval(test.expr, stud.env)
    p.value = test.res$p.value
  }
  if (p.value > alpha.failure) {
    add.failure(ex,short.message,failure.message,test_name=test.name,p_value=p.value)
    return(FALSE)
  }
  
  add.success(ex,success.message,test_name=test.name,p_value=p.value,...)
  
  if (p.value > alpha.warning & check.warning) {
    add.warning(ex,short.message,warning.message,test_name=test.name,p_value=p.value)
    return("warning")
  }
  
  return(TRUE)
}

#' Check whether a certain null hypothesis is not significantly rejected
#' @param test.expr an expression that calls a test which will be evaluated in stud.env. The test must return a list that contains a field "p.value"
#' @param p.value Instead of providing test.expr, one can directly provide a p.value from a previously run test
#' @param test.name an optional test.name that can be used to fill the {{test_name}} whiskers in warning or failure messages.
#' @param alpha.failure default=0.001 the critical p.value below which the stud code is considered wrong
#' @param alpha.warning default=0.05 a p.value below a warning is printed that the code may be wrong
#' @param short.message,failure.messages, warning.messages Messages in case of a failure and warning and  short message for the log.file
#' @param check.warning if FALSE don't check for a warning 
#' @return TRUE if H0 cannot be rejected, FALSE if not and "warning" if it can be weakly rejected
#' @export 
test.H0 = function(test.expr,p.value,test.name="",
                   alpha.warning = 0.05,alpha.failure =0.001,
                  short.message,warning.message,failure.message,
                   success.message = "Great, I could not significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}!",
                   
                  check.warning=TRUE, hint.name=NULL,
                  ex=get.ex(),stud.env = ex$stud.env,...) {
  set.current.hint(hint.name)
  
  if (!missing(test.expr)) {
    test.expr = substitute(test.expr)
    if (test.name=="") {
      test.name = deparse(test.expr)
    }
  }
  if (missing(p.value)) {
    test.res = eval(test.expr, stud.env)
    p.value = test.res$p.value
  }
  if (missing(short.message)) {
    short.message = paste0("rejected '{{test_name}}' has p.value = {{p_value}}")
  }
  if (missing(failure.message)) {
    failure.message = paste0("The null hypothesis from the test '{{test_name}}' shall hold, but it is rejected at p.value = {{p_value}}")
  }
  if (missing(warning.message)) {
    warning.message = paste0("The null hypothesis from the test '{{test_name}}', should not be rejcected, but I get a fairly low p.value of {{p_value}}.")
  }  
  if (p.value < alpha.failure) {
    add.failure(ex,short.message,failure.message,test.name=test.name,p_value=p.value,...)
    return(FALSE)
  }

  add.success(ex,success.message,test_name=test.name,p_value=p.value,...)
  
  if (p.value < alpha.warning & check.warning) {
    add.warning(ex,short.message,warning.message,test.name=test.name,p_value=p.value,...)
    return("warning")
  }
  return(TRUE)
}

#' Test: The variance of the distribution from which a vector of random numbers has been drawn
#' @export
test.variance = function(vec, true.val, test = "t.test",short.message,warning.message,failure.message, success.message = "Great, I cannot statistically reject that {{var}} has the desired variance {{vari_sol}}!", ex=get.ex(),stud.env = ex$stud.env,...) {
  call.str = as.character(match.call())
  var.name = call.str[2]
  p.value = sigma.test(vec,sigmasq=true.val)$p.value

  if (missing(short.message)) {
    short.message ="wrong variance {{var}}: is {{vari_stud}} shall {{vari_sol}}, p.value = {{p_value}}"
  }
  if (missing(failure.message)) {
    failure.message = "{{var}} has wrong variance! \n Your random variable {{var}} has a sample variance of {{vari_stud}} but shall have {{vari_sol}}. A chi-square test tells me that if that null hypothesis were true, it would be very unlikely (p.value={{p_value}}) to get a sample as extreme as yours!"
  }
  if (missing(warning.message)) {
    warning.message = "{{var}} has a suspicious variance! \n Your random variable {{var}} has a sample variance of {{vari_stud}} but shall have {{vari_sol}}. A chi-square test tells me that if that null hypthesis were true, the probability would be just around {{p_value}} to get such an extreme sample variance"
  }
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message,ex=ex, success.message=success.message,
  var = var.name, vari_stud = var(vec), vari_sol=true.val,...)
} 

#' Test: The mean of the distribution from which a vector of random numbers has been drawn
#' @export
test.mean = function(vec, true.val, test = "t.test", short.message,warning.message,failure.message, success.message = "Great, I cannot statistically reject that {{var}} has the desired mean of {{mean_sol}}!", ex=get.ex(),stud.env = ex$stud.env,...) {
  call.str = as.character(match.call())
  stopifnot(test=="t.test")
  var.name = call.str[2]
  p.value = t.test(vec,mu=true.val)$p.value

  if (missing(short.message)) {
    short.message ="wrong mean {{var}}: is {{mean_stud}} shall {{mean_sol}}, p.value = {{p_value}}"
  }
  if (missing(failure.message)) {
    failure.message = "{{var}} has wrong mean! \n Your random variable {{var}} has a sample mean of {{mean_stud}} but shall have {{mean_sol}}. A t-test tells me that if that null hypothesis were true, it would be very unlikely (p.value={{p_value}}) to get a sample mean as extreme as yours!"
  }
  if (missing(warning.message)) {
    warning.message = "{{var}} has a suspicious variance! \n Your random variable {{var}} has a sample mean of {{mean_stud}} but shall have {{mean_sol}}. A t-test tells me that if that null hypthesis were true, the probability would be just around {{p_value}} to get such an extreme sample mean_"
  }
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message,success.message=success.message,ex=ex,
        var = var.name, mean_stud = mean(vec), mean_sol=true.val,...)
}

#' Test: Has a vector of random numbers been drawn from a normal distribution?
#' @export
test.normality = function(vec,short.message,warning.message,failure.message,ex=get.ex(),stud.env = ex$stud.env, success.message = "Great, I cannot statistically reject that {{var}} is indeed normally distributed!",...) {
  call.str = as.character(match.call())
  restore.point("test.normality")
  var.name=call.str[2]
  p.value = shapiro.test(vec)$p.value
  
  if (missing(short.message)) {
    short.message ="{{var}} not normally distributed, p.value = {{p_value}}"
  }
  if (missing(failure.message)) {
    failure.message = "{{var}} looks really not normally distributed.\n A Shapiro-Wilk test rejects normality at an extreme significance level of {{p_value}}."
  }
  if (missing(warning.message)) {
    warning.message = "{{var}} looks not very normally distributed.\n A Shapiro-Wilk test rejects normality at a significance level of {{p_value}}."
  }
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message,success.message=success.message,ex=ex,
        var = var.name,...)
}

#' Test: Does a certain condition on the stud's generated variables hold true
#' @export
holds.true = function(cond, short.message = failure.message,failure.message="Failure in holds.true",success.message="Great, the condition {{cond}} holds true in your solution!",hint.name=NULL,ex=get.ex(),stud.env = ex$stud.env,...) {
  restore.point("holds.true")
  set.current.hint(hint.name)
  
  cond = substitute(cond)
  cond.str = deparse(cond)
  if (!all(eval(cond,stud.env))) {
    add.failure(ex,short.message,failure.message,cond=cond.str,...)
    return(FALSE)
  }
  add.success(ex,success.message,cond=cond.str,...)
  #cat(paste0("\n",message, "... ok!"))
  return(TRUE)
}
