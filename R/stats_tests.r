# This file contains deprecated tests
# for different statistical questions
# 
# They come from the early RTutor days
# where I thought one would provide
# a lot of fine grained custom test 
# functions that the problem set designer
# uses.
# 

# A helper function for hypothesis test about whether student solution is correct
# @export
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

# Test for variance (copied from TeachingDemos)
# @export
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
  out$data.name <- deparse1(substitute(x))
  names(out$estimate) <- paste("var of", out$data.name)
  return(out)
}


# Test whether a certain H0 can be significantly rejected
#
# @param test.expr an expression that calls a test which will be evaluated in stud.env. The test must return a list that contains a field "p.value"
# @param p.value Instead of providing test.expr, one can directly provide a p.value from a previously run test
# @param test.name an optional test.name that can be used to fill the {{test_name}} whiskers in warning or failure messages.
# @param alpha.failure default=0.001 the critical p.value below which the stud code is considered wrong
# @param alpha.warning default=0.05 a p.value below a warning is printed that the code may be wrong
# @param short.message,failure.messages, warning.messages Messages in case of a failure and warning and  short message for the log.file
# @param check.warning if FALSE don't check for a warning
# @return TRUE if H0 can be rejected, FALSE if not and "warning" if it can be weakly rejected
# @export
test.H0.rejected = function(test.expr,p.value,test.name="",
  alpha.warning = 0.01,alpha.failure =0.05,
  short.message="Fail to reject '{{test_name}}', p.value = {{p_value}}",
  warning.message="The null hypothesis from the test '{{test_name}}', should not be rejcected, but I get a fairly low p.value of {{p_value}}.",
  failure.message="I couldn't significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}",
  success.message = "Great, I could significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}!",
  check.warning=TRUE, ps=get.ps(),stud.env = ps$stud.env, part=NULL,...)
{


  if (!missing(test.expr)) {
    test.expr = substitute(test.expr)
    if (test.name=="") {
      test.name = deparse1(test.expr)
    }
  }
  if (missing(p.value)) {
    test.res = eval(test.expr, stud.env)
    p.value = test.res$p.value
  }
  if (p.value > alpha.failure) {
    add.failure(failure.message,test_name=test.name,p_value=p.value)
    return(FALSE)
  }

  add.success(success.message,test_name=test.name,p_value=p.value)

  if (p.value > alpha.warning & check.warning) {
    add.warning(warning.message,test_name=test.name,p_value=p.value)
    return("warning")
  }

  return(TRUE)
}

# Check whether a certain null hypothesis is not significantly rejected
# @param test.expr an expression that calls a test which will be evaluated in stud.env. The test must return a list that contains a field "p.value"
# @param p.value Instead of providing test.expr, one can directly provide a p.value from a previously run test
# @param test.name an optional test.name that can be used to fill the {{test_name}} whiskers in warning or failure messages.
# @param alpha.failure default=0.001 the critical p.value below which the stud code is considered wrong
# @param alpha.warning default=0.05 a p.value below a warning is printed that the code may be wrong
# @param short.message,failure.messages, warning.messages Messages in case of a failure and warning and  short message for the log.file
# @param check.warning if FALSE don't check for a warning
# @return TRUE if H0 cannot be rejected, FALSE if not and "warning" if it can be weakly rejected
# @export
test.H0 = function(test.expr,p.value,test.name="",
                   alpha.warning = 0.05,alpha.failure =0.001,
                  short.message,warning.message,failure.message,
                   success.message = "Great, I could not significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}!",

                  check.warning=TRUE, part=NULL,
                  ps=get.ps(),stud.env = ps$stud.env,...) {


  #browser()
  if (!missing(test.expr)) {
    test.expr = substitute(test.expr)
    if (test.name=="") {
      test.name = deparse1(test.expr)
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
    add.failure(failure.message,test_name=test.name,p_value=p.value,...)
    return(FALSE)
  }

  add.success(success.message,test_name=test.name,p_value=p.value,...)

  if (p.value < alpha.warning & check.warning) {
    add.warning(warning.message,test_name=test.name,p_value=p.value,...)
    return("warning")
  }
  return(TRUE)
}

# Test: The variance of the distribution from which a vector of random numbers has been drawn
# @export
test.variance = function(vec, true.val, test = "t.test",short.message,warning.message,failure.message, success.message = "Great, I cannot statistically reject that {{var}} has the desired variance {{vari_sol}}!", ps=get.ps(),stud.env = ps$stud.env,part=NULL,...) {
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
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message, success.message=success.message,
  var = var.name, vari_stud = var(vec), vari_sol=true.val,...)


}


# Test: The mean of the distribution from which a vector of random numbers has been drawn
# @export
test.mean = function(vec, true.val, test = "t.test", short.message,warning.message,failure.message, success.message = "Great, I cannot statistically reject that {{var}} has the desired mean of {{mean_sol}}!", ps=get.ps(),stud.env = ps$stud.env,part=NULL,...) {
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
    warning.message = "{{var}} has a suspicious mean!\n Your random variable {{var}} has a sample mean of {{mean_stud}} but shall have {{mean_sol}}. A t-test tells me that if that null hypthesis were true, the probability would be just around {{p_value}} to get such an extreme sample mean_"
  }
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message,success.message=success.message,
        var = var.name, mean_stud = mean(vec), mean_sol=true.val,...)
}

# Test: Has a vector of random numbers been drawn from a normal distribution?
# @export
test.normality = function(vec,short.message,warning.message,failure.message,ps=get.ps(),stud.env = ps$stud.env, success.message = "Great, I cannot statistically reject that {{var}} is indeed normally distributed!",part=NULL,...) {
  call.str = as.character(match.call())
  restore.point("test.normality")

  var.name=call.str[2]

  # Cannot use more than 5000 observations
  if (length(vec)>5000)
    vec = vec[sample.int(n=length(vec), size=5000)]
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
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message,success.message=success.message,
        var = var.name,...)
}
