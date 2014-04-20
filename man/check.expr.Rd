\name{check.expr}
\alias{check.expr}
\title{Test: Compare students variables with either the values from the given solutions or with the result of an expression that is evaluated in the students solution}
\usage{
  check.expr(check.expr, correct.expr,
    failure.message = "{{check_expr}} has the wrong values!",
    success.message = "Great, {{check_expr}} seems correct.",
    hint.name = NULL, part = NULL, ex = get.ex(),
    stud.env = ex$stud.env, verbose = FALSE,
    unsubst.check.expr = NULL, unsubst.correct.expr = NULL,
    str.check.expr = NULL, str.correct.expr = NULL,
    sol.env = ex$sol.env, tol = .Machine$double.eps^0.5)
}
\arguments{
  \item{vars}{a variable name or vector of variable names}

  \item{exists}{shall existence be checked (similar length,
  class, values)}

  \item{failure.exists}{a message that is shown if the
  variable does not exists (similar the other failure.???
  variables)}

  \item{failure.message.add}{a text that will be added to
  all failure messages}

  \item{expr}{}
}
\description{
  Test: Compare students variables with either the values
  from the given solutions or with the result of an
  expression that is evaluated in the students solution
}

