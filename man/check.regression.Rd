\name{check.regression}
\alias{check.regression}
\title{Check whether an object from a call to lm, glm or some other regression function is correct}
\usage{
\method{check}{regression}(var, str.expr, hint.name = NULL, part = NULL,
  ex = get.ex(), stud.env = ex$stud.env, verbose = FALSE,
  sol.env = ex$sol.env, failure.message = paste0("Hmm... your regression ",
  var, " seems incorrect."),
  success.message = paste0("Great, your regression ", var, " looks correct."),
  tol = 1e-10)
}
\description{
Check whether an object from a call to lm, glm or some
other regression function is correct
}

