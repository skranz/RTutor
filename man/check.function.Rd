\name{check.function}
\alias{check.function}
\title{Checks a function written by the student}
\usage{
  check.function(code.or.name, ..., check.args = TRUE,
    check.defaults = FALSE, check.args.order = TRUE,
    allow.extra.args = TRUE, ex = get.ex(),
    stud.env = ex$stud.env, verbose = FALSE,
    sol.env = ex$sol.env, hint.name = NULL)
}
\arguments{
  \item{code.or.name}{a string containing the name of the
  function as specified in the solution}

  \item{...}{you can add several test calls to the
  function. It will be checked whether the users' function
  returns the same values in those calls than the function
  in the solution. You can also have a code block wrapped
  in {} that ends with a call to the function. In this way
  you can e.g. specify a random seeds before calling the
  function.}

  \item{check.args}{if TRUE check the arguments of the user
  function. If a character vector only check the given
  arguments.}

  \item{check.defaults}{TRUE = check the default values of
  the arguments of the user function. If a character vector
  only check the default values of the given arguments.}

  \item{check.args.order}{if TRUE make sure that the
  checked arguments appear in the same order in the user
  function than in the solution}

  \item{allow.extra.args}{if TRUE the user function can
  have additional arguments (at the end) that are not in
  the solution}

  \item{hint.name}{name of a hint that is associated with
  this test}
}
\description{
  Checks a function written by the student
}

