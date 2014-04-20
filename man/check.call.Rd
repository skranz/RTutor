\name{check.call}
\alias{check.call}
\title{Checks whether the user makes a particular function call in his code or call a particular R statement}
\usage{
  check.call(call, check.arg.by.value = TRUE,
    allow.extra.arg = FALSE, ignore.arg = NULL,
    success.message = NULL, failure.message = NULL,
    no.command.failure.message = NULL, ex = get.ex(),
    stud.env = ex$stud.env, verbose = FALSE,
    sol.env = ex$sol.env, hint.name = NULL, part = NULL,
    ...)
}
\description{
  Checks whether the user makes a particular function call
  in his code or call a particular R statement
}

