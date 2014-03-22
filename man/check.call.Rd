\name{check.call}
\alias{check.call}
\title{Checks whether the user makes a particular function call in his code or call a particular R statement}
\usage{
  check.call(call.str, check.args = TRUE,
    check.arg.val = NULL, check.arg.name = NULL,
    success.message = NULL, failure.message = NULL,
    no.command.failure.message = "You have not yet included correctly, all required R commands in your code...",
    ex = get.ex(), stud.env = ex$stud.env, verbose = FALSE,
    sol.env = ex$sol.env, hint.name = NULL, ...)
}
\description{
  Checks whether the user makes a particular function call
  in his code or call a particular R statement
}

