\name{check.assign}
\alias{check.assign}
\title{Checks an assignment to a variable}
\usage{
  check.assign(call, success.message = NULL,
    failure.message = NULL,
    no.command.failure.message = "You have not yet included correctly, all required R commands in your code...",
    ok.if.same.val = TRUE, ex = get.ex(),
    stud.env = ex$stud.env, verbose = FALSE,
    sol.env = ex$sol.env, hint.name = NULL, part = NULL,
    ...)
}
\description{
  Checks an assignment to a variable
}

