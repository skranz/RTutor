\name{check.var}
\alias{check.var}
\title{Test: Compare students variables with either the values from the given solutions or with the result of an expression that is evaluated in the students solution}
\usage{
  check.var(vars, expr = NULL, exists = FALSE,
    length = FALSE, class = FALSE, values = FALSE,
    failure.exists = "You have not generated the variable {{var}}.",
    failure.length = "Your variable {{var}} has length {{length_stud}} but it shall have length {{length_sol}}.",
    failure.class = "Your variable {{var}} has a wrong class. It should be {{class_sol}} but it is {{class_stud}}.",
    failure.values = "Your variable {{var}} has wrong values.",
    failure.message.add = NULL,
    success.message = "Great, {{vars}} has correct {{tests}}.",
    hint.name = NULL, ex = get.ex(),
    stud.env = ex$stud.env, verbose = FALSE,
    sol.env = ex$sol.env)
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

