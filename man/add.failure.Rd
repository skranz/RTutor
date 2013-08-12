\name{add.failure}
\alias{add.failure}
\title{Used inside tests: adds a failure to an exercise}
\usage{
  add.failure(ex, short, message, ...)
}
\arguments{
  \item{ex}{the exercise (environment) that is currently
  tested (typically ex=get.ex())}

  \item{short}{a short id of the error that will be stored
  in the log file}

  \item{message}{a longer description shown to the user}

  \item{...}{variables that will be rendered into messages
  that have whiskers}
}
\description{
  Used inside tests: adds a failure to an exercise
}

