\name{add.warning}
\alias{add.warning}
\title{Used inside tests: adds a warning to an exercise}
\usage{
add.warning(ex, short, message, ...)
}
\arguments{
  \item{ex}{the exercise (environment) that is currently
  tested (typically ex=get.ex())}

  \item{short}{a short id of the warning that will be
  stored in the log file}

  \item{message}{a longer description shown to the user}

  \item{...}{variables that will be rendered into messages
  that have whiskers}
}
\description{
Used inside tests: adds a warning to an exercise
}

