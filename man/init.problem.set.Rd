\name{init.problem.set}
\alias{init.problem.set}
\title{Initialize a problem set for the student}
\usage{
  init.problem.set(name, stud.path,
    stud.short.file = paste0(prefix, name, ".r"),
    log.file = paste0(prefix, name, ".log"),
    state.file = paste0(prefix, name, ".Rdata"),
    prefix = "", require.stud.file = TRUE)
}
\arguments{
  \item{name}{the name of the problem set}

  \item{stud.path}{the path in which the stud has stored
  his file}

  \item{stud.file}{the file name in which the stud has
  stored her files}
}
\description{
  Initialize a problem set for the student
}

