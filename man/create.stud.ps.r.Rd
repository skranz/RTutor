\name{create.stud.ps.r}
\alias{create.stud.ps.r}
\title{Internal function to generate a problem set skeleton for a student and save it in a file}
\usage{
  create.stud.ps.r(ps,
    file = paste0(ps$stud.path, "/", ps$name, ".r"),
    ps.dir = "C:/...",
    user.name = "ENTER A USER NAME HERE", header = NULL,
    footer = NULL)
}
\description{
  Internal function to generate a problem set skeleton for
  a student and save it in a file
}

