\name{check.exercise}
\alias{check.exercise}
\title{Check the student's solution of an exercise contained in student.file}
\usage{
  check.exercise(ex.name, ps = get.ps(),
    student.file = get.student.file())
}
\arguments{
  \item{ex.name}{The name of the exercise}

  \item{ps}{The problem set object. By default the problem
  set loaded with load.problem.set}

  \item{student.file}{The r file in which the students
  solution to the problem set is stored}
}
\description{
  Check the student's solution of an exercise contained in
  student.file
}

