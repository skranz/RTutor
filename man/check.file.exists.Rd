\name{check.file.exists}
\alias{check.file.exists}
\title{Check whether a given file exists}
\usage{
  check.file.exists(file,
    failure.message = paste0("Sorry, but I cannot find the file \"", file, "\" in your current working directory."),
    success.message = paste0("Great, I have found the file \"", file, "\"!"),
    ex = get.ex(), hint.name = NULL, ...)
}
\description{
  Check whether a given file exists
}

