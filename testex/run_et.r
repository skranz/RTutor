cat("\nRun example tests...")
library(testex)

et = readRDS("testex/et.Rds")

library(RTutor)
exemptions=testex_exemptions(fun=c("setwd"))

res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exemptions=exemptions, print.code=TRUE, print.output=TRUE)

issue.df = res$issue.df

if (res$num.issues>0) {
  stop("Example tests failed!")
}
