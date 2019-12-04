cat("\nRun example tests...")
library(testex)

et = readRDS("testex/et.Rds")

library(RTutor)
exemptions=testex_exemptions()

res = testex_run(et,log.file = "testex/log.Rmd",stat.file = "testex/stats.csv", exemptions=exemptions)

if (res$num.issues>0) {
  stop("Example tests failed!")
}
