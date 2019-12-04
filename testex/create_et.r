setwd("D:/libraries/RTutor/RTutor")
library(testex)
sources = testex_sources(ex.files = "testex/tests.r")
et = testex_create(sources)

saveRDS(et, "testex/et.Rds")
