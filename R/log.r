# Structure of a log entry

# A csv file

# date (date with time)
# user.name
# ps.name
# chunk.ind
# chunk.name
# type: check, hint, run, start, load, import
# success: TRUE, FALSE, NA (has a check passed)
# e.ind:
# test.ind:
# seed: a random seed
# chunk.code: replace " with §
# run.code: replace " with §



log.hint = function(hint, ex=get.ex(), log.file = ps$log.file, ps = get.ps(), do.log = ex$code.changed, part=ex$part ) {
  restore.point("log.int")
  if (!do.log)
    return
    
  user.name = get.user()$name

  hint.out = paste0(capture.output(eval(hint$code,ex$user.env)), collapse="\n")
  
  entry = list(ps.name = ps$name, ex.name=ex$name, test.ind=ex$test.ind, part=part, date=as.character(ex$check.date), user.name = user.name, hint.name = hint$name, hint.out = hint.out, stud.seed = ex$stud.seed,code.changed=as.logical(ex$code.changed),failure.short = ex$failure.short,checks=ex$checks, attempts=ex$attempts, solved=ex$solved, was.solved=ex$was.solved, stud.code=paste0(ex$stud.code, collapse="\n"),
warnings=ex$warning.messages)
  
  #library(RJSONIO)
  #json.entry = toJSON(entry)
  con = file(log.file,open="at")
  
  #writeLines(json.entry, con)
  close(con) 
}

log.exercise = function(ex=get.ex(), log.file = ps$log.file, ps = get.ps(), do.log = isTRUE(ex$code.changed), part=ex$part) {
  restore.point("log.exercise")
  return()
  user.name = get.user()$name
  if (!do.log)
    return
  entry = list(ps.name = ps$name, ex.name=ex$name, test.ind=ex$test.ind, part=part, date=as.character(ex$check.date), user.name = user.name, stud.seed = ex$stud.seed,code.changed=as.logical(ex$code.changed),failure.short = ex$failure.short,checks=ex$checks, attempts=ex$attempts, solved=ex$solved, was.solved=ex$was.solved, stud.code=paste0(ex$stud.code, collapse="\n"),
warnings=ex$warning.messages)
  
  #library(RJSONIO)
  #json.entry = toJSON(entry)
  con = file(log.file,open="at")
  
  #writeLines(json.entry, con)
  close(con) 
}

