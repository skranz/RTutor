# Store user specific information about the problem set solution

# Store in a data.frame for each test:

# i) When first run
# ii) How often failed before successful
# iii) How often hint() called before successful
# iv) When first successful

# clear.user()


#' Specify which information will be automatically saved in ups
default.ups.save = function(
    chunks = TRUE,
    awards = TRUE,
    addons = TRUE,
    code = FALSE | all,
    chunk.ind = FALSE | all,
    all = FALSE
) {
  nlist(
    chunks,
    awards,
    addons,
    code,
    chunk.ind
  )
}


get.user.name = function(ps=get.ps()) {
  ps$user.name
}

init.ups = function(user.name=ps$user.name, ps = get.ps(), ups.save=ps$ups.save) {
  restore.point("init.ups")

  cdt = ps$cdt
  if (isTRUE(ups.save$chunks)) {
    # Store chunk results
    cu = data_frame(solved=rep(FALSE, NROW(cdt)), first.check.date=as.POSIXct(NA),  num.failed=0, num.hint=0, solved.date=as.POSIXct(NA))
  } else {
    cu = NULL
  }

  # Store add-on results
  ao.dt = ps$rps$ao.dt
  if (NROW(ao.dt)>0 & isTRUE(ups.save$addons)) {
    aou = data_frame(solved=rep(FALSE,NROW(ao.dt)) , first.check.date=as.POSIXct(NA),  num.failed=0, num.hint=0, solved.date=as.POSIXct(NA), points=0, score=NA_real_)
  } else {
    aou = NULL
  }

  if (ups.save$chunk.ind) {
    chunk.ind = 1
  } else {
    chunk.ind = NULL
  }
  awards = list()
  ups = as.environment(list(ps.name=ps$name, user.name=user.name, chunk.ind = chunk.ind, cu=cu, aou=aou, awards = awards))

  save.ups(ups=ups,ps=ps)
  ups
}

ups.init.shiny.ps = function(ps=get.ps(), ups=get.ups(), rerun=FALSE, sample.solution=FALSE, precomp=isTRUE(ps$precomp), replace.with.sample.sol = isTRUE(ps$replace.with.sample.sol), ups.save=ps$ups.save) {
  restore.point("init.shiny.ps.from.ups")
  
  if (NROW(ps$cdt)==0) return()

  chunk.ind = ups$chunk.ind
  if (is.null(chunk.ind)) {
    chunk.ind = 1
  }
  
  is.solved = rep(FALSE, NROW(ps$cdt))
  ps$cdt$mode = "output"
  ps$cdt$mode[chunk.ind] = "input"
  
  if (!sample.solution) {
    if (!is.null(ups$cu$stud.code) & !sample.solution) {
      ps$cdt$stud.code = ups$stud.code
    } else if (!sample.solution) {
      ps$cdt$stud.code = ps$cdt$task.txt
    }
    if (!is.null(ups$cu$solved)) {
      is.solved = ups$cu$solved
    } else {
      is.solved =  rep(FALSE, NROW(ps$cdt))
    }
  } else {
    ps$cdt$stud.code = ps$cdt$sol.txt
    is.solved = rep(TRUE, NROW(ps$cdt))
  }
  if (ups.save$code & is.null(ups$cu$stud.code)) {
    ups$cu$stud.code = ps$cdt$stud.code
  }
  
  if (rerun) {
    ps$cdt$is.solved = is.solved
    rerun.solved.chunks(ps)
    ps$cdt$mode[1] = "output"
  } else if (precomp) {
    ps$cdt$is.solved = is.solved
  } else {
    ps$cdt$is.solved = rep(FALSE, NROW(ps$cdt))
  }

  if (replace.with.sample.sol) {
    ps$cdt$stud.code[ps$cdt$is.solved] = ps$cdt$sol.txt[ps$cdt$is.solved]
  }
  
}

get.ups = function() {
  ps = get.ps()
  ps[["ups"]]
}

set.ups = function(ups) {
  ps = get.ps()
  ps[["ups"]] = ups
}


load.ups = function(user.name, ps.name = ps$name, ps = get.ps(),...) {
  restore.point("load.ups")
  
  dir = get.ps()$ups.dir
  file = paste0(dir,"/",user.name,"_",ps$name,".ups")
  
  if (nchar(user.name)==0)
    return(NULL)
  
  
  if (!file.exists(file)) {
    ups = init.ups(user.name = user.name, ps=ps,...)
  } else {
    load(file=file)
  }
  return(ups)
}

update.ups = function(ups = get.ups(), ps=get.ps(), addon=NULL, award=NULL,chunk=NULL, hint=NULL, code=NULL, chunk.ind=NULL, ups.save = ps$ups.save) {
  restore.point("update.ups")
  
  if (!is.null(chunk.ind)) {
    ups$chunk.ind = chunk.ind
  } else if (!is.null(chunk)) {
    ups$chunk.ind = chunk
  } else if (!is.null(hint)) {
    ups$chunk.ind = hint
  } else {
    ups$chunk.ind = ps$chunk.ind
  }

  save.ups(ups=ups,ps=ps)
}

save.ups = function(ups = get.ups(), ps=get.ps()) {
  restore.point("save.ups")
  
  if (isTRUE(ps$save.nothing)) return()

  if (is.null(ups$chunk.ind))
    ups$chunk.ind = ps$chunk.ind
  
  dir = ps$ups.dir
  file = paste0(dir,"/",ups$user.name,"_",ps$name,".ups")

  suppressWarnings(save(ups,file=file))
}

#' Shows your progress
#' @export
stats = function(do.display = TRUE, use.old.stats=FALSE, ups = get.ups()) {

  restore.point("stats")

  ps = get.ps()
  if (is.null(ps)) {
    display("No problem set specified. You must check a problem before you can see your stats.")
    return(invisible())
  }

  if (use.old.stats)
    return(old.stats())




  # Results of chunks
  cu = as_data_frame(cbind(ups$cu, dplyr::select(ps$cdt,ex.ind, points)))
  cu = mutate(cu, type="chunk", max.points = points, points=max.points*solved)

  # Results of addons like quizes

  if (NROW(ups$aou)>0) {
    aou = as_data_frame(cbind(ups$aou, dplyr::select(ps$rps$ao.dt, max.points, ex.name)))
    aou$ex.ind = match(ps$rps$ao.dt$ex.name, ps$edt$ex.name)
    idf = rbind(
      dplyr::select(aou,ex.ind,solved, num.hint, points, max.points),
      dplyr::select(cu,ex.ind, solved, num.hint, points, max.points)
    )

  } else {
    idf = dplyr::select(cu,ex.ind, solved, num.hint, points, max.points)
  }



  # Aggregate on exercise level
  res = group_by(idf, ex.ind) %>%
    summarise(
      points = sum(points),
      max.points = sum(max.points),
      percentage = round(points/max.points*100),
      hints = -sum(num.hint)
    )
  res$ex.name = ps$edt$ex.name[res$ex.ind]
  all.res = idf %>%
    summarise(
      ex.ind = 0,
      points = sum(points),
      max.points = sum(max.points),
      percentage = round(points/max.points*100),
      hints = sum(num.hint),
      ex.name = "Total"
    )
  res = rbind(res, all.res)
  sr = dplyr::select(res,ex.name,percentage, points, max.points, hints)
  colnames(sr) = c("Excercise","Solved (%)","Points", "Max. Points", "Hints")
  rownames(sr) = NULL


  if (do.display) {
    display(ups$user.name, "'s stats for problem set ",ps$name,":\n")
    print(as.data.frame(sr))
    return(invisible(sr))
  }
  sr
}


#' Shows your progress
#' @export
old.stats = function(do.display=TRUE) {
  ps = get.ps()
  if (is.null(ps)) {
    display("No problem set specified. You must check a problem before you can see your stats.")
    return(invisible())
  }

  ups = get.ups()

  res = summarise(group_by(as.data.frame(ups$tdt),ex.ind),
    num.test = length(test.e.ind),
    percentage.solved=round(sum(success)/num.test*100),
    hints = -sum(num.hint),
    end.time = max(success.date)
  )
  res$completed = ifelse(is.na(res$end.time), "",strftime(res$end.time, format="%H:%M %d.%m."))
  res$ex.name = ps$edt$ex.name[res$ex.ind]
  if (do.display) {
    sr = dplyr::select(res,ex.name,percentage.solved,hints, completed)
    colnames(sr) = c("Ex","solved (%)","hints","completed")
    rownames(sr) = NULL
    display(ups$user.name, "'s stats for problem set ",ps$name,":\n")
    print(as.data.frame(sr))
    return(invisible(res))
  }
  return(res)
}


# remove old ups files when new problem set structure is generated
remove.ups = function(ps.name = get.ps()$name, dir = get.ps()$ups.dir) {
  set.ups(NULL)

  if (is.null(dir)) dir =getwd()

  files = list.files(path = dir,full.names = TRUE)
  files = files[str.ends.with(files,paste0("_",ps.name,".ups"))]
  if (length(files)>0) {
    file.remove(files)
  }
}

