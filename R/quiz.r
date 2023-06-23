# #< quiz
# parts:
#   - question: What is 20*20?
#     choices:
#         - 100
#         - 200
#         - 400*
#         - 500
#     multiple: FALSE
#     success: Great, your answer is correct!
#     failure: Try again.
#   - question: State pi up to 2 digits
#     answer: 3.14
#     roundto: 0.01
# award:
#   title: Quiz master
#   text: You solved the quiz!
# #>

answer.quiz = function(name, ps = get.ps()) {
  restore.point("answer.quiz")
  
  qu = ps$rps$addons[[paste0("addon__quiz__", name)]]
  if (is.null(qu)) {
    message(paste0("Cannot find quiz ", name,". Check the problem set at least once before calling this function."))
    return(invisible())
  }
  
  if (length(qu$parts)>1) {
    stop("Sorry, multi part quizzes cannot yet be solved inside RStudio.")
  }
  part = qu$parts[[1]]
  
  if (part$type == "sc") {
    msg = paste0("Type the answer's number (1-",length(part$choices),") and then press Enter: ")
  } else if (part$type != "mc") {
    msg = paste0("Type the answer and then press Enter: ")
  } else {
    stop("Multiple choice quizzes with more than one solution are not yet supported in the RStudio environment.")
  }
  # Give focus to console
  try(rstudioapi::sendToConsole("", execute=FALSE, echo=TRUE),silent = TRUE)
  
  # Collect answer
  answer <- readline(prompt=msg)
  
  
  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else if (part$type == "sc"){
    answer = as.numeric(answer)
    if (is.na(answer)) {
      cat("\nYou have to enter the number of the correct answer.")
      return(invisible())
    }
    correct = isTRUE(answer==part$correct.choices)
  } else {
    correct = setequal(answer,part$answer)
  }
  
  if (correct) {
    ans.msg = part$success
    ans.msg = gsub("<p><font color='black'>","",ans.msg, fixed=TRUE)
    ans.msg = gsub("</font></p>","",ans.msg, fixed=TRUE)
    cat(paste0("\n",ans.msg))
  } else {
    ans.msg = part$failure
    ans.msg = gsub("<p><font color='red'>","",ans.msg, fixed=TRUE)
    ans.msg = gsub("</font></p>","",ans.msg, fixed=TRUE)
    message(paste0(ans.msg))
  }
  
  # Update state
  qu$state$part.solved[1] = correct
  qu$state$solved = all(qu$state$part.solved)

  
  rta = qu$rta; state=qu$state
  rta$solved = state$solved
  rta$points = (sum(state$part.solved) / length(state$part.solved))*rta$max.points
  
  # Save ups with correctly answered quiz
  process.checked.addon(rta,from.shiny=FALSE)
  return(invisible())
}


rtutor.addon.quiz = function() {
  list(
    package = "RTutor",
    type = "quiz",
    mode = "block",
    parse.fun = rtutor.quiz.block.parse,
    shiny.init.fun = rtutor.quiz.init.shiny,
    shiny.ui.fun = rtutor.quiz.shiny.ui,
    task.txt.fun = rtutor.quiz.task.txt.fun,
    sol.txt.fun = rtutor.quiz.sol.txt.fun,
    out.txt.fun = rtutor.quiz.sol.txt.fun
  )
}

rtutor.quiz.task.txt.fun = function(ao,solved=FALSE,...) {
  quiz.md(ao,solution = solved)
} 

rtutor.quiz.sol.txt.fun = function(ao,solved=TRUE,...) {
  quiz.md(ao,solution = solved)
}

rtutor.quiz.shiny.ui = function(ao, ...) {
  quiz.ui(ao)  
}

rtutor.quiz.init.shiny = function(ao,ps=get.ps(), app=getApp(),...) {
  add.quiz.handlers(qu=ao, quiz.handler=rtutor.quiz.handler)    
}

rtutor.quiz.block.parse = function(txt,type="quiz",name="",id=paste0("addon__",type,"__",name),args=NULL,...) {
  restore.point("rtutor.quiz.block.parse")
  qu = shinyQuiz(id = id,yaml = merge.lines(txt),add.handler = FALSE)

  rta = as.environment(list(
    id=id,type=type,optional=TRUE, changes.env=FALSE, max.points=qu$max.points,
    solved=FALSE, points=0, was.solved=FALSE, had.points=0
  ))
  qu$rta = rta
  qu
}

rtutor.quiz.handler = function(app,qu,part.ind, part.solved, solved,...) {
  restore.point("rtutor.quiz.handler")
  
  rta = qu$rta; state=qu$state
  
  rta$solved = state$solved
  rta$points = (sum(state$part.solved) / length(state$part.solved))*rta$max.points
  process.checked.addon(rta)
}

examples.quiz = function() {
    yaml = '
parts:
  - question: What is 20*20?
    choices:
        - 100
        - 200
        - 400*
        - 500
    multiple: FALSE
    success: Great, your answer is correct!
    failure: Try again.
  - question: State pi up to 2 digits
    answer: 3.14
    roundto: 0.01
award:
  title: Quiz master
  text: You solved the quiz!

  '
  app = eventsApp()

  qu = parse.quiz.yaml(yaml)
  qu$ui = quiz.ui(qu)
  app$ui = qu$ui
  add.quiz.handlers(qu)
  
  runEventsApp(app, launch.browser=rstudioapi::viewer)
  
}

quizDefaults = function(lang="en") {
  if (lang=="de") {
    list(
      success = "Richtig!",
      failure= "Leider noch nicht richtig.",
      success_color = "black",
      failure_color = "red",
      points_txt = "Punkte",
      point_txt = "Punkt"
    )
  } else {
    list(
      success = "Great, you answered correctly!",
      failure= "Sorry, not yet correct.",
      success_color = "black",
      failure_color = "red",
      points_txt = "points",
      point_txt = "point"
    )
  }
}

# Create a shiny quiz widget
#
# @param id the id of the quiz
# @param qu a list that contains the quiz fields as would have
#        been parsed by read.yaml from package YamlObjects
# @param yaml alternatively to qu, is yaml a string that specifies the quiz
# @param quiz.handler a function that will be called if the quiz is checked.
#        The boolean argument solved is TRUE if the quiz was solved
#        and otherwise FALSE
shinyQuiz = function(id=paste0("quiz_",sample.int(10e10,1)),qu=NULL, yaml,  quiz.handler=NULL, add.handler=TRUE, single.check.btn=TRUE, defaults=quizDefaults(lang=lang), lang="en") {
  restore.point("shinyQuiz")

  if (is.null(qu)) {
    yaml = enc2utf8(yaml)
    qu = try(mark_utf8(read.yaml(text=yaml)), silent=TRUE)
    if (is(qu,"try-error")) {
      err = paste0("When importing quiz:\n",paste0(yaml, collapse="\n"),"\n\n",as.character(qu))
      stop(err,call. = FALSE)
    }
  }

  if (is.null(qu[["id"]])) {
    qu$id = id
  }
  if (is.null(qu$parts)) {
    qu$parts = list(qu)
  }


  qu$single.check.btn = single.check.btn
  if (qu$single.check.btn) {
     qu$checkBtnId = paste0(qu$id,"__checkBtn")
  }

  qu$parts = lapply(seq_along(qu$parts), function(ind) init.quiz.part(qu$parts[[ind]],ind,qu))
  np = length(qu$parts)
  
  qu$max.points = sum(sapply(qu$parts, function(part) part[["points"]]))
  
  qu$state = as.environment(list(part.solved=rep(FALSE,np), solved=FALSE))

  qu$ui = quiz.ui(qu)

  if (add.handler)
    add.quiz.handlers(qu, quiz.handler)
  qu
}

init.quiz.part = function(part=qu$parts[[part.ind]], part.ind=1, qu, has.check.btn=!qu$single.check.btn, defaults=quizDefaults()) {
  restore.point("init.quiz.part")

  part = copy.into.missing.fields(dest=part, source=defaults)

  if (!is.null(part[["sc"]])) {
    part$choices = part$sc
    part$multiple = FALSE
    #part$type = "sc"
  } else if (!is.null(part[["mc"]])) {
    part$choices = part$mc
    part$multiple = TRUE
    #part$type = "mc"
  }


  if (!is.null(part$choices)) {
    correct.choices = which(str.ends.with(part$choices,"*"))
    if (is.null(part$multiple)) {
      part$multiple = length(correct.choices) != 1
    }
    part$correct.choices = correct.choices
    part$choices[correct.choices] = str.remove.ends(part$choices[correct.choices],right=1)
    part$answer = unlist(part$choices[correct.choices])
    names(part$choices) =NULL
    if (part$multiple) {
      part$type = "mc"
    } else {
      part$type = "sc"
    }
  } else if (!is.null(part$answer)) {
    if (is.numeric(part$answer)) {
      part$type = "numeric"
      if (is.null(part$roundto)) part$roundto=1e-7
    } else {
      part$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", part$question, " has neither defined the field 'answer' nor the field 'choices'."))
  }

  if (is.null(part[["points"]])) {
    part$points = 1
  }

  txt = part$success
  
  if (part$points==1) {
    txt = paste0(txt," (", part$points, " ", defaults$point_txt,")")
  } else if (part$points > 0 ) {
    txt = paste0(txt," (", part$points, " ", defaults$points_txt,")")
  }
  txt = colored.html(txt, part$success_color)
  part$success =  markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)

  txt = colored.html(part$failure, part$failure_color)
  part$failure =  markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)

  part$id = paste0(qu$id,"__part", part.ind)
  part$answerId = paste0(part$id,"__answer")
  if (has.check.btn) {
    part$checkBtnId = paste0(part$id,"__checkBtn")
  } else {
    part$checkBtnId = NULL
  }
  part$resultId = paste0(part$id,"__resultUI")
  part$ui = quiz.part.ui(part)
  part$solved = FALSE

  if (is.null(part$points)) {
    part$points = 1
  }
  
  part
}

quiz.ui = function(qu, solution=FALSE) {
  restore.point("quiz.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    if (i < length(qu$parts)) {
      hr = hr()
    } else {
      hr = NULL
    }

    if (solution) {
      if (is.null(part$sol.ui)) {
        part$sol.ui = quiz.part.ui(part, solution=TRUE)
      }
      return(list(part$sol.ui,hr))
    } else {
      return(list(part$ui,hr))
    }
  })
  if (!is.null(qu$checkBtnId)) {
    pli = c(pli, list(actionButton(qu$checkBtnId,label = "check")))
  }

  pli
}

quiz.part.ui = function(part, solution=FALSE, add.button=!is.null(part$checkBtnId)) {
  #question = rmdtools::md2html(part$question)
  question = markdownToHTML(text=part$question, fragment.only=TRUE,encoding="UTF-8")
  head = list(
    HTML(paste0("<p>",question,"</p>"))
  )
  if (solution) {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = "",value = part$answer)
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = "",value = part$answer)
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, "",part$choices,selected = part$answer)
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, "",part$choices, selected=part$answer)
    }
  } else {
    if (part$type=="numeric") {
      answer = textInput(part$answerId, label = "",value = "")
    } else if (part$type =="text") {
      answer = textInput(part$answerId, label = "",value = "")
    } else if (part$type=="mc") {
      answer = checkboxGroupInput(part$answerId, "",part$choices)
    } else if (part$type=="sc") {
      answer = radioButtons(part$answerId, "",part$choices, selected=NA)
    }
  }

  if (add.button) {
    button = actionButton(part$checkBtnId,label = "check")
  } else {
    button = NULL
  }
  list(head,answer,uiOutput(part$resultId),button)
}

quiz.md = function(qu, solution=FALSE, add.numbers=FALSE) {
  restore.point("quiz.md")
  li = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    quiz.part.md(part, solution=solution, add.numbers=add.numbers)
  })
  paste0(li, collapse="\n")
}


quiz.part.md = function(part, solution=FALSE, add.numbers=FALSE) {
  restore.point("quiz.part.md")
  
  head = paste0("\nQuiz: ",part$question,"\n")
  if (solution) {
    if (part$type=="numeric" | part$type == "text") {
      answer = paste0("Answer: ", part$answer)
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      mark = rep("[ ]", length(ans))
      mark[ans %in% part$answer] =  "[x]"
      answer = paste0("- ", ans, " ", mark,"\n", collapse="\n")
    }
  } else {
    if (part$type=="numeric" | part$type == "text") {
      answer = "Answer: "
    } else if (part$type=="mc" | part$type=="sc") {
      ans = part$choices
      if (add.numbers) {
        #answer = paste0("[", seq_along(ans),"]: ", ans, collapse="\n")
        answer = paste0("(", seq_along(ans),") ", ans, collapse="\n")
      } else {
        answer = paste0("- ", ans, " [   ]\n", collapse="\n")
      }
    }
  }
  paste0(head,"\n", answer)
}


add.quiz.handlers = function(qu, quiz.handler=NULL, id=qu$id){
  restore.point("add.quiz.handlers")
  app = getApp()
  if (is.null(app)) {
    cat("\nCannot add quiz handlers since no shinyEvents app object is set.")
    return()
  }

  if (!qu$single.check.btn) {
    for (part.ind in seq_along(qu$parts)) {
      part = qu$parts[[part.ind]]
      buttonHandler(part$checkBtnId,fun = click.check.quiz, part.ind=part.ind, qu=qu, quiz.handler=quiz.handler)
    }
  } else {
    buttonHandler(qu$checkBtnId,fun = click.check.quiz, part.ind=0, qu=qu, quiz.handler=quiz.handler)
  }
}

click.check.quiz = function(app=getApp(), part.ind, qu, quiz.handler=NULL, ...) {
  restore.point("click.check.quiz")

  # check all parts
  if (part.ind == 0) {
    for (part.ind in seq_along(qu$parts))
      click.check.quiz(app=app, part.ind=part.ind,qu=qu, quiz.handler=NULL)

    if (!is.null(quiz.handler)) {
      quiz.handler(app=app, qu=qu, part.ind=0, part.correct=NA, solved=qu$state$solved)
    }
    return(qu$state$solved)
  }

  part = qu$parts[[part.ind]]
  answer = getInputValue(part$answerId)
  restore.point("click.check.quiz.inner")


  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else {
    correct = setequal(answer,part$answer)
  }
  if (correct) {
    cat("Correct!")
    setUI(part$resultId,withMathJax(HTML(part$success)))
  } else {
    cat("Wrong")
    setUI(part$resultId,withMathJax(HTML(part$failure)))
  }
  qu$state$part.solved[part.ind] = correct
  qu$state$solved = all(qu$state$part.solved)

  if (!is.null(quiz.handler)) {
    quiz.handler(app=app, qu=qu, part.ind=part.ind, part.correct=correct, solved=qu$state$solved)
  }

}


replace.quizes.by.chunks = function(txt) {
  #txt = readLines("C:/libraries/RTutor/examples/Quiz2_sol.Rmd")
  
  restore.point("replace.quizes.by.chunks")
  start.lines = which(startsWith(txt, "#< quiz"))
  if (length(start.lines)==0) return(txt)
  
  all.end.lines = which(startsWith(txt, "#>"))

  end.lines = rep(0,length((start.lines)))
  for (i in seq_along(start.lines)) {
    end.lines[i] = min(all.end.lines[all.end.lines > start.lines[i]])
  }
  
  i = 1
  new.txt = sapply(seq_along(start.lines), function(i) {
    str = txt[start.lines[i]:end.lines[i]]
    qu = rtutor.quiz.block.parse(str)
    
    quiz = md = quiz.md(qu, solution=FALSE, add.numbers=TRUE)
    
    #str = trimws(str)
    has.success = any(startsWith(str,"success:"))
    has.failure = any(startsWith(str,"failure:"))
    
    part = qu$parts[[1]]
    if (has.failure) {
      hint.add = paste0(str.between(part$failure,"'red'>","</font>"),"\n")
    } else {
      hint.add = "If you do not know the correct answer, you can try out different ones.\n"
    }
    if (has.success) {
      award.txt = paste0(
'#< award "Quiz ', i,'"

', part$success,'
#>')
    } else {
      award.txt = ""
    }
    
    
    chunk = paste0(
'

```{r}
#< fill_in
# Replace the ___ by the correct answer 1-',length(qu$parts[[1]]$sc),' of the quiz.

answer_number = ___
#>
answer_number = ',qu$parts[[1]]$correct.choices,'
#< hint
cat("For example, if you think the 2nd answer of the quiz above is correct then write

answer_number = 2

', hint.add,'
")
#>
```
')
  paste0(md, chunk, award.txt)
  })

  txt[start.lines] = new.txt
  del.lines = sapply(seq_along(start.lines), function(i) {
    (start.lines[i]+1):end.lines[i]
  }) %>% unlist()
  txt = txt[-del.lines]
  txt = sep.lines(txt)
  txt
}

