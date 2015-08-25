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



rtutor.addon.quiz = function() {
  list(
    package = "RTutor",
    type = "quiz",
    mode = "block",
    parse.fun = rtutor.quiz.block.parse,
    shiny.init.fun = rtutor.quiz.init.shiny,
    shiny.ui.fun = rtutor.quiz.shiny.ui,
    task.txt.fun = function(...) "-- Ommited Quiz---",
    sol.txt.fun = function(...) "-- Ommited Quiz---",
    out.txt.fun = function(...) "-- Ommited Quiz---"
  )
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

  rta = as.environment(list(type=type,name=name,optional=TRUE, prev.solved=FALSE, solved=FALSE))
  qu$rta = rta
  qu
}

rtutor.quiz.handler = function(app,qu,part.ind, part.solved, solved,...) {
  cat("Quiz solved = ", solved)
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
  
  runEventsApp(app, launch.browser=rstudio::viewer)
  
}

shinyQuiz = function(id=paste0("quiz_",sample.int(10e10,1)), yaml, quiz.handler=NULL, add.handler=TRUE, single.check.btn=TRUE) {
  restore.point("shinyQuiz")
  
  qu = parse.quiz.yaml(yaml, id)
  
  qu$single.check.btn = single.check.btn
  if (qu$single.check.btn) {
     qu$checkBtnId = paste0(qu$id,"__checkBtn")
  }
  
  qu$parts = lapply(seq_along(qu$parts), function(ind) init.quiz.part(qu$parts[[ind]],ind,qu))
  np = length(qu$parts)
  qu$state = as.environment(list(part.solved=rep(FALSE,np), solved=FALSE))

  
  if (add.handler)
    add.quiz.handlers(qu, quiz.handler)
  qu
}

parse.quiz.yaml = function(yaml, id=paste0("quiz_",sample.int(10e10,1))) {
  restore.point("parse.quiz.yaml")
  library(YamlObjects)
  qu = read.yaml(text=yaml)
  
  if (is.null(qu[["id"]])) {
    qu$id = id
  }
  if (is.null(qu$parts)) {
    qu$parts = list(qu)
  }
  
  qu    
}

init.quiz.part = function(part=qu$parts[[part.ind]], part.ind=1, qu, has.check.btn=!qu$single.check.btn) {
  restore.point("init.quiz.part")
  
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
      if (is.null(part$roundto)) part$roundto=0
    } else {
      part$type = "text"
    }
  } else {
    stop(paste0("The quiz with question ", part$question, " has neither defined the field 'answer' nor the field 'choices'."))
  } 
  
  if (is.null(part$success)) {
    part$success = "<p><font color='blue'>correct</font></p>"
  } else {
    part$success =  markdownToHTML(text=part$success,encoding = "UTF-8", fragment.only=TRUE)
  }
  if (is.null(part$failure)) {
    part$failure = "<p><font color='red'>not correct</font></p>"
  } else {
    part$failure =  markdownToHTML(text=part$failure,encoding = "UTF-8", fragment.only=TRUE)
  }
  
  
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
  
  part
}

quiz.ui = function(qu) {
  restore.point("quiz.ui")
  pli = lapply(seq_along(qu$parts), function(i) {
    part = qu$parts[[i]]
    if (i < length(qu$parts)) return(list(part$ui,hr()))
    part$ui
  })
  if (!is.null(qu$checkBtnId)) {
    pli = c(pli, list(actionButton(qu$checkBtnId,label = "check")))
  }
  
  pli
}

quiz.part.ui = function(part) {
  head = list(
    HTML(paste0("<p>",part$question,"</p>"))
  )
  if (part$type=="numeric") {
    answer = numericInput(part$answerId, label = "",value = NULL)  
  } else if (part$type =="text") {
    answer = textInput(part$answerId, label = "",value = "")  
  } else if (part$type=="mc") {
    answer = checkboxGroupInput(part$answerId, "",part$choices)
  } else if (part$type=="sc") {
    answer = radioButtons(part$answerId, "",part$choices, selected=NA)
  }
  if (!is.null(part$checkBtnId)) {
    button = actionButton(part$checkBtnId,label = "check")
  } else {
    button = NULL
  }
  list(head,answer,button, uiOutput(part$resultId))
}

add.quiz.handlers = function(qu, quiz.handler=NULL){
  restore.point("add.quiz.handlers")
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
  
  
  if (part$type =="numeric") {
    answer = as.numeric(answer)
    correct = is.true(abs(answer-part$answer)<part$roundto)
  } else {
    correct = setequal(answer,part$answer)
  }
  if (correct) {
    cat("Correct!")
    setUI(part$resultId,HTML(part$success))
  } else {
    cat("Wrong")
    setUI(part$resultId,HTML(part$failure))
  }
  qu$state$part.solved[part.ind] = correct
  qu$state$solved = all(qu$state$part.solved)
  
  if (!is.null(quiz.handler)) {
    quiz.handler(app=app, qu=qu, part.ind=part.ind, part.correct=correct, solved=qu$state$solved)
  }
  
}
