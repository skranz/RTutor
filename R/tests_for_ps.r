# Direct testing functions

#' Tests whether a package is loaded. Not yet implemented for speed reasons
#' @export
check.package = function(package) {
  return(TRUE)
}


examples.check.function = function() {
## Here is an example of how you could implement check.function in a solution file.
  
#< task
ols = function(y,X) {
  
  # enter code to compute beta.hat here ...
  
  return(as.numeric(beta.hat))
}
#>
#< test
check.function({
  ols = function(y,X) {
    beta.hat = solve(t(X) %*% X) %*% t(X) %*% y
    return(as.numeric(beta.hat))
  }},
  ols(c(100,50,20,60),cbind(1,c(20,30,15,20)))
)
#>

}

#' Checks a function written by the student
#' 
#' @param code code of the form {fun_name = function(x,y) {#body of function}}. It is important to wrap the code in {} and to assign the function name with = (don't use <-). See example below.
#' @param ... you can add several test calls to the function. It will be checked whether the users' function returns the same values in those calls than the function in the solution. You can also have a code block wrapped in {} that ends with a call to the function. In this way you can e.g. specify a random seeds before calling the function.
#' @param check.args if TRUE check the arguments of the user function. If a character vector only check the given arguments.
#' @param check.defaults TRUE = check the default values of the arguments of the user function. If a character vector only check the default values of the given arguments.
#' @param check.args.order if TRUE make sure that the checked arguments appear in the same order in the user function than in the solution
#' @param allow.extra.arg if TRUE the user function can have additional arguments (at the end) that are not in the solution
#' @export
check.function = function(code, ..., check.args = TRUE, check.defaults=FALSE, check.args.order=TRUE, allow.extra.arg = TRUE, ps=get.ps(),stud.env = ps$stud.env, verbose=FALSE, part = NULL) {

  test.calls = eval(substitute(alist(...)))

  code = substitute(code)
  
  
  restore.point("check.function")

  #part.str = ifelse(is.null(part),"",paste0(" in part ", part))
  part.str = paste0(" in chunk ",  ps$chunk.name)

  
  env = new.env(parent=stud.env)
  eval(code,env)
  fun.name = ls(env)[1]
  sol.fun = get(fun.name,env)
  
  if (!exists(fun.name,stud.env, inherits=FALSE)) {
    short.failure = paste0(fun.name, " does not exist.")
    failure.message = paste0("You have not yet created the function ",fun.name, part.str, ".")
    add.failure(short.failure,failure.message, var = var)
    return(FALSE)
  }
  
  # Check the function's arguments

  stud.fun = get(fun.name,stud.env)
  stud.args = formals(stud.fun)
  sol.args = formals(sol.fun)
  
  if (identical(check.args,TRUE)) check.args = names(sol.args)
  if (identical(check.args,TRUE)) check.defaults = names(sol.args)
  
  if (is.character(check.args)) {
    missing.args = setdiff(check.args, names(stud.args))
    if (length(missing.args)>0) {
      failure.message = paste0("Your function ", fun.name, part.str, " misses the argument(s) ", paste0(missing.args, collapse=", "),".")
      add.failure(failure.message)
      return(FALSE) 
    }
    arg.ind = seq_along(check.args)
    if (check.args.order) {
      if (!identical(names(stud.args)[arg.ind], check.args)) {
        failure.message = paste0("Your function ", fun.name, part.str, " has the wrong order of arguments. Please arrange them as follows: ", paste0(check.args, collapse=", "),".")
        add.failure(failure.message)
        return(FALSE) 
      }
    }
    if (!identical(stud.args[check.defaults], sol.args[check.defaults])) {
      failure.message = paste0("Not all arguments of your function ", fun.name, " have the correct default value.")
      add.failure(failure.message)
      return(FALSE) 
    }
    if (!allow.extra.arg) {
      extra.args = set.diff(names(stud.args),names(sol.args))
      failure.message =  paste0("Your function ", fun.name, " is not allowed to have the additional arguments ", paste0(extra.args, collapse=", "),".")
      add.failure(failure.message)
      return(FALSE) 
    }
  }
  
  # Test calls
  stud.tenv = new.env(parent=stud.env)
  sol.tenv = new.env(parent=stud.env)
  assign(fun.name, sol.fun,sol.tenv)
  
  i = 1
  for (i in seq_along(test.calls)) {
    sol.res = eval(test.calls[[i]], sol.tenv)
    ok = TRUE
    failure.message = ""
    stud.res = tryCatch(eval(test.calls[[i]], stud.tenv),
                        error = function(e) {
                          failure.message <<- as.character(e)
                          ok <<- FALSE
                        })
    if (!ok) {
      add.failure(failure.message,...)
      return(FALSE) 
    }                    
    
    res = compare.values(stud.res, sol.res)
    if (length(res)>0) {
      call.str = deparse1(test.calls[[i]], collapse="\n")
      failure.message = paste0("Your function ",fun.name, " seems not ok. I test it with the call\n\n", call.str, "\n\n and your returned object differs from my solution, it has wrong ", paste0(res, collapse=","),".\n\n I stored in the variables 'test.sol.res' and 'test.your.res' the return values of the correct function and your function. You can take a look at them.")
      add.failure(failure.message,...)
      assign(paste0("test.sol.res"),sol.res,.GlobalEnv)
      assign(paste0("test.your.res"),stud.res,.GlobalEnv)
      return(FALSE) 
    }
  }
  
  success.message = paste0("Great, I good not find an error in your function ", fun.name, "!")
  add.success(success.message,...)
  return(TRUE)    
}




#' Simply shows a success message when this test is reached for the first time!
#' @export 
show.success.message = function(success.message,...) {
  add.success(success.message,...)
  return(TRUE)
}


#' Checks whether the user makes a particular function call in his code or call a particular R statement
#' @export
check.call = function(call, check.arg.by.value=TRUE, allow.extra.arg=FALSE, ignore.arg=NULL, success.message=NULL, failure.message = NULL,no.command.failure.message = NULL, ok.if.same.val = FALSE,s3.method=NULL,
  ps=get.ps(),stud.env = ps$stud.env, part=ps$part, stud.expr.li = ps$stud.expr.li, verbose=FALSE,  ... 
) {

  expr = substitute(call)
  #restore.point("check.call")
    
  #part.str = ifelse(is.null(part),"",paste0(" in part ", part))
  part.str = paste0(" in chunk ",  ps$chunk.name)
  
  ce = match.call.object(expr, envir=stud.env, s3.method=s3.method)
  dce = describe.call(call.obj=ce)
  check.na = dce$name
  
  stud.na = sapply(stud.expr.li,  name.of.call)
  # Filter all student calls that have the same name of call 
  stud.expr.li = stud.expr.li[which(stud.na == check.na)]

  
  # Check if a student call with the same name has the same return value
  if (ok.if.same.val) {
    check.val <- eval(ce, stud.env)
    ok = FALSE
    for (se in stud.expr.li) {
      tryCatch({
        sval <- eval(se,stud.env)
        if (is.same(check.val,sval)) {
          ok <- TRUE
          break
        }
      }, error = function(e){})
    }
    if (ok) {
      success.message = paste0("Great,",part.str," you correctly (I guess) called the command: ",deparse1(se))
      add.success(success.message)
      return(TRUE)  
    }
  }

  stud.expr.li = lapply(stud.expr.li, function(e) match.call.object(e, envir=stud.env, s3.method=s3.method))

  
  ret = internal.check.call(ce,dce, stud.expr.li,stud.env, allow.extra.arg=allow.extra.arg, ignore.arg=ignore.arg, check.arg.by.value=check.arg.by.value)
  if (ret[[1]]==TRUE) {
     success.message = paste0("Great,",part.str," you correctly called the command: ",ret[[2]])
     add.success(success.message)
     return(TRUE)
  } else {

    if (is.null(failure.message))
      failure.message = paste0("You have not yet entered all correct commands", part.str,".")   
    add.failure(failure.message,...)
    return(FALSE)    
  }
}

internal.check.call = function(ce,dce, stud.expr.li,stud.env, allow.extra.arg=FALSE, ignore.arg=NULL,check.arg.by.value=TRUE) {
  restore.point("internal.check.call")
  check.na = dce$name
  stud.na = sapply(stud.expr.li,  name.of.call)

  
  # Filter all student calls that have the same name of call 
  stud.expr.li = stud.expr.li[which(stud.na == check.na)]
  if (length(stud.expr.li)==0) {
    return(list(FALSE,"not found"))
  }

  # Check if directly the same call exists
  if (list(ce) %in% stud.expr.li) {
    return(list(TRUE,deparse1(ce)))
  }
  
  if (dce$type == "fun") {    
    if (check.na %in% c("library")) {
      check.arg.by.value = FALSE
    }
    
    # Environment in which argument values shall be evaluated. Is a data frame
    # if the function is a dplyer function like mutate(dat,...)
    if (check.arg.by.value) {
      val.env = stud.env
      if (is.dplyr.fun(check.na)) {
        val.env = eval(dce$arg[[".data"]],val.env)
      }
    } else {
      val.env = NULL
    }
    
    correct = sapply(stud.expr.li,check.call.args,check.call=ce, val.env = val.env, allow.extra.arg=allow.extra.arg, ignore.arg=ignore.arg)
    if (any(correct)) {
      call.str = deparse1(stud.expr.li[[which(correct)[1]]])
      return(list(TRUE, call.str))
    }
    return(list(FALSE,"wrong arg"))
  }

  # For the moment let us check everything but fun by the return value
  if (dce$type != "fun") {
    check.val = eval(ce, stud.env)
    
    se = stud.expr.li[[1]]
    for (se in stud.expr.li) {
      has.error = FALSE
      tryCatch( stud.val <- eval(se, stud.env),
        error = function(e) has.error <<- TRUE
      )
      if (!has.error) {
        if (is.same(check.val, stud.val)) {
          call.str = deparse1(se)
          return(list(TRUE, call.str))
        }
      }
    }
    return(list(FALSE, "not found"))
  }
  return(list(TRUE, "test not implemented"))  
}

standardize.assign = function(call, null.if.no.assign=TRUE) {
  #restore.point("standardize.assign")
  if (length(call)<=1)
    return(NULL)
  char.op = as.character(call[[1]])
  if (char.op == "=" | char.op == "<-") {
    call[[1]] <- `<-`
    return(call)
  }
  if (null.if.no.assign)
    return(NULL)
  call
}

#' Checks an assignment to a variable
#' @export
check.assign = function(
  call,check.arg.by.value=TRUE, allow.extra.arg=FALSE, ignore.arg=NULL, success.message=NULL, failure.message = NULL,no.command.failure.message = "You have not yet included correctly, all required R commands in your code...", ok.if.same.val = TRUE,call.object=NULL,  s3.method=NULL,
  ps=get.ps(),stud.env = ps$stud.env, part=ps$part, stud.expr.li = ps$stud.expr.li, verbose=FALSE,  ...) {

  if (!is.null(call.object)) {
    call = call.object
  } else {
    call = substitute(call)
  }
  #part.str = ifelse(is.null(part),"",paste0(" in part ", part))
  part.str = paste0(" in chunk ",  ps$chunk.name)

  check.expr = match.call.object(call,envir=stud.env, s3.method=s3.method)

  restore.point("check.assign")
  
  check.expr = standardize.assign(check.expr)
  
  stud.expr.li = lapply(as.list(stud.expr.li), standardize.assign)
  stud.expr.li = stud.expr.li[(!sapply(stud.expr.li,is.null))]
  
  # Check names
  var = deparse1(check.expr[[2]])
  var.expr = check.expr[[2]]
  stud.var = sapply(stud.expr.li,function(e) deparse1(e[[2]]))
  stud.expr.li = stud.expr.li[stud.var == var]

  if (length(stud.expr.li) == 0) {
    if (is.null(failure.message))
      failure.message = paste0("You have not yet made an assignment to ", var, part.str,".")
    add.failure(failure.message,...)
    return(FALSE) 
  }

  
  ce.rhs = match.call.object(check.expr[[3]], envir=stud.env,s3.method=s3.method)
  dce.rhs = describe.call(call.obj=ce.rhs)
  se.rhs.li = lapply(stud.expr.li, function(e) match.call.object(e[[3]], envir=stud.env, s3.method=s3.method))

  # Check if a student rhs has the same return value as ce.rhs
  if (ok.if.same.val) {
    check.val = eval(ce.rhs, stud.env)
    ok = FALSE
    if (length(se.rhs.li)>1) {
      for (se.rhs in se.rhs.li) {
        tryCatch({
          sval = eval(se.rhs,stud.env)
          if (is.same(check.val,sval)) {
            ok <- TRUE
            break
          }
        }, error = function(e){})
      }
    } else {
      tryCatch({
          sval = eval(var.expr,stud.env)
          if (is.same(check.val,sval)) {
              ok <- TRUE
          }
        }, error = function(e){})
    }
    if (ok) {
     success.message = paste0("Great,",part.str," you correctly assigned ", var,"!")
     add.success(success.message)
     return(TRUE)  
    }
  }
  
  ret = internal.check.call(ce.rhs,dce.rhs, se.rhs.li,stud.env,allow.extra.arg=allow.extra.arg, ignore.arg=ignore.arg, check.arg.by.value=check.arg.by.value)
  if (ret[[1]]==TRUE) {
     success.message = paste0("Great,",part.str," you correctly assigned ", var, " = ",ret[[2]],"!")
     add.success(success.message)
     return(TRUE)
  } else {
    if (is.null(failure.message))
      failure.message = paste0("You have not made a correct assignment to ", var, part.str,".")   
    add.failure(failure.message)
    return(FALSE)    
  }
}


#' Check whether a given file exists
#' @export
check.file.exists = function(
  file,
  failure.message=paste0('Sorry, but I cannot find the file "', file,'" in your current working directory.'),
  success.message=paste0('Great, I have found the file "', file,'"!'), ps = get.ps(), part=NULL,
...) {
# Check given variables

  restore.point("check.file.exists")
  if (file.exists(file)) {
    add.success(success.message,...)
    return(TRUE)    
  }
  add.failure(failure.message,...)
  return(FALSE)
}  

#' Check whether an object from a call to lm, glm or some other regression function is correct
#' @export 
check.regression = function(var, str.expr, part=NULL, ps=get.ps(),stud.env = ps$stud.env, verbose=FALSE,  failure.message = paste0("Hmm... your regression ", var," seems incorrect."), success.message = paste0("Great, your regression ", var," looks correct."), tol = 1e-10) {
  restore.point("check.regression")

  ret = check.variable(var,str.expr=str.expr,check.all=FALSE,exists=TRUE, class=TRUE)
  if (!ret) return(FALSE)
  
  cond.str = paste0('
  {
    coef1 = coef(',var,')
    coef2 = coef(',str.expr,')
    if (length(coef1) != length(coef2))
      return(FALSE)
    isTRUE(max(sort(coef1)-sort(coef2))<',tol,') & setequal(names(coef1),names(coef2))
  }
  ')
  ret = holds.true(cond.str = cond.str, success.message = success.message,
             failure.message = failure.message)
  
  if (!ret) return(FALSE)
  return(TRUE)
}



#' Test: Compare the expression check.expr evaluated in the student's environment with the solution correct.expr
#' 
#' @param vars a variable name or vector of variable names
#' @param exists shall existence be checked (similar length, class, values)
#' @param failure.exists a message that is shown if the variable does not exists (similar the other failure.??? variables)
#' @param failure.message.add a text that will be added to all failure messages
#' @param expr
#' @export 
check.expr = function(check.expr, correct.expr,
    failure.message = "{{check_expr}} has the wrong values!",
    success.message = "Great, {{check_expr}} seems correct.",
    part =NULL, ps=get.ps(),stud.env = ps$stud.env, verbose=FALSE,
    unsubst.check.expr = NULL, unsubst.correct.expr=NULL,
    str.check.expr=NULL,str.correct.expr=NULL,
    tol = .Machine$double.eps ^ 0.5) {

  if (!is.null(unsubst.check.expr)) {
    check.expr = unsubst.check.expr
  } else if (!is.null(str.check.expr)) {
    check.expr = parse(text=str.check.expr, srcfile = NULL)
  } else {
    check.expr = substitute(check.expr)
  }
  
  if (!is.null(unsubst.correct.expr)) {
    correct.expr = unsubst.correct.expr
  } else if (!is.null(str.correct.expr)) {
    correct.expr = parse(text=str.correct.expr, srcfile = NULL)
  } else {
    correct.expr = substitute(correct.expr)
  }
    
  val.check = eval(check.expr,stud.env)
  val.sol = eval(correct.expr,stud.env)
  
  check.expr.str = deparse1(check.expr)
  
  if (!identical(class(val.check),class(val.sol))) {
    add.failure(failure.message, check_expr=check.expr.str)
    return(FALSE)
  }
  if (!identical(length(val.check),length(val.sol))) {
    add.failure(failure.message, check_expr=check.expr.str)
    return(FALSE)
  }
              
  if (is.list(val.check) | is.environment(val.sol)) {
    if (!identical(val.sol, val.stud, ignore.environment=TRUE)) {
      add.failure(failure.message, check_expr=check.expr.str)
      return(FALSE)
    }
  } else {
    if (! all(val.check==val.sol)) {
      add.failure(failure.message, check_expr=check.expr.str)
      return(FALSE)
    }
  }
  add.success(success.message)
  return(TRUE)
}

check.quiz = function(vars, empty="",
    failure.message= "You have not yet assigned the correct answer to {{var}}.",
    sucess.message = "Great, your answer for {{var}} is correct!",
    empty.message = "You have not yet assigned an answer to {{var}}."
  , part=NULL, ps=get.ps(),stud.env = ps$stud.env){

  restore.point("check.quiz")
  for (var in vars) {
    ret = check.variable(vars=var,check.all=TRUE,failure.exists = empty.message,failure.length=failure.message, failure.class=failure.message, failure.values = failure.message, success.message=sucess.message, ps=ps,stud.env = stud.env, verbose=FALSE, part=part )
    if (!ret)
      return(FALSE)
  }
  return(TRUE)
}

check.class = function(expr, classes,unsubst.expr=NULL, str.expr=NULL, ps=get.ps(),stud.env = ps$stud.env, part=NULL) {
  
  if (!is.null(unsubst.expr)) {
    expr = unsubst.expr
  } else if (!is.null(str.expr)) {
    expr = parse(text=str.expr, srcfile = NULL)
  } else {
    expr = substitute(expr)
  }

  class = class(eval(expr,envir=stud.env))
  if (any(class %in% classes))
    return(TRUE)
  
  if (is.null(str.expr))
    str.expr = deparse1(expr)
  
  if (length(classes)>1) {
    failure.message=paste0(str.expr, " has class ", paste0(class,collapse=" and ")," but it should be one of ", paste0(classes, collapse=", "),".")
  } else {
    failure.message=paste0(str.expr, " has wrong class. It should be ", paste0(classes, collapse=", "),".")    
  }
  add.failure(failure.message)
  return(FALSE)
  
}

#' Test: Compare the column col of the matrix or data.frame df with either the values from the given solutions or with the result of an expression that is evaluated in the students solution 
#' @param df name of the data frame or matrix
#' @param col name of the column
#' @param expr
#' @param exists shall existence be checked (similar length, class, values)
#' @param failure.exists a message that is shown if the variable does not exists (similar the other failure.??? variables)
#' @param failure.message.add a text that will be added to all failure messages

#' @export 
check.col = function(df,col, expr=NULL, class.df = c("data.frame","data.table","matrix"),check.all = FALSE,exists=check.all, length=check.all, class=check.all, values=check.all,tol = .Machine$double.eps ^ 0.5,
    failure.exists="{{df}} does not have a column {{col}}.",
    failure.length="{{df}} has {{length_stud}} rows but it shall have {{length_sol}} rows.",
    failure.class = "Column {{col}} of {{df}} has a wrong class. It should be {{class_sol}} but it is {{class_stud}}.",
    failure.values = "Column {{col}} of {{df}} has wrong values.",
    failure.message.add = NULL,
    success.message = "Great, column {{col}} of {{df}} has correct {{tests}}.",part=NULL,
    ps=get.ps(),stud.env = ps$stud.env, verbose=FALSE,unsubst.expr = NULL, str.expr = NULL) {
    
  
  if (!is.null(unsubst.expr)) {
    expr = unsubst.expr
  } else if (!is.null(str.expr)) {
    expr = parse(text=str.expr, srcfile = NULL)
  } else {
    expr = substitute(expr)
  }

  restore.point("check.col")

  
  ret = check.variable(df,check.all=FALSE,exists =TRUE)
  if (!ret) return(FALSE)
  
  dat =  get(df,stud.env)
  
  if (!is.null(failure.message.add)) {
    failure.exists = paste0(failure.exists,"\n", failure.message.add)
    failure.length = paste0(failure.length,"\n", failure.message.add)
    failure.class = paste0(failure.class,"\n", failure.message.add)
    failure.values = paste0(failure.values,"\n", failure.message.add)
  }
  
  var.sol = list(eval(expr,stud.env))
  
  if (exists != FALSE) {
    if (is.character(col)) {
      does.exist = col %in% colnames(dat)
    } else {
      does.exist = NCOL(dat)>=col
    }
    if (!does.exist) {
      add.failure(failure.exists,failure.exists, col = col,df=df)
      return(FALSE)
    }
  }
  
  var.stud = dat[,col]
  
  if (length != FALSE) {
    if (!length(var.stud)==length(var.sol)) {
      add.failure(failure.length, failure.length, col=col,df=df, length_stud = length(var.stud), length_sol=length(var.sol))
      return(FALSE)
    }
  }  
  if (class != FALSE) {
    class.stud = class(var.stud)[1]
    class.sol = class(var.sol)[1]
    if (class.stud == "integer") class.stud = "numeric"
    if (class.sol == "integer") class.sol = "numeric"
    
    if (class.stud!=class.sol) {
      add.failure(failure.class, failure.class, col=col,df=df, class_stud=class.stud, class_sol = class.sol)
      return(FALSE)
    }
  }  
  if (values != FALSE) {
    if (is.numeric(var.stud) & is.numeric(var.sol)) {
      if (max(abs(var.stud-var.sol), na.rm=TRUE)>tol ) {
        add.failure(failure.values, failure.values, col=col,df=df)
        return(FALSE)
      }
      if (!identical(is.na(var.stud),is.na(var.sol))) {
        add.failure(failure.values, failure.values, col=col,df=df)
        return(FALSE)        
      }
      
    } else {
      if (! all(var.stud==var.sol)) {
        add.failure(failure.values, failure.values, col=col,df=df)
        return(FALSE)
      }
    }
  }
  
  tests.str = flags.to.string(length=length,class=class,values=values)
  add.success(success.message, col=col, df=df, tests=tests.str)
  return(TRUE)
}


check.var.exists = function(var, ps=get.ps(),stud.env = ps$stud.env) {
  if (!exists(var,stud.env, inherits=FALSE)) {
      msg = paste0("You have not yet generated the variable '", var,"'.")
      add.failure(msg, var = var)
      return(FALSE)
  }
  return(TRUE)  
}

#' Test: Check whether a variable is equal to a specified expression 
#' @param var a the variable name as string
#' @param expr an expression that will be evaluated in the student environment and returns the variable
#' @param length shall length be checked (similar dim, class, values)
#' @param failure.length a message that is shown if the variable does not exists (similar the other failure.??? variables)
#' @param failure.message.add a text that will be added to all failure messages
#' @export 
check.variable = function(var, expr, length=check.all,dim=check.all, class=check.all, values=check.all, check.all=TRUE, ..., tol = .Machine$double.eps ^ 0.5,
  failure.exists="You have not yet generated the variable {{var}}.",
  failure.length="Your variable {{var}} has length {{length_stud}} but it shall have length {{length_sol}}.",
  failure.dim="Your variable {{var}} has the wrong dimensions (rows x columns).",
  failure.class = "Your variable {{var}} has a wrong class. It should be {{class_sol}} but it is {{class_stud}}.",
  failure.values = "Your variable {{var}} has wrong values.",
  success.message = "Great, {{var}} has correct {{tests}}.",
  ps=get.ps(),stud.env = ps$stud.env, verbose=FALSE, part=NULL) {
    
  expr = substitute(expr)
  restore.point("check.variable")
  
    
  if (!exists(var,stud.env, inherits=FALSE)) {
      short.message = paste0("{{var}} does not exist")
      add.failure(failure.exists, var = var)
      return(FALSE)
  }

  sol.env = new.env(parent = stud.env)
  
  var.sol = suppressWarnings(eval(expr,sol.env))  
  var.stud = get(var,stud.env)
  if (length != FALSE) {
    short.message = paste0("wrong length {{var}}: is {{length_stud}} must {{length_sol}}")
    if (!length(var.stud)==length(var.sol)) {
      add.failure( failure.length, var=var, length_stud = length(var.stud), length_sol=length(var.sol))
      return(FALSE)
    }
  }  
  if (class != FALSE) {
    short.message = "wrong class {{var}}: is {{class_stud}} must {{class_sol}}"
    class.stud = class(var.stud)[1]
    class.sol = class(var.sol)[1]
    if (class.stud == "integer") class.stud = "numeric"
    if (class.sol == "integer") class.sol = "numeric"
      
    if (class.stud!=class.sol) {
      add.failure( failure.class, var=var, class_stud=class.stud, class_sol = class.sol)
      return(FALSE)
    }
  }
  if (dim != FALSE) {
    if (!identical(dim(var.stud), dim(var.sol))) {
      add.failure(failure.dim, failure.dim, var=var)
      return(FALSE)
    }
  }  

  
  if (values != FALSE) {
    if (is.list(var.sol) | is.environment(var.sol)) {
      if (!identical(var.sol, var.stud, ignore.environment=TRUE)) {
        add.failure("{{var}} has wrong values", failure.values, var=var)
        return(FALSE)
      }
    } else if (is.numeric(var.stud) & is.numeric(var.sol)) {
      if (max(abs(var.stud-var.sol), na.rm=TRUE)>tol ) {
        add.failure("{{var}} has wrong values", failure.values, var=var)
        return(FALSE)
      }
      if (!identical(is.na(var.stud),is.na(var.sol))) {
        add.failure("{{var}} has wrong values", failure.values, var=var)
        return(FALSE)        
      }
      
    } else {
      if (! all(var.stud==var.sol)) {
        add.failure("wrong values of {{var}}", failure.values, var=var)
        return(FALSE)
      }
    }
  }
  
  tests.str = flags.to.string(length=length,dim=dim,class=class,values=values)
  add.success(success.message, var=var, tests=tests.str)
  return(TRUE)
}

flags.to.string = function(..., sep=", ", last.sep = " and ") {
  args = list(...)
  
  args = args[unlist(args)]
  if (length(args)==1)
    return(names(args))
  if (length(args)==2)
    return(paste0(names(args),collapse=last.sep))
  
  return(paste0(paste0(names(args[-length(args)]),collapse=last.sep),
         last.sep,names(args[length(args)])))                  
  
}


#' A helper function for hypothesis test about whether student solution is correct
#' @export 
hypothesis.test.result = function(p.value, alpha.warning=0.05, alpha.failure = 0.0001, verbose=FALSE) {
  if (p.value < alpha.failure) {
    if (verbose)
      message("  H0 is highly significantly rejected... check fails!")
    return(FALSE)
  } else if (p.value < alpha.warning) {
    if (verbose)
      message(paste0("  H0 signicantly rejected... warning!"))
    return("warning")
  } else {
    if (verbose)
      cat(paste0("... ok!"))
    return(TRUE)    
  }
  
} 

#' Test for variance (copied from TeachingDemos)
#' @export
sigma.test = function (x, sigma = 1, sigmasq = sigma^2,
          alternative = c("two.sided", "less", "greater"),
          conf.level = 0.95,  ...) {
  alternative <- match.arg(alternative)
  sigma <- sqrt(sigmasq)
  n <- length(x)
  xs <- var(x)*(n-1)/sigma^2
  out <- list(statistic = c("X-squared" = xs))
  class(out) <- "htest"
  out$parameter <- c(df = n-1)
  minxs <- min(c(xs, 1/xs))
  maxxs <- max(c(xs, 1/xs))
  PVAL <- pchisq(xs, df = n - 1)
  
  out$p.value <- switch(alternative,
                        two.sided = 2*min(PVAL, 1 - PVAL),
                        less = PVAL,
                        greater = 1 - PVAL)
  out$conf.int <- switch(alternative,
                         two.sided = xs * sigma^2 *
                           1/c(qchisq(1-(1-conf.level)/2, df = n-1), qchisq((1-conf.level)/2, df
                                                                            = n-1)),
                         less = c(0, xs * sigma^2 /
                                    qchisq(1-conf.level, df = n-1)),
                         greater = c(xs * sigma^2 /
                                       qchisq(conf.level, df = n-1), Inf))
  attr(out$conf.int, "conf.level") <- conf.level
  out$estimate <- c("var of x" = var(x))
  out$null.value <- c(variance = sigma^2)
  out$alternative <- alternative
  out$method <- "One sample Chi-squared test for variance"
  out$data.name <- deparse1(substitute(x))
  names(out$estimate) <- paste("var of", out$data.name)
  return(out)
}


#' Test whether a certain H0 can be significantly rejected
#' 
#' @param test.expr an expression that calls a test which will be evaluated in stud.env. The test must return a list that contains a field "p.value"
#' @param p.value Instead of providing test.expr, one can directly provide a p.value from a previously run test
#' @param test.name an optional test.name that can be used to fill the {{test_name}} whiskers in warning or failure messages.
#' @param alpha.failure default=0.001 the critical p.value below which the stud code is considered wrong
#' @param alpha.warning default=0.05 a p.value below a warning is printed that the code may be wrong
#' @param short.message,failure.messages, warning.messages Messages in case of a failure and warning and  short message for the log.file
#' @param check.warning if FALSE don't check for a warning 
#' @return TRUE if H0 can be rejected, FALSE if not and "warning" if it can be weakly rejected
#' @export 
test.H0.rejected = function(test.expr,p.value,test.name="",
  alpha.warning = 0.01,alpha.failure =0.05,
  short.message="Fail to reject '{{test_name}}', p.value = {{p_value}}",
  warning.message="The null hypothesis from the test '{{test_name}}', should not be rejcected, but I get a fairly low p.value of {{p_value}}.",
  failure.message="I couldn't significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}",
  success.message = "Great, I could significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}!",
  check.warning=TRUE, ps=get.ps(),stud.env = ps$stud.env, part=NULL,...)
{


  if (!missing(test.expr)) {
    test.expr = substitute(test.expr)
    if (test.name=="") {
      test.name = deparse1(test.expr)
    }
  }
  if (missing(p.value)) {
    test.res = eval(test.expr, stud.env)
    p.value = test.res$p.value
  }
  if (p.value > alpha.failure) {
    add.failure(failure.message,test_name=test.name,p_value=p.value)
    return(FALSE)
  }
  
  add.success(success.message,test_name=test.name,p_value=p.value,...)
  
  if (p.value > alpha.warning & check.warning) {
    add.warning(warning.message,test_name=test.name,p_value=p.value)
    return("warning")
  }
  
  return(TRUE)
}

#' Check whether a certain null hypothesis is not significantly rejected
#' @param test.expr an expression that calls a test which will be evaluated in stud.env. The test must return a list that contains a field "p.value"
#' @param p.value Instead of providing test.expr, one can directly provide a p.value from a previously run test
#' @param test.name an optional test.name that can be used to fill the {{test_name}} whiskers in warning or failure messages.
#' @param alpha.failure default=0.001 the critical p.value below which the stud code is considered wrong
#' @param alpha.warning default=0.05 a p.value below a warning is printed that the code may be wrong
#' @param short.message,failure.messages, warning.messages Messages in case of a failure and warning and  short message for the log.file
#' @param check.warning if FALSE don't check for a warning 
#' @return TRUE if H0 cannot be rejected, FALSE if not and "warning" if it can be weakly rejected
#' @export 
test.H0 = function(test.expr,p.value,test.name="",
                   alpha.warning = 0.05,alpha.failure =0.001,
                  short.message,warning.message,failure.message,
                   success.message = "Great, I could not significantly reject the null hypothesis from the test '{{test_name}}', p.value = {{p_value}}!",
                   
                  check.warning=TRUE, part=NULL,
                  ps=get.ps(),stud.env = ps$stud.env,...) {


  #browser()
  if (!missing(test.expr)) {
    test.expr = substitute(test.expr)
    if (test.name=="") {
      test.name = deparse1(test.expr)
    }
  }
  if (missing(p.value)) {
    test.res = eval(test.expr, stud.env)
    p.value = test.res$p.value
  }
  if (missing(short.message)) {
    short.message = paste0("rejected '{{test_name}}' has p.value = {{p_value}}")
  }
  if (missing(failure.message)) {
    failure.message = paste0("The null hypothesis from the test '{{test_name}}' shall hold, but it is rejected at p.value = {{p_value}}")
  }
  if (missing(warning.message)) {
    warning.message = paste0("The null hypothesis from the test '{{test_name}}', should not be rejcected, but I get a fairly low p.value of {{p_value}}.")
  }
  
  if (p.value < alpha.failure) {
    add.failure(failure.message,test_name=test.name,p_value=p.value,...)
    return(FALSE)
  }

  add.success(success.message,test_name=test.name,p_value=p.value,...)
  
  if (p.value < alpha.warning & check.warning) {
    add.warning(warning.message,test_name=test.name,p_value=p.value,...)
    return("warning")
  }
  return(TRUE)
}

#' Test: The variance of the distribution from which a vector of random numbers has been drawn
#' @export
test.variance = function(vec, true.val, test = "t.test",short.message,warning.message,failure.message, success.message = "Great, I cannot statistically reject that {{var}} has the desired variance {{vari_sol}}!", ps=get.ps(),stud.env = ps$stud.env,part=NULL,...) {
  call.str = as.character(match.call())

  var.name = call.str[2]
  p.value = sigma.test(vec,sigmasq=true.val)$p.value

  if (missing(short.message)) {
    short.message ="wrong variance {{var}}: is {{vari_stud}} shall {{vari_sol}}, p.value = {{p_value}}"
  }
  if (missing(failure.message)) {
    failure.message = "{{var}} has wrong variance! \n Your random variable {{var}} has a sample variance of {{vari_stud}} but shall have {{vari_sol}}. A chi-square test tells me that if that null hypothesis were true, it would be very unlikely (p.value={{p_value}}) to get a sample as extreme as yours!"
  }
  if (missing(warning.message)) {
    warning.message = "{{var}} has a suspicious variance! \n Your random variable {{var}} has a sample variance of {{vari_stud}} but shall have {{vari_sol}}. A chi-square test tells me that if that null hypthesis were true, the probability would be just around {{p_value}} to get such an extreme sample variance"
  }
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message, success.message=success.message,
  var = var.name, vari_stud = var(vec), vari_sol=true.val,...)
  
  
} 


#' Test: The mean of the distribution from which a vector of random numbers has been drawn
#' @export
test.mean = function(vec, true.val, test = "t.test", short.message,warning.message,failure.message, success.message = "Great, I cannot statistically reject that {{var}} has the desired mean of {{mean_sol}}!", ps=get.ps(),stud.env = ps$stud.env,part=NULL,...) {
  call.str = as.character(match.call())

  stopifnot(test=="t.test")
  
  var.name = call.str[2]
  p.value = t.test(vec,mu=true.val)$p.value

  if (missing(short.message)) {
    short.message ="wrong mean {{var}}: is {{mean_stud}} shall {{mean_sol}}, p.value = {{p_value}}"
  }
  if (missing(failure.message)) {
    failure.message = "{{var}} has wrong mean! \n Your random variable {{var}} has a sample mean of {{mean_stud}} but shall have {{mean_sol}}. A t-test tells me that if that null hypothesis were true, it would be very unlikely (p.value={{p_value}}) to get a sample mean as extreme as yours!"
  }
  if (missing(warning.message)) {
    warning.message = "{{var}} has a suspicious mean!\n Your random variable {{var}} has a sample mean of {{mean_stud}} but shall have {{mean_sol}}. A t-test tells me that if that null hypthesis were true, the probability would be just around {{p_value}} to get such an extreme sample mean_"
  }
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message,success.message=success.message,
        var = var.name, mean_stud = mean(vec), mean_sol=true.val,...)
}

#' Test: Has a vector of random numbers been drawn from a normal distribution?
#' @export
test.normality = function(vec,short.message,warning.message,failure.message,ps=get.ps(),stud.env = ps$stud.env, success.message = "Great, I cannot statistically reject that {{var}} is indeed normally distributed!",part=NULL,...) {
  call.str = as.character(match.call())
  restore.point("test.normality")

  var.name=call.str[2]
  
  # Cannot use more than 5000 observations
  if (length(vec)>5000)
    vec = vec[sample.int(n=length(vec), size=5000)]
  p.value = shapiro.test(vec)$p.value
  
  if (missing(short.message)) {
    short.message ="{{var}} not normally distributed, p.value = {{p_value}}"
  }
  if (missing(failure.message)) {
    failure.message = "{{var}} looks really not normally distributed.\n A Shapiro-Wilk test rejects normality at an extreme significance level of {{p_value}}."
  }
  if (missing(warning.message)) {
    warning.message = "{{var}} looks not very normally distributed.\n A Shapiro-Wilk test rejects normality at a significance level of {{p_value}}."
  }
  test.H0(p.value=p.value,short.message=short.message, warning.message=warning.message, failure.message=failure.message,success.message=success.message,
        var = var.name,...)
}

#' Test: Does a certain condition on the stud's generated variables hold true
#' @export
holds.true = function(cond, short.message = failure.message,failure.message="Failure in holds.true",success.message="Great, the condition {{cond}} holds true in your solution!",part=NULL,ps=get.ps(),stud.env = ps$stud.env, cond.str=NULL,...) {

  if (is.null(cond.str)) {
    cond = substitute(cond)
    cond.str = deparse1(cond)
  } else {
    cond = parse(text=cond.str,srcfile=NULL)
  }
  restore.point("holds.true")
  
  if (!all(eval(cond,stud.env))) {
    add.failure(failure.message,cond=cond.str,...)
    return(FALSE)
  }
  add.success(success.message,cond=cond.str,...)
  #cat(paste0("\n",message, "... ok!"))
  return(TRUE)
}
