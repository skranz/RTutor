rtutorAlert = function(session=getApp()$session, id,title=NULL, content=message,style=type, message=content, append=FALSE,type=style,...) {
    res = try(createAlert(session,id, 
        title = title, 
        message= content,
        type = "info", append=FALSE,...
    ), silent=TRUE)
    if (is(res,"try-error")) {
      res = try(createAlert(session,id, 
          title = title, 
          content= content,
          style = "info", append=FALSE,...
      ), silent=TRUE)
    }
}