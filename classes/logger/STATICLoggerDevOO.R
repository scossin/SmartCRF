STATICloggerDev <- R6::R6Class(
  "STATICloggerDev",
  
  public = list(
    initialize = function(){
      cat("creating a new Dev StaticLogger")
    },
    
    info = function(...){
      msg <- NULL
      msg <- append(msg, "INFO - ")
      args <- list(...)
      private$writeMsg(msg,args)
    },
    
    user = function(...){
      msg <- NULL
      msg <- append(msg, "USERINPUT - ")
      args <- list(...)
      private$writeMsg(msg,args)
    },
    
    error = function(...){
      msg <- NULL
      msg <- append(msg, "ERROR - ")
      
      args <- list(...)
      private$writeMsg(msg,args)
    }
    
  ),
  
  private = list(
    writeMsg = function(msg, args){
      msg <- append(msg, " : ")
      for(arg in args){
        if (is.null(arg)){
          arg <- "NULL"
        }
        if (is.factor(arg) || is.numeric(arg)){
          arg <- as.character(arg)
        }
        if (is.character(arg)){
          if (length(arg) == 1){
            msg <- append(msg, arg)
            msg <- append(msg," ")
          } else {
            arg <- paste(arg, collapse="-")
            arg <- paste0("(",arg,")")
            msg <- append(msg, arg)
            msg <- append(msg," ")
          }
        } else { ## if not character nor vector
          msg <- append(msg,"LoggerWarning : arguments not character nor vector")
        }
      }
      
      cat(msg,"\n")
      msg <- paste0(msg,collapse = "")
      ## not writing anything in dev mode
      # write(msg,self$conFileLog)
    }
)
)