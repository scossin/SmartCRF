TextDisplay <- R6::R6Class(
  "TextDisplay",
  inherit=uiObject,
  
  public=list(
    text = character(),
    START_DATE = NULL,
    NAME_CHAR = character(),
    formDate = character(),
    groupName = character(),

    initialize = function(parentId, where, groupName){
      staticLogger$info("Initializing new text object")
      super$initialize(parentId = parentId, where = where)
      self$groupName <- groupName
      private$setRandomNumber()
    },
    
    setText = function(text, START_DATE, NAME_CHAR){
      self$START_DATE <- START_DATE
      self$NAME_CHAR <- NAME_CHAR
      self$text <- text
    },
    
    getUI = function(){
      # label <- paste0(self$START_DATE, " - ", self$NAME_CHAR)
      txt <- gsub("\n","<br>",self$text)
      ui <- div(id = self$getDivId(),
                h2(paste0(self$groupName)),
                h3(paste0(self$START_DATE, " - ", self$NAME_CHAR)),
                div( style = "font-size:1.5em",
                     HTML(paste0("<p>",txt,"</p>")))
      )
      return(ui)
    },
    
    renderUI = function(){
      staticLogger$info("Rendering UI TextArea ")
      shiny::updateTextAreaInput(session = session,
                                 inputId = self$getTextId(),
                                 value = self$text)
    },
    
    destroy = function(){
      staticLogger$info("Destroy function NOT IMPLEMENTED")
    },
    
    
    getDivId = function(){
      return(paste0("DivText-",private$randomNumber,self$parentId))
    },
    
    getTextId = function(){
      return(paste0("Text",self$getDivId()))
    }
  ),
  
  private = list(
    randomNumber = numeric(),
    
    setRandomNumber = function(){
      private$randomNumber =  abs(round(runif(1)*10000000,0))
    }
  )
)
