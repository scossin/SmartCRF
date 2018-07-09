UIdisplay <- R6::R6Class(
  "UIdisplay",
  inherit=uiObject,
  
  public = list(
    
    ui = NULL,
    currentUIid = character(),
    
    initialize = function(parentId, where){
      super$initialize(parentId, where)
      private$setRandomNumber()
      self$addObserverRemoveUI()
      self$addObserverAnchor()
    },
    
    setUI = function(ui, UIid, boolAnchor = T){
      self$ui <- ui
      ## with anchor button
      if (boolAnchor){
        ui <- div(id = self$getDivId(),
                  ui,
                  shiny::actionButton(inputId = self$getAnchorButtonId(),
                                      label = "",
                                      icon = icon("anchor")),
                  shiny::actionButton(inputId = self$getRemoveButtonId(),
                                      label = "",
                                      icon = icon("trash"))
        )
      } else {
        ui <- div(id = self$getDivId(),
                  ui,
                  shiny::actionButton(inputId = self$getRemoveButtonId(),
                                      label = "",
                                      icon = icon("trash"))
        )
      }

      self$removePreviousUI()
      jquerySelector <- private$getJquerySelector(self$parentId)
      insertUI(selector = jquerySelector, 
               where = "afterBegin", ## inside the selector element, before its firt child
               ui = ui,
               immediate = T)
      self$currentUIid <- UIid
      return(NULL)
    },
    
    removePreviousUI = function(){
      jquerySelector <- private$getJquerySelector(self$getDivId())
      staticLogger$info("\t Removing previous UI below timeline")
      removeUI(selector = jquerySelector,
               immediate = T)
    },
    
    
    getDivId = function(){
      return(paste0("divUIdisplay-", private$randomNumber, self$parentId))
    },
    
    addObserverRemoveUI = function(){
      observeEvent(input[[self$getRemoveButtonId()]],{
        staticLogger$info("Button clicked to remove UI below timeline")
        self$removePreviousUI()
      })
    },
    
    addObserverAnchor = function(){
      observeEvent(input[[self$getAnchorButtonId()]],{
        staticLogger$info("Button clicked to anchor UI below timeline")
        self$removePreviousUI()
        self$anchor()
      })
    },
    
    anchor = function(){
      uiDisplay <- UIdisplay$new(parentId = self$parentId, where="beforeEnd")
      uiDisplay$setUI(ui = self$ui, UIid = self$currentUIid,boolAnchor = F)
    },
    
    getRemoveButtonId = function(){
      return(paste0("ButtonRemove-", self$getDivId()))
    },
    
    getAnchorButtonId = function(){
      return(paste0("ButtonAnchor-", self$getDivId()))
    },
    
    getUIparentId = function(){
      return(self$parentId)
    }
  ),
  
  private = list(
    randomNumber = numeric(),
    
    setRandomNumber = function(){
      private$randomNumber =  abs(round(runif(1)*10000000,0))
    }
    
  )
  
)