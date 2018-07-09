Selectize <- R6::R6Class(
  "Selectize",
  inherit=uiObject,
  
  public = list(
    choices = character(),
    
    initialize = function(parentId, where){
      super$initialize(parentId = parentId, where = where)
      self$addObserverQueryAvailableDocuments()
      self$addObserverSetAvailableDocuments()
      self$addObserverOrder()
    },
    
    setChoices = function(choices){
      self$choices <- choices
    },
    
    renderSelectizeUI = function(){
      jquerySelector <- private$getJquerySelector(self$parentId)
      insertUI(selector = jquerySelector, 
               where = self$where,
               ui = self$getUI(),
               immediate = T)
    },
    
    getUI = function(){
     ui <-  div (id = "tempId",
               shiny::selectizeInput(inputId = self$getSelectizeId(),
                           label = "",
                choices = NULL,
                selected = NULL,
                multiple=T,
                options = list(plugins=list('drag_drop','remove_button'))),
               
               shiny::actionButton(inputId = self$getButtonLoadDocumentsId(),
                                   label = "Initializer")
     )
      ## remove_button
      return(ui)
    },
    
    
    getDivId = function(){
      return(paste0("DivSelectizeDocument-", self$parentId))
    },
    
    getButtonLoadDocumentsId = function(){
      return(paste0("ButtonLoadDocuments-", self$getDivId()))
    },
    
    getSelectizeId = function(){
      return(paste0("Selectize-",self$getDivId()))
    },
    
    ## get all div in GLOBALaddTimelineID
    addObserverQueryAvailableDocuments = function(){
      observeEvent(input[[self$getButtonLoadDocumentsId()]],{
        staticLogger$info("B")
        session$sendCustomMessage("getAvailableDocuments", 
                                  message = list (divId = GLOBALaddTimelineID))
        return(NULL)
      },once = T)
    },
    
    addObserverSetAvailableDocuments = function(){
      observeEvent(input[[GLOBALjsInputMessageAvailableDocument]],{
        staticLogger$info("availableDocuments received !")
        availableDocuments <- input[[GLOBALjsInputMessageAvailableDocument]]
        staticLogger$info("\t documents : ", availableDocuments)
        shiny::updateSelectInput(session = session,
                                 inputId =  self$getSelectizeId(),
                                 choices = availableDocuments)
        staticLogger$info("hidding all documents :")
        for (document in availableDocuments) {
          objectId <- paste0(document, "-",GLOBALaddTimelineID)
          session$sendCustomMessage("displayHideId", 
                                    message = list(objectId = objectId))
        }
      },once = T)
    },
    
    addObserverOrder = function(){
      observeEvent(input[[self$getSelectizeId()]],{
        staticLogger$info("Change in selectize Div Id")
        choices <- input[[self$getSelectizeId()]]
        
        ## no add or remove : user wants to reorder the div :
        if (length(choices) == length(self$choices)){
          staticLogger$info("\t choices didn't change, I guess the user wants to reorder the div")
          session$sendCustomMessage("orderDiv", choices)
          return(NULL)
        }
        
        ## users wants to add
        if (length(choices) > length(self$choices)){
          staticLogger$info("\t user added a new choice to div")
          newDiv <- setdiff(choices, self$choices)
          staticLogger$info("\t \t div : ", newDiv)
          objectId <- paste0(newDiv, "-",GLOBALaddTimelineID)
          session$sendCustomMessage("displayShowId", 
                                    message = list(objectId = objectId))
        } else {
          staticLogger$info("\t user removed a div")
          removedDiv <- setdiff(self$choices, choices)
          staticLogger$info("\t \t div : ", removedDiv)
          objectId <- paste0(removedDiv, "-",GLOBALaddTimelineID)
          session$sendCustomMessage("displayHideId", 
                                    message = list(objectId = objectId))
        }
        ## updating the choices :
        self$choices <- choices 
        
      },ignoreNULL = F)
    }
  ),
  
  private = list(
    
  ))

