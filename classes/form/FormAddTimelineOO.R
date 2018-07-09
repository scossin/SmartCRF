FormAddTimeline <- R6::R6Class(
  "FormAddTimeline",
  inherit=uiObject,
  
  public = list(
    form = data.frame(),
    rowsSelected = numeric(),
    columnToAdd = character(),
    groupName = character(),
    
    initialize = function(parentId, where, groupName){
      staticLogger$info("Initializing new FormAddTimeline object")
      self$groupName <- groupName
      super$initialize(parentId = parentId, where = where)
    },
    
    setForm = function(form){
      self$form <- form
    },
    
    setRowsSelected = function(rowsSelected){
      self$rowsSelected = rowsSelected
    },
    
    ## 
    insertUIformTimeline = function(){
      jQuerySelector = paste0("#",self$parentId)
      insertUI(selector = jQuerySelector,
               where = self$where,
               ui = self$getUI())
    },
    
    getUI = function(){
      ui <- div(id = self$getDivId(),
                h2(paste0(groupName, " - ajouter à la timeline : ")),
                DT::dataTableOutput(self$getFormId()))
      return(ui)
    },
    
    destroy = function(){
      staticLogger$info("Destroying form NOT IMPLEMENTED")
    },
    
    
    getDivId = function(){
      stop("not implemented")
    },
    
    getFormId = function(){
      return(paste0("Form",self$getDivId()))
    }
  ),
  
  
  private = list(
  )
)
