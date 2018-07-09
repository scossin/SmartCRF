SelectizeDrugs <- R6::R6Class(
  "SelectizeDrugs",
  inherit=uiObject,
  
  public = list(
    choices = character(),
    
    initialize = function(parentId, where){
      staticLogger$info("New instance of SelectizeDrugs")
      super$initialize(parentId = parentId, where = where)
    },
    
    setChoices = function(choices){
      self$choices <- choices
      shiny::updateSelectizeInput(session = session, 
                           inputId = self$getSelectizeId(), 
                           choices = choices,
                           server = TRUE)
    },
    
    setSelected = function(selected){
      bool <- selected %in% self$choices
      if (!bool){
        staticLogger$info("Can't find selected : ", seletected, " in available choices")
        return(NULL)
      }
      shiny::updateSelectizeInput(session = session, 
                                  inputId = self$getSelectizeId(), 
                                  selected = selected,
                                  choices = self$choices,
                                  server = TRUE)
    },
    
    renderSelectizeUI = function(){
      jquerySelector <- private$getJquerySelector(self$parentId)
      insertUI(selector = jquerySelector, 
               where = self$where,
               ui = self$getUI(),
               immediate = T)
    },
    
    
    ## see : https://github.com/selectize/selectize.js/blob/master/docs/usage.md
    
    getUI = function(){
      ui <- div(id = self$getDivId(),  
                style="font-size:1.5em;margin-left:30%;margin-right:30%;width:40%",
                     shiny::selectizeInput(inputId = self$getSelectizeId(),
                           label = "",
                choices = "",
                selected = NULL,
                # selectize = F,
                multiple = T,
                width = "100%",
                size=0,
                options = list(hideSelected = T,
                               openOnFocus = F,
                               closeAfterSelect = T,
                               maxOptions = 5,
                               render = I(
                                                                             '{
                                   option: function(item, escape) {
                                     return "<div class=\'searchItem\'><strong>" + escape(item.value) + "</strong></div>";
                                   }
                                 }'),
                               plugins=list('remove_button'))))
  #                                            ## I

      return(ui)
    },
    
    
    getDivId = function(){
      return(paste0("DivSelectize",self$parentId))
    },
    
    getSelectizeId = function(){
      return(paste0("Selectize-",self$getDivId()))
    }
  ),
  
  private = list(
    
  ))

