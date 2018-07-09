FormDisplay <- R6::R6Class(
  "FormDisplay",
  inherit=uiObject,
  
  public=list(
    form = data.frame(),
    formDate = character(),
    groupName = character(),
    
    pageLength = 10, ## number of page to display in DT table
    caption = "", ## caption to display in DT table
    
    initialize = function(parentId, where, groupName){
      staticLogger$info("Initializing new form object")
      super$initialize(parentId = parentId, where = where)
      self$groupName <- groupName
      private$setRandomNumber()
    },
    
    setForm = function(form){
      self$form <- form
    },
    
    getUI = function(){
      ui <- div(id = self$getDivId(),
                h2(paste0(self$groupName)),
                DT::dataTableOutput(self$getFormId()))
      return(ui)
    },
    
    showPreviousSelectedRows = function(searchValue){
      staticLogger$info("showing Previous SearchValue : ",searchValue)
      DT::dataTableProxy(outputId = self$getFormId()) %>% 
        DT::updateSearch(keywords = list(global=searchValue))
    },
    
    setPageLength = function(pageLength){
      self$pageLength <- pageLength
    },
    
    setCaption = function(caption){
      self$caption <- caption
    },
    
    renderUI = function(){
      output[[self$getFormId()]] <- DT::renderDataTable({
        staticLogger$info("Rendering UI ")
        DT::datatable(self$form, rownames= FALSE,
                      caption=self$caption,
                      options=list(
          pageLength = self$pageLength,
          lengthMenu = c(5,10,15,20,30,50,100)
          #,rowCallback=JS(jscode)
        ))
        
      })
    },
    
    
    destroy = function(){
      staticLogger$info("Destroying form NOT IMPLEMENTED")
    },
    
    
    getDivId = function(){
      return(paste0("DivForm-",private$randomNumber,self$parentId))
    },
    
    getFormId = function(){
      return(paste0("Form",self$getDivId()))
    }
  ),
  
  private = list(
    
    randomNumber = numeric(),
    
    setRandomNumber = function(){
      private$randomNumber =  abs(round(runif(1)*10000000,0))
    }
  )
)
