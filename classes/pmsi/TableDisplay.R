TableDisplay <- R6::R6Class(
  "TableDisplay",
  
  public = list(
    groupName = character(),
    tableSearchObserver = NULL, ## the observer to record the searchValue
    
    formDisplay = NULL, ## display the raw data PMSI form when timeline is clicked
    IsInitialized = F, ## when form is initialized, put back previous search value
    
    searchValue = NULL, ## save searchValue in the form
    
    # formAddTimeline = NULL, ## display pmsiSelect
    # observerEventTable = NULL,
    
    
    initialize = function(){
      stop("abstract class")
    },

    addTimelineObserver = function(){
      observeEvent(input[[self$groupName]],{
        staticLogger$info("A PMSI event was clicked on the timeline")
        id <- staticTimeline$lastIdClicked
        staticLogger$info("\t id event : ", id)
        start_date <- unlist(strsplit(id,split="\t"))[2]
        # start_date <- gsub(paste0(self$groupName,"-"), "",id) ## PMSI-date
        staticLogger$info("\t extracted start_date from id : ",start_date)
        
        form = self$getForm2display(START_DATE = start_date) ### a PMSI form to display
        
        ## create a new FormDisplay :
        parentId <- staticTimeline$uiDisplay$getUIparentId() ## get the UIdisplay of the timeline
        formDisplay <- FormDisplay$new(parentId = parentId,
                                       where = "beforeEnd", ## doesn't matter this one
                                       groupName = self$groupName)
        formDisplay$setForm(form)
        ui <-  formDisplay$getUI() ## 
        UIid <- formDisplay$getDivId()
        
        ## display the UI below Timeline 
        staticTimeline$uiDisplay$setUI(ui = ui,
                                       UIid = UIid)
        formDisplay$renderUI() ## display the graphic
        
        self$formDisplay <- formDisplay ##
        
        self$IsInitialized <- T
        self$addTableSearchObserver(formId = formDisplay$getFormId())
      })
    },
    
    ### when searching in the DT : record the search
    addTableSearchObserver = function(formId){
      if (!is.null( self$tableSearchObserver)){ ## destroy the previous observer
        staticLogger$info(" \t destroying previous observer tableSearchObserver")
        self$tableSearchObserver$destroy()
      }
      inputName <- paste0(formId,"_search")
      self$tableSearchObserver <- observeEvent(input[[inputName]],{
        staticLogger$info("tableSearchObserver called")
        ## Hard to keep the previous value ! renderUI is the last thing Shiny do, so we need to catch initial value and check previsous value :
        # if previous value is not null and nchar > 1 ; then we replace the previous value
        if ((is.null(input[[inputName]]) || input[[inputName]] == "" )  && !is.null(self$searchValue) && nchar(self$searchValue) > 0){ 
          
          if (self$IsInitialized){ 
            staticLogger$info("\t call to reset searchValue :",self$searchValue)
            self$formDisplay$showPreviousSelectedRows(self$searchValue)
            self$IsInitialized <- F
          } else { ## case all searchValue is erased
            self$searchValue <- ""
          }
          return(NULL)
        }
        print(input[[inputName]])
        self$searchValue <- input[[inputName]]
        staticLogger$info("\t searchValue :",self$searchValue)
      },ignoreNULL = T)
    }),
    
  private = list(
  )
)