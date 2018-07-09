ControllerTimelineForm <- R6::R6Class(
  "ControllerTimelineForm",
  
  public = list(
    ## 
    timeline = NULL,
    form = NULL,
    formDisplay = NULL,
    tableSearchObserver = NULL,
    observerEventTable = NULL,
    timelineChangedObserver = NULL,
    searchValue = NULL,
    formAddTimeline = NULL,
    dygraphCurveList = list(),
    
    initialize = function(timeline, form, formDisplay,formAddTimeline){
      ### Timeline
      bool <- inherits(timeline, "Timeline")
      if (!bool) {
        stop("timeline object must be an instance of a Timeline")
      }
      self$timeline <- timeline
      
      ### Form
      bool <- inherits(form, "Form")
      if (!bool) {
        stop("form object must be an instance of a Form")
      }
      self$form <- form
      
      ### formDisplay
      bool <- inherits(formDisplay, "FormDisplay")
      if (!bool) {
        stop("formDisplay object must be an instance of a FormDisplay")
      }
      self$formDisplay <- formDisplay
      
      
      ### formAddTimeline
      bool <- inherits(formAddTimeline, "FormAddTimeline")
      if (!bool) {
        stop("formAddTimeline object must be an instance of a FormAddTimeline")
      }
      self$formAddTimeline <-  formAddTimeline
      
      ### observers :
      self$addTableSearchObserver()
      self$addObserverEventTable()
      self$addTimelineChangedObserver()
    },
    
    
    initializeObjects = function(){
      ### timeline
      self$setInitialTimevisDf()
      
      self$timeline$insertUItimeline()
      self$timeline$plotTimeline()
      
      ### formDisplay : 
      currentForm <- self$form$currentForm
      self$formDisplay$setForm(currentForm)
      self$formDisplay$insertUIForm()
      self$formDisplay$renderUI()
      
      ## addTimeline : 
      sectionQuestion <- subset(self$form$allForms, select=c("section","NAME_CHAR"))
      colnames(sectionQuestion) <- c("Section","Question")
      sectionQuestion <- unique(sectionQuestion)
      formAddTimeline$setForm(form = sectionQuestion)
      formAddTimeline$insertUIForm()
      formAddTimeline$renderUI()
      
      ## dygraphs (hard coded) :
      parentId="dygraphCurvesId"
      dygraphcDAI <- DygraphCurve$new(parentId = parentId,
                                      where = "beforeEnd")
      numericalVariable <- "c DAI"
      dfCategoriesNumericalValue <- data.frame(categories = c("normal","low",
                                                              "moderate","high"),
                                               values = c(0,3.3,11,26))
      liste <- self$form$getMyTS(numericalVariable = numericalVariable,
                                 dfCategoriesNumericalValue = dfCategoriesNumericalValue)
      print(ls(liste))
      
      myTS <- liste$myTS
      print(class(myTS))
      colors <- liste$colors
      dygraphcDAI$setMyTS(myTS)
      dygraphcDAI$setGroup("groupe")
      dygraphcDAI$setColors(colors)
      horizontalValues <- dfCategoriesNumericalValue$values
      dygraphcDAI$setHorizontalValues(horizontalValues)
      dygraphcDAI$insertDygraphUI()
      dygraphcDAI$plotDygraph()
      self$dygraphCurveList[[1]] <- dygraphcDAI
    },
    
    setInitialTimevisDf = function(){
      dates <- self$form$getAllDates()
      id <- 1:length(dates)
      timevisDf <- data.frame(id = id,
                              group = "Formulaire",
                              start = dates,
                              end = NA,
                              content = "",
                              color = NA,
                              style = NA,
                              title = NA)
      self$timeline$setTimevisDf(timevisDf)
      #self$timeline$setGroupDf(newGroup)
      return(NULL)
    },
    
    
    ### when searching in the DT : record the search
    addTableSearchObserver = function(){
      inputName <- paste0(self$formDisplay$getFormId(),"_search")
      self$tableSearchObserver <- observeEvent(input[[inputName]],{
        self$searchValue <- input[[inputName]]
        #staticLogger$info("searchValue :",self$searchValue)
      },ignoreNULL = F)
    },
    
    ### when clicking in the DT to add column to the timeline :
    addObserverEventTable = function(){
      clickedInput <- paste0(self$formAddTimeline$getFormId(),"_rows_selected")
      self$observerEventTable <- observeEvent(input[[clickedInput]],{
        staticLogger$info("User wants to add a new event to the timeline")
        rowClicked <- input[[clickedInput]]
        
        print(rowClicked)
        
        if (is.null(rowClicked)) {
          staticLogger$info("No rows selected to formAddTimeline")
          self$timeline$addTimevisDfdata(NULL)
          return(NULL)
        }
        
        self$formAddTimeline$setRowsSelected(rowClicked)
        sectionQuestion <- self$formAddTimeline$getSectionQuestionSelected()
        
        timevisDfdata <- self$form$getAllTimeVisSeries(sectionQuestion)
        
        self$timeline$addTimevisDfdata(timevisDfdata)
        self$timeline$plotTimeline()
        
        staticLogger$info("Done !")
      },ignoreNULL = F)
    },
    
    addTimelineChangedObserver = function(){
      #
      dateWindowInput <- paste0(self$timeline$getTimelinePlotId(),"_window")
      self$timelineChangedObserver <- observeEvent(input[[dateWindowInput]],{
        print("date_window changed ! ")
        values <- input[[dateWindowInput]]
        print(values)
        minValue <- values[1]
        maxValue <- values[2]
        dygraphCurve <- self$dygraphCurveList[[1]]
        dygraphCurve$setMinMaxDateValues(minValue, maxValue)
        dygraphCurve$rePlotDygraph()
        
        ####
        
      })
    }
    # 
  ),
  
  private = list(
    
  )
  
)


