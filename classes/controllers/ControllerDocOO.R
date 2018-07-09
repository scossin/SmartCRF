ControllerDoc <- R6::R6Class(
  "ControllerDoc",
  
  public = list(
    groupName = "DOCUMENT",
    textDisplay = TextDisplay,
    
    dfDocument = data.frame(), ### dataframe of biology values
    
    initialize = function(dfDocument){
      staticLogger$info("New Controller Document object")
      
      private$checkColumns(dfDocument)
      self$dfDocument <- dfDocument

      self$addTimelineObserver() ## when a Document code is clicked on the timeline
      ## observe clicked event : 
      #self$addObserverEventTable()
    },
    
    ###
    getDocument2display = function(START_DATE){
      bool <- self$dfDocument$START_DATE == START_DATE
      temp <-  subset(self$dfDocument,bool)
      return(temp)
    },
    
    getDivId = function(){
      return(GLOBALbiologyDiv)
    },
    
    getDOCTimevisDf = function(){
      startEnd <- subset (self$dfDocument, select=c("START_DATE","HTML_ICON","CONCEPT_CD","ENCOUNTER_NUM","NAME_CHAR"))
      startEnd <- unique(startEnd)
      startEnd$START_DATE <- as.POSIXct(startEnd$START_DATE)
      id <- paste0(self$groupName, "-",startEnd$START_DATE,"-",startEnd$CONCEPT_CD)
      timevisDf <- data.frame(id = id,
                              group = self$groupName,
                              start = startEnd$START_DATE,
                              end =  NA,## add one day to display correctly the icon
                              content = startEnd$HTML_ICON,
                              color = NA,
                              style = NA, #"border-width:0px;background-color:#ecffb3;",
                              title = NA,
                              type = "box",
                              ENCOUNTER_NUM = startEnd$ENCOUNTER_NUM,
                              NAME_CHAR = startEnd$NAME_CHAR)
      
      return(timevisDf)
    },
    
    addTimelineObserver = function(){
      observeEvent(input[[self$groupName]],{
        staticLogger$info("A Document event was clicked on the timeline")
        id <- staticTimeline$lastIdClicked
        ## id : groupName + "-" + start_date + "-" + concept_CD
        staticLogger$info("\t id event : ", id)
        start_date <- gsub(paste0(self$groupName,"-"), "",id) ## PMSI-date
        # start_date <- "2012-11-07 07:00:00" : 19 characters
        start_date <- substr(x = start_date, start=1, stop=19)
        staticLogger$info("\t extracted start_date from id : ",start_date)
        
        doc = self$getDocument2display(START_DATE = start_date) ### a PMSI form to display
        staticLogger$info("\t number of document retrieved :", nrow(doc))
        ## variable : 
        text <- doc$OBSERVATION_BLOB
        START_DATE <- as.character(as.Date(doc$START_DATE))
        NAME_CHAR <-  as.character(doc$NAME_CHAR)
        
        ## create a new FormDisplay :
        parentId <- staticTimeline$uiDisplay$getUIparentId() ## get the UIdisplay of the timeline
        textDisplay = TextDisplay$new(parentId = parentId,
                                      where = "beforeEnd",
                                      groupName = self$groupName
                                      )
        
        staticLogger$info("\t setting the doc and adding it to the timeline :")
        textDisplay$setText(text = text, START_DATE = START_DATE, NAME_CHAR = NAME_CHAR)
        ui <-  textDisplay$getUI() ## 
        UIid <- textDisplay$getDivId()
         
        # ## display the UI below Timeline 
        staticTimeline$uiDisplay$setUI(ui = ui,
                                       UIid = UIid)
        textDisplay$renderUI() ## display the graphic
        self$textDisplay <- textDisplay ##
        staticLogger$info("\t after render UI")
        # self$IsInitialized <- T
      })
    }
  ),
  
  
  
  private = list(
    expectedColumns = c("CONCEPT_CD",
                        "START_DATE",
                        "NAME_CHAR",
                        "OBSERVATION_BLOB",
                        "HTML_ICON",
                        "ENCOUNTER_NUM"
    ),
    
    checkColumns = function(dfDocument){
      bool <-  private$expectedColumns %in% colnames(dfDocument)
      if (!all(bool)) {
        stop("missing columns in dfDocument : ", private$expectedColumns[!bool])
      }
    }
  )
)
