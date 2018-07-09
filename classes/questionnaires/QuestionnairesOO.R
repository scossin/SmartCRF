Questionnaires <- R6::R6Class(
  "Questionnaires",
  inherit = TableDisplay,
  
  public = list(
    questionnaires = data.frame(), ## all the questionnaires data
    currentQuestionnaire = data.frame(), ## the questionnaire currently displayed
    previousRowsSelect = numeric(),
    
    groupName = "QUESTIONNAIRES",
    
    tableSearchObserver = NULL, ## the observer to record the searchValue
    
    formDisplay = NULL, ## display the raw data questionnaires form when timeline is clicked
    IsInitialized = F, ## when form is initialized, put back previous search value
    
    observerEventTable = NULL, ## when user wants to add a Concept_CD to the timeline
    
    initialize = function(questionnaires){
      private$checkColumns(questionnaires)
      questionnaires <- questionnaires[order(questionnaires$INSTANCE_NUM,
                                         questionnaires$ordre),]
      questionnaires$START_DATE <- as.POSIXct(questionnaires$START_DATE)
      self$questionnaires <- questionnaires
      
      ##
      self$addTimelineObserver() ## when a questionnaires code is clicked on the timeline
      ## observe clicked event : 
     #  self$addObserverEventTable()
      
      # self$addShowModalObserver() display the different questionnaire ?
    },
    
    ## what happens when an event on the timeline is clicked
    addTimelineObserver = function(){
      observeEvent(input[[self$groupName]],{
        staticLogger$info("A questionnaire event was clicked on the timeline")
        id <- staticTimeline$lastIdClicked
        staticLogger$info("\t id event : ", id)
        start_date <- unlist(strsplit(id, split="\t"))[3]
        ## start_date <- gsub(paste0(self$groupName,"-"), "",id) ## questionnaires-date
        staticLogger$info("\t extracted start_date from id : ",start_date)
        
        ### retrieve this form : 
        bool <- self$questionnaires$START_DATE == start_date
        form <- subset (self$questionnaires, bool)
        self$currentQuestionnaire <- form
        
        form <- subset (form, 
                        select=c("section",
                                 "Qlabel","TVAL_CHAR","questionnaire"))
        colnames(form) <- c("Section","Question","Reponse","questionnaire")
        formName <- unique(form$questionnaire)
        formName <- paste(formName, collapse="-")
        
        ## create a new FormDisplay :
        parentId <- staticTimeline$uiDisplay$getUIparentId() ## get the UIdisplay of the timeline
        formDisplay <- FormDisplay$new(parentId = parentId,
                                       where = "beforeEnd", ## doesn't matter this one
                                       groupName = self$groupName)
        form$questionnaire <- NULL ## not needed
        formDisplay$setForm(form)
        formDisplay$setPageLength(10)
        caption <- paste0(formName, " - ", start_date)
        formDisplay$setCaption(caption)
        
        # formDisplay
        ui <-  formDisplay$getUI() ## 
        UIid <- formDisplay$getDivId()
        
        ## display the UI below Timeline 
        staticTimeline$uiDisplay$setUI(ui = ui,
                                       UIid = UIid)
        formDisplay$renderUI() ## display the graphic
        
        self$formDisplay <- formDisplay ##
        
        self$IsInitialized <- T
        self$addTableSearchObserver(formId = formDisplay$getFormId())
        self$addObserverEventTable()
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
    },
    
    ### show questionnaires on the timeline
    getQuestionnairesTimevisDf = function(){
      startEnd <- subset (self$questionnaires, 
                          select=c("START_DATE",
                                   "questionnaire",
                                   "ENCOUNTER_NUM"))
      startEnd <- unique(startEnd)
      
      # style : (one color by questionnaire type)
      nColor <- length(table(startEnd$questionnaire))
      startEnd$color <- as.factor(startEnd$questionnaire)
      levels(startEnd$color) <- rainbow(nColor,start = 0.6,end = 1)
      startEnd$style <- paste0("background-color: ",startEnd$color)
      
      # startEnd <- subset (self$questionnaires, select=c("START_DATE","END_DATE"))
      id <- paste0(self$groupName, "\t", startEnd$questionnaire,"\t",
                   startEnd$START_DATE)
      
      timevisDf <- data.frame(id = id,
                              group = self$groupName,
                              start = startEnd$START_DATE,
                              end = NA,
                              # end = startEnd$END_DATE,
                              content = "<img src=\"questionnaire.png\">",
                              color = NA,
                              style = startEnd$style,
                              title = startEnd$questionnaire,
                              type = "box",
                              ENCOUNTER_NUM = startEnd$ENCOUNTER_NUM)
      timevisDf <- unique(timevisDf)
      # print(timevisDf)
      # save(timevisDf, file="timevisDf.rdata")
      # self$timeline$setTimevisDf(timevisDf)
      return(timevisDf)
    },
    
    
    # ### when clicking in the DT to add column to the timeline :
    addObserverEventTable = function(){
      ## destroy observer if it exists :
      if (!is.null(self$observerEventTable)){
        self$observerEventTable$destroy()
        self$previousRowsSelect <- numeric()
      }
      
      clickedInput <- paste0(self$formDisplay$getFormId(),"_rows_selected")
      self$observerEventTable <- observeEvent(input[[clickedInput]],{
        staticLogger$info("User wants to add a conceptCD of a questionnaire to the timeline")
        
        rowsSelected <- input[[clickedInput]]
        
        previousRowsSelect <- self$previousRowsSelect 

        ## case nothing happened :
        if (length(previousRowsSelect) == length(rowsSelected)){
          staticLogger$info("Something weird happened : rowsSelected change but it didn't change previous Selection")
          return(NULL)
        }
        
        ## case remove :
        if (length(previousRowsSelect) > length(rowsSelected)){
          staticLogger$info("The user removed a row to remove a group in the timeline")
          rowRemoved <- setdiff(previousRowsSelect, rowsSelected)
          
          line <- self$currentQuestionnaire[rowRemoved,]
          group <- paste0(self$groupName, "<br>",line$Qlabel) ## name of the group
          staticTimeline$removeGroup(group = group)
        } else {
          staticLogger$info("The user wants to add a new group to the timeline")
          rowAdded <- setdiff(rowsSelected,previousRowsSelect)
          line <- self$currentQuestionnaire[rowAdded,]
          group <- paste0(self$groupName, "<br>",line$Qlabel)
          line <- self$currentQuestionnaire[rowAdded,]
          concept_cd <- unique(line$CONCEPT_CD)
          staticLogger$info("\t concept_cd :",concept_cd)
          
          ## all questionnaires with this answer :
          bool <- self$questionnaires$CONCEPT_CD == concept_cd
          concept_selected <- subset (self$questionnaires, bool, 
                                  select=c("START_DATE","TVAL_CHAR"))
          ## aggregation if multiple values
          concept_selected <- concept_selected %>% 
            group_by(START_DATE) %>% 
            summarize (TVAL_CHARS = paste(TVAL_CHAR, collapse="-"))
          concept_selected <- as.data.frame(concept_selected)
          colnames(concept_selected) <- c("START_DATE","TVAL_CHAR")
          
          rawTimeSeries <-  concept_selected
          # save(rawTimeSeries, file="rawTimeSeries.rdata")
          timevisDfdata <- staticUtil$getAggregatedTimeSeries(rawTimeSeries = rawTimeSeries)
          timevisDfdata$group <- group
          timevisDfdata$id <- paste0(group, "-",timevisDfdata$start) ## id : group - start

          ## color yes/no
          # timevisDfdata$content <- paste0("<p>",timevisDfdata$content,"</p>") ## not a good idea...
          timevisDfdata$color <- NA
          timevisDfdata$style <- "background-color: cyan"
          bool <- timevisDfdata$start == timevisDfdata$end
          
          # timevisDfdata$type <- ifelse (bool, "box","range") ## not a good idea...
          timevisDfdata$type <- "range"
          timevisDfdata$ENCOUNTER_NUM  <- 999 ## no encounterNUM ! this is aggregated !
          # print(timevisDfdata)
          timevisDfdata$title <- timevisDfdata$content
          staticTimeline$addTimevisDfdata(timevisDfdata = timevisDfdata)
          
        } ## end of else : add to the timeline

        self$previousRowsSelect  <- rowsSelected
        staticLogger$info("Row added to the timeline !")
      },ignoreNULL = F)
    }
    ###
  ),
  
  private = list(
    expectedColumns = c("ENCOUNTER_NUM",
                        "CONCEPT_CD",
                        "PROVIDER_ID",
                        "START_DATE",
                        "INSTANCE_NUM",
                        "TVAL_CHAR",
                        "Qlabel",
                        "ordre",
                        "questionnaire",
                        "section"),
    checkColumns = function(questionnaires){
      bool <-  private$expectedColumns %in% colnames(questionnaires)
      if (!all(bool)) {
        stop("missing columns in questionnaires : ",
             private$expectedColumns[!bool])
      }
    }
  )
)