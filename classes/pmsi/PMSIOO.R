PMSI <- R6::R6Class(
  "PMSI",
  inherit = TableDisplay,
  
  public = list(
    pmsi = data.frame(), ## all the pmsi data
    pmsiSelect = data.frame(), ## the overview of the different codes
    groupName = "PMSI",
    
    tableSearchObserver = NULL, ## the observer to record the searchValue
    
    formDisplay = NULL, ## display the raw data PMSI form when timeline is clicked
    IsInitialized = F, ## when form is initialized, put back previous search value
    
    FormAddTimelinePMSI = NULL, ## display pmsiSelect
    observerEventTable = NULL,
    
    initialize = function(pmsi){
      private$checkColumns(pmsi)
      pmsi <- pmsi[order(pmsi$SOURCESYSTEM_CD),]
      self$pmsi <- pmsi
      self$setPMSIselect(pmsi)
      
      self$FormAddTimelinePMSI <- FormAddTimelinePMSI$new(parentId = staticUtil$getDivModal(),
                                                          where = "beforeEnd",
                                                          groupName = self$groupName)
      self$FormAddTimelinePMSI$setForm(self$pmsiSelect)
      # self$formAddTimeline$setNumColumnToAdd("code")
      
      ## not render initially !
      # self$FormAddTimelinePMSI$renderUI()
      
      ##
      self$addTimelineObserver() ## when a PMSI code is clicked on the timeline
      ## observe clicked event : 
       self$addObserverEventTable()
       self$addObserverEventTableDAS()
      self$addShowModalObserver()
    },
    
    addShowModalObserver = function(){
      observeEvent(input[["showModalPMSIid"]],{
        dialog <- staticUtil$getModalDialog()
        pmsi$FormAddTimelinePMSI$insertUIformTimeline()
        pmsi$FormAddTimelinePMSI$renderUI()
        showModal(dialog)
      })
    },
    
    ## what happens when an event on the timeline is clicked
    addTimelineObserver = function(){
      observeEvent(input[[self$groupName]],{
        staticLogger$info("A PMSI event was clicked on the timeline")
        id <- staticTimeline$lastIdClicked
        staticLogger$info("\t id event : ", id)
        
        start_date <- unlist(strsplit(id,split="\t"))[2]
        
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
    },
    
    ## PMSI overview
    setPMSIselect = function(pmsi){    
      pmsiSelect <- subset (pmsi, select=c("START_DATE","code","END_DATE",
                                           "SOURCESYSTEM_CD","NAME_CHAR"))
      
      pmsiSelect <- pmsiSelect %>%
        group_by(code, SOURCESYSTEM_CD) %>% 
        mutate(n = n())
      
      pmsiSelect <- pmsiSelect %>%
        group_by(code, SOURCESYSTEM_CD) %>% 
        mutate(lastDate = max(END_DATE))
      
      pmsiSelect <- pmsiSelect %>%
        group_by(code, SOURCESYSTEM_CD) %>% 
        mutate(firstDate = min(START_DATE))
      
      pmsiSelect <- as.data.frame(pmsiSelect)
      pmsiSelect$START_DATE <- NULL
      pmsiSelect$END_DATE <-  NULL
      pmsiSelect <- unique(pmsiSelect)
      pmsiSelect <- pmsiSelect[order(-pmsiSelect$n),] ## order by frequency of codes
      pmsiSelect$lastDate <- as.Date(pmsiSelect$lastDate)
      pmsiSelect$firstDate <- as.Date(pmsiSelect$firstDate)
      self$pmsiSelect <- pmsiSelect
    },
    
    ### show PMSI on the timeline
    getPMSITimevisDf = function(){
      ### DP or DR when DP == Z
      bool <- self$pmsi$SOURCESYSTEM_CD %in% c("PMSI--DP","PMSI--DR") 
      pmsiDPDR <- subset (self$pmsi, bool)
      
      ## DP or DR ?
      bool <- pmsiDPDR$SOURCESYSTEM_CD  == "PMSI--DR"
      pmsiDPDR$isDR <- ifelse (bool, T, F)
      
      startEnd <- subset (pmsiDPDR, select=c("START_DATE","END_DATE","code",
                                             "ENCOUNTER_NUM","isDR","PROVIDER_ID","NAME_CHAR"))
      startEnd$letter <- substr(startEnd$code,1,1)
      startEnd$number <- as.numeric(substr(startEnd$code,2,3))
      
      ## remove  Z codes
      bool <- startEnd$letter == "Z"
      startEnd <- subset (startEnd, !bool)
      
      ## unique
      startEnd <- unique(startEnd)
      
      startEnd$HTML_CODE <- NA
      for (i in 1:nrow(startEnd)){
        letter <- startEnd$letter[i]
        number <- startEnd$number[i]
        startEnd$HTML_CODE[i] <- private$getHTML_CODEpmsi(letter = letter, number = number)
      }
      
      ## style :
      startEnd$style <- ifelse (startEnd$isDR, "background-color:#7F7FFF",
                                "background-color:#ccddff")
      
      startEnd$letter <- NULL
      startEnd$number <- NULL
      
      # startEnd <- subset (self$pmsi, select=c("START_DATE","END_DATE"))
      startEnd <- unique(startEnd)
      startEnd$START_DATE <- as.POSIXct(startEnd$START_DATE)
      startEnd$END_DATE <- as.POSIXct( startEnd$END_DATE)
      id <- paste0(self$groupName, "\t",startEnd$START_DATE,"\t",
                   startEnd$PROVIDER_ID)
      timevisDf <- data.frame(id = id,
                              group = self$groupName,
                              start = startEnd$START_DATE,
                              end = NA,
                              # end = startEnd$END_DATE,
                              content = startEnd$HTML_CODE,
                              color = NA,
                              style = startEnd$style,
                              title = startEnd$NAME_CHAR,
                              type = "box",
                              ENCOUNTER_NUM = startEnd$ENCOUNTER_NUM)
      timevisDf <- unique(timevisDf)
      # print(timevisDf)
      # save(timevisDf, file="timevisDf.rdata")
      
      # self$timeline$setTimevisDf(timevisDf)
      return(timevisDf)
    },
    
    ### when clicking in the DT to add column to the timeline :
    addObserverEventTable = function(){
      clickedInput <- paste0(self$FormAddTimelinePMSI$getFormDPId(),"_rows_selected")
      self$observerEventTable <- observeEvent(input[[clickedInput]],{
        staticLogger$info("User wants to add a new event to the timeline")
        rowsSelected <- input[[clickedInput]]
        staticLogger$info("Rows Selected change in group : ", self$groupName)
        
        previousRowsSelect <- self$FormAddTimelinePMSI$rowsSelected
        
        ## case nothing happened :
        if (length(previousRowsSelect) == length(rowsSelected)){
          staticLogger$info("Something weird happened : rowsSelected change but it didn't change previous Selection")
          return(NULL)
        }
        ## case remove :
        if (length(previousRowsSelect) > length(rowsSelected)){
          staticLogger$info("The user removed a row to remove a group in the timeline")
          rowRemoved <- setdiff(previousRowsSelect, rowsSelected)
          line <- self$FormAddTimelinePMSI$getLineDP(rowRemoved)
          # group <- paste0(line$SOURCESYSTEM_CD, "\n",line$NAME_CHAR) ## name of the group
          group <- paste0("PMSI--DP/DR", "\n",line$libelle)
          staticTimeline$removeGroup(group = group)
        } else {
          staticLogger$info("The user wants to add a new group to the timeline")
          rowAdded <- setdiff(rowsSelected,previousRowsSelect)
          line <- self$FormAddTimelinePMSI$getLineDP(rowAdded)
          group <- paste0("PMSI--DP/DR", "\n",line$libelle)
          
          ##### Get TimeVisData :
          bool <- self$pmsi$code == line$code & self$pmsi$SOURCESYSTEM_CD %in% c("PMSI--DR","PMSI--DP")
          codeSelected <- subset (self$pmsi, bool, select=c("START_DATE"))
          allStartDate <- subset (self$pmsi, select=c("START_DATE"))
          bool <- allStartDate$START_DATE %in% codeSelected$START_DATE
          staticLogger$info("\t number of occurences of the code : ", sum(bool))
          allStartDate$TVAL_CHAR <- ifelse (bool, "oui","non") ## oui : code is present ; non : absent
          rawTimeSeries <-  allStartDate
          # save(rawTimeSeries, file="rawTimeSeries.rdata")
          timevisDfdata <- staticUtil$getAggregatedTimeSeries(rawTimeSeries = rawTimeSeries)
          timevisDfdata$group <- group
          timevisDfdata$id <- paste0(group, "-",timevisDfdata$start) ## id : group - start 
          
          ## color yes/no
          bool <- timevisDfdata$content == "oui"
          timevisDfdata$style <- ifelse(bool, "background-color: cyan",
                                        "background-color: white")
          timevisDfdata$color <- ifelse(bool, "cyan",
                                        "white")
          timevisDfdata$content <- line$libelle 
          timevisDfdata <- subset (timevisDfdata, bool) ### remove no 
          timevisDfdata$title <- timevisDfdata$content
          timevisDfdata$type <- "range"
          timevisDfdata$ENCOUNTER_NUM  <- 999 ## no encounterNUM ! this is aggregated !
          # print(timevisDfdata)
          # timevisDfdata$content <- "test"
          staticTimeline$addTimevisDfdata(timevisDfdata = timevisDfdata)
        } ## end of else : add to the timeline
        
        self$FormAddTimelinePMSI$rowsSelected <- rowsSelected
        staticLogger$info("Row added to the timeline !")
      },ignoreNULL = F)
    },
    
    addObserverEventTableDAS = function(){
      clickedInput <- paste0(self$FormAddTimelinePMSI$getFormDASId(),"_rows_selected")
      self$observerEventTable <- observeEvent(input[[clickedInput]],{
        staticLogger$info("User wants to add a new event DAS to the timeline")
        rowsSelected <- input[[clickedInput]]
        staticLogger$info("Rows Selected change in group : ", self$groupName)
        
        previousRowsSelect <- self$FormAddTimelinePMSI$rowsSelectedDAS
        
        ## case nothing happened :
        if (length(previousRowsSelect) == length(rowsSelected)){
          staticLogger$info("Something weird happened : rowsSelected change but it didn't change previous Selection")
          return(NULL)
        }
        ## case remove :
        if (length(previousRowsSelect) > length(rowsSelected)){
          staticLogger$info("The user removed a row to remove a group in the timeline")
          rowRemoved <- setdiff(previousRowsSelect, rowsSelected)
          line <- self$FormAddTimelinePMSI$getLineDAS(rowRemoved)
          # group <- paste0(line$SOURCESYSTEM_CD, "\n",line$NAME_CHAR) ## name of the group
          group <- paste0("PMSI--DAS", "\n",line$libelle)
          staticTimeline$removeGroup(group = group)
        } else {
          staticLogger$info("The user wants to add a new group to the timeline")
          rowAdded <- setdiff(rowsSelected,previousRowsSelect)
          line <- self$FormAddTimelinePMSI$getLineDAS(rowAdded)
          group <- paste0("PMSI--DAS", "\n",line$libelle)
          
          ##### Get TimeVisData :
          bool <- self$pmsi$code == line$code & self$pmsi$SOURCESYSTEM_CD %in% c("PMSI--DAS")
          codeSelected <- subset (self$pmsi, bool, select=c("START_DATE"))
          allStartDate <- subset (self$pmsi, select=c("START_DATE"))
          bool <- allStartDate$START_DATE %in% codeSelected$START_DATE
          staticLogger$info("\t number of occurences of the code : ", sum(bool))
          allStartDate$TVAL_CHAR <- ifelse (bool, "oui","non") ## oui : code is present ; non : absent
          rawTimeSeries <-  allStartDate
          # save(rawTimeSeries, file="rawTimeSeries.rdata")
          timevisDfdata <- staticUtil$getAggregatedTimeSeries(rawTimeSeries = rawTimeSeries)
          timevisDfdata$group <- group
          timevisDfdata$id <- paste0(group, "-",timevisDfdata$start) ## id : group - start 
          
          ## color yes/no
          bool <- timevisDfdata$content == "oui"
          timevisDfdata$style <- ifelse(bool, "background-color: cyan",
                                        "background-color: white")
          timevisDfdata$color <- ifelse(bool, "cyan",
                                        "white")
          timevisDfdata$content <- line$libelle 
          timevisDfdata <- subset (timevisDfdata, bool) ### remove no 
          timevisDfdata$title <- timevisDfdata$content
          timevisDfdata$type <- "range"
          timevisDfdata$ENCOUNTER_NUM  <- 999 ## no encounterNUM ! this is aggregated !
          # print(timevisDfdata)
          # timevisDfdata$content <- "test"
          staticTimeline$addTimevisDfdata(timevisDfdata = timevisDfdata)
        } ## end of else : add to the timeline
        
        self$FormAddTimelinePMSI$rowsSelectedDAS <- rowsSelected
        staticLogger$info("Row added to the timeline !")
      },ignoreNULL = F)
    },
    
    ###
    getForm2display = function(START_DATE){
      pmsiInstant <- subset (self$pmsi, select=c("code","NAME_CHAR","SOURCESYSTEM_CD",
                                                 "START_DATE","END_DATE"))
      bool <- pmsiInstant$START_DATE == START_DATE
      temp <-  subset(pmsiInstant,bool)
      return(temp)
    }
  ),
  
  private = list(
    expectedColumns = c("SOURCESYSTEM_CD",
                        "NAME_CHAR", ## lib of the code
                        "code", ## ICD10 code 
                        "START_DATE", ## start date of the VISIT ! (not the code)
                        "END_DATE",## end date of the VISIT ! (not the code)
                        "ENCOUNTER_NUM",
                        "PROVIDER_ID"), ## encounter
    checkColumns = function(pmsi){
      bool <-  private$expectedColumns %in% colnames(pmsi)
      if (!all(bool)) {
        stop("missing columns in pmsi : ", private$expectedColumns[!bool])
      }
    },
    
    getHTML_CODEpmsi = function(letter, number){
      getContent = function(letter){
        return(paste0(  "<img src=\"ICD10images/",letter,".png\">"))
      }
      
      ## only one possibility
      if (letter %in% c("A","B","C","E","F","G","I","J","K","L","M","N","O","P",
                        "Q","R","S","T","V","Y","Z")){
        return(getContent(letter))
      }
      
      if (letter == c("D")){
        if (number > 48 ){
          return(getContent("D"))
        } else {
          return(getContent("C"))
        }
      }
      
      ## H
      if (letter == c("H")){
        if (number < 60 ){
          return(getContent("H"))
        } else {
          return(getContent("H2"))
        }
      }
    }
  )
)

