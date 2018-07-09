ControllerBiologie2 <- R6::R6Class(
  "ControllerBiologie2",
  inherit = TableDisplay,
  
  public = list(
    groupName = "BIOLOGIE",
    formDisplayBiologie = FormDisplayBiologie,
    selectedExams = character(), ## to add to the timeline
    
    dfBiologie = data.frame(), ### dataframe of biology values
    
    ## this instance must be in valueEnv environment so categoricalGraphics can call it
    initialize = function(dfBiologie){
      staticLogger$info("New Controller Biologie object")
      private$checkColumns(dfBiologie)
      self$setDfBiologie(dfBiologie)
     
      ### list of available biological tests : 
      ##
      self$addTimelineObserver() ## when a Biological test is clicked on the timeline
      self$addShowModalObserver()
      
      self$formDisplayBiologie <- FormDisplayBiologie$new(parentId = staticUtil$getDivModal(),
                                                     where = "beforeEnd",
                                                     groupName = self$groupName)
      form = self$getForm()
      self$formDisplayBiologie$setForm(form)
      self$addSelectizeObserver() ## add or remove biology in timeline
      
      ## observe clicked event : 
      #self$addObserverEventTable()
      
    },
    
    setDfBiologie = function(dfBiologie){
      # VALUEFLAG_CD
      ## H
      ## L 
      ## N
      arrowLowIcon <- "<i class=\"fas fa-arrow-circle-down fa-lg\"></i>"
      arrowHighIcon <- "<i class=\"fas fa-arrow-circle-up fa-lg\"></i>"
      # arrowNormalIcon <- "<i class=\"fas fa-clipboard-check fa-lg\"></i>"
      # arrowNormalIcon <- "<img src=\"like.png\">"
      arrowNormalIcon <- "<p>N</p>"
      arrowNAIcon <-  "<img src=\"blood-test.png\">"
      # arrowNAIcon <-  "<i class=\"fas fa-vial fa-lg\"></i>" ## same Icon as biologie timeline
      ## add HTML icon : 
      boolNA <- is.na(dfBiologie$VALUEFLAG_CD)
      boolH <- dfBiologie$VALUEFLAG_CD == "H"
      boolL <- dfBiologie$VALUEFLAG_CD == "L"
      # boolN <- startEnd$VALUEFLAG_CD == "N"
      
      dfBiologie$HTML_ICON <- ifelse (boolNA, arrowNAIcon,
                                    ifelse(boolH, arrowHighIcon,
                                           ifelse(boolL, arrowLowIcon,arrowNormalIcon)))
      
      dfBiologie$style <-  ifelse (boolNA, "background-color: cyan",
                                 ifelse(boolH, "background-color: red",
                                        ifelse(boolL, "background-color: red",
                                               "background-color: green")))
      
      dfBiologie$START_DATE_day <- as.Date(dfBiologie$START_DATE)
      self$dfBiologie <- dfBiologie
    },
    
    getForm = function(){
      # anormalBio <- subset (self$dfBiologie, VALUEFLAG_CD %in% c("H","L"))
      temp <- subset (self$dfBiologie, select=c("NVAL_NUM","VALUEFLAG_CD",
                                           "UNITS_CD","NAME_CHAR",
                                           "START_DATE_day","HTML_ICON"))
      temp <- temp[,c(5,3,1,4,2,6)]
      colnames(temp) <- c("Date","Unite","Valeur","Examen","LowNormalHigh","Icone")
      temp <- temp[rev(order(temp$Date)),]
      return(temp)
    },
    
    ### TODo
    addShowModalObserver = function(){
      observeEvent(input[["showModalBIOLOGIEid"]],{
        dialog <- staticUtil$getModalDialog()
        self$formDisplayBiologie$insertUI(self$selectedExams)
        self$formDisplayBiologie$renderUI()
        showModal(dialog)
        # self$categoricalGraphic$updateSelection()
      })
    },
    
    getDivId = function(){
      return(staticUtil$getDivModal())
    },
    
    ###
    getForm2display = function(START_DATE){
      if (is.null(START_DATE)){
        temp <- self$dfBiologie
      } else {
        bool <- self$dfBiologie$START_DATE_day == START_DATE
        temp <-  subset(self$dfBiologie,bool)
      }
      temp <- subset (temp, select=c("NVAL_NUM","VALUEFLAG_CD",
                                     "UNITS_CD","NAME_CHAR","START_DATE_day"))
      temp <- temp[,c(5,4,1,3,2)]
      colnames(temp) <- c("Date","Examen","Valeur","Unite","Low/Normal/High")
      return(temp)
    },
    
    getBIOLOGIETimevisDf = function(){
      startEnd <- subset (self$dfBiologie, select=c("START_DATE_day",
                                                    "ENCOUNTER_NUM"))
      startEnd <- unique(startEnd)
      startEnd$START_DATE_day <- as.POSIXct(startEnd$START_DATE_day)
      id <- paste0(self$groupName, "\t",startEnd$START_DATE_day,
                   "\t",startEnd$ENCOUNTER_NUM)
      timevisDf <- data.frame(id = id,
                              group = self$groupName,
                              start = startEnd$START_DATE_day,
                              end =  NA,
                              content = "<img src=\"blood-test.png\">",
                              # content = "<i class=\"fas fa-vial fa-lg\"></i>",
                              color = NA,
                              style = NA,
                              title = NA,
                              type = "box",
                              ENCOUNTER_NUM = startEnd$ENCOUNTER_NUM)
      return(timevisDf)
    },
    
    ## create a new dygraph 
    
    addSelectizeObserver = function(){
      selectizeObserver <- observeEvent(input[[self$formDisplayBiologie$getSelectizeId()]],{
        staticLogger$info("New choice in biology to add to the timeline")
        selectedExams <- input[[self$formDisplayBiologie$getSelectizeId()]]
        
        if (length(selectedExams) == length(self$selectedExams)){
          staticLogger$info("\t Nothing new finally")
          return(NULL)
        }
        
        if (length(selectedExams) > length(self$selectedExams)){
          staticLogger$info("\t user wants to add new biology to the timeline")
          newValue <- setdiff(selectedExams,self$selectedExams)
          self$addToTimelineBio(newValue)
          self$selectedExams <- selectedExams
          return(NULL)
        }
        
        if (length(selectedExams) < length(self$selectedExams)){
          staticLogger$info("\t user wants to remove biology to the timeline")
          oldValue <- setdiff(self$selectedExams,selectedExams)
          group <- paste0(self$groupName, "-",oldValue)
          staticTimeline$removeGroup(group)
          return(NULL)
        }
        
      },ignoreNULL = F)
    },
    
    ## add to timeline : 
    addToTimelineBio = function(newValue){
      staticLogger$info("Change in group : ", self$groupName)
      staticLogger$info("User wants to add biology to the timeline")
      staticLogger$info("\t newValue : ", newValue)
      
      group <- paste0(self$groupName, "-",newValue)
      bool <- self$dfBiologie$NAME_CHAR == newValue
      startEnd <- subset (self$dfBiologie, bool, select=c("START_DATE","NVAL_NUM",
                                                          "ENCOUNTER_NUM","VALUEFLAG_CD",
                                                          "HTML_ICON","style"))
      startEnd <- unique(startEnd)
      staticLogger$info("\t number of occurences of biological value : ", nrow(startEnd))
      
      ## put the value, not the icon !
      startEnd$HTML_ICON <- paste0("<p>",startEnd$NVAL_NUM,"</p>")
      
      id <- paste0(group, "-",startEnd$START_DATE)
      timevisDfdata <- data.frame(id = id,
                              group = group,
                              start = startEnd$START_DATE,
                              end =  NA,
                              content = startEnd$HTML_ICON,
                              color = NA,
                              style =  startEnd$style,
                              title = startEnd$NVAL_NUM,
                              ENCOUNTER_NUM = startEnd$ENCOUNTER_NUM)
      ## color yes/no
      # bool <- timevisDfdata$content == "oui"
      # timevisDfdata$style <- ifelse(bool, "background-color: cyan",
      #                               "background-color: white")
      # timevisDfdata$color <- ifelse(bool, "cyan",
      #                               "white")
      timevisDfdata$type <- "box"
      staticTimeline$addTimevisDfdata(timevisDfdata)
    },
    
    ## retrieve the concept_cd by the NAME_CHAR (biology name)
    getConceptCd = function(NAME_CHAR){
      staticLogger$info("\t \t Retrieving value of : ", NAME_CHAR)
      bool <- self$dfBiologie$NAME_CHAR == NAME_CHAR
      staticLogger$info("\t \t Number of retrieved values : ", sum(bool))
      concept_cd <-  unique(as.character(self$dfBiologie$CONCEPT_CD[bool]))
      staticLogger$info("\t \t Number of retrieved values : ", sum(bool))
      if (length(concept_cd) > 1) {
        staticLogger$info("WARNING : multiple concept_CD for this NAME_CHAR")
      }
      staticLogger$info("CONCEPT_CD : ", concept_cd[1])
      return(concept_cd[1])
    },getTS = function(concept_cd){
      ## return the right format for dygraph
      myTS <-  staticUtil$getMyTSnumeric(df = self$dfBiologie, 
                                         concept_cd = concept_cd,
                                         dfCategoriesNumericalValue = NULL)
      return(myTS$myTS)
    }
  ),
  
  
  
  private = list(
    expectedColumns = c("CONCEPT_CD",
                        "START_DATE",
                        "NVAL_NUM",
                        "NAME_CHAR",
                        "ENCOUNTER_NUM",
                        "VALUEFLAG_CD",
                        "UNITS_CD",
                        "PROVIDER_ID"
    ),
    
    checkColumns = function(dfBiologie){
      bool <-  private$expectedColumns %in% colnames(dfBiologie)
      if (!all(bool)) {
        stop("missing columns in dfBiologie : ", private$expectedColumns[!bool])
      }
    }
  )
)