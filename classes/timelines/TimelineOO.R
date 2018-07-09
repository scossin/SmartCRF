Timeline <- R6::R6Class(
  inherit = uiObject,
  "Timeline",
  
  public = list(
    context = character(),
    currentTimevisDf = data.frame(),
    minDateWindow = NULL,
    maxDateWindow = NULL,
    clickedEventObserver = NULL,
    groupDf = NULL,
    lastIdClicked = NULL,
    uiDisplay = NULL,
    
    initialize = function(parentId, where){
      super$initialize(parentId, where)
      self$uiDisplay <- UIdisplay$new(self$getUIdisplayDivId(), 
                                      where = "beforeEnd" )
      self$addClickedEventObserver()
      self$addObserverWindow()
    },
    
    getUIdisplayDivId = function(){
      # paste0("UIdisplay-",self$parentId)
      return(GLOBALaddTimelineID)
    },
    
    
    addTimevisDfdata = function(timevisDfdata){
      staticLogger$info("Timeline must add a new timevisDfdata")
      private$checkColumns(timevisDfdata)
      
      ### check Id is unique : 
      nIds <- length(unique(timevisDfdata$id))
      if (nrow(timevisDfdata) != nIds){
        stop("Not unique Id detected in the timeline !")
      }
      
      timevisDfadd <- subset(timevisDfdata, 
                             select = private$expectedColumns)
      
      newGroup <- unique(as.character(timevisDfdata$group))
      self$setGroupDf(newGroup)
      newGroupId <- self$getGroupId(newGroup)
      timevisDfadd$group <- newGroupId
      timevisDfadd$initialColor <-  as.character(timevisDfadd$style)
      timevisDfadd$end <-  as.character(timevisDfadd$end) ## avoid factor !
      
      if (nrow(self$currentTimevisDf) != 0){
        self$currentTimevisDf$initialColor <- as.character(self$currentTimevisDf$initialColor)
      }
      self$currentTimevisDf <- rbind(self$currentTimevisDf,
                                     timevisDfadd)
      
      ## initialization
      if (is.null(self$minDateWindow)){
        self$minDateWindow <- min(self$currentTimevisDf$start,na.rm = T)
        self$maxDateWindow <- max(self$currentTimevisDf$end,na.rm = T)
      }

      
      self$rePlotGroup()
      self$plotTimeline()
    },
    
    setGroupDf = function(newGroup){
      staticLogger$info("Setting group")
      enumGroup <- data.frame(groupName = c("PMSI","BIOLOGIE","DOCUMENT","QUESTIONNAIRES"),
                              groupRange = c("199","299","399","499"))
      newGroupBegin <- stringr::str_extract(string = newGroup, pattern = "^[A-Z]+")
      staticLogger$info("group type : ", newGroupBegin)
      bool <- enumGroup$groupName == newGroupBegin
      if (!any(bool)){
        stop("newGroup not found in enumGroup")
      }
      
      groupId <- as.numeric(as.character(enumGroup$groupRange[bool]))
      bool <- groupId %in% self$groupDf$id
      while (bool){
        groupId <- groupId - 1
        bool <- groupId %in% self$groupDf$id
      }
      
      staticLogger$info("new Group id :", groupId)
      
      ### initial case :
      if (is.null(self$groupDf) || nrow(self$groupDf) == 0){
        staticLogger$info("Creating group ",groupId)
        self$groupDf <- data.frame(id = groupId, content = newGroup,
                                   className = newGroupBegin)
        return(NULL)
      }
      
      newGroup <- data.frame(id = groupId, content = newGroup,
                             className = newGroupBegin)
      self$groupDf <-  rbind(self$groupDf, newGroup)
      # self$groupDf <- self$groupDf[order(self$groupDf$id),]
      # self$groupDf$id <- 1:nrow(self$groupDf)
      print(self$groupDf)
    },
    
    getGroupId = function(group){
      bool <- self$groupDf$content == group
      return(as.numeric(self$groupDf$id[bool]))
    },
    
    removeGroup = function(group){
      staticLogger$info("Timeline must removed a group from the timeline")
      staticLogger$info("\t groupName : ",group)
      bool <- self$groupDf$content == group
      if (!any(bool)){
        staticLogger$error("Timeline didn't find the group", group)
        return(NULL)
      }
      groupId <- self$getGroupId(group)
      bool <-  self$currentTimevisDf$group == groupId
      self$currentTimevisDf <- subset(self$currentTimevisDf,!bool)
      
      bool <- self$groupDf$id == groupId
      self$groupDf <- subset (self$groupDf, !bool) ## remove from group df
      
      self$rePlotGroup()
      self$plotTimeline()
    },
    
    setTimevisDfColor = function(){
      bool <- timevisDf$id %in% self$eventsSelected$event
      timevisDf$style <- "background-color: cyan"
      timevisDf$style[bool] <- "background-color: orange"
      self$timevisDf <- timevisDf
    },
    
    plotTimeline = function(){
      config <- list(
        clickToUse = F,
        editable = F, ### can't delet on the timeline
        multiselect = F, ## can't select multiple forms
        stack = F  ## 
      )
      output[[self$getTimelinePlotId()]] <- timevis::renderTimevis(
        timevis(data = self$currentTimevisDf,
                groups = self$groupDf,
                options = config) %>%  setWindow(start = as.Date(self$minDateWindow),
                             end = as.Date(self$maxDateWindow),
                             options = list(animation=F) ## animation false is important !
                             ))
          
            #           itemId = self$lastIdClicked,
            # options = list(animation = T)))
      
      # if (!is.null(self$minDateWindow)){
      #   self$setWindowTimeline()
      # }
    },
    
    rePlotGroup = function(){
      timevis::setGroups(id = self$getTimelinePlotId(),
                         data = self$groupDf)
    },
    
    insertUItimeline = function(){
      jQuerySelector = paste0("#",self$parentId)
      insertUI(selector = jQuerySelector,
               where = self$where,
               ui = self$getUI())
    },
    
    getUI = function(){
      ui <- div(id = self$getDivId(),
                timevis::timevisOutput(self$getTimelinePlotId()),
                div (id = self$getUIdisplayDivId())
                )
      return(ui)
    },
    
    getStartDate = function(id){
      bool <- self$currentTimevisDf$id == id
      if (!any(bool)){
        stop("unfound id : ", id, " in timevisDf dataframe id column")
      }
      startDate <- self$currentTimevisDf$start[bool]
      return(as.character(startDate))
    },
    
    getDivId = function(){
      return(paste0("timelineInfoDiv",self$parentId))
    }, 
    
    getTimelinePlotId = function(){
      return(paste0("timelinePlotId",self$getDivId()))
    },
    
    ### when clicking on the timeline
    
    addObserverWindow = function(){
      inputName <- paste0(self$getTimelinePlotId(),"_window")
      observeEvent(input[[inputName]],{
        windowDates <- input[[inputName]]
        self$minDateWindow <- windowDates[1]
        self$maxDateWindow <- windowDates[2]
      })
    },
    
    # setWindowTimeline = function(){
    #   staticLogger$info("resetting window timeline")
    #   staticLogger$info("minDate :", self$minDateWindow)
    #   staticLogger$info("maxDate :", self$maxDateWindow)
    #   timevis::centerItem(id = self$getTimelinePlotId(),
    #                      start = as.Date(self$minDateWindow),
    #                      end = as.Date(self$maxDateWindow),
    #                      options = F
    #                      )
    # },
    
    highlightEncounterNum = function(ENCOUNTER_NUM){
      bool <- self$currentTimevisDf$ENCOUNTER_NUM %in% ENCOUNTER_NUM
      
      staticLogger$info("Number of encounterNum similar :",sum(bool))
      ##
      bool2 <- !is.na(self$currentTimevisDf$style[bool]) & self$currentTimevisDf$style[bool] == "background-color: yellow"
      if (all(bool2)){
        staticLogger$info("id clicked already in yellow")
        return(NULL)
      }
      
      self$currentTimevisDf$style <- self$currentTimevisDf$initialColor
      self$currentTimevisDf$style[bool] <- "background-color: yellow"
      self$plotTimeline()
    },
    
    addClickedEventObserver = function(){
      inputName <- paste0(self$getTimelinePlotId(),"_selected")
      self$clickedEventObserver <- observeEvent(input[[inputName]],{
        id <- input[[inputName]]
        staticLogger$info("This id was clicked on the timeline : ", id)
        self$lastIdClicked <- id
        dispatcher <- stringr::str_extract(id, pattern = "^[A-Za-z]+")
        staticLogger$info("\t dispatching to ", dispatcher)
        session$sendCustomMessage(type = "timelineClicked",
                                  message = list(dispatcher = dispatcher))
        
        staticLogger$info("Highlighting the : ", id)
        bool <- self$currentTimevisDf$id == id
        ENCOUNTER_NUM <- as.character(self$currentTimevisDf$ENCOUNTER_NUM[bool])
        self$highlightEncounterNum(ENCOUNTER_NUM)
        
        ## debug 
        # timevisDf <- self$currentTimevisDf
        # save(timevisDf,file="timevisDf.rdata")
      #   startDate <- self$timeline$getStartDate(id = as.numeric(id))
      #   staticLogger$info("Trying to retrieve currentFrom at date :",
      #                     as.character(startDate))
      #   tryCatch(expr = {
      #     self$form$setCurrentForm(oneDate = startDate)
      #     ## changing the currentForm and rendering it
      #     currentForm <- self$form$getCurrentForm()
      #     self$formDisplay$setForm(currentForm)
      #     self$formDisplay$renderUI()
      #     self$formDisplay$showPreviousSelectedRows(self$searchValue)
      #   }, error = function(e){
      #     staticLogger$error("Something want wrong, probably the item clicked on the timeline is not a Formulaire")
      #     staticLogger$error(e$message)
      #   })
      })
    }
  ),
  
  private = list(
    expectedColumns = c("id",
                        "start",
                        "end",
                        "group",
                        "content",
                        "color",
                        "style",
                        "title",
                        "type",
                        "ENCOUNTER_NUM"),
    
    checkColumns = function(timevisDf){
      bool <-  private$expectedColumns %in% colnames(timevisDf)
      if (!all(bool)) {
        stop("missing columns in timevisDf : ", private$expectedColumns[!bool])
      }
    },
    
    checkColumnsGroup = function(groupDf){
      expectedColumns <- c("id","content")
      bool <-  expectedColumns %in% colnames(groupDf)
      if (!all(bool)) {
        stop("missing columns in groupDf : ", expectedColumns[!bool])
      }
    }
  )
)