Form <- R6::R6Class(
  "Form",
  
  public = list(
    allForms = data.frame(),
    currentForm = data.frame(),
    
    initialize = function(allForms){
      staticLogger$info("Initializing new Form object")
      self$setDf(allForms)
    },
    
    setDf = function(allForms){
      private$checkColumns(allForms)
      self$allForms <- allForms
      private$setUniqueDates()
      self$setCurrentForm(self$getLastDate())
    },
    
    getLastDate = function(){
      return(max(private$uniqueDates))
    },
    
    getAllDates = function(){
      return(private$uniqueDates)
    },
    
    getCurrentForm = function(){
      return(self$currentForm)
    },

    setCurrentForm = function(oneDate){
      bool <- as.character(oneDate) %in% as.character(private$uniqueDates)
      if (!bool){
        staticLogger$error(as.character(private$uniqueDates))
        stop("Unknown date : ", oneDate, " in uniqueDates")
      }
      dates <- as.character(self$allForms$START_DATE)
      bool <- dates == as.character(oneDate)
      self$currentForm <- subset(self$allForms, bool)
    },
    
    ## sectionQuestion :
    # a vecotr of sectionQuestion, section and question are separated by \t
    getAllTimeVisSeries = function(sectionQuestion){
      staticLogger$info("Section Question : ", sectionQuestion)
      allTimeVisSeries <- NULL
      for (sectionQ in sectionQuestion){
        section <- unlist(strsplit(sectionQ,split="\t"))[1]
        question <- unlist(strsplit(sectionQ,split="\t"))[2]
        allTimeVisSeries <- rbind(allTimeVisSeries,
                         self$getNewTimeVisSeries(section,question))
      }
      allTimeVisSeries$id <- 1:nrow(allTimeVisSeries) ## will be change later
      ## negative to not conflict
      #allTimeVisSeries$id <- allTimeVisSeries$id * 1
      return(allTimeVisSeries)
    },
    
    ## ! form$allForms outisde !
    getNewTimeVisSeries = function(Section, Question){
      staticLogger$info("Trying to create TimevisSeries for : ", Section, " \t", Question)
      bool <- self$allForms$NAME_CHAR == Question &
        form$allForms$section == Section
      if (!any(bool)){
        stop("Unfound : ", Section , " with Question : ",
             Question)
      }
      
      ### raw data timeviz 
      rawTimeSeries <- subset (form$allForms, bool, select=c("TVAL_CHAR","START_DATE"))
      
      ##
      transformedTimevisDfseries <- staticUtil$getAggregatedTimeSeries(rawTimeSeries)
      
      ### merging raw and transformed 
      colnames(rawTimeSeries) <- c("content","start")
      rawTimeSeries$end <- NA
      
      timevisDfseries <- rbind(transformedTimevisDfseries, 
                               rawTimeSeries)
      
      # timevisDfseries$id <- 1:nrow(timevisDfseries)
      
      levelsContent <- levels(timevisDfseries$content)
      
      ### 2 possibilities handled :
      ## levelsContent in oui/non or not
      bool <- levelsContent %in% c("Oui","Non")
      if (all(bool)) {
        ## color
        bool <- timevisDfseries$content == "Oui"
        timevisDfseries$color <-  ifelse(bool, "green","red")
        
        ## hovering the mouse
        timevisDfseries$title <- Question
      } else {
        ## color
        nLevels <- length(levelsContent)
        colfunc <- colorRampPalette(c("green", "blue")) ## gradient function from green to blue color
        timevisDfseries$color <- timevisDfseries$content
        levels(timevisDfseries$color) <- colfunc(nLevels) ## number of color to generate
        
        ## hovering the mouse
        timevisDfseries$title <- timevisDfseries$content
      }
      
      ## same operations for both possibilities :
      timevisDfseries$style <- paste0("background-color: ",timevisDfseries$color,";")
      bool <- is.na(timevisDfseries$end) ## remove content of raw data (no text)
      timevisDfseries$content[bool] <- NA
      
      ## name of the group is the question
      timevisDfseries$group <- paste0("<h5>",Section,"</h2>","<p>",Question,"</p>")
      timevisDfseries$SectionQuestion <- paste0(Section,"\t",Question)
      
      return(timevisDfseries)
    }
  ),
  
  private = list(
    checkColumns = function(allForms){
      expectedColumns <- c("section","TVAL_CHAR","START_DATE",
                           "ordre","NAME_CHAR")
      bool <-  expectedColumns %in% colnames(allForms)
      if (!all(bool)) {
        stop("missing columns in PRform : ", expectedColumns[!bool])
      }
    },
    
    setUniqueDates = function(){
      private$uniqueDates <- as.POSIXct(names(table(self$allForms$START_DATE)))
    }
  )
)



