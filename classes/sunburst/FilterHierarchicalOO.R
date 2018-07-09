FilterHierarchical <- R6::R6Class(
  "FilterHierarchical",
  inherit = uiObject,
  
  public = list(
    hierarchy = data.frame(),
    hierarchicalData = data.frame(),
    choice = character(),
    sunburstObserver = NULL,
    
    initialize = function(parentId, where, hierarchy){
      super$initialize(parentId, where)
      staticLogger$info("initiliazing a new HierarchicalSunburst")
      self$hierarchy <- hierarchy
    }, 
    
    setEventCount = function(eventCount){
      bool <- c("className","count") %in%  colnames(eventCount) 
      if (!all(bool)){
        stop("missing columns in eventCount")
      }
      self$setHierarchicalData(eventCount)
    },
    
    insertUIhierarchical = function(){
      staticLogger$info("inserting HierarchicalSunburst")
      ui <- div (id = self$getDivSunburstId(),
                 sunburstR::sunburstOutput(outputId = self$getSunburstId()))
      insertUI(
        selector = private$getJquerySelector(self$parentId),
        where = self$where,
        ui = ui,
        immediate = T
      )
    },
    
    makePlot = function(){
      staticLogger$info("Plotting hierarchical", self$getHierarchicalPlotId())
      output[[self$getSunburstId()]] <- renderSunburst({
        sunburstData <- private$getSunburstData()
        sunburstData <- subset (sunburstData, size !=0)
        staticLogger$info("Trying to plot sunburst")
        # randomNumber <- round(abs(rnorm(1,1000000,1000000)),0)
        # save(sunburstData, file=paste0("sunburstDP",randomNumber,".rdata"))
        sunburstR::add_shiny(sunburstR::sunburst(sunburstData, count=T,legend = list(w=200)))
      })
    },
    
    insertUIandPlot = function(){
      staticLogger$info("Inserting UI hierarchical", self$getHierarchicalPlotId())
      self$insertUIhierarchical()
      self$makePlot()
    },
    
    
    setHierarchicalData = function(eventCount){
      staticLogger$info("Setting hierarchical Data")
      staticLogger$info("\t Merging hierarchy and eventCount ...")
      bool <- nrow(eventCount) == 0
      if (bool){
        hierarchicalData <- hierarchy
        hierarchicalData$count <- 0
      } else {
        ## we keep only codes with count != 0 
        bool <- hierarchy$code %in% eventCount$className
        staticLogger$info(sum(!bool)," codes have 0 count in the hierarchy")
        hierarchy <- subset (hierarchy, bool)
        hierarchicalData <- merge (hierarchy, eventCount, by.x="code", by.y="className")
      }
      # bool <- is.na(hierarchicalData$count) | hierarchicalData$count == 0
      # hierarchicalData$count[bool] <- 0
      colnames(hierarchicalData) <- c("code","label","hierarchy","size")
      #hierarchicalData <- rbind(hierarchicalData, data.frame(event="Event",hierarchy="Event",size=0))
      # private$checkHierarchicalData(hierarchicalData)
      self$hierarchicalData <- hierarchicalData
      # print(self$hierarchicalData)
    }, 
    
    destroy = function(){
      staticLogger$info("Destroying hierarchicalSunburst",self$getHierarchicalPlotId())
      
      staticLogger$info("\t Destroying observer sunburstObserver")
      if (!is.null(self$sunburstObserver)){
        self$treeObserver$destroy()
        staticLogger$info("\t Done")
      }
      
      staticLogger$info("\t Removing hierarchical UI")
      self$removeUIhierarchical()
      
      staticLogger$info("End destroying hierarchical Filter")
    },
    
    getCodeFromLabel = function(label){
      # print(self$hierarchicalData)
      bool <- self$hierarchicalData$label == label
      if (sum(bool)== 0){
        staticLogger$error(label, "not found in hierarchicalData")
        stop("")
      }
      if (sum(bool) > 1){
        staticLogger$error(label, "more than 1 label found")
        stop("")
      }
      return(as.character(self$hierarchicalData$code[bool]))
    },
    
    getDivId = function(){
      return(paste0("hierarchical-", private$randomNumber,self$parentId))
    },
    
    getHierarchicalPlotId = function(){
      return(paste0("HierarchicalPlot",self$getDivId()))
    },
    
    getDivSunburstId = function(){
      return(paste0("DivSunburstId", self$getHierarchicalPlotId()))
    },
    
    getSunburstId = function(){
      return(paste0("sunburst",self$getDivSunburstId()))
    },
    
    getUIsunburst = function(){
      ui <- div (id = self$getDivSunburstId(),
                 sunburstR::sunburstOutput(outputId = self$getSunburstId()))
      return(ui)
    },
    
    getCodeChoice = function(){
      labelChoices <- self$getEventChoice()
      # print(self$hierarchy$label)
      bool <- as.character(self$hierarchy$label) %in% as.character(labelChoices)
      # print(labelChoices)
      return(as.character(self$hierarchy$code[bool]))
    },
    
    getEventChoice = function(){
      return(as.character(private$eventChoice))
    },
    
    addEventChoiceSunburst = function(selection){
      previousChoices <- private$eventChoice
      bool <- selection %in% previousChoices
      private$eventChoice <- c(selection[!bool],previousChoices)
      return(NULL)
    },
    
    addSunburstObserver = function(){
      inputSunburst <- paste0(self$getSunburstId(), "_click")
      self$sunburstObserver <- observeEvent(input[[inputSunburst]],{
        sunburstChoice <- input[[inputSunburst]]
        if (is.null(sunburstChoice)){
          return(NULL)
        }
        selection <- self$getEventTypeSunburst(sunburstChoice = sunburstChoice)
        staticLogger$user(selection, "selected in ", self$getSunburstId())
        self$addEventChoiceSunburst(selection)
        self$printChoice()
      })
    }
  ),
  
  private = list(
    
    randomNumber = numeric(),
    
    setRandomNumber = function(){
      private$randomNumber =  abs(round(runif(1)*10000000,0))
    },
    
    eventChoice = character(),
    currentChoice = c("SUNBURST"),
    plotChoice = c("SUNBURST"),
    
    getSunburstData = function(){
      staticLogger$info("getSunburstData for HierarchicalSunburst")
      sunburstData <- subset(self$hierarchicalData, select=c("hierarchy","size"))
      return(sunburstData)
    }
  )
)