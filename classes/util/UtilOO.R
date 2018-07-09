Util <- R6::R6Class(
  "Util",
  
  public = list(
    
    initialize = function(){
      
    },
    
    ### private function to get transformed data :
    # the idea is to merge a sequence :
    # yes yes yes no no => yes-yes-yes no-no
    getAggregatedTimeSeries = function(rawTimeSeries){
      expectedColumns <- c("START_DATE","TVAL_CHAR")
      bool <- expectedColumns %in% colnames(rawTimeSeries)
      if (!all(bool)) {
        stop("missing columns : ", expectedColumns[!bool])
      }
      
      ## function to extract the sequence :
      rawTimeSeries <- rawTimeSeries[order(rawTimeSeries$START_DATE),]
      result <-  rle(as.character(rawTimeSeries$TVAL_CHAR))
      indexSwitch <- cumsum(result$lengths)
      indexSwitch <- append(0, indexSwitch)
      niveaux <- result$values
      
      transformedTimevisDfseries <- NULL
      for (i in 1:length(result$lengths)){
        startIndex <- indexSwitch[i] + 1
        endIndex <- indexSwitch[i+1]
        niveau <- niveaux[i]
        
        ## debug :
        # cat("start:",startIndex,
        #     "\t end:",endIndex,
        #     "\t niveau:", niveau,"\n")
        oneAggregate <- data.frame(
          start = rawTimeSeries$START_DATE[startIndex],
          end = rawTimeSeries$START_DATE[endIndex],
          content = niveau
        )
        transformedTimevisDfseries <- rbind(transformedTimevisDfseries,
                                            oneAggregate)
      }
      return(transformedTimevisDfseries)
    },
    
    
    getMyTSnumeric = function(df, concept_cd, dfCategoriesNumericalValue = NULL){
      bool <- c("START_DATE","NAME_CHAR","CONCEPT_CD","NVAL_NUM") %in% colnames(df)
      if (!all(bool)){
        stop("important columns missing in df")
      }
      bool <- df$CONCEPT_CD == concept_cd
      numericalVariableDf <- subset(df, bool,
                                    select = c("START_DATE","NVAL_NUM"))
      
      numericalVariableDf$START_DATE
      
      myTS <- xts(x=numericalVariableDf,
                  order.by = numericalVariableDf$START_DATE, 
                  frequency = numericalVariableDf$NVAL_NUM)
      numericValues <- as.numeric(numericalVariableDf$NVAL_NUM)
      
      # Using cut
      if (!is.null(dfCategoriesNumericalValue)){
        categories <- cut(numericValues, 
                          breaks = c(dfCategoriesNumericalValue$values,Inf), 
                          labels = dfCategoriesNumericalValue$categories, 
                          right = T)
      } else {
        categories <- "all"
      }
      myTS$category <- as.character(categories) ## as.character is important !
      numericalVariablePosition <- which(colnames(myTS) == "NVAL_NUM")
      for (category in categories){ ## add a column to df : category
        bool <- myTS$category == category
        if (!any(bool)){
          next
        }
        values <- ifelse(bool, myTS[,2],NA)
        colnames(values) <- category
        myTS <- cbind(myTS, values)
      }
      colfunc <- colorRampPalette(c("orange", "red")) ## gradient function from green to blue color
      colors <- colfunc(length(unique(myTS$category)))## number of color to generate
      ### remove unecessary variables or it will be ploted
      myTS$START_DATE <- NULL
      myTS$TVAL_CHAR <- NULL
      myTS$category <- NULL
      
      ## very weird ! : to fix a bug
      myTS$norm2 <- 0
      myTS$norm2 <- NULL 
      return(list(myTS = myTS,
                  colors = colors))
    },
    
    getModalDialog = function(){
      dialog <- modalDialog(
        fluidPage(
          div(id=self$getDivModal()
          )
        ),
        size="l",
        easyClose = T
      )
      return(dialog)
    },
    
    getDivModal = function(){
      return("modalDialogDiv")
    },
    
    getModalDialogMedium = function(){
      dialog <- modalDialog(
        fluidPage(
          div(id=self$getDivModalMediumId()
          )
        ),
        size="m",
        easyClose = F
      )
      return(dialog)
    },
    
    getDivModalMediumId = function(){
      return("DivModalDialogMedium")
    }
  ),
  private = list(
  )
)