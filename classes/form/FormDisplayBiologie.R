FormDisplayBiologie <- R6::R6Class(
  "FormDisplayBiologie",
  inherit=uiObject,
  
  public=list(
    form = data.frame(), ## all the data
    formToDisplay = data.frame(), ## selected data
    formDate = character(),
    groupName = character(),
    selectizeObserver = NULL,
    checkboxObserver = NULL,
    selectedExams = character(), ## 
    chosenValuesLNH = character(), ## Low Normal High
    plotIds = data.frame(), ## 2 columns : examName, 
    
    initialize = function(parentId, where, groupName){
      staticLogger$info("Initializing new form Biology object")
      super$initialize(parentId = parentId, where = where)
      self$groupName <- groupName
      private$setRandomNumber()
      self$addSelectizeObserver()
      self$addCheckboxObserver()
      self$chosenValuesLNH <- c("Low","High")
    },
    
    setForm = function(form){
      self$form <- form
      self$setFormToDisplay()
    },
    
    insertUI = function(selected = NULL){
      staticLogger$info("inserting UI for biologie")
      ui <- self$getUI() ## datatable
      jQuerySelector = paste0("#",self$parentId)
      shiny::insertUI(selector = jQuerySelector,
               where = "beforeEnd",
               ui = ui,
               immediate = F)
      
      ## selectize :
      choices <- unique(self$form$Examen)
      ui <- shiny::selectizeInput(inputId = self$getSelectizeId(),
                                  label="",
                                  choices = choices,
                                  selected = selected,
                                  multiple = T,
                                  width = "100%",
                                  options = list(hideSelected = T,
                                                 closeAfterSelect = T,
                                                 plugins=list('remove_button','drag_drop')))
      jQuerySelector = paste0("#",self$parentId)
      shiny::insertUI(selector = jQuerySelector,
               where = self$where,
               ui = ui,immediate = F)
      return(NULL)
    },
    
    getUI = function(){
      ui <- div(id = self$getDivId(),
                h2("Resultats biologiques : "),
                DT::dataTableOutput(self$getFormId()),
                shiny::checkboxGroupInput(inputId = self$getCheckBoxId(),
                                          label = "",
                                          choices = c("High","Low","Normal"),
                                          selected = c("High","Low"),
                                          inline = T,
                                          choiceValues = c("H","F","N"))
      )
      return(ui)
    },
    
    getCheckBoxId = function(){
      return(paste0("checkBoxGroup-",self$getDivId()))
    },
    
    showPreviousSelectedRows = function(searchValue){
      staticLogger$info("showing Previous SearchValue : ",searchValue)
      DT::dataTableProxy(outputId = self$getFormId()) %>% 
        DT::updateSearch(keywords = list(global=searchValue))
    },
    
    setFormToDisplay = function(){
      df <- self$form 
      ## filter by chosenValues : 
      chosenValues <- self$chosenValuesLNH
      if (length(chosenValues) == 0){ # print an empty table
        staticLogger$info("\t printing an empty table")
        self$formToDisplay <- data.frame()
        return(NULL)
      }
      
      ## renaming
      chosenValues <- gsub("Low","L",chosenValues)
      chosenValues <- gsub("Normal","N",chosenValues)
      chosenValues <- gsub("High","H",chosenValues)
      bool <- df$LowNormalHigh %in% chosenValues
      df <- subset (df, bool) ## selected
      
      ## ordering by selectedExams : 
      df$Examen <- as.factor(df$Examen)
      selectedExams <- self$selectedExams
      numExamens <- which(levels(df$Examen) %in% selectedExams)
      otherNumExamens <- which(!levels(df$Examen) %in% selectedExams)
      df$Examen <- factor(df$Examen, levels(df$Examen)[c(numExamens,
                                                         otherNumExamens)])
      df <- df[order(df$Examen,- as.numeric(df$Date)),]
      self$formToDisplay <- df
    },
    
    renderUI = function(){
      output[[self$getFormId()]] <- DT::renderDataTable({
        staticLogger$info("Rendering DT UI ")
        DT::datatable(self$formToDisplay,
                      escape=F, rownames = F,
                      options=list(
                        pageLength = 10,
                        lengthMenu = c(5,10,15,20,30,50,100),
                        columnDefs = list(
                          list(className = 'dt-center', targets = c(0,1,4)),
                          list(className = 'dt-right', targets = c(2)))
                        #,rowCallback=JS(jscode)
                      ))
      })
    },
    
    addCheckboxObserver = function(){
      self$checkboxObserver <- observeEvent(input[[self$getCheckBoxId()]],{
        staticLogger$info("New choice in checkBox Biology")
        chosenValuesLNH <- input[[self$getCheckBoxId()]]
        staticLogger$info("Chosen Values : ",chosenValuesLNH)
        self$chosenValuesLNH <- chosenValuesLNH
        self$setFormToDisplay()
        self$renderUI()
      },ignoreNULL = F,ignoreInit = T)
    },
    
    addSelectizeObserver = function(){
      self$selectizeObserver <- observeEvent(input[[self$getSelectizeId()]],{
        staticLogger$info("New choice in selectize ... ")
        self$selectedExams <- input[[self$getSelectizeId()]]
        self$setFormToDisplay()
        self$renderUI()
        self$removeAllPlotId()
        dfSelect <- subset (self$form, Examen %in% self$selectedExams)
        minDate <- min(dfSelect$Date)
        maxDate <- max(dfSelect$Date)
        for (selectedExamen in self$selectedExams){
          self$plotBiologie(selectedExamen, minDate, maxDate)
        }
      },ignoreNULL = F,ignoreInit = T)
    },
    
    plotBiologie = function(selectedExamen, minDate, maxDate){
      staticLogger$info("Making biology plot of ", selectedExamen)
      
      # selectedExams <- c("VGM","Volum plaquettaire")
      # selectedExamen <- "VGM"
      
      temp <- subset (self$form, Examen == selectedExamen)
      temp$color <- ifelse(temp$LowNormalHigh %in% c("L","H"),
                               "red","green")
      ## 
      plotId <- self$getPlotId()
      self$plotIds <- c(self$plotIds, plotId)
      ui <- shiny::plotOutput(outputId = plotId,height = "200px")
      self$insertPlotUI(ui)
      
      staticLogger$info("\t rendering plot : ", selectedExamen)
      
      output[[plotId]] <- shiny::renderPlot({
        par(mar=c(2,4,1,0))
        minYlim <- min(temp$Valeur) - 0.1*min(temp$Valeur)
        maxYlim <- max(temp$Valeur) + 0.1*max(temp$Valeur)
        
        plot(temp$Date, temp$Valeur, ylim = c(minYlim,maxYlim), 
             xlim = c(minDate-15,maxDate+15),
             xaxt='n', xlab="",ylab=selectedExamen,col="black",cex=1.2,
             pch=15,type="b",lty=2,lwd=1)
        axis(side=1, at = temp$Date,labels = as.character(temp$Date),
             las=1)
        points(x=temp$Date,y = temp$Valeur, col=temp$color, cex=1.2,
               pch = 15)
      },res = 100,height=200)
    },
    
    insertPlotUI = function(ui){
      jQuerySelector = paste0("#",self$parentId)
      insertUI(selector = jQuerySelector,
               where = "beforeEnd",
               ui = ui,immediate = F)
    },
    
    removeAllPlotId = function(){
      for (plotId in self$plotIds){
        selector <- paste0("#", plotId)
        shiny::removeUI(selector = selector,
                        immediate = T)
      }
      self$plotIds <- character()
      return(NULL)
    },
    
    getPlotId = function(){
      return(paste0("BiologyPlot-",private$getRandomNumber()))
    },
    
    getSelectizeId = function(){
      return(paste0("selectize",self$parentId))
    },
    
    
    destroy = function(){
      staticLogger$info("Destroying form NOT IMPLEMENTED")
    },
    
    
    getDivId = function(){
      return(paste0("DivForm-",private$randomNumber,self$parentId))
    },
    
    getFormId = function(){
      return(paste0("Form",self$getDivId()))
    }
  ),
  
  private = list(
    
    randomNumber = numeric(),
    
    setRandomNumber = function(){
      private$randomNumber =  abs(round(runif(1)*10000000,0))
    },
    
    getRandomNumber = function(){
      return(abs(round(runif(1)*10000000,0)))
    }
  )
)
