SaveData <- R6::R6Class(
  "SaveData",
  inherit=uiObject,
  
  public = list(
    
    arguments = character(), ## in the text to support the decision
    observerArgumentsSent = NULL,
    observerShowSave = NULL,
    saveObserver = NULL,
    variableName = character(),
    variableChoices = character(),
    selected = NULL,
    
    initialize = function(parentId, where, variableName, variableChoices){
      super$initialize(parentId = parentId, where = where)
      self$addObserverArgumentsSent()
      self$addShowModalObserver()
      self$addSaveObserver()
      self$variableName <- variableName
      self$variableChoices <- variableChoices
    },
    
    setVariableName = function(variableName){
      self$variableName <- variableName
    },
    
    ### TODo
    addShowModalObserver = function(){
      self$observerShowSave <- observeEvent(input[["showModalSAVE"]],{
        dialog <- staticUtil$getModalDialogMedium()
        self$insertUI()
        # self$renderUI()
        showModal(dialog)
        # self$categoricalGraphic$updateSelection()
      })
    },
    
    insertUI = function(){
      selector <- private$getJquerySelector(elementId = self$parentId)
      ui <- self$getUI()
      shiny::insertUI(selector = selector,
                      where = self$where,
                      ui = ui,
                      immediate = F
                      )
    },
    
    getDivId = function(){
      return(paste0("DivSaveData", self$parentId))
    },
    
    getCheckBoxId = function(){
      return(paste0("CheckBox-", self$getDivId()))
    },
    
    getVariablesChoicesId = function(){
      return(paste0("VariablesChoices-", self$getDivId()))
    },
    getSaveButtonId = function(){
      return(paste0("SaveButton-", self$getDivId()))
    },
    
    getUI = function(){
      ui <- div(id = self$getDivId(), style="font-size:2em",
                shiny::HTML("<p style='text-align:center;font-size:2em;'> 
Enregistrement dans la base de donnees</p>"),
                shiny::p("patient numero : 123456789 ",style="float:right;"),
                shiny::p(paste0("Variable : ", self$variableName),class="testClass"),
                shiny::HTML("<p class='space' 
                            style='height:20px'></p>"),
                h3("Choix :"),
                shinyWidgets::radioGroupButtons(inputId = self$getVariablesChoicesId(), 
                                                label = "", 
                                                choices = self$variableChoices, 
                                                selected = self$selected,
                                                justified = TRUE, 
                                                checkIcon = list(yes = icon("ok", 
                
                                                                                                                                        lib = "glyphicon"))),
                shiny::HTML("<p class='space' 
                            style='height:20px'></p>"),
                h3("Annotations :"),
                shinyWidgets::awesomeCheckboxGroup(inputId = self$getCheckBoxId(), 
                                                   label = "", 
                                                   choices = self$arguments, 
                                                   selected = self$arguments,
                                                   width = "100%"),
                shiny::actionButton(inputId = self$getSaveButtonId(),
                                    label = "Sauvegarder",
                                    icon = icon("save"),
                                    style = "width: 40%;margin-left: 30%;margin-right: 30%;
                                    font-size:1.2em;background-color:orange")
      )
      return(ui)
    },
    
    addSaveObserver = function(){
      self$saveObserver <- observeEvent(input[[self$getSaveButtonId()]],{
        staticLogger$info("User clicked on the saved button")
        self$selected <- input[[self$getVariablesChoicesId()]]
        shiny::removeModal()
        shiny::showNotification(ui = "Sauvegarde effectuée")
        # self$categoricalGraphic$updateSelection()
      })
    },
    
    addObserverArgumentsSent = function(){
      self$observerArgumentsSent <- observeEvent(input[["arguments"]],{
        staticLogger$info("A new argument was received")
        argument <- input[["arguments"]]
        if (is.null(argument) || argument == ""){
          staticLogger$info("Argument received is empty, nothing will be done")
          shiny::showNotification(ui = "Demande d'enregistrement mais passage vide...
                                  Avez-vous surligné le passage à enregistrer ?",
                                  duration = 5, type="warning")
          return(NULL)
        }
        
        self$arguments <- c(self$arguments, argument)
        shiny::showNotification(ui = "Passage enregistré...",
                                duration = 3)
      })
    }
  ),
  
  private = list(
    
  )
)