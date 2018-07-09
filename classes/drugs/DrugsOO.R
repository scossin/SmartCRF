Drugs <- DataTableBDPM <- R6::R6Class(
  "Drugs",
  inherit = uiObject,
  
  public = list(
    document = data.frame(),
    documentFound = data.frame(),
    patient_num = numeric(),
    drugsCount = data.frame(),
    toUI = character(), ## html to print
    selectizeDrugs = SelectizeDrugs,
    observerSelectizeDrug = NULL,
    lemmaTerms = NULL,
    mapLemmaCTstring = NULL,
    
    initialize = function(patient_num, document, lemmaTerms, mapLemmaCTstring,parentId, where){
      super$initialize(parentId = parentId, where = where)
      self$document <- document
      private$setPatientNum()
      self$lemmaTerms <- lemmaTerms
      self$mapLemmaCTstring <- mapLemmaCTstring
      # self$lemmaTerms <- private$getLemmaCount(patient_num = self$patient_num)
      staticLogger$info("Nombre de lignes lemmaTerms : ", nrow(self$lemmaTerms))
      # self$drugsCount <- private$getDrugsCount()
      self$selectizeDrugs <- SelectizeDrugs$new(parentId = self$getDivDrugsInDocument(), 
                                                where = "beforeEnd") ## before first child
      self$addModifyClickEvent()
    },
    
    insertUIdrugs = function(){
      ui <- self$getUI()
      jQuerySelector = paste0("#",self$parentId)
      insertUI(selector = jQuerySelector,
               where = self$where,
               ui = ui,
               immediate = T)
      self$selectizeDrugs$renderSelectizeUI()
      # choices <- unique(self$drugsCount$label)
      # load("lemmaTermCountPatient1.rdata")
      choices <- unique(as.character(self$lemmaTerms$lemmaTerm))
      # choices <- c("amiante","tabac")
      self$selectizeDrugs$setChoices(choices)
      self$addObserverSelectizeDrug()
      return(NULL)
    },
    
    getUI = function(){
      ui <- div (
        #h3("Moteur de recherche"),
        id = self$getDivDrugsInDocument(), style="font-size: 1.5em;", ## to see below !
        div(id = self$getDivHTML())
      )
      return(ui)
    },
    
    getDivDrugsInDocument = function(){
      return(paste0("DrugsInDocument-",self$parentId))
    },
    
    getDivHTML = function(){
      return(paste0("htmlDiv-",self$getDivDrugsInDocument()))
    },
    
    removeHTML = function(){
      staticLogger$info("removing Drugs Div HTML")
      jQuerySelector = paste0("#",self$getDivHTML())
      shiny::removeUI(selector = jQuerySelector,
                      immediate = F)
    },
    
    insertHTML = function(){
      staticLogger$info("adding Drugs Div HTML")
      ui <- div(id = self$getDivHTML(),
                shiny::HTML(self$toUI))
      jQuerySelector = paste0("#",self$getDivDrugsInDocument())
      insertUI(selector = jQuerySelector,
               where = "beforeEnd",
               ui = ui,
               immediate = T)
    },
    
    addObserverSelectizeDrug = function(){
      self$observerSelectizeDrug <- shiny::observeEvent(input[[self$selectizeDrugs$getSelectizeId()]],{
        staticLogger$info("New drug input !!")
        drugsChoice <- input[[self$selectizeDrugs$getSelectizeId()]]
        if (is.null(drugsChoice) || drugsChoice == ""){
          staticLogger$info("drugsChoice is null")
          #self$removeHTML()
          return(NULL)
        }
        self$removeHTML()
        staticLogger$info("drugsChoice :",drugsChoice)
        drugsChoice <- tolower(drugsChoice)
        # self$searchDruginDocument(drug = drugsChoice)
        self$searchTermLemmainDocument(drug = drugsChoice)
        self$insertHTML()
      },ignoreNULL = F)
    },
    
    
    ## set "toUI"
    searchDruginDocument = function(drug){
      ## get the body requet 
      body <- private$getBodyQueryDrug(drug = drug)
      resultJson <- private$sendESquery(body = body)
      resultList <- resultJson$hits$hits$`_source`$drugs
      drugPosition <- lapply(resultList, function(x, drug){
        bool <- tolower(x$normalLabel) == drug
        if (any(bool)){
          return(subset (x, bool))
        } else {
          return(NULL)
        }
      }, drug = drug)
      i <- 1
      document <- self$document
      df <- NULL
      for (i in 1:length(drugPosition)){
        candidateTerms <- drugPosition[[i]] ## many candidateTerms for each one
        if (!is.null(candidateTerms)){
          ### retrieve the document
          provider_id <- resultJson$hits$hits$`_source`$provider_id[i]
          encounter_num <- resultJson$hits$hits$`_source`$encounter_num[i]
          tvalchar <- resultJson$hits$hits$`_source`$tvalchar[i]
          concept_cd <- resultJson$hits$hits$`_source`$concept_cd[i]
          start_date <- resultJson$hits$hits$`_source`$start_date[i]
          bool <- document$START_DATE == start_date & document$PROVIDER_ID == provider_id & 
            document$ENCOUNTER_NUM == encounter_num & document$CONCEPT_CD == concept_cd
          doc <- subset (document, bool)
          if (nrow(doc) == 0){
            stop("depreacted function")
            
          }
          txt <- ifelse(tvalchar, doc$TVAL_CHAR, doc$OBSERVATION_BLOB)
          
          ## make a span for every candidateTermString :
          candidateTermString <- candidateTerms$candidateTermString
          for (candidateTermString in unique(candidateTerms$candidateTermString)){
            htmlTxt <- stringr::str_replace_all(string = txt, 
                                                pattern = candidateTermString,
                                                paste0("<span class=\"detected\">", candidateTermString, "</span>"))
          }
          ## selected only the sentence :
          sentencesHTMLtxt <- strsplit(htmlTxt,split="\n")
          sentences <- lapply(sentencesHTMLtxt, function(x){
            bool <- grepl(pattern = "<span class=\"detected\">",x = x,fixed = T)
            if (any(bool)){
              return(x[bool])
            } else {
              return(NULL)
            }
          })
          sentences <- unlist(sentences)
          ajout <- data.frame(sentences = sentences, provider_id = provider_id,
                              start_date = start_date, htmlTxt = htmlTxt)
          df <- rbind (df,ajout)
        }
      }
      df$start_date <- as.Date(df$start_date)
      df <- df[rev(order(df$start_date)),]
      df$toUI <- paste0("<h4><a href=\"\">",df$start_date, " - ", df$provider_id,
                        "</a><button type=\"button\" class=\"modifyButton\" id=modifyButton_",1:nrow(df),"><i class=\"fas fa-eye\"></i></button>",
                        "</h4>",
                        "<p>",df$sentences,"</p>")
      self$documentFound <- df
      self$toUI <- paste(df$toUI, collapse="\n")
    },
    
    
    searchTermLemmainDocument = function(drug){
      staticLogger$info("new search term : ", drug)
      bool <- self$mapLemmaCTstring$lemmaTerm == drug
      if (!any(bool)){
        staticLogger$error("lemmaTerm not found in dataframe ! ", drug)
        return(NULL)
      }
      temp <- subset (self$mapLemmaCTstring, bool)
      candidateTermStrings <- as.character(temp$candidateTermString)
      ## make a span for every candidateTermString :
      #print(candidateTermString)
      df <- NULL
      
      for (candidateTermString in candidateTermStrings){
        staticLogger$info("Searching with ", candidateTermString)
        candidateTermString <- gsub("\r","",candidateTermString)
        candidateTermString <- gsub("[?]","",candidateTermString)
        candidateTermString <- trimws(candidateTermString)
        bool <- grepl(candidateTermString, as.character(self$document$OBSERVATION_BLOB),ignore.case = T)
        if (!any(bool)){
          staticLogger$error("no document found with ", candidateTermString)
          next
        }
        staticLogger$error("term detected in  ", sum(bool), " documents")
        documentFound <- subset (self$document, bool)
        
        for (i in 1:nrow(documentFound)){
          provider_id <- documentFound$PROVIDER_ID[i]
          start_date <- documentFound$START_DATE[i]
          encounter_num <- documentFound$ENCOUNTER_NUM[i]
          txt <- as.character(documentFound$OBSERVATION_BLOB[i])
          htmlTxt <- stringr::str_replace_all(string = txt, 
                                              pattern = candidateTermString,
                                              paste0("<span class=\"detected\">", candidateTermString, "</span>")) 
          
          ## selected only the sentence :
          sentencesHTMLtxt <- strsplit(htmlTxt,split="\n")
          sentences <- lapply(sentencesHTMLtxt, function(x){
            bool <- grepl(pattern = "<span class=\"detected\">",x = x,fixed = T)
            if (any(bool)){
              return(x[bool])
            } else {
              return(NULL)
            }
          })
          sentences <- unlist(sentences)
          
          tryCatch({
            ajout <- data.frame(sentences = sentences, provider_id = provider_id,
                                start_date = start_date, htmlTxt = htmlTxt,
                                encounter_num = encounter_num)
            df <- rbind (df,ajout)
          },
          error = function(e){
            staticLogger$info("an error occured with candidateTermString : ",candidateTermString)
          })
        }
      }
      
      # print(sentences)
      if (is.null(df)){
        staticLogger$error("no document found at all for ", drug)
        return()
      }
      df$start_date <- as.Date(df$start_date)
      df <- df[rev(order(df$start_date)),]
      df$toUI <- paste0("<h4><a href=javascript:void(0) onclick=seeDocument(this); value=",1:nrow(df),"
                        >",df$start_date, " - ", df$provider_id,
                        "</a>",
                        #<button type=\"button\" class=\"modifyButton\" id=modifyButton_",1:nrow(df),"><i class=\"fas fa-eye\"></i></button>",
                        "</h4>",
                        "<p>",df$sentences,"</p>")
      self$documentFound <- df
      self$toUI <- paste(df$toUI, collapse="\n")
      
      staticLogger$info("Trying to highlight documents")
      staticTimeline$highlightEncounterNum(ENCOUNTER_NUM = df$encounter_num)
    },
    
    addModifyClickEvent = function(){
      observeEvent(input$seeDocument,{
        staticLogger$user("User wants to see a document")
        rowNumber <- as.numeric(input$seeDocument)
        staticLogger$user("User wants to see document number : ", rowNumber)
        line <- self$documentFound[rowNumber,]
        htmlTxt <- as.character(line$htmlTxt)
        
        ### show dialogu
        dialog <- self$getModalDialog(htmlTxt)
        staticLogger$info("Trying to show Modal dialog")
        showModal(dialog)
      })
    },
    
    getModalDialog = function(htmlTxt){
      htmlTxt <- gsub("\n", "<br>",htmlTxt)
      dialog <- modalDialog(
        fluidPage(
          div(id="modalDialog", style='height:500px;',
              shiny::HTML(paste0("<p id ='modalDialogCR'>",htmlTxt,"</p>"))
          )
        ),
        size="l",
        easyClose = T
      )
      return(dialog)
    }
  ),
  
  private = list(
    setPatientNum = function(){
      self$patient_num <-  unique(document$PATIENT_NUM)
      # self$patient_num <- 1
      return(NULL)
    }
  )
)
