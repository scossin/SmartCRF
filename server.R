library(dplyr)
library(shiny)
library(timevis)
library(DT)
library(R6)
library(RColorBrewer)
library(xts)
library(plotly)
library(ggplot2)
library(stringr)
library(sunburstR)
library(viridis)
library(wordcloud2)
# writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
# source("global.R")

server <- function(session,input, output) {
  
  ## logging in development
  source("classes/logger/STATICLoggerDevOO.R",local = T)
  staticLogger <- STATICloggerDev$new()
  
  ## logging in production 
  source("classes/logger/STATICLoggerOO.R",local = T)
  staticLogger <- STATIClogger$new()
  
  ## closing logger connection when user disconnect
  session$onSessionEnded(function() {
    staticLogger$close()
  })
  
  source("classes/uiObject.R",local = T)
  
  source("classes/pmsi/TableDisplay.R", local = T) ## div controller
  
  source("classes/pmsi/PMSIOO.R",local=T)
  
  source("classes/util/UtilOO.R",local = T)
  
  source("classes/timelines/TimelineOO.R",local = T)
  
  source("classes/timelines/UIdisplay.R", local = T)
  
  source("classes/form/FormDisplayOO.R",local = T)
  
  source("classes/form/FormAddTimelineOO.R", local = T)
  
  
  source("classes/sunburst/FilterHierarchicalOO.R", local = T)
  
  source("classes/form/FormAddTimelinePMSIOO.R", local = T)
  
  source("classes/selectize/selectizeOO.R", local = T) ## div controller
  
  load("hierarchy.rdata")
  
  load("fake/pmsiJeanne.rdata")
  
  pmsiJeanne$SOURCESYSTEM_CD <- as.factor(pmsiJeanne$MODIFIER_CD)
  table(pmsiJeanne$SOURCESYSTEM_CD)
  
  staticUtil <- Util$new()
  
  pmsi <- PMSI$new(pmsi = pmsiJeanne)
  
  staticTimeline <- Timeline$new(parentId = "timelineQ",
                                 where = "afterBegin")
  
  staticTimeline$insertUItimeline()
  staticTimeline$plotTimeline()
  
  selectizeDiv <- Selectize$new(parentId = GLOBALlayerControl, 
                                where = "beforeEnd")
  selectizeDiv$renderSelectizeUI()
  
  ### biologie : 
  source("classes/form/FormDisplayBiologie.R", local = T)
  source("classes/controllers/ControllerBiologieOO2.R", local = T) 
  load("fake/biologieJeanne.rdata")
  biologieJeanne$START_DATE <-  as.POSIXct(biologieJeanne$START_DATE)
  controllerBiologie <-  ControllerBiologie2$new(dfBiologie = biologieJeanne)
  
  #### document :
  source("classes/form/TextDisplayOO.R", local = T)
  load("fake/documentJeanne.rdata")
  
  # documentJeanne$OBSERVATION_BLOB[3]
  documentJeanne$OBSERVATION_BLOB <- iconv(  documentJeanne$OBSERVATION_BLOB,from = "ISO8859-1",to = "UTF-8")
  Encoding(documentJeanne$OBSERVATION_BLOB) <- "UTF-8"
  
  documentJeanne$START_DATE <-  as.POSIXct(documentJeanne$START_DATE)
  source("classes/controllers/ControllerDocOO.R",local=T)
  mapIconConcept <- data.frame(NAME_CHAR = c("Compte-rendu hospitalisation",
                                             "Compte-rendu consultation",
                                             "Ordonnances",
                                             "Compte-rendu d'intervention",
                                             "RCP"),
                               HTML_ICON = c(
                                 "<img src=\"hostpital-building.png\">",      
                                 "<i class=\"fas fa-user-md fa-lg\"></i>",
                                 "<img src=\"prescription.png\">",    
                                 "<img src=\"surgeon.png\">", 
                                 "<i class=\"fas fa-users fa-lg\"></i>")
                               
  )
  document <- merge (documentJeanne, mapIconConcept, by.x="NAME_CHAR",all.x = T)
  bool <- is.na(document$HTML_ICON)
  sum(bool)
  document$HTML_ICON <- as.character(document$HTML_ICON)
  document$HTML_ICON[bool] <- "<i class=\"fas fa-file\"></i>"
  controllerDoc <- ControllerDoc$new(dfDocument = document)
  
  ## drugs (moteur de recherche) : 
  source("classes/drugs/SelectizeDrugsOO.R", local =T)
  source("classes/drugs/DrugsOO.R", local =T)
  
  load("fake/lemmaTerm.rdata")
  lemmaTerm <- lemmaTerm[order(-lemmaTerm$count),]
  load("fake/mapLemmaCTstring.rdata")
  drugs <- Drugs$new(patient_num = patient_num,document = document, lemmaTerms = lemmaTerm,
                     mapLemmaCTstring = mapLemmaCTstring,
                     parentId = GLOBALdrugsDiv, where = "beforeEnd"
  )
  drugs$insertUIdrugs()
  
  #### add TimeVis : 
  timevisDfPMSI <- pmsi$getPMSITimevisDf()
  
  staticTimeline$addTimevisDfdata(timevisDfPMSI)
  
  timevisDfBIOLOGIE <- controllerBiologie$getBIOLOGIETimevisDf()
  
  staticTimeline$addTimevisDfdata(timevisDfBIOLOGIE)
  
  timevisDfDOC <- controllerDoc$getDOCTimevisDf()
  timevisDfDOC$group <- as.character(timevisDfDOC$group)
  bool <- timevisDfDOC$NAME_CHAR == "Compte-rendu hospitalisation"
  sum(bool)
  timevisDfDOC$group[bool] <-  paste0(timevisDfDOC$group[bool], "--CRH")
  
  bool <- timevisDfDOC$NAME_CHAR == "Compte-rendu consultation"
  timevisDfDOC$group[bool] <-  paste0(timevisDfDOC$group[bool], "--CRC")
  
  bool <- timevisDfDOC$NAME_CHAR == "Ordonnances"
  timevisDfDOC$group[bool] <-  paste0(timevisDfDOC$group[bool], "--Ordo")
  
  tab <- table(timevisDfDOC$group)
  for (groupName in names(tab)){
    temp <- subset (timevisDfDOC, group == groupName)
    staticTimeline$addTimevisDfdata(temp)
  }
  
  ### questionnaires : 
  load("fake/questionnairesJeanne.rdata")
  ### TVAL : NA only ! for PathologiesAuto-immunesV2
  # voir <- subset (questionnaires,questionnaire == "PathologiesAuto-immunesV2")
  source("classes/questionnaires/QuestionnairesOO.R",local=T)
  questionnairesJeanne$questionnaire <- as.character(questionnairesJeanne$questionnaire)
  questionnairesInstance <- Questionnaires$new(questionnaires = questionnairesJeanne)
  timevisDfquestionnaires <- questionnairesInstance$getQuestionnairesTimevisDf()
  staticTimeline$addTimevisDfdata(timevisDfquestionnaires)
  
  
  ###### box to save annotation : 
  source("classes/savedata/savedataOO.R", local=T)
  saveData <- SaveData$new(staticUtil$getDivModalMediumId(),
                           where = "beforeEnd",
                           variableName = "Statut tabagique",
                           variableChoices = c("Fumeur actif",
                                               "Fumeur sevré",
                                               "Non Fumeur"))
  
  
  load("fake/wordCloudDisordersJeanne.rdata")
  colnames(wordCloudDisordersJeanne) <- c("word","freq")
  
  output[["wordcloud"]] <- wordcloud2::renderWordcloud2(
    wordcloud2(data = wordCloudDisordersJeanne,size=0.6))
  
  # output[["wordcloudAnatomy"]] <- wordcloud2::renderWordcloud2(
  #   wordcloud2(data = anatomyWord,size=0.30))
  
  observeEvent(input[["wordcloudclicked"]],{
    wordFreq <- input[["wordcloudclicked"]]
    word <- unlist(strsplit(x = wordFreq,split = ":"))[1]
    drugs$selectizeDrugs$setSelected(selected=word)
  })
}
