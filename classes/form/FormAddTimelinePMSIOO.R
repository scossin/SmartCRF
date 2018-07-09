FormAddTimelinePMSI <- R6::R6Class(
  "FormAddTimelinePMSI",
  inherit=FormAddTimeline,
  
  public = list(
    formDP = data.frame(),
    formDAS = data.frame(),
    form = data.frame(),
    rowsSelected = numeric(),
    rowsSelectedDAS = numeric(),
    groupName = character(),
    filterHierarchicalDP = FilterHierarchical,
    filterHierarchicalDAS = FilterHierarchical,
    
    initialize = function(parentId, where, groupName){
      staticLogger$info("Initializing new FormAddTimeline PMSI")
      super$initialize(parentId = parentId, where = where, groupName)
      self$filterHierarchicalDP <- FilterHierarchical$new(parentId = self$getDivDPId(),
                                                        where = "beforeEnd",
                                                        hierarchy = hierarchy)
      
      self$filterHierarchicalDAS <- FilterHierarchical$new(parentId = self$getDivDASId(),
                                                        where = "beforeEnd",
                                                        hierarchy = hierarchy)
      
      ### don't insert it initially !
      # self$insertUIformTimeline()
    },
    
    insertUIformTimeline = function(){
      jQuerySelector = paste0("#",self$parentId)
      insertUI(selector = jQuerySelector,
               where = self$where,
               ui = self$getUI())
    },
    
    setForm = function(form){
      self$form <- form
      ### DP : 
      pmsiDP <- subset (form, SOURCESYSTEM_CD %in% c("PMSI--DP","PMSI--DR"))
      self$formDP <- pmsiDP
      colnames(self$formDP) <- c("code","source","libelle","n","last","first")
      

      ### DP sunburst :
      pmsiDP <- subset (pmsiDP,select=c(code,n))
      pmsiDP$letter <- substr(pmsiDP$code, 1,1)
      bool <- pmsiDP$letter == "Z"
      pmsiDP <- subset (pmsiDP, !bool)
      pmsiDP$letter <- NULL
      colnames(pmsiDP) <- c("className","count")
      pmsiDP$className <- gsub("[.]","",pmsiDP$className)
      self$filterHierarchicalDP$setEventCount(pmsiDP)
      
      ### DAS : 
      pmsiDAS <- subset (form, SOURCESYSTEM_CD == c("PMSI--DAS"))
      self$formDAS <- pmsiDAS
      colnames(self$formDAS) <- c("code","source","libelle","n","last","first")
      self$formDAS$source <- NULL
      
      ### DAS sunburst :
      pmsiDAS <- subset (pmsiDAS,select=c(code,n))
      colnames(pmsiDAS) <- c("className","count")
      pmsiDAS$className <- gsub("[.]","",pmsiDAS$className)
      self$filterHierarchicalDAS$setEventCount(pmsiDAS)
    },
    
    getUI = function(){
      sunburstUIDP <- self$filterHierarchicalDP$getUIsunburst()
      sunburstUIDAS <- self$filterHierarchicalDAS$getUIsunburst()
      ui <- div(id = self$getDivId(),
                # style = "display:flex;",
                div(id = self$getDivDPId(),
                    shiny::h2("DP et DR"),
                    sunburstUIDP,
                    # style = "display:0 0 40%;",
                    style="display:inline-block;width:40%;float:left;",
                  DT::dataTableOutput(self$getFormDPId())
                  ),
                div(id = self$getDivDASId(),
                    shiny::h2("DAS"),
                    sunburstUIDAS,
                    style="width:40%;float:left; margin-left:10%;",
                    # style = "display:0 0 40%; margin-left:10px",
                    DT::dataTableOutput(self$getFormDASId())
                    )
      )
      return(ui)
    },
    
    getLineDP = function(rowRemovedAdded){
      line <- self$formDP[rowRemovedAdded,]
      staticLogger$info("code selected :", line$code)
      return(line)
    },
    
    getLineDAS = function(rowRemovedAdded){
      line <- self$formDAS[rowRemovedAdded,]
      staticLogger$info("code selected :", line$code)
      return(line)
    },
    
    
    renderUI = function(){
      output[[self$getFormDPId()]] <- DT::renderDataTable({
        DT::datatable(self$formDP, rownames= FALSE, caption=self$formDate,
                      options=list(
                        pageLength = 5,
                        lengthMenu = c(5,10,15,20,30,50,100)
                      ))
      })
      output[[self$getFormDASId()]] <- DT::renderDataTable({
        DT::datatable(self$formDAS, rownames= FALSE, caption=self$formDate,
                      options=list(
                        pageLength = 5,
                        lengthMenu = c(5,10,15,20,30,50,100)
                      ))
      })
      self$filterHierarchicalDP$makePlot()
      self$filterHierarchicalDAS$makePlot()
    },
    
    destroy = function(){
      staticLogger$info("Destroying form NOT IMPLEMENTED")
    },
    
    
    getDivId = function(){
      return(paste0("PMSI-",self$parentId))
    },
    
    getDivDPId = function(){
      return(paste0("divDP-",self$parentId))
    },
    
    getDivDASId = function(){
      return(paste0("divDAS-",self$parentId))
    },
    
    getFormDPId = function(){
      return(paste0("FormDP-",self$getDivDPId()))
    },
    
    getFormDASId = function(){
      return(paste0("FormDAS-",self$getDivDASId()))
    }
  ),
  
  
  private = list(
    
  )
)