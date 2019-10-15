

preProcScriptDB<-eactiveValues(
  points=tibble( scriptName='bogus', cmd="bogus", script='bogus')[0,],
  attrs=tibble( scriptName='bogus', cmd="bogus", script='bogus')[0,]
)

preProcPageDB<-reactiveValues(
  tibble( tabId="bogus", tibName="bogus", colName='bogus', scriptName='bogus')[0,],
)



# todo refactor this: ptColName => colName; eliminate points?
preProcDB<-reactiveValues(
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,]
)

#' used by moduleFooterRight.R 
#' serverFooterRight.R  
#' serverMouseCmdMoveMatrix 
#' serverMouseCmdAddPt.R 
#' serverMouseCmdMovePt.R 
#' serverPanelCoordinator.R
hasPtScript<-reactiveVal(FALSE, label='hasPtScript' )

#' must rewrite 
#' to use columnName instead of getTibColumnName
observeEvent(c(preProcDB$points, input$pages, getTibTabId(), getAssetName(), getTibColumnName()),{
  if(!is.null(input$pages) && identical( getTibTabId(), input$pages )){
    newVal<-(
      !is.null(preProcDB$points) &&
        !is.null(getTibTabId()) &&
        !is.null(getAssetName()) &&
        !is.null(getTibColumnName()) && 
        nrow(filter(preProcDB$points, tabId==getTibTabId() & tibName==getAssetName() & ptColName== getTibColumnName()))>0
      
    )
    hasPtScript(newVal)
  }
})

#' used solely by in 
#' cmdRemovePreProc.R
removePreProcPtEntry<-function(tab_Id, tib_Name, pt_Column_Name, cmdNames){
  temp1<-filter(
    preProcDB$points, 
    !(tabId==tab_Id & tibName==tib_Name &  ptColName==pt_Column_Name) | !(cmd %in% cmdNames)          
  ) 
  preProcDB$points<-temp1
}

#' used in 
#' cmdFileImportPreProc.R, 
#' serverPlotBar.R
insertPreProcPtEntry<-function(tab_Id, tib_Name, pt_Column_Name, newScript){
  # todo addd tests for newScript (is character...)
  # log.fin( insertPreProcPtEntry)
  temp2<-tibble( 
    tabId=rep(tab_Id,length(newScript)), 
    tibName=rep(tib_Name, length(newScript)), 
    ptColName=rep(pt_Column_Name,length(newScript)), 
    cmd=names(newScript), 
    script=newScript            
  )
  temp1<-filter(preProcDB$points, 
    !(
      tabId==tab_Id & 
      tibName==tib_Name &  
      ptColName==pt_Column_Name & 
      cmd %in% names(newScript)
    )           
  )
  preProcDB$points<-bind_rows( temp1, temp2)
  serverAssetDB$ptScriptSel=names(newScript)[1]
  # possibly we should save page?
  savePage(pageId=tab_Id )
  # log.fout( insertPreProcPtEntry)
}

#' used once by 
#' serverPreProcPts.R
setPreProcPtScript<-function(tab_Id, tib_Name, pt_Column_Name,  cmd_name, newScript){
  # log.fin( setPreProcPtScript)
  preProcDB$points[ 
      preProcDB$points$tabId==tab_Id &
      preProcDB$points$tibName==tib_Name &
      preProcDB$points$ptColName==pt_Column_Name &
      preProcDB$points$cmd==cmd_name  
     ,"script"]<-newScript
  # log.fout( setPreProcPtScript)
}


#' used by
#' serverMouseCmdMoveMatrix
#' serverMouseCmdAddPt.R
#' serverMouseCmdMovePt.R
#' cmdFileExportPreProc.R
#' serverPreProcPts.R
getPreProcPtScript<-reactive({
  x<-filter(preProcDB$points, tabId==getTibTabId() & tibName==getAssetName() & ptColName== getTibColumnName())
  temp<-x$script
  if(length(temp)>0){
    names(temp)<-x$cmd
  }
  temp
})

#' used by 
#' serverPage2Workspace.R
getPreProcPtEntries<-function(pageId){
  tibColNames<-getTibColumnNameChoices()
  ptpts<-filter(preProcDB$points, tabId==pageId & ptColName %in% tibColNames)
  ptpts
}

getPreProcOnNewRowScripts<-function(pageId, assetName  ){
  tibColNames<-getTibColumnNameChoices()
  x<-filter(preProcDB$points, tabId==pageId & tibName==assetName & cmd=='onNewRow' & ptColName %in% tibColNames)
  scripts<-x$script
  if(length( scripts)>0){
    cols<-x$ptColName
    names(scripts)<-cols
  } 
  scripts
}
