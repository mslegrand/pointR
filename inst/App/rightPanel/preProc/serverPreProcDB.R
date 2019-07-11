

preProcDB<-reactiveValues(
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,],
  attrs=tibble(  tabId="bogus", tibName="bogus",  atColName='bogus', cmd="bogus", script='bogus')[0,] #,
  #matrix=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,] # !!! not used????
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
    # cat('>---> oe: preProcDB$points, input$pages, getTibTabId(), getAssetName(), getTibColumnName()\n')
    newVal<-(
      !is.null(preProcDB$points) &&
        !is.null(getTibTabId()) &&
        !is.null(getAssetName()) &&
        !is.null(getTibColumnName()) && 
        nrow(filter(preProcDB$points, tabId==getTibTabId() & tibName==getAssetName() & ptColName== getTibColumnName()))>0
      
    )
    hasPtScript(newVal)
    # cat('<---< oe: preProcDB$points, input$pages, getTibTabId(), getAssetName(), getTibColumnName()\n')
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
  # cat(">---> insertPreProcPtEntry\n")
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
  # cat("<---< insertPreProcPtEntry\n")
}

#' used once by 
#' serverPreProcPts.R
setPreProcPtScript<-function(tab_Id, tib_Name, pt_Column_Name,  cmd_name, newScript){
  # cat(">---> setPreProcPtScript\n")
  preProcDB$points[ 
      preProcDB$points$tabId==tab_Id &
      preProcDB$points$tibName==tib_Name &
      preProcDB$points$ptColName==pt_Column_Name &
      preProcDB$points$cmd==cmd_name  
     ,"script"]<-newScript
  # cat("<---< setPreProcPtScript\n")
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
  # cat(">---> getPreProcPtEntries\n")
  ptpts<-filter(preProcDB$points, tabId==pageId)
  # cat("<---< getPreProcPtEntries\n")
  ptpts
}


# getPreProcPtScript<-function(pageId=getTibTabId(), assetName=getAssetName(), columnName= getTibColumnName() ){
#   if(!is.null( columnName)){
#     x<-filter(preProcDB$points, tabId==pageId & tibName==assetName & ptColName== columnName)
#   } else {
#     x<-filter(preProcDB$points, tabId==pageId & tibName==assetName )
#   }
#   temp<-x$script
#   if(length(temp)>0){
#     names(temp)<-x$cmd
#   }
#   temp
# }

getPreProcOnNewRowScripts<-function(pageId, assetName  ){
  x<-filter(preProcDB$points, tabId==pageId & tibName==assetName & cmd=='onNewRow' )
  
  scripts<-x$script
  if(length( scripts)>0){
    cols<-x$ptColName
    names(scripts)<-cols
  } 
  scripts
}
