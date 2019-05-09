
# !!! todo: either
#   1. add staging area for points, 
#      and  then refactor so that each script text is 
#      retained between tabs and all are save upon commit
#   or
#   2. replace radio with tab panels, each containing it's own 
#      instance of ace
#   Note: 2 retains ace undo between choices, but 1 does not.
preProcDB<-reactiveValues(
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,],
  matrix=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,] # !!! not used????
)

hasPtScript<-reactiveVal(FALSE, label='hasPtScript' )


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


removePreProcPtEntry<-function(tab_Id, tib_Name, pt_Column_Name){
  # cat(">---> removePreProPtEntry\n")
  temp1<-filter(
    preProcDB$points, 
    !(tabId==tab_Id & tibName==tib_Name &  ptColName==pt_Column_Name)           
  ) 
  # cat("<---< removePreProPtEntry\n")
  preProcDB$points<-temp1
}

insertPreProcPtEntry<-function(
  tab_Id, tib_Name, pt_Column_Name, 
  newScript = c(
    onNewPt=fileTemplates[['newPtTemplate.R']],
    onMovePt=fileTemplates[['movePtTemplate.R']],
    onMoveMat=fileTemplates[['moveMatTemplate.R']]  
  ) 
  ){
  
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



getPreProcPtScript<-reactive({
  tab_Id=getTibTabId()
  tib_Name= getAssetName()
  pt_Column_Name= getTibColumnName()
  x<-filter(preProcDB$points, tabId==getTibTabId() & tibName==getAssetName() & ptColName== getTibColumnName())
  temp<-x$script
  if(length(temp)==3){
    names(temp)<-x$cmd
  }
  temp
})

getPreProcPtEntries<-function(pageId){
  # cat(">---> getPreProcPtEntries\n")
  ptpts<-filter(preProcDB$points, tabId==pageId)
  # cat("<---< getPreProcPtEntries\n")
  ptpts
}


