
preProcDB<-reactiveValues(
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,],
  matrix=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,]
)

hasPtScript<-reactiveVal(FALSE, label='hasPtScript' )


observeEvent(c(preProcDB$points, input$pages, getTibTabId(), getAssetName(), getTibColumnName()),{
  if(!is.null(input$pages) && identical( getTibTabId(), input$pages )){
    newVal<-nrow(filter(preProcDB$points, tabId==getTibTabId() & tibName==getAssetName() & ptColName== getTibColumnName()))>0
    hasPtScript(newVal)
  }
})


removePreProcPtEntry<-function(tab_Id, tib_Name, pt_Column_Name){
  temp1<-filter(
    preProcDB$points, 
    !(tabId==tab_Id & tibName==tib_Name &  ptColName==pt_Column_Name)           
  )  
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
}

setPreProcPtScript<-function(tab_Id, tib_Name, pt_Column_Name,  cmd_name, newScript){
  preProcDB$points[ 
      preProcDB$points$tabId==tab_Id &
      preProcDB$points$tibName==tib_Name &
      preProcDB$points$ptColName==pt_Column_Name &
      preProcDB$points$cmd==cmd_name  
     ,"script"]<-newScript
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
  # cat("getPreProcPtEntries::pageId=",pageId,"\n")
  # cat("nrow(preProcDB$points)=",nrow(preProcDB$points),"\n")
  # print(preProcDB$points)
  ptpts<-filter(preProcDB$points, tabId==pageId)
  #print(ptpts)
  ptpts
}


