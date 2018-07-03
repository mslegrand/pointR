
preProcDB<-reactiveValues(
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,],
  matrix=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,]
)


newPreProcPtEntry<-function(tab_Id, tib_Name, pt_Column_Name){
  temp<-tibble( 
    tabId=c(tab_Id, tab_Id, tab_Id), 
    tibName=c(tib_Name, tib_Name, tib_Name), 
    ptColName=c(pt_Column_Name,pt_Column_Name,pt_Column_Name), 
    cmd=c('onNewPt', 'onMovePt', 'onDeletePt'), 
    script=c(
      fileTemplates[['newPtTemplate.R']],
      fileTemplates[['movePtTemplate.R']],
      fileTemplates[['deletePtTemplate.R']]            
    )
  )
  preProcDB$points<-bind_rows( preProcDB$points, temp)
}

setPreProcPtScript<-function(tab_Id, tib_Name, pt_Column_Name,  cmd_name, newScript){

  preProcDB$points[ 
    (
      preProcDB$points$tabId==tab_Id &&
      preProcDB$points$tibName==tib_Name &&
      preProcDB$points$ptColName==pt_Column_Name &&
      preProcDB$points$cmd==cmd_name  
     ),]$script=newScript
}

getPreProcPtScript<-reactive({
  tab_Id=getTibTabId()
  tib_Name= getAssetName()
  pt_Column_Name= getTibColumnName()
  
  preProcDB$points[ 
    (
      preProcDB$points$tabId==tab_Id &&
        preProcDB$points$tibName==tib_Name &&
        preProcDB$points$ptColName==pt_Column_Name &&
        preProcDB$points$cmd==selectedAsset$ptScriptSel  
    ),]$script
})

# removePreProcPtEntry<-function(tabId, tibName, ptColName){
#   tibble( tabId="bogus", 
#           tibName="bogus", 
#           ptColName='bogus', 
#           cmd="bogus", 
#           script='bogus'
#   )
# }


