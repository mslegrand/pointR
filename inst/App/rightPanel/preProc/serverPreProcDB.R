
preProcDB<-reactiveValues(
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,],
  matrix=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,]
)

hasPtScript<-reactive({
  x1<-filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName())
  x2<-filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName(), ptColName== getTibColumnName())
  if(nrow(x1)>0){
    cat("number of rows x1= ",nrow(x1),"\n")
    cat('cmd x1 are', paste(x1$cmd, collapse=", "), "\n")
    cat("number of rows x2= ",nrow(x2),"\n")
    cat('cmd x2 are', paste(x2$cmd, collapse=", "), "\n")
  } else {
    cat(
      'nrow(x1) in zero\n',
      'tabId=',format(getTibTabId()), " ",
      'tibName=', format(getAssetName()),"\n"
    )
  }
  
  nrow(filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName()))>0
})

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
 cat("=======setPreProcPtScript:: entering ===\n")
  temp<-preProcDB$points
  temp[ 
      preProcDB$points$tabId==tab_Id &
      preProcDB$points$tibName==tib_Name &
      preProcDB$points$ptColName==pt_Column_Name &
      preProcDB$points$cmd==cmd_name  
     ,"script"]<-newScript
  preProcDB$points<-temp
  cat( "setPreProcPtScript:: values given to set:\n",
    "tab_Id=", tab_Id, ", ",
    "tib_Name=", tib_Name, ", ",
    "pt_Column_Name=", pt_Column_Name, ", ",
    "cmd_name=", cmd_name, "\n ",
    "newScript=\n", newScript, "\n "
    )
  # cat("echo preProcDB$points$script=")
  # cat(paste(preProcDB$points[ 
  #   preProcDB$points$tabId==tab_Id &&
  #     preProcDB$points$tibName==tib_Name &&
  #     preProcDB$points$ptColName==pt_Column_Name &&
  #     preProcDB$points$cmd==cmd_name  
  #   ,]$script, collapse=", "))
  cat("setPreProcPtScript:: result after setting:\n")
  print(preProcDB$points)
  cat("===========setPreProcPtScript:: exiting==========\n")
}

getPreProcPtScript<-reactive({
  tab_Id=getTibTabId()
  tib_Name= getAssetName()
  pt_Column_Name= getTibColumnName()
  x<-filter(preProcDB$points, tabId==getTibTabId() && tibName==getAssetName(), ptColName== getTibColumnName())
  temp<-x$script
  # temp<-preProcDB$points[ 
  #   (
  #     preProcDB$points$tabId==tab_Id &&
  #       preProcDB$points$tibName==tib_Name &&
  #       preProcDB$points$ptColName==pt_Column_Name &&
  #       preProcDB$points$cmd==selectedAsset$ptScriptSel  
  #   ),]$script
  if(length(temp)==3){
    names(temp)<-c('onNewPt', 'onMovePt', 'onDeletePt')
  }
  cat("getPreProcPtScript:: return\n")
  print(temp)
  temp
})

# removePreProcPtEntry<-function(tabId, tibName, ptColName){
#   tibble( tabId="bogus", 
#           tibName="bogus", 
#           ptColName='bogus', 
#           cmd="bogus", 
#           script='bogus'
#   )
# }


