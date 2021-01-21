
preProcPageDB<-reactiveVal(
  tibble( tabId="bogus", tibName="bogus", colName='bogus', scriptName='bogus')[0,]
)


getPreProcScript<-reactive({
  script_Name<-getPreProcScriptName(
    tab_Id=getTibTabId(), tib_Name=getAssetName(),column_Name= getTibColumnName()
  )
  if(!is.null(script_Name) && script_Name!='none'){
    if(getColumnType()=='point'){
      tb<-filter(preProcScriptDB$points, scriptName==script_Name)
    } else {
      tb<-filter(preProcScriptDB$attrs, scriptName==script_Name)
    }
    scripts<-unlist(tb$script)
    names(scripts)<-tb$cmd
  } else {
    scripts<-NULL
  }
  scripts
})

extractPreProcScript<-function(tab_Id, tib_Name, column_Name){
  script_Name<-getPreProcScriptName(
    tab_Id, tib_Name,column_Name
  )
  if(length(script_Name)>0 && script_Name!='none'){
    tibs<-getPtDefs()$tib
    col<-tibs[[tib_Name]][[column_Name]]
    ctype<-extractColType(col)
    if(ctype=='point'){
      tb<-filter(preProcScriptDB$points, scriptName==script_Name)
    } else {
      tb<-filter(preProcScriptDB$attrs, scriptName==script_Name)
    }
    scripts<-unlist(tb$script)
    names(scripts)<-tb$cmd
  } else {
    scripts<-NULL
  }
  scripts
}


getPreProcOnNewRowScripts<-function(pageId, assetName  ){
  pgDB<-preProcPageDB()
  x<-filter(pgDB, tabId==pageId & tibName==assetName)
  if(nrow(x)==0){
    return(NULL)
  }
  # select colName, scriptName
  
  y<-filter(preProcScriptDB$attrs ,cmd=='onNewRow')
  if(nrow(y)==0){
    return(NULL)
  }
  z<-inner_join(x, y, by='scriptName')
  scripts<-z$script
  names(scripts)<-z$colName
  scripts
}

setPreProcScriptName<-function(tab_Id, tib_Name, column_Name,  script_Name){
  # log.fin( setPreProcScriptName)
  # filter  on tab_Id, tib_Name, pt_Column_Name
  ppDB<-preProcPageDB()
  ppDB<-filter(ppDB, !(tabId==tab_Id &
           tibName==tib_Name & 
           colName==column_Name )
  )
  if(script_Name!='none'){
      ppDB<-rbind(ppDB, tibble(tabId=tab_Id, tibName=tib_Name, colName=column_Name, scriptName=script_Name))
  }
  preProcPageDB(ppDB)
  # log.fout( setPreProcScriptName )
}

getPreProcScriptName<-function(tab_Id, tib_Name, column_Name){
  if(any(sapply(c(tab_Id, tib_Name, column_Name), is.null))
     || tab_Id=='bogus')
  {
    return( NULL) 
  }
  ppDB<-preProcPageDB()
    ppDB<-filter(ppDB, 
               tabId==tab_Id &tibName==tib_Name & colName==column_Name 
  )
  if(nrow(ppDB)>0){
    ppDB$scriptName
  } else {
    NULL
  }
}

getPagePreprocPageDB<-function(pageId){
  if(!is.null(pageId)){
    ppDB<-preProcPageDB()
    rtv<-filter(ppDB,tabId==pageId)
  } else {
    rtv<-NULL
  }
  rtv
}

