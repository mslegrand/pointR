
cmdPreProcPtsRemove<-function(pptype){
  cmdNames<-preprocChoices[[pptype]]
  removePreProcPtEntry(
    tab_Id=getTibTabId(), 
    tib_Name=getAssetName(),
    pt_Column_Name=getTibColumnName(),
    cmdNames=cmdNames
  )
}




