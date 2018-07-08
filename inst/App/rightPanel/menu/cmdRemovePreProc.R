
cmdPreProcPtsRemove<-function(){
  removePreProcPtEntry(
    tab_Id=getTibTabId(), 
    tib_Name=getAssetName(),
    pt_Column_Name=getTibColumnName()
  )
}




