

colSetPageDB<-reactiveVal(
  tibble( tabId="bogus", tibName="bogus", colName='bogus', colSetName='bogus')[0,]
)

getColSetPage<-function(pageId){
    db<-colSetPageDB()
    filter(db, tabId==pageId )
}

getColSet4PageName<-function(tab_Id, tib_Name, column_Name){
  if(any(sapply(c(tab_Id, tib_Name, column_Name), is.null))
     || tab_Id=='bogus')
  {
    return( "none") 
  }
  csDB<-colSetPageDB()
  csDB<-filter(csDB, 
               tabId==tab_Id &tibName==tib_Name & colName==column_Name 
  )
  if(nrow(csDB)>0){
    csDB$colSetName
  } else {
    NULL
  }
}

setColSet4PageName<-function(tab_Id, tib_Name, column_Name,  colSet_Name){
  csDB<-colSetPageDB()
  csDB<-filter(csDB, !(tabId==tab_Id &
                         tibName==tib_Name & 
                         colName==column_Name )
  )
  if(!is.null(colSet_Name)){
    csDB<-rbind(csDB, tibble(tabId=tab_Id, tibName=tib_Name, colName=column_Name, colSetName=colSet_Name))
  }
  colSetPageDB(csDB)
}