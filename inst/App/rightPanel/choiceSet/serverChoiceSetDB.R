

choiceSetPageDB<-reactiveVal(
  tibble( tabId="bogus", tibName="bogus", colName='bogus', choiceSetName='bogus')[0,]
)

observeEvent(choiceSetPageDB(),{
  if(nrow(  choiceSetPageDB()  )>0){
    enableDMDM(session, 'plotNavBar','Edit Choices')
  } else{
    disableDMDM(session, 'plotNavBar',"Edit Choices")
  }
})

getChoiceSetPage<-function(pageId){
    db<-choiceSetPageDB()
    filter(db, tabId==pageId )
}

getChoiceSet4PageName<-function(tab_Id, tib_Name, column_Name){
  if(any(sapply(c(tab_Id, tib_Name, column_Name), is.null))
     || tab_Id=='bogus')
  {
    return( NULL) 
  }
  csDB<-choiceSetPageDB()
  csDB<-filter(csDB, 
               tabId==tab_Id &tibName==tib_Name & colName==column_Name 
  )
  if(nrow(csDB)>0){
    csDB$choiceSetName
  } else {
    NULL
  }
}

setChoiceSet4PageName<-function(tab_Id, tib_Name, column_Name,  choiceSet_Name){
  csDB<-choiceSetPageDB()
  csDB<-filter(csDB, !(tabId==tab_Id &
                         tibName==tib_Name & 
                         colName==column_Name )
  )
  if(!is.null(choiceSet_Name)){
    csDB<-rbind(csDB, tibble(tabId=tab_Id, tibName=tib_Name, colName=column_Name, choiceSetName=choiceSet_Name))
  }
  choiceSetPageDB(csDB)
}