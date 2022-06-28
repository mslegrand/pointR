cmdCustColumnEdit<-function( custColumnName){
  showModal( 
    modalCustColumnEditor(custColumnName=custColumnName) 
  )
}

# todo


modalCustColumnEditor <- function(  custColumnName, value="TRUE\nFALSE"  ) {
  #CustColumnModalType(type)
  modalDialog(
    div( id='custColumnBackPanelModalEdit', class='backPanel', style='padding:20px;',
         textInput("modalCustColumnName", 
                   label=span(style='color: #00ffff;', 'Name'),  
                   value=custColumnName, 
                   width='100%', 
                   placeholder = 'Enter a name with at least 6 characters for this set of choices '
         ),
         div(
           aceEditor(
             outputId="customColumnChoiceTxt",
             height = "380px",
             mode='r',
             value=value
           )
         )
    ),
    title="CustColumn Editor",
    easyClose = TRUE,
    footer = tagList(
      modalButton("Dismiss"),
      actionButton("modalCustColumnEditorCommitOk", "Commit")
    )
  )
}

observeEvent(input$modalCustColumnName,{
  CustColumnName<-input$modalCustColumnName
  #if(length(CustColumnName)==0 || nchar(CustColumnName)<6 || CustColumnName %in% names(allWidgetChoices)){
  if(!goodRName(CustColumnName,5) || CustColumnName %in% allWidgetNames){
      hideElement("modalCustColumnEditorCommitOk")
  } else {
    showElement("modalCustColumnEditorCommitOk")
  }
})

getAuxChoicesPath<-reactive({file.path(getDirPath(),'aux','choices')})

writeAuxCustColumnList<-function(filePath, customColumnChoiceList){
  cat(customColumnChoiceList, file=filePath)
}

readAuxChoices<-function(filePath){
  files<-dir(getAuxChoicesPath(), pattern="*\\.txt$",full.names=TRUE )
  customChoiceList<-list()
  customChoiceList<-lapply(files, function(f){
    scan(file=f, what=character(), quiet=TRUE, sep="\n")
  })
  nms<-gsub('\\.txt','', basename(files))
  names(customChoiceList)<-nms
  aux$colChoiceSet<-customChoiceList
}




observeEvent( input$modalCustColumnEditorCommitOk,{
  
  CustColumnName<-input$modalCustColumnName
  CustColumnName<-sub('\\.txt$','',CustColumnName)
  CustColumnName<-paste0(CustColumnName,'.txt')
  
  customColumnChoiceList<-input$customColumnChoiceTxt
  if(!dir.exists(getAuxChoicesPath())){
    dir.create(getAuxChoicesPath() )
  }
  filePath<-file.path(getAuxChoicesPath(), CustColumnName)
  writeAuxCustColumnList(filePath, customColumnChoiceList)
  readAuxChoices()
  removeModal()
})

aux<-reactiveValues(colChoiceSet=list())

getChoiceSetElements<-function(name){
  rtv<-NULL
  if(length(name)==1){
    cs<-aux$colChoiceSet
    rtv<- cs[[name]]
  } 
  rtv
}


observeEvent(aux$colChoiceSet,{
  # reset submenu for dropDown-cmdEditColumnChoices
  if(length(aux$colChoiceSet )==0){
    removeDMDM(session, 'plotNavBar','Edit Choices')
  } else{
    kids<-lapply(names(aux$colChoiceSet), function(nn){
      shinyDMDMenu::menuItem(nn, value=paste0('editChoiceSet-',nn))
    })
    if(length(kids)>0){
      
      afterEntry='cmdNewColumnChoices'
      label=paste0('Edit Choices')
      shinyDMDMenu::removeDMDM(session, "plotNavBar", 'Edit Choices')
      shinyDMDMenu::insertAfterDMDM(
        session, 
        menuBarId  ="plotNavBar",  
        entry=afterEntry,
        submenu=
          do.call(
            function(...){ subMenuDropdown( label,...) },
            kids
          )
      )
      enableDMDM(session, 'plotNavBar','Edit Choices')
    }
  }
})


