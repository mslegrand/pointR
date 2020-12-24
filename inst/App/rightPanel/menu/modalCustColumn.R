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
                   placeholder = 'Enter a name with at least 8 characters for this set of choices '
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
  if(length(CustColumnName)==0 || nchar(CustColumnName)<8){
    hideElement("modalCustColumnEditorCommitOk")
  } else {
    showElement("modalCustColumnEditorCommitOk")
  }
})

getCustColumnAuxPath<-reactive({file.path(getDirPath(),'aux','columnChoiceSets')})

writeAuxCustColumnList<-function(filePath, customColumnChoiceList){
  cat(customColumnChoiceList, file=filePath)
}

readAuxCustColumns<-function(filePath){
  files<-dir(getCustColumnAuxPath(), pattern="*\\.txt$",full.names=TRUE )
  customChoiceList<-list()
  customChoiceList<-lapply(files, function(f){
    scan(file=f, what=character(), quiet=TRUE)
  })
  nms<-gsub('\\.txt','', basename(files))
  names(customChoiceList)<-nms
  customChoiceList
}


observeEvent( input$modalCustColumnEditorCommitOk,{
  
  CustColumnName<-input$modalCustColumnName
  CustColumnName<-sub('\\.txt$','',CustColumnName)
  CustColumnName<-paste0(CustColumnName,'.txt')
  
  customColumnChoiceList<-input$customColumnChoiceTxt
  if(!dir.exists(getCustColumnAuxPath())){
    dir.create(getCustColumnAuxPath() )
  }
  filePath<-file.path(getCustColumnAuxPath(), CustColumnName)
  writeAuxCustColumnList(filePath, customColumnChoiceList)
  aux$colChoiceSet<-readAuxCustColumns()
  removeModal()
})

colSetPageDB<-reactiveVal(
  tibble( tabId="bogus", tibName="bogus", colName='bogus', colChoiceSet='bogus')[0,]
)


populateChoiceSetEditMenu<-function(choice){
  # append tabId="bogus", tibName="bogus", colName='bogus', colChoiceSet='bogus' to colSetPageDB
}

aux<-reactiveValues(colChoiceSet=list())

observeEvent(aux$colChoiceSet,{
  # reset submenu for dropDown-cmdEditColumnChoices
  if(length(aux$colChoiceSet )==0){
    disableDMDM(session, 'plotNavBar','dropDown-cmdEditColumnChoices')
  } else{
    kids<-lapply(names(aux$colChoiceSet), function(nn){
      shinyDMDMenu::menuItem(nn, value=paste0('editChoiceSet-',nn))
    })
    if(length(kids)>0){
      enableDMDM(session, 'plotNavBar','dropDown-cmdEditColumnChoices')
      afterEntry='cmdNewColumnChoices'
      label=paste0('Edit Choice Set')
      shinyDMDMenu::removeDMDM(session, menuBarId  ="plotNavBar", entry='Edit Choice Set', type='dropdown')
      shinyDMDMenu::insertAfterDMDM(
        session, 
        menuBarId  ="plotNavBar",  
        entry=afterEntry,
        submenu=
          do.call(
            function(...){ menuDropdown( label,...) },
            kids
          )
      )
    }
  }
})