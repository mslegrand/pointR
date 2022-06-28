cmdPreProcEdit<-function(preprocScripts, preprocName, type){
  showModal( 
    modalPreProcEditor(preprocScripts=preprocScripts, preprocName=preprocName, type=type) 
  )
}

# creates a single tabPanel entry
newPreProcPanel<-function(label, value){
  outputId<-paste0('preProcAce-',label)
  tabPanel(label,
           div(
             aceEditor(
               outputId=outputId,
               height = "380px",
               mode='r',
               value=value
             )
           )
  )
}

preProcTabSetPanel<-function(id='ptPreProcpages', preprocScripts ){
  
  preprocScripts<-unlist(preprocScripts, use.names = TRUE)
  pptabs<-mapply(newPreProcPanel, names(preprocScripts), preprocScripts, 
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  do.call(tabsetPanel, pptabs)
}

preProcModalType<-reactiveVal(NULL)

# modal for preprocessor

modalPreProcEditor <- function( preprocScripts, preprocName, type='points'  ) {
  preProcModalType(type)
  modalDialog(
    div( id='ptPreProcBackPanelModalEdit', class='backPanel', style='padding:20px;',
        textInput("modalPreprocName", 
                  label=span(style='color: #00ffff;', 'Preprocessor Name'),  
                  value=preprocName, 
                  width='100%', 
                  placeholder = 'enter name for preproc with at least 8 characters'
        ),
        preProcTabSetPanel(id='ptPreProcpages', preprocScripts=preprocScripts)
    ),
    title="Preproc Editor",
    easyClose = TRUE,
    footer = tagList(
      modalButton("Dismiss"),
      actionButton("modalPreprocEditorCommitOk", "Commit")
    )
  )
}

observeEvent(input$modalPreprocName,{
  preprocName<-input$modalPreprocName
  if(length(preprocName)==0 || nchar(preprocName)<6){
    hideElement("modalPreprocEditorCommitOk")
  } else {
    showElement("modalPreprocEditorCommitOk")
  }
})


observeEvent( input$modalPreprocEditorCommitOk,{
  
  preprocName<-input$modalPreprocName
  preprocName<-sub('\\.R$','',preprocName)
  preprocName<-paste0(preprocName,'.R')
  type<-preProcModalType()
  cmds<-preprocChoices[[type]]
  aceIds<-paste0('preProcAce-', cmds)
  scripts<-lapply(aceIds, function(x){input[[x]]})

  names(scripts)<-cmds
  if(type=='points'){
    filePath<-file.path(getPreProcPPAuxPath(), preprocName)
  } else {
    filePath<-file.path(getPreProcPAAuxPath(), preprocName)
  }
  writeAuxPreprocList(filePath, scripts)
  
  readAuxPreProcs()
  removeModal()
  
})
