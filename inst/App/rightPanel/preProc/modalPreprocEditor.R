cmdPreProcEdit<-function(preprocScripts, preprocName){
  showModal( 
    modalPreProcEditor(preprocScripts=preprocScripts, preprocName=preprocName) 
  )
}

# creates a single tabPanel entry
newPreProcPanel<-function(label, value){
  outputId<-paste0('ptPreProcAce',label)
  tabPanel(label,
           div(
             aceEditor(
               outputId=outputId,
               height = "600px",
               mode='r',
               value=value
             )
           )
  )
}

preProcTabSetPanel<-function(id='ptPreProcpages', preprocScripts ){
  preprocScripts<-unlist(preprocScripts, use.names = FALSE)
  # browser()
  # pptabs<- lapply(preprocScripts,newPreProcPanel)
  # browser()
  pptabs<-mapply(newPreProcPanel, names(preprocScripts), preprocScripts, 
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  browser()
  do.call(tabsetPanel, pptabs)
}


# dropdown for preprocessor

modalPreProcEditor <- function( preprocScripts, preprocName ) {
  modalDialog(
    #absolutePanel( id='preProcEdDiv', left=5, bottom=5, 
         div( id='ptPreProcBackPanel', class='backPanel',
              div( style='margin-left:20px; color: #00ffff; ', h4(textOutput("ptPreProcSource")) ) ,
              preProcTabSetPanel(id='ptPreProcpages', preprocScripts=preprocScripts)#,
              # div(style="width: 100%; overflow: hidden; margin-top:5px; margin-bottom:5px",
              #     div( style="float:left;",
              #          actionButton(inputId= "commitPtPreProcButton", label='Commit', class=c("btn") )
              #     ),
              #     div( style="float:right;",
              #          actionButton(inputId= "dimissPtPreProcButton", label='Dismiss', class=c("btn") )
              #     )
              # )
         ),
    title="Preproc Editor",
    footer=tagList(
      #textInput("preprocName", preprocName),
      actionButton("preprocEditorCommit", "Commit"),
      actionButton("preprocEditordismiss", "Dismiss")
    )
  #)
  )
}

# for(name in unlist(preprocChoices, use.names = FALSE)){
#   if(name %in% names(preprocScripts)){
#     editorId=paste0('ptPreProcAce', name)
#     showTab("ptPreProcpages",name )
#     updateAceEditor(session, editorId=editorId, value=preprocScripts[name])
# 
# 

