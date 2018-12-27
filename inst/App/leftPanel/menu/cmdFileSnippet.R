
cmdSnippetImport<-function(){
  sendPtRManagerMessage(  sender='cmd.snippet.file.import', openFile=runif(1) )
}

cmdSnippetUnload<-function(){
  updateAceExt( id= getAceEditorId(), sender='fileCmd.unloadSnippets', snippets="" )
}

observeEvent(input$buttonSnippetImport,{
  fp.dt<-parseFilePaths(c(home='~'), input$buttonSnippetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    snippetText<-paste(readLines(datapath), collapse = "\n")
    # cat(snippetText)
    aceId<-getAceEditorId()
    updateAceExt( id=aceId, sender='snippetImport', snippets=snippetText)
  }
})


