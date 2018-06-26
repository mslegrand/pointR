
cmdSnippetImport<-reactive({
  sendPtRManagerMessage(  sender='cmd.snippet.file.import', openFile=runif(1) )
})

cmdSnippetUnload<-reactive({
  updateAceExt( id= getAceEditorId(), sender='fileCmd.unloadSnippets', snippets="" )
})

observeEvent(input$buttonSnippetImport,{
  cat('input$buttonSnippetImport\n')
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonSnippetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    snippetText<-paste(readLines(datapath), collapse = "\n")
    #updateAceExt( id= getAceEditorId(), sender='fileCmd.importSnippets', snippets=snippetText )
  }
})


  
# cmdSnippetFileOpen<-reactive({
#   fullPath<-getCurrentDir()
#   try(
#     fileName<-dlgOpen(
#       default=fullPath,
#       title = "Select snippet file to Open", 
#       filters = dlgFilters[c("*.snp", "All","snp") ]
#     )$res
#   )
#   if(length(fileName)>0 && nchar(fileName)>0){ 
#     snippetText<-paste(readLines(fileName), collapse = "\n")
#     
#     if(nchar(snippetText)>0){
#       session$sendCustomMessage(
#         type = "shinyAceExt",    
#         list(id= getAceEditorId(), snippets=snippetText)
#       )
#     }
#   }
# })
# 
