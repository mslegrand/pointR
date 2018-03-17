
cmdSnippetFileOpen<-reactive({
  session$sendCustomMessage(
    type = "ptRManager", 
    list(id= getAceEditorId(), openFile=TRUE, sender='cmd.snippet.file.open' )
  )
  
})


observeEvent(input$buttonSnippetOpen,{
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonSnippetOpen)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    snippetText<-paste(readLines(datapath), collapse = "\n")
    session$sendCustomMessage(
      type = "shinyAceExt",    
      list(id= getAceEditorId(), snippets=snippetText)
    )
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
