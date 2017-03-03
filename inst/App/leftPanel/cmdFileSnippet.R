
cmdSnippetFileOpen<-reactive({
  fullPath<-getCurrentDir()
  try(
    fileName<-dlgOpen(
      default=fullPath,
      title = "Select snippet file to Open", 
      filters = dlgFilters[c("*.snp", "All","snp") ]
    )$res
  )
  if(length(fileName)>0 && nchar(fileName)>0){ 
    snippetText<-paste(readLines(fileName), collapse = "\n")
    
    if(nchar(snippetText)>0){
      session$sendCustomMessage(
        type = "shinyAceExt",    
        list(id= "source", snippets=snippetText)
      )
    }
  }
})
