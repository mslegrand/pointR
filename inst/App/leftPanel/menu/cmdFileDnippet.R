
cmdDnippetImport<-function(){
    sendPtRManagerMessage(  sender='cmd.dnippet.file.import', openFile=runif(1) )
}

# loads the drippets given the datapath
# then calls addDrippets to add to drippet toolbar
loadDndSnippets<-function(datapath){
  dnippetText<-paste(readLines(datapath), collapse = "\n")
  dnippetList<-dripplets2List2(dnippetText)
  # browser()
  dnippets<-getDnippets4ToolBar(dnippetList)
  # browser()
  dnName<-basename(datapath)
  # adds to selection
  addDrippets( dnName, dnippets, select=dnName )
}

observeEvent(input$buttonDnippetImport,{
  
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonDnippetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    loadDndSnippets(datapath)
  }
})