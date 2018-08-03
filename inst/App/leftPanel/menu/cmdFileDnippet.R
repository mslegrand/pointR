
cmdDnippetImport<-function(){
    sendPtRManagerMessage(  sender='cmd.dnippet.file.import', openFile=runif(1) )
}

# loads the drippets given the datapath
# then calls addDrippets to add to drippet toolbar
loadDndSnippets<-function(datapath){
  dnippetText<-paste(readLines(datapath), collapse = "\n")
  dnippetList<-dripplets2List2(dnippetText) # contains hint, snippet, logo where logo has been processed into SVG
  dnippets<-getDnippets4ToolBar(dnippetList) # minor reshape
  
  dnName<-basename(datapath)
  # adds to selection
  #addDrippets( dnName, dnippets, select=dnName )
  addDnippets2AllDnippets( dnName, dnippets )
  addDnippetPath2DB(dnName, datapath )
  add2DnippetChoices(dnName, TRUE)
}

observeEvent(input$buttonDnippetImport,{
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonDnippetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    #addToDnippetsFiles(datapath ) 
    loadDndSnippets(datapath)
  }
})