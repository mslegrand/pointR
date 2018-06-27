
cmdDnippetImport<-function(){
    sendPtRManagerMessage(  sender='cmd.dnippet.file.import', openFile=runif(1) )
}

observeEvent(input$buttonDnippetImport,{
  cat('input$buttonDnippetImport\n')
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonDnippetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    dnippetText<-paste(readLines(datapath), collapse = "\n")
    dnippetList<-dripplets2List2(dnippetText)
    # browser()
    dnippets<-getDnippets4ToolBar(dnippetList)
    # browser()
    dnName<-basename(datapath)
    # adds to selection
    addDrippets( dnName, dnippets, select=dnName )
    # adds drippets to toolbar
    
    # lapply(dnippets, function(dnpt){
    #   print(dnpt)
      #html(id='dndSnippetList', add=TRUE, html=HTML(dnippets))
    # })
    
  }
})