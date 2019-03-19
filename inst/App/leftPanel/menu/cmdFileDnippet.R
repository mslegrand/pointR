
cmdDnippetImport<-function(){
    sendPtRManagerMessage(  sender='cmd.dnippet.file.import', openFile=runif(1) )
}

# loads the drippets given the datapath
# then calls addDrippets to add to drippet toolbar
# 1. wrap in  tryCatch
# 2. if dn name is sampleShapes , read from App/templates
loadDndSnippets<-function(datapath, startup=FALSE){
  log.fin(loadDndSnippets)
  try({
    dnippetText<-paste(readLines(datapath), collapse = "\n")
    dnippetList<-dripplets2List2(dnippetText) # contains hint, snippet, logo where logo has been processed into SVG
    dnippets<-getDnippets4ToolBar(dnippetList) # minor reshape
    
    dnName<-basename(datapath)
    # adds to selection
    #The first 2 are almost the same thing, recording path and dname
    add2DnippetsSelectionAll( dnName, dnippets )
    add2DnippetDBPath( dnName, datapath )
    #This sets the dname default value for existing pages (no effect on pages not yet loaded)
    if( !identical( startup, TRUE ) ){
      add2DnippetChoices( dnName, TRUE )
    }
  } )
  log.fout(loadDndSnippets)
}

observeEvent(input$buttonDnippetImport,{
  fp.dt<-parseFilePaths(c(home='~'), input$buttonDnippetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    loadDndSnippets(datapath)
  }
})
