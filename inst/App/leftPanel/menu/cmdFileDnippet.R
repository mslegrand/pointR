
cmdDnippetImport<-function(){
   log.fin(cmdDnippetImport )
    sendPtRManagerMessage(  sender='cmd.dnippet.file.import', openFile=runif(1) )
    log.fout(cmdDnippetImport)
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
    # add2DnippetDBPath( dnName, datapath )
    #This sets the dname default value for existing pages (no effect on pages not yet loaded)
    if( !identical( startup, TRUE ) ){
      add2DnippetChoices( dnName, TRUE )
    }
  } )
  log.fout(loadDndSnippets)
}

observeEvent(input$buttonDnippetImport,{
  log.fin(input$buttonDnippetImport )
  browser()
  fp.dt<-parseFilePaths(c(home='~'), input$buttonDnippetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    # if this is a project, copy to resource/dnds
    ptRproj<-pprj()
    if(!is.null(ptRproj$pathToProj) && dir.exists( ptRproj$pathToProj )){
      # check for dnds dir and if not existing, create
      dndsDir<-path_join(c(editOption$currentProjectDirectory, resourceDir, 'dnds'))
      if(!dir.exists(dndsDir)){
        dir.create(dndsDir, showWarnings = F, recursive=T) # TODO!!! eventually can remove this.
      }
      # copy over
      file.copy(datapath, dndsDir)
      # add to dnippet db
      dname=path_file(datapath)
      dndspath<-path_join(c(dndsDir,dname))
      #dname=sub('\\.dnds$','',dname)
      #add2DnippetDBPath(dname, dndspath)
      # save database
      #saveDnippetsFileNames()
      #load
      loadDndSnippets(dndspath)
      # should force redraw?
      refreshPageDNDs()
    } else { # not a project
      loadDndSnippets(datapath) # !!! may need to rethink this
      # !!! issue: does not add to dnippetDBP
      refreshPageDNDs()
    }
    
    
  }
  log.fout(input$buttonDnippetImport)
})

# todo: reload dnds in aux/dnd/ dir
reloadDndDir<-function(dirPath){
 cat('dirPath=',dirPath,"\n")
  dndfiles<-dir(dirPath, full.names=TRUE)
  dndNames<-basename(dndfiles)
  dndDBNames<-names(dnippetsDB$usage)
  dndDBNames<-dndDBNames[dndDBNames!='tabId']
  for(sname in dndDBNames) {
    if(!sname %in% dndNames){ #not in current dnds dir
      #remove sname column  from dnippetsDB$usage
      cat('\n\n\n---------------\n')
      cat('removing ', sname,'\n')
      dnippetsDB$usage<-select(dnippetsDB$usage, -sname)
        #remove sname row from dnippetsDB$paths
      # dnippetsDB$paths<-filter(dnippetsDB$paths, sname!=dname)
      # removeFromDnippetsSelectionAll()
    }
  }
  # refresh/add any dnd whose file has just appeared
  for(datapath in dndfiles ){
    try({
      dnippetText<-paste(readLines(datapath), collapse = "\n")
      
      dnippetList<-dripplets2List2(dnippetText) # contains hint, snippet, logo where logo has been processed into SVG
      dnippets<-getDnippets4ToolBar(dnippetList) # minor reshape
      bName<-basename(datapath) 
      add2DnippetsSelectionAll( bName, dnippets )
      add2DnippetChoices(bName, FALSE)
    })
  }
  refreshPageDNDs()
}

refreshPageDNDs<-function(){
  selected<-getDnippetsSelected()
  dnippets<-dnippetSelection$all[selected]
  dnippets<-unlist(dnippets,recursive=F)
  names(dnippets)<-NULL
  if(length(dnippets)==0){ 
    sendPtRManagerMessage(sender='cmd.dnippet.refresh', removeDrippets=runif(1));
  } else{
    sendPtRManagerMessage(sender='cmd.dnippet.refresh', insertDrippets=dnippets)
  }
}


