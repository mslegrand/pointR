observe({input$messageFromAce
  isolate({
    #cat('serverAce:...observe input$messageFromAce:: entering\n')
    #browser()
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      
      
      # if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
      #   reqSelector<-input$messageFromAce$selector
      #   code<-input$messageFromAce$code
      #   ptDefs<-ex.getPtDefs(src)
      #   #todo: add isTransformable here
      #   cntrlSelector<-control$selector
      #   cntrlSelector<-selectorUpdate(tibs,  reqSelector, cntrlSelector )
      #   cntrlSelector$ptDefs<-ptDefs
      #   cntrlSelector$selector<-cntrlSelector #should this trigger a commit???
      # }
      # 
      
      request$code<-input$messageFromAce$code
      request$sender<-input$messageFromAce$sender
      cat('serverAce:...input$messageFromAce::sender=',request$sender,"\n")
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        updateSelected4Ace(reqSelector)
      }
      
      if(length(input$messageFromAce$dirty)>0){
        editOption$.saved <- !(as.numeric(input$messageFromAce$dirty) > 0)
      }
      if(request$sender %in% c('cmd.commit','cmd.openFileNow', 'cmd.saveFileNow', 'cmd.file.new' )){
        #note, request$code now contains the code from ace
        # and getPtDefs depends on getCode depends on request$code
        # we should now get the tibs and reset the name, namechoices ... of tibEd
        #browser()
        
        if(request$sender=='cmd.commit' && !is.null(getTibName())){ 
          name=getTibName()
        } else { 
          name=NULL
        }
        tibs<-getPtDefs()$tib
        resetSelectedTibbleName(tibs=tibs, name=name)
        #control$selector<-selectorUpdate( tibs, request$selector, control$selector )
        processCommit()
      } 
      # if( request$sender %in% 'cmd.openFileNow'){
      #   # !!! TODO: set point.index to end of points (if points)
      # }
      
      if(request$sender %in% 'cmd.saveFileNow'){
        datapath<-input$messageFromAce$auxValue
        txt<-input$messageFromAce$code
        writeLines(txt, datapath)
        setCurrentFilePath(datapath)
        editOption$currentFile<-basename(datapath)
        editOption$currentDirectory<-dirname(datapath)
        session$sendCustomMessage(
          type = "shinyAceExt",
          list(id= "source", setClean=TRUE, sender='cleanPlease')
        )
        
      }
    }
  })
})

updateAceExtDef<-function(newPtDef, sender, selector=list() ){
  cat('serverAce:...sender=',sender,"\n")
  cat("serverAce:...-----newPtDefs$tib\n")
  # browser()
  # print(newPtDef$tib)
  newPtDef$tib<-pts2Integers(newPtDef$tib )
  
  replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() )
  if( length(replacementList)>0 ){
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= "source", replacement=replacementList, selector=selector, sender=sender, ok=1)
    )
  }
}

updateAceExt<-function(sender, ...){
  data<-as.list(...)
  if(length(data)>0){
    data<-c(list(id=='source', sender=sender), data )
    session$sendCustomMessage(
      type = "shinyAceExt",
      data
    )
  }
}

observe({
  request$sender
  isolate({
    if(request$sender=='startup'){
      cat("request: startup cmdFileNew")
      cmdFileNew()
    }
    # if(request$sender %in% c( "cmd.openFileNow", "cmd.newFile")){ #!!! check these names
    #   # get valid point name, then set index to last valid index. (length of points?)
    #   pd<-getPtDefs()
    #   if(length(pd)>0){
    #     tibs<-pd$tib #!!! check this
    #     #name<-tail(names(tibs),1) # !!! KLUDGE, last name 
    #     resetSelectedTibbleName(tibs=tibs, name=NULL)
    #   }
    # } 
  })
})

updateSelected4Ace<-function( reqSelector){
  #name, row, columnName, matCol,  ptColName ){
  # triggerRefresh(sender='update.tibEd', rollBack=FALSE, auxValue=FALSE)
  # tmp<-list( name= NULL, selTib=list())
  # tibs<-getPtDefs()$tib
  if(!is.null(reqSelector[['name']])){
    #cat("updateSelected::name=",name,"\n")
    selectedTibble$name=reqSelector[['name']]
  }
  if(!is.null(reqSelector[['ptColName']])){
    #cat("updateSelected::ptColName=",ptColName,"\n")
    selectedTibble$ptColName=reqSelector[['ptColName']]
  }
  if(!is.null(reqSelector[['rowIndex']])){ # !!! may want to provide a check here
    #cat("updateSelected::row=",row,"\n")
    selectedTibble$rowIndex=reqSelector[['rowIndex']]
  }
  if(!is.null(reqSelector[['matCol']])){
    # #cat("updateSelected::matCol=",matCol,"\n")
    # if(matCol=='end'){
    #   mc<-ncol(getTibPts()[[selectedTibble$tibSel$row]])
    #   #selectedTibble$tibSel$matCol = ifelse(is.integer(mc), mc, 0)
    #   tmp$selTib$matCol== ifelse(is.integer(mc), mc, 0)
    # } else {
    selectedTibble$matCol=reqSelector[['matCol']]
    #}
  }
  if(!is.null(reqSelector[['columnName']])){
    #cat("updateSelected::columnName=",columnName,"\n")
    selectedTibble$columnName=reqSelector[['columnName']]
  }
} 

