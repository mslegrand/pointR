src2sourceType<-function(src){  #not used !!
  lines<-strsplit(src,"\n") 
  lines<-lines[[1]]
  if(length(lines)==0){
    setSourceType(sourceType=textPanelTag)
  } else {
    if(grepl("^---",lines[1])){
      setSourceType(sourceType=rmdPanelTag)
    } else{
      svgRPos<-grep("^\\s*svgR\\(",lines)
      if(length(svgRPos)==0){ # just R code I guess
        # browser()
        setSourceType(sourceType=RPanelTag) #
      } else {
        setSourceType(sourceType=svgPanelTag) #SVG code
      }         
    }
  }
}


# uses to mode, input$pages
# called exclusively by processMssgFromAceMssgPageIn
processCommit<-reactive({
  # log.fin( processCommit)
  clearErrorMssg()
  
  mode<-getModeX()
  log.val(mode)
  if(length(mode)!=1){
    cat('missing mode\n'); browser()
    return(NULL)
  }
  if(!is.null(getProjectFullPath()) && dir.exists(getProjectFullPath())){
    setwd( editOption$currentProjectDirectory)
  } else {
    # else set to home
    setwd("~")
  }
  
  
  if( identical(mode, 'ptr')){
    processSvgR()
  } else if(  identical(mode, 'ptrrmd') ){
    processKnit()
  } else if( identical(mode, 'dnippets')){
    processDnip()
  } else if (identical(mode, 'text') #||identical(mode, 'txt') 
             ){
    setSourceType(textPanelTag)
  } else if (identical(mode, 'snippets')){
    setSourceType(snippetPanelTag)
  } 
  else if (identical(mode, 'javascript')){
    setSourceType(javascriptPanelTag)
  }
  else if (identical(mode, 'app')){
    setSourceType(appPanelTag)
  }
  else {
    cat("unknown mode\n"); browser()
  }
  if(!hasError()){
    tabId<-input$pages
    savePage(tabId)
  }
  # log.fout(processCommit)
})

processSvgR<-reactive({
#  log.fin(processSvgR)
  src<-getCode()
  if(length(src)==1){
    tryCatch({
      initialEnv<-getEnvList()
      lines<-strsplit(src,"\n")
      lines<-lines[[1]]
      ptRPos<-grep("^\\s*ptR<-",lines)
      svgRPos<-grep("^\\s*svgR\\(",lines)
      if(length(svgRPos)==0){ # just R code I guess
        setSourceType(sourceType=RPanelTag) #
      } else {
        setSourceType(sourceType=svgPanelTag) #SVG code
      }
      if(length(ptRPos)>1 || length(svgRPos)>1){
        base::stop("Bad File: Multiple  ptR lists or svgR calls")
      }
      if(length(ptRPos)>=1 && length(svgRPos)>=1 && !(ptRPos[1]<svgRPos[1])){
        base::stop("Bad File: ptR list must come prior to svgR call")
      }
      
      if(length(svgRPos)==0){ # just R code I guess
        #test for error and capture output
        # capture capture output as mssg
        env1<-getEnvList()
        parsedCode<-parse(text=src)
        output<-lapply(parsedCode, function(x){
          captureOutput(eval(x, envir= env1    ))
        })
        output<-paste( unlist(output), collapse="\n" )
        output<-paste("Output:",output,sep="\n")
        setCapturedMssg(output)
        setSourceType(sourceType=RPanelTag) #no error, just R code
      } else { # presume to be svgR code
        env2<-getEnvList()
        #if parMode is dndsnippet, need to add to env2, WH=c(48,32)
        if(identical(getParMode(), 'dnippets')){
          env2<-c(env2, list(WH=c(48,32)))
        }
        
        parsedCode<-parse(text=paste0(src) )
        
        output<-captureOutput(eval(parsedCode, envir=env2 ))
        output<-paste( output, collapse="\n" )
        output<-paste("Output:",output,sep="\n")
        setCapturedMssg(output)
        setSourceType(sourceType=svgPanelTag) #SVG code
      }
        # passed so far
        # no error occured so all systems go!!!!
        #remove all removeAllMarkers from ace since all sys go.
        updateAceExt( id= getAceEditorId(), sender='commit.removeMarkers', removeAllMarkers='removeAllMarkers', setOk=TRUE)
    }, #end of try
    error=function(e){ 
        #Error handler for commit
        e<-e$message
        if(all(!str_detect(e,'Output:'))){
          e<-c(e,traceback())
        }
        err<-unlist(e)
        err<-paste(err, collapse="\n", sep="\n")
        # log.val(err)
        #try to locate where the error occured
        if(str_detect(err, 'parse')){
          m<-str_match(err, ":([0-9]+):([0-9]+):")
          if(length(m)==3){
            row=as.numeric(m[2])-1
            col=as.numeric(m[3])-1
            updateAceExt(id= getAceEditorId(), addMarker=c(row,col), sender='commit.addmarker' )
          }
        }
        if(str_detect(err,' not found')){
          m<-str_match(err, "object '([^']+)' not found")
          if(length(m)==2){ #find the first line where this occurs
            notFound<-paste0("\\b",m[2],"\\b")
            srcs<-str_split(src,"\n")[[1]]
            row<-min(which(str_detect(srcs,notFound)))-1
            col<-1
            updateAceExt(id= getAceEditorId(), addMarker=c(row,col), sender='commit.addmarker' )
          }
        }
        setErrorMssg(err)
      } #end of error handler
    ) #end of tryCatch 
  } #end of if(length==1)
  
  # log.fout(processSvgR)
})



