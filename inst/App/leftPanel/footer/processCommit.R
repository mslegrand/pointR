src2sourceType<-function(src){  #not used !!
  lines<-strsplit(src,"\n") 
  lines<-lines[[1]]
  svgRPos<-grep("^\\s*svgR\\(",lines)
  if(length(svgRPos)==0){ # just R code I guess
    # browser()
    setSourceType(sourceType=RPanelTag) #
  } else {
    setSourceType(sourceType=svgPanelTag) #SVG code
  }
}


# uses to mode, input$pages
# called exclusively by processMssgFromAceMssgPageIn
processCommit<-reactive({
  # cat_list<<-c( cat_list,">---> processCommit\n")
  clearErrorMssg()
  
  mode<-getModeX()
  if(length(mode)!=1){
    browser()
    return(NULL)
  }
  # cat("class(mode)=" ,class(mode),"\n")
  # cat('ProcessCommit: mode=',format(mode),"\n")
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
    browser()
  }
  if(!hasError()){
    tabId<-input$pages
    # cat("tabId=",tabId,"\n")
    #cat_list<<-c( cat_list,'>---> processCommit::savePage\n')
    savePage(tabId)
    #cat_list<<-c( cat_list,'<---< processCommit::savePage\n')
  }
  #cat_list<<-c( cat_list,"<---< processCommit\n")
})


processSvgR<-reactive({
  src<-request$code
  # cat('>----> processSvgR::\n')
  if(length(src)==1){
    ptRList<-getPtDefs()$tib
    tryCatch({
      lines<-strsplit(src,"\n") 
      lines<-lines[[1]]
      # cat('ptRPos\n')
      ptRPos<-grep("^\\s*ptR<-",lines)
      # cat('svgRPos\n')
      svgRPos<-grep("^\\s*svgR\\(",lines)
      # cat('done\n')
      if(length(svgRPos)==0){ # just R code I guess
        # browser()
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
        env<-new.env()
        parsedCode<-parse(text=src)
        output<-lapply(parsedCode, function(x){
          captureOutput(eval(x, envir=env))
        })
        output<-paste( unlist(output), collapse="\n" )
        output<-paste("Output:",output,sep="\n")
        setCapturedMssg(output)
        setSourceType(sourceType=RPanelTag) #no error, just R code
      } else { # presume to be svgR code
        # next check if it can be run
        # cat("processCommit::captureOutput2\n")
        parsedCode<-parse(text=src) 
        # svg<-eval(parsedCode)
        # if(identical(class(svg),'svgDoc')){
        #   w<-svg$root$getAttr('width')
        #   h<-svg$root$getAttr('height')
        #   #set WH in selected...
        # }
        output<-captureOutput(eval(parsedCode))
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
        if(all(!str_detect(e,'Output:'))){
          e<-c(e,traceback())
        }
        err<-paste(unlist(e), collapse="\n", sep="\n")
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
  # cat('<----< processSvgR::\n')
})



