src2sourceType<-function(src){  #not used !!
  lines<-strsplit(src,"\n") 
  lines<-lines[[1]]
  svgRPos<-grep("^\\s*svgR\\(",lines)
  if(length(svgRPos)==0){ # just R code I guess
    setSourceType(sourceType=RPanelTag) #
  } else {
    setSourceType(sourceType=svgPanelTag) #SVG code
  }
}



processCommit<-reactive({
  clearErrorMssg()

  #cat('ProcessCommit: request$mode=',format(request$mode),"\n")
  if( identical(request$mode, 'ptr')){
    processSvgR()
  } else if(  identical(request$mode, 'ptrrmd') ){
    processKnit()
  } else if( identical(request$mode, 'markdown')){
    processDnip()
  }

})


processSvgR<-reactive({
  src<-request$code
  cat('processSvgR::\n')
  if(length(src)==1){
    ptRList<-getPtDefs()$tib
    tryCatch({
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
        env<-new.env()
        output<-lapply(lines, function(line){
          cat("processCommit::captureOutput\n")
          captureOutput(eval(parse(text=line), envir=env))
        })
        output<-paste( output, collapse="\n" )
        output<-paste("Output:",output,sep="\n")
        setCapturedMssg(output)
        setSourceType(sourceType=RPanelTag) #no error, just R code
      } else { # presume to be svgR code
        # next check if it can be run
        cat("processCommit::captureOutput2\n")
        parsedCode<-parse(text=src) 
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
})



