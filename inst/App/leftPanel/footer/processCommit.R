src2sourceType<-function(src){
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
  src<-getCode() #input$source #------ace editor
  if(length(src)==1 && nchar(src)>0){
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
        # capture capture output as mssg
        
        output<-captureOutput(eval(parse(text=src)))
        output<-paste( output, collapse="\n" )
        output<-paste("Output:",output,sep="\n")
        
        #setSourceType(sourceType='logPanel') #no error, just R code
        setSourceType(sourceType=RPanelTag) #no error, just R code
        
        base::stop(output , call.=FALSE, domain=NA);
      } else {
        setSourceType(sourceType=svgPanelTag) #SVG code
      }
      # passed so far
      # next check if it can be run
      parsedCode<-parse(text=src) 
      eval(parsedCode)
      # no error occured so all systems go!!!!
      
      
      cat("commit :: getTibName() is =",format(getTibName()),"\n")
      
      #remove all removeAllMarkers from ace since all sys go.
      
      session$sendCustomMessage(
        type = "shinyAceExt",
        list(id= "source", removeAllMarkers='removeAllMarkers', sender='commit.removeMarkers', setOk=TRUE)
      )
      
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
          session$sendCustomMessage(
            type = "shinyAceExt", 
            list(id= "source", addMarker=c(row,col), sender='commit.addmarker')
          )
        }
      }
      if(str_detect(err,' not found')){
        m<-str_match(err, "object '([^']+)' not found")
        if(length(m)==2){ #find the first line where this occurs
          notFound<-paste0("\\b",m[2],"\\b")
          srcs<-str_split(src,"\n")[[1]]
          row<-min(which(str_detect(srcs,notFound)))-1
          col<-1
          session$sendCustomMessage(
            type = "shinyAceExt", 
            list(id= "source", addMarker=c(row,col), sender='commit.addmarker')
          )
        }
      }
      setErrorMssg(err)
      cat("commit got an error\n",mssg$error,"\n")
      #setSourceType(sourceType='logPanel')
      
    }) 
  }
})

