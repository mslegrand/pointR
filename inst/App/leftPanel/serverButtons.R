#----BUTTON EVENTS BEGIN-----------------
#-----POINTS--------------------------
# #---Insert Value-------------------


#------TAG BUTTONS
# if moveTag 
# load tagmove script and reload svgR, wait for movement
# and update ptR

observeEvent(
  input$insertVal2Col, {
    tagIndx<-as.numeric(input$tagIndx)
    tagCol<- input$tagCol
    if(length(tagCol)>0){ #or not NULL
      value<-input$tagValEd
      tagRList<-getPtDefs()$df
      tagPtName<-input$tagPts
      df<-tagRList[[tagPtName]]
      df[df$tag==tagIndx,tagCol]<-value
      tagRList[[tagPtName]]<-df
      src<-getCode()
      src<-df2Source(src, tagRList)
      setCode(src)
    }
})
#---commit  button----- 

#(updates src with editor contents)
# alternatively can use observeEvent( input$commit, { ... })
observe({ 
  c(input$commit, input$commitMssg)
  session$sendCustomMessage(
    type = "shinyAceExt", 
    list(id= "source", removeAllMarkers='removeAllMarkers')
  )
  #get text from editor
  isolate({ 
    src<-input$source #------ace editor
    if(nchar(src)>0){
      tryCatch({
        lines<-strsplit(src,"\n") 
        lines<-lines[[1]]
        ptRPos<-grep("^\\s*ptR<-",lines)
        svgRPos<-grep("^\\s*svgR\\(",lines)
        
        if(length(ptRPos)>1 || length(svgRPos)>1){
          stop("Bad File: Multiple  ptR lists or svgR calls")
        }
        
        if(length(ptRPos)>=1 && length(svgRPos)>=1 && !(ptRPos[1]<svgRPos[1])){
          stop("Bad File: ptR list must come prior to svgR call")
        }
        
        if(length(svgRPos)==1){
          if(length(ptRPos)==0){
            # switch to Points
            updateRightPanel("Points")
          }
        }
        
        if(length(svgRPos)==0){
          
          # capture capture output as mssg
             
            output<-captureOutput(eval(parse(text=src)))
            output<-paste( output, collapse="\n" )
            output<-paste("Output:",output,sep="\n")

            updateRightPanel("logPanel")
            stop(output , call.=FALSE);
        }
        
        src<-preProcCode(src) 
        parsedCode<-parse(text=src) #insert points into src
        eval(parsedCode)
        
        # no error occured so all systems go!!!!
        mssg$error<-""
        setCode(src) #push code onto stack
        
        
        #if in log page move to points
        if(rightPanel()=="logPanel"){
          updateRightPanel("Points")
        } 
        #remove all removeAllMarkers from ace since all sys go.
        session$sendCustomMessage(
          type = "shinyAceExt", 
          list(id= "source", removeAllMarkers='removeAllMarkers')
        )
        editOption$.saved<-FALSE
      }, #end of try
      error=function(e){ 
        #Error handler for commit
        e<-c(e,traceback())
        err<-paste(unlist(e), collapse="\n", sep="\n")
        #try to locate where the error occured
        if(str_detect(err, 'parse')){
          m<-str_match(err, ":([0-9]+):([0-9]+):")
          if(length(m)==3){
            row=as.numeric(m[2])-1
            col=as.numeric(m[3])-1
            session$sendCustomMessage(
              type = "shinyAceExt", 
              list(id= "source", addMarker=c(row,col))
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
              list(id= "source", addMarker=c(row,col))
            )
          }
        }
        mssg$error<-err
        updateRightPanel("logPanel")
      }) 
     }
  })
})
#----BUTTON EVENTS END-----------------
