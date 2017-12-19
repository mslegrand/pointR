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
      newPtDefs<-ptDefs()
      tagRList<-newPtDefs()$df
      tagPtName<-input$tagPts
      df<-tagRList[[tagPtName]]
      df[df$tag==tagIndx,tagCol]<-value
      tagRList[[tagPtName]]<-df
      newPtDefs$df<-tagRList
      updateAceExtDef(newPtDefs, "tagButtons")
      #src<-getCode() # !!!
      #src<-df2Source(src, tagRList) !!!
      #setCode(src) # !!!
    }
})
#---commit  button----- 

#(updates src with editor contents)
# alternatively can use observeEvent( input$commit, { ... })
# observe({
#   c(input$commit, input$commitMssg )
#   isolate({
#     if(request$refresh>0){
#       
#     }
#     
#   })
# })

observe({
  c(input$commit,input$commitMssg )
  isolate(
    {
    sender='cmd.commit'
    triggerRefresh(sender, rollBack=FALSE)
  })
})

#!!!  unused?
checkPtrSyntax<-function(src){
  lines<-strsplit(src,"\n") 
  lines<-lines[[1]]
  ptRPos<-grep("^\\s*ptR<-",lines)
  svgRPos<-grep("^\\s*svgR\\(",lines)
  if(length(ptRPos)>1 )
    base::stop("Bad File: Multiple  ptR lists")
  if(length(svgRPos)>1) 
    base::stop("Bad File: Multiple  ptR lists or svgR calls")
}

processCommit<-reactive({
    src<-getCode() #input$source #------ace editor
    if(length(src)==1 && nchar(src)>0){
      ptRList<-getPtDefs()$tib
      tryCatch({
        lines<-strsplit(src,"\n") 
        lines<-lines[[1]]
        ptRPos<-grep("^\\s*ptR<-",lines)
        svgRPos<-grep("^\\s*svgR\\(",lines)

        if(length(ptRPos)>1 || length(svgRPos)>1){
          base::stop("Bad File: Multiple  ptR lists or svgR calls")
        }
        if(length(ptRPos)>=1 && length(svgRPos)>=1 && !(ptRPos[1]<svgRPos[1])){
          base::stop("Bad File: ptR list must come prior to svgR call")
        }
        if(length(svgRPos)==1){
          if(length(ptRPos)==0){
            # switch to tagValues
            updateRightPanel("tagValues")
          }
        }
        if(length(svgRPos)==0){ # just R code I guess
          # capture capture output as mssg
          output<-captureOutput(eval(parse(text=src)))
          output<-paste( output, collapse="\n" )
          output<-paste("Output:",output,sep="\n")
          updateRightPanel("logPanel")
          base::stop(output , call.=FALSE, domain=NA);
        }
        # passed so far
        # next check if it can be run
        parsedCode<-parse(text=src) 
        eval(parsedCode)
        # no error occured so all systems go!!!!
        mssg$error<-""

        #if in log page move to points
        if(rightPanel()=="logPanel"){
          updateRightPanel("tagValues")
        } 
        #remove all removeAllMarkers from ace since all sys go.
       
        session$sendCustomMessage(
          type = "shinyAceExt",
          list(id= "source", removeAllMarkers='removeAllMarkers', sender='commit.removeMarkers', setOk=TRUE)
        )
        #editOption$.saved<-FALSE # !!! soon to be obsolete!!!
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
        mssg$error<-err
        cat("commit got an error\n")
        updateRightPanel("logPanel")
      }) 
    }
})

#----BUTTON EVENTS END-----------------
