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
      user$code<-df2Source(user$code, tagRList)
      # updateSelectInput(session, "tagColVal",
      #                   choices=choices, selected=value )
    }
})
#---commit  button----- 



#(updates user$code with editor contents)
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
        if(length(ptRPos)!=1){
          stop("Bad File: Missing ptR list or multiple  ptR lists")
        }
        if(length(svgRPos)!=1){
          stop("Bad File: Missing svgR call or multiple svgR calls")
        }
        if(!(ptRPos[1]<svgRPos[1])){
          stop("Bad File: ptR list must come prior to svgR call")
        }
        src<-preProcCode(src) 
        parsedCode<-parse(text=src)
        eval(parsedCode)
        mssg$error<-""
        user$code<-src
        if(input$plotNavBar=="Log"){
          updateNavbarPage(session, "plotNavBar", selected ="Points")
        } 
        session$sendCustomMessage(
          type = "shinyAceExt", 
          list(id= "source", removeAllMarkers='removeAllMarkers')
        )
      }, #end of try
      error=function(e){ 
        e<-c(e,traceback())
        mssg$error<-paste(e, collapse="\n", sep="\n")
        err<-mssg$error
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
        updateNavbarPage(session, "plotNavBar", selected ="Log")
      }) 
     }
  })
})
#----BUTTON EVENTS END-----------------
