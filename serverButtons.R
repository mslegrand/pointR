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
  input$commit
  #get text from editor
  isolate({ 
    src<-input$source #------ace editor
    if(nchar(src)>0){
      lines<-strsplit(src,"\n")
      lines<-lines[[1]]
      ptRPos<-grep("^\\s*ptR<-",lines)
      svgRPos<-grep("^\\s*svgR\\(",lines)
      Err<-NULL
      if(length(ptRPos)!=1){
        Err<-"Missing ptR list or multiple  ptR lists"
      }
      if(length(svgRPos)!=1){
        Err<-"Missing svgR call or multiple svgR calls"
      }
      if(is.null(Err) & !(ptRPos[1]<svgRPos[1])){
        Err<-"ptR list must come prior to svgR call"
      }
      if(!is.null(Err)){
        src<-""
        updateNavbarPage(session, "plotNavBar", selected ="Log")
        mssg$error<-Err
      } 
    }
    if(nchar(src)>0){
      #check source and update if ok
      tryCatch({
        src<-preProcCode(src)
        user$code<-src
        mssg$error<-"" #ie. ok
        if(input$plotNavBar=="Log"){
          updateNavbarPage(session, "plotNavBar", selected ="Points")
        }
      }, 
      error=function(e){
        mssg$error<-paste(mssg$error, e, collapse="\n", sep="\n")
        updateNavbarPage(session, "plotNavBar", selected ="Log")
      }
        
      )
     }
  })
})
#----BUTTON EVENTS END-----------------
