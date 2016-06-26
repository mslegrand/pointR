#----BUTTON EVENTS BEGIN-----------------
#-----POINTS--------------------------
# #---Insert Value-------------------

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

#---BUTTON: remove selected point  -----
observeEvent( input$removePt, {
  selection<-input$ptRSelect
  if(selection!=""){
    ptRList<-getPtDefs()$pts
    pts<-ptRList[[selection]]
    if(length(pts)==0){  #if no points, return
      return(NULL)
    }
    indx=selectedPoint$point.index 
    src<-user$code
    
    #delete the point from the ptR list
    
    if(indx>=1){
      pts<-pts[-c(2*indx,2*indx-1)]
      selectedPoint$point.index<-max(1,selectedPoint$point.index-1)
    } 
    if( indx==0 || length(pts)<1 ){
      pts<-list(NULL)
      selectedPoint$point.index<-0 
      ptRList[selection]<-pts
    } else {
      ptRList[[selection]]<-pts
    }
    
    
    src<-pts2Source(src,ptRList)
    
    tagList<-getPtDefs()$df
    if(!is.null(tagList)){ #tagR exists
      df<-tagList[[selection]] 
      if(!is.null(df)){ #df==tagR$x exists
        if(length(ptRList[[selection]])==0){ #no points 
          tagList[selection]<-NULL #remove tagR$x
         # browser()
        } else { # has points
          tags<-df$tag
          if(indx==1 && !(2 %in% tags)){ #do nothing
            
          } else {
            if(indx %in% tags){ #remove the tag row
              df<-subset(df,tags!=indx)
              tags<-df$tag
            }
          }
          #slide tags nos. down
          tags2move<-tags>indx
          if(length(tags2move)>0){
            tags[tags>indx]<-tags[tags>indx]-1
            df$tag<-tags
            tagList[[selection]]<-df
          }
        }
        src<-df2Source(src,tagList)
      }
    }
    user$code<-src
    #updateAceEditor( session,"source", value=src)
  }
})

#---selected point forward button-----
observeEvent(input$forwardPt,{
  selection<-input$ptRSelect
  ptRList<-getPtDefs()$pts
  len<-length(ptRList[[selection ]])/2
  selectedPoint$point.index<-min(len, selectedPoint$point.index+1)
})

#---selected point backward button-----
observeEvent(input$backwardPt,{
  #decrement selectedPointIndex
  selection<-input$ptRSelect
  ptRList<-getPtDefs()$pts
  len<-length(ptRList[[selection ]])/2
  if(len>0){
    selectedPoint$point.index<-max(1,selectedPoint$point.index-1)
  } else {
    selectedPoint$point.index<-0
  }
})


#---TAG POINT button-----
observeEvent(input$tagPt, {
  selection<-input$ptRSelect
  ptDefs<-getPtDefs()
  ptsList<-ptDefs$pts
  dfList<-ptDefs$df
  point.index<-max(1,selectedPoint$point.index) #can change later
  ok=TRUE
  #There are 3 distinct cases: 
  # 1. tagR list not there, add tagR list, selection and insert 
  # 2. tagR list there, but selection is not: add selection and insert
  # 3  Both tagR list and tagR[[selection]] are there, just add tag no.
  if(ok && is.null(dfList) ){ # 1 Adds a new tagR list
    if(!is.null(selection)){
      if(point.index>1){
        tags<-c(1,point.index)
      }else {
        tags<-1
      }
      
      reactiveTag$freq[[selection]]<-NULL
      updateSelectInput(session, "tagFreq", selected="Off" )
      
      df<-data.frame(tag=tags)
      dfList= structure( list( df) , names=selection )
      replacement<-paste0("\n\n", formatDFDefs(dfList),"\n\n")
      src<-user$code
      pos<-getDefPos(src, "ptR")     
      src<-paste0( substr(src, 1, pos[2]), replacement,  
                   substr(src, pos[2]+1, nchar(src))) 
      user$code<-src        
    } 
    
    ok=FALSE
  } 
  if(ok && is.null(dfList[[selection]]) ){ #2. Adds new TagR entry
    if(point.index>1){
      tags<-c(1,point.index)
    }else {
      tags<-1
    }
    # freq<-reactiveTag$freq
    # reactiveTag$freq[[tagName]]<-NULL
    # updateSelectInput(session, "tagFreq", selected="Off" )
    
    df<-data.frame(tag=tags)
    dfList[[selection]]<-df
    user$code<-df2Source(user$code,dfList)
    #message=paste0("Need to add ", selection," to tagR list") 
    #session$sendCustomMessage(type='error', message=message )
    ok=FALSE
  }
  if(ok ){ # 3. add tag
    len<-length(ptsList[[selection]])/2 #number of points in selection
    if(len>0){
      df<-dfList[[selection]]
      if(length(df)==0){ # selection is not listed in tags
        df<-data.frame(tag=1)
      }
      if("tag" %in% names(df)){ # if not, then do nothing
        tags<-df$tag
        if(!(point.index %in% tags)){
          row<-max(tags[tags<point.index])
          tmp.df<-subset(df,tag==row)
          tmp.df$tag<-point.index
          df<-rbind(df, tmp.df)
          ordrows<-order(df$tag)
          df<-df[ordrows,,drop=FALSE]
          dfList[[selection]]<-df
          #src<-user$code
          user$code<-df2Source(user$code,dfList)
          #user$code<-src
        }
      }
    }
    
  }
  
})

#------TAG BUTTONS
# if moveTag 
# load tagmove script and reload svgR, wait for movement
# and update ptR


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
