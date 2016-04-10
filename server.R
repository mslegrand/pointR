
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(stringr)
library(svDialogs)

#options(shiny.error = recover)

#----begin external rc------------
source("format.R")
source("ptrUtil.R")
source("trUtils.R")

defTag<-"ptR"

js.scripts<-list(
  Points=readFile("www/pointsIO.js"),
  Translate=readFile("www/transIO.js"),
  Rotate=readFile("www/rotIO.js"),
  Scale=readFile("www/scaleIO.js")
)


pts2Source<-function(txt,ptRList){
  replacement<-formatPtDefs(defTag=defTag, ptRList=ptRList)
  txt<-replaceDef(txt, replacement, defTag=defTag) 
}

df2Source<-function(txt, dfList){
  replacement<-formatDFDefs(dfList)
  txt<-replaceDef(txt, replacement, defTag="tagR") 
}

#used by open and commit
preProcCode<-function(src){
  ptDefs<-ex.getPtDefs(src)
  ptRList<-ptDefs$pts
  dfList<-ptDefs$df
  src<-pts2Source(src,ptRList)
  if(!is.null(dfList)){
    src<-df2Source(src, dfList)
  }
  return(src)
}



# called by either a new/load source or upon a commit
ex.getSelectInfo<-function(ptRList, selected, point.index){
  choices<-names(ptRList)
  if(length(choices)==0){
    rtv<-list(selected=NULL, point.index =0 )  
    return(rtv)
  }
  if(!(selected %in% choices)){ # a new choice
    #pick the first choice candidate
    N<-1
    rtv<-list(
      selected=choices[N],
      point.index=length(ptRList[[N]])/2
    )
    return(rtv)
  }
  #default: an existing choice
  point.index=max(point.index, length( ptRList[[selected]])/2 )
  rtv<-list(
    selected=selected, 
    point.index=point.index
  )
  return(rtv)  
}

#
#----end external ------------

#---begin server--------------

shinyServer(function(input, output,session) {
 
# Reactive values----------
  user <- reactiveValues( code=codeTemplate) #  internal copy of user code
  file <-reactiveValues( name="newFile.R")       #  file path
  selectedPoint <- reactiveValues(
    name=NULL,       # name of current point array
    point.index=0    #  selected pt.indx (column) in current point array
  ) 
  tagVal<-reactiveValues(hasTag=FALSE)
  
  init<-reactiveValues(val=0)                   #  kludge for initialization (shouldn't need this)
  mssg<-reactiveValues(error="")
  
# Reactive expressions------------- 
  getPtDefs<-reactive({ ex.getPtDefs(user$code) })  #extract points from user code

  

# Event Observers--------------------------------  

# -----------ACE EDITOR------------------------
observeEvent(
  user$code, {
    if(mssg$error==""){
      updateAceEditor( session,"source", value=user$code)
    }
  }
)
 
  # -----------ACTIVE POINT MATRIX------------------------
  #  observes code and plotNavBar
  #  sets active Point, point selection,  and selectedPoint$point.index
  
  observe({
    user$code
    plotMode<-input$plotNavBar
    isolate({
      #if( plotMode=='Points'){
        point.index<-selectedPoint$point.index
        selected<-input$ptSet
        ptRList<-getPtDefs()$pts
        res<-ex.getSelectInfo(ptRList, selected, point.index)
        selectedPoint$point.index<-res$point.index
        updateSelectInput(session, "ptSet",
                          choices=names(ptRList),
                          selected= res$selected )
      #}
    })
  })
  
  observe({
    input$ptSet
    isolate({
      ptRList<-getPtDefs()$pts
      selectedPoint$point.index<-length(ptRList[[input$ptSet]])
    })
  })

   
  # -----------ACTIVE TAG PT------------------------
  #  observes code and plotNavBar
  #  sets active Tag and Tag index
  #       tagPts
  #       tagIndx
  #       ptSet
  #       point.index

  observe({
    user$code
    input$plotNavBar
    isolate({
      if(input$plotNavBar=="Tags"){
        #point.index<-selectedPoint$point.index
        selected   <-input$ptSet
        ptRList    <-getPtDefs()$pts
        tagRList   <-getPtDefs()$df
        tagNamechoices    <-intersect(names(ptRList),names(tagRList))
        if(length(tagNamechoices)>0){
          # Use selection of ptSet if in choices, ow last avail.
          ptChosen<-input$ptSet
          if(ptChosen %in% tagNamechoices){
            tagName<-ptChosen
          } else{
            tagName<-tail(tagNamechoices,1)
            updateSelectInput(session, "ptSet", selected=tagName )
          }
          updateSelectInput(session, "tagPts", choices=tagNamechoices, selected=tagName )
        }
      }
    })
  })

  
# -----------ACTIVE TAG INDX ------------------------
# observers tagPts
# sets tagIndx
#     
  
  observe({ 
    input$tagPts
    user$code
    input$plotNavBar
    isolate({ 
      if(input$plotNavBar=="Tags"){
        tagName<-input$tagPts
        if(!is.null(tagName)){
            tagRList<-getPtDefs()$df
            df<-tagRList[[tagName]]
            tagIndxChoices<-df[["tag"]]
            pt.indx<-max(1,selectedPoint$point.index)
            selectedTagIndx<-max(tagIndxChoices[ tagIndxChoices<= pt.indx])
            if(selectedPoint$point.index>0){
              selectedPoint$point.index<-selectedTagIndx
            }
            updateSelectInput(session, "tagIndx",
                            choices=tagIndxChoices,
                            selected=selectedTagIndx
            )
            updateSelectInput(session, "ptSet",
                              choices=names(getPtDefs()$pts),
                              selected=tagName
            )
        }
      }
    })
  })

# -----------ACTIVE TAG COL------------------------
# observers tagPts
# sets tagCol
    
  observe({
    input$tagPts
    user$code
    input$plotNavBar
    isolate({
      if(input$plotNavBar=="Tags"){
        tagName<-input$tagPts
        if(!is.null(tagName)){
          tagRList<-getPtDefs()$df
          df<-tagRList[[tagName]]
          tagColChoices<-setdiff(names(df),"tag")
          tagColChoice<-input$tagCol
          if(length(tagColChoices)>0){
            tagColChoices<-sort(tagColChoices)
            if( length(tagColChoice)==0 || 
                !(tagColChoice %in% tagColChoices ) ){
              tagColChoice<-tagColChoices[length(tagColChoices)]
            }
            updateSelectInput(session, "tagCol",
                              choices=tagColChoices, selected=tagColChoices)
          } else { #hide it
            updateSelectInput(session, "tagCol",
                              choices=list(), selected=NULL)
            
          }
        }
      }
    })
  })

  
  
# -----------ACTIVE TAG VALUE------------------------
  
  observe({ 
    tagPtName<-input$tagPts
    tagIndx<-as.numeric(input$tagIndx)
    tagCol<- input$tagCol
    user$code
    isolate({ 
      if( input$plotNavBar=="Tags"){
        if(length(tagCol)>0){ #or not NULL
          tagRList<-getPtDefs()$df
          
          df<-tagRList[[tagPtName]]
          choices<-sort(unique(df[[tagCol]]))
          value<-subset(df,df$tag==tagIndx)[[tagCol]]
          updateSelectInput(session, "tagColVal",
                            choices=choices, selected=value )
        }        
      }

    })
  })
  
  # -----------CHANGE TAG VALUE SEL------------------------
  
  # observe({
  #   input$tagValue
  #   isolate({
  #     if(length(tagCol)>0){ #or not NULL
  #       tagRList<-getPtDefs()$df
  #       tagPtName<-input$tagPts
  #       df<-tagRList[[tagPtName]]
  #       choices<-sort(unique(df[[tagCol]]))
  #       value<-subset(df,df$tag==tagIndx)[[tagCol]]
  #       updateSelectInput(session, "tagColVal",
  #                         choices=choices, selected=value )
  #     }
  #   })
  # })
  
  
  
  
    
#----BUTTON EVENTS BEGIN-----------------
  
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

  #---BUTTON: remove last point  -----
  observeEvent( input$removePt, {
    selection<-input$ptSet
    if(selection!=""){
      ptRList<-getPtDefs()$pts
      pts<-ptRList[[selection]]
      indx=selectedPoint$point.index 
      src<-user$code
      
      if(indx>=1){
        pts<-pts[-c(2*indx,2*indx-1)]
        selectedPoint$point.index<-selectedPoint$point.index-1
      } else {
        pts<-NULL
        selectedPoint$point.index<-0
      }
      ptRList[[selection]]<-pts
      src<-pts2Source(src,ptRList)
      
      tagList<-getPtDefs()$df
      if(!is.null(tagList)){
        df<-tagList[[selection]]
        if(!is.null(df)){
          tags<-df$tag
          if(indx %in% tags){ #remove the tag row
            df<-df[tags!=indx,]
            tags<-df$tag
          } 
          #slide tags nos. down
          tags2move<-tags>indx
          if(length(tags2move)>0){
            tags[tags>indx]<-tags[tags>indx]-1
            df$tag<-tags
            tagList[[selection]]<-df
            src<-df2Source(src,tagList)
          }
        }
      }
      #if(length(ptRList)==0){
        #ptRList<-list(x=c())
      #}   
       user$code<-src
      #updateAceEditor( session,"source", value=src)
    }
  })
  
  #---selected point forward button-----
  observeEvent(input$forwardPt,{
    selection<-input$ptSet
    ptRList<-getPtDefs()$pts
    len<-length(ptRList[[selection ]])/2
    selectedPoint$point.index<-min(len, selectedPoint$point.index+1)
  })
  
  #---selected point backward button-----
  observeEvent(input$backwardPt,{
    #decrement selectedPointIndex
    selection<-input$ptSet
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
    selection<-input$ptSet
    ptDefs<-getPtDefs()
    ptsList<-ptDefs$pts
    dfList<-ptDefs$df
    ok=TRUE
    if(ok && is.null(dfList) ){
      message="Need to add tagR list prior to calling this" 
      session$sendCustomMessage(type='error', message=message )
      ok=FALSE
    } 
    if(ok && is.null(dfList[[selection]]) ){
      message=paste0("Need to add ", selection," to tagR list") 
      session$sendCustomMessage(type='error', message=message )
      ok=FALSE
    }
    if(ok ){
      len<-length(ptsList[[selection]])/2 #number of points in selection
      point.index<-max(1,selectedPoint$point.index) #can change later
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
          session$sendCustomMessage(type='testmessage', message=Err)
          src<-""
        } 
      }
      if(nchar(src)>0){
        #check source and update if ok
        src<-preProcCode(src)
        user$code<-src
        # if input$plotNavBar=="Points" or "Transform"
        #    get safe name and index for ptSet
        #    and if necessary, 
        #       set ptSet$selection<-name
        # if input$plotNavBar=="tagPts"
        #    get safe points and index for tagPts
        #    and if necessary
        #        set ptSet$selection<-name
        
        
        # point.index<-selectedPoint$point.index
        # selected<-input$ptSet
        # ptRList<-getPtDefs()$pts
        # res<-ex.getSelectInfo(ptRList, selected, point.index)
        # selectedPoint$index<-res$point.index
        # updateSelectInput(session, "ptSet", 
        #                   choices=names(ptRList), selected=res$selected )  
      #})
      }
    })
  })
#----BUTTON EVENTS END-----------------
  
  
  
#observers --------------------------
#--------------------------------navbarMenuBar--------
observeEvent( input$editNavBar, { 
  fileCmd<-input$editNavBar
  if(fileCmd=="New"){ #-----new
    txt<-codeTemplate
    user$code<-codeTemplate
    # the next  line update the ptRList; probably should redo with observer
    file$name<-"newSVG.R"
    selectedPoint$point.index<-0
    updateSelectInput(session, "ptSet",  choices=c("x"), selected="x" ) 
    updateNavbarPage(session, "editNavBar", selected ="Source") 
    updateNavbarPage(session, "plotNavBar", selected ="Points") 
  }
  if(fileCmd=="Open"){ #-----open 
    fileName=""
    try(fileName<-dlgOpen(title = "Select one R file", 
                          filters = dlgFilters[c("R", "All"), ])$res)
    if(length(fileName)>0){ 
      src<-paste(readLines(fileName), collapse = "\n")
      file$name<-fileName
      if(nchar(src)>0){
        src<-preProcCode(src)
        user$code<-src
        # point.index<-selectedPoint$point.index
        # selected<-input$ptSet
        # ptRList<-getPtDefs()$pts
        # res<-ex.getSelectInfo(ptRList, selected, point.index)
        # selectedPoint$point.index<-res$point.index
        # updateSelectInput(session, "ptSet",  choices=names(ptRList), selected=res$selected ) 
        
      }
    }
    updateNavbarPage(session, "editNavBar", selected ="Source")
    updateNavbarPage(session, "plotNavBar", selected ="Points") 
  }
  if(fileCmd=="Save"){ #-----save
    fileName=""
    default="newfile.R"
    try(fileName<-dlgSave(title = "Save R script to", 
                          filters = dlgFilters[c("R", "All"), ])$res
    )
    if(fileName!=""){ 
      file$name<-fileName
      txt<-user$code
      writeLines(txt, fileName)
      #updateAceEditor( session,"source", value=txt)
    }
    updateNavbarPage(session, "editNavBar", selected ="Source")
  }
})


#-----------------------MOUSE CLICKS---------------------------------
#todo: onmove get the new postion and update
observe({
  input$mydata #may want to rename this
  isolate({
     
    if(length(input$mydata)>0){
      #get cmd
      cmd<-input$mydata[1]
      pt<- input$mydata[2]
      src<-user$code
      #todo: error check???
      
      pt<-eval(parse(text=pt)) #we assume this is an array??
      ptRList<-getPtDefs()$pts
      if(cmd=='add'){ #---------add point
        newPt<-pt
        #get selection
        selection<-input$ptSet
        #update local ptRList
        indx<-selectedPoint$point.index
        ptRList[[selection]]<-append(ptRList[[selection]],newPt,2*indx)
      
          df<-NULL
          tagList<-getPtDefs()$df
          if(!is.null(tagList)){
            df<-tagList[[selection]]
            if(!is.null(df)){
              tags<-df$tag
              # locate the position of the new point
              #wrt the tags
              tags2move<-tags>indx
              if(length(tags2move)>0){
                tags[tags2move]<-1+ tags[tags2move]
                df$tag<-tags
                tagList[[selection]]<-df
                src<-df2Source(src,tagList)
              }
            }
          }
        #update point values
        selectedPoint$point.index<-selectedPoint$point.index+1
        src<-pts2Source(src,ptRList)
      } 
      if(cmd=='move'){ # --------move point
        id<-input$mydata[3]
        vid<-strsplit(id,"-")[[1]]
        #get selection
        selection<-vid[2]
        #get point index
        indx<-2*as.numeric(vid[3])-1
        #reassign point
        ptRList[[selection]][indx:(indx+1)]<-pt
        #update point values
        src<-pts2Source(src,ptRList)
      }
      #-------transformations 
      if(cmd=='trans'){ # -- translate
          tid<-input$mydata[3]
          tmp<-input$mydata[2]
          trDefDelta<-formatC(eval(parse(text=tmp)))
          trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" )
          src<-tr2src( src, tid, trDefDelta2 )
      }
      if(cmd=='rotate'){ # ----rotate
        tid<-input$mydata[3]
        tmp<-input$mydata[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" )
        src<-tr2src( src, tid, trDefDelta2 )
      } 
      if(cmd=='scale'){ # ----scale
        tid<-input$mydata[3]
        tmp<-input$mydata[2]
        trDefDelta<-formatC(eval(parse(text=tmp)))
        trDefDelta2<-paste0("matrix(c(",paste0(trDefDelta,collapse=", "), "),2,)" )
        src<-tr2src( src, tid, trDefDelta2 )
      } 
      # update internal user source
      user$code<-src
      
      #update editor
      # isolate( #no dependency on editor
      #   updateAceEditor( session,"source", value=src)
      # )
    }
  })
})

#---------BEGIN OUTPUT PANELS------------------------------------
#------fileName-------------
  output$fileName <- renderText({ 
    fileName<-file$name
    if(is.null(fileName) ){
      fileName==""
    }
    paste("Editing", basename(fileName))
  })
  
  
source("serverSVGHTML.R", local=TRUE)
#---------END OUTPUT PANELS------------------------------------

 
})
