
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
  point.index<-min(point.index, length( ptRList[[selected]])/2 )
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
  
  reactiveTag<-reactiveValues(freq=list())
  
  #errorText <-reactiveValues( mssg="hello") 
  
  init<-reactiveValues(val=0)                   #  kludge for initialization (shouldn't need this)
  mssg<-reactiveValues(error="")
  
# Reactive expressions------------- 
  getPtDefs<-reactive({ ex.getPtDefs(user$code) })  #extract points from user code


# TODO ------------------------------
#  If the user changes the code, (and deletes or adds a 
#  named data.tabele )then we  need to update reactiveTag$freq
#  This will happen only upon
#  1. code edit (submit)
#  2. new
#  3. load
# observe(user$code, 
#   isolate({
#     plotMode<-input$plotNavBar
#     if(plotMode=="points"){
#       # 1. pick out any tags that have disappered 
#       # and delete from reactiveTag$freq
#       # 2. pick out an new tags and add to reactiveTag$freq[[newTag]]<-0
#     }
#   })
# )


# If the user changes the name point Selection
# must update the tag selection.
#
#



#  If the user changes the tag freq selection
#  Need to update reactiveTag$freq
  
observe({
  input$tagFreq
  isolate({
    if(input$ptSet %in% names( reactiveTag$freq)){
      reactiveTag$freq[[input$ptSet]]<-input$tagFreq
    }
  })  
})
  
  
# If the user adds a point, use reactivTag$freq  
# observe(addingPt,
#   isolate({
#     if(pointName %in% namesreactiveTag$freq){
#       indxLast<-getIndx of last tag with pointName
#       indxPt<-getIndx of Pt with pointName
#       if(indxPt==(indxLast + reactiveTag$freq[[pointName]]){
#           tag this point
#       }
#     }
#   })
# )
  
  

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
        point.index<-selectedPoint$point.index
        selected<-input$ptSet
        ptRList<-getPtDefs()$pts
        res<-ex.getSelectInfo(ptRList, selected, point.index)
        selectedPoint$point.index<-res$point.index
        updateSelectInput(session, "ptSet",
                          choices=names(ptRList),
                          selected= res$selected )
    })
  })
  
  observe({
    input$ptSet
    isolate({
      ptRList<-getPtDefs()$pts
      selectedPoint$point.index<-length(ptRList[[input$ptSet]])
      if(input$ptSet %in% names( reactiveTag$freq) ){
        updateSelectInput(session, "tagFreq", 
                          selected=reactiveTag$freq[[input$ptSet]] )
      } else {
        updateSelectInput(session, "tagFreq", 
                          selected="Off" )
      }
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
  
    
#----BUTTON EVENTS BEGIN-----------------
source("serverButtons.R",local = TRUE)
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
    updateNavbarPage(session, "tagFreq", selected ="Off") 
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
      }
    }
    updateNavbarPage(session, "editNavBar", selected ="Source")
    updateNavbarPage(session, "plotNavBar", selected ="Points") 
    updateNavbarPage(session, "tagFreq", selected ="Off") 
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
    }
    updateNavbarPage(session, "editNavBar", selected ="Source")
  }
})


#-----------------------MOUSE CLICKS---------------------------------

observe({
  input$mydata #may want to rename this
  isolate({
     
    if(length(input$mydata)>0){
      #get cmd
      cmd<-input$mydata[1]
      pt<- input$mydata[2]
      src<-user$code
      #todo: error check???
      
      pt<-eval(parse(text=pt)) 
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
              tags2move<-which(tags>indx)
              if(length(tags2move)>0){
                tags[tags2move]<-1+ tags[tags2move]
                df$tag<-tags
                tagList[[selection]]<-df
                src<-df2Source(src,tagList)
              } else { #we are at the end
                freq<-reactiveTag$freq
                if(freq!='Off'){
                  freq<-as.integer(freq)
                  offset<-1+indx-tail(tags,1)
                  if(offset==freq){ # at the end and needs to be tagged
                    df2append<-tail(df,1)
                    df2append$tag<-1+indx
                    df<-rbind(df,df2append)
                    tagList[[selection]]<-df
                    src<-df2Source(src,tagList)
                  }
                }
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
        selectedPoint$point.index<-(indx+1)/2
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
  
# output$errorPanel <- renderText({ 
#     errorText$mssg
# })
  output$out_log <- renderText({
    mssg$error
  })

#---------END OUTPUT PANELS------------------------------------

 
})
