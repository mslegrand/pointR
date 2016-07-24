
#ui
output$PointsPanel<-renderUI({
  conditionalPanel( "input.plotNavBar=='Points'", 
    absolutePanel( top=50, left=0, width=650, draggable=TRUE,
      style=cstyle$wellPoint,
      fluidRow(
        column(4, 
          selectInput("ptRSelect", "Point Matrix", list("x"), 
            selected="x", multiple=FALSE,  selectize = FALSE,
            width="150px",size=1  )
        ),
        column(3,
          selectInput("ptDisplayMode", "Display Mode",
            list("Normal","Labeled","Hidden"), selected="Normal", 
            multiple=FALSE, selectize = FALSE,
            width="150px", size=1 )
        ),
        column(2,
          selectInput("tagFreq", "Auto Tag",
            c(list("Off"),1:20), selected="Off", 
            multiple=FALSE, selectize = FALSE,
            width="80px", size=1  )
        ),
        column(3,
          checkboxInput("insertMode","Insert",value = TRUE, width = "100px"),
          checkboxInput("showGrid", "Grid",   value = TRUE, width = "100px")
        )
      )) #end Points 
    )
})

#server
# -----------ACTIVE POINT MATRIX------------------------
#  observes code and plotNavBar
#  sets active Point, point selection,  and selectedPoint$point.index
observe({
user$code
  #input$plotNavBar
  name<-selectedPoint$name
  point.index<-selectedPoint$point.index  
  if(input$plotNavBar=='Points'){
    isolate({
      ptRList<-getPtDefs()$pts
      res<-ex.getSelectInfo(ptRList, name, point.index)
      selectedPoint$point.index<-res$point.index
      updateSelectInput(session, "ptRSelect",
                        choices=names(ptRList),
                        selected= res$selected )
    })
  }
})

# fires when ptRmatrix changes
observe({
  input$ptRSelect
  if(input$plotNavBar=='Points'){
    isolate({
      if(!is.null(input$ptRSelect)){
        selectedPoint$name<-input$ptRSelect
        #ptRList<-getPtDefs()$pts
  #      selectedPoint$point.index<-length(ptRList[[input$ptRSelect]])
  #      selected=reactiveTag$freq[[input$ptRSelect]] 
  #      if(is.null(selected)){
  #        selected<-"Off"
  #      }
  #      updateSelectInput(session, "tagFreq", selected=selected )
      }
    })
  }
})

observe({
  displayOptions$insertMode<-input$insertMode
  displayOptions$showGrid<-input$showGrid
  displayOptions$ptMode<-input$ptDisplayMode
  #showOptions$showGrid<-input$showGrid
})

#---------ShowPts----------------------------------

  # called when we need to show points (in epilog)
  # to do: rewrite to make this work with call for tags
  # where each tag is a group, so that we can edit a tag set 
  # to provide ability for translate, rotate, scale of points
  showPts.PtCmd %<c-% function(ptName, pts=NULL,  selectedPointIndx=NULL, ptDisplayMode="Normal"){
    if(is.null(pts) ){ return(NULL) } 
    if(is.null(ptDisplayMode) || ptDisplayMode=="Hidden"){ return(NULL) } 
    if(length(pts)<2 ){ return(NULL)}
    #print("entering showPts.PtCmd")
    selectedPointIndx<-as.numeric(selectedPointIndx)
    
    colorScheme<-c(default="green", ending="red", selected="blue")
    m<-matrix(pts,2) # is this really necessary????
     
    #form list of  all point renderings
    lapply(1:ncol(m), function(i){
      id<-paste("pd",ptName,i,sep="-")
      pt<-m[,i]
      color=colorScheme['default']
      if(i==length(pts)/2) { #ncol(m)){
          color=colorScheme['ending']   
      }
      list(
        if(identical(selectedPointIndx, as.numeric(i) )){
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=9, fill="yellow", 
                 opacity=1,
                 stroke=colorScheme['selected'], stroke.width=3,
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        } else { #a non-selected point
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=8, fill=color, opacity=1,
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        },
        if(ptDisplayMode=="Labeled"){
            text(paste(i), cxy=pt+10*c(1,-1),  
               stroke='black', font.size=12, opacity=1) 
        } else {
          NULL
        }
      )
    }) #end lapply
  } #end showPts.PtCmd
  


