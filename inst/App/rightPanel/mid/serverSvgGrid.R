# -----------Grid ------------- 

svgGridDB<-reactiveVal(
  tibble(
    tabId='bogus',
    show=FALSE,
    dx=50,
    dy=50,
    color='lightgrey'
  )[0,]

)

# getSvgGrid<-reactive({
#   as.list(svgGridDb())
#   reactiveValuesToList(svgGrid)
# })


getPageSvgGrid<-function(pageId){
  if(!is.null(pageId)){
    tb<-svgGridDB()
    rtv<-as.list(filter(tb, tabId==pageId))
    if(any(sapply(rtv, length)==0)){
      rtv<-list( tabId=pageId, show=FALSE, dx=50, dy=50, color='lightgrey')
    }
  } else {
    rtv<-list( tabId=pageId, show=FALSE, dx=50, dy=50, color='lightgrey')
  }
  return(rtv)
}

getSvgGrid<-reactive({
  getPageSvgGrid(input$pages)
})

setSvgGrid<-function(pageId, show,color,dx,dy){
  if(length(pageId)==0){
    return(NULL)
  }
  tb<-svgGridDB()
  tt<-as.list(filter(tb, tabId==pageId))
  if(length(tt$tabId)==0){ #default color if no row
    tt<-list(
      tabId=pageId,
      show=FALSE,
      dx=50,
      dy=50,
      color='lightgrey'
    )
  }
  if(!missing(show)){
    tt$show<-show
  }
  if(!missing(color)){
    tt$color<-color
  }
  if(!missing(dx)){
    tt$dx<-dx
  }
  if(!missing(dy)){
    tt$dy<-dy
  }
  tb<-filter(tb,tabId!=pageId)
  tb<-bind_rows(tb,tt)
  svgGridDB(tb)
  savePage(input$pages)
}


observeEvent( input$Hspacing, {
  # browser()
  dx<-as.numeric(input$Hspacing)
  setSvgGrid(pageId=input$pages, dx=dx)
})

observeEvent( input$Vspacing, {
  dy<-as.numeric(input$Vspacing)
  setSvgGrid(pageId=input$pages, dy=dy)
})

#color show done in cmd

observeEvent(input$pages,{
  # cat(">---> input$pages 5\n")
  tb<-getSvgGrid()
  if(length(tb$show)>0){
    if(tb$show){
      renameDMDM(session,  "plotNavBar", "cmdShowGrid", "Hide Grid", newValue="cmdHideGrid")
    }else{
      renameDMDM(session,  "plotNavBar",  "cmdHideGrid", "Show Grid",newValue="cmdShowGrid")
    }
  }
  # cat("<---< input$pages 5\n")
})


observeEvent(input$modalGridSpacingCancel, {
  removeModal()
}) 

observeEvent(input$modalGridSpacingOk, {
  setSvgGrid(
    pageId=input$pages,
    dx= as.numeric(input$selectGridDX),
    dy= as.numeric(input$selectGridDY)
  )
  editOption$tabSize<-input$selectIndentSize
  savePage(input$pages)
  removeModal()
})

