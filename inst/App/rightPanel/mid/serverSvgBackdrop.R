#--Backdrop-----------------------------------

backDropDB<-reactiveVal(
  tibble(
    tabId='bogus',
    color='white',
    checked=TRUE
  )[0,]
)

setBackDrop<-function(pageId, checked, color){
  if(!is.null(pageId)){
    tb<-backDropDB()
    tt<-as.list(filter(tb, tabId==pageId))
    if(length(tt$tabId)==0){ #default color if no row
      tt<-list(
        tabId=pageId,
        color='white',
        checked=FALSE
      )
    } 
    if(!missing(color)){ tt$color=color}
    if(!missing(checked)){tt$checked=checked}
    tb<-filter(tb,tabId!=pageId)
    tb<-bind_rows(tb,tt)
    backDropDB(tb)
    savePage(pageId)
  }
}


observeEvent(input$pages,{
  # cat(">---> input$pages 4\n")
  if(length(input$pages)>0){
      
    # cat('input$pages=',format(input$pages),"\n")
    
      bd<-getPageBackDrop(input$pages)
      # print(bd)
    updateColourInput(session , inputId="backdropColour", value=bd$color)
    updateCheckboxInput(session, inputId="solidBackdrop", value=!bd$checked)
  }

  # cat("<---< input$pages 4\n")
}, ignoreNULL=TRUE)

observeEvent(input$solidBackdrop,{
  checked<-!input$solidBackdrop
  setBackDrop(pageId=input$pages, checked=checked)
}, ignoreNULL=TRUE)

observeEvent(input$backdropColour,{
  color=input$backdropColour
  setBackDrop(pageId=input$pages, color=color)
}, ignoreNULL = TRUE)


getPageBackDrop<-function(pageId){
  if(!is.null(pageId)){
    tb<-backDropDB()
    rtv<-as.list(filter(tb, tabId==pageId))
    if(any(sapply(rtv, length)==0)){
      rtv<-list(tabId=pageId, color='white', checked=FALSE)
    }
  } else {
    rtv<-list(tabId=pageId, color='white', checked=FALSE)
  }
  return(rtv)
}

getBackDrop<-reactive({
  getPageBackDrop(input$pages)
})


