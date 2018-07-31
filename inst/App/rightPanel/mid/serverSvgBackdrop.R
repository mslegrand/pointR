#--Backdrop-----------------------------------

backDropDB<-reactiveVal(
  tibble(
    tabId='bogus',
    color='white',
    checked=TRUE
  )
)

setBackDrop<-function(pageId, checked, color){
  if(!is.null(pageId)){
    tb<-backDropDB()
    tt<-as.list(filter(tb, tabId==pageId))
    if(length(tt$tabId)==0){
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
  }
}


observeEvent(input$pages,{
  bd<-getBackDrop()
  updateColourInput(session , inputId="backdropColour", value=bd$color)
  updateCheckboxInput(session, inputId="solidBackdrop", value=!bd$checked)
})

observeEvent(input$solidBackdrop,{
  checked<-!input$solidBackdrop
  setBackDrop(pageId=input$pages, checked=checked)
}, ignoreNULL=TRUE)

observeEvent(input$backdropColour,{
  color=input$backdropColour
  setBackDrop(pageId=input$pages, color=color)
}, ignoreNULL = TRUE)


getPageBackDrop<-function(pageId){
  tb<-backDropDB()
  if(!is.null(pageId) && pageId %in% tb$tabId){
    list(color=tb[tb$tabId==pageId,]$color, checked=tb[tb$tabId==pageId,]$checked )
  } else {
    list(color='white', checked=FALSE)
  }  
}

getBackDrop<-reactive({
  getPageBackDrop(input$pages)
})


