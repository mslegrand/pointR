
useTribbleFormatDB<-reactiveVal(
  tibble(
    tabId='bogus',
    value=TRUE
  )[0,]
)

getPageUseTribble<-function(pageId){
  if(!is.null(pageId)){
    tb<-useTribbleFormatDB()
    rtv<-as.list(filter(tb, tabId==pageId))
    #if(length(rtv)>0 && any(sapply(rtv, length)==0)){
    if( any(sapply(rtv, length)==0)){
      rtv<-list(value=TRUE)
    } 
  } else{
    rtv<-list(value=TRUE)
  }
  rtv
}

getUseTribble<-reactive({
  pageId<-input$pages
  tb<-useTribbleFormatDB()
  if(is.null(pageId)){
    TRUE
  } else if(pageId %in% tb$tabId){
      tb[tb$tabId==pageId,]$value
  } else { 
    TRUE
  }
})

setUseTribble<-function(pageId, value){
  if(!is.null(pageId)){
    tb<-useTribbleFormatDB()
    tt<-filter(tb, tabId==pageId)
    if(nrow(tt)>0){
      tt$value=value
    } else {
      tt<-tibble(tabId=pageId, value=value)
    }
    tb<-bind_rows( filter(tb, tabId!=pageId),  tt)
    useTribbleFormatDB(tb)
  }
}

observeEvent(input$useTribble,{
  useTribble<-ifelse(input$useTribble=='Tribble',TRUE,FALSE)
  if(!identical(useTribble,getUseTribble() )){
    setUseTribble(pageId=input$pages,value=useTribble)
    newPtDefs<-getPtDefs()
    sender='useTibble'
    updateAceExtDef(newPtDefs, sender=sender)
  }
})


observeEvent(input$pages,{
  value<-getUseTribble()
  choice<-ifelse(value, "Tribble", "Tibble")
  updateAwesomeRadio(session, inputId="useTribble",  selected=choice)
}, ignoreNULL = TRUE)



