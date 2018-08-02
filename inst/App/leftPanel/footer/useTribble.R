
useTribbleFormatDB<-reactiveVal(
  tibble(
    tabId='bogus',
    value=TRUE
  )[0,]
)

getPageUseTribble<-function(pageId){
  cat('getPageUseTribble:: pageId=', format(pageId),"\n")
  if(!is.null(pageId)){
    tb<-useTribbleFormatDB()
    cat('getPageUseTribble:: tibble=')
    print(tb)
    rtv<-as.list(filter(tb, tabId==pageId))
    cat('rtvAslist=')
    print(rtv)
    if(any(sapply(rtv, length)==0)){
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
  cat("setUseTribble:: pageId=",format(pageId), " value= ", format(value) , "\n")
  if(!is.null(pageId)){
    #browser()
    cat("setUseTribble2:: pageId=",format(pageId), " value= ", format(value) , "\n")
    tb<-useTribbleFormatDB()
    tt<-filter(tb, tabId==pageId)
    if(nrow(tt)>0){
      tt$value=value
    } else {
      tt<-tibble(tabId=pageId, value=value)
    }
    tb<-bind_rows( filter(tb, tabId!=pageId),  tt)
    print(tb)
    useTribbleFormatDB(tb)
  }
}

observeEvent(input$useTribble,{
  useTribble<-ifelse(input$useTribble=='Tribble',TRUE,FALSE)
  if(!identical(useTribble,getUseTribble() )){
    #editOption$useTribbleFormat=useTribble
    #browser()
    cat('useTribble =', useTribble, "\n")
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



