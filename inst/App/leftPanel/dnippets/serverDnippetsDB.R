dnippetsDB<-reactiveValues(
  usage=tibble(
    tabId='bogus'
  )[0,],
  paths=tibble(
  )
)

# addToDnippetsFiles adds filePath to editOption$dnippetsFiles
 
# addDnippet2DB<-function(filePath){
#   dname<-basename(filePath)
#   if(!dname %in% names(dnippetsDB$usage)){
#       val<-rep(TRUE, max(1, nrow(dnippetsDB$usage)))
#       dnippetsDB$usage[[dname]]=val
#       dnippetsDB$paths<-bindrows(dnippetsDB$paths, list(fullpath=filePath, dname=dname ) )(file)
#   }
# }

addDnippetPath2DB<-function(dnName, datapath){
  cat('entering addDnippetPath2DB\n')
  dnippetsDB$paths<-bind_rows(dnippetsDB$paths, list(fullpath=datapath, dname=dnName ) )
  cat('exiting addDnippetPath2DB\n')
}

add2DnippetChoices<-function(dnName, value=TRUE){
  cat('entering add2DnippetChoices\n')
  if(nrow(dnippetsDB$usage)>0){
    if(!dnName %in% names(dnippetsDB$usage)){
      val<-rep(value, max(1, nrow(dnippetsDB$usage)))
      dnippetsDB$usage[[dnName]]=val
    }  
  }
  cat('exiting add2DnippetChoices\n')
}

getPageDnippetsDB<-function(pageId){
  cat('entering getPageDnippetsDB\n')
  if(!is.null(pageId)){
    #browser()
    rtv<-as.list(filter(dnippetsDB$usage,tabId==pageId))
    rtv[['tabId']]<-NULL
  } else {
    rtv<-NULL
  }
  cat('exiting getPageDnippetsDB(',format(pageId), ') with rtv=',format(rtv),'\n')
  rtv
}

getDnippetsUsageVec<-reactive({
  cat('entering getDnippetsUsageVec\n')
  getPageDnippetsDB(input$pages)
})

getDnippetsSelected<-reactive({
  cat('entering getDnippetsSelected\n')
  all<-dnippetsDB$paths$dname
  val<-unlist(getPageDnippetsDB(input$pages))
  all[val==TRUE]
})

getDnippetsAll<-reactive({
  dnippetsDB$paths$dname
})

setDnippetsSelected<-function(pageId, selected){
  # we require usageVec has full path
  #browser()
  cat('entering setDnippetsSelected\n')
  #browser()
  
  nms<-dnippetsDB$paths$dname
  tmp<-rep_along(nms,FALSE)
  tmp[match(selected, nms, 0)]<-TRUE
  names(tmp)<-nms
  tbb<-c(list(tabId=pageId), as.list(tmp))
  tbc<-filter(dnippetsDB$usage, tabId!=pageId)
  dnippetsDB$usage<-bind_rows(tbc,tbb)
}

addNewPage2dnippetsDB<-function(pageId){
  #tbb<-sapply(names(dnippetsDB$usage), function(x)TRUE)
  #browser()
  dn<-dnippetsDB$paths$dname
  cat('entering addNewPage2dnippetsDB\n')
  if(length(pageId)>0 && !(pageId %in% dnippetsDB$usage$tabId)){
    #browser()
    tbb<-structure(as.list(rep_along(dn,TRUE)), names= dn)
    tbb[['tabId']]=pageId
    dnippetsDB$usage<-bind_rows(dnippetsDB$usage,tbb)
  }
 
}

