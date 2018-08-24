dnippetsDB<-reactiveValues(
  usage=tibble(
    tabId='bogus'
  )[0,],
  paths=tibble(
  )
)

resetDnippetsDB<-function(){
  dnippetsDB$usage=tibble(tabId='bogus')[0,]
  dnippetsDB$paths=tibble(fullpath="datapath", dname="dnName" )[0,]
}

# resetDnippetsDB() # on startup

# addToDnippetsFiles adds filePath to editOption$dnippetsFiles
 
# addDnippet2DB<-function(filePath){
#   dname<-basename(filePath)
#   if(!dname %in% names(dnippetsDB$usage)){
#       val<-rep(TRUE, max(1, nrow(dnippetsDB$usage)))
#       dnippetsDB$usage[[dname]]=val
#       dnippetsDB$paths<-bindrows(dnippetsDB$paths, list(fullpath=filePath, dname=dname ) )(file)
#   }
# }

add2DnippetDBPath<-function(dnName, datapath){
  # cat('entering add2DnippetDBPath\n')
  dnippetsDB$paths<-bind_rows(dnippetsDB$paths, list(fullpath=datapath, dname=dnName ) )
  # cat('exiting add2DnippetDBPath\n')
}

# Note:: Adds column to usage: this requires at least one page to be loaded
add2DnippetChoices<-function(dnName, value=TRUE){
  # cat('\n>----> entering add2DnippetChoices\n')
  if(nrow(dnippetsDB$usage)>0){
    if(!dnName %in% names(dnippetsDB$usage)){
      val<-rep(value, max(1, nrow(dnippetsDB$usage)))
      dnippetsDB$usage[[dnName]]=val
    }
  } else {
  }
  saveDnippetsFileNames() # save loaded dnippets even if there are no pages
  # cat('<----< exiting add2DnippetChoices\n\n')
}

getPageDnippetsDB<-function(pageId){
  # cat('\n>----> entering getPageDnippetsDB with pageId=',format(pageId),'\n')
  if(!is.null(pageId)){
    rtv<-as.list(filter(dnippetsDB$usage,tabId==pageId))
  } else {
    rtv<-NULL
  }
  # cat('<----< exiting getPageDnippetsDB(',format(pageId), ') with rtv=',format(rtv),'\n\n')
  rtv
}

getDnippetsUsageVec<-reactive({
  # cat('>---> entering getDnippetsUsageVec\n')
  getPageDnippetsDB(input$pages)
})

getDnippetsSelected<-reactive({
  # cat('\n>---> entering getDnippetsSelected\n')
  alles<-getDnippetsAll()
  val<-getPageDnippetsDB(input$pages)
  val[['tabId']]<-NULL
  val<-unlist(val)
  #cat('<---<  getDnippetsSelected\n')
  alles[val==TRUE]
})

getDnippetsAll<-reactive({
  dnippetsDB$paths$dname
})

setDnippetsSelected<-function(pageId, selected){
  # we require usageVec has full path
  # cat('\n>---> entering setDnippetsSelected\n')
  nms<-dnippetsDB$paths$dname
  tmp<-rep_along(nms,FALSE)
  tmp[match(selected, nms, 0)]<-TRUE
  names(tmp)<-nms
  tbb<-c(list(tabId=pageId), as.list(tmp))
  tbc<-filter(dnippetsDB$usage, tabId!=pageId)
  dnippetsDB$usage<-bind_rows(tbc,tbb)
  # cat('<---< entering setDnippetsSelected\n\n')
}

addNewPage2dnippetsDB<-function(pageId){
  dn<-dnippetsDB$paths$dname
  # cat('\n>---> entering addNewPage2dnippetsDB\n')
  if(length(pageId)>0 && !(pageId %in% dnippetsDB$usage$tabId)){
    
    tbb<-structure(as.list(rep_along(dn,TRUE)), names= dn)
    tbb[['tabId']]=pageId
    dnippetsDB$usage<-bind_rows(dnippetsDB$usage,tbb)
  }
  # cat('<---< exiting addNewPage2dnippetsDB\n\n')
}

