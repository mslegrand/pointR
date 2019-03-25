# dnippetsDB has 2 entries: usage and paths
# paths consists of datapath and dname
# datapath tells where to look for dname
# dname should contain the short names for all availble dnippets
# usage is a tibble whose columns are tabId and the dnames
# for example: 'tabId', 'shapes', 'arrows'
# the tabId column consists of the ids of the pages
# the columns for the dnames are booleans

dnippetsDB<-reactiveValues(
  usage=tibble(tabId='bogus')[0,],
  paths=tibble(fullpath="datapath", dname="dnName" )[0,]
)

resetDnippetsDB<-function(){
  dnippetsDB$usage=tibble(tabId='bogus')[0,]
  dnippetsDB$paths=tibble(fullpath="datapath", dname="dnName" )[0,]
}

add2DnippetDBPath<-function(dnName, datapath){
  if(! datapath %in% dnippetsDB$paths$fullpath){ #donot add if already there
    dnippetsDB$paths<-bind_rows(dnippetsDB$paths, list(fullpath=datapath, dname=dnName ) )
  }
}

# Note:: Adds column to usage: this requires at least one page to be loaded
add2DnippetChoices<-function(dnName, value=TRUE){
  if(nrow(dnippetsDB$usage)>0){
    if(!dnName %in% names(dnippetsDB$usage)){
      val<-rep(value, max(1, nrow(dnippetsDB$usage)))
      dnippetsDB$usage[[dnName]]=val
    }
  } else {
  }
  saveDnippetsFileNames() # save loaded dnippets even if there are no pages
}

getPageDnippetsDB<-function(pageId){
  if(!is.null(pageId)){
    stopifnot('tabId' %in% names(dnippetsDB$usage))
    rtv<-as.list(filter(dnippetsDB$usage,tabId==pageId))
  } else {
    rtv<-NULL
  }
  rtv
}

getDnippetsUsageVec<-reactive({
  getPageDnippetsDB(input$pages)
})

getDnippetsSelected<-reactive({
  alles<-getDnippetsAll()
  val<-getPageDnippetsDB(input$pages)
  val[['tabId']]<-NULL
  val<-unlist(val)
  alles[val==TRUE]
})

getDnippetsAll<-reactive({
  dnippetsDB$paths$dname
})

setDnippetsSelected<-function(pageId, selected){
  # we require usageVec has full path
  nms<-dnippetsDB$paths$dname
  tmp<-rep_along(nms,FALSE)
  tmp[match(selected, nms, 0)]<-TRUE
  names(tmp)<-nms
  tbb<-c(list(tabId=pageId), as.list(tmp))
  stopifnot('tabId' %in% names(dnippetsDB$usage))
  tbc<-filter(dnippetsDB$usage, tabId!=pageId)
  dnippetsDB$usage<-bind_rows(tbc,tbb)
}

addNewPage2dnippetsDB<-function(pageId){
  log.fin(addNewPage2dnippetsDB)
  dn<-dnippetsDB$paths$dname
  if(length(pageId)>0 && !(pageId %in% dnippetsDB$usage$tabId)){
    tbb<-structure(as.list(rep_along(dn,TRUE)), names= dn)
    tbb[['tabId']]=pageId
    dnippetsDB$usage<-bind_rows(dnippetsDB$usage,tbb)
  }
  log.fout(addNewPage2dnippetsDB)
}

