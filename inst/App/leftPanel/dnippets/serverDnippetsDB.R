# dnippetsDB has 2 entries: usage and paths
# paths consists of datapath and dname
# datapath tells where to look for dname
# dname should contain the short names for all availble dnippets
# usage is a tibble whose columns are tabId and the dnames
# for example: 'tabId', 'shapes', 'arrows'
# the tabId column consists of the ids of the pages
# the columns for the dnames are booleans

dnippetsDB<-reactiveValues(
  usage=tibble(tabId='bogus')[0,]#,
)

resetDnippetsDB<-function(){
  dnippetsDB$usage=tibble(tabId='bogus')[0,]
  dnippetSelection$all=list()
}



# Note:: Adds column to usage: this requires at least one page to be loaded
add2DnippetChoices<-function(dndName, value=TRUE){
  # refuse id dnName is already there
  if(!dndName %in% names(dnippetsDB$usage)){
      dnippetsDB$usage<-add_column(dnippetsDB$usage, !!dndName:=value)
  }
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
  val<-names(which(val==TRUE))
  intersect(alles,val)
})

getDnippetsAll<-reactive({
  names(dnippetSelection$all)
})

setDnippetsSelected<-function(pageId, selected){
  # we require usageVec has full path
  nms<-getDnippetsAll()
  stopifnot('tabId' %in% names(dnippetsDB$usage))
  newRow<-c(list(tabId=pageId), as.list(sapply(nms, function(x)any(grepl(x,selected)))))
  tbc<-filter(dnippetsDB$usage, tabId!=pageId)
  dnippetsDB$usage<-bind_rows(tbc,newRow)
}

# by default all dnippets are selected for page not in usage
addNewPage2dnippetsDB<-function(pageId){
  log.fin(addNewPage2dnippetsDB)
  dn<-getDnippetsAll()
  if(length(pageId)>0 && !(pageId %in% dnippetsDB$usage$tabId)){
    tbb<-structure(as.list(rep_along(dn,TRUE)), names= dn)
    tbb[['tabId']]=pageId
    dnippetsDB$usage<-bind_rows(dnippetsDB$usage,tbb)
  }
  log.fout(addNewPage2dnippetsDB)
}

