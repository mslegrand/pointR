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
    dnippetsDB$paths<-add_row(dnippetsDB$paths, fullpath=datapath, dname=dnName  )
  }
}

# Note:: Adds column to usage: this requires at least one page to be loaded
add2DnippetChoices<-function(dnName, value=TRUE){
  dnippetsDB$usage<-add_column(dnippetsDB$usage, !!dName:=value)
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
  val<-names(which(val==TRUE))
  intersect(alles,val)
  # val[['tabId']]<-NULL
  # val<-unlist(val)
  # alles[val==TRUE]
})

getDnippetsAll<-reactive({
  dnippetsDB$paths$dname
})

setDnippetsSelected<-function(pageId, selected){
  # we require usageVec has full path
  nms<-dnippetsDB$paths$dname
  stopifnot('tabId' %in% names(dnippetsDB$usage))
  newRow<-c(list(tabId=pageId), as.list(sapply(nms, function(x)any(grepl(x,selected)))))
  tbc<-filter(dnippetsDB$usage, tabId!=pageId)
  dnippetsDB$usage<-bind_rows(tbc,newRow)
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

