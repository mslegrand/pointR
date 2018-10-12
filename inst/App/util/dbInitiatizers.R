
initialFileDescDB<-function(){tibble(
  tabId='bogus',
  isSaved=FALSE,
  filePath="?",
  anonNo =1,
  mode='ptr'
)[0,]
}


# initialDnippetsDBUsage<-function(){
#   tibble(tabId='bogus')[0,]
# }

initialPreprocDB<-function(){
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,]
  points
}

initialTribbleDB<-function(){
  tibble(
    tabId='bogus',
    show=FALSE,
    dx=50,
    dy=50,
    color='lightgrey'
  )[0,]
}

initialSvgGridDB<-function(){
  tibble(
    tabId='bogus',
    show=FALSE,
    dx=50,
    dy=50,
    color='lightgrey'
  )[0,]
}

initialBackDropDB<-function(){
  tibble(
    tabId='bogus',
    color='white',
    checked=TRUE
  )[0,]
}

#preprocChoices<-c("onNewPt",  "onMovePt", "onMoveMat")

initialServerAsset<-function(){
  tibble(
    tabId="bogus",
    name="bogus",        # name of current point array
    rowIndex=1,
    columnName="bogus", # currently used only by tibbleEditor and could be placed there.
    matCol=0, #
    ptColName="bogus",      # !!! KLUDGE for now. should this default to last col?
    selIndex=1, # only used is to determine if in matrix or point mode !! 
    transformType='Translate', # TODO!!! replace this with selIndex
    ptScriptSel=preprocChoices[1]
  )
}

initialServerAssetDB<-function(){
  # tmp<-initialServerAsset()
  # tmp[sapply(tmp, is.null)]<-'NULL' # need to cast as character.
  initialServerAsset()[0,]
}



