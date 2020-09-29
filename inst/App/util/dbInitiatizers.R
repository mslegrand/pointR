
initialFileDescDB<-function(){tibble(
  tabId='bogus',
  isSaved=FALSE,
  filePath="?",
  anonNo =1,
  mode='ptr'
)[0,]
}


initialPreProcScriptDB<-function(){
  tibble( scriptName='bogus', cmd="bogus", script='bogus')[0,]
}

initialPreProcPageDB<-function(){
  tibble( tabId="bogus", tibName="bogus", colName='bogus', scriptName='bogus')[0,]
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
    ptScriptSel=preprocChoices$points[1]
  )
}

initialServerAssetDB<-function(){
  initialServerAsset()[0,]
}

initialWidgetDB<-function(){
  tibble(tabId='Tab0', name='x',column='y',type='character',minVal=NA, maxVal=NA,step=1, selectedWidget='radio')[0,]
}

initialRowGroupDB<-function(){
  tibble(tabId='Tab0', name='x',rows=0, colName='col')[0,]
}

