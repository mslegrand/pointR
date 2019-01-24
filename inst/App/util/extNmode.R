


extMode.TB<-tibble(
  ext=c('R', 'Rmd', 'snippets', 'dnippets', 'txt', 'js'),
  mode=c('ptr', 'ptrrmd', 'snippets', 'dnippets', 'text', 'javascript')
)

pathExt2mode<-function(pathExt){
  mode<-extMode.TB[extMode.TB$ext==pathExt,]$mode
  if(length(mode)==0){
    mode='text'
  }
  return(mode)
}

mode2pathExt<-function(fileMode){
  if(!(fileMode %in% extMode.TB$mode)){
   'txt'
  } else {
    extMode.TB[extMode.TB$mode==fileMode,]$ext
  }
}