


extMode.TB<-tibble(
  ext=c('R', 'Rmd', 'snippets', 'dnippets', 'txt', 'js'),
  mode=c('ptr', 'ptrrmd', 'snippets', 'dnippets', 'txt', 'javascript')
)

pathExt2mode<-function(pathExt){
  mode<-extMode.TB[extMode.TB$ext==pathExt,]$mode
  if(length(mode)==0){
    mode='txt'
  }
  return(mode)
}

mode2pathExt<-function(fileMode){
  extMode.TB[extMode.TB$mode==fileMode,]$ext
}