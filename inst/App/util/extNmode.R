


extMode.TB<-tibble(
  ext=c('R', 'Rmd', 'snippets', 'dnippets', 'txt'),
  mode=c('ptr', 'ptrrmd', 'snippets', 'dnippets', 'txt')
)

pathExt2mode<-function(pathName){
  mode<-extMode.TB[extMode.TB$ext==fileExt,]$mode
  if(length(mode)==0){
    mode='txt'
  }
  return(mode)
}

mode2pathExt<-function(fileMode){
  extMode.TB[extMode.TB$mode==fileMode,]$ext
}