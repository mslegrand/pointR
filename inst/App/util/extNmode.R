# defines global extMode.TB 


# extMode.TB is used in ui.R by UIGenShinySaveFilesButtons

extMode.TB<-tibble(
  ext=c('R', 'Rmd', 'snip', 'dnds', 'txt', 'js', 'css'),
  mode=c('ptr', 'ptrrmd', 'snippets', 'dnippets', 'text', 'javascript', 'css')
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

importExt<-tibble(
  name=c('dnds', 'preProcPts', 'snip'),
  ext=c('dnds', 'R', 'snip'),
  path=c('dnds', 'preProcPts', 'snip')
)

exportExt<-tibble(
  name=c( 'PreProcPts', 'SVG'),
  ext=c( 'R', 'svg'),
  path=c( 'preProcPts', '')
)