
saveButtonFileNames<-setNames(paste0('buttonFileSave', extMode.TB$ext), extMode.TB$mode )

UIGenShinySaveFilesButtons<-function(){
  tt<-setNames(as.list(extMode.TB$ext),extMode.TB$mode)
  fileTypes<-lapply(tt, function(x){
    xx<-c(x,tt[tt!=x]) #move the prefered mode to the top
    setNames(as.list(xx),xx)
  }) 
  tmp<-lapply(extMode.TB$mode, function(mode){
    id=saveButtonFileNames[mode]
    filetype=fileTypes[[mode]]
    shinySaveButton( id=id, label="", title="Save as ...", filetype=filetype, class="hiddenButton")
  })
  saveButtons<-do.call(tagList, tmp)
  saveButtons
}
  