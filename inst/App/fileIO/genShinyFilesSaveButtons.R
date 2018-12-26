
  modes2filetypes=list(ptr="R",ptrrmd="Rmd",text="txt", snippets="snippets", dnippets="dnippets", javascript='js')

  getExtenstionList<-function(ext){
    tmp<-modes2filetypes
    names(tmp)<-unlist(tmp)
    tmp<-c(tmp[tmp==ext], tmp[tmp!=ext], '*'="")
    tmp
  }
  
  type2ExtensionList<-lapply( unlist(modes2filetypes), getExtenstionList )
  
  
  saveButtonFileNames<-paste0('buttonFileSave', modes2filetypes)
  names(saveButtonFileNames )<-names(modes2filetypes )
  
  saveButtonFileNames2<-saveButtonFileNames[-1]
  
  genShinySaveFilesButtons<-function(){
    saveButtonFileTypes<-names(type2ExtensionList)
    tmp<-lapply(1:length(saveButtonFileTypes), function(i){
      id=saveButtonFileNames[i] 
      filetype=type2ExtensionList[[i]]
      shinySaveButton( id, label="", title="Save as ...", filetype=filetype, class="hiddenButton")
    })
    saveButtons<-do.call(tagList, tmp)
    saveButtons
  }
  