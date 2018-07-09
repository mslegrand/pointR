


  modes2filetypes=list(ptr="R",ptrrmd="Rmd",text="txt", snippets="snippets", markdown="dnippets")
  
  getExtenstionList<-function(ext){
    tmp<-modes2filetypes
    names(tmp)<-unlist(tmp)
    tmp<-c(tmp[tmp==ext], tmp[tmp!=ext], '*'="")
    tmp
  }
  
  type2ExtensionList<-lapply( unlist(modes2filetypes), getExtenstionList )
  
  
  saveButtonFileNames<-paste0('buttonFileSave', modes2filetypes)
  names(saveButtonFileNames )<-names(modes2filetypes )
  
  genShinySaveFilesButtons<-function(){
    saveButtonFileTypes<-names(type2ExtensionList)
    tmp<-lapply(1:length(saveButtonFileTypes), function(i){
      id=saveButtonFileNames[i] #paste0('buttonFileSave',ext)
      filetype=type2ExtensionList[[i]]
      shinySaveButton( id, label="", title="Save as ...",    filetype=filetype, class="hiddenButton")
    })
    saveButtons<-do.call(tagList, tmp)
    saveButtons
  }
  