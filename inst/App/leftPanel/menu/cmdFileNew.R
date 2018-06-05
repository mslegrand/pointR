cmdFileNewPtR<-function(){
  src<-fileTemplates[[ "ptRTemplate.R" ]] #ptR template
  tabName<-getNextAnonymousFileName()
  #aceId<-tabName2AceId(tabId)
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptr')
  delay(500, 
    updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}

cmdFileNewR<-function(){
  src<-fileTemplates[[ "rTemplate.R" ]] #r Template
  tabName<-getNextAnonymousFileName()
  #aceId<-tabName2AceId(tabId)
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptr')
  delay(500, 
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}


cmdFileNewRmd<-function(){
  src<-fileTemplates[[ "rmdTemplate.Rmd" ]] #rmdTemplate
  #src<-rmdTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='markdown')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}

cmdNewIOSlides<-function(){
  src<-ioslidesTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='markdown')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}