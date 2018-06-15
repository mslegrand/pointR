cmdFileNewPtR<-function(fileCmd="newPtrTibScript"){
    templateChoices<-list(
      newPtrTibScript=  "ptRTemplate.R",
      newPtRMatScript = "matTemplate.R",
      newPtRSVGScript = "svgRTemplate.R",
      newRScript      = "rTemplate.R"
    )
    templateName<-templateChoices[[fileCmd]]
    src<-fileTemplates[[ templateName ]] #ptR template
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
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}

cmdNewIOSlides<-function(){
  src<-ioslidesTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', setValue= src, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}