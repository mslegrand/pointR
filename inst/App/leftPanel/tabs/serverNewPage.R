  

# gets what we need from fileDescDB()
newPage<-function(tabId, title, txt, docFilePath, mode,  fileSaveStatus, link=NULL ){
  # fileSaveStatus is boolean
  log.fin(newPage)
  if(is.null(tabId)){
    cat('big probs\n')
    stop()
  }
  aceId<-tabID2aceID(tabId)
  if(mode=='javascript'){
    divClass="cAceContainer"
  } else if(mode=='ptr' && !title %in% c('app.R','App.R')){
    divClass="cAceContainer"
  } else {
    divClass="cAceRmdContainer"
  }
  # print("**** newPage link")
  # print(link)
  appendTab(
    inputId = "pages", select=TRUE,
    tabPanel(
      title=tabTitleRfn(title, tabId, docFilePath, fileSaveStatus), # maybe we should save title in fileDescriptor?
      div(
        class=divClass,
        overflow= "hidden",inline=FALSE,
        shinyAce4Ptr(
          outputId = aceId,
          value    = txt,
          mode     = mode,
          theme    = editOption$theme, 
          fontSize = as.numeric(editOption$fontSize),
          tabSize  = as.numeric(editOption$tabSize),
          whiteSpace = editOption$whiteSpace,
          autoComplete="enabled",
          if(mode=='ptr'){
            autoCompleteList =list(names(svgR:::eleDefs))
          } else { # handle js mode, ... here
            NULL
          },
          docFilePath =docFilePath,
          initSaved   =fileSaveStatus,
          link=link
        )
      ),
      value=tabId
    )
  )
  log.fout(newPage)
  aceId
} 

