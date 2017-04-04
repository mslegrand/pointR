
#manages the editor instances and content history

# The last entry in the editorNames will be the editor in focus (if there multiple instances)
# The last entry of the stack will be the most recent commit for that editor
src<-reactiveValues(
  limit=2, #this probably should be a constant, but needed somewhere safe to put this
  editorNames='history', # last is current editor
  history=list(
    type="svgRCode",
    stack=codeTemplate,
    index=1
  )
)

#-------- editor list management ------
srcGetEditorNames<-reactive({
  src$editorNames
})

srcSetCurrentEditor<-function(edName){
  if(edName %in% src$editorNames){
    pos<-which(edName==src$editorNames)
    src$editorNames<-c(src$editorNames[-pos],edName)
  }
}

srcGetCurrentEditor<-reactive({
  tail(src$editorNames,1)
})

srcAddEditor<-function(editor, value="default src", type="svgRSource"){
  if(!(editor %in% srcGetEditorNames() )){
    src[[editor]]<-list(
      type="svgRCode",
      stack=value,
      index=1
    )
    src$editorNames<-c(src$editorNames, editor)
    srcSetCurrentEditor(editor)
  }
}

srcRemoveEditor<-function(editor){
  if(editor %in% src$editorNames){
    pos<-which(editor==src$editorNames)
    src$editorNames<-src$editorNames[-pos]
    src[[editor]]<-NULL
  }
}


#---------

srcPushTxt<-function(what, edName="history"){
  length(src[[edName]]$stack)<-src[[edName]]$index
  src[[edName]]$stack<-c(src[[edName]]$stack,what)
  if(length(src[[edName]]$stack)>src$limit){
    src[[edName]]$stack<-tail(src[[edName]]$stack, src$limit)
  }
  src[[edName]]$index<-length(src[[edName]]$stack)
}

srcBackup<-function(edName="history"){
  src[[edName]]$index<-max(1, src[[edName]]$index-1)
}

srcGet<-reactive({ #return code on top of stack
  edName<-srcGetCurrentEditor()
  tail(src[[edName]]$stack,1)
})


srcGoForward<-function(edName="history"){
  src[[edName]]$index<-min(
    length(src[[edName]]$stack), 
    src[[edName]]$index+1
  )
}

srcGo2End<-function(edName="history"){
  src[[edName]]$index<-length(src[[edName]]$stack)
}

srcGo2Beginning<-function(edName="history"){
  src[[edName]]$index<-1
}

srcRevert<-function(edName="history" ){
  src[[edName]]$index<-max(1, src[[edName]]$index-1)
  length(src[[edName]]$stack)<-src[[edName]]$index
}

