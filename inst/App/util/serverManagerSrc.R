
# #manages the editor instances and content history
# 
# # The last entry in the editorNames will be the editor in focus (if there multiple instances)
# # The last entry of the stack will be the most recent commit for that editor
# serverManagerSrc<-reactiveValues(
#   limit=1, #this probably should be a constant, but needed somewhere safe to put this
#   editorNames='history', # last is current editor
#   history=list(
#     type="svgRCode",
#     stack=codeTemplate,
#     index=1
#   )
# )
# 
# #-------- editor list management ------
# srcGetEditorNames<-reactive({
#   serverManagerSrc$editorNames
# })
# 
# srcSetCurrentEditor<-function(edName){
#   if(edName %in% serverManagerSrc$editorNames){
#     pos<-which(edName==serverManagerSrc$editorNames)
#     serverManagerSrc$editorNames<-
#       c(serverManagerSrc$editorNames[-pos],edName)
#   }
# }
# 
# srcGetCurrentEditor<-reactive({
#   names<-serverManagerSrc$editorNames
#   tail(names,1)
# })
# 
# srcAddEditor<-function(editor, value="default src", type="svgRSource"){
#   if(!(editor %in% srcGetEditorNames() )){
#     serverManagerSrc[[editor]]<-list(
#       type="svgRCode",
#       stack=value,
#       index=1
#     )
#     serverManagerSrc$editorNames<-c(serverManagerSrc$editorNames, editor)
#     srcSetCurrentEditor(editor)
#   }
# }
# 
# srcRemoveEditor<-function(editor){
#   if(editor %in% serverManagerSrc$editorNames){
#     pos<-which(editor==serverManagerSrc$editorNames)
#     serverManagerSrc$editorNames<-serverManagerSrc$editorNames[-pos]
#     serverManagerSrc[[editor]]<-NULL
#   }
# }
# 
# 
# #---------
# 
# srcPushTxt<-function(what, edName="history"){
#   length(serverManagerSrc[[edName]]$stack)<-serverManagerSrc[[edName]]$index
#   serverManagerSrc[[edName]]$stack<-c(serverManagerSrc[[edName]]$stack,what)
#   if(length(serverManagerSrc[[edName]]$stack)>serverManagerSrc$limit){
#     serverManagerSrc[[edName]]$stack<-tail(serverManagerSrc[[edName]]$stack, serverManagerSrc$limit)
#   }
#   serverManagerSrc[[edName]]$index<-length(serverManagerSrc[[edName]]$stack)
# }
# 
# srcBackup<-function(edName="history"){
#   #serverManagerSrc[[edName]]$index<-max(1, serverManagerSrc[[edName]]$index-1)
# }
# 
# srcGet<-reactive({ #return code on top of stack
#   edName<-srcGetCurrentEditor()
#   stack<-serverManagerSrc[[edName]]$stack
#   #serverManagerSrc[[edName]]$stack[[length(serverManagerSrc[[edName]]$stack)]]
#   #cat(stack[length(stack)] )
#   stack[length(stack)]
# })
# 
# 
# srcGoForward<-function(edName="history"){
#   serverManagerSrc[[edName]]$index<-min(
#     length(serverManagerSrc[[edName]]$stack), 
#     serverManagerSrc[[edName]]$index+1
#   )
# }
# 
# srcGo2End<-function(edName="history"){
#   serverManagerSrc[[edName]]$index<-length(serverManagerSrc[[edName]]$stack)
# }
# 
# srcGo2Beginning<-function(edName="history"){
#   serverManagerSrc[[edName]]$index<-1
# }
# 
# srcRevert<-function(edName="history" ){
#   serverManagerSrc[[edName]]$index<-max(1, serverManagerSrc[[edName]]$index-1)
#   length(serverManagerSrc[[edName]]$stack)<-serverManagerSrc[[edName]]$index
# }
# 
