


# snippets<-as.list(letters)
# names(snippets)<-snippets
# load snippets and iconts

snippets<-toolbarSnippets

buildSnippetToolBar<-function(){
  lapply(snippets, function(n){
    title<-n[1]
    snp<-n[3]
    jqui_draggabled(
      div( class="snippetButton", draggable=TRUE, title ,
           "data-snippet"= n[3]
# "if (${1:condition_name}) {
# 	${2:body}
#}"
             #snippets[[n]] 
      ),options=list(
        opacity= 0.5,
        stroke= "#FFFFFF",
        helper= 'clone',
        revert= 'invalid',
        appendTo= 'body'
      )
    )
  })
}


