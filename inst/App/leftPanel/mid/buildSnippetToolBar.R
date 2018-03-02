


buildSnippetToolBar<-function(){
  lapply(toolbarSnippets, function(n){
    
    #title<-tag("svg", list(viewBox="0 0 32 32", tag("use", list(`xlink:href`="snippetIcons/ellipse.svg"))) )
    hint<-n[1]
    if(is.null(hint) || nchar(hint)==0){
      hint="no hint"
    }
    if(nchar(n[2])>0){
      iconImage<-img(src=paste0("snippetIcons/", n[2]))
    } else{
      iconImage<-'NA'
    }
    snp<-n[3]
    d<-div( class="snippetButton", draggable=TRUE, iconImage , color='blue',
           "data-snippet"= n[3]
      ) %>% bs_embed_tooltip(title = hint)
    jqui_draggabled(
      d,options=list(
        opacity= 0.5,
        stroke= "#FFFFFF",
        helper= 'clone',
        revert= 'invalid',
        appendTo= 'body'
      )
    )  #
  })
}


