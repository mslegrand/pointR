


buildSnippetToolBar<-function(){
  cat('===========buildSnippetToolBar==================')
  temp<-lapply(toolbarSnippets, function(n){
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
    cat('===========snp1==================')
    print(snp)
    d<-tags$li(id=sample(letters,12), class="snippetButton", draggable=TRUE, iconImage , color='blue',
           "data-snippet"= n[3]
      ) %>% bs_embed_tooltip(title = hint)
    #browser()
    jqui_draggable(
      d,options=list(
        opacity= 0.5,
        stroke= "#FFFFFF",
        helper= 'clone',
        revert= 'invalid',
        appendTo= 'body'
      )
    )  #
  })
  # jqui_draggable(
  #   ".snippetButton", operation="enable", options=list(
  #     opacity= 0.5,
  #     stroke= "#FFFFFF",
  #     helper= 'clone',
  #     revert= 'invalid',
  #     appendTo= 'body'
  #   )
  #)
  #temp<-NULL
#browser()
  paste(sapply(temp, as.character), collapse="\n")
  temp
}

