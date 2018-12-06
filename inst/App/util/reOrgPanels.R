
reOrgPanels<-function(id, mode){
  cat(">---> reOrgPanels\n")
  # cat('id=',format(id),"\n")
  # cat('mode=',format(mode),"\n")
  if(length(id)==0 || length(mode)==0){
    hideElement("TopRightPanel")
    hideElement("snippetToolBarContainer")
    hideElement("aceToobarTop1")
    hideElement("aceToobarTop2")
    hideElement("useTribble")
    hideElement("commitButton")
    addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
    hideElement("aceTabSet")
    hideElement("midRightPanels")
    hideElement("BottomRightPanel")
    showElement("logo.right")
    showElement("logo.left")
  } else {
    hideElement("logo.right")
    hideElement("logo.left")
    # editing ptr
    if(identical(mode,'ptr')){
      showElement("BottomRightPanel")
      showElement("TopRightPanel")
      showElement("snippetToolBarContainer")
      showElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      addCssClass( id= 'midRightPanels', class='ctop140')
    } else { # editing other
      removeCssClass( id= 'midRightPanels', class='ctop140')
      hideElement("TopRightPanel")
      hideElement("snippetToolBarContainer")
      hideElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      if(identical(mode,'ptrrmd')){
        removeCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
      else{
        addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
    }
    showElement("aceToobarTop1")
    showElement("aceToobarTop2")
    showElement("commitButton")
    showElement("aceTabSet")
    showElement("midRightPanels")
  }
  cat("<---< reOrgPanels\n")
}