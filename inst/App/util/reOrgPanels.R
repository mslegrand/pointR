#todo:: rewrite with sapply or loop


reOrgPanels<-function(id, mode){
 
  log.fin(reOrgPanels)
  # if(length(mode)>1){
  #   browser()
  # }
  if(length(id)==0 || length(mode)==0){
    
    hideElement("TopRightPanel")
    hideElement("snippetToolBarContainer")
    hideElement("aceToobarTop1")
    hideElement("aceToobarTop2")
    hideElement("useTribble")
    hideElement("commitButton")
    hideElement("aceTabSet")
    hideElement("midRightPanels")
    hideElement("BottomRightPanel")
    hide(id="rmd-edit-code")
    hide(id="rmd-insert-svgR")
    hide(id="rmd-insert-ptR")
    addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
    showElement("logo.right")
    showElement("logo.left")
  } else { # id and mode defined
    hideElement("logo.right")
    hideElement("logo.left")
    # editing ptr
    if(identical(mode,'ptr')){
      showElement("BottomRightPanel")
      showElement("TopRightPanel")
      showElement("snippetToolBarContainer")
      showElement("commitButton")
      showElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      hide(id="rmd-edit-code")
      hide(id="rmd-insert-svgR")
      hide(id="rmd-insert-ptR")
      if(usingElectron){
        addCssClass( id= "runAppPanel", class="hiddenPanel")
        addCssClass( id= "stopAppPanel", class="hiddenPanel")
      }
      addCssClass( id= 'midRightPanels', class='ctop140')
    } else { # editing other: 'ptrrmd', 'snippets', 'dnippets', 'text', 'javascript'
      if(identical(mode,'javascript')){
        showElement("snippetToolBarContainer")
      } else {
        hideElement("snippetToolBarContainer")
      }
      removeCssClass( id= 'midRightPanels', class='ctop140')
      hideElement("TopRightPanel")
      
      hideElement("useTribble") # todo!!! show only if mode==ptR and there is a tribble or tibble
      # now consider which mode it is
      if(identical(mode,'ptrrmd')){
        show(id="rmd-edit-code")
        show(id="rmd-insert-svgR")
        show(id="rmd-insert-ptR")
        
        removeCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
      else{
        hide(id="rmd-edit-code")
        hide(id="rmd-insert-svgR")
        hide(id="rmd-insert-ptR")
        addCssClass( id= "rmdBrowserButtonPanel", class="hiddenPanel")
      }
      if(usingElectron){
        if(identical(mode,'app')){
          removeCssClass( id= "runAppPanel", class="hiddenPanel") 
          removeCssClass( id= "stopAppPanel", class="hiddenPanel")
        }
        else{
          addCssClass( id= "runAppPanel", class="hiddenPanel")
          addCssClass( id= "stopAppPanel", class="hiddenPanel")
        }
      }
      if(mode %in% c('ptrrmd', 'dnippets')){
        showElement("commitButton")
      } else { # mode is one of 'snippets', 'text', 'javascript'
        hideElement("commitButton")
      }
    }
    showElement("aceToobarTop1")
    showElement("aceToobarTop2")
    showElement("aceTabSet")
    showElement("midRightPanels")
  } # end id and mode defined
  log.fout(reOrgPanels)
}