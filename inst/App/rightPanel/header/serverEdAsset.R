
returnValue4ModuleEdAsset<-callModule(
  module=moduleEdAsset,
  id="edAssetCh",
  name=reactive({ if(hasError()){ errorPanelTag } else{ getTibName()}}),
  nameChoices=getRightPanelChoices
)

#name
observeEvent(returnValue4ModuleEdAsset$name(),{
  name<-returnValue4ModuleEdAsset$name()
  if( name==getTibName() ){ return(NULL) } #bail if moduleEdAsset did not change name
  if( !(name %in% c(errorPanelTag) )){
    if(name==transformTag){
      updateSelected(name=transformTag)
    } else  {
      tibs<-getPtDefs()$tib
      resetSelectedTibbleName(tibs=tibs, name=name)
    }
  }
})