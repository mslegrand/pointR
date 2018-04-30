
returnValue4ModuleEdAsset<-callModule(
  module=moduleEdAsset,
  id="edAssetCh",
  name=reactive({ if(hasError()){ errorPanelTag } else{ getTibName()}}),
  nameChoices=getRightPanelChoices
)

#name
observeEvent(returnValue4ModuleEdAsset$name(),{
  name<-returnValue4ModuleEdAsset$name()
  
  if( is.null(getTibName())  || name==getTibName() ){ return(NULL) } #bail if moduleEdAsset did not change name
  if( !(name %in% c(errorPanelTag) )){
    # cat('returnValue4ModuleEdAsset$name()= ', name, '\n')
    if(name==transformTag){
      updateSelected(name=transformTag)
    } else  {
      tibs<-getPtDefs()$tib
      resetSelectedTibbleName(tibs=tibs, name=name)
    }
  }
}
,ignoreInit = TRUE)

observeEvent( returnValue4ModuleEdAsset$newAsset(),{
  showModal( addNewAssetModal() )
}, label='EdTib-rtv-newColumn', ignoreInit = TRUE, ignoreNULL = TRUE)
