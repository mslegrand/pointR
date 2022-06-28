
returnValue4ModuleEdAsset<-callModule(
  module=moduleEdAsset,
  id="edAssetCh",
  name=reactive({    
    if(hasError()){ rtv<-errorPanelTag } else{ rtv<-getAssetName()}
    rtv
  }),
  
  nameChoices={ reactive({
    rtv<-getRightPanelChoices()
    rtv
  })}
)

#name
observeEvent(returnValue4ModuleEdAsset$name(),{
  name<-returnValue4ModuleEdAsset$name()
  
  if( is.null(getAssetName())  || name==getAssetName() ){ return(NULL) } #bail if moduleEdAsset did not change name
  if( !(name %in% c(errorPanelTag) )){
    if(name==transformTag){
      updateSelected(name=transformTag)
    } else  {
      tibs<-getPtDefs()$tib
      resetSelectedTibbleName(tibs=tibs, name=name)
    }
  }
}
,ignoreInit = FALSE)

observeEvent( returnValue4ModuleEdAsset$newAsset(),{
  showModal( addNewAssetModal() )
}, label='EdTib-rtv-newColumn', ignoreInit = TRUE, ignoreNULL = TRUE)
