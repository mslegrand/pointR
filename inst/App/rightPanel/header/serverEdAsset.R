
returnValue4ModuleEdAsset<-callModule(
  module=moduleEdAsset,
  id="edAssetCh",
  name=reactive({    
    # cat('>--->returnValue4ModuleEdAsset::callModule\n')
    # cat('returnValue4ModuleEdAsset:: next:: getAssetName \n')
    if(hasError()){ rtv<-errorPanelTag } else{ rtv<-getAssetName()}
    # print(rtv)
    rtv
  }),
  
  nameChoices={ reactive({
    # cat('>--->returnValue4ModuleEdAsset::callModule\n')
    # cat('returnValue4ModuleEdAsset:: next:: getRightPanelChoices\n')
    rtv<-getRightPanelChoices()
    # print(rtv)
    # cat('returnValue4ModuleEdAsset:: after:: getRightPanelChoices\n')
    rtv
  })}
)

#name
observeEvent(returnValue4ModuleEdAsset$name(),{
  name<-returnValue4ModuleEdAsset$name()
  
  if( is.null(getAssetName())  || name==getAssetName() ){ return(NULL) } #bail if moduleEdAsset did not change name
  if( !(name %in% c(errorPanelTag) )){
    cat('returnValue4ModuleEdAsset$name()= ', name, '\n')
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
