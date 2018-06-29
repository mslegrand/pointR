returnValue4ModuleEdTransform<-callModule(
  module=moduleEdTransform,
  id="edTransform",
  assetName=reactive({ if(hasError()){ errorPanelTag } else{ getAssetName()}}),
  getTransformType=getTransformType
)

observeEvent(returnValue4ModuleEdTransform$type(),{
  type<-returnValue4ModuleEdTransform$type()
  cat("type=",format(type),"\n")
  cat( " getTransformType()=", format(getTransformType()), "\n")
  if(!(hasError() && !is.null(type) && !identical(type, getTransformType() )) ){
    cat("Again type=",format(type),"\n")
    updateSelected(transformType=type)
  }
})