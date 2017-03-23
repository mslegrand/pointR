#ordinary fns
exGetTagName<-function(tagNameChoices, ptChosen){
  if(length(tagNameChoices)>0){
    if(length(ptChosen)>0 && (ptChosen %in% tagNameChoices)){
      tagChosen<-ptChosen
    } else {
      tagChosen<-tail(tagNameChoices,1)
    }
  } else {
    tagChosen<-NULL
  }
  tagChosen
}

exGetTagIndx<-function(tagIndxChoices, point.indx){
  if(length(tagIndxChoices)<1 ){
    return(NULL)
  }
  if(  length(point.indx)<1){
    point.index<-max(tagIndxChoices)
  } else {
    if( point.indx>0 ){
      t.point.indx<-max(tagIndxChoices[ tagIndxChoices<= point.indx] )
    } else {
      0
    }    
  }
}  
