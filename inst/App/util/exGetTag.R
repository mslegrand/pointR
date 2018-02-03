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

