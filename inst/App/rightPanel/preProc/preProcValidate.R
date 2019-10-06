
assertConsistantTibPair<-function(tib1, tib2){
  # assert col names are equal
  # if(!setequal(names(tib1), names(tib2))){
  #   stop('Inconsistency: returned missing or extra tibble columns')
  # }
  # assert coltypes are equal

  for(nm in intersect(names(tib1), names(tib1)) ){
    c1<-tib1[[nm]]
    if(all(is.na(c1))){
      next
    }
    c2<-tib2[[nm]]
    if(all(is.na(c2))){
      next
    }
    c1<-na.omit(c1)
    c2<-na.omit(c2)
    if(!identical(extractColType(c1), extractColType(c2))){
      stop('Not allowed: this operation forbids the column type to be changed')
    }
  }
}

validateTibLists<-function(tibs1, tibs2){
  Assert$check({!is.null(tibs2)}, message='returned null instead of a list')
  Assert$check({is.list(tibs2)}, message='fails to return a list of tibbles')
  # if(!setequal(names(tibs1), names(tibs2))){
  #   stop('returned missing or extra ptR members')
  # }
  for(n in intersect(names(tibs1),names(tibs2))){
    assertConsistantTibPair(tibs1[[n]], tibs2[[n]])
  }  
}
