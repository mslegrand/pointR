
# sole caller: observeEvent(returnValue4ModuleEdTib$entryValue() in module EdTib
#returns TRUE if all numeric
isNumericString<-function(x){
  all(grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+)$",x))
}



# sole caller: charColType (below)
isColorString <- function(x) {
  all(sapply(x, function(X) {
    if (!is.logical(X)) tryCatch(is.matrix(col2rgb(X)),
                                 error = function(e) FALSE) else FALSE
  }))
}

# takes a  column x and checks if the entries are all 2x* matrices of numerics
isPoints<-function(x){
  points<-is.list(x) &&  all(unlist(lapply(x, function(m){
    is.matrix(m) && dim(m)[1]==2 # && all(apply(m,1,is.numeric))
  })))
  points
}

# sole caller: charColType (below)
isPercentageString<-function(x){
  all(grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+)%$",x))
}

# sole callers: listColType, extractColType (below)
charColType<-function(x){
  if(isColorString(x)){ return("colourable")}
  if(isPercentageString(x)){return("percentage")}
  return("character")
}

#sole caller: extractColType (below)
listColType<-function(x){
  
  lens<- sapply(x, length)
  if(isPoints(x)){
    return("point")
  }
  if(all(lens %in% c(0,1))){
    sz<-''
  } else if(all(lens %in% c(0,2))){
    sz<-'.2'
  } else {
    sz<-'.vec'
  }
  if(all(sapply(x,is.character))){
    type<-charColType(x)
    return(paste0(type,".list",sz))
  }
  if(all(sapply(x,is.numeric))){
    # points
    if(isPoints(x)){
      return("point")
    }
    xx<-unlist(x)
    if(all(xx==as.integer(xx))){
      type="integer"
    } else {
      type="numeric"
    } 
    return(paste0(type,".list",sz))
  }
  return(paste0('other.list'))
}

# callers: PanelCoordinator::getColumnType, preProcValidate::assertConsistantTibPair
extractColType<-function( column ){
  
  if(is.list(column)){
    return(listColType(column))
  } else {
    if(is.character(column)){
      return(charColType(column))
    } else if(is.numeric(column)){
      if(all(column==as.integer(column))){
        return("integer")
      } else {
        return("numeric")
      }
    } else if(is.logical(column)) {
      return("logical")
    } else if(is.na(column)){
      return("NA")
    } else {
      return("other")
    }
  }
}

