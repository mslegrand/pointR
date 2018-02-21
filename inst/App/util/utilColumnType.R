

#returns TRUE if all numeric
isNumericString<-function(x){
  all(grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+)$",x))
}

isIntegerString<-function(x){
  all(nchar(x)>0 && !grepl("[^[:digit:]]",x))
}

isInteger<-function(x){
  all(x==as.integer(x))
}

isColorString <- function(x) {
  all(sapply(x, function(X) {
    if (!is.logical(X)) tryCatch(is.matrix(col2rgb(X)),
                                 error = function(e) FALSE) else FALSE
  }))
}

isBooleanString<-function(x){
  all(x %in% c('T','F','TRUE','FALSE'))
}

isBooleanString<-function(x){
  all(x %in% c('T','F','TRUE','FALSE'))
}

isPoints<-function(x){
  points<-!is.null(x) && all(unlist(lapply(x, function(m){
    is.matrix(m) && dim(m)[1]==2
  })))
  #cat('isPoins=',points,'"\n')
  points
}



#installr package
# integer test
#all.equal(a, as.integer(a))


ptR<-list(
  x=tribble(
    ~x,                           ~fill, ~stroke.width,  ~opacity, ~name,   ~visibility,
    matrix(c(100,200,300,200),2), 'red',  3,            .3,         'cat',   'visibile',
    matrix(c(100,300,300,300),2), 'red',  1,             1,         'dog',   'hidden',
    matrix(0,2,0),                'blue', 3,             0,         'bird',  'visible'
  ),
  y=tribble(
    ~y,             ~stroke,
    matrix(0,2,0), 'black'
  ),
  z=matrix(c(100,100,100,200,300,200),2)
)

isPercentageString<-function(x){
  all(grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+)%$",x))
}

charColType<-function(x){
  if(isColorString(x)){ return("colourable")}
  if(isPercentageString(x)){return("percentage")}
  return("character")
}

listColType<-function(x){
  # points
  # pairs
  
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

extractColType<-function( column ){
  #cat("extractColType: class(column)",class(column),"\n")
  if(is.character(column)){ print(column)}
  if(is.list(column)){
    cat("listColType(column)=", format(listColType(column)),"\n" )
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
    } else if(is.logical(xx)) {
      return("logical")
    } else {
      return("other")
    }
  }
}

