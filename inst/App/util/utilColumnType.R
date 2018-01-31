

#returns TRUE if all numeric
isNumericString<-function(x){
  all(grepl("^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+)$",x))
}

isIntegerString<-function(x){
  all(nchar(x)>0 && !grepl("[^[:digit:]]",x))
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
  !is.null(x) && all(unlist(lapply(x, function(m){
    is.matrix(m) && dim(m)[1]==2
  })))
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


