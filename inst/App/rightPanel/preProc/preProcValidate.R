
assertConsistantTibPair<-function(tib1, tib2){
  # assert names are equal
  if(!setequal(names(tib1), names(tib2))){
    stop('Inconsistency: returned missing or extra tibble columns')
  }
  # assert coltypes are equal
  for(n in names(tib1)){
    if(!identical(extractColType(tib1[[n]]), extractColType(tib2[[n]]))){
      stop('Operation not allowed: column type for column n cannot be changed')
    }
  }
}

validateTibLists<-function(tibs1, tibs2){
  Assert$check({!is.null(tibs2)}, message='returned null instead of a list')
  Assert$check({is.list(tibs2)}, message='fails to return a list of tibbles')
  if(!setequal(names(tibs1), names(tibs2))){
    stop('returned missing or extra ptR members')
  }
  for(n in names(tibs1)){
    assertConsistantTibPair(tibs1[[n]], tibs2[[n]])
  }  
}


# testing 
# source("inst/App/util/utilColumnType.R")
# tib1<-tibble(points=list(matrix(1:4,2)), fill=c('red','blue'))
# tib2<-tibble(points=list(matrix(1:6,2)), fill=c('red','blue'))
# tib3<-tibble(points=list(matrix(1:6,2)), dril=c('red','blue'))
# tib4<-tibble(points=1, dril=c('red','blue'))
# 
# tryCatch({
#   assertConsistantTibPair(tib1,tib2)
#   cat('assertConsistantTibPair(tib1,tib2) ok\n')
# }, 
# error = function(e){
#   cat('assertConsistantTibPair(tib1,tib2) bad\n')
# })
# 
# tryCatch({
#   assertConsistantTibPair(tib3,tib2)
#   cat('assertConsistantTibPair(tib3,tib2) bad\n')
# }, 
# error = function(e){
#   cat('assertConsistantTibPair(tib3,tib2) ok\n')
# })
# 
# tryCatch({
#   assertConsistantTibPair(tib3,tib4)
#   cat('assertConsistantTibPair(tib3,tib4) bad\n')
# }, 
# error = function(e){
#   cat('assertConsistantTibPair(tib3,tib4) ok\n')
# })