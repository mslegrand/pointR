


# getTibColClass<-function(tib){
#   sapply(tib, function(j)class(tib[[1,j]]))
# }
getTibColClass<-function(tib){ #used by fmtTribble
  sapply(1:ncol(tib), function(j)class(tib[[1,j]]))
}


# formatMatrix<-function(pts){
#   if(length(pts)==0 ){
#     fpts<-'list()'
#   } else{
#     tmp<-as.integer(unlist(pts)) #force to have only integers numbers as points
#     tmp<-matrix(tmp,2)
#     fpts<-apply(tmp, 2, function(x)paste(x,collapse=","))
#     fpts<-paste0("c(",fpts,")")
#     fpts<-paste(fpts, collapse=",")
#   }
#   sp<-"  "
#   fpts<-paste0("matrix( ","c( ", fpts, " ), 2",")")
#   return(fpts)
# }




# 
# toStrPtR<-function(x,...){ UseMethod("toStrPtR") }
# 
# toStrPtR.default<-function(x,...){ toString(x,...) }
# toStrPtR.character<-function(x, ...){
#   toString(shQuote(x), ...)
# }
# toStrPtR.matrix<-function(x, digits=0, ...){
#   if(length(x)==0){
#     "matrix(0,2,0)"
#   } else {
#     tmp<-as.numeric(format(x, digits, ...))
#     tmp<-matrix(tmp,2)
#     fpts<-apply(tmp, 2, function(x)paste(x,collapse=","))
#     fpts<-paste0("c(",fpts,")")
#     fpts<-paste(fpts, collapse=",")
#     paste0("matrix( c(",fpts,"), 2)")
#   }
# 
# }


toStrPtR0<-function(x,...){ UseMethod("toStrPtR0") }

toStrPtR0.default<-function(x,...){ format(x,...) }
toStrPtR0.character<-function(x, ...){
  if(length(x)>1){
    paste0("c(",paste0(shQuote(x),collapse=","),")")
  } else {
    shQuote(x)
  }
}
toStrPtR0.matrix<-function(x, digits=0, ...){
  if(length(x)==0){
    "matrix(0,2,0)"
  } else {
    tmp<-as.numeric(format(x, digits, ...))
    tmp<-matrix(tmp,2)
    fpts<-apply(tmp, 2, function(x)paste(x,collapse=","))
    fpts<-paste0("c(",fpts,")")
    fpts<-paste(fpts, collapse=",")
    paste0("matrix( c(",fpts,"), 2)")
  }

}


# 
# pfmt<-function(x){
#   UseMethod("pfmt")
# }
# 
# pfmt.default<-function(x, ...){
#   paste("c(", toStrPtR(x, ...), ")" )
# }
# 
# pfmt.list<-function(x, digits=0, ...){
#   temp<-sapply(x, function(y) pfmt(y,  ...))
#   temp<-paste0(temp, collapse=", ")
#   paste0("list(", temp, ")")
# }
# 
# pfmt.matrix<-function(x, digits=0, ...){
#   toStrPtR0.matrix(x,digits, ...)
# }
# 



toStrPtR0.list<-function(x, digits=0, ...){
  temp<-sapply(x, function(y) toStrPtR0(y,  ...))
  temp<-paste0(temp, collapse=", ")
  paste0("list(", temp, ")")
}

# fmtPtR<-function( pts,  indent="  ", digits=0, ...){
#   n<-names(pts)
#   tmp<-sapply(n, function(x){
#     paste0(indent, x , '=', pfmt( pts[[x]] ) )
#   })
#   tmp<-paste0(tmp, collapse=",\n")
#   tmp<-paste("ptR<-list(",tmp,")", sep="\n")
#   tmp
# }

#to do detect the points and place at the end
# get colclasses
# get indices of candidats: which =='matrix'
# the set tib to be cbind( tib[[,-ind]], tib[[,ind]])
# to do: tibble name 
fmtTribble<-function(tib, tibName, movePtsBack=TRUE, indent="  ", ...){
  #this moves the points to the rear
  n=2 # indent factor of 2 tabs
  indentName<-paste0(rep(indent, n=n),collapse="")

  if(movePtsBack){
    cl<-getTibColClass(tib)
    indx<-c(which(cl!='matrix'), which(cl=='matrix'))
  }
  tib<-tib[,indx]
  n<-names(tib)
  nn<-matrix(paste0("~",n),1)
  tmp<-sapply(1:ncol(tib), function(j){
    sapply(1:nrow(tib),function(i){
      toStrPtR0(tib[[i,j]])
    })
  })
  tmp<-rbind(nn,tmp)
  tmp[-length(tmp)]<-paste0(tmp[-length(tmp)],",")
  # now make each col (sans last) to have same width
  tmp[,-ncol(tmp)]<-format(tmp[,-ncol(tmp)])
  #prepend with indent
  indentContent<-paste0(indentName,indent)
  indentContent<-matrix( indentContent, nrow(tmp), 1)
  tmp<-cbind(indentContent,tmp)
  # now combine into rows
  tmp<-apply(tmp,1, function(x)paste0(x, collapse="  "))
  # next combine rows
  tname<-paste0(indentName, tibName, "= tribble(")
  tend<-paste0(indentName,")")
  tmp<-paste0(c(tname,tmp,tend), collapse="\n")
  tmp
}

fmtTibble<-function(tib, tibName, is.mat=FALSE, indent="  ", ...){
  n=2
  indentName<-paste0(rep(indent, n=n),collapse="")
  
  nam<-names(tib)
  tmp<-sapply(nam, function(n){
    toStrPtR0(tib[[n]])
  })
  tmp<-paste0(indentName,indent, nam,' = ',tmp)
  
  tmp<-paste0(indent,tmp,collapse=",\n")
  tmp<-paste0(indent,tibName,'=tibble(\n',tmp,"\n",indent,")")
}

fmtMat<-function(tib, tibName, indent="  ", ...){ 
  n=2 # indent factor of 2 tabs
  indentName<-paste0(rep(indent, n=n),collapse="")
  m<-tib[[1,tibName]]
  tmp<-paste0(indentName, tibName, "= ", toStrPtR0.matrix(m))
  tmp  
}


fmtTibbleList<-function(tibList, mats, as.Tribble){
  as.Tribble<-unlist(as.Tribble)
  tmp<-sapply(names(tibList), function(nm){
    if(mats[nm]==TRUE){
      fmtMat(tibList[[nm]],nm)
    } else {
      if(as.Tribble){
        fmtTribble(tibList[[nm]],nm)
      } else {
        fmtTibble(tibList[[nm]],nm)
      }
    }
  })
  tmp<-paste0(tmp, collapse=",\n")
  paste('ptR<-list(',tmp,')',sep="\n")
}


  # 
  # x=tribble(
  #   ~fill,      ~pts,
  #   #------|---------
  #   'red',  list( matrix(c(1,2,3,4,5,6), 2) ) ,
  #   'blue', list( matrix(c(11,12,13,14,15,16), 2) )
  # )
  # 
  # 
  # 
  # xx=tribble(
  #   ~fill,      ~pts,
  #   #------|---------
  #   'red',   matrix(c(1,2,3,4,5,6), 2) ,
  #   'blue',  matrix(c(11,12,13,14,15,16), 2)
  # )
  # 
  # tib<-xx
  
  #vapply(tib, pfmt, character(1))
  #vapply(tib, toStrPtR0, character(1))
  
  
  # tt<-fmtTribble(tib, 'tib')
  # # 
  # yy<-tribble(
  #   ~ptss,      ~stroke,
  #   #------|---------
  #      matrix(c(1,2,3,4,5,6), 2) ,'red',
  #    matrix(c(11,12,13,14,15,16), 2), 'blue'
  # )
  # 
  # cat(fmtTribble(yy, 'yy'))
  # 
  # pts<-yy$ptss
  # cat(pfmt(pts))
  # #todo tree manupiplation with svgR
  
  # ptR<-list(
  #   x=list(matrix(1:6,2),matrix(11:18,2)),
  #   y=list(matrix(100:105,2))
  # )
  # 
  # cat(fmtPtR(ptR))
  
  # tibList=list(
  #   x=tribble(
  #   ~fill, ~pts,
  #   'red', matrix((1:6+.01),2)
  #   ),
  #   y=tribble(
  #      ~pts, ~stroke,
  #      matrix(1:4,2), 'blue'
  #   )
  # )
  
  
   
  