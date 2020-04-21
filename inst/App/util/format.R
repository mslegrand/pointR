



toStrPtR0<-function(x,...){ UseMethod("toStrPtR0") }

toStrPtR0.default<-function(x,...){ format(x,...) }
toStrPtR0.name<-function(x,...){ paste0('quote(',as.character(x),')') }
toStrPtR0.call<-function(x,...){ 
  paste0('quote(',
         paste0(format(x), collapse=""),
         ')'
  )
}

toStrPtR0.character<-function(x, ...){
  xx<-sapply(x,function(y){if(!is.na(y)){shQuote(y)} else {'NA'}})
  if(length(xx)==0){
    'character(0)'
  } else if (length(xx)==1){
    xx
  } else{
    paste0("c(",paste0(xx,collapse=","),")")
  } 
}
toStrPtR0.numeric<-function(x, digits=0, ...){
  if(length(x)==0){
    "numeric(0)"
  } else if(length(x)==1){
    x
  } else{
    paste0("c(",paste(x,collapse=", "),")")
  }
}

# used by fmtMat
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

toStrPtR0.list<-function(x, digits=0, ...){
  temp<-sapply(x, function(y) toStrPtR0(y,  ...))
  temp<-paste0(temp, collapse=", ")
  paste0("list(", temp, ")")
}

#

#to do detect the points and place at the end
# get colclasses
# get indices of candidats: which =='matrix'
# the set tib to be cbind( tib[[,-ind]], tib[[,ind]])
# to do: tibble name 
# used by fmtTibbleList (below)
fmtTribble<-function(tib, tibName, movePts2Back=TRUE, indent="  ", ...){
  #this moves the points to the rear
  
  n=2 # indent factor of 2 tabs
  indentName<-paste0(rep(indent, n=n),collapse="")

  if(movePts2Back){
    cl<-sapply(1:ncol(tib), function(n)isPoints(tib[[n]]) )
    indx<-c(which(cl==FALSE), which(cl==TRUE)) #reorders the indices
    tib<-tib[,indx] #reorder tib columns
  }
  n<-names(tib)
  nn<-matrix(paste0("~",n),1)
  tmp<-sapply(1:ncol(tib), function(j){
    sapply(1:nrow(tib),function(i){
      toStrPtR0( tib[[j]][[i]] )
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

# used by fmtTibbleList (below)
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

# used only by fmtTibbleList (below)
fmtMat<-function(tib, tibName, indent="  ", ...){ 
  n=2 # indent factor of 2 tabs
  indentName<-paste0(rep(indent, n=n),collapse="")
  m<-tib[[1,tibName]]
  tmp<-paste0(indentName, tibName, "= ", toStrPtR0.matrix(m))
  tmp  
}

# used only by ptDef2ReplacementList
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


