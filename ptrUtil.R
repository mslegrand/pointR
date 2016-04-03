
source("parsingUtil.R")
#---------------------------------------------------------------
#code template 

paste(names(svgR:::eleDefs), collapse=" ")->element.names

paste0("#svgR elements: ", element.names, "\n",
"WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list( x=c() )

ptR.df<-list(x=data.frame(ptIndx=1))

svgR(wh=WH, 
  #your custom code goes here
  
  NULL
  
  
 
)
")->codeTemplate
#-------------------------------------------------

#debug template
paste0("#svgR elements: ", element.names, "\n",
       "WH<-c(600,620)

ptDefs<-list(
   x=c(c( 137,339 ),c( 110.5,180 ),c( 329.5,157 ),c( 357.5,329 ))
)


svgR(wh=WH, 
     
     polygon(points=ptDefs$x, fill=\"blue\",opacity=.5),
     rect( class=\"draggable\", opacity=.5,
           xy=c(100,100), wh=c(100,100), fill=\"blue\", 
           transform=\"matrix(1 0 0 1 200 0)\"
     ),
     circle( class=\"draggable\", 
             cxy=c(100,230), r=50, fill=\"red\", opacity=.5,
             transform=\"matrix(1 0 0 1 179 223)\"
     )
)
")->debugTemplate2

#-------------------------------

# defines


indent<-function(n){
  rep(" ", n*2)
}

#---external fns----
as.text<-function(q){
  paste(deparse(q), collapse="\n")
}

getScript<-function(file){
  paste0(readLines(file),collapse="\n")
}

readFile<-function(fileName){
  paste0(readLines(fileName),collapse="\n")
}


getDefPos<-function(txt, defTag){
  p.df<-getParseDataFrame(txt)
  cumCharLines<-getcumCharLines(txt)
  tag.df<-extractTagDF(p.df, tag="ptR")
  pos<-extractPositions(cumCharLines, tag.df)
}


replaceDef<-function(txt, replacement, defTag){
  pos<-getDefPos(txt, defTag)
  paste0(
    substr(txt, 1, pos[1]-1),
    replacement,
    substr(txt,pos[2]+1,nchar(txt)),
    sep=""
  )
}

replaceTxt<-function(txt, replacements, positions){
  stopifnot(length(replacments)+1==ncol(positions))
  txtKeep<-textOutsidePos(txt, positions)
  replacements<-c(replacements,"")
  newtxt<-rbind(txtKeep,replacements)
  newtxt<-paste0(nextxt, collapse="")
}

replaceDefs<-function(txt, replacements, defTags){
  positions<-sapply(defTags, function(defTag)
    getDefsPos(txt, defTag)
  )
  replaceTxt(txt, replacements, defTags)
}
  
getDef<-function(txt, defTag){
  pos<-getDefPos(txt, defTag)
  return(substr(txt, pos[1], pos[2]))
}

# formatPts<-function(pts){
#   if(length(pts)==0 ){
#     return("c()")
#   } else{
#     
#     tmp<-unlist(pts)
#     tmp<-matrix(tmp,2)
#     tmp<-apply(tmp, 2, function(x)paste(x,collapse=","))
#     tmp<-paste("c(",tmp,")")
#     tmp<-paste(tmp, collapse=",")
#     tmp<-paste0("matrix(\n    c(", tmp, "),\n  2,)")
#     return(tmp)
#   }
# }



formatTrs<-function(tr){ #not used
  paste0('"',tr,'"')
}


# findTransforms<-functions(txt){
#   pos<-str_locate_all(txt,"transform", boundry="word")
#   N<-length(pos)
#   lapply(1:N, )
# }


#todo: modify script1, so either edit points or edit transform

q.svgX<-quote(
  svgX<-function(...){
    args<-list(...)
    graphPaper %<c-% function(wh=c(600,600), dxy=c(50, 50), labels=TRUE ){
      seq(0,wh[1],dxy[1])->xs
      seq(0,wh[2],dxy[2])->ys
      grph<-c(
        lapply(xs, function(x)line(xy1=c(x,0),xy2=c(x,wh[2]))),
        lapply(ys, function(y)line(xy1=c(0,y),xy2=c(wh[1],y)))
      )
      if(labels){
        grph<-c(grph, 
                lapply(xs, function(x)text(xy=c(x+2,10),x)),
                lapply(ys, function(y)text(xy=c(2,y),y))
        )
      }
      g( stroke.width=1,
         font.size=10,
         stroke="grey",
         grph
      )       
    }
    showPts  %<c-% function(ptName){
      if(is.null(ptName)){
        return(NULL)
      }
      pts<-ptDefs[[ptName]]
      if(length(pts)<2){
        return(NULL)
      } else{
        m<-matrix(pts,2,)
        lapply(1:ncol(m), function(i){
          id<-paste("pd",ptName,i,sep="-")
          pt<-m[,i]
          circle(class="draggable", 
                 id=id,  
                 cxy=pt, r=8, fill="red", 
                 transform="matrix(1 0 0 1 0 0)", 
                 onmousedown="selectPoint(evt)" )
        })
      }
    }

 
    svgR( style(".draggable {
             cursor: move;
          }"),
          script( js.scripts[ svgBarCmd ]  ),      
          # background
          rect(xy=c(0,0), wh=WH, fill="#FFFFFF", stroke='black', opacity=1),
          graphPaper(wh=WH),
          rect(xy=c(0,0), wh=WH, fill="#ADADFF", stroke='black', opacity=.0, onmousedown="newPoint(evt)"),
          args,
          showPts(selPtSet)
    )

    
  }
)
