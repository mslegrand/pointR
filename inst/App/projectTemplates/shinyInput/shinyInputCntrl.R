library(shiny)
library(svgR)
library(jsonlite)

try({ removeInputHandler("shinyInputCntrlBinding") })

#helper functions

Normalize<-function(Z){
    S2<-sum(Mod(Z)^2)
    if(S2>0){
        Z<-Z/sqrt(S2)
    }
    Z
}

# use toJSON for non-trivial initialization
ZtoJSON<-function(Z){ 
   toJSON(data.frame(re=Re(Z),im=Im(Z))) 
}

# wrapper around svgR code   
shinyInputSvgCntrl<-function(params){
    source('shinyInput_svg.R', local=T)$value
}

# return svg given params
newShinyInputCntrl<-function(ID, WH, CMDS, Z){
  svg<-shinyInputSvgCntrl(
    params=list(ID=ID, WH=WH, CMDS=CMDS, Z=Z )
  )
  tmp<-HTML(as.character(svg))
  return(tmp)
}

# definition of what to call for the given mouse events
id2CMDS<-function(inputId){
    c(
       sprintf('shinyInputCntrlBinding.clicked("%s", %d, evt);',inputId, 0 ),
       sprintf('shinyInputCntrlBinding.clicked("%s", %d, evt);',inputId, 1 )
   )
}

# Cntrl constructor to insert in app ui
shinyInputCntrl<-function(inputId, wh=c(50,100), Z=c(1+0i, 1+1i) ){
  Z<-Normalize(Z)
  #CMDS<-paste0("alert('qubit |", c(0,1), "> selected')")
  CMDS=id2CMDS(inputId)

  tagList(
      singleton(tags$head(tags$script(src = "shinyInput.js"))),
      div( id=inputId, 
           class="shinyInputCntrl",  
           # customize for initializationby attaching  property(s) to this div
           'data-Z'=ZtoJSON(Z),
           newShinyInputCntrl(ID=inputId, WH=wh, CMDS=CMDS, Z=Z)
      )        
  )
}

# server to client update
updateShinyInputCntrl<-function(session, inputId, wh=c(200,400), Z=NULL){
  # validate input
  if(length(Z)!=2){
        cat('bad dim')
        return(NULL)
  }
  
  # normalize first
    Z<-Normalize(Z)
    # CMDS<-paste0("alert('qubit |", c(0,1), "> selected')")
    CMDS=id2CMDS(inputId)
    #recreate the entire svg 
    node<-as.character(newShinyInputCntrl(ID=inputId, WH=wh,  CMDS=CMDS, Z=Z))
    mssg<-list(value=node, Z=ZtoJSON(Z))
    session$sendInputMessage(inputId, mssg)
}


# preprocess data returned to server from the client
shiny::registerInputHandler(
  "shinyInputCntrlBinding", 
  function(value, shinysession, inputId) {
    if(is.null(value) ) {
      return("NULL")
    } else {
        ZDF<-fromJSON(value$Z)
        print(ZDF)
        if(nrow(ZDF)<2){
            return(NULL)
        }
        Z=complex(nrow(ZDF), ZDF$re, ZDF$im)
        Index=1+value$Index
        L<-1-sum(Mod(Z[ Index])^2)
        LL<- sum(Mod(Z[-Index])^2)
        Z[-Index]<-sqrt(L/LL)*Z[-Index]
        print("ZZ")
        print(Z)
       updateShinyInputCntrl(shinysession, inputId, wh=c(200,400), Z=Z)
     return(Z)
    }
  }
)

