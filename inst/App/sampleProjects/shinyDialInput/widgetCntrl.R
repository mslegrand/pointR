library(shiny)
library(svgR)

try({ removeInputHandler("widgetCntrlBinding") })

widgetSvgCntrl<-function(params){
    source('widget_svg.R', local=T)$value
}

newWidgetCntrl<-function(WH, theta0, CXY, R, CMD ){
  svg<-widgetSvgCntrl(list(WH=WH, theta0=theta0, CXY=CXY, R=R, CMD=CMD))
  HTML(as.character(svg))
}

widgetCntrl<-function(inputId, wh, theta0=0){
  cxy<-c(.5,.9)*wh
  r<-.4*wh[1]
  CMD=sprintf('widgetCntrlBinding.clicked("%s", evt)',inputId)
  tagList(
      singleton(tags$head(tags$script(src = "widget.js"))),
      div( id=inputId, 
           class="widgetCntrl",  # 
           "data-theta"=theta0, # customize for initialization
           'data-x'=cxy[1],
           'data-y'=cxy[2],
           newWidgetCntrl(WH=wh,  theta0=theta0, CXY=cxy, R=r, CMD=CMD)
      )        
  )
}

updateWidgetCntrl<-function(session, inputId, wh=c(300,200), value=NULL){
  if(!is.null(value)){
    theta=value
    cxy<-c(.5,.9)*wh
    r<-.4*wh[1]
    CMD=sprintf('widgetCntrlBinding.clicked("%s", evt)',inputId)
    node<-as.character(newWidgetCntrl(WH=wh,  theta0=theta, CXY=cxy, R=r, CMD=CMD))
    mssg<-list(value=node)
    session$sendInputMessage(inputId, mssg)
  }
  
  # mssg<-list(value=value)
  # use theta to generate new svg script
  # get new svg as script
  # send message to name to update script
  # return value of theta as list
  
  if(length(mssg)>0){
    session$sendInputMessage(inputId, mssg)
  }
}

shiny::registerInputHandler(
  "widgetCntrlBinding", 
  function(val, shinysession, name) {
    
    if(is.null(val) || is.null(val$dXY$x)) {
      return(NULL)
    } else {
      # Parse return value from JSON into R format dataframe
      # browser()
      dxy<-val$dXY
      theta<-90-(180*atan(dxy$x/dxy$y))/pi
      cxy<-unlist(val$CXY)
      # recompute WH from CXY as 
      wh<-cxy/c(.5,.9)
      # then R by
      r<-0.4*wh[1]
      inputId<-name
      CMD=sprintf('widgetCntrlBinding.clicked("%s", evt)',inputId)
      node<-as.character(newWidgetCntrl(WH=wh,  theta0=theta, CXY=cxy, R=r, CMD=CMD))
      mssg<-list(value=node)
      shinysession$sendInputMessage(inputId, mssg)
      
      #xy <- jsonlite::fromJSON(x)
      # convert xy into theta
      
      # get new svg as script
      # send message to name to update script
      # return value of theta as list
      
      # Extract the values of the data frame as a list
      
      
      return(theta)
    }
  }
)

