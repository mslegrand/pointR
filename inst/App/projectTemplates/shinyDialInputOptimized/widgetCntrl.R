library(shiny)
library(svgR)

widgetSvgCntrl<-function(args){
    source('widget_svg.R', local=T)$value
}

newWidgetCntrl<-function(WH, theta0, CXY, R, CMD ){
  svg<-widgetSvgCntrl(list(WH=WH, theta0=theta0, CXY=CXY, R=R, CMD=CMD))
  HTML(as.character(svg))
}

widgetCntrl<-function(inputId, wh, theta0=0){
  cxy<-c(.5,.9)*wh
  r<-.4*wh[1]
#   SCRIPT<-'
# function clicked(evt, ctrlId){
#       // compute theta (in radians)
#       var theta = widgetCntrlBinding.mouse2theta(ctrlId, evt.clientX, evt.clientY);
#       widgetCntrlBinding.updateNeedle(ctrlId,theta );
#       //update value
#       theta=Math.round((180 *theta) / Math.PI );
#       widgetCntrlBinding.setValue("#" + ctrlId, theta);
# }'
    CMD=sprintf('widgetCntrlBinding.clicked("%s", evt)',inputId)
    tagList(
        singleton(tags$head(tags$script(src = "widget.js"))),
        div( id=inputId, 
             class="widgetCntrl",  # 
             "data-theta"=theta0, # customize for initialization
             'data-x'=cxy[1],
             'data-y'=cxy[2],
             'data-r'=r,
             newWidgetCntrl(WH=wh,  theta0=theta0, CXY=cxy, R=r, CMD=CMD)
        )        
    )
}

updateWidgetCntrl<-function(session, inputId, value=NULL){
  mssg<-list(value=value)
  if(length(mssg)>0){
    session$sendInputMessage(inputId, mssg)
  }
}

