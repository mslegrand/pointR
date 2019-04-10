mssg<-reactiveValues(
  error="",
  capturedOutput=""
) 
setErrorMssg<-function(errMssg){ mssg$error<-errMssg }
clearErrorMssg<-function(){ mssg$error<-"" }
hasError<-reactive({ nchar(mssg$error)>0 })
getErrorMssg<-reactive({ mssg$error })
setCapturedMssg<-function(capturedMssg)({ 
  mssg$capturedOutput<-capturedMssg
})
getCapturedMssg<-reactive({ 
  mssg$capturedOutput
})


getAptRunnerMssg<-reactive({
    if(identical(appRunner$tabId,input$pages)){
      rtv<-appRunner$log
    } else {
      rtv<-""
    }
    rtv
  })
