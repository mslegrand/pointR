

UIcontextMenu<-function(){
  div( id ='cntnr', # ace context menu
       if(usingElectron){# usingElectron
         tagList(
           tag('ul',  list(
             id='items',
             tag('li',list(class='clickMe', span(class='icon-clone'),span('Copy')  )),
             tag('li',list(class='clickMe', span(class='icon-scissors'),span('Cut')  )),
             tag('li',list(class='clickMe', span(class='icon-paste'),span('Paste')  )),
             tag('li',list(class='clickMe', span(class='icon-cancel'),span('Delete')  ))
           )),
           hr(class='contexthr')
         )
       } else {
         NULL
       },
       tag('ul', list(
         id='items',
         tag('li',list(class='clickMe', span(class="icon-help"), span( 'Lookup element')))
       ))
  )
}