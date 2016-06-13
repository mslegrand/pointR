library(svgR)

panelHeights<-c(
  aceHeight = "490px", svg="490px", log="640px"
)

# cstyle<-list(
#   h1="font-family: 'ChunkFiveRegular'; font-style: italic; font-weight: 500; line-height: 1.1;  color: #4d3a7d; margin-bottom:5px; padding-bottom:5px;",
#   sidePanel="background-color: #88B0CA; padding-top: 0px; margin:0px; ", 
#   
#   wellPanel="width:500px; background-color: #88B0CA; padding-top:0px; margin:0px; padding-bottom:0px; margin-bottom:0px; margin-left:0px;",
#   button="background-color: #222244; color: white; border-radius: 24px;",
#   wellPoint="opacity:0.7, width:500px; background-color: #8888AA; border-color: #88AAAA; padding-top:0px; padding-bottom:0px; margin-left:-15px; margin-right:15px; margin-top:0px; margin-bottom:0px;",
#   svg=paste0("width:700px ;height: ",
#              panelHeights["svg"] ,
#              "; border: 1px solid darkblue; overflow: auto; background-color: white; margin:0px"
#              ),
#   log=paste0("width:500px ;height: ",
#              panelHeights["log"], #"640px",
#             "; border: 1px solid darkblue; overflow: auto; background-color: white;"
#     )
# )

cstyle<-list(
  h1="font-family: 'ChunkFiveRegular'; font-style: italic; font-weight: 500; line-height: 1.1;  color: #4d3a7d;",
  sidePanel="background-color: #88B0CA; padding-top: 0px;", 
  wellPanel="background-color: #88B0CA; padding-top: 0px;",
  button="background-color: #222244; color: white; border-radius: 24px;",
  buttonSmall="background-color: #222244; color: white; border-radius: 24px; font-size: 12px;",
  wellPoint="background-color: #8888AA; border-color: #88AAAA; margin-top: 0px; margin-bottom: 0px; padding-top: 0px; padding-bottom: 0px; font-size:12px; padding-left: 5px;padding-right: 5px;",
  svg=paste0("width:650px ;height: ",
             panelHeights["svg"] ,
             "; border: 1px solid darkblue; overflow: auto; background-color: white;"
  ),
  log=paste0("width:600px ;height: ",
             panelHeights["log"], #"640px",
             "; border: 1px solid darkblue; overflow: auto; background-color: white;"
  )
)






#-------------------------------------------------

styleSpec<-
" 
      @font-face {
font-family: 'ChunkFiveRegular';
src: url('Chunkfive-webfont.eot');
src: url('Chunkfive-webfont.eot?#iefix') format('embedded-opentype'),
url('Chunkfive-webfont.woff') format('woff'),
url('Chunkfive-webfont.ttf') format('truetype'),
url('Chunkfive-webfont.svg#ChunkFiveRegular') format('svg');
font-weight: normal;
font-style: normal;
}
background-color: #66999;
h1 {
font-family: 'Arial', cursive;
font-weight: 400;
line-height: 1.1;
color: #48ca3b;
}
.navbar-default .navbar-brand:hover,
.navbar-default .navbar-brand:focus {
color: #5E5E5E;
}
.navbar-default .navbar-brand {
color: #333388;
}
.navbar .nav > li > a {
color: #333388;
}
.navbar .nav > li > a:hover {
float: none;
color: #FFFFFF;
background-color: transparent;
}
.navbar-default .navbar-nav > li > a:focus {
color: white;
background-color: transparent;
}
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus {
color: black;
font-weight: bold;
background-color: white;
opacity: 0.5;
}
.navbar {
color: #333388;
border-color: black;
font-weight: bold;
/* Permalink - use to edit and share this gradient: http://colorzilla.com/gradient-editor/#6199c7+-1,cedbe9+0,aac5de+0,aac5de+0,6199c7+0,419ad6+16,419ad6+17,8dceef+51,3a8bc2+84,26558b+100 */
background: #6199c7; /* Old browsers */
background: -moz-linear-gradient(top, #6199c7 -1%, #cedbe9 0%, #aac5de 0%, #aac5de 0%, #6199c7 0%, #419ad6 16%, #419ad6 17%, #8dceef 51%, #3a8bc2 84%, #26558b 100%); /* FF3.6-15 */
background: -webkit-linear-gradient(top, #6199c7 -1%,#cedbe9 0%,#aac5de 0%,#aac5de 0%,#6199c7 0%,#419ad6 16%,#419ad6 17%,#8dceef 51%,#3a8bc2 84%,#26558b 100%); /* Chrome10-25,Safari5.1-6 */
background: linear-gradient(to bottom, #6199c7 -1%,#cedbe9 0%,#aac5de 0%,#aac5de 0%,#6199c7 0%,#419ad6 16%,#419ad6 17%,#8dceef 51%,#3a8bc2 84%,#26558b 100%); /* W3C, IE10+, FF16+, Chrome26+, Opera12+, Safari7+ */
filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#6199c7', endColorstr='#26558b',GradientType=0 ); /* IE6-9 */      }
"


#---------------------------------------------------------------
#code template 


# #debug template
# paste0("#svgR elements: ", element.names, "\n",
#        "WH<-c(600,620)
# 
# ptDefs<-list(
#    x=c(c( 137,339 ),c( 110.5,180 ),c( 329.5,157 ),c( 357.5,329 ))
# )
# 
# 
# svgR(wh=WH, 
#      
#      polygon(points=ptDefs$x, fill=\"blue\",opacity=.5),
#      rect( class=\"draggable\", opacity=.5,
#            xy=c(100,100), wh=c(100,100), fill=\"blue\", 
#            transform=\"matrix(1 0 0 1 200 0)\"
#      ),
#      circle( class=\"draggable\", 
#              cxy=c(100,230), r=50, fill=\"red\", opacity=.5,
#              transform=\"matrix(1 0 0 1 179 223)\"
#      )
# )
# ")->debugTemplate2
