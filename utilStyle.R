library(svgR)

#this is mostly adjusting dimensions, unfortunately some pasteing occurs
# making it inconvenient to put into css, at least for now. Should eventially
# put in css
panelHeights<-c(
  aceHeight = "490px", svg="490px", log="640px"
)

cstyle<-list(
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

#' styleSpecXX<- #XX added to insure not used
#' " 
#' #@font-face {
#' font-family: 'ChunkFiveRegular';
#' src: url('Chunkfive-webfont.eot');
#' src: url('Chunkfive-webfont.eot?#iefix') format('embedded-opentype'),
#' url('Chunkfive-webfont.woff') format('woff'),
#' url('Chunkfive-webfont.ttf') format('truetype'),
#' url('Chunkfive-webfont.svg#ChunkFiveRegular') format('svg');
#' font-weight: normal;
#' font-style: normal;
#' }
#' background-color: #66999;
#' h1 {
#' font-family: 'Arial', cursive;
#' font-weight: 400;
#' line-height: 1.1;
#' color: #48ca3b;
#' }
#' .navbar-default .navbar-brand:hover,
#' .navbar-default .navbar-brand:focus {
#' color: #5E5E5E;
#' }
#' .navbar-default .navbar-brand {
#' color: #333388;
#' }
#' .navbar .nav > li > a {
#' color: #333388;
#' }
#' .navbar .nav > li > a:hover {
#' float: none;
#' color: #FFFFFF;
#' background-color: transparent;
#' }
#' .navbar-default .navbar-nav > li > a:focus {
#' color: white;
#' background-color: transparent;
#' }
#' .navbar-default .navbar-nav > .active > a,
#' .navbar-default .navbar-nav > .active > a:hover,
#' .navbar-default .navbar-nav > .active > a:focus {
#' color: black;
#' font-weight: bold;
#' background-color: white;
#' opacity: 0.5;
#' }
#' .navbar {
#' color: #333388;
#' border-color: black;
#' font-weight: bold;
#' /* Permalink - use to edit and share this gradient: http://colorzilla.com/gradient-editor/#6199c7+-1,cedbe9+0,aac5de+0,aac5de+0,6199c7+0,419ad6+16,419ad6+17,8dceef+51,3a8bc2+84,26558b+100 */
#' background: #6199c7; /* Old browsers */
#' background: -moz-linear-gradient(top, #6199c7 -1%, #cedbe9 0%, #aac5de 0%, #aac5de 0%, #6199c7 0%, #419ad6 16%, #419ad6 17%, #8dceef 51%, #3a8bc2 84%, #26558b 100%); /* FF3.6-15 */
#' background: -webkit-linear-gradient(top, #6199c7 -1%,#cedbe9 0%,#aac5de 0%,#aac5de 0%,#6199c7 0%,#419ad6 16%,#419ad6 17%,#8dceef 51%,#3a8bc2 84%,#26558b 100%); /* Chrome10-25,Safari5.1-6 */
#' background: linear-gradient(to bottom, #6199c7 -1%,#cedbe9 0%,#aac5de 0%,#aac5de 0%,#6199c7 0%,#419ad6 16%,#419ad6 17%,#8dceef 51%,#3a8bc2 84%,#26558b 100%); /* W3C, IE10+, FF16+, Chrome26+, Opera12+, Safari7+ */
#' filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#6199c7', endColorstr='#26558b',GradientType=0 ); /* IE6-9 */      }
#' "
#' 
#' 
