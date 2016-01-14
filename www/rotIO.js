var chosen =[];
var movedTo=[];

var selectedElement = 0;
var currentMatrix   = 0;

var svg = document.querySelector("svg");
var cxy = svg.createSVGPoint();  //center of shape
var ut  = svg.createSVGPoint();
var originalCTM=svg.createSVGMatrix();

function cursorPoint(evt){
  var tmp=svg.createSVGPoint();
  tmp.x = evt.clientX; tmp.y = evt.clientY;
  return tmp.matrixTransform(svg.getScreenCTM().inverse());
}

function dot(v1,v2){
        return v1.x * v2.x + v1.y * v2.y;
}

function length(v){
  return Math.sqrt(dot(v,v));
}

function getCos( u, v){
  var l=length(u)*length(v);
  if(l===0) {
    return 1;
  } else {
    return dot(u,v)/l;
  }
}

function getSin( u, v ){
  var l=length(u)*length(v);
  if( l===0 ){
    return 1;
  } else {
    return -(u.x*v.y-v.x*u.y)/l;
  }
}

function diff(u,v){
  var tmp=svg.createSVGPoint();
  tmp.x = u.x - v.x; 
  tmp.y = u.y - v.y;
  return tmp;
}

function rotM( cxy, u, v){
  var c=getCos(u,v);
  var s=getSin(u,v);
  var h=cxy.x;
  var k=cxy.y;
  var m=svg.createSVGMatrix();
  m.a=c; m.b=s; m.c=-s; m.d=c; m.e=h+k*s-h*c; m.f= k-h*s-k*c;
  return m;
}


// selectElement 
function selectElement(evt) {
  selectedElement = evt.target;
    
  originalCTM=selectedElement.getCTM();
  
  // extract cxy
  var bbBox = selectedElement.getBBox();
  cxy.x= bbBox.x+bbBox.width/2;
  cxy.y=bbBox.y+bbBox.height/2;
  cxy=cxy.matrixTransform(originalCTM);
  // extract u
  var qt=cursorPoint(evt);
  ut=diff(qt,cxy);
  var lenut = length(ut);
  if( lenut < 0.001 ){ //not too close
    selectedElement=0;
    return;
  }
  
  //add eventattrs to element
  selectedElement.parentNode.appendChild( selectedElement ); //brings to top
  selectedElement.setAttributeNS(null, "onmousemove", "moveElement(evt)");
  selectedElement.setAttributeNS(null, "onmouseout", "deselectElement(evt)");
  selectedElement.setAttributeNS(null, "onmouseup",  "deselectElement(evt)");
}

// translation of an element
function moveElement(evt) {
  if(selectedElement!==0){ // this shouldn"t be necessary 
    var pt = cursorPoint(evt);
    var vt =  diff(pt, cxy);
    var lenvt = length(vt);
    if( lenvt < 0.001 ){ //not too close
      return;
    }
    var rotationMatrix=rotM( cxy, vt, ut );
    var ctm=rotationMatrix.multiply(originalCTM );
    currentMatrix=[ctm.a, ctm.b, ctm.c, ctm.d, ctm.e, ctm.f];
    selectedElement.setAttributeNS(null, "transform", "matrix(" + currentMatrix.join(" ") + ")");   
  }
  
}

// deselect that element
function deselectElement(evt) {
  if(selectedElement !== 0){
    var tid = selectedElement.getAttribute("tid");    
    var currentMatrixAsString="c(" + currentMatrix.join(",") + ")";    
    //var cxyStr="c("+cxy.x+","+cxy.y+")"; //to use for display?
    //chosen=["rotate", currentMatrixAsString, tid, cxyStr];
    chosen=["rotate", currentMatrixAsString, tid];
    Shiny.onInputChange("mydata",chosen);    
    selectedElement.removeAttributeNS(null, "onmousemove");
    selectedElement.removeAttributeNS(null, "onmouseout");
    selectedElement.removeAttributeNS(null, "onmouseup");
    selectedElement = 0;
  }
}
