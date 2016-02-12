var chosen =[];
var movedTo=[];

var selectedElement = 0;
var currentMatrix   = 0;

var svg = document.querySelector("svg");
var cxy = svg.createSVGPoint();  //center of shape
var WH = svg.createSVGPoint();  //center of shape
var ptQ  = svg.createSVGPoint(); //original pt
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

function diff(u,v){
  var tmp=svg.createSVGPoint();
  tmp.x = u.x - v.x; 
  tmp.y = u.y - v.y;
  return tmp;
}

function scaleM( cxy, q, p){
  var m=svg.createSVGMatrix();
  //first set to identity
  m.a=1; m.b=0; m.c=0; m.d=1; m.e=0; m.f=0; 
  
  var delta=diff(p,q); 
  
  m.a= 1 + 2*delta.x/WH.x;
  m.e= cxy.x * (1-m.a);
  m.d= 1 + 2*delta.y/WH.y;
  m.f= cxy.y * (1-m.d);
  
  return m;
}


// selectElement 
function selectElement(evt) {
  selectedElement = evt.currentTarget;
    
  originalCTM=selectedElement.getCTM();
  
  // extract cxy
  var bbBox = selectedElement.getBBox();
  WH.x=bbBox.width;
  WH.y=bbBox.height;
  cxy.x= bbBox.x+bbBox.width/2;
  cxy.y= bbBox.y+bbBox.height/2;
  cxy=cxy.matrixTransform(originalCTM);
  // check
  ptQ=cursorPoint(evt);
  ptQ=diff(ptQ,cxy);
  ptQ.x=Math.abs(ptQ.x);
  ptQ.y=Math.abs(ptQ.y);
  var lenut = length(ptQ);
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
    var ptP = cursorPoint(evt);
    ptP =  diff(ptP, cxy);
    ptP.x= Math.abs(ptP.x);
    ptP.y= Math.abs(ptP.y);
    var lenvt = length(ptP);
    if( lenvt < 0.001 ){ //not too close
      return;
    }
    var scaleMatrix=scaleM( cxy, ptQ, ptP );
    var ctm=scaleMatrix.multiply(originalCTM );
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
    chosen=["scale", currentMatrixAsString, tid];
    Shiny.onInputChange("mydata",chosen);    
    selectedElement.removeAttributeNS(null, "onmousemove");
    selectedElement.removeAttributeNS(null, "onmouseout");
    selectedElement.removeAttributeNS(null, "onmouseup");
    selectedElement = 0;
  }
}
