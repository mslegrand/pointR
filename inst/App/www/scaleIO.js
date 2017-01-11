// scales an element
// var chosen =[];
// var movedTo=[];

var ptR_selectedElement = 0;
var ptR_currentMatrix   = 0;

var ptR_svg = document.querySelector("#ptR_SVG_TRANSFORM");
var ptR_cxy = ptR_svg.createSVGPoint();  //center of shape
var ptR_WH = ptR_svg.createSVGPoint();  //center of shape
var ptR_ptQ  = ptR_svg.createSVGPoint(); //original pt
var ptR_originalCTM=ptR_svg.createSVGMatrix();

function cursorPoint(evt){
  var tmp=ptR_svg.createSVGPoint();
  tmp.x = evt.clientX; tmp.y = evt.clientY;
  return tmp.matrixTransform(ptR_svg.getScreenCTM().inverse());
}

function dot(v1,v2){
        return v1.x * v2.x + v1.y * v2.y;
}

function length(v){
  return Math.sqrt(dot(v,v));
}

function diff(u,v){
  var tmp=ptR_svg.createSVGPoint();
  tmp.x = u.x - v.x; 
  tmp.y = u.y - v.y;
  return tmp;
}

function scaleM( cxy, q, p){
  var m=ptR_svg.createSVGMatrix();
  //first set to identity
  m.a=1; m.b=0; m.c=0; m.d=1; m.e=0; m.f=0; 
  
  var delta=diff(p,q); 
  
  m.a= 1 + 2*delta.x/ptR_WH.x;
  m.e= cxy.x * (1-m.a);
  m.d= 1 + 2*delta.y/ptR_WH.y;
  m.f= cxy.y * (1-m.d);
  
  return m;
}


// selectElement 
function selectElement(evt) {
  ptR_selectedElement = evt.currentTarget;
    
  ptR_originalCTM=ptR_selectedElement.getCTM();
  
  // extract cxy
  var bbBox = ptR_selectedElement.getBBox();
  ptR_WH.x=bbBox.width;
  ptR_WH.y=bbBox.height;
  ptR_cxy.x= bbBox.x+bbBox.width/2;
  ptR_cxy.y= bbBox.y+bbBox.height/2;
  ptR_cxy=ptR_cxy.matrixTransform(ptR_originalCTM);
  // check
  ptR_ptQ=cursorPoint(evt);
  ptR_ptQ=diff(ptR_ptQ,ptR_cxy);
  ptR_ptQ.x=Math.abs(ptR_ptQ.x);
  ptR_ptQ.y=Math.abs(ptR_ptQ.y);
  var lenut = length(ptR_ptQ);
  if( lenut < 0.001 ){ //not too close
    ptR_selectedElement=0;
    return;
  }
  
  
  
  //add eventattrs to element
  ptR_selectedElement.parentNode.appendChild( ptR_selectedElement ); //brings to top
  ptR_selectedElement.setAttributeNS(null, "onmousemove", "moveElement(evt)");
  ptR_selectedElement.setAttributeNS(null, "onmouseout", "deselectElement(evt)");
  ptR_selectedElement.setAttributeNS(null, "onmouseup",  "deselectElement(evt)");
}

// translation of an element
function moveElement(evt) {
  if(ptR_selectedElement!==0){ // this shouldn"t be necessary 
    var ptP = cursorPoint(evt);
    ptP =  diff(ptP, ptR_cxy);
    ptP.x= Math.abs(ptP.x);
    ptP.y= Math.abs(ptP.y);
    var lenvt = length(ptP);
    if( lenvt < 0.001 ){ //not too close
      return;
    }
    var scaleMatrix=scaleM( ptR_cxy, ptR_ptQ, ptP );
    var ctm=scaleMatrix.multiply(ptR_originalCTM );
    ptR_currentMatrix=[ctm.a, ctm.b, ctm.c, ctm.d, ctm.e, ctm.f];
    ptR_selectedElement.setAttributeNS(null, "transform", "matrix(" + ptR_currentMatrix.join(" ") + ")");   
  }
  
}

// deselect that element
function deselectElement(evt) {
  if(ptR_selectedElement !== 0){
    var tid = ptR_selectedElement.getAttribute("tid");    
    var currentMatrixAsString="c(" + ptR_currentMatrix.join(",") + ")";    
    var chosen=["scale", currentMatrixAsString, tid];
    Shiny.onInputChange("mouseMssg",chosen);    
    ptR_selectedElement.removeAttributeNS(null, "onmousemove");
    ptR_selectedElement.removeAttributeNS(null, "onmouseout");
    ptR_selectedElement.removeAttributeNS(null, "onmouseup");
    ptR_selectedElement = 0;
  }
}
