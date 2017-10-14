// var chosen =[];
// var movedTo=[];

var ptR_selectedElement = 0;
var ptR_currentMatrix   = 0;

var ptR_svg = document.querySelector("#ptR_SVG_TRANSFORM");
var ptR_cxy = ptR_svg.createSVGPoint();  //center of shape
var ptR_ut  = ptR_svg.createSVGPoint();
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
  var tmp=ptR_svg.createSVGPoint();
  tmp.x = u.x - v.x; 
  tmp.y = u.y - v.y;
  return tmp;
}

function rotM( cxy, u, v){
  var c=getCos(u,v);
  var s=getSin(u,v);
  var h=cxy.x;
  var k=cxy.y;
  var m=ptR_svg.createSVGMatrix();
  m.a=c; m.b=s; m.c=-s; m.d=c; m.e=h+k*s-h*c; m.f= k-h*s-k*c;
  return m;
}


// selectElement 
function selectElement(evt) {
  ptR_selectedElement = evt.currentTarget;
    
  ptR_originalCTM=ptR_selectedElement.getCTM();
  
  // extract cxy
  var bbBox = ptR_selectedElement.getBBox();
    ptR_cxy.x= bbBox.x+bbBox.width/2;
    ptR_cxy.y=bbBox.y+bbBox.height/2;
    ptR_cxy=  ptR_cxy.matrixTransform( ptR_originalCTM);
  // extract u
  var qt=cursorPoint(evt);
  ptR_ut=diff(qt,  ptR_cxy);
  var lenut = length(ptR_ut);
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
    var pt = cursorPoint(evt);
    var vt =  diff(pt,   ptR_cxy);
    var lenvt = length(vt);
    if( lenvt < 0.001 ){ //not too close
      return;
    }
    var rotationMatrix=rotM(   ptR_cxy, vt, ptR_ut );
    var ctm=rotationMatrix.multiply( ptR_originalCTM );
    ptR_currentMatrix=[ctm.a, ctm.b, ctm.c, ctm.d, ctm.e, ctm.f];
    ptR_selectedElement.setAttributeNS(null, "transform", "matrix(" + ptR_currentMatrix.join(" ") + ")");   
  }
  
}

// deselect that element
function deselectElement(evt) {
  if(ptR_selectedElement !== 0){
    //var tid = ptR_selectedElement.getAttribute("tid");    
    //var currentMatrixAsString="c(" + ptR_currentMatrix.join(",") + ")";    
    //var cxyStr="c("+  ptR_cxy.x+","+  ptR_cxy.y+")"; //to use for display?
    //chosen=["rotate", ptR_currentMatrixAsString, tid, cxyStr];
    //var chosen=["rotate", currentMatrixAsString, tid];
    //Shiny.onInputChange("mouseMssg",chosen); 
    Shiny.onInputChange("mouseMssg",{
      cmd: "rotate",
      vec: ptR_currentMatrix, // !!! Todo replace with ptR_currentMatrix
      id :  ptR_selectedElement.getAttribute("tid")
    });
    ptR_selectedElement.removeAttributeNS(null, "onmousemove");
    ptR_selectedElement.removeAttributeNS(null, "onmouseout");
    ptR_selectedElement.removeAttributeNS(null, "onmouseup");
    ptR_selectedElement = 0;
  }
}
