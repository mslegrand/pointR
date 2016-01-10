var chosen =[];
var movedTo=[];

var selectedElement = 0;
var currentMatrix   = 0;
//var originalMatrix  =0;

//var tmp=svg.createSVGPoint();

var svg = document.querySelector("svg");
//var pt  = svg.createSVGPoint();
//var qt  = svg.createSVGPoint(); //original mousedown point
var cxy = svg.createSVGPoint();  //center of shape
var ut  = svg.createSVGPoint();
var originalCTM=svg.createSVGMatrix();

//var q=[]; //original mousedown point
//var u=[]; //not needed

//var pt = svg.createSVGPoint();

//return pt.matrixTransform(svg.getScreenCTM().inverse());

function cursorPoint(evt){
  var tmp=svg.createSVGPoint();
  tmp.x = evt.clientX; tmp.y = evt.clientY;
  return tmp.matrixTransform(svg.getScreenCTM().inverse());
}

// my defs
function dot(v1,v2){
        return v1.x * v2.x + v1.y * v2.y;
}

function length(v){
  return Math.sqrt(dot(v,v));
}

function multMatMat(m1,m2){
//hint
// m1[0,]=m1[0,2,4]
// m1[,0]=m1[0,1]
  var m=[m1[0]*m2[0] + m1[2]*m2[1],
  m1[1]*m2[0] + m1[3]*m2[1],
  m1[0]*m2[2] + m1[2]*m2[3],
  m1[1]*m2[2] + m1[3]*m2[3],
  m1[0]*m2[4] + m1[2]*m2[5] + m1[4],
  m1[1]*m2[2] + m1[3]*m2[3] + m1[5]];
  return m;
}
  
function multMatVec(m,v){
  var r=[ m[0]*v[0]+m[2]*v[1]+m[4], 
    m[1]*v[0]+m[3]*v[1]+m[5] ];
  return r;
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
  //alert("hi")
  var c=getCos(u,v);
  //alert("hi");
  var s=getSin(u,v);
  var h=cxy.x;
  var k=cxy.y;
  //alert('hi');
  //var m=[c, s, -s, c, h*(k*s-h*c), -k*(h*s+k*c)]; 
  var m=svg.createSVGMatrix();
  m.a=c; m.b=s; m.c=-s; m.d=c; m.e=h+k*s-h*c; m.f= k-h*s-k*c;
  return m;
}


// selectElement 
function selectElement(evt) {
  selectedElement = evt.target;
  
  //extract originalMatrix
  //originalMatrix = selectedElement.getAttributeNS(null, "transform").slice(7,-1).split(' ');
  //for(var i=0; i<originalMatrix.length; i++) {
  //  originalMatrix[i] = parseFloat(originalMatrix[i]);
  //} 
  
  originalCTM=selectedElement.getCTM();
  
  // extract cxy
  var bbBox = selectedElement.getBBox();
  cxy.x= bbBox.x+bbBox.width/2;
  cxy.y=bbBox.y+bbBox.height/2;
  //cxy=multMatVec(originalMatrix,cxy);
  cxy=cxy.matrixTransform(originalCTM);
  // extract u
  var qt=cursorPoint(evt);
  //q=[evt.clientX,evt.clientY];
  ut=diff(qt,cxy);
  // alert("ut.x=" + ut.x + ", ut.y=" + ut.y);
  var lenut = length(ut);
  // alert("length ut=" + lenut);
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
  if(selectedElement!==0){ // this shouldn't be necessary
  
    // get cxy, p, q, and compute rotation.
    // apply rotation to saved currentMatrix, 
    // get product and set selected element
    //var p = [evt.clientX, evt.clientY];
    //var pt=cursorPoint(evt);
    //var v=  diff(p, cxy);
//    var rotationMatrix=rotM( cxy, v, u );
//    currentMatrix=multMatMat(rotationMatrix,originalMatrix);
//    selectedElement.setAttributeNS(null, "transform", "matrix(" + currentMatrix.join(' ') + ")");
//  };
  
    var pt = cursorPoint(evt);
    var vt =  diff(pt, cxy);
    var lenvt = length(vt);
    if( lenvt < 0.001 ){ //not too close
      return;
    }
   
    var rotationMatrix=rotM( cxy, vt, ut );
    var ctm=rotationMatrix.multiply(originalCTM );
    currentMatrix=[ctm.a, ctm.b, ctm.c, ctm.d, ctm.e, ctm.f];

    selectedElement.setAttributeNS(null, "transform", "matrix(" + currentMatrix.join(' ') + ")");
    
    
  }
  
}

// deselect that element
function deselectElement(evt) {
  if(selectedElement !== 0){
    
    //alert("deselecting");
    var tid = selectedElement.getAttribute("tid");
    
    var currentMatrixAsString="c(" + currentMatrix.join(',') + ")";
    
    var cxyStr="c("+cxy.x+","+cxy.y+")"; //to use for display?
    chosen=["rotate", currentMatrixAsString, tid, cxyStr];
    Shiny.onInputChange("mydata",chosen);
    
    selectedElement.removeAttributeNS(null, "onmousemove");
    selectedElement.removeAttributeNS(null, "onmouseout");
    selectedElement.removeAttributeNS(null, "onmouseup");
    selectedElement = 0;
    
  }
}
