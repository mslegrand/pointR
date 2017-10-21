
function PtRPanelRotate(svgId){ //currently svgId is not used here
  this.svg=document.querySelector("#" + svgId); 
  this.svgId=svgId +"_ROTATE";
  this.selectedElement = 0; //
  
  this.cxy = this.svg.createSVGPoint(); //
  
  this.ut  = this.svg.createSVGPoint(); //
  this.originalCTM = this.svg.createSVGMatrix(); //
  this.currentMatrix = 0; //
}



PtRPanelRotate.prototype.cursorPoint = function(evt){
  var tmp=this.svg.createSVGPoint();
  tmp.x = evt.clientX; tmp.y = evt.clientY;
  return tmp.matrixTransform(this.svg.getScreenCTM().inverse());
};

PtRPanelRotate.prototype.dot = function(v1,v2){
        return v1.x * v2.x + v1.y * v2.y;
};

PtRPanelRotate.prototype.norm = function(v){
  return Math.sqrt(this.dot(v,v));
};

PtRPanelRotate.prototype.getCos = function( u, v){
  var l=this.norm(u)*this.norm(v);
  if(l===0) {
    return 1;
  } else {
    return this.dot(u,v)/l;
  }
};

PtRPanelRotate.prototype.getSin = function( u, v ){
  var l= this.norm(u) * this.norm(v);
  if( l===0 ){
    return 1;
  } else {
    return -(u.x*v.y-v.x*u.y)/l;
  }
};

PtRPanelRotate.prototype.diff = function(u,v){
  var tmp=this.svg.createSVGPoint();
  tmp.x = u.x - v.x; 
  tmp.y = u.y - v.y;
  return tmp;
};

PtRPanelRotate.prototype.rotM = function( cxy, u, v){
  var c=this.getCos(u,v);
  var s=this.getSin(u,v);
  var h=cxy.x;
  var k=cxy.y;
  var m=this.svg.createSVGMatrix();
  m.a=c; m.b=s; m.c=-s; m.d=c; m.e=h+k*s-h*c; m.f= k-h*s-k*c;
  return m;
};


// selectElement 
PtRPanelRotate.prototype.selectElement = function(evt) {
  var pth = "ptRPlotter_"+this.svgId;
  
  this.selectedElement = evt.currentTarget;
  
  console.log("this.selectedElement = " + JSON.stringify(this.selectedElement));  
  
  this.originalCTM=this.selectedElement.getCTM();
  
  // extract cxy
  var bbBox = this.selectedElement.getBBox();
  this.cxy.x= bbBox.x+bbBox.width/2;
  this.cxy.y=bbBox.y+bbBox.height/2;
  this.cxy=  this.cxy.matrixTransform( this.originalCTM);
  
   
  // extract u
  var qt=this.cursorPoint(evt);
  
  
  this.ut=this.diff(qt,  this.cxy);
  
  var lenut = this.norm(this.ut);
  if( lenut < 0.001 ){ //not too close
    this.selectedElement=0;
    return;
  }
  
  
  
  //add eventattrs to element
  this.selectedElement.parentNode.appendChild( this.selectedElement ); //brings to top
  this.selectedElement.setAttributeNS(null, "onmousemove", pth + ".moveElement(evt)");
  this.selectedElement.setAttributeNS(null, "onmouseout", pth + ".deselectElement(evt)");
  this.selectedElement.setAttributeNS(null, "onmouseup",  pth + ".deselectElement(evt)");};

// translation of an element
PtRPanelRotate.prototype.moveElement = function(evt) {
  if(this.selectedElement!==0){ // this shouldn"t be necessary 
    var pt = this.cursorPoint(evt);
    var vt =  this.diff(pt,   this.cxy);
    var lenvt = this.norm(vt);
    if( lenvt < 0.001 ){ //not too close
      return;
    }
    var rotationMatrix=this.rotM(   this.cxy, vt, this.ut );
    var ctm=rotationMatrix.multiply( this.originalCTM );
    this.currentMatrix=[ctm.a, ctm.b, ctm.c, ctm.d, ctm.e, ctm.f];
    this.selectedElement.setAttributeNS(null, "transform", "matrix(" + this.currentMatrix.join(" ") + ")");   
  }
  
};

// deselect that element
PtRPanelRotate.prototype.deselectElement = function(evt) {
  if(this.selectedElement !== 0){
  
    Shiny.onInputChange("mouseMssg",{
      cmd: "rotate",
      vec: this.currentMatrix, // !!! Todo replace with this.currentMatrix
      id :  this.selectedElement.getAttribute("tid")
    });
    this.selectedElement.removeAttributeNS(null, "onmousemove");
    this.selectedElement.removeAttributeNS(null, "onmouseout");
    this.selectedElement.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
  }
};


    
