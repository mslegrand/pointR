// scales an element

function PtRPanelScale(svgId){ //currently svgId is not used here
  this.svg=document.querySelector("#" + svgId); 
  this.svgId=svgId +"_SCALE";
  this.selectedElement = 0;
  this.currentX = 0;
  this.currentY = 0;
  this.origX = 0;
  this.origY = 0;
  
  this.cxy = this.svg.createSVGPoint(); 
  this.WH = this.svg.createSVGPoint(); 
  this.ptQ = this.svg.createSVGPoint(); 
  this.originalCTM = this.svg.createSVGMatrix();
  this.currentMatrix = 0;
}




PtRPanelScale.prototype.cursorPoint =  function(evt){
  var tmp=this.svg.createSVGPoint();
  tmp.x = evt.clientX; tmp.y = evt.clientY;
  return tmp.matrixTransform(this.svg.getScreenCTM().inverse());
};

PtRPanelScale.prototype.dot =  function(v1,v2){
        return v1.x * v2.x + v1.y * v2.y;
};

PtRPanelScale.prototype.length =  function(v){
  return Math.sqrt(this.dot(v,v));
};

PtRPanelScale.prototype.diff =  function(u,v){
  var tmp=this.svg.createSVGPoint();
  tmp.x = u.x - v.x; 
  tmp.y = u.y - v.y;
  return tmp;
};

PtRPanelScale.prototype.scaleM =  function( cxy, q, p){
  var m=this.svg.createSVGMatrix();
  //first set to identity
  m.a=1; m.b=0; m.c=0; m.d=1; m.e=0; m.f=0; 
  
  var delta=this.diff(p,q); 
  
  m.a= 1 + 2*delta.x/this.WH.x;
  m.e= cxy.x * (1-m.a);
  m.d= 1 + 2*delta.y/this.WH.y;
  m.f= cxy.y * (1-m.d);
  
  return m;
};

// selectElement 
PtRPanelScale.prototype.selectElement = function(evt) {
  var pth = "ptRPlotter_"+this.svgId;
   
  this.selectedElement = evt.currentTarget;
  
  
  this.originalCTM=this.selectedElement.getCTM();
  
  // extract cxy
  var bbBox = this.selectedElement.getBBox();
  this.WH.x=bbBox.width;
  this.WH.y=bbBox.height;
  this.cxy.x= bbBox.x+bbBox.width/2;
  this.cxy.y= bbBox.y+bbBox.height/2;
  this.cxy=this.cxy.matrixTransform(this.originalCTM);
  // check
  this.ptQ=this.cursorPoint(evt);
  this.ptQ=this.diff(this.ptQ,this.cxy);
  this.ptQ.x=Math.abs(this.ptQ.x);
  this.ptQ.y=Math.abs(this.ptQ.y);
  var lenut = this.length(this.ptQ);
  if( lenut < 0.001 ){ //not too close
    this.selectedElement=0;
    return;
  }
  
  
  
  //add eventattrs to element
  this.selectedElement.parentNode.appendChild( this.selectedElement ); //brings to top
  this.selectedElement.setAttributeNS(null, "onmousemove", pth + ".moveElement(evt)");
  this.svg.setAttributeNS(null, "onmouseout", pth + ".deselectElement(evt)");
  this.svg.setAttributeNS(null, "onmouseup",  pth + ".deselectElement(evt)");
};


// translation of an element
PtRPanelScale.prototype.moveElement = function(evt) {
  if(this.selectedElement!==0){ // this shouldn"t be necessary 
    var ptP = this.cursorPoint(evt);
    ptP =  this.diff(ptP, this.cxy);
    ptP.x= Math.abs(ptP.x);
    ptP.y= Math.abs(ptP.y);
    var lenvt = this.length(ptP);
    if( lenvt < 0.001 ){ //not too close
      return;
    }
    var scaleMatrix=this.scaleM( this.cxy, this.ptQ, ptP );
    var ctm=scaleMatrix.multiply(this.originalCTM );
    this.currentMatrix=[ctm.a, ctm.b, ctm.c, ctm.d, ctm.e, ctm.f];
    this.selectedElement.setAttributeNS(null, "transform", "matrix(" + this.currentMatrix.join(" ") + ")");   
  }
  
};

// deselect that element
PtRPanelScale.prototype.deselectElement = function(evt) {
  if(this.selectedElement !== 0){
    var tid = this.selectedElement.getAttribute("tid"); 

    Shiny.onInputChange("mouseMssg",
    {
        cmd: "scale",
        vec: this.currentMatrix,
        id: tid,
        altKey:   !!evt.altKey,
        shiftKey: !!evt.shiftKey,
        ctrlKey:  !!evt.ctrlKey,
        metaKey:  !!evt.metaKey
    });
    
    this.selectedElement.removeAttributeNS(null, "onmousemove");
    this.svg.removeAttributeNS(null, "onmouseout");
    this.svg.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
  }
};



