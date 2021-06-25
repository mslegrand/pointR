
function PtRPanelTagDrag(svgId){ //currently svgId is not used here
  this.selectedElement = 0;
  this.currentX = 0;
  this.currentY = 0;
  this.origX = 0;
  this.origY = 0;
  this.svgId=svgId;
  this.svg=document.querySelector("#" + svgId); 
  this.sx=1.0;
  this.sy=1.0;
  if(this.svg.getAttribute('viewBox')){
    //console.log(JSON.stringify( this.svg.getAttribute('viewBox') ));
    var vb=this.svg.getAttribute('viewBox').split(/[ ,]+/).filter(Boolean);
    //console.log('vb='+JSON.stringify(vb));
    this.sx=Number(vb[2]);
    this.sy=Number(vb[3]);
    this.sx=this.sx/Number(this.svg.getAttribute('width'));
    //console.log('svg height='+this.svg.getAttribute('height'));
    this.sy=this.sy/Number(this.svg.getAttribute('height'));
    //console.log('sy='+this.sy);
    //console.log('sx='+this.sx);
  } 
  this.currentMatrix = 0;
  this.pt= this.svg.createSVGPoint();
}

//TRANSFORM

// selectElement 
PtRPanelTagDrag.prototype.selectElement = function (evt) {
  evt.stopPropagation();
  this.selectedElement = evt.currentTarget;
  this.currentX = evt.clientX;
  this.currentY = evt.clientY;
  this.origX    = evt.clientX;
  this.origY    = evt.clientY;
  this.currentMatrix = this.selectedElement.getAttributeNS(null, "transform").slice(7,-1).split(" ");
  //console.log(JSON.stringify( this.selectedElement.getAttributeNS(null, "transform") ));
  for(var i=0; i<this.currentMatrix.length; i++) {
    this.currentMatrix[i] = parseFloat(this.currentMatrix[i]);
  }
  //add eventattrs to element
  //this.selectedElement.parentNode.appendChild( this.selectedElement ); //brings to top
  var pth = "ptRPlotter_"+this.svgId;
  
  this.selectedElement.setAttributeNS(null, "onmousemove", pth + ".moveElement(evt)");
  //this.svg.setAttributeNS(null, "onmouseout", pth + ".deselectElement(evt)");
  this.svg.setAttributeNS(null, "onmouseup",  pth + ".deselectElement(evt)");
};

// translation of an element
PtRPanelTagDrag.prototype.moveElement = function (evt) {
  if(this.selectedElement!==0){ // this should not be necessary
    evt.stopPropagation();
//    this.svg.parentNode.style.cursor="move";
//    this.selectedElement.style.cursor="move";
    var dx = evt.clientX - this.currentX;
    var dy = evt.clientY - this.currentY;
    
    this.currentMatrix[4] += (dx*this.sx);
    this.currentMatrix[5] += (dy*this.sy);

    this.selectedElement.setAttributeNS(null, "transform", "matrix(" + this.currentMatrix.join(" ") + ")");
    this.currentX = evt.clientX;
    this.currentY = evt.clientY;
  }
};
  
  // deselect that element
PtRPanelTagDrag.prototype.deselectElement =  function (evt) {
  if(this.selectedElement !== 0){
    
    evt.stopPropagation();
    var movedByX = evt.clientX - this.origX;
    var movedByY = evt.clientY - this.origY;
    this.selectedElement.style.cursor="default";
    this.svg.parentNode.style.cursor=="default";
    var kc=$( "#svgOutPanel" ).data("keycode");
    let r = Math.random().toString(36).substring(7);
    var dxy=[movedByX*this.sx, movedByY*this.sy];
    console.log('moving by'+ JSON.stringify( dxy  ));
    Shiny.onInputChange("mouseMssg",
      {
          cmd: "transGrp",
          
          vec: [movedByX*this.sx, movedByY*this.sy],
          id : this.selectedElement.getAttribute("tid"),
          keycode:      kc,
          altKey:   !!evt.altKey,
          shiftKey: !!evt.shiftKey,
          ctrlKey:  !!evt.ctrlKey,
          metaKey:  !!evt.metaKey , 
          rnd: r
      }
    );
    this.selectedElement.removeAttributeNS(null, "onmousemove");
    this.selectedElement.removeAttributeNS(null, "onmouseout");
    this.selectedElement.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
    
  }
};








