
function PtRPanelPoints(svgId){
  this.selectedElement = 0;
  this.currentX = 0;
  this.currentY=0;
  this.svgId=svgId;
  this.svg=document.querySelector("#" + svgId); 
  this.pt= this.svg.createSVGPoint();
}

//called to create a new point
PtRPanelPoints.prototype.newPoint = function (evt) {
  this.pt.x = evt.clientX;
  this.pt.y = evt.clientY;
  
  // The cursor point, translated into svg coordinates
  var cursorpt =  this.pt.matrixTransform(this.svg.getScreenCTM().inverse());
  Shiny.onInputChange("mouseMssg",
                      {
                        cmd: "add",
                        vec: [cursorpt.x, cursorpt.y],
                        id: "dummyId"
                      }
  );
};

// implements moving a single point
PtRPanelPoints.prototype.selectPoint = function (evt){
  this.selectedElement = evt.target;
  this.currentX = evt.clientX;
  this.currentY = evt.clientY;
  var pth = "ptRPlotter_"+this.svgId;
  this.selectedElement.setAttributeNS(null, "onmousemove",  pth + ".movePoint(evt)");
  this.svg.setAttributeNS(null, "onmouseout", pth + ".deselectPoint(evt)");
  this.svg.setAttributeNS(null, "onmouseup",  pth + ".deselectPoint(evt)");
};

PtRPanelPoints.prototype.movePoint = function (evt){
  if(this.selectedElement !== 0){
    this.selectedElement = evt.target;
    var dx = evt.clientX - this.currentX;
    var dy = evt.clientY - this.currentY; 
    
    //get the element attribute values x,y 
    var cx = Number(this.selectedElement.getAttribute("cx"));
    var cy = Number(this.selectedElement.getAttribute("cy"));
    // update each attribute by x=x+dx, y=y+dy
    cx=cx+dx;
    cy=cy+dy;
    this.selectedElement.setAttributeNS(null, "cx", cx.toString());
    this.selectedElement.setAttributeNS(null, "cy", cy.toString());
    //update the current position
    this.currentX = evt.clientX;
    this.currentY = evt.clientY;
  }
};

PtRPanelPoints.prototype.deselectPoint = function (evt){
  if(this.selectedElement !== 0){
    this.pt.x = evt.clientX;
    this.pt.y = evt.clientY;
    
    // The cursor point, translated into svg coordinates
    var cursorpt =  this.pt.matrixTransform(this.svg.getScreenCTM().inverse());
    
    Shiny.onInputChange("mouseMssg",{
      cmd: "move",
      vec: [cursorpt.x, cursorpt.y],
      id: this.selectedElement.getAttribute("id")
    });
    this.selectedElement.removeAttributeNS(null, "onmousemove");
    this.svg.removeAttributeNS(null, "onmouseout");
    this.svg.removeAttributeNS(null, "onmouseup");
    this.selectedElement = 0;
  }              
};



