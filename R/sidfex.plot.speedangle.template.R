sidfex.plot.speedangle.template <- function(device="pdf", file=paste0("~/sidfex.plot.speedangle.template.",device), width=NULL,
                                   labels=TRUE,labels.cex=1,bg.lines.lwd=labels.cex,...) {
  
  if (is.null(width) && !is.null(device)) {
    if (device == "pdf") {width = 8} else {width = 600}
  }
  
  if (!is.null(device)) {
    dev.fun = match.fun(device,descend=FALSE)
    dev.fun(file=file, width=width, height=width)
  }
  
  par(mar=rep(-0,4), oma=c(0,0,0,0))
  
  plot(NA,xlim=c(-2,2),ylim=c(-2,2),xaxt="n",yaxt="n",bty="n")
  unitcircle.x = sin(seq(0,2*pi,2*pi/360))
  unitcircle.y = cos(seq(0,2*pi,2*pi/360))
  lines(x=c(-2,2),y=c(0,0),col="grey",lwd=bg.lines.lwd)
  lines(x=c(0,0),y=c(0,-2),col="grey",lwd=bg.lines.lwd)
  lines(x=c(0,0),y=c(0,2),col="grey",lwd=bg.lines.lwd)
  lines(x=c(-sqrt(2),sqrt(2)),y=c(-sqrt(2),sqrt(2)),col="grey",lwd=bg.lines.lwd)
  lines(x=c(-sqrt(2),sqrt(2)),y=c(sqrt(2),-sqrt(2)),col="grey",lwd=bg.lines.lwd)
  lines(unitcircle.x,unitcircle.y,col="grey",lwd=bg.lines.lwd)
  
  lines(2*unitcircle.x,2*unitcircle.y,lwd=bg.lines.lwd)
  if (labels) {
    axis(side=1,at=c(0,.25,.5,.75,1,1.25,1.5,1.75,2),labels=c(as.character(c(0,0.25,0.5,0.75,1,1.33,2,4)),"Inf      "),pos=0,lwd=bg.lines.lwd,cex.axis=labels.cex)
    text("relative drift distance (fcst/obs)",x=1,y=-0.3,cex=labels.cex)
    lines.x = sin(seq(pi/4,2*pi,pi/4))
    lines.y = cos(seq(pi/4,2*pi,pi/4))
    lines.y[2] = 0.03
    labs = c("45R","90R","135R","180","135L","90L","45L","0")
    for (ilab in 1:length(labs)) {
      text(labs[ilab],x=lines.x[ilab]*1.9,y=lines.y[ilab]*1.9,cex=labels.cex)
    }
    text("relative",x=0.25,y=1.88,srt=-7,cex=labels.cex)
    text("drift",x=0.51,y=1.825,srt=-17,cex=labels.cex)
    text("angle",x=0.73,y=1.74,srt=-23,cex=labels.cex)
    text("(fcst-obs)",x=1.03,y=1.58,srt=-33,cex=labels.cex)
  } else {
    axis(side=1,at=c(0,.25,.5,.75,1,1.25,1.5,1.75,2),labels="",pos=0)
  }
  lines(x=c(0,0),y=c(0.95,1.05),lwd=bg.lines.lwd*3)
  lines(x=c(-0.05,0.05),y=c(1,1),lwd=bg.lines.lwd*3)

  if (!is.null(device)) {dev.off()}
}