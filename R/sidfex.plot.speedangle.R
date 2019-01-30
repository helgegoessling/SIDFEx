sidfex.plot.speedangle <- function(index=NULL,read.fcst.res=NULL,read.obs.res=NULL,remap.res=NULL,
                                   col.by="DaysLeadTime",colbar=sl.colbar.redgreyblue_256,
                                   colbar.breaks=NULL,colbar.breaks.log=FALSE,points.type="p",device="pdf",
                                   file=paste0("~/sidfex.plot.speedangle.",device),width=NULL,
                                   labels=TRUE,labels.cex=1,bg.lines.lwd=labels.cex,...) {

  if (is.null(width) && !is.null(device)) {
    if (device == "pdf") {width = 8} else {width = 600}
  }

  if (is.null(read.fcst.res)) {
    if (is.null(index)) {stop("At least one of 'index' and 'read.fcst.res' must be specified.")}
    read.fcst.res = sidfex.read.fcst(index)
    flx = sapply(strsplit(sapply(strsplit(sapply(read.fcst.res$res.list,"[[","fl"),split="/"),tail,n=1),split=".txt",fixed=TRUE),head,n=1)
  }
  else {
    if (is.null(index)) {
      index = sidfex.load.index()
      flx = sapply(strsplit(sapply(strsplit(sapply(read.fcst.res$res.list,"[[","fl"),split="/"),tail,n=1),split=".txt",fixed=TRUE),head,n=1)
      index = index[index$File %in% flx,]
    } else if (nrow(index) != length(read.fcst.res$res.list)) {
      stop("'index' and 'read.fcst.res$res.list' must have the same length")
    } else {
      flx = sapply(strsplit(sapply(strsplit(sapply(read.fcst.res$res.list,"[[","fl"),split="/"),tail,n=1),split=".txt",fixed=TRUE),head,n=1)
    }
  }

  # remap observations temporally to forecast times
  if (is.null(remap.res)) {
    remap.res = sidfex.remaptime.obs2fcst(obs=read.obs.res,fcst=read.fcst.res)
  }

  is.continuous = TRUE
  categs=NULL
  col.from.index = FALSE
  if (col.by %in% names(index) && col.by != "File") {
    col.from.index = TRUE
    col.vals = index[[col.by]]
    if (col.by %in% c("GroupID","MethodID","TargetID","InitYear","EnsMemNum","SubmitYear","ProcessedYear",
                      "FirstTimeStepYear","LastTimeStepYear")) {
      is.continuous = FALSE
      categs = unique(col.vals)
      Ncategs = length(categs)
    }
  } else if (col.by %in% c("Year","DayOfYear","Lat","Lon","DaysLeadTime")) {
    col.vals = unlist(lapply(lapply(read.fcst.res$res.list,"[[","data"),"[[",col.by))
    if (col.by == "Year") {
      is.continuous = FALSE
      categs = unique(col.vals)
      Ncategs = length(categs)
    }
  } else {
    stop(paste0("'cols.by' must be one of '",paste(names(index)[names(index)!="File"],collapse="', '"),"', '",
                paste(c("Year","DayOfYear","Lat","Lon","DaysLeadTime"),collapse="', '"),"'"))
  }

  if (!is.continuous) {
    if (length(colbar) < Ncategs) {
      stop(paste0("'length(colbar)' (",length(colbar),") must be >= the number of categories (",Ncategs,")"))
    } else if (length(colbar) > Ncategs) {
      warning(paste0("'colbar' (",length(colbar)," elements) is reduced to the number of categories (",Ncategs,")"))
      if (Ncategs > 1) {colbar = colbar[round(seq(1,length(colbar),(length(colbar)-1)/(Ncategs-1)))]}
      else {colbar = colbar[1]}
    }
  }

  if (is.null(colbar.breaks)) {
    require(spheRlab)
    if (is.continuous) {
      print(paste0("values for '",col.by,"' to determine the colourbar are in the following range:"))
      print(quantile(col.vals))
      colbar.breaks = sl.num2colbarbreaks(num=col.vals, breaks.log=colbar.breaks.log, colbar=colbar)
    }
  } else {
    if (!is.continuous) {
      warning("'colbar.breaks' are ignored when colours are defined from a categorical variable")
      colbar.breaks = NULL
    } else if (length(colbar) != length(colbar.breaks)+1) {
      colbar = sl.colbar(cols = colbar, N = length(colbar.breaks)+1)
    }
  }

  if (!is.null(device)) {
    dev.fun = match.fun(device,descend=FALSE)
    dev.fun(file=file, width=width, height=width)
  }
  par(mar=rep(0,4))

  plot(NA,xlim=c(-2,2),ylim=c(-2,2),xaxt="n",yaxt="n",bty="n")
  unitcircle.x = sin(seq(0,2*pi,2*pi/360))
  unitcircle.y = cos(seq(0,2*pi,2*pi/360))
  lines(x=c(-2,2),y=c(0,0),col="grey",lwd=bg.lines.lwd)
  lines(x=c(0,0),y=c(0,-2),col="grey",lwd=bg.lines.lwd)
  lines(x=c(0,0),y=c(0,2),col="grey",lwd=bg.lines.lwd)
  lines(x=c(-sqrt(2),sqrt(2)),y=c(-sqrt(2),sqrt(2)),col="grey",lwd=bg.lines.lwd)
  lines(x=c(-sqrt(2),sqrt(2)),y=c(sqrt(2),-sqrt(2)),col="grey",lwd=bg.lines.lwd)
  lines(unitcircle.x,unitcircle.y,col="grey",lwd=bg.lines.lwd)

  for (i in 1:length(read.fcst.res$res.list)) {
    Nfcst = read.fcst.res$res.list[[i]]$Ntimesteps
    ilat = remap.res$res.list[[i]]$InitLat
    ilon = remap.res$res.list[[i]]$InitLon
    abg = sl.lonlatrot2abg(lonlatrot=c(ilon,ilat,0))
    fcst.rot = sl.rot(lon=read.fcst.res$res.list[[i]]$data$Lon,lat=read.fcst.res$res.list[[i]]$data$Lat,alpha=abg[1],beta=abg[2],gamma=abg[3])
    obs.rot = sl.rot(lon=remap.res$res.list[[i]]$data$Lon,lat=remap.res$res.list[[i]]$data$Lat,alpha=abg[1],beta=abg[2],gamma=abg[3])
    latx = (90-obs.rot$lat)
    relspeed = (90-fcst.rot$lat) / latx
    rslt1 = (relspeed>1)
    rslt1[is.na(rslt1)] = FALSE
    relspeed[rslt1] = 2 - (1/relspeed[rslt1])
    ang = fcst.rot$lon - obs.rot$lon
    ang[latx==0] = NA
    if (col.from.index) {
      if (is.continuous) {
        col.ind = sl.num2colbar(num=index[[col.by]][match(flx[i],index$File)],
                                breaks=colbar.breaks)$colour.index
      } else {
        col.ind = match(index[[col.by]][match(flx[i],index$File)],categs)
      }
    } else {
      if (is.continuous) {
        col.ind = sl.num2colbar(num=remap.res$res.list[[i]]$data[[col.by]],
                                breaks=colbar.breaks)$colour.index
      } else {
        col.ind = match(remap.res$res.list[[i]]$data[[col.by]],categs)
      }
    }
    col = unlist(colbar)[col.ind]
    x = -sin(2*pi*ang/360)*relspeed
    y = cos(2*pi*ang/360)*relspeed
    if (!col.from.index && points.type %in% c("l","b")) {
      for (ip in 1:(length(ang)-1)) {
        points(x=x[ip:(ip+1)],y=y[ip:(ip+1)],col=col[ip],...,type="l")
      }
      if (points.type == "b") {
        for (ip in 1:length(ang)) {
          points(x=x[ip],y=y[ip],col=col[ip],...,type="p")
        }
      }
    } else {
      points(x=x,y=y,col=col,...,type=points.type)
    }
  }

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

  return(list(col.by=col.by,colbar=colbar,categorical=!is.continuous,breaks=colbar.breaks,labels=categs))

}
