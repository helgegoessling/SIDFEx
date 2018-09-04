sidfex.fcst.search.extractFromTable <- 
  function(indexTable.path = NULL, gid=NULL, mid=NULL, tid=NULL, 
           iy=NULL, idoy=NULL, emn=NULL, sy=NULL, sdoy=NULL, py=NULL, pdoy=NULL, 
           del=NULL, nt=NULL, fy=NULL, fdoy=NULL, ly=NULL, ldoy=NULL, per=NULL){

    # check if specific directory for indexList is given, otherwise use default 
    if (is.null(indexTable.path)) {
      no.indexTable.path=TRUE
      if (file.exists(file.path("~",".SIDFEx"))) {
        source(file.path("~",".SIDFEx"))
        if (exists("indexTable.path.in")) {no.indexTable.path=FALSE}
      }
      if (no.indexTable.path) {
        stop(paste0("With indexTable.path=NULL , indexTable.path.in must be specified in a file ~/.SIDFEx as a line like indexTable.path.in=..."))
      }
    } else {
      indexTable.path.in = indexTable.path
    } 
    
    # open table
    if(file.exists(paste0(indexTable.path.in, "indexTable.rda"))) {
      load(paste0(indexTable.path.in, "indexTable.rda"))
    }
    else {
      print("No indexTable.rda at given location.")
      return(NULL)
    }
    
    # work on a copy of rTab, not rTab itself (most certainly not necessary, but old habit)
    dummy.df = rTab
    
    # filter rows by criteria on columns, first the easy ones (point queries)
    if (!is.null(gid)) {dummy.df <- dummy.df[dummy.df$GroupID == gid,]}
    if (!is.null(tid)) {dummy.df <- dummy.df[dummy.df$TargetID == tid,]}
    if (!is.null(mid)) {dummy.df <- dummy.df[dummy.df$MethodID == mid,]}
    
    # now take care of (possible) range queries
    if (!is.null(emn)) {dummy.df <- dummy.df[dummy.df$EnsMemNum %in% emn,]}
    if (!is.null(per)) {dummy.df <- dummy.df[dummy.df$FcstTime %in% per,]}
    if (!is.null(nt)) {dummy.df <- dummy.df[dummy.df$nTimeSteps %in% nt,]}
    if (!is.null(del)) {dummy.df <- dummy.df[dummy.df$Delay %in% del,]}
    
    if (!is.null(idoy)) {dummy.df <- dummy.df[dummy.df$InitDayOfYear %in% idoy,]}
    if (!is.null(iy)) {dummy.df <- dummy.df[dummy.df$InitYear %in% iy,]}
    if (!is.null(fdoy)) {dummy.df <- dummy.df[dummy.df$InitDayOfYear %in% fdoy,]}
    if (!is.null(fy)) {dummy.df <- dummy.df[dummy.df$InitYear %in% fy,]}
    if (!is.null(ldoy)) {dummy.df <- dummy.df[dummy.df$InitDayOfYear %in% ldoy,]}
    if (!is.null(ly)) {dummy.df <- dummy.df[dummy.df$InitYear %in% ly,]}
    if (!is.null(pdoy)) {dummy.df <- dummy.df[dummy.df$InitDayOfYear %in% pdoy,]}
    if (!is.null(py)) {dummy.df <- dummy.df[dummy.df$InitYear %in% py,]}
    if (!is.null(sdoy)) {dummy.df <- dummy.df[dummy.df$InitDayOfYear %in% sdoy,]}
    if (!is.null(sy)) {dummy.df <- dummy.df[dummy.df$InitYear %in% sy,]}
    
    # return list of files
    return(dummy.df$File)
  }