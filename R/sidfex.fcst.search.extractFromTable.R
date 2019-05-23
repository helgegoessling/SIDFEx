sidfex.fcst.search.extractFromTable <-
  function(indexTable.path = NULL, return.dataframe=FALSE, gid=NULL, mid=NULL, tid=NULL,
           iy=NULL, idoy=NULL, emn=NULL, sy=NULL, sdoy=NULL, py=NULL, pdoy=NULL,
           del=NULL, nt=NULL, fy=NULL, fdoy=NULL, ly=NULL, ldoy=NULL, per=NULL, fcstrange=NULL){

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

    if (length(per)>2) {per = NULL; warning("length(per) > 2, parameter 'per' will be ignored")}
    if (length(del)>2) {del = NULL; warning("length(del) > 2, parameter 'del' will be ignored")}
    if (length( nt)>2) {nt  = NULL; warning("length(nt) > 2, parameter 'nt' will be ignored")}
    
    # open table
    if(file.exists(file.path(indexTable.path.in, "indexTable.rda"))) {
      load(file.path(indexTable.path.in, "indexTable.rda"))
    }
    else {
      print("No indexTable.rda at given location.")
      return(NULL)
    }

    # work on a copy of rTab, not rTab itself (most certainly not necessary, but old habit)
    df = rTab

    # filter rows by criteria on columns, first the easy ones
    if (!is.null(gid)) {df <- df[df$GroupID %in% gid,]}
    if (!is.null(tid)) {df <- df[df$TargetID %in% tid,]}
    if (!is.null(mid)) {df <- df[df$MethodID %in% mid,]}
    if (!is.null(emn)) {df <- df[df$EnsMemNum %in% emn,]}
    
    # now take care of (possible) range queries, this min/max thing is an elegant solution for length(<parameter>)=1
    if (!is.null(per)) {df <- df[ which((df$FcstTime <= max(per)) & (df$FcstTime >= min(per))),]}
    if (!is.null(nt)) {df <- df[which((df$nTimeSteps <= max(nt)) & (df$nTimeSteps >= min(nt))),]}
    if (!is.null(del)) {df <- df[which((df$Delay <= max(del)) & (df$Delay >= min(del))),]}

    # now the ugly stuff (taking care of range queries over different years)
    # initial time
    if(!is.null(iy)){
      if (length(iy)==1 | iy[1]==iy[2]){ # case 1: year and day are only a point query (only one year or iy[1]=iy[2])
        df <- df[df$InitYear == iy,]
        if (!is.null(idoy)){
          df <- df[df$InitDayOfYear %in% seq(min(idoy), max(idoy)),] 
        }
      } else if (iy[1]!=iy[2]) { # case 2: year and days are range queries (idoy must have length 2!)
        df <- df[which((df$InitYear==iy[1] & df$InitDayOfYear >= idoy[1])        |
                         (df$InitYear>iy[1] & df$InitYear <iy[2])                 |
                         (df$InitYear==iy[2] & df$InitDayOfYear <= idoy[2])),]
      }
    }

    # first date of forecast
    if(!is.null(fy)){
      if (length(fy)==1 | fy[1]==fy[2]){
        df <- df[df$FirstTimeStepYear == fy,]
        if (!is.null(fdoy)){
          df <- df[df$FirstTimeStepDayOfYear %in% seq(min(fdoy), max(fdoy)),]
        }
      } else if (fy[1]!=fy[2]) {
        df <- df[which((df$FirstTimeStepYear==fy[1] & df$FirstTimeStepDayOfYear >= fdoy[1])        |
                         (df$FirstTimeStepYear > fy[1] & df$FirstTimeStepYear < fy[2])                 |
                         (df$FirstTimeStepYear==fy[2] & df$FirstTimeStepDayOfYear <= fdoy[2])),]
      }
    }

    # last date of forecast
    if(!is.null(ly)){
      if (length(ly)==1 | ly[1]==ly[2]){
        df <- df[df$LastTimeStepYear == ly,]
        if (!is.null(ldoy)){
          df <- df[df$LastTimeStepDayOfYear %in% seq(min(ldoy), max(ldoy)),]
        }
      } else if (ly[1]!=ly[2]) {
        df <- df[which((df$LastTimeStepYear==ly[1] & df$LastTimeStepDayOfYear >= ldoy[1])        |
                         (df$LastTimeStepYear > ly[1] & df$LastTimeStepYear < ly[2])                 |
                         (df$LastTimeStepYear==ly[2] & df$LastTimeStepDayOfYear <= ldoy[2])),]
      }
    }

    # year of file processing
    if(!is.null(py)){
      if (length(py)==1 | py[1]==py[2]){
        df <- df[df$ProcessedYear == py,]
        if (!is.null(pdoy)){
          df <- df[df$ProcessedDayOfYear %in% seq(min(pdoy), max(pdoy)),]
        }
      } else if (py[1]!=py[2]) {
        df <- df[which((df$ProcessedYear==sy[1] & df$ProcessedDayOfYear >= pdoy[1])        |
                         (df$ProcessedYear > sy[1] & df$ProcessedYear < py[2])                 |
                         (df$ProcessedYear==sy[2] & df$ProcessedDayOfYear <= pdoy[2])),]
      }
    }


    # date of submission
    if(!is.null(sy)){
      if (length(sy)==1 | sy[1]==sy[2]){
        df <- df[df$SubmitYear == sy,]
        if (!is.null(sdoy)){
          df <- df[df$SubmitDayOfYear %in% seq(min(sdoy), max(sdoy)),]
        }
      } else if (sy[1]!=sy[2]) {
        df <- df[which((df$SubmitYear==sy[1] & df$SubmitDayOfYear >= sdoy[1])        |
                         (df$SubmitYear > sy[1] & df$SubmitYear < sy[2])                 |
                         (df$SubmitYear==sy[2] & df$SubmitDayOfYear <= sdoy[2])),]
      }
    }

    # range input (important for daterange input from shiny!)
    if(!is.null(fcstrange)){
      fact = 400
      yearmin = as.integer(format(fcstrange[1], "%Y"))
      yearmax = as.integer(format(fcstrange[2], "%Y"))
      doymin =  as.integer(format(fcstrange[1], "%j"))
      doymax =  as.integer(format(fcstrange[2], "%j"))

      # brute force date comparison, baby, yeah
      datemin.float = yearmin + doymin/fact
      datemax.float = yearmax + doymax/fact

      # either (the forecast starts AND/OR ends in the given range) OR (it starts before AND ends after given range)
      df <- df[which(
                      (df$FirstTimeStepYear + df$FirstTimeStepDayOfYear/fact <= datemax.float &
                        df$FirstTimeStepYear + df$FirstTimeStepDayOfYear/fact >= datemin.float) | # case: fcst starts in range
                      (df$LastTimeStepYear + df$LastTimeStepDayOfYear/fact <= datemax.float &
                         df$LastTimeStepYear + df$LastTimeStepDayOfYear/fact >= datemin.float) |  # case: fcst stops in range
                        (df$FirstTimeStepYear + df$FirstTimeStepDayOfYear/fact < datemin.float &
                           df$LastTimeStepYear + df$LastTimeStepDayOfYear/fact > datemax.float)   # case: fcst starts before, stops afterwards
                    )
              ,]
    }

    # return list of files or corresponding data frame
    if (!return.dataframe) {
      return(df$File)
    } else {
      return(df)
    }
  }
