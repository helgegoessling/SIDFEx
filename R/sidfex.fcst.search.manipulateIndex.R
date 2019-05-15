sidfex.fcst.search.manipulateIndex <-
  function(rTab) {

    gids_mids = c("nrl001_flatearth24","nrl001_navyespc-subseasonal")

    for (gimi in gids_mids) {

      gid = strsplit(gimi,"_")[[1]][1]
      mid = strsplit(gimi,"_")[[1]][2]

      tids = unique(rTab$TargetID[rTab$GroupID==gid & rTab$MethodID==mid])

      for (tid in tids) {

        gmt.inds = which(rTab$GroupID==gid & rTab$MethodID==mid & rTab$TargetID==tid)
        rTab.gmt = rTab[gmt.inds,]

        n = 1
        n.gmt = nrow(rTab.gmt)
        ne = 1
        repeat {
          n.end = (n >= n.gmt)
          if (n.end || rTab.gmt$EnsMemNum[n+1] <= rTab.gmt$EnsMemNum[n]) {
            if (ne > 1) {
              rTab.gmt$EnsSize[(n-ne+1):n] = ne
              rTab.gmt$EnsParentFile[(n-ne+1):n] = rTab.gmt$File[n]
              ne = 1
            }
          } else {
            ne = ne + 1
          }
          n = n + 1
          if (n.end) {break}
        }

        rTab[gmt.inds,] = rTab.gmt[,]

      }

    }

    return(rTab)
  }
