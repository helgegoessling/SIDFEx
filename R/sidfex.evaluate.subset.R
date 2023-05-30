sidfex.evaluate.subset <- function (eval.res=NULL,keep=NULL,do.multifcst.stats=TRUE,multifcst.stats.na.rm=TRUE) {

  require(spheRlab)

  if (is.null(eval.res)) {stop("argument 'eval.res' must be provided")}
  if (!("res.list" %in% names(eval.res))) {stop("'eval.res' must contain an element named 'res.list'")}
  if (is.null(keep)) {stop("argument 'keep' must be provided")}
  keep.falseformat = FALSE
  if (!is.vector(keep)) {keep.falseformat = TRUE} else {
    if (length(keep) < length(eval.res$res.list)) {
      if (length(keep) < 2 || any(keep != floor(keep))) {keep.falseformat = TRUE}
      if (max(keep) > length(eval.res$res.list) || min(keep) < 1) {keep.falseformat = TRUE}
    } else {
      if (length(keep) != length(eval.res$res.list)) {keep.falseformat = TRUE} else {
        if (!is.logical(keep)) {keep.falseformat = TRUE}
      }
    }
  }
  if (keep.falseformat) {
    stop("'keep' must be a vector consisting of at least two (positive) integer numbers or be a logical vector of the same length as 'eval.res$res.list'")
  }

  rl = eval.res$res.list[keep]

  multifcst.stats = NULL
  if (do.multifcst.stats) {
    multifcst.stats.names = c("ens.mean.gc.dist","ens.mean.lat.err","ens.mean.lon.err","ens.individual.gc.dist.mean",
                              "ens.individual.lat.err.mean","ens.individual.lat.err.meanabs",
                              "ens.individual.lon.err.mean","ens.individual.lon.err.meanabs",
                              "ens.spread.gc.dist","ens.spread.lat","ens.spread.lon","ens.mean.relspeed","ens.mean.angle",
                              "ens.individual.relspeed.mean","ens.individual.angle.mean","ens.individual.angle.meanabs")
    multifcst.stats.names.present = NULL
    for (i in 1:length(rl)) {
      multifcst.stats.names.present = c(multifcst.stats.names.present,names(rl[[i]]))
    }
    multifcst.stats.names.present = unique(multifcst.stats.names.present)
    multifcst.stats.names = multifcst.stats.names[multifcst.stats.names %in% multifcst.stats.names.present]
    nTimeSteps = length(rl[[1]]$ens.mean.gc.dist)
    multifcst.stats.mats = list()

    for (irl in 1:length(rl)) {
      if (length(rl[[irl]]$ens.mean.gc.dist) != nTimeSteps) {
        warning(paste("Multi-forecast statistics not possible, forecasts have inconsistent numbers of time steps.",
                      "Consider using sidfex.remaptime.fcst() first."))
        break
      }
      for (err.name in multifcst.stats.names) {
        add.vec = rl[[irl]][[err.name]]
        if (is.null(add.vec)) {
          multifcst.stats.mats[[err.name]] = cbind(multifcst.stats.mats[[err.name]], rep(NA,nTimeSteps))
        } else {
          multifcst.stats.mats[[err.name]] = cbind(multifcst.stats.mats[[err.name]], add.vec)
        }
      }
    }

    multifcst.stats = list()
    for (err.name in multifcst.stats.names) {
      multifcst.stats[[err.name]] = data.frame(
        mean = apply(multifcst.stats.mats[[err.name]], 1, function (x) {mean(x, na.rm=multifcst.stats.na.rm)}),
        st.dev = apply(multifcst.stats.mats[[err.name]], 1, function (x) {sd(x, na.rm=multifcst.stats.na.rm)}),
        st.err = apply(multifcst.stats.mats[[err.name]], 1, function (x) {sd(x, na.rm=multifcst.stats.na.rm)/sqrt(sum(!is.na(x)))}),
        median = apply(multifcst.stats.mats[[err.name]], 1, function (x) {median(x, na.rm=multifcst.stats.na.rm)})
      )
    }
  }

  return(list(ens.merge=eval.res$ens.merge,
              evaluate.arguments=list(do.speedangle=eval.res$evaluate.arguments$do.speedangle,do.multifcst.stats=do.multifcst.stats),
              res.list=rl,
              multifcst.stats=multifcst.stats))

}
