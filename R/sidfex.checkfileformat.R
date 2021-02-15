sidfex.checkfileformat = function (filepathnames) {

	if (length(filepathnames) == 1 && dir.exists(filepathnames)) {
		lastchar = substr(filepathnames,nchar(filepathnames),nchar(filepathnames))
		if (lastchar != "/") {filepathnames = paste0(filepathnames,"/")}
		filepathnames = paste0(filepathnames,system(paste0("ls ",filepathnames),intern=TRUE))
	}

	N = length(filepathnames)
	res.list = list()

	for (i in 1:N) {

		filepathname = filepathnames[[i]]
		if (!file.exists(filepathname)) {
			res = "File does not exist."
			res.list[[filepathname]] = res
			next
		}

		res = NULL

		### check file name

		filenameX = strsplit(filepathname,split="/",fixed=TRUE)[[1]]
		Nstr = length(filenameX)
		filenameX = filenameX[Nstr]

		filename = strsplit(filenameX,split=".",fixed=TRUE)[[1]]
		Nstr = length(filename)
		suffix = filename[Nstr]
		if (Nstr == 1 || suffix != "txt") {
			res = c(res,paste0("File name must have suffix '.txt'. Not checking further."))
			res.list[[filepathname]] = res
			next
		}
		if (Nstr > 1) {
			filename = paste(filename[1:(Nstr-1)],collapse=".")
		}

		filename.flds = strsplit(filename,split="_",fixed=TRUE)[[1]]
		Nstr = length(filename.flds)
		if (Nstr != 5) {
			res = c(res,paste0("File name should contain exactly four underscores but contains ",Nstr-1,". Not checking further."))
			res.list[[filepathname]] = res
			next
		}
		GroupID = filename.flds[1]
		MethodID = filename.flds[2]
		TargetID = filename.flds[3]
		InitTime = strsplit(filename.flds[4],split="-",fixed=TRUE)[[1]]
		Nstr = length(InitTime)
		if (Nstr != 2) {
			res = c(res,paste0("Initial time provided as '",InitTime,"' in file name must have format InitYear-InitDayOfYear (note the minus sign) where InitYear is an integer and InitDayOfYear is a float (or integer)."))
			InitYear = NA
			InitDayOfYear = NA
		} else {
			InitYear = InitTime[1]
			InitYear.int = as.integer(InitYear)
			if (is.na(InitYear.int) || InitYear.int != as.numeric(InitYear)) {
				res = c(res,paste0("InitYear provided as '",InitYear,"' in file name must be an integer."))
			}
			InitDayOfYear = InitTime[2]
			if (is.na(as.numeric(InitDayOfYear))) {
				res = c(res,paste0("InitDayOfYear provided as '",InitDayOfYear,"' in file name must be numeric."))
			}
		}
		EnsMemNum = filename.flds[5]
		EnsMemNum.int = as.integer(EnsMemNum)
		if (is.na(EnsMemNum.int) || EnsMemNum.int != as.numeric(EnsMemNum) || nchar(EnsMemNum) != 3) {
			res = c(res,paste0("EnsMemNum provided as '",EnsMemNum,"' in file name must be a 3-digit integer (with leading zeros if needed)."))
		}

		### check file header

		filecont = scan(filepathname,sep="\n",what="character",quiet=TRUE)
		Nr = length(filecont)
		if (Nr < 1) {
			res = c(res,paste0("File empty."))
			res.list[[filepathname]] = res
			next
		}

		nrGroupID = 0
		GroupIDfound = FALSE
		while (nrGroupID < Nr && !GroupIDfound) {
			nrGroupID = nrGroupID + 1
			rowcont = filecont[nrGroupID]
			if (substr(rowcont,1,7) == "GroupID") {
				GroupIDfound = TRUE
				break
			}
		}

		if (!GroupIDfound) {
			res = c(res,"No row starting with 'GroupID' found in file.  Not checking further.")
			res.list[[filepathname]] = res
			next
		}

		if (nrGroupID != 1) {

		  if (nrGroupID != 6) {
		    res = c(res,"'GroupID' is neither in row 1 (incoming format) nor in row 6 (processed format).  Not checking further.")
		    res.list[[filepathname]] = res
		    next
		  }

		  ### check auto file header (processed format)

		  row.strs = c("SubmitYear","SubmitDayOfYear","ProcessedYear","ProcessedDayOfYear")
		  for (nr in 1:length(row.strs)) {
		    rowcont = filecont[nr]
		    row.str = row.strs[nr]
		    row.flds = strsplit(rowcont,split=" ",fixed=TRUE)[[1]]
		    row.flds = row.flds[row.flds != ""]
		    if (row.flds[1] != paste0(row.str,":")) {
		      res = c(res,paste0("Row number ",nr," of processed file must start with '",row.str,":' (followed by one or more space characters) instead of '",row.flds[1],"'."))
		    }
		    if (length(row.flds) != 2) {
		      res = c(res,paste0("Row number ",nr," of processed file, provided as '",filecont[nr],"', must contain exactly two strings, separated by one or more space characters."))
		      next
		    }
		    if (is.na(as.numeric(row.flds[2]))) {
		      res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') is not numeric."))
		    }
		  }

		  if(filecont[5] != "### end of auto header") {
		    res = c(res,"Row number 5 must be '### end of auto header' for processed files. Not checking further.")
		    res.list[[filepathname]] = res
		    next
		  }

		}

		if ((Nr - nrGroupID) < 10) {
			res = c(res,paste0("File must have at least 10 rows after 'GroupID' but has only ",Nr-nrGroupID,". Not checking further."))
			res.list[[filepathname]] = res
			next
		}

		row.strs = c("GroupID","MethodID","TargetID","InitYear","InitDayOfYear","InitLat","InitLon","EnsMemNum")
		for (nrx in 1:length(row.strs)) {
			nr = nrGroupID + nrx - 1
			rowcont = filecont[nr]
			row.str = row.strs[nrx]
			row.flds = strsplit(rowcont,split=" ",fixed=TRUE)[[1]]
			row.flds = row.flds[row.flds != ""]
			if (row.flds[1] != paste0(row.str,":")) {
				res = c(res,paste0("Row number ",nrx," (counting GroupID row as first row) must start with '",row.str,":' (followed by one or more space characters) instead of '",row.flds[1],"'."))
			}
			if (length(row.flds) != 2) {
				res = c(res,paste0("Row number ",nrx," (counting GroupID row as first row), provided as '",filecont[nr],"', must contain exactly two strings, separated by one or more space characters."))
				next
			}
			if (row.str == "GroupID") {
				if (row.flds[2] != GroupID) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') does not match the ",row.str," provided in the file name ('",GroupID,"')."))
				}
			}
			if (row.str == "MethodID") {
				if (row.flds[2] != MethodID) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') does not match the ",row.str," provided in the file name ('",MethodID,"')."))
				}
			}
			if (row.str == "TargetID") {
				if (row.flds[2] != TargetID) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') does not match the ",row.str," provided in the file name ('",TargetID,"')."))
				}
			}
			if (row.str == "InitYear") {
				if (row.flds[2] != InitYear) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') does not match the ",row.str," provided in the file name ('",InitYear,"')."))
				}
			}
			if (row.str == "InitDayOfYear") {
				if (row.flds[2] != InitDayOfYear) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') does not match the ",row.str," provided in the file name ('",InitDayOfYear,"')."))
				}
			}
			if (row.str == "InitLat") {
				if (is.na(as.numeric(row.flds[2]))) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') is not numeric."))
				}
			}
			if (row.str == "InitLon") {
				if (is.na(as.numeric(row.flds[2]))) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') is not numeric."))
				}
			}
			if (row.str == "EnsMemNum") {
				if (row.flds[2] != EnsMemNum) {
					res = c(res,paste0(row.str," specified within the file ('",row.flds[2],"') does not match the ",row.str," provided in the file name ('",EnsMemNum,"')."))
				}
			}
		}

		nr = nr + 1
		if(filecont[nr] != "### end of header") {
			res = c(res,"Row number 9 (counting GroupID row as first row) must be '### end of header'. Not checking further.")
			res.list[[filepathname]] = res
			next
		}

		### check file data table

		tab.names = unlist(strsplit(unlist(strsplit(filecont[nr+1],split="\t",fixed=TRUE)),split=" ",fixed=TRUE))
		tab.names = tab.names[tab.names != ""]
		if (length(tab.names) != 4) {
			res = c(res,paste0("Forecast table must have four column names (space or tab delimited), but has ",length(tab.names),". Not checking further."))
			res.list[[filepathname]] = res
			next
		}
		tab.names.x = c("Year","DayOfYear","Lat","Lon")
		if (any(tab.names != tab.names.x)) {
			res = c(res,paste0("Forecast table column names must be 'Year DayOfYear Lat Lon'."))
		}
		Year.nonint = FALSE
		DayOfYear.nonnum = FALSE
		Lat.nonnum = FALSE
		Lon.nonnum = FALSE
		for (k in (nr+2):Nr) {
			row.flds = unlist(strsplit(unlist(strsplit(filecont[k],split="\t",fixed=TRUE)),split=" ",fixed=TRUE))
			row.flds = row.flds[row.flds!=""]
			if (length(row.flds) != 4) {
				res = c(res,paste0("At least one of the forecast table rows (total file row number ",k,") does not have four column entries (space or tab delimited). Not checking further."))
				res.list[[filepathname]] = res
				break
			}
			Year = row.flds[1]
			Year.int = as.integer(Year)
			if (is.na(Year.int) || Year.int != as.numeric(Year)) {
				Year.nonint = TRUE
			}
			DayOfYear = row.flds[2]
			if (is.na(as.numeric(DayOfYear))) {
				DayOfYear.nonnum = TRUE
			}
			Lat = row.flds[3]
			if (Lat != "NaN" && Lat != "nan" && is.na(as.numeric(Lat))) {
				Lat.nonnum = TRUE
			}
			Lon = row.flds[4]
			if (Lon != "NaN" && Lon != "nan" && is.na(as.numeric(Lon))) {
				Lon.nonnum = TRUE
			}
		}
		if (Year.nonint) {
			res = c(res,paste0("Year column contains at least one non-integer value."))
		}
		if (DayOfYear.nonnum) {
			res = c(res,paste0("DayOfYear column contains at least one non-numeric value."))
		}
		if (Lat.nonnum) {
			res = c(res,paste0("Lat column contains at least one non-numeric value that is not 'NaN'."))
		}
		if (Lon.nonnum) {
			res = c(res,paste0("Lon column contains at least one non-numeric value that is not 'NaN'."))
		}

		###

		if (length(res) == 0) {
			res.list[[filepathname]] = "No file format violations found."
		} else {
			res.list[[filepathname]] = res
		}

	}

	if (length(res.list) == 1) {
		return(res.list[[1]])
	} else {
		return(res.list)
	}

}
