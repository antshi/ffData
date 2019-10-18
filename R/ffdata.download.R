#' @title The ffdata.download function
#'
#' @description This function performs the automatic download of the user-specified portfolio returns from the Kenneth R. French Data Library.
#' The downloaded data frame of returns is saved then as a .csv-file in the chosen directory.
#'
#' @param dir character, the directory for saving the data. If not specified, current working directory (default).
#' @param type character, the portfolio type, "USResearch" (default) for the Fama-French 3 or 5 Factors,
#' "Industry" for the Industry portfolios, "Bivariate" for the Bivariate sorts on Size (ME), Book-to-Market (BE), Operational Profitability (OP) and Investment (INV),
#' "Threeway" for the Three-way sorts on Size (ME), Book-to-Market (BE), Operational Profitability (OP) and Investment (INV).
#' @param subtype character, the portfolio subtype. Possible values for the Bivariate type: "ME_BE", "ME_OP", "ME_INV", "BEME_OP", "BEME_INV", "OP_INV", possible
#' values for the Threeway type: "ME_BEME_OP", "ME_BEME_INV", "ME_OP_INV".
#' @param factors.n integer, the number of factors for the portfolios. Possible values for the USResearch type: 3, 5. Possible values for the Industry type: 5, 10, 12, 17, 30, 38, 48, 49.
#' Possible values for the Bivariate type: 6, 25, 100.
#' @param freq character, the frequency of the returns, "m" for monthly (default) or "d" for daily.
#' @param dividends logical, with dividends (default) if TRUE and without dividends if FALSE.
#' @param start character, start date for the download in the format YYmm (default).
#' @param end character, end date for the download in the format YYmm. The default value is calculated as two months before Sys.Date() to reassure availability.
#' @param na.clean logical, if TRUE, NAs are replaced with zoo::na.locf() (default). If FALSE, NAs are not cleaned.
#' @return .csv-file within the defined directory.
#' @examples ffdata.download()
#' ffdata.download(freq="m", type="Bivariate", subtype="ME_BE", factors.n=25)
#' @export ffdata.download

ffdata.download <- function(dir=NULL, type="USResearch", subtype=NULL, factors.n=NULL, freq = "m", dividends=TRUE, start=NULL, end=NULL, na.clean=TRUE){

    base = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

    if(is.null(start)){
        start <- "197501"
        }
    if(is.null(end)){
        end <- format(zoo::as.yearmon(Sys.Date())-.2, "%Y%m") # 2 months back, to ensure the data availability
        }
    if(is.null(dir)){
        dir <- getwd()
        }

    try(if(freq=="d" & dividends==TRUE) stop("Please, set dividends TRUE only for a monthly frequency.") )
    try(if(freq=="d" & type=="Threeway") stop("Please, set the Threeway portfolio type only for a monthly frequency.") )

    sub.path <- paste(type, freq, start, end, sep="_")
    dir.create(sub.path, showWarnings=FALSE)

    if(type=="USResearch"){
        if(is.null(factors.n)){
            factors.n <- 3
        }
    }else if(type=="Industry"){
        if(is.null(factors.n)){
            factors.n <- 5
        }
    }else if(type=="Bivariate"){
        if(is.null(subtype)){
            subtype <- "ME_BE"
        }
        if(is.null(factors.n)){
            factors.n <- 6
        }
    }else if(type=="Threeway"){
        if(is.null(subtype)){
            subtype <- "ME_BEME_OP"
        }
    }

    factors.usr.all <- c(3,5)
    factors.ind.all <- c(5, 10, 12, 17, 30, 38, 48, 49)
    factors.bi.all <- c(6, 25, 100)
    subtypes.bivar.all <- c("ME_BE", "ME_OP", "ME_INV", "BEME_OP", "BEME_INV", "OP_INV")
    subtypes.threeway.all <- c("ME_BEME_OP", "ME_BEME_INV", "ME_OP_INV")

    if(type=="USResearch"){
        if(is.na(match(factors.n, factors.usr.all))){
            stop("Please, set the argument factors.n to one of the following for the USResearch Dataset: 3, 5.")
        }else{
            usresearch.download(base, dir, sub.path, factors.n, freq, start, end, na.clean)
        }
    }else if(type=="Industry"){
        if(is.na(match(factors.n, factors.ind.all))){
            stop("Please, set the argument factors.n to one of the following for the Industry Dataset: 5, 10, 12, 17, 30, 38, 48, 49.")
        }else{
            industry.download(base, dir, sub.path, factors.n, freq, dividends, start, end, na.clean)
        }
    }else if(type=="Bivariate"){
        if(is.na(match(subtype, subtypes.bivar.all))){
            stop("Please, set the argument subtype to one of the following for the Bivariate Dataset: ME_BE, ME_OP, ME_INV, BEME_OP, BEME_INV, OP_INV.")
        }else if(is.na(match(factors.n, factors.bi.all))){
            stop("Please, set the argument factors.n to one of the following for the Bivariate Dataset: 6,25,100.")
        }else{
            if((subtype == "BEME_OP" | subtype == "BEME_INV" | subtype=="OP_INV") & factors.n!=25){
                stop("Please, set the argument factors.n to 25 for the Bivariate BEME_OP, BEME_INV or OP_INV datasets.")
                }
            bivariate.download(base, dir, sub.path, factors.n, subtype, freq, dividends, start, end, na.clean)
            }
    }else if(type=="Threeway"){
        if(is.na(match(subtype, subtypes.threeway.all))){
            stop("Please, set the argument subtype to one of the following for the Threeway Dataset: ME_BEME_OP, ME_BEME_INV, ME_OP_INV.")
        }else{
            threeway.download(base, dir, sub.path, subtype, dividends, start, end, na.clean)
        }
    }

    setwd(dir)
}


#' @title The usresearch.download function
#'
#' @description This function performs the automatic download of the US-Research portfolio returns from the Kenneth R. French Data Library.
#' The downloaded data frame of returns is saved then as a .csv-file in the chosen directory.
#'
#' @param base character string with the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir character, the directory for saving the data. If not specified, current working directory (default).
#' @param sub.path character string with the folder subpath, created by ffdata.download.
#' @param freq character, the frequency of the returns, "m" for monthly (default) or "d" for daily.
#' @param factors.n integer, the number of factors for the portfolios. Possible values for the USResearch type: 3, 5.
#' @param start character, start date for the download in the format YYmm (default).
#' @param end character, end date for the download in the format YYmm. The default value is calculated as two months before Sys.Date() to reassure availability.
#' @param na.clean logical, if TRUE, NAs are replaced with zoo::na.locf() (default). If FALSE, NAs are not cleaned.
#' @return .csv-file within the defined directory.
#' @export usresearch.download

usresearch.download <- function(base, dir, sub.path, factors.n, freq, start, end, na.clean){

    setwd(sub.path)
    format  <-  "_CSV.zip"

    if(factors.n==3){
        factors  <-  "F-F_Research_Data_Factors"

        if(freq=="d"){factors <- paste(factors, "_daily", sep="")}else{factors <- factors}
        full_url  <-  paste(base, factors, format, sep="")

        ##download
        temp  <-  tempfile()
        download.file(full_url,temp,quiet = TRUE)

        ##open
        data <- unzip(temp)
        text <- readLines(data)
        skip.index <- which(substr(text,1,2)=="19")[1]-2
        data.rets <- read.csv(data, skip=skip.index,  header=TRUE, stringsAsFactors=FALSE)
        colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
        index.begin <- which(substr(data.rets[,1],1,6)==start)[1]
        index.end <- which(substr(data.rets[,1],1,6)==end)[1]
        if(is.na(index.end)){
        final <- format(max(as.Date(paste(substr(data.rets[,1],1,6), 1), "%Y%m%d"), na.rm=TRUE), "%Y%m")
        print(paste("The end point ", end, " is still not present in the data. The time series' end is set to automatically to the latest reported date ", final, ".", sep=""))
        index.end <- which(substr(data.rets[,1],1,6)==final)[1]
        }
        data.rets <- data.rets[index.begin:index.end,]
        data.rets  <-  data.rets[order(data.rets[,1]),]
        dates  <-  data.rets[,1]
        rets  <-  data.rets[,-1]
        rets  <-  apply(rets,2,as.numeric)
        if(na.clean){
            index.na <- apply(rets, 2, function(x) which(ceiling(x)==-99 | ceiling(x)==-999))
            if(length(index.na)!=0){
                for(m in 1:ncol(rets)){
                    if(length(index.na[[m]])!=0){
                        rets[index.na[[m]],m] <- NA
                        rets[,m] <- na.locf(rets[,m], na.rm=FALSE)
                        }
                    }
                }
        }
        rets  <-  rets/100
        data.rets  <-  data.frame(dates,rets)
        colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
        files <- list.files()
        delindex <- substr(files,nchar(files)-(3-1),nchar(files))
        index.del <- which(delindex=="CSV")
        file.remove(files[index.del])
        write.csv(data.rets, paste0(factors, ".csv", sep="") , row.names=FALSE)

        }else{
            factors <- "F-F_Research_Data_5_Factors_2x3"
            if(freq=="d"){factors <- paste(factors, "_daily", sep="")}else{factors <- factors}
            full_url  <-  paste(base, factors, format, sep="")

            ##download
            temp  <-  tempfile()
            download.file(full_url,temp,quiet = TRUE)

            ##open
            data <- unzip(temp)
            text <- readLines(data)
            skip.index <- which(substr(text,1,2)=="19")[1]-2
            data.rets <- read.csv(data, skip=skip.index,  header=TRUE, stringsAsFactors=FALSE)
            colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
            index.begin <- which(substr(data.rets[,1],1,6)==start)[1]
            index.end <- which(substr(data.rets[,1],1,6)==end)[1]
            if(is.na(index.end)){
                final <- format(max(as.Date(paste(substr(data.rets[,1],1,6), 1), "%Y%m%d"), na.rm=TRUE), "%Y%m")
                print(paste("The end point ", end, " is still not present in the data. The time series' end is set to automatically to the latest reported date ", final, ".", sep=""))
                index.end <- which(substr(data.rets[,1],1,6)==final)[1]
            }
            data.rets <- data.rets[index.begin:index.end,]
            data.rets  <-  data.rets[order(data.rets[,1]),]
            dates  <-  data.rets[,1]
            rets  <-  data.rets[,-1]
            rets  <-  apply(rets,2,as.numeric)
            if(na.clean){
                index.na <- apply(rets, 2, function(x) which(ceiling(x)==-99 | ceiling(x)==-999))
                if(length(index.na)!=0){
                    for(m in 1:ncol(rets)){
                        if(length(index.na[[m]])!=0){
                            rets[index.na[[m]],m] <- NA
                            rets[,m] <- zoo::na.locf(rets[,m], na.rm=FALSE)
                            }
                        }
                    }
            }
            rets  <-  rets/100
            data.rets  <-  data.frame(dates,rets)
            colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
            files <- list.files()
            delindex <- substr(files,nchar(files)-(3-1),nchar(files))
            index.del <- which(delindex=="CSV")
            file.remove(files[index.del])
            write.csv(data.rets, paste0(factors, ".csv", sep="") , row.names=FALSE)
        }

    setwd(dir)

}

#' @title The industry.download function
#'
#' @description This function performs the automatic download of the Industry portfolio returns from the Kenneth R. French Data Library.
#' The downloaded data frame of returns is saved then as a .csv-file in the chosen directory.
#'
#' @param base character string with the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir character, the directory for saving the data. If not specified, current working directory (default).
#' @param sub.path character string with the folder subpath, created by ffdata.download.
#' @param factors.n integer, the number of factors for the portfolios. Possible values for the Industry type: 5, 10, 12, 17, 30, 38, 48, 49.
#' @param freq character, the frequency of the returns, "m" for monthly (default) or "d" for daily.
#' @param dividends logical, with dividends (default) if TRUE and without dividends if FALSE.
#' @param start character, start date for the download in the format YYmm (default).
#' @param end character, end date for the download in the format YYmm. The default value is calculated as two months before Sys.Date() to reassure availability.
#' @param na.clean logical, if TRUE, NAs are replaced with zoo::na.locf() (default). If FALSE, NAs are not cleaned.
#' @return .csv-file within the defined directory.
#' @export industry.download

industry.download <- function(base, dir, sub.path, factors.n, freq, dividends, start, end, na.clean){

    setwd(sub.path)
    format <-  "_CSV.zip"

    ##url
    factors  <-  paste(factors.n, "Industry_Portfolios", sep="_")

    if(freq=="d"){
        factors <- paste(factors, "_daily", sep="")
        }else{
            if(dividends){
                factors <- factors
            }else{
                factors <- paste(factors, "_Wout_Div", sep="")
                }
        }

    ##url
    full_url  <-  paste(base, factors, format, sep="")

    ##download (main)
    temp  <-  tempfile()
    download.file(full_url,temp,quiet = TRUE)

    ##open
    data <- unzip(temp)
    text <- readLines(data)
    skip.index <- which(substr(text,1,2)=="19")[1]-2
    data.rets <- read.csv(data, skip=skip.index,  header=TRUE, stringsAsFactors=FALSE)
    colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
    index.begin <- which(substr(data.rets[,1],1,6)==start)[1]
    index.end <- which(substr(data.rets[,1],1,6)==end)[1]
    if(is.na(index.end)){
        final <- format(max(as.Date(paste(substr(data.rets[,1],1,6), 1), "%Y%m%d"), na.rm=TRUE), "%Y%m")
        print(paste("The end point ", end, " is still not present in the data. The time series' end is set to automatically to the latest reported date ", final, ".", sep=""))
        index.end <- which(substr(data.rets[,1],1,6)==final)[1]
    }
    data.rets <- data.rets[index.begin:index.end,]
    data.rets  <-  data.rets[order(data.rets[,1]),]
    dates  <-  data.rets[,1]
    rets  <-  data.rets[,-1]
    rets  <-  apply(rets,2,as.numeric)
    if(na.clean){
        index.na <- apply(rets, 2, function(x) which(ceiling(x)==-99 | ceiling(x)==-999))
        if(length(index.na)!=0){
            for(m in 1:ncol(rets)){
                if(length(index.na[[m]])!=0){
                    rets[index.na[[m]],m] <- NA
                    rets[,m] <- zoo::na.locf(rets[,m], na.rm=FALSE)
                    }
                }
            }
    }
    rets  <-  rets/100
    data.rets  <-  data.frame(dates,rets)
    colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
    files <- list.files()
    delindex <- substr(files,nchar(files)-(3-1),nchar(files))
    index.del <- which(delindex=="CSV")
    file.remove(files[index.del])
    write.csv(data.rets, paste0(factors, ".csv", sep="") , row.names=FALSE)

    setwd(dir)
}


#' @title The bivariate.download function
#'
#' @description This function performs the automatic download of the Bivariate portfolio returns from the Kenneth R. French Data Library.
#' The downloaded data frame of returns is saved then as a .csv-file in the chosen directory.
#'
#' @param base character string with the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir character, the directory for saving the data. If not specified, current working directory (default).
#' @param sub.path character string with the folder subpath, created by ffdata.download.
#' @param factors.n integer, the number of factors for the portfolios. Possible values for the Bivariate type: 6, 25, 100.
#' @param subtype character, the portfolio subtype. Possible values for the Bivariate type: "ME_BE", "ME_OP", "ME_INV", "BEME_OP", "BEME_INV", "OP_INV".
#' @param freq character, the frequency of the returns, "m" for monthly (default) or "d" for daily.
#' @param dividends logical, with dividends (default) if TRUE and without dividends if FALSE.
#' @param start character, start date for the download in the format YYmm (default).
#' @param end character, end date for the download in the format YYmm. The default value is calculated as two months before Sys.Date() to reassure availability.
#' @param na.clean logical, if TRUE, NAs are replaced with zoo::na.locf() (default). If FALSE, NAs are not cleaned.
#' @return .csv-file within the defined directory.
#' @export bivariate.download

bivariate.download <- function(base, dir, sub.path, factors.n, subtype, freq, dividends, start, end, na.clean){

    setwd(sub.path)
    format <- "_CSV.zip"

    if(subtype=="ME_BE"){subtype <- ""} #according to FamaFrench webseite

        if(subtype == ""){
            if(factors.n==6){
                factors <- paste(factors.n, "Portfolios", "2x3", sep="_")
            }else if(factors.n==25){
                factors <- paste(factors.n, "Portfolios", "5x5", sep="_")
            }else{
                factors <- paste(factors.n, "Portfolios", "10x10", sep="_")
            }
        }else{
        if(factors.n==6){
            factors <- paste(factors.n, "Portfolios", subtype, "2x3", sep="_")
            }else if(factors.n==25){
                factors <- paste(factors.n, "Portfolios", subtype, "5x5", sep="_")
                } else {
                    factors <- paste(factors.n, "Portfolios", subtype, "10x10", sep="_")
                    }
        }
        if(freq=="d"){
            factors<-paste(factors, "_daily", sep="")
        }else{
            factors<-factors
            if(dividends){
                factors <- factors
            }else{
                if((subtype=="ME_OP" | subtype=="ME_INV") & factors.n==100){
                    factors<-paste(factors.n, "Portfolios", "10x10", subtype, "Wout_Div", sep="_")
                }else{
                factors<-paste(factors, "_Wout_Div", sep="")
                }
            }
        }

    ##url(s)
    full_url <- paste(base, factors, format, sep="")

    ##download (main)
    temp <- tempfile()
    download.file(full_url,temp,quiet = TRUE)

    ##open
    data<-unzip(temp)
    text<-readLines(data)
    skip.index<-which(substr(text,1,2)=="19")[1]-2
    data.rets<-read.csv(data, skip=skip.index,  header=TRUE, stringsAsFactors=FALSE)
    colnames(data.rets)<-c("Date", colnames(data.rets)[-1])
    index.begin<-which(substr(data.rets[,1],1,6)==start)[1]
    index.end<-which(substr(data.rets[,1],1,6)==end)[1]
    if(is.na(index.end)){
        final <- format(max(as.Date(paste(substr(data.rets[,1],1,6), 1), "%Y%m%d"), na.rm=TRUE), "%Y%m")
        print(paste("The end point ", end, " is still not present in the data. The time series' end is set to automatically to the latest reported date ", final, ".", sep=""))
        index.end <- which(substr(data.rets[,1],1,6)==final)[1]
    }
    data.rets<-data.rets[index.begin:index.end,]
    data.rets <- data.rets[order(data.rets[,1]),]
    dates <- data.rets[,1]
    rets <- data.rets[,-1]
    rets <- apply(rets,2,as.numeric)
    if(na.clean){
        index.na <- apply(rets, 2, function(x) which(ceiling(x)==-99 | ceiling(x)==-999))
        if(length(index.na)!=0){
        for(m in 1:ncol(rets)){
            if(length(index.na[[m]])!=0){
                rets[index.na[[m]],m]<- NA
                rets[,m]<-zoo::na.locf(rets[,m], na.rm=FALSE)
                }
            }
        }
    }

    rets <- rets/100
    data.rets <- data.frame(dates,rets)
    colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
    files<-list.files()
    delindex <- substr(files,nchar(files)-(3-1),nchar(files))
    index.del <- which(delindex=="CSV")
    file.remove(files[index.del])
    write.csv(data.rets, paste0(factors, ".csv", sep="") , row.names=FALSE)

    setwd(dir)
}

#' @title The threeway.download function
#'
#' @description This function performs the automatic download of the Threeway portfolio returns from the Kenneth R. French Data Library.
#' The downloaded data frame of returns is saved then as a .csv-file in the chosen directory.
#'
#' @param base character string with the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir character, the directory for saving the data. If not specified, current working directory (default).
#' @param sub.path character string with the folder subpath, created by ffdata.download.
#' @param subtype character, the portfolio subtype. Possible values for the Threeway type: "ME_BEME_OP", "ME_BEME_INV", "ME_OP_INV".
#' @param dividends logical, with dividends (default) if TRUE and without dividends if FALSE.
#' @param start character, start date for the download in the format YYmm (default).
#' @param end character, end date for the download in the format YYmm. The default value is calculated as two months before Sys.Date() to reassure availability.
#' @param na.clean logical, if TRUE, NAs are replaced with zoo::na.locf() (default). If FALSE, NAs are not cleaned.
#' @return .csv-file within the defined directory.
#' @export threeway.download

threeway.download <- function(base, dir, sub.path,  subtype, dividends, start, end, na.clean){

    setwd(sub.path)
    format<- "_CSV.zip"
    factors <- paste(32, "Portfolios", subtype, "2x4x4", sep="_")
    if(dividends){
        factors <- factors
        }else{
            factors <- paste(factors, "_Wout_Div", sep="")
        }

    full_url <- paste(base, factors, format, sep="")

    ##download (main)
    temp <- tempfile()
    download.file(full_url,temp,quiet = TRUE)

    ##open
    data<-unzip(temp)
    text<-readLines(data)
    skip.index<-which(substr(text,1,2)=="19")[1]-2
    data.rets<-read.csv(data, skip=skip.index,  header=TRUE, stringsAsFactors=FALSE)
    colnames(data.rets)<-c("Date", colnames(data.rets)[-1])
    index.begin<-which(substr(data.rets[,1],1,6)==start)[1]
    index.end<-which(substr(data.rets[,1],1,6)==end)[1]
    if(is.na(index.end)){
        final <- format(max(as.Date(paste(substr(data.rets[,1],1,6), 1), "%Y%m%d"), na.rm=TRUE), "%Y%m")
        print(paste("The end point ", end, " is still not present in the data. The time series' end is set to automatically to the latest reported date ", final, ".", sep=""))
        index.end <- which(substr(data.rets[,1],1,6)==final)[1]
    }
    data.rets<-data.rets[index.begin:index.end,]
    data.rets <- data.rets[order(data.rets[,1]),]
    dates <- data.rets[,1]
    rets <- data.rets[,-1]
    rets <- apply(rets,2,as.numeric)
    if(na.clean){
        index.na<-apply(rets, 2, function(x) which(ceiling(x)==-99 | ceiling(x)==-999))
        if(length(index.na)!=0){
        for(m in 1:ncol(rets)){
            if(length(index.na[[m]])!=0){
                rets[index.na[[m]],m]<-NA
                rets[,m]<-zoo::na.locf(rets[,m], na.rm=FALSE)
                }
            }
        }
    }
    rets <- rets/100
    data.rets <- data.frame(dates,rets)
    colnames(data.rets) <- c("Date", colnames(data.rets)[-1])
    files<-list.files()
    delindex <- substr(files,nchar(files)-(3-1),nchar(files))
    index.del <- which(delindex=="CSV")
    file.remove(files[index.del])
    write.csv(data.rets, paste0(factors, ".csv", sep="") , row.names=FALSE)

    setwd(dir)
}




