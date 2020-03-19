#' @title The ffDataDownload function
#'
#' @description This function automatically downloads user-specified portfolio returns from the Kenneth R. French Data Library.
#' The downloaded returns are saved as a .csv-file in the chosen directory.
#'
#' @param dir a character string, the directory for saving the data. The current working directory is set by default.
#' @param type a character string, the portfolio type. Possible values are "USResearch" (default) for the Fama-French 3 or 5 Factors,
#' "Industry" for the Industry portfolios, "Bivariate" for the Bivariate sorts on Size (ME), Book-to-Market (BE), Operational Profitability (OP) and Investment (INV),
#' "Threeway" for the Three-way sorts on Size (ME), Book-to-Market (BE), Operational Profitability (OP) and Investment (INV).
#' @param subtype a character string, the portfolio subtype. Possible values are "ME_BE", "ME_OP", "ME_INV", "BEME_OP", "BEME_INV", "OP_INV" (for the Bivariate type) and
#' "ME_BEME_OP", "ME_BEME_INV", "ME_OP_INV" (for the Threeway type).
#' @param number_factors an integer, the number of factors for the portfolios. Possible values are 3 (default) or 5 (for the USResearch portfolios); 5 (default), 10, 12, 17, 30, 38, 48 or 49 (for the Industry portfolios) and
#' 6 (default), 25 or 100 (for the Bivariate portfolios).
#' @param freq a character string, the frequency of returns. Possible values are "m" for monthly (default) or "d" for daily.
#' @param dividends a logical, TRUE (default) considers dividends. FALSE downloads the data without dividends.
#' @param start a character string, start date for the download in the format "YYmm". Default value is 197501.
#' @param end a character string, end date for the download in the format "YYmm". Default value is two months before Sys.Date() to insure availability.
#' @param cleanNAs a logical, TRUE (default) replaces NAs with zoo::na.locf(). If FALSE, NAs are not cleaned.
#' @return a .csv-file within the directory, defined in dir.
#' @examples
#' library(ffData)
#' ffDataDownload()
#' ffDataDownload(freq="m", type="Bivariate", subtype="ME_BE", number_factors=25)
#' @export ffDataDownload

ffDataDownload <-
    function(dir = NULL,
             type = "USResearch",
             subtype = NULL,
             number_factors = NULL,
             freq = "m",
             dividends = TRUE,
             start = NULL,
             end = NULL,
             cleanNAs = TRUE) {
        base = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

        if (is.null(start)) {
            start <- "197501"
        }
        if (is.null(end)) {
            end <- format(zoo::as.yearmon(Sys.Date()) - .2, "%Y%m")
        }
        if (is.null(dir)) {
            dir <- getwd()
        }

        try(if (freq == "d" &
                dividends == TRUE)
            stop("Please, set dividends TRUE only for a monthly frequency."))
        try(if (freq == "d" &
                type == "Threeway")
            stop("Please, set the Threeway portfolio type only for a monthly frequency."))

        sub_path <- paste(type, freq, start, end, sep = "_")
        dir.create(sub_path, showWarnings = FALSE)

        if (type == "USResearch") {
            if (is.null(number_factors)) {
                number_factors <- 3
            }
        } else if (type == "Industry") {
            if (is.null(number_factors)) {
                number_factors <- 5
            }
        } else if (type == "Bivariate") {
            if (is.null(subtype)) {
                subtype <- "ME_BE"
            }
            if (is.null(number_factors)) {
                number_factors <- 6
            }
        } else if (type == "Threeway") {
            if (is.null(subtype)) {
                subtype <- "ME_BEME_OP"
            }
        }

        factors_usr_all <- c(3, 5)
        factors_ind_all <- c(5, 10, 12, 17, 30, 38, 48, 49)
        factors_bi_all <- c(6, 25, 100)
        subtypes_bivar_all <-
            c("ME_BE", "ME_OP", "ME_INV", "BEME_OP", "BEME_INV", "OP_INV")
        subtypes_threeway_all <-
            c("ME_BEME_OP", "ME_BEME_INV", "ME_OP_INV")

        if (type == "USResearch") {
            if (is.na(match(number_factors, factors_usr_all))) {
                stop(
                    "Please, set the argument number_factors to one of the following for the USResearch Dataset: 3, 5."
                )
            } else{
                USresearch_download(base,
                                    dir,
                                    sub_path,
                                    number_factors,
                                    freq,
                                    start,
                                    end,
                                    cleanNAs)
            }
        } else if (type == "Industry") {
            if (is.na(match(number_factors, factors_ind_all))) {
                stop(
                    "Please, set the argument number_factors to one of the following for the Industry Dataset: 5, 10, 12, 17, 30, 38, 48, 49."
                )
            } else{
                industry_download(base,
                                  dir,
                                  sub_path,
                                  number_factors,
                                  freq,
                                  dividends,
                                  start,
                                  end,
                                  cleanNAs)
            }
        } else if (type == "Bivariate") {
            if (is.na(match(subtype, subtypes_bivar_all))) {
                stop(
                    "Please, set the argument subtype to one of the following for the Bivariate Dataset: ME_BE, ME_OP, ME_INV, BEME_OP, BEME_INV, OP_INV."
                )
            } else if (is.na(match(number_factors, factors_bi_all))) {
                stop(
                    "Please, set the argument number_factors to one of the following for the Bivariate Dataset: 6,25,100."
                )
            } else{
                if ((subtype == "BEME_OP" |
                     subtype == "BEME_INV" | subtype == "OP_INV") & number_factors != 25) {
                    stop(
                        "Please, set the argument number_factors to 25 for the Bivariate BEME_OP, BEME_INV or OP_INV datasets."
                    )
                }
                bivariate_download(
                    base,
                    dir,
                    sub_path,
                    number_factors,
                    subtype,
                    freq,
                    dividends,
                    start,
                    end,
                    cleanNAs
                )
            }
        } else if (type == "Threeway") {
            if (is.na(match(subtype, subtypes_threeway_all))) {
                stop(
                    "Please, set the argument subtype to one of the following for the Threeway Dataset: ME_BEME_OP, ME_BEME_INV, ME_OP_INV."
                )
            } else{
                threeway_download(base,
                                  dir,
                                  sub_path,
                                  subtype,
                                  dividends,
                                  start,
                                  end,
                                  cleanNAs)
            }
        }

        setwd(dir)
    }


#' @title The USresearch_download function
#'
#' @description This function automatically downloads US-Research portfolios returns from the Kenneth R. French Data Library.
#' The downloaded returns are saved then as a .csv-file in the chosen directory.
#'
#' @param base a character string, the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir a character string, the directory for saving the data. The current working directory is set by default.
#' @param sub_path a character string, the folder subpath, created by ffDataDownload.
#' @param number_factors an integer, the number of factors for the USResearch portfolios. Possible values are 3 (default) and 5.
#' @param freq a character string, the frequency of returns. Possible values are "m" for monthly (default) or "d" for daily.
#' @param start a character string, start date for the download in the format "YYmm". Default value is 197501.
#' @param end a character string, end date for the download in the format "YYmm". Default value is two months before Sys.Date() to insure availability.
#' @param cleanNAs a logical, TRUE (default) replaces NAs with zoo::na.locf(). If FALSE, NAs are not cleaned.
#' @return  a .csv-file within the directory, defined in dir.
#' @export USresearch_download

USresearch_download <-
    function(base,
             dir,
             sub_path,
             number_factors,
             freq,
             start,
             end,
             cleanNAs) {
        setwd(sub_path)
        format <- "_CSV.zip"

        if (number_factors == 3) {
            factors <- "F-F_Research_Data_Factors"

            if (freq == "d") {
                factors <- paste(factors, "_daily", sep = "")
            } else{
                factors <- factors
            }
            full_url  <-  paste(base, factors, format, sep = "")

            ##download
            temp  <-  tempfile()
            download.file(full_url, temp, quiet = TRUE)

            ##open
            data <- unzip(temp)
            text <- readLines(data)
            skip_indx <- which(substr(text, 1, 2) == "19")[1] - 2
            data_rets <-
                read.csv(
                    data,
                    skip = skip_indx,
                    header = TRUE,
                    stringsAsFactors = FALSE
                )
            colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
            begin_indx <- which(substr(data_rets[, 1], 1, 6) == start)[1]
            end_indx <- which(substr(data_rets[, 1], 1, 6) == end)[1]
            if (is.na(end_indx)) {
                final <-
                    format(max(as.Date(paste(
                        substr(data_rets[, 1], 1, 6), 1
                    ), "%Y%m%d"), na.rm = TRUE), "%Y%m")
                print(
                    paste(
                        "The end point ",
                        end,
                        " is still not present in the data. The time series' end is set to automatically to the latest reported date ",
                        final,
                        ".",
                        sep = ""
                    )
                )
                end_indx <- which(substr(data_rets[, 1], 1, 6) == final)[1]
            }
            data_rets <- data_rets[begin_indx:end_indx, ]
            data_rets  <-  data_rets[order(data_rets[, 1]), ]
            dates  <-  data_rets[, 1]
            rets  <-  data_rets[, -1]
            rets  <-  apply(rets, 2, as.numeric)
            if (cleanNAs) {
                indxNAs <-
                    apply(rets, 2, function(x)
                        which(ceiling(x) == -99 | ceiling(x) == -999))
                if (length(indxNAs) != 0) {
                    for (m in 1:ncol(rets)) {
                        if (length(indxNAs[[m]]) != 0) {
                            rets[indxNAs[[m]], m] <- NA
                            rets[, m] <- na.locf(rets[, m], na.rm = FALSE)
                        }
                    }
                }
            }
            rets  <-  rets / 100
            data_rets  <-  data.frame(dates, rets)
            colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
            files <- list.files()
            del_indx <- substr(files, nchar(files) - (3 - 1), nchar(files))
            indxDel <- which(del_indx == "CSV")
            file.remove(files[indxDel])
            write.csv(data_rets, paste0(factors, ".csv", sep = "") , row.names =
                          FALSE)

        } else{
            factors <- "F-F_Research_Data_5_Factors_2x3"
            if (freq == "d") {
                factors <- paste(factors, "_daily", sep = "")
            } else{
                factors <- factors
            }
            full_url  <-  paste(base, factors, format, sep = "")

            ##download
            temp  <-  tempfile()
            download.file(full_url, temp, quiet = TRUE)

            ##open
            data <- unzip(temp)
            text <- readLines(data)
            skip_indx <- which(substr(text, 1, 2) == "19")[1] - 2
            data_rets <-
                read.csv(
                    data,
                    skip = skip_indx,
                    header = TRUE,
                    stringsAsFactors = FALSE
                )
            colnames(data_rets) <-
                c("Date", colnames(data_rets)[-1])
            begin_indx <- which(substr(data_rets[, 1], 1, 6) == start)[1]
            end_indx <- which(substr(data_rets[, 1], 1, 6) == end)[1]
            if (is.na(end_indx)) {
                final <-
                    format(max(as.Date(paste(
                        substr(data_rets[, 1], 1, 6), 1
                    ), "%Y%m%d"), na.rm = TRUE), "%Y%m")
                print(
                    paste(
                        "The end point ",
                        end,
                        " is still not present in the data. The time series' end is set to automatically to the latest reported date ",
                        final,
                        ".",
                        sep = ""
                    )
                )
                end_indx <-
                    which(substr(data_rets[, 1], 1, 6) == final)[1]
            }
            data_rets <- data_rets[begin_indx:end_indx, ]
            data_rets  <-  data_rets[order(data_rets[, 1]), ]
            dates  <-  data_rets[, 1]
            rets  <-  data_rets[, -1]
            rets  <-  apply(rets, 2, as.numeric)
            if (cleanNAs) {
                indxNAs <-
                    apply(rets, 2, function(x)
                        which(ceiling(x) == -99 | ceiling(x) == -999))
                if (length(indxNAs) != 0) {
                    for (m in 1:ncol(rets)) {
                        if (length(indxNAs[[m]]) != 0) {
                            rets[indxNAs[[m]], m] <- NA
                            rets[, m] <-
                                na.locf(rets[, m], na.rm = FALSE)
                        }
                    }
                }
            }
            rets  <-  rets / 100
            data_rets  <-  data.frame(dates, rets)
            colnames(data_rets) <-
                c("Date", colnames(data_rets)[-1])
            files <- list.files()
            del_indx <-
                substr(files, nchar(files) - (3 - 1), nchar(files))
            indxDel <- which(del_indx == "CSV")
            file.remove(files[indxDel])
            write.csv(data_rets, paste0(factors, ".csv", sep = "") , row.names =
                          FALSE)
        }

        setwd(dir)

    }

#' @title The industry_download function
#'
#' @description  This function automatically downloads Industry portfolios returns from the Kenneth R. French Data Library.
#' The downloaded returns are saved then as a .csv-file in the chosen directory.
#'
#' @param base a character string, the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir a character string, the directory for saving the data. The current working directory is set by default.
#' @param sub_path a character string, the folder subpath, created by ffDataDownload.
#' @param number_factors an integer, the number of factors for the Industry portfolios. Possible values are 5 (default), 10, 12, 17, 30, 38, 48 or 49.
#' @param freq a character string, the frequency of the returns, "m" for monthly (default) or "d" for daily.
#' @param dividends a logical, TRUE (default) considers dividends. FALSE downloads the data without dividends.
#' @param start a character string, start date for the download in the format "YYmm". Default value is 197501.
#' @param end a character string, end date for the download in the format "YYmm". Default value is two months before Sys.Date() to insure availability.
#' @param cleanNAs a logical, TRUE (default) replaces NAs with zoo::na.locf(). If FALSE, NAs are not cleaned.
#' @return  a .csv-file within the directory, defined in dir.
#' @export industry_download

industry_download <-
    function(base,
             dir,
             sub_path,
             number_factors,
             freq,
             dividends,
             start,
             end,
             cleanNAs) {
        setwd(sub_path)
        format <-  "_CSV.zip"

        ##url
        factors  <-
            paste(number_factors, "Industry_Portfolios", sep = "_")

        if (freq == "d") {
            factors <- paste(factors, "_daily", sep = "")
        } else{
            if (dividends) {
                factors <- factors
            } else{
                factors <- paste(factors, "_Wout_Div", sep = "")
            }
        }

        ##url
        full_url  <-  paste(base, factors, format, sep = "")

        ##download (main)
        temp  <-  tempfile()
        download.file(full_url, temp, quiet = TRUE)

        ##open
        data <- unzip(temp)
        text <- readLines(data)
        skip_indx <- which(substr(text, 1, 2) == "19")[1] - 2
        data_rets <-
            read.csv(
                data,
                skip = skip_indx,
                header = TRUE,
                stringsAsFactors = FALSE
            )
        colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
        begin_indx <- which(substr(data_rets[, 1], 1, 6) == start)[1]
        end_indx <- which(substr(data_rets[, 1], 1, 6) == end)[1]
        if (is.na(end_indx)) {
            final <-
                format(max(as.Date(paste(
                    substr(data_rets[, 1], 1, 6), 1
                ), "%Y%m%d"), na.rm = TRUE), "%Y%m")
            print(
                paste(
                    "The end point ",
                    end,
                    " is still not present in the data. The time series' end is set to automatically to the latest reported date ",
                    final,
                    ".",
                    sep = ""
                )
            )
            end_indx <- which(substr(data_rets[, 1], 1, 6) == final)[1]
        }
        data_rets <- data_rets[begin_indx:end_indx, ]
        data_rets  <-  data_rets[order(data_rets[, 1]), ]
        dates  <-  data_rets[, 1]
        rets  <-  data_rets[, -1]
        rets  <-  apply(rets, 2, as.numeric)
        if (cleanNAs) {
            indxNAs <-
                apply(rets, 2, function(x)
                    which(ceiling(x) == -99 | ceiling(x) == -999))
            if (length(indxNAs) != 0) {
                for (m in 1:ncol(rets)) {
                    if (length(indxNAs[[m]]) != 0) {
                        rets[indxNAs[[m]], m] <- NA
                        rets[, m] <- na.locf(rets[, m], na.rm = FALSE)
                    }
                }
            }
        }
        rets  <-  rets / 100
        data_rets  <-  data.frame(dates, rets)
        colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
        files <- list.files()
        del_indx <- substr(files, nchar(files) - (3 - 1), nchar(files))
        indxDel <- which(del_indx == "CSV")
        file.remove(files[indxDel])
        write.csv(data_rets, paste0(factors, ".csv", sep = "") , row.names =
                      FALSE)

        setwd(dir)
    }


#' @title The bivariate_download function
#'
#' @description  This function automatically downloads Bivariate portfolios returns from the Kenneth R. French Data Library.
#' The downloaded returns are saved then as a .csv-file in the chosen directory.
#'
#' @param base a character string, the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir a character string, the directory for saving the data. The current working directory is set by default.
#' @param sub_path a character string, the folder subpath, created by ffDataDownload.
#' @param number_factors an integer, the number of factors for the Bivariate portfolios. Possible values are 6 (default), 25 or 100.
#' @param subtype a character string, the Bivariate portfolio subtype. Possible values are "ME_BE" (default), "ME_OP", "ME_INV", "BEME_OP", "BEME_INV", "OP_INV".
#' @param freq a character string, the frequency of the returns, "m" for monthly (default) or "d" for daily.
#' @param dividends a logical, TRUE (default) considers dividends. FALSE downloads the data without dividends.
#' @param start a character string, start date for the download in the format "YYmm". Default value is 197501.
#' @param end a character string, end date for the download in the format "YYmm". Default value is two months before Sys.Date() to insure availability.
#' @param cleanNAs a logical, TRUE (default) replaces NAs with zoo::na.locf(). If FALSE, NAs are not cleaned.
#' @return  a .csv-file within the directory, defined in dir.
#' @export bivariate_download

bivariate_download <-
    function(base,
             dir,
             sub_path,
             number_factors,
             subtype,
             freq,
             dividends,
             start,
             end,
             cleanNAs) {
        setwd(sub_path)
        format <- "_CSV.zip"

        if (subtype == "ME_BE") {
            subtype <- ""
        } #according to FamaFrench webseite

        if (subtype == "") {
            if (number_factors == 6) {
                factors <- paste(number_factors, "Portfolios", "2x3", sep = "_")
            } else if (number_factors == 25) {
                factors <- paste(number_factors, "Portfolios", "5x5", sep = "_")
            } else{
                factors <- paste(number_factors, "Portfolios", "10x10", sep = "_")
            }
        } else{
            if (number_factors == 6) {
                factors <-
                    paste(number_factors, "Portfolios", subtype, "2x3", sep = "_")
            } else if (number_factors == 25) {
                factors <-
                    paste(number_factors, "Portfolios", subtype, "5x5", sep = "_")
            } else {
                factors <-
                    paste(number_factors,
                          "Portfolios",
                          subtype,
                          "10x10",
                          sep = "_")
            }
        }
        if (freq == "d") {
            factors <- paste(factors, "_daily", sep = "")
        } else{
            factors <- factors
            if (dividends) {
                factors <- factors
            } else{
                if ((subtype == "ME_OP" | subtype == "ME_INV") &
                    number_factors == 100) {
                    factors <-
                        paste(number_factors,
                              "Portfolios",
                              "10x10",
                              subtype,
                              "Wout_Div",
                              sep = "_")
                } else{
                    factors <- paste(factors, "_Wout_Div", sep = "")
                }
            }
        }

        ##url(s)
        full_url <- paste(base, factors, format, sep = "")

        ##download (main)
        temp <- tempfile()
        download.file(full_url, temp, quiet = TRUE)

        ##open
        data <- unzip(temp)
        text <- readLines(data)
        skip_indx <- which(substr(text, 1, 2) == "19")[1] - 2
        data_rets <-
            read.csv(
                data,
                skip = skip_indx,
                header = TRUE,
                stringsAsFactors = FALSE
            )
        colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
        begin_indx <- which(substr(data_rets[, 1], 1, 6) == start)[1]
        end_indx <- which(substr(data_rets[, 1], 1, 6) == end)[1]
        if (is.na(end_indx)) {
            final <-
                format(max(as.Date(paste(
                    substr(data_rets[, 1], 1, 6), 1
                ), "%Y%m%d"), na.rm = TRUE), "%Y%m")
            print(
                paste(
                    "The end point ",
                    end,
                    " is still not present in the data. The time series' end is set to automatically to the latest reported date ",
                    final,
                    ".",
                    sep = ""
                )
            )
            end_indx <- which(substr(data_rets[, 1], 1, 6) == final)[1]
        }
        data_rets <- data_rets[begin_indx:end_indx, ]
        data_rets <- data_rets[order(data_rets[, 1]), ]
        dates <- data_rets[, 1]
        rets <- data_rets[, -1]
        rets <- apply(rets, 2, as.numeric)
        if (cleanNAs) {
            indxNAs <-
                apply(rets, 2, function(x)
                    which(ceiling(x) == -99 | ceiling(x) == -999))
            if (length(indxNAs) != 0) {
                for (m in 1:ncol(rets)) {
                    if (length(indxNAs[[m]]) != 0) {
                        rets[indxNAs[[m]], m] <- NA
                        rets[, m] <- na.locf(rets[, m], na.rm = FALSE)
                    }
                }
            }
        }

        rets <- rets / 100
        data_rets <- data.frame(dates, rets)
        colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
        files <- list.files()
        del_indx <- substr(files, nchar(files) - (3 - 1), nchar(files))
        indxDel <- which(del_indx == "CSV")
        file.remove(files[indxDel])
        write.csv(data_rets, paste0(factors, ".csv", sep = "") , row.names =
                      FALSE)

        setwd(dir)
    }

#' @title The threeway_download function
#'
#' @description This function automatically downloads Threeway portfolios returns from the Kenneth R. French Data Library.
#' The downloaded returns are saved then as a .csv-file in the chosen directory.
#'
#' @param base a character string, the main webpage address "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/".
#' @param dir a character string, the directory for saving the data. The current working directory is set by default.
#' @param sub_path a character string, the folder subpath, created by ffDataDownload.
#' @param subtype a character string, the Threeway portfolio subtype. Possible values are "ME_BEME_OP" (default), "ME_BEME_INV" or "ME_OP_INV".
#' @param dividends a logical, TRUE (default) considers dividends. FALSE downloads the data without dividends.
#' @param start a character string, start date for the download in the format "YYmm". Default value is 197501.
#' @param end a character string, end date for the download in the format "YYmm". Default value is two months before Sys.Date() to insure availability.
#' @param cleanNAs a logical, TRUE (default) replaces NAs with zoo::na.locf(). If FALSE, NAs are not cleaned.
#' @return  a .csv-file within the directory, defined in dir.
#' @export threeway_download

threeway_download <-
    function(base,
             dir,
             sub_path,
             subtype,
             dividends,
             start,
             end,
             cleanNAs) {
        setwd(sub_path)
        format <- "_CSV.zip"
        factors <- paste(32, "Portfolios", subtype, "2x4x4", sep = "_")
        if (dividends) {
            factors <- factors
        } else{
            factors <- paste(factors, "_Wout_Div", sep = "")
        }

        full_url <- paste(base, factors, format, sep = "")

        ##download (main)
        temp <- tempfile()
        download.file(full_url, temp, quiet = TRUE)

        ##open
        data <- unzip(temp)
        text <- readLines(data)
        skip_indx <- which(substr(text, 1, 2) == "19")[1] - 2
        data_rets <-
            read.csv(
                data,
                skip = skip_indx,
                header = TRUE,
                stringsAsFactors = FALSE
            )
        colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
        begin_indx <- which(substr(data_rets[, 1], 1, 6) == start)[1]
        end_indx <- which(substr(data_rets[, 1], 1, 6) == end)[1]
        if (is.na(end_indx)) {
            final <-
                format(max(as.Date(paste(
                    substr(data_rets[, 1], 1, 6), 1
                ), "%Y%m%d"), na.rm = TRUE), "%Y%m")
            print(
                paste(
                    "The end point ",
                    end,
                    " is still not present in the data. The time series' end is set to automatically to the latest reported date ",
                    final,
                    ".",
                    sep = ""
                )
            )
            end_indx <- which(substr(data_rets[, 1], 1, 6) == final)[1]
        }
        data_rets <- data_rets[begin_indx:end_indx, ]
        data_rets <- data_rets[order(data_rets[, 1]), ]
        dates <- data_rets[, 1]
        rets <- data_rets[, -1]
        rets <- apply(rets, 2, as.numeric)
        if (cleanNAs) {
            indxNAs <-
                apply(rets, 2, function(x)
                    which(ceiling(x) == -99 | ceiling(x) == -999))
            if (length(indxNAs) != 0) {
                for (m in 1:ncol(rets)) {
                    if (length(indxNAs[[m]]) != 0) {
                        rets[indxNAs[[m]], m] <- NA
                        rets[, m] <- na.locf(rets[, m], na.rm = FALSE)
                    }
                }
            }
        }
        rets <- rets / 100
        data_rets <- data.frame(dates, rets)
        colnames(data_rets) <- c("Date", colnames(data_rets)[-1])
        files <- list.files()
        del_indx <- substr(files, nchar(files) - (3 - 1), nchar(files))
        indxDel <- which(del_indx == "CSV")
        file.remove(files[indxDel])
        write.csv(data_rets, paste0(factors, ".csv", sep = "") , row.names =
                      FALSE)

        setwd(dir)
    }
