#' raadtools::timedateFrom
raad_timedate_from <- function(x, ...) {
  as.POSIXct(x, tz = "UTC", ...)
}

#' raadtools:::.valiDates
raad_vali_dates <- function(x, allOK = TRUE) {
  xs <- raad_timedate_from(x)
  bad <- is.na(xs)
  if (all(bad)) stop("no input dates are valid")
  if (any(bad)) {
    notOK <- "not all input dates are valid"
    if (allOK) stop(notOK) else warning(notOK)
  }
  xs[!bad]
}

#' raadtools:::.indexDates
raad_index_dates <- function(xdate, filedate) {
  windex <- findInterval(xdate, filedate)
  windex[windex < 1] <- 1
  windex[windex > length(filedate)] <- length(filedate)
  windex
}
#' raadtools:::.sortDates
raad_sort_dates <- function(x, resortOK = FALSE) {
  ord <- order(x)
  if (any(diff(ord) < 0)) {
    sortOK <- "dates out of order and will be sorted"
    if (resortOK) warning(sortOK) else stop(sortOK)
    x <- x[ord]
  }
  x
}

#' raadtools:::.dedupe
raad_dedupe <- function(index, date, removeDupes = TRUE) {
  nondupes <- !duplicated(index)
  if (sum(nondupes) < length(index)) {
    if (removeDupes) warning("duplicated dates will be dropped") else stop("duplicated dates not allowed")
    index <- index[nondupes]
    date <- date[nondupes]
  }
  list(index = index, date = date)
}

#' raadtools:::.matchFiles
raad_match_files <- function(querydate, refdate, index, daytest = 7) {
  ##
  deltatime <- abs(difftime(querydate, refdate, units = "days"))
  deltatest <- deltatime > daytest
  if (all(deltatest)) {
    message(sprintf("\nnearest available date is %s", as.Date(refdate)))
    stop(sprintf("no data file within %.1f days of %s", daytest, format(querydate)))
  }
  if (any(deltatest)) {
    warning(sprintf("%i input dates have no corresponding data file within %f days of available files", sum(deltatest), daytest))
    index <- index[!deltatest]
  }
  index
}

#' raadtools:::.processDates
raad_process_dates <- function(qdate, fdate, timeres) {
  ## checks on dates, we drop any that are NA
  #qdate <- .valiDates(qdate, allOK = FALSE)
 qdate <- raad_vali_dates(qdate, allOK = FALSE)

  ## sort dates if need be
  #qdate <- .sortDates(qdate, resortOK = TRUE)
 qdate <- raad_sort_dates(qdate, resortOK = TRUE)

  ## mapping of files/dates, so we can process time series
  #findex <- .indexDates(qdate, fdate)
 findex <- raad_index_dates(qdate, fdate)

  ## check for duplicates
  #dedupedates <- .dedupe(findex, qdate, removeDupes = TRUE)
  dedupedates <- raad_dedupe(findex, qdate, removeDupes = TRUE)
  findex <- dedupedates$index
  date <- dedupedates$date

  #.matchFiles(date, fdate[findex], findex,
  raad_match_files(date, fdate[findex], findex,
              daytest = switch(timeres, "4hourly" = 1/6, "12hourly" = 1/2, "3hourly" = 1/8, "6hourly" = 0.25, daily = 1.5, weekly = 4, monthly = 15, weekly3 = 26, "8daily" = 5, "8D" = 5))

}

#' raadtools:::.processFiles
raad_process_files <- function(dt, f, tr) {
  #findex <- .processDates(dt, f$date, tr)
  findex <- raad_process_dates(dt, f$date, tr)
  f[findex, ]
}
