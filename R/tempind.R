yearDays <- function( time ) {
  # Copied from Hmisc (so that the whole package don't have to be loaded due to this single function)
  time <- as.POSIXlt(time)
  time$mon[] <- time$mday[] <- time$sec[] <- time$min <- time$hour <- 0
  time$year <- time$year + 1
  return(as.POSIXlt(as.POSIXct(time))$yday + 1)
}

#' Create temporal indicators
#'
#' @param Dates The vector of dates from which the indicators should be calculated
#' @param DATEnum Numeric dates needed?
#' @param DOW Day-of-week indicator needed?
#' @param week_start_in Day on which week starts (1: Monday, 7: Sunday)
#' @param YEAR Year indicator needed?
#' @param DOY Day-of-year indicator needed?
#' @param WEEK Week indicator needed?
#' @param NWD Non-working day indicator needed? (NULL if not, "None" if needed but without pre/post indicators, "Global"
#' if needed with one single pre/post indicator, regardless of the non-working day, "Individual" if an own pre/post indicator
#' is needed for each non-working day individually)
#' @param SWAP Swapped working day indicator needed? (NULL if not, a data frame with columns 'date' (date of swap) and 'name'
#' (names, e.g. 'Non-working day' and 'Working day' ) if yes)
#' @param Congresses What congress indicators are needed? (NULL if none, list of congresses otherwise)
#' @param CongressDatesIn Data frame that holds the dates of congresses with columns 'CongressName' (name of the congress),
#' 'StartDate' (starting date), 'EndDate' (end date)
#' @param CongPrePost Pre/post indicators for the congress(es) needed?
#' @param SDAY Semmelweis Day indicator needed?
#' @param MONTH Month indicator needed?
#' @param SEASON Season indicator needed?
#'
#' @return A data frame containing the requested indicators in its columns
#' @export
#'
#' @examples TemporalIndicators( seq( as.Date( "2018-01-01" ), as.Date( "2019-03-31" ), by = "days" ) )
TemporalIndicators <- function( Dates, DATEnum = TRUE, DOW = TRUE, week_start_in = 1, YEAR = TRUE, DOY = TRUE, WEEK = TRUE,
                                NWD = "Individual", SWAP = TemporalIndicators::SwappedWorkingDays, Congresses = NULL,
                                CongressDatesIn = TemporalIndicators::CongressDates,
                                CongPrePost = TRUE, SDAY = TRUE, MONTH = TRUE, SEASON = TRUE ) {
  if( !lubridate::is.Date( Dates ) )
    stop( "Dates need to be of date type!" )
  res <- data.frame( ID = seq_along( Dates ) )
  if( DATEnum )
    res$DATEnum <- as.numeric( Dates )
  if( DOW )
    res$DOW <- lubridate::wday( Dates, week_start = week_start_in )
  if( YEAR )
    res$YEAR <- lubridate::year( Dates )
  if( DOY )
    res$DOY <- (lubridate::yday( Dates )-1)/(yearDays( Dates )-1)
  if( WEEK )
    res$WEEK <- lubridate::isoweek( Dates )
  if( !is.null( NWD ) ) {
    if( !NWD%in%c( "Individual", "Global", "None" ) )
      stop( "NWD can only take the value of 'Individual', 'Global' or 'None'!" )
    datesyears <- do.call( seq, as.list( lubridate::year( range( Dates ) ) ) )
    datesyears <- c( min( datesyears )-1, datesyears, max( datesyears )+1 )
    holidaydates <- c( `Jan 1` = "01-01", `March 15` = "03-15", `May 1` = "05-01", `Aug 20` = "08-20", `Oct 23` = "10-23",
                       `Nov 1` = "11-01", `Dec 25` = "12-25", `Dec 26` = "12-26" )
    holidays <- data.frame( name = rep( c( names( holidaydates ), "Easter", "Pentecost" ), each = length( datesyears ) ),
                            date = c( as.Date( apply( expand.grid( datesyears, holidaydates ), 1, paste0,
                                                      collapse = "-" ) ), as.Date( timeDate::EasterMonday( datesyears ) ),
                                      as.Date( timeDate::PentecostMonday( datesyears ) ) ), stringsAsFactors = FALSE )
    if( any( datesyears>=2017 ) )
      holidays <- rbind( holidays, data.frame( name = "Good Friday",
                                               date = as.Date( timeDate::GoodFriday( unique( pmax( 2017, datesyears ) ) ) ) ) )

    if( NWD=="Individual" ) {
      holidays <- rbind( holidays,
                         data.frame( name = paste0( "pre-", holidays$name[ holidays$name!="Dec 26" ] ),
                                     date = holidays$date[ holidays$name!="Dec 26" ]-1 ),
                         data.frame( name = paste0( "post-", holidays$name[ holidays$name!="Dec 25" ] ),
                                     date = holidays$date[ holidays$name!="Dec 25" ]+1 ) )

    } else if( NWD=="Global" ) {
      holidays <- rbind( holidays,
                         data.frame( name = "Pre", date = holidays$date[ holidays$name!="Dec 26" ]-1 ),
                         data.frame( name = "Post", date = holidays$date[ holidays$name!="Dec 25" ]+1 ) )
    }

    res$NWD <- dplyr::left_join( data.frame( date = Dates ), holidays, by = "date" )$name
    res$NWD[ is.na( res$NWD ) ] <- "None"
  }
  if( !is.null( SWAP ) ) {
    if( !is.data.frame( SWAP )|!"name"%in%colnames( SWAP )|!"date"%in%colnames( SWAP ) )
      stop( "SWAP must be a data frame with columns named 'date' and 'name'!" )
    res$SWAP <- dplyr::left_join( data.frame( date = Dates ), SWAP, by = "date" )$name
    res$SWAP[ is.na( res$SWAP ) ] <- "None"
  }
  if( !is.null( Congresses ) ) {
    if( any( !Congresses%in%CongressDatesIn$CongressName ) )
      stop( "Requested congresses must be available in 'CongressDates'!" )
    for( cong in Congresses ) {
      congdays <- data.frame( name = "Congress day",
                              date = do.call( c, apply( CongressDatesIn[ CongressDatesIn$CongressName==cong, ], 1,
                                                        function( x ) seq( as.Date( x[ "StartDate" ] ),
                                                                           as.Date( x[ "EndDate" ] ), by = "days" ) ) ),
                              stringsAsFactors = FALSE )
      if( CongPrePost ) {
        congdays <- rbind( congdays,
                           data.frame( name = "Pre-congress day",
                                       date = CongressDatesIn[ CongressDatesIn$CongressName==cong, ]$StartDate-1 ),
                           data.frame( name = "Post-congress day",
                                       date = CongressDatesIn[ CongressDatesIn$CongressName==cong, ]$EndDate+1 ) )
      }
      res[[ cong ]] <- dplyr::left_join( data.frame( date = Dates ), congdays, by = "date" )$name
      res[[ cong ]][ is.na( res[[ cong ]] ) ] <- "None"
    }
  }
  if( SDAY )
    res$SDAY <- ifelse( lubridate::year( Dates )>=2011&lubridate::month( Dates )==7&lubridate::day( Dates )==1,
                        "Semmelweis Day", "None" )
  if( MONTH )
    res$MONTH <- lubridate::month( Dates )
  if( SEASON )
    res$SEASON <- cut( lubridate::month( Dates )%%12, breaks = c( -Inf, 2, 5, 8, 11 ), labels = FALSE )

  return( res[ , -1 ] )
}

#' Congress dates in Hungary, 2004-2019
#'
#' Dates of the cardiological, surgical and cataracta societies in Hungary, 2004-2019.
#'
#' @format A data frame with 40 rows and 3 variables:
#' \describe{
#'   \item{CongressName}{Name of the congress}
#'   \item{StartDate}{Starting date of the congress in a given year}
#'   \item{EndDate}{End date of the congress in a given year}
#' }
"CongressDates"

#' Swapped working and non-working days in Hungary, 2004-2019
#'
#' Dates when working and non-working days were swapped in Hungary, 2004-2019.
#'
#' @format A data frame with 90 rows and 2 variables:
#' \describe{
#'   \item{date}{Date of swapped}
#'   \item{name}{Name of that date, e.g. 'Non-working day' and 'Working day'}
#' }
"SwappedWorkingDays"
