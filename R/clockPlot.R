#' #' @title Create a Clock plot for a single monitor
#' #'
#' #' @description
#' #' Create a "clock plot" showing PM2.5 data for a single day for the given 
#' #' monitors. A colored bar curves around in a clockwise manner with 12/4 of the
#' #' bar colored for each hour of the local time day.
#' #' 
#' #' This function presents a simplified interface to \code{\link{clockPlotBase}}
#' #' and collects common options into a set of named styles. Currently 
#' #' supported styles consist of a \emph{base} style followed by one or more 
#' #' \emph{options} separated by underscores.
#' #' 
#' #' The style \emph{base} must be one of:
#' #' \itemize{
#' #' \item{\code{icon} -- no annotations}
#' #' \item{\code{full} -- fully annotated}
#' #' }
#' #' 
#' #' Style \emph{options} include:
#' #' \itemize{
#' #' \item{\code{fan} -- show nighttime shading}
#' #' \item{\code{avg} -- display center dot with yesterday average AQI color}
#' #' }
#' #' 
#' #' The returned object may be further amended with `ggplot` elements.
#' #' 
#' #' @details Dates are interpreted to be in monitor local time.
#' #' 
#' #' If either \code{starttime} or \code{endtime} is \code{NULL}, the plot will
#' #' represent a single day. If the specified time range covers multiple days, the
#' #' hourly bars will represent time-of-day averages while the center dot will 
#' #' show the average associated with the last day in the time range.
#' #' 
#' #' @param ws_monitor \emph{ws_monitor} object.
#' #' @param monitorID Monitor ID of interest.
#' #' @param startdate Desired start date (integer or character in Ymd format
#' #'        or \code{POSIXct}).
#' #' @param enddate Desired end date (integer or character in Ymd format
#' #'        or \code{POSIXct}).
#' #' @param style Plot style as described below.
#' #' @param centerColor Color used for the center of the circle.
#' #' @param labelScale Scale factor applied to labels.
#' #' @param title Optional title for the plot.
#' #'
#' #' @return A `ggplot` plot object with a "clock plot" for a single monitor.
#' #'
#' #' @seealso \code{\link{clockPlotBase}}
#' #' 
#' #' @export
#' #' @examples
#' #' mon <- PWFSLSmoke::Carmel_Valley
#' #' id <- mon$meta$monitorID[1]
#' #' start <- "2016-08-07"
#' #' end <- "2016-08-09"
#' #' icon <- clockPlot(mon, id, start, end, "icon",
#' #'                   title = "icon")
#' #' full <- clockPlot(mon, id, start, end, "full_fan_avg", "white",
#' #'                   title = "full_fan_avg")
#' #' gridExtra::grid.arrange(icon, full, nrow = 1)
#' 
#' 
#' clockPlot <- function(ws_monitor,
#'                       monitorID = NULL,
#'                       startdate = NULL,
#'                       enddate = NULL,
#'                       style = "icon",
#'                       centerColor = "black",
#'                       labelScale = 1.0,
#'                       title = "") {
#'   
#'   
#'   # For debugging --------------------------------------------------------------
#'   
#'   if (FALSE) {
#'     
#'     # Carmel Valley
#'     ws_monitor <- PWFSLSmoke::Carmel_Valley
#'     monitorID <- "060530002_01"
#'     startdate <- "2016-08-07"
#'     enddate <- NULL
#'     style <- "icon"
#'     centerColor <- "black"
#'     
#'   }
#'   
#'   # Validate arguments ---------------------------------------------------------
#'   
#'   if ( !monitor_isMonitor(ws_monitor) ) {
#'     stop("Argument 'ws_monitor' is not a valid ws_monitor object")
#'   } else if ( monitor_isEmpty(ws_monitor) ) {
#'     stop("Argument 'ws_monitor' is empty.")
#'   }
#'   
#'   if ( nrow(ws_monitor$meta) == 1 ) {
#'     monitorID <- ws_monitor$meta$monitorID[1]
#'   } else {
#'     if ( is.null(monitorID) ) {
#'       stop("Argument 'monitorID' must be defined.")
#'     } else if ( !monitorID %in% ws_monitor$meta$monitorID ) {
#'       stop(paste0("Monitor ", monitorID, " is not found in 'ws_monitor'"))
#'     }
#'     ws_monitor <- monitor_subset(ws_monitor, monitorIDs = monitorID)
#'   }
#'   
#'   # Accept any variation of style options with no required order
#'   validStyleBase <- c("icon", "full")
#'   validStyleOptions <- c("fan", "avg")
#'   styleParts <- unlist(stringr::str_split(style, "_"))
#'   styleBase <- styleParts[1]
#'   styleOptions <- styleParts[-1]
#'   
#'   if ( !styleBase %in% validStyleBase ) {
#'     stop(
#'       paste0(
#'         "Invalid style: \"", styleBase, "\". ",
#'         "The 'style' argument must begin with one of: \"", 
#'         paste0(validStyleBase, collapse = "|"), "\""
#'       )
#'     )
#'   }
#'   
#'   for ( option in styleOptions ) {
#'     if ( !option %in% validStyleOptions ) {
#'       stop(
#'         paste0(
#'           "Invalid style option: \"", option, "\". ",
#'           "The following 'style' argument options are supported: \"", 
#'           paste0(validStyleOptions, collapse = "|"), "\""
#'         )
#'       )
#'     }
#'   }
#'   
#'   # Set up style ---------------------------------------------------------------
#'   
#'   centerTextSize = 16 * labelScale
#'   
#'   # Create the plot ------------------------------------------------------------
#'   
#'   if ( styleBase == "icon" ) {
#'     
#'     if ( "fan" %in% styleOptions ) {
#'       plotRadius <- 1.4
#'       shadedNight <- TRUE
#'     } else {
#'       plotRadius <- 1.0
#'       shadedNight <- FALSE
#'     }
#'     
#'     clockPlotBase <- clockPlotBase(ws_monitor,
#'                                    startdate,
#'                                    enddate,
#'                                    centerColor = centerColor,
#'                                    gapFraction = 1/15,
#'                                    plotRadius = plotRadius,
#'                                    dataRadii = c(0.5,1),
#'                                    shadedNight = shadedNight,
#'                                    hoursPerTick = NULL,
#'                                    solarLabels = FALSE,
#'                                    labelScale = labelScale,
#'                                    title = title)
#'     
#'     
#'   } else if ( stringr::str_detect(style, "^full") ) {
#'     
#'     if ( "fan" %in% styleOptions ) {
#'       plotRadius <- 1.4
#'       shadedNight <- TRUE
#'     } else {
#'       plotRadius <- 1.2
#'       shadedNight <- FALSE
#'     }
#'     
#'     clockPlotBase <- clockPlotBase(ws_monitor,
#'                                    startdate,
#'                                    enddate,
#'                                    centerColor = centerColor,
#'                                    gapFraction = 1/15,
#'                                    plotRadius = plotRadius,
#'                                    dataRadii = c(0.5,1),
#'                                    shadedNight = shadedNight,
#'                                    hoursPerTick = 3,
#'                                    solarLabels = TRUE,
#'                                    labelScale = labelScale,
#'                                    title = title)
#'     
#'     
#'   }
#'   
#'   # Add a colored circle with last daily mean ----------------------------------
#'   
#'   if ( "avg" %in% styleOptions ) {
#'     
#'     # Get the last full daily mean
#'     dailyMeans <- monitor_getDailyMean(ws_monitor, 
#'                                        startdate = startdate, 
#'                                        enddate = enddate)
#'     dailyMean <- dailyMeans[length(dailyMeans)] # most efficient technique
#'     
#'     clockPlotBase <- clockPlotBase +
#'       
#'       # colored center (slightly smaller than filled center)
#'       geom_rect(
#'         aes(
#'           xmin = 0.0,
#'           xmax = 0.45,
#'           ymin = 0.0,
#'           ymax = 1.0
#'         ),
#'         fill = aqiPalette("aqi")(dailyMean)) +
#'       
#'       if ( styleBase == "full" ) {
#'         annotate("text", x = 0, y = .5,
#'                  label = round(dailyMean, digits=0),
#'                  color = "black",
#'                  size = centerTextSize)
#'       }
#'     
#'     
#'   }
#'   
#'   return(clockPlotBase)
#'   
#' }
