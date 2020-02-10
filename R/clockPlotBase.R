#' #' @title Base clock plot function
#' #'
#' #' @description
#' #' Create a "clock plot" showing PM2.5 data for a single day for the given 
#' #' monitors. A colored bar curves around in a clockwise manner with 12/4 of the
#' #' bar colored for each hour of the local time day.
#' #' 
#' #' This function will typically be called from \code{\link{clockPlot}} which
#' #' presents a simplified interface.
#' #' 
#' #' Room for annotations can be created by setting \code{plotRadius = 1.2}.
#' #' 
#' #' @param ws_monitor \emph{ws_monitor} object containing a single monitor.
#' #' @param startdate Desired start date (integer or character in Ymd format 
#' #'        or \code{POSIXct}).
#' #' @param enddate Desired end date (integer or character in Ymd format
#' #'        or \code{POSIXct}).
#' #' @param gapFraction Fraction of the circle used as the day boundary gap.
#' #' @param centerColor Color used for the center of the circle.
#' #' @param gapColor Color used for the day-break gap.
#' #' @param plotRadius Full radius of the plot. 
#' #' @param dataRadii Inner and outer radii for the data portion of the plot [0:1]. 
#' #' @param shadedNight Add nighttime shading.
#' #' @param hoursPerTick Add tick marks every # hours. Defaults to no ticks.
#' #' @param solarLabels Add sunrise/sunset labels.
#' #' @param colorPalette Palette function to convert monitor values into colors.
#' #' @param labelScale Scale factor applied to labels.
#' #' @param title Optional title for the plot.
#' #'
#' #' @return A `ggplot` plot object with a "clock plot" for a single monitor.
#' #' 
#' #' @seealso \code{\link{clockPlot}}
#' #' 
#' #' @importFrom rlang .data
#' #' @import dplyr
#' #' @export
#' #' @examples
#' #' ws_monitor <- PWFSLSmoke::Carmel_Valley
#' #' startdate <- "2016-08-07"
#' #' clockPlotBase(ws_monitor, startdate)
#' 
#' 
#' clockPlotBase <- function(ws_monitor,
#'                           startdate = NULL,
#'                           enddate = NULL,
#'                           centerColor = "black",
#'                           gapColor = "black",
#'                           gapFraction = 1/15,
#'                           plotRadius = 1.0,
#'                           dataRadii = c(0.5,1.0),
#'                           shadedNight = FALSE,
#'                           hoursPerTick = NULL,
#'                           solarLabels = FALSE,
#'                           colorPalette = aqiPalette("aqi"),
#'                           labelScale = 1.0,
#'                           title = "") {
#'   
#'   
#'   # For debugging --------------------------------------------------------------
#'   
#'   if (FALSE) {
#'     
#'     # Carmel Valley
#'     ws_monitor <- PWFSLSmoke::Carmel_Valley
#'     startdate <- "2016-08-07"
#'     enddate <- NULL
#'     centerColor <- "white"
#'     gapFraction <- 1/15
#'     plotRadius <- 1.2
#'     dataRadii <- c(0.5, 1.0)
#'     hoursPerTick = 3
#'     shadedNight <- TRUE
#'     solarLabels <- TRUE
#'     colorPalette <- aqiPalette("aqi")
#'     labelScale <- 1.0
#'     title <- ""
#'     
#'   }
#'   
#'   # Validate arguments ---------------------------------------------------------
#'   
#'   if ( !monitor_isMonitor(ws_monitor) ) {
#'     stop("Required parameter 'ws_monitor' is not a valid ws_monitor object")
#'   } else if ( monitor_isEmpty(ws_monitor) ) {
#'     stop("Required parameter 'ws_monitor' is empty.")
#'   }
#'   
#'   if ( ! nrow(ws_monitor$meta) == 1 ) {
#'     stop("Required parameter 'ws_monitor' must contain only one monitor.")
#'   }
#'   
#'   if ( is.null(startdate) && is.null(enddate) ) {
#'     stop("Required parameters 'startdate' and/or 'enddate' must be defined.")
#'   }
#'   
#'   # Set up style ---------------------------------------------------------------
#'   
#'   if ( shadedNight ) {
#'     shadedNightColor <- "gray90"
#'   } else {
#'     shadedNightColor <- "transparent"
#'   }
#'   
#'   shadedNightRadius <- dataRadii[2] + 0.5 * (plotRadius - dataRadii[2])
#'   solarLabelColor <- "black"
#'   solarLabelSize <- 4 * labelScale
#'   tickColor <- "black"
#'   tickLength <- 0.05 * plotRadius
#'   tickSize <- 0.5
#'   tickLabelSize = 4 * labelScale
#'   tickLabelColor = "black"
#'   
#'   # For bottom gap between the start and end of the day
#'   thetaOffset <- pi + (2 * pi) * (gapFraction / 2)
#'   
#'   # Time limits ----------------------------------------------------------------
#'   
#'   # Subset based on startdate and enddate
#'   
#'   timezone <- ws_monitor$meta$timezone[1]
#'   
#'   # If a startdate argument was passed, make sure it converts to a valid datetime
#'   if ( !is.null(startdate) ) {
#'     if ( is.numeric(startdate) || is.character(startdate) ) {
#'       startdate <- lubridate::ymd(startdate, tz = timezone)
#'     } else if ( lubridate::is.POSIXct(startdate) ) {
#'       startdate <- lubridate::force_tz(startdate, tzone = timezone)
#'     } else if ( !is.null(startdate) ) {
#'       stop(paste0(
#'         "Required parameter 'startdate' must be integer or character",
#'         " in Ymd format or of class POSIXct."))
#'     }
#'   }
#'   
#'   # If an enddate argument was passed, make sure it converts to a valid datetime
#'   if ( !is.null(enddate) ) {
#'     if ( is.numeric(enddate) || is.character(enddate) ) {
#'       enddate <- lubridate::ymd(enddate, tz = timezone)
#'     } else if ( lubridate::is.POSIXct(enddate) ) {
#'       enddate <- lubridate::force_tz(enddate, tzone = timezone)
#'     } else if ( !is.null(enddate) ) {
#'       stop(paste0(
#'         "Required parameter 'enddate' must be integer or character",
#'         " in Ymd format or of class POSIXct."))
#'     }
#'   }
#'   
#'   if ( !is.null(startdate) && is.null(enddate) ) {
#'     enddate <- startdate + lubridate::dhours(23)
#'   } else if ( is.null(startdate) && !is.null(enddate) ) {
#'     startdate <- enddate
#'     enddate <- enddate + lubridate::dhours(23)
#'   } else if ( !is.null(startdate) && !is.null(enddate) ) {
#'     enddate <- enddate + lubridate::dhours(23)
#'   }
#'   
#'   mon <- monitor_subset(ws_monitor, tlim=c(startdate,enddate))
#'   
#'   # Solar data -----------------------------------------------------------------
#'   
#'   ti <- timeInfo(startdate,
#'                  mon$meta$longitude,
#'                  mon$meta$latitude,
#'                  mon$meta$timezone)
#'   
#'   # Formatting the sunrise and sunset time of day
#'   sunriseHours <- as.numeric(difftime(ti$sunrise, startdate, units = "hours"))
#'   sunriseFraction <- sunriseHours * (1 - gapFraction) / 24
#'   sunsetHours <- as.numeric(difftime(ti$sunset, startdate, units = "hours"))
#'   sunsetFraction <- sunsetHours * (1 - gapFraction) / 24
#'   
#'   shadedNightData <- data.frame(
#'     xmin = c(0,0),
#'     xmax = c(shadedNightRadius,shadedNightRadius),
#'     ymin = c(0,sunsetFraction),
#'     ymax = c(sunriseFraction,1.0)
#'   )
#'   
#'   sunriseText <- paste0("Sunrise\n",
#'                         lubridate::hour(ti$sunrise), ":",
#'                         lubridate::minute(ti$sunrise))
#'   sunsetText <- paste0("Sunset\n",
#'                        lubridate::hour(ti$sunset), ":",
#'                        lubridate::minute(ti$sunset))
#'   
#'   # Clock data -----------------------------------------------------------------
#'   
#'   if (startdate == enddate) {
#'     stop("'startdate' and 'enddate' cannot be equal")
#'   }
#'   
#'   # Load hourly PM 2.5 data
#'   hours <- lubridate::hour(lubridate::with_tz(mon$data$datetime, tz=timezone))
#'   hourData <- data.frame(
#'     hour = hours,
#'     pm25 = mon$data[,2]
#'   )
#'   
#'   # Group readings by hour, then take the average reading of each hour
#'   clockData <- group_by(hourData, .data$hour) %>%
#'     summarise(pm25 = mean(.data$pm25, na.rm = TRUE))
#'   
#'   # Sanity check
#'   if ( !all(clockData$hour == 0:23) ) {
#'     stop("'datetime' hours are not 0:23")
#'   }
#'   
#'   # Define the start, end, and color of each period
#'   clockData$fraction = (1 - gapFraction) / 24
#'   clockData$ymax = cumsum(clockData$fraction)
#'   clockData$ymin <- c(0, lag(clockData$ymax)[-1])
#'   clockData$color = colorPalette(clockData$pm25)
#'   
#'   # Tick marks
#'   if ( !is.null(hoursPerTick) ) {
#'     tickCount <- round(24/hoursPerTick) + 1
#'     tickData <- data.frame(
#'       x = rep(dataRadii[2] - (0.5*tickLength), tickCount),
#'       xend = rep(dataRadii[2] + (0.5*tickLength), tickCount),
#'       y = seq(0, (1-gapFraction), length.out = tickCount),
#'       yend = seq(0, (1-gapFraction), length.out = tickCount),
#'       label_x = rep(dataRadii[2] + 1.5*tickLength, tickCount),
#'       hour = as.character(seq(0,(24),hoursPerTick))
#'     )
#'     # Omit ticks associated with 0 and 24
#'     tickData <- slice(tickData, 2:(n()-1))
#'   }
#'   
#'   # Plot data ------------------------------------------------------------------
#'   
#'   clockPlotBase <- ggplot() +
#'     
#'     # add shaded night
#'     geom_rect(
#'       data = shadedNightData,
#'       aes(
#'         xmin = .data$xmin,
#'         xmax = .data$xmax,
#'         ymin = .data$ymin,
#'         ymax = .data$ymax
#'       ),
#'       fill = shadedNightColor) +
#'     
#'     # polar coordinate system
#'     coord_polar(theta = 'y', direction = 1, start = thetaOffset) +
#'     xlim(0, plotRadius) +
#'     ylim(0, 1) + 
#'     
#'     # filled center
#'     geom_rect(
#'       aes(
#'         xmin = 0.0,
#'         xmax = 1.0,
#'         ymin = 0.0,
#'         ymax = 1.0
#'       ),
#'       fill = centerColor) +
#'     
#'     # add colored hour segments
#'     geom_rect(
#'       data = clockData,
#'       aes(
#'         ymin = .data$ymin,
#'         ymax = .data$ymax,
#'         xmin = dataRadii[1],
#'         xmax = dataRadii[2]),
#'       fill = clockData$color,
#'       color = clockData$color) +
#'     
#'     # day-break wedge
#'     geom_rect(
#'       aes(
#'         xmin = dataRadii[1],
#'         xmax = shadedNightRadius,
#'         ymin = 1 - gapFraction,
#'         ymax = 1.0
#'       ),
#'       fill = centerColor,
#'       color = centerColor)
#'   
#'   # tick marks  
#'   if ( !is.null(hoursPerTick) ) {
#'     
#'     clockPlotBase <- clockPlotBase +
#'       geom_segment(
#'         data = tickData,
#'         aes(
#'           x = .data$x,
#'           xend = .data$xend,
#'           y = .data$y,
#'           yend = .data$yend
#'         ),
#'         color = solarLabelColor,
#'         size = tickSize
#'       ) + 
#'       
#'       geom_text(
#'         data = tickData,
#'         aes(
#'           x = .data$label_x,
#'           y = .data$y,
#'           label = .data$hour
#'         ),
#'         color = tickColor,
#'         size = tickLabelSize
#'       )
#'     
#'   }
#'   
#'   # solar labels
#'   if ( solarLabels ) {
#'     
#'     clockPlotBase <- clockPlotBase +
#'       annotate("text",
#'                x = 1.0*plotRadius,
#'                y = sunriseFraction,
#'                label = sunriseText,
#'                color = solarLabelColor, 
#'                size = solarLabelSize) +
#'       annotate("text", 
#'                x = 1.0*plotRadius, 
#'                y = sunsetFraction, 
#'                label = sunsetText,
#'                color = solarLabelColor, 
#'                size = solarLabelSize)
#'     
#'   }
#'   
#'   # Remove all plot decorations
#'   clockPlotBase <- clockPlotBase +
#'     theme(panel.background = element_rect(fill = "transparent", color = NA)) +
#'     theme(plot.background = element_rect(fill = "transparent", color = NA)) +
#'     theme(panel.grid = element_blank()) +
#'     theme(axis.title = element_blank()) +
#'     theme(axis.text = element_blank()) +
#'     theme(axis.ticks = element_blank())
#'   
#'   # Add plot title
#'   clockPlotBase <- clockPlotBase +
#'     ggtitle(title) +
#'     theme(plot.title = element_text(color = "gray30", size = 20, hjust = 0.5))
#'   
#'   
#'   return(clockPlotBase)
#'   
#' }