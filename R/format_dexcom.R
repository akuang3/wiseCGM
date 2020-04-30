#' Format DEXCOM data
#'
#' Format DEXCOM data into an analyzable dataframe.
#' @param input.path The name of the file which the data are to be read from.
#' @param rds.out The name of the output file
#' @param output.file logical. If TRUE, will output a list containing the formatted DEXCOM data and meta data.
#' @param check.calibration logical. If TRUE, check if devices older than G6 have at least 2 calibrations each day
#' @export
#'

format_dexcom <- function(input.path, rds.out=NULL, output.file=TRUE, check.calibration=FALSE){
  if(grepl('xls', input.path)){
    dexcom <- xlsx::read.xlsx(input.path, sheetIndex=1, check.names=FALSE,
                              stringsAsFactors=FALSE,
                              colClasses=c('integer', rep('character', 6), rep('numeric', 2), 'POSIXct', rep('numeric', 2), 'character'))
    if(is.null(rds.out)){
      rds.out <- gsub('xls', 'RDS', input.path)
    }

    dexcom[, grep('Duration', names(dexcom), value=TRUE)] <- strftime(dexcom[, grep('Duration', names(dexcom), value=TRUE)],
                                                                      format='%H:%M:%S', tz='UTC')

  }else{
    dexcom <- utils::read.csv(input.path, header=TRUE, stringsAsFactors=FALSE,
                              check.names=FALSE, na.strings='')
    if(is.null(rds.out)){
      rds.out <- gsub('csv', 'RDS', input.path)
    }
  }

  rds.out <- file.path(dirname(rds.out), 'RDS', basename(rds.out))
  if(!dir.exists(dirname(rds.out))){
    dir.create(dirname(rds.out))
  }

  names(dexcom) <- gsub(':|\\/| |-', '_', names(dexcom))
  names(dexcom) <- gsub('\\(|\\)', '', names(dexcom))
  #names(dexcom)[grepl('Index', names(dexcom))] <- 'Index'
  names(dexcom) <- iconv(names(dexcom), 'latin1', 'ASCII', sub='')

  meta.data <- dexcom[which(is.na(dexcom$Timestamp_YYYY_MM_DDThh_mm_ss)), ]
  names(meta.data) <- iconv(names(meta.data), 'latin1', 'ASCII', sub='')

  ## delete columns with only NAs
  remove.meta.cols <- sapply(meta.data, function(x) sum(is.na(x)))
  keep.meta.cols <- names(remove.meta.cols)[remove.meta.cols < nrow(meta.data)]
  meta.data <- meta.data[, keep.meta.cols]

  if('Patient_Info' %in% names(meta.data)){
    patient.meta.data <- meta.data[grep('FirstName|LastName|DateOfBirth', meta.data$Event_Type),
                                   c('Event_Type','Patient_Info')]
  }else{
    patient.meta.data <- NULL
  }

  meta.data <- meta.data[-grep('FirstName|LastName|DateOfBirth', meta.data$Event_Type), ]

  meta.data.list <- split(meta.data, f=meta.data$Source_Device_ID)
  meta.data.list <- lapply(meta.data.list, function(meta){
    meta$Device_Info[is.na(meta$Device_Info)] <- meta$Device_Info[!is.na(meta$Device_Info)];meta
    meta <- meta[!grepl('Device', meta$Event_Type), ]
  })

  #names(meta.data.list) <- paste0('Alert ', names(meta.data.list))

  #meta.data <- do.call(rbind, meta.data.list)

  if(!is.null(patient.meta.data)){
    meta.data.list <- c(list(patient.meta.data), meta.data.list)
    names(meta.data.list)[1] <- 'Patient Information'
  }

  ## informative meta data
  informative.meta.data <- lapply(meta.data.list, function(mdl){
    mdl <- mdl[, c('Event_Subtype', 'Glucose_Value_mg_dL')]
    mdl <- as.data.frame.matrix(t(mdl), stringsAsFactors=FALSE)
    names(mdl) <- mdl[1, ]
    mdl <- mdl[2, ]
    mdl <- mdl[, !is.na(mdl)]
    return(mdl)
  })

  ## dexcom cgm data
  cgm.data <- dexcom[-which(is.na(dexcom$Timestamp_YYYY_MM_DDThh_mm_ss)), ]

  ## delete columns with only NAs
  remove.cgm.cols <- sapply(cgm.data, function(x) sum(is.na(x)))
  keep.cgm.cols <- names(remove.cgm.cols)[remove.cgm.cols < nrow(cgm.data)]

  cgm.data <- cgm.data[, keep.cgm.cols[!grepl('Index', keep.cgm.cols)]]

  ## create date / time variable variable
  cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss <- gsub('T', ' ', cgm.data$Timestamp_YYYY_MM_DDThh_mm_ss)
  #cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss  <- gsub('\\..*', '', cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss )
  cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss <- base::as.POSIXct(lubridate::parse_date_time(cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss,
                                                                                        c("mdy HM", "mdy HMS", "mdY HM", "mdY HMS",
                                                                                          "dmy HM", "dmy HMS", "dmY HM", "dmY HMS", "Ymd HM", "Ymd HMS",
                                                                                          "ymd HM", "ymd HMS", "Ydm HM", "Ydm HMS", "ydm HM", "ydm HMS")),
                                                             tz='UTC')

  cgm.data$Timestamp_YYYY_MM_DDThh_mm_ss <- NULL

  cgm.data$Date <- as.Date(cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss)
  cgm.data$Year <- lubridate::year(cgm.data$Date)
  cgm.data$Month <- lubridate::month(cgm.data$Date)
  cgm.data$Day <- lubridate::day(cgm.data$Date)
  cgm.data$WeekDay <- factor(base::weekdays(cgm.data$Date, abbreviate=FALSE),
                             levels=weekdays(x=as.Date(0:6, origin='1950-01-01')))
  cgm.data$Time <- strftime(cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss, format='%H:%M:%S', tz='UTC')
  cgm.data$Hour <- lubridate::hour(strftime(cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss, fomrat='%H:%M:%S', tz='UTC'))
  cgm.data$Min <- lubridate::minute(strftime(cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss, fomrat='%H:%M:%S', tz='UTC'))
  cgm.data$Sec <- lubridate::second(strftime(cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss, fomrat='%H:%M:%S', tz='UTC'))

  cgm.data$Day_Time <- cgm.data$Day + ((cgm.data$Hour*60*60) + (cgm.data$Min*60) + (cgm.data$Sec))/86400

  cgm.data$TP <- month.name[cgm.data$Month]

  ## create a column to indicate levels of glucose warnings
  cgm.data$EGV_Warnings <- ifelse(cgm.data$Event_Type=='EGV' & grepl('^[A-Za-z]+$', cgm.data$Glucose_Value_mg_dL),
                                  cgm.data$Glucose_Value_mg_dL, NA)
  cgm.data$Glucose_Value_mg_dL_v2 <- as.numeric(cgm.data$Glucose_Value_mg_dL)

  cgm.data <- cgm.data[order(cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss), ]


  cgm.data.list <- split(cgm.data, f=cgm.data$Source_Device_ID)

  ## determine if glucose variable is within, above, or below target
  cgm.data.list <- lapply(names(cgm.data.list), function(device){
    check <- meta.data.list[[device]]
    urgent.low <- as.numeric(check$Glucose_Value_mg_dL[which(check$Event_Subtype=='Urgent Low')])
    low <- as.numeric(check$Glucose_Value_mg_dL[which(check$Event_Subtype=='Low')])
    high <- as.numeric(check$Glucose_Value_mg_dL[which(check$Event_Subtype=='High')])

    ## split between EGV and Calibration from others
    temp <- cgm.data.list[[device]]
    temp.others <- temp[!grepl('EGV|Calibration', temp$Event_Type), ]

    temp <- temp[grepl('EGV|Calibration', temp$Event_Type), ]

    temp$TARGET <- sapply(temp[,'Glucose_Value_mg_dL_v2'], function(x){
      ifelse(is.na(x), NA,
             ifelse(x <= urgent.low, 'Urgent Low',
                    ifelse(x > urgent.low & x <= low, 'Low',
                           ifelse(x >= high, 'High', 'On Target'))))
    })
    temp$TARGET <- ifelse(is.na(temp$TARGET), temp$Event_Subtype, temp$TARGET)

    ## merge back other
    temp <- data.table::rbindlist(list(temp, temp.others), fill=TRUE)

    return(temp)
  })

  cgm.data <- do.call(rbind, cgm.data.list)
  ### if TARGET is NA, use Warnings
  cgm.data$TARGET <- ifelse(is.na(cgm.data$TARGET), cgm.data$EGV_Warnings, cgm.data$TARGET)


  #if(any(!names(cgm.data) %in% 'Event_Subtype')){
  #  cgm.data[, 'Event_Subtype'] <- NA
  #}

  ## create a copy without the high/low values in glucouse value mg dl column
  cgm.data.sub <- cgm.data
  #cgm.data.sub <- cgm.data.sub[, !names(cgm.data.sub) %in% 'Event_Subtype']

  ## number of calibration(s) each day
  date.calibrations <- as.data.frame(table(cgm.data.sub$Event_Type, cgm.data.sub$Date), stringsAsFactors=FALSE)
  date.calibrations <- date.calibrations[which(date.calibrations$Var1=='Calibration'), ]

  if(check.calibration==TRUE){
    ## determine what device were used
    ## if before Dexcom G6, remove those with less than 2 calibration
    as.integer(sub(".*?G.*?(\\d+).*", "\\1", 'Dexcom G6 Mobile App'))

    if(any(sapply(meta.data.list, function(mdl){
      device.gen <- mdl$Device_Info[!is.na(mdl$Device_Info)]
      return(as.integer(sub(".*?G.*?(\\d+).*", "\\1", device.gen)))
    }) < 6)){
      before.g6 <- which(sapply(meta.data.list, function(mdl){
        device.gen <- mdl$Device_Info[!is.na(mdl$Device_Info)]
        return(as.integer(sub(".*?G.*?(\\d+).*", "\\1", device.gen)))
      }) < 6)
      names(before.g6) <- gsub('Alert ', '', names(before.g6))
      cgm.device <- split(cgm.data.sub, f=cgm.data.sub$Source_Device_ID)
      cgm.device <- lapply(names(cgm.device), function(x){
        if(x %in% names(before.g6)){
          temp <- cgm.device[[x]]
          ## dates with 2 or more calibrations
          date.calibrations <- date.calibrations[which(date.calibrations$Freq >= 2), ]
          date.calibrations$Var2 <- as.Date(date.calibrations$Var2)
          temp <- temp[temp$Date %in% date.calibrations$Var2, ]
          return(temp)
        }else{
          cgm.device[[x]]
        }
      })
      cgm.data.sub <- do.call(rbind, cgm.device)
    }
  }

  if(dim(cgm.data.sub)[1] != 0){
    ## remove calibrations
    cgm.data.sub <- cgm.data.sub[which(cgm.data.sub$Event_Type!='Calibration'), ]
    ## 2020/04/29: Imputed data for every 5 minute

    cgm.transmitter.id <- split(cgm.data.sub, f=cgm.data.sub$Transmitter_ID)
    cgm.transmitter.id <- lapply(cgm.transmitter.id, function(x){
      ## find difference between adjancet rows
      cgm.start.dt <- x[which.min(x$Day_Time), ]
      cgm.end.dt <- x[which.max(x$Day_Time), ]

      cgm.start <- x[which.min(x$Transmitter_Time_Long_Integer), ]
      cgm.end <- x[which.max(x$Transmitter_Time_Long_Integer), ]

      full.ttl <- seq(cgm.start$Transmitter_Time_Long_Integer,
                      cgm.end$Transmitter_Time_Long_Integer,
                      by=300)
      full.dt <- seq(cgm.start.dt$Day_Time,
                     cgm.end.dt$Day_Time,
                     by=5/1440)

      full.timeframe <- data.frame(Transmitter_Time_Long_Integer=full.ttl,
                                   Day_Time_Full=full.dt,
                                   stringsAsFactors=FALSE)

      x <- merge(full.timeframe, x, by='Transmitter_Time_Long_Integer', all=TRUE)
      x <- x[order(x$Transmitter_Time_Long_Integer), ]
      x$ORD <- 1:nrow(x)
      x$Time_Diff <- c(300,
                       x$Transmitter_Time_Long_Integer[-1]-x$Transmitter_Time_Long_Integer[-nrow(x)])

      time.diff.table <- as.numeric(names(table(x$Time_Diff)))
      time.diff.table <- sapply(time.diff.table, function(x){
        temp <- data.frame(V1=x, V2=time.diff.table)
        temp <- temp[which(temp$V1+temp$V2==300),]
        if(dim(temp)[1]>0){
          min(temp$V1, temp$V2)
        }else{
          NULL
        }
      })
      time.diff.table <- unlist(time.diff.table)

      ## remove those that are off by a few seconds
      x <- x[-which(is.na(x$Day_Time) &
                      x$Time_Diff <= max(time.diff.table)), ]

      x$Day_Time <- ifelse(is.na(x$Day_Time), x$Day_Time_Full, x$Day_Time)
      x$Day <- ifelse(is.na(x$Day), floor(x$Day_Time_Full), x$Day)
      x$Total_Seconds <- (x$Day_Time %% 1)*(24*60*60)

      x$Hour <- ifelse(is.na(x$Hour), floor(x$Total_Seconds/3600), x$Hour)

      x$Total_Seconds <- (x$Total_Seconds - (x$Hour*60*60))

      x$Min <- ifelse(is.na(x$Min), floor(x$Total_Seconds/60), x$Min)

      x$Total_Seconds <- (x$Total_Seconds - (x$Min*60))

      x$Sec<- ifelse(is.na(x$Sec), round(x$Total_Seconds), x$Sec)

      x$Time <- ifelse(is.na(x$Time), paste(sprintf('%02d', x$Hour),
                                            sprintf('%02d', x$Min),
                                            sprintf('%02d', x$Sec), sep=':'),
                       x$Time)

      ## append month, duration of time wearing device can cross over months
      ## work on this later, assume month, and devices are the same
      x$Source_Device_ID <- ifelse(is.na(x$Source_Device_ID), x$Source_Device_ID[1], x$Source_Device_ID)
      x$Transmitter_ID <- ifelse(is.na(x$Transmitter_ID), x$Transmitter_ID[1], x$Transmitter_ID)
      x$Month <- ifelse(is.na(x$Month), x$Month[1], x$Month)
      x$Year <- ifelse(is.na(x$Year), x$Year[1], x$Year)
      x$Event_Type <- ifelse(is.na(x$Event_Type), x$Event_Type[1], x$Event_Type)
      x$TP <- ifelse(is.na(x$TP), x$TP[1], x$TP)

      x$Date <- as.Date(paste(x$Year, x$Month,
                              x$Day, sep='-'), format='%Y-%m-%d')
      x$WeekDay <- factor(base::weekdays(as.Date(x$Date), abbreviate=FALSE),
                          levels=weekdays(x=as.Date(0:6, origin='1950-01-01')))

      return(x)

    })

    cgm.data.sub <- do.call(rbind, cgm.transmitter.id)


    ## find the first and last reading for each device



    cgm.data.sub.list <- split(cgm.data.sub, f=cgm.data.sub$Source_Device_ID)

    cgm.data.sub.list <- lapply(names(cgm.data.sub.list), function(device){
      check <- meta.data.list[[device]]
      ## split between EGV and Calibration from others
      temp <- cgm.data.sub.list[[device]]

      ## separate EGV and calibrations from other
      temp.other <- temp[!grepl('EGV|Calibration', temp$Event_Type), ]
      temp <- temp[grepl('EGV|Calibration', temp$Event_Type), ]
      temp <- temp[order(temp$Day), ]

      for(i in 3:(nrow(temp)-2)){
        check.event <- temp$EGV_Warnings[i]
        check.threshold <- as.numeric(check$Glucose_Value_mg_dL[which(check$Event_Subtype==check.event)])
        check.value <- temp$Glucose_Value_mg_dL_v2[i]
        if(is.na(check.value)){
          intermediate.sv <- temp[((i-2):(i+2))[!((i-2):(i+2)) %in% i], ]
          i.sv.mean <- mean(intermediate.sv$Glucose_Value_mg_dL_v2, na.rm=TRUE)
          i.sv.sd <- sd(intermediate.sv$Glucose_Value_mg_dL_v2, na.rm=TRUE)

          upp.b <- i.sv.mean + i.sv.sd
          low.b <- i.sv.mean - i.sv.sd

          if(grepl('Low', check.event)){
            temp$Glucose_Value_mg_dL_v2[i] <- min(i.sv.mean, check.threshold)
          }else{
            temp$Glucose_Value_mg_dL_v2[i] <- max(i.sv.mean, check.threshold)
          }

        }
      }

      temp <- do.call(rbind, list(temp, temp.other))
      #temp <- temp[order(temp$Timestamp_YYYY_MM_DD_hh_mm_ss), ]
      return(temp)
    })


    cgm.data.sub <- do.call(rbind, cgm.data.sub.list)
    cgm.data.sub <- cgm.data.sub[order(cgm.data.sub$Day_Time), ]
    cgm.data.sub$Timestamp_YYYY_MM_DD_hh_mm_ss <- cgm.data.sub$Transmitter_Time_Long_Integer <- cgm.data.sub$Day_Time_Full <- NA
    cgm.data.sub$ORD <- cgm.data.sub$Time_Diff <- cgm.data.sub$Total_Seconds <- NA
  }


  ### relabel TARGET with mean imputed glucose
  cgm.data.sub.list <- split(cgm.data.sub, f=cgm.data.sub$Source_Device_ID)

  ## determine if glucose variable is within, above, or below target
  cgm.data.sub.list <- lapply(names(cgm.data.sub.list), function(device){
    check <- meta.data.list[[device]]
    urgent.low <- as.numeric(check$Glucose_Value_mg_dL[which(check$Event_Subtype=='Urgent Low')])
    low <- as.numeric(check$Glucose_Value_mg_dL[which(check$Event_Subtype=='Low')])
    high <- as.numeric(check$Glucose_Value_mg_dL[which(check$Event_Subtype=='High')])

    ## split between EGV and Calibration from others
    temp <- cgm.data.sub.list[[device]]
    temp.others <- temp[!grepl('EGV|Calibration', temp$Event_Type), ]

    temp <- temp[grepl('EGV|Calibration', temp$Event_Type), ]

    temp$TARGET <- sapply(temp[,'Glucose_Value_mg_dL_v2'], function(x){
      ifelse(is.na(x), NA,
             ifelse(x <= urgent.low, 'Urgent Low',
                    ifelse(x > urgent.low & x <= low, 'Low',
                           ifelse(x >= high, 'High', 'On Target'))))
    })
    temp$TARGET <- ifelse(is.na(temp$TARGET), temp$Event_Subtype, temp$TARGET)

    ## merge back other
    temp <- data.table::rbindlist(list(temp, temp.others), fill=TRUE)

    return(temp)
  })

  cgm.data.sub <- do.call(rbind, cgm.data.sub.list)
  cgm.data.sub$Timestamp_YYYY_MM_DD_hh_mm_ss <- cgm.data.sub$Transmitter_Time_Long_Integer <- NULL


  cgm.data$Timestamp_YYYY_MM_DD_hh_mm_ss <- cgm.data$Transmitter_Time_Long_Integer <- NULL


  meta.data.list <- lapply(meta.data.list, function(mdl){
    mdl$TP <- paste(unique(cgm.data$TP), collapse=', ');mdl
  })

  informative.meta.data <- lapply(informative.meta.data, function(mdl){
    mdl$TP <- paste(unique(cgm.data$TP), collapse=', '); mdl
  })


  rds.list <- list(full.cgm.data=cgm.data,
                   mean.imputed.cgm.data=cgm.data.sub,
                   meta.data=meta.data.list,
                   informative.meta.data=informative.meta.data)

  saveRDS(rds.list,
          file=rds.out)

  if(output.file==TRUE){
    return(list(full.cgm.data=cgm.data,
                informative.meta.data=informative.meta.data,
                mean.imputed.cgm.data=cgm.data.sub))
  }

}

utils::globalVariables(names=c('sd'))
