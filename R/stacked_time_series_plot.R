#' Stacked Time Series Plot
#'
#' Generate Time Series Plot from all three time points stacked on top of each other
#' @param cgm.data A dataframe of CGM data formatted by format_dexcom() for a patient, containing all timepoints
#' @param informative.cgm.meta A dataframe of CGM meta data formatted by format_dexcom()
#' @param file.name The name of the output file
#' @export
#'
stacked_time_series_plot <- function(cgm.data, informative.cgm.meta,
                                     file.name=NULL){

  ## calculate summary statistics to place on title
  cgm.data.list <- split(cgm.data, f=cgm.data$TP)
  df.info.mean <- lapply(cgm.data.list, function(x){
    round(mean(x$Glucose_Value_mg_dL, na.rm=TRUE), 2)
  })

  df.info.sd <- lapply(cgm.data.list, function(x){
    round(sd(x$Glucose_Value_mg_dL, na.rm=TRUE), 2)
  })

  df.info.percent.wir <- lapply(cgm.data.list, function(x){
    round(100*(sum(x$TARGET=='On Target', na.rm=TRUE))/(sum(!is.na(x$TARGET))), 2)
  })

  df.info <- lapply(names(cgm.data.list), function(month){
    data.frame(TP=month,
               INFO=paste0(month, ': ', df.info.mean[[month]], ' (', df.info.sd[[month]], '), ', df.info.percent.wir[[month]], '%'),
               stringsAsFactors=FALSE)
  })
  df.info <- do.call(rbind, df.info)

  info.order <- sapply(names(cgm.data.list), function(month){
    df.info[grep(month, df.info$TP), 'INFO']
  })

  cgm.data <- merge(cgm.data, df.info, by='TP', all=TRUE)

  cgm.data$INFO <- factor(cgm.data$INFO, levels=info.order)

#  cgm.spline.list <- lapply(names(cgm.data.list), function(month){

#    temp <- cgm.data.list[[month]][cgm.data.list[[month]]$Event_Type=='EGV', ]
#    temp <- temp[!is.na(temp$Glucose_Value_mg_dL), ]


#    matrix_gam <- data.table::data.table(Glucose=temp[, 'Glucose_Value_mg_dL'],
#                                         Hourly=temp[ ,'Hour'],
#                                         Daily=temp[, 'Day'],
#                                         Hourly_Sq=(temp[,'Hour'])^2)

#    gam_1 <- gam(Glucose~s(Hourly, bs='cr', k=12) + s(Daily, bs='ps', k=24),
#                 data=matrix_gam, family=gaussian)

#    datas <- data.table::data.table(value=gam_1$fitted.values,
#                                    Day_Time=temp[,'Day_Time'])

#    datas[, TP := rep(month, nrow(datas))][, Model := rep('Simple', nrow(datas))]


#    ## refit with using same smoothed function to both variable f(x1) x f(x2) or f(x1, x2)
#    gam_2 <-  gam(Glucose~s(Hourly, Daily),
#                  data=matrix_gam, family=gaussian)
#
#    gam_3 <- gam(Glucose~s(Hourly, Hourly_Sq, Daily),
#                 data=matrix_gam, family=gaussian)

#    summary(gam_3)

#    ## refit with tensor product interaction
#    gam_tensor<- gam(Glucose~te(Hourly, Daily,
#                                k=c(12, 24),
#                                bs=c('cr', 'ps')),
#                     data=matrix_gam, family=gaussian)

    #return(datas)


  #})

  ##plot(cgm.spline.list[[1]]$gam)
  ##names(cgm.spline.list) <- names(cgm.data.list)
  ##spline.plot.data <- lapply(names(cgm.spline.list), function(month){
  ##  original <-  temp <- cgm.data.list[[month]][cgm.data.list[[month]]$Event_Type=='EGV', ]
  ##
  ##  temp <- cgm.spline.list[[month]]
  ##  plot.data <- plot(cgm.spline.list[[month]]$gam, residuals=TRUE)
  ##
  ##  keep.data <- data.frame(Day_Time=plot.data[[1]]$x,
  ##                          smoothed.values=plot.data[[1]]$fit,
  ##                          TP=month,
  ##                          stringsAsFactors=FALSE)
  ##  return(keep.data)
  ##
  ##})
  ##spline.plot.data <- do.call(rbind, spline.plot.data)
  ##spline.plot.data <- merge(spline.plot.data, df.info, by='TP', all=TRUE)

  #spline.plot.data <- do.call(rbind, cgm.spline.list)
  #spline.plot.data <- merge(spline.plot.data, df.info, by='TP', all=TRUE)
  #spline.plot.data$INFO <- factor(spline.plot.data$INFO, levels=levels(cgm.data$INFO))

  stacked.ts <- ggplot(data=cgm.data[cgm.data$Event_Type=='EGV', ], aes(x=Day_Time, y=Glucose_Value_mg_dL)) +
    geom_point(size=1) +
    geom_point(data=cgm.data[cgm.data$Event_Type=='Calibration', ], aes(x=Day_Time, y=Glucose_Value_mg_dL), colour='red', size=1) +
    facet_wrap(.~INFO, ncol=1) +
    geom_rect(data=informative.cgm.meta, aes(ymin=0, ymax=as.numeric(`Urgent Low`), xmin=-Inf, xmax=Inf), fill='#FF0000', alpha=0.1, inherit.aes=FALSE) +
    geom_rect(data=informative.cgm.meta, aes(ymin=as.numeric(`Urgent Low`), ymax=as.numeric(Low), xmin=-Inf, xmax=Inf), fill='#FFA500', alpha=0.1, inherit.aes=FALSE) +
    geom_rect(data=informative.cgm.meta, aes(ymin=as.numeric(High), ymax=Inf, xmin=-Inf, xmax=Inf), fill='#FFFF00', alpha=0.1, inherit.aes=FALSE) +
    # geom_line(data=spline.plot.data, aes(x=Day_Time, y=value), inherit.aes=FALSE, colour='blue') +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
          panel.background=element_blank(), axis.line=element_line(colour='black'),
          axis.title=element_text(size=30),
          axis.text=element_text(size=30),
          strip.text.x=element_text(size=30),
          plot.title=element_text(size=40, hjust=0.5)) +
    ylab('Glucose Value mg/dL') +
    xlab('Day') +
    ggtitle('Daily Trends Across Time Points') +
    scale_x_continuous(breaks=as.numeric(names(table(cgm.data$Day))))

  if(!is.null(file.name)){
    print({
      png(filename=file.name,
          width=1800, height=1000)
      stacked.ts
    })
    dev.off()
  }

  return(stacked.ts)
}

utils::globalVariables(c('cgm.data', 'informative.cgm.meta',
                         'file.name', 'png', 'dev.off'))
