#' Time Series Plot
#'
#' Generate Time Series Plot
#' @param cgm.data A dataframe of CGM data formatted by format_dexcom()
#' @param informative.cgm.meta A dataframe of CGM meta data formatted by format_dexcom()
#' @param file.name The name of the output file
#' @param month The corresponding month to plot
#' @param overlay a logical value (TRUE of FALSE). If TRUE, the insulin and/or carbs intake will be overlay on top of time series plot
#' @export
#'
monthly_time_series_plot <- function(cgm.data, informative.cgm.meta,
                                     file.name=NULL, month, overlay=FALSE){

  stopifnot(is.character(month))
  cgm.data <- cgm.data[cgm.data$TP==month, ]
  informative.cgm.meta <- informative.cgm.meta[informative.cgm.meta$TP==month, ]

  bp_trend <- ggplot(data=cgm.data, aes(x=Day_Time, y=Glucose_Value_mg_dL)) +
    geom_point(size=2) +
    geom_point(data=cgm.data[cgm.data$Event_Type=='Calibration', ],
               aes(x=Day_Time, y=Glucose_Value_mg_dL), colour='red', size=2) +
    geom_rect(data=informative.cgm.meta, aes(ymin=0, ymax=as.numeric(`Urgent Low`), xmin=-Inf, xmax=Inf),
              fill='#FF0000', alpha=0.1, inherit.aes=FALSE) +
    geom_rect(data=informative.cgm.meta, aes(ymin=as.numeric(`Urgent Low`), ymax=as.numeric(Low), xmin=-Inf, xmax=Inf),
              fill='#FFA500', alpha=0.1, inherit.aes=FALSE) +
    geom_rect(data=informative.cgm.meta, aes(ymin=as.numeric(High), ymax=Inf, xmin=-Inf, xmax=Inf),
              fill='#FFFF00', alpha=0.1, inherit.aes=FALSE) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
          panel.background=element_blank(), axis.line=element_line(colour='black'),
          axis.title.y=element_text(size=30),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=30),
          axis.text.x=element_blank(),
          strip.text.x=element_text(size=30),
          plot.title=element_text(size=40, hjust=0.5)) +
    ylab('Glucose value mg/dL') +
    ggtitle(paste0('Daily Trends Across ', month)) +
    scale_x_continuous(breaks=as.numeric(names(table(cgm.data$Day))),
                       limits=c(min(as.numeric(names(table(cgm.data$Day)))),
                                max(as.numeric(names(table(cgm.data$Day)))))) +
    scale_y_continuous(breaks=c(0, 100, 200, 300, 400), limits=c(0, 400))

  if(overlay==TRUE){
    bp_trend <- bp_trend +
      geom_segment(data=cgm.data[cgm.data$Event_Type=='Insulin',],
                   aes(x=Day_Time, xend=Day_Time, y=0, yend=400), colour='red') +
      geom_segment(data=cgm.data[cgm.data$Event_Type=='Carbs',],
                   aes(x=Day_Time, xend=Day_Time, y=0, yend=400), colour='blue')
  }

  insulin_trend <- ggplot(data=cgm.data, aes(x=Day_Time)) +
    geom_point(aes(y=Insulin_Value_u, colour='Insulin'), size=2) +
    geom_point(aes(y=Carb_Value_grams/5, colour='Carbs'), size=2, shape=17) +
    scale_y_continuous(sec.axis=sec_axis(~.*5, name='Carbs Value (grams)')) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
          panel.background=element_blank(), axis.line=element_line(colour='black'),
          axis.title=element_text(size=25),
          axis.text=element_text(size=25),
          strip.text.x=element_text(size=30),
          plot.title=element_text(size=40, hjust=0.5),
          legend.position='bottom',
          legend.title=element_blank(),
          legend.text=element_text(size=25)) +
    scale_colour_manual(values=c('blue', 'red')) +
    labs(x='Day',
         y='Insulin value u') +
    scale_x_continuous(breaks=as.numeric(names(table(cgm.data$Day))),
                       limits=c(min(as.numeric(names(table(cgm.data$Day)))),
                                max(as.numeric(names(table(cgm.data$Day))))))

  if(!is.null(file.name)){
    print({
      png(filename=file.name,
          width=1800, height=1000)
      ggpubr::ggarrange(bp_trend, insulin_trend, nrow=2, heights=c(3,1), align='v')
    })
    dev.off()
  }


  return(ggpubr::ggarrange(bp_trend, insulin_trend, nrow=2, heights=c(3,1), align='v'))


}


utils::globalVariables(c('Insulin_Value_u', 'Carb_Value_grams'))

