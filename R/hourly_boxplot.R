#' Hourly Boxplot
#'
#' Generate hourly boxplot
#' @param cgm.data A dataframe of CGM data formatted by format_dexcom() for a patient, containing all timepoints
#' @param informative.cgm.meta A dataframe of CGM meta data formatted by format_dexcom()
#' @param file.name The name of the output file
#' @export

hourly_boxplot <- function(cgm.data, informative.cgm.meta,
                           file.name=NULL){

  ### percent of within target range for each hourly
  ## for each hour, get the highest value, per month
  cgm.data.month <- split(cgm.data, f=cgm.data$TP)
  cgm.data.month.hour <- lapply(cgm.data.month, function(x) split(x, f=x$Hour))

  cgm.mh.info <- lapply(names(cgm.data.month.hour), function(month){
    lapply(names(cgm.data.month.hour[[month]]), function(hour){

      temp <- cgm.data.month.hour[[month]][[hour]]
      temp <- temp[temp$Event_Type=='EGV', ]

      per.in.range <- paste0(round((100*sum(temp$TARGET=='On Target',na.rm=TRUE)/sum(!is.na(temp$TARGET))), 2), '%')
      df.info <- data.frame(TP=month,
                            Hour=hour,
                            text=per.in.range,
                            YMAX=max(temp$Glucose_Value_mg_dL, na.rm=TRUE),
                            stringsAsFactors=FALSE)
      return(df.info)

    })
  })

  df.text <- lapply(cgm.mh.info, function(x){
    do.call(rbind, x)
  })

  df.text <- do.call(rbind, df.text)
  df.text$TP <- factor(df.text$TP, levels=levels(cgm.data$TP))

  hr.bp <- ggplot(data=cgm.data[cgm.data$Event_Type=='EGV', ], aes(x=factor(Hour), y=Glucose_Value_mg_dL)) +
    geom_boxplot(outlier.size=2.5) +
    facet_wrap(.~TP, ncol=1) +
    geom_rect(data=informative.cgm.meta, aes(ymin=0, ymax=as.numeric(`Urgent Low`), xmin=-Inf, xmax=Inf), fill='#FF0000', alpha=0.1, inherit.aes=FALSE) +
    geom_rect(data=informative.cgm.meta, aes(ymin=as.numeric(`Urgent Low`), ymax=as.numeric(Low), xmin=-Inf, xmax=Inf), fill='#FFA500', alpha=0.1, inherit.aes=FALSE) +
    geom_rect(data=informative.cgm.meta, aes(ymin=as.numeric(High), ymax=Inf, xmin=-Inf, xmax=Inf), fill='#FFFF00', alpha=0.1, inherit.aes=FALSE) +
    geom_text(data=df.text, aes(x=factor(Hour), y=YMAX, label=text), vjust=-1, inherit.aes=FALSE) +
    theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
          panel.background=element_blank(), axis.line=element_line(colour='black'),
          axis.title=element_text(size=30),
          axis.text=element_text(size=30),
          strip.text.x=element_text(size=30),
          plot.title=element_text(size=40, hjust=0.5)) +
    ylim(0, (max(cgm.data$Glucose_Value_mg_dL, na.rm=TRUE)+25))+
    ylab('Glucose Value mg/dL') +
    xlab('Hour of the Day') +
    ggtitle('Hourly Trends Across Time Points')

  if(!is.null(file.name)){
    print({
      png(filename=file.name,
          width=1800, height=1000)
      hr.bp
    })
    dev.off()
  }

  return(hr.bp)
}

utils::globalVariables(c('Hour', 'Glucose_Value_mg_dL', 'Urgent Low',
                         'Low', 'High', 'YMAX', 'text','Day_Time',
                         'png', 'dev.off'))

