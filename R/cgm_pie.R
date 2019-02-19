#' Pie Chart
#'
#' Generate pie chart for within, below, and above range percentage
#' @param cgm.data: A dataframe of CGM data formatted by format_dexcom() for a patient, containing all timepoints
#' @param file.name: The name of the output file
#' @export
#' @import ggplot2 dplyr
#'

cgm_pie <- function(cgm.data, file.name=NULL){
  ## pie chart for target
  cgm.months <- split(cgm.data, f=cgm.data$TP)
  cgms <- c(list(cgm.data), cgm.months)
  names(cgms) <- c('Overall', names(cgm.months))
  cgm.pie.df.list <- lapply(cgms, function(cgm){
    temp <- data.frame(table(cgm[cgm$Event_Type=='EGV', ]$TARGET))
    temp$Var1 <- factor(temp$Var1, levels=c('High', 'On Target', 'Low', 'Urgent Low'))
    temp$Label <- paste0(temp$Var1, '\n', round(100*(temp$Freq/sum(temp$Freq)), 2), '%')

    temp <- temp %>%
      mutate(end = 2 * pi * cumsum(Freq)/sum(Freq),
             start = lag(end, default = 0),
             middle = 0.5 * (start + end),
             hjust = ifelse(middle > pi, 1, 0),
             vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))
  })

  names(cgm.pie.df.list) <- c('Overall', names(cgm.months))

  cgm.pie.list <- lapply(names(cgm.pie.df.list), function(month){
    cgm <- cgm.pie.df.list[[month]]
    pie.chart <- ggplot(cgm) +
      scale_fill_manual(values=c('#FFFA72', '#228B22', '#FFA500', '#FF0000')) +
      ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                                start = start, end = end, fill = Var1)) +
      geom_text(aes(x=1.05*sin(middle), y=1.05*cos(middle), label=Label,
                    hjust=hjust, vjust=vjust), size=10) +
      coord_fixed() +
      scale_x_continuous(limits=c(-1.75, 1.75),  # Adjust so labels are not cut off
                         name = "", breaks = NULL, labels = NULL) +
      scale_y_continuous(limits=c(-1.75, 1.75),      # Adjust so labels are not cut off
                         name="", breaks = NULL, labels = NULL) +
      theme(legend.position='none',
            panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
            panel.background=element_blank(), axis.line=element_line(colour='black'),
            plot.title=element_text(size=40, hjust=0.5)) +
      ggtitle(month)
    return(pie.chart)
  })

  if(!is.null(file.name)){
    print({
      png(filename=file.name,
          width=1800, height=1400)
      ggpubr::ggarrange(cgm.pie.list[[1]],
                        cgm.pie.list[[2]],
                        cgm.pie.list[[3]],
                        cgm.pie.list[[4]],
                        nrow=2, ncol=2, align='v')
    })
    dev.off()
  }

  return(ggpubr::ggarrange(cgm.pie.list[[1]],
                           cgm.pie.list[[2]],
                           cgm.pie.list[[3]],
                           cgm.pie.list[[4]],
                           nrow=2, ncol=2, align='v'))

}
