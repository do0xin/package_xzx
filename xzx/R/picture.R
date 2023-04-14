#'draw pictures in batches with facset_grid() and
#'order them according to "sequ"
#'
#'@param x A datatable,includes cols as follows
#'"type":for facet
#'"year":for grouping data
#'"time":raw time
#'"date":processed "time" with update(),for x-axis
#'"value":for y-axis
#'"sequ":for ordering
#'@export

picture <- function(x){
  x[, .(line_pic = (ggplot(.SD) +
                               facet_grid(.~type,scales = "free")+
                               geom_line(size = 1.5, aes(x = date, group = year, y = value, colour = year))+ scale_color_manual(values=cols)+
                               scale_x_date(breaks=date_breaks("2 months"),labels=date_format("%b"))+
                               theme_classic() +
                               labs(x = NULL, y = NULL, title = NULL) +
                               theme(
                                 strip.text.x = element_text(size = rel(2)),
                                 strip.background = element_rect(colour = "white",fill = "#dadada"),
                                 axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                                 axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
                                 axis.line = element_line(size = 1.5,linetype = 1),
                                 axis.text = element_text(size = rel(1)),
                                 axis.ticks.length=unit(-0.4, "cm"),
                                 axis.ticks = element_line(size = 1.5),
                                 legend.title = element_blank(),
                                 legend.position = "bottom",
                                 legend.text = element_text(size = rel(1)),
                                 legend.key = element_rect(size = rel(1), colour = "white", fill = "white")
                               ))%>% list()), keyby = .(sequ)]
}
