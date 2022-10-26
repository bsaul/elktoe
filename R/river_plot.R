#' Prints plots of the river
#'
#' @param y name of variable in river data to plot
#' @return a plot
#' @export

river_plot <- function(y){
  rp <- ggplot(river[!is.na(river[, y]), ], aes_string(x = 'date_',
                                                       y = y,
                                                       group = 'site',
                                                       shape = 'river',
                                                       color = 'river',
                                                       linetype = 'river')) +
    geom_point() + geom_line() +
#     scale_x_date(breaks = date_breaks("4 weeks"),
#                  labels = date_format("%y-%b-%d")) +
    geom_vline(xintercept = as.numeric(as.Date(c('2013-04-02',
                                             '2013-09-19',
                                             '2014-05-06'))),
               linetype = c(4, 3, 3),
               size = c(.5, .5, 1)) +
    scale_color_brewer(palette = 'Dark2') +

    xlab('') +
    theme_classic() +
    theme(legend.key = element_blank(), legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  return(rp)
}

