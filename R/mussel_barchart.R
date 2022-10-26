#' Prints barcharts
#'
#' @param y name of variable in river data to plot
#' @return a plot
#' @export

mussel_barchart <- function(data, x, y, ylabtext){
  rp <- ggplot(data, aes_string(x = x, y = y, fill = 'river')) +
    geom_bar(stat='identity') +
    scale_fill_brewer(guide = 'none', palette = 'Set2') +
    xlab('') + ylab(ylabtext) +
    theme_classic() +
    scale_y_continuous(limits = c(0, 1)) +
    theme(legend.key = element_blank(), legend.title = element_blank())
  print(rp)
}
