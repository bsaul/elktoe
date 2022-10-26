#' Prints plots of the mussels over time
#'
#' @param species name of species to plot
#' @param y name of variable in mussels to plot
#' @return a plot
#' @export

mussel_plot <- function(species, y){
  ggplot(mussels[mussels$species == species, ],
         aes_string(y = y, x = 'date',
                    group = 'id',
                    color = 'river',
                    shape = 'site')) +
    geom_point() +
    geom_line()  +
    scale_color_brewer(palette = 'Set2') +
    scale_shape_manual(guide = 'none', values = c(1,2,3,4,5,6)) +
    scale_x_date(breaks = date_breaks("4 weeks"),
                 labels = date_format("%b-%d")) +
    xlab('') +
    theme_classic() +
    theme(legend.key = element_blank(), legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}
