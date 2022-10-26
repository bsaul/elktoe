#' Prints boxplots by river
#'
#' @param y name of variable in river data to plot
#' @return a plot
#' @export

mussel_boxplot <- function(y){
  rp <- ggplot(mussels_wide, aes_string(x = 'river', y = y, fill = 'river')) +
    geom_boxplot() +
    scale_fill_brewer(guide = 'none', palette = 'Set2') +
    xlab('') +
    theme_classic() +
    theme(legend.key = element_blank(), legend.title = element_blank()) +
    facet_wrap( ~ species)
  print(rp)
}

#' Prints boxplots by site
#'
#' @param y name of variable in river data to plot
#' @param species
#' @return a plot
#' @export

mussel_boxplot_site <- function(y, species){
  rp <- ggplot(subset(mussels_wide, species = species),
               aes_string(x = 'site', y = y, fill = 'river')) +
    geom_boxplot() +
    scale_fill_brewer(guide = 'none', palette = 'Set2') +
    xlab('') +
    theme_classic() +
    theme(legend.key = element_blank(), legend.title = element_blank())
    #facet_wrap( ~ river)
  print(rp)
}



