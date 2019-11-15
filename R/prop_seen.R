#' Calculate proportion of individuals seen for specific time
#'
#' @param DT        Data.table with required data
#' @param category  Column that inculdes categories for which one wants to calculate the proportion seen (as.character)
#' @param ID_unique Column that inculdes Unique ID by which one wants to calculte the days seen (as.character)
#' @param time_seen Column that inculdes time seen (as.character)
#'
#' @return          Data.table with category, time_step and proportion seen
#' @export
#'
#' @examples
#' require(data.table)
#' require(foreach)
#' require(ggplot2)
#'
#' dt = data.table(category  = c(rep('M', 100), rep('F', 100)),
#'                 ID_unique = as.character(c(1:200)),
#'                 time_seen = sample(x = c(1:40), size = 200, replace = TRUE) )
#'
#' ds = prop_seen(dt, 'category', 'ID_unique', 'time_seen')
#'
#' ggplot(data = ds, aes(x = time_step, y = prop_seen, col = category)) +
#'   scale_y_continuous(limits = c(0, 1)) +
#'   geom_line(size = 1.5) +
#'   scale_color_brewer(name = 'Category', palette = 'Dark2') +
#'   labs(x = 'Days seen', y = 'Proportion of individuals') +
#'   theme_classic(base_size = 24)

prop_seen = function(DT, category, ID_unique, time_seen){

  setnames(DT, c(category, ID_unique, time_seen), c('category', 'ID_unique', 'time_seen'))

  # loop calculating for each time step which ID was still seen
  o = foreach(k = unique(DT$category), .combine = 'rbind') %do% {
    # subset category
    ds = DT[category == k]

    # open matrix
    dt = data.table(time_step = seq(0, max(ds$time_seen) %>% as.integer + 1, by = 0.1) )

    foreach(i = ds$ID_unique) %do% {

      dt[, as.character(i) := ifelse(time_step < ds[ID_unique == i, time_seen], 1, 0)]

    }

    # calculate days proportion of days seen
    dt[, sum       := rowSums(.SD), .SDcols = grep('t', names(dt), invert = TRUE)]
    dt[, prop_seen := sum/max(sum)]

    dt[, category := k]
    dt = dt[, .(category, time_step, prop_seen)]
    dt

  }

  setnames(DT, c('category', 'ID_unique', 'time_seen'), c(category, ID_unique, time_seen))
  o

}
