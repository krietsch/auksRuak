#' Get names of GPS points before and after
#'
#' Used to check if the point was realistic
#'
#' @param obs_id Observation ID in format Year_GPSID_GPSWaypoint
#' @param start  N of positions before
#' @param end    N of positions after
#'
#' @return       obs_id's to subset GPS points with
#' @export
#'
#' @examples
#' obs_id_check('18_2_221')

obs_id_check = function(obs_id, start = -3, end = 3){

  g_id = g_y = obs_id_g = NULL

  g_wp = strsplit(obs_id, '_')[[c(1,3)]] %>% as.numeric

  d = data.table(g_wp = seq(start, end, 1) + g_wp)
  d[, g_y := strsplit(obs_id, '_')[[c(1,1)]]]
  d[, g_id := strsplit(obs_id, '_')[[c(1,2)]]]
  d[, obs_id_g := paste0(g_y, '_', g_id, '_', g_wp)]

  d$obs_id_g

}
