#' Check ranges of included data.
#'
#' This checks data ranges of data for the
#' analysis.
#'
#' @param d A data set prepared within the
#' \code{prepare_data} function.
#'
#' @returns Optionally a message with data that
#' are out of range and indicator to stop the
#' analysis.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' data <- prepare_data(p)
#' }
check_ranges <- function(d) {
  # Adding zero to a sequence of integers:
  seq0 <- function(x) c(0, seq_len(x))
  mistakes <- list(
    updrsiii_off       = subset(d, !(updrsiii_off %in% seq0(132))),
    updrsiii_on        = subset(d, !(updrsiii_on %in% seq0(132))),
    drsii              = subset(d, !(drsii %in% seq0(144))),
    nart               = subset(d, !(nart %in% seq0(50))),
    moca_cube          = subset(d, !(moca_cube %in% seq0(1))),
    moca_7             = subset(d, !(moca_7 %in% seq0(3))),
    moca_5words        = subset(d, !(moca_5words %in% seq0(5))),
    moca_anim          = subset(d, !(moca_anim %in% seq0(3))),
    moca_abs           = subset(d, !(moca_abs %in% seq0(2))),
    moca_clock_contour = subset(d, !(moca_clock_contour %in% seq0(1))),
    moca_clock_digits  = subset(d, !(moca_clock_digits %in% seq0(1))),
    moca_clock_hands   = subset(d, !(moca_clock_hands %in% seq0(1))),
    moca_total         = subset(d, !(moca_total %in% seq0(30))),
    smoca_total        = subset(d, !(smoca_total %in% seq0(16))),
    faq                = subset(d, !(faq %in% seq0(30))),
    bdi                = subset(d, !(bdi %in% seq0(63))),
    stai_1             = subset(d, !(stai_1 %in% 20:80)),
    stai_2             = subset(d, !(stai_2 %in% 20:80)),
    gds_15             = subset(d, !(gds_15 %in% seq0(15))),
    lns                = subset(d, !(lns %in% seq0(21))),
    #ds_f               = subset(d, !(ds_f %in% seq0(16))),
    ds_b               = subset(d, !(ds_b %in% seq0(14))),
    #corsi_f            = subset(d, !(corsi_f %in% seq0(16))),
    corsi_b            = subset(d, !(corsi_b %in% seq0(16))),
    tol                = subset(d, !(tol %in% seq0(108))),
    sim                = subset(d, !(sim %in% seq0(33))),
    bnt_60             = subset(d, !(bnt_60 %in% seq0(60))),
    ravlt_irs          = subset(d, !(ravlt_irs %in% seq0(75))),
    ravlt_b            = subset(d, !(ravlt_b %in% seq0(15))),
    ravlt_6            = subset(d, !(ravlt_6 %in% seq0(15))),
    ravlt_30           = subset(d, !(ravlt_30 %in% seq0(15))),
    ravlt_drec50       = subset(d, !(ravlt_drec50 %in% seq0(50))),
    ravlt_drec15       = subset(d, !(ravlt_drec15 %in% seq0(15))),
    bvmt_irs           = subset(d, !(bvmt_irs %in% seq0(36))),
    bvmt_30            = subset(d, !(bvmt_30 %in% seq0(12))),
    bvmt_drec          = subset(d, !(bvmt_drec %in% seq0(6))),
    jol                = subset(d, !(jol %in% seq0(30))),
    clox_i             = subset(d, !(clox_i %in% seq0(15))) # Should it be used?
  )
  zerotrunc <- c('vf_k', 'vf_s', 'tmt_a', 'pst_d', 'tmt_b', 'pst_w', 'pst_c', 'cf', 'gp_r', 'gp_l')
  for (i in zerotrunc) mistakes[[i]] <- subset(d, get(i) < 0)
  for (i in paste0('faq_',seq_len(10))) mistakes[[i]] <- subset(d, !(get(i) %in% seq0(3)))
  stop <- F
  for (i in names(mistakes)) {
    mistakes[[i]] <-
      mistakes[[i]] |>
      select(all_of(c('id', 'surname', 'firstname', 'assdate', i))) |>
      filter(!is.na(get(i)))
    if(nrow(mistakes[[i]]) > 0) {
      stop <- T
    }
  }
  # Return the results:
  list(stop = stop, typos = mistakes)
}
