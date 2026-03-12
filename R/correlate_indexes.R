#' Compute correlation between test indexes
#'
#' Given raw data, computes selected type of pairwise correlations ("Pearson"
#' by default) of all selected indexes and plots them. Optionally, the user
#' can specify an "ideal" correlation matrix of the same variables for
#' comparison purposes.
#'
#' @param d0 Input data.
#' @param v Named character vector of variables to be included.
#' @param ideal Optionally a matrix of ideal values. Write `NULL` for the default
#'   or `NA` if no ideal matrix should be plotted.
#' @param ... Other variables for [cor()]
#'
#' @returns A list containing:
#' \describe{
#'   \item{R_observed}{Correlation matrix of observed data}
#'   \item{R_ideal}{Correlation matrix of the "ideal" pattern}
#' }
#'
#' @export
correlate_indexes <- function(
    d0,
    v = setNames(
      c("moca_7","mmse_7","moca_cloc","cloc","vf_s","vf_k",
        "moca_5words","mmse_3words","moca_cube","mmse_pent",
        "moca_abs","moca_anim"),
      c("MoCA Sevens","MMSE Sevens","MoCA Clock","Clock Drawing",
        "VF S","VF K","MoCA 5 words","MMSE 3 words",
        "MoCA Cube","MMSE Pent.","MoCA Abs.","MoCA An.")
    ),
    ideal = NULL,
    ...
){

  make_ideal <- function(v){

    R <- diag(length(v))
    dimnames(R) <- list(names(v), names(v))

    doms <- list(3:6, c(7,8), c(9,10), c(11,12))

    for (d in doms){
      if(length(d)==2){
        R[d[1],d[2]] <- R[d[2],d[1]] <- .56
      } else {
        R[d,d][lower.tri(R[d,d])] <- .56
        R[d,d][upper.tri(R[d,d])] <- .56
      }
    }

    pairs_same <- list(c(1,2), c(3,4), c(5,6))
    for (p in pairs_same){
      R[p[1],p[2]] <- R[p[2],p[1]] <- .72
    }

    R
  }

  if (is.null(ideal)){
    ideal <- TRUE
    R_ideal <- make_ideal(v)
  } else if (is.na(ideal)){
    ideal <- FALSE
    R_ideal <- NULL
  } else {
    R_ideal <- ideal
    ideal <- TRUE
  }

  dat <- d0 |>
    dplyr::select(tidyselect::all_of(v)) |>
    rlang::set_names(names(v))

  R_obs <- cor(dat, ...)

  list(
    R_observed = R_obs,
    R_ideal = R_ideal
  )
}
