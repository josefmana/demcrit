#' Import REDCap data.
#'
#' Using data path and scoring information,
#' this function imports the REDCap data in
#' a suitable format. The function and is
#' supposed to be run within the larger
#' \code{prepare_data}.
#'
#' @param paths Path to data. Should be generated
#' by \code{data_paths}.
#'
#' @returns A tibbles with appropriate REDCap data.
#'
#' @examples
#' \dontrun{
#' p    <- data_paths('data-raw')
#' scor <- readr::read_delim(p[4], delim = ";")
#' data <- import_redcap_data(p[2])
#' }
#'
#' @export
import_redcap_data <- function(path, scoring) {
  df <-
    read_csv(path, show_col_types = F) |>
    select(-contains('dbs'), -contains('post')) |>
    filter(grepl('screening', redcap_event_name)) |>
    rename(
      id             = study_id,
      pd_dur         = rok_vzniku_pn,
      ledd           = levodopa_equivalent,
      birth          = dob,
      neuropsy_years = datum_neuropsy_23afdc,
      drsii          = drsii_total,
      nart           = nart_7fd846,
      tol            = tol_anderson
    )
  # Rename MDS-UPDRS III levodopa test items:
  mot_its <- names(df)[grepl('mdsupdrs', names(df))]
  for (i in mot_its) {
    df[ , gsub('_', '', gsub('_ldopatest', '', i))] <- df[ , i]
    df[ , i] <- NULL
  }
  # Extract correct FAQ scores:
  faq_its <- unlist(strsplit(with(scoring, item[scale == 'faq']), ','))
  for(i in faq_its) {
    df[ , paste0('faq_',i)] <- NA
    for (j in seq_len(nrow(df))) {
      if (is.na(df[j, paste0('faq_uvod_',i)])) next
      else if (df[j, paste0('faq_uvod_',i)] == 1) df[j, paste0('faq_',i)] <- df[j , paste0('faq_vykon_',i)] # The patient evaluated an activity directly.
      else if (df[j, paste0('faq_uvod_',i)] == 2) df[j, paste0('faq_',i)] <- df[j , paste0('faq_nikdy_',i)] # The patient evaluated an activity indirectly.
    }
  }
  # Reverse item scores where applicable:
  with(
    scoring,
    for (i in scale[complete.cases(rev)]) {
      for (j in unlist(strsplit(rev[scale == i], ','))) {
        df[ , paste0(i,'_',j)] <<- (max[scale == i] + min[scale == i]) - df[ , paste0(i,'_',j)]
      }
    }
  )
  # Check whether MoCA verbal fluency and vf_k are the same:
  vf_fail <-
    df |>
    filter(vf_k != moca_fluence_k) |>
    select(id, neuropsy_years, vf_k, moca_fluence_k)
  stop <- F
  if (nrow(vf_fail) > 0) {
    stop <- T
    cat('\nSee the problematic cases below:\n\n')
    print(vf_fail)
  }
  if(stop) cat('
  There are some incongruities in verbal fluency data between MoCA and Level II.
  Using the Level II data in these cases to keep it consistent with the rest of data.\n\n'
  )
  #if(stop) stop('
  #There are some incongruities in verbal fluency data between MoCA and Level II.
  #Check the data printed above to locate these inconsistencies and repair them.'
  #)
  df |>
    select(-all_of( starts_with( paste0('faq_', c('fill','uvod','vykon','nikdy','score'))))) |>
    mutate(
      faq      = rowSums(across(starts_with('faq'))),
      bdi      = rowSums(across(starts_with('bdi'))),
      stai_1   = rowSums(across(starts_with('staix1'))),
      stai_2   = rowSums(across(starts_with('staix2'))),
      pd_dur   = year(as.Date(neuropsy_years))-pd_dur,
      updrsiii_off = rowSums(across(all_of(paste0('mdsupdrs3', strsplit(pull(scoring[scoring$scale == 'updrs_iii', 'item']), ',')[[1]])))),
      updrsiii_on  = rowSums(across(all_of(paste0('mdsupdrs3', strsplit(pull(scoring[scoring$scale == 'updrs_iii', 'item']), ',')[[1]], 'on')))),
      age_lvl2 = time_length(
        difftime(as.Date(neuropsy_years), as.Date(birth)),
        'years'
      ),
      sex = factor(
        sex,
        levels  = 0:1,
        labels  = c('female', 'male'),
        ordered = F
      ),
      type_pd = factor(
        type_pd,
        levels  = 1:2,
        labels  = c('tremor-dominant', 'akinetic-rigid'),
        ordered = F
      ),
      asym_park = factor(
        asym_park,
        levels  = 1:2,
        labels  = c('right', 'left'),
        ordered = F
      ),
      moca_cloc   = rowSums(across(starts_with('moca_clock'))),
      moca_anim = rowSums(across(starts_with('moca_naming'))),
      moca_abs  = rowSums(across(starts_with('moca_abstraction'))),
      moca_7raw = rowSums(across(starts_with('moca_substr'))),
      moca_7    = case_when(
        moca_7raw == 0        ~ 0,
        moca_7raw == 1        ~ 1,
        moca_7raw %in% c(2,3) ~ 2,
        moca_7raw %in% c(4,5) ~ 3
      )
    ) |>
    rename(
      #vf_k        = moca_fluence_k,
      moca_5words = moca_recall_sum,
      moca_total  = moca_score,
      smoca_total = s_moca_score
    ) |>
    select(
      birth, id, age_lvl2, sex, type_pd, hy_stage, pd_dur, asym_park, ledd,
      updrsiii_off, updrsiii_on,
      drsii, mmse, nart,
      moca_cube, moca_7, vf_k, moca_5words, moca_anim, moca_abs, moca_cloc, starts_with('moca_clock'),
      moca_total, smoca_total,
      starts_with('faq'), bdi, stai_1, stai_2, gds_15,
      lns, ds_b, corsi_b, tmt_a, pst_d,
      tol, tmt_b, pst_w, pst_c, cf,
      sim, bnt_60,
      ravlt_irs, ravlt_b, ravlt_6, ravlt_30, ravlt_drec50, ravlt_drec15, bvmt_irs, bvmt_30, bvmt_drec,
      jol, clox_i,
      gp_r, gp_l
    )
}
