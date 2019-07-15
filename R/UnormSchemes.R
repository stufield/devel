## For PX-like Normalization scheme #4: 3 dilution groups, or #2: only one subset
# soma_groups is a list of vector somamer names, 
# indicating the groups that will be normalized separately.
# Whatever part of the menu is not in any of the soma_groups will go untouched 
# through all the stages. 
# (In general, none of the groups has (should have?) the Hybridization Control SOMAmers)
UnormInGroups <- function(adat,
                         calref,
                         popref,
                         soma_groups,
                         split_by = "PlateId",
                         hyb_by = "PlateId",
                         cutoff = 2.5, 
                         do_round = TRUE,
                         verbose = getOption("verbose")) {
    # split in groups, do the full standard scheme on each piece
    normed_list <- lapply(soma_groups, function(sg) {
        calref_sg <- calref[getSeqId(sg, trim.version = TRUE)]
        popref_sg <- popref %>% dplyr::filter(SeqId %in% getSeqId(sg, trim.version = TRUE))
        
        UnormOneGroup(dplyr::select(adat, c(getMeta(adat), sg)),
                      calref = calref_sg,
                      popref = popref_sg,
                      split_by = split_by,
                      hyb_by = hyb_by,
                      cutoff = cutoff,
                      do_round = do_round,
                      verbose = verbose) 
      }
      
    )
    
    # put together the pieces, overwriting the input data
    out_list <- list()
    # hyb (input) stage
    out_list$adats$hyb <- adat # easy
    # for each normalization stage (except for the income data)
    for (i in 2:length(normed_list[[1]]$adats)) {
      tmp <- out_list$adats[[i-1]] # copy a template to fill in (previous stage)
      # for each SOMAmer subset
      for (sgn in names(soma_groups)) {
        # find names of new metadata
        mdn <- setdiff(getMeta(normed_list[[sgn]]$adats[[i]]), getMeta(normed_list[[sgn]]$adats[[i-1]]))
        # rename
        new_mdn <- paste0(mdn,"_",sgn)
        # insert values in new adat
        tmp[,c(new_mdn, soma_groups[[sgn]])] <- normed_list[[sgn]]$adats[[i]][,c(mdn, soma_groups[[sgn]])]
        # we might need to update NormScales, too, in the anml stage... 
        # if it's not new, merge over the previous values (multiply, setting NA to 1)
        # can't think of a better way to check we might need to update NormScale here:
        if (names(normed_list[[sgn]]$adats)[i] == "anml") {
          norm_names <- getNormNames(normed_list[[sgn]]$adats[[i-1]])
          if (length(norm_names)>0) {
            new_norm_names <- paste0(norm_names, "_", sgn)
            normed_list[[sgn]]$adats[[i]][,norm_names] %<>% ifelse(is.na(.), 1, .)
            tmp[, new_norm_names] %<>% multiply_by(normed_list[[sgn]]$adats[[i]][,norm_names]) 
          }
        }
      }
      # fix attributes (broken, probably)
      out_list$adats[[i]] <- tmp %>% 
        createChildAttributes(out_list$adats[[i-1]], verbose = verbose)
    }
    out_list$adats %<>% set_names(names(normed_list[[1]]$adats))
    # fix the EffectSize data.frame, too
    # initialize to all NAs
    out_list$EffectSize <- as.data.frame(adat[, getAptamers(adat)]) 
    out_list$EffectSize[] <- NA
    # now put in the real values
    for (sgn in names(soma_groups)) {
      out_list$EffectSize[,soma_groups[[sgn]]] <- 
        normed_list[[sgn]]$EffectSize
    }
    # return
    out_list
}

## For PX-like Normalization scheme #1: Standard (also called by the others)
## urine normalization scheme, treating all SOMAmers as one group
## I am not sure, but let's assume we need calref and popref to have
## values for the same set of SOMAmers as adat has
UnormOneGroup <-   function(adat,
                            calref,
                            popref,
                            split_by = "PlateId",
                            hyb_by = "PlateId",
                            do_calibration = FALSE,
                            cutoff = 2.5,
                            do_round = TRUE,
                            verbose = getOption("verbose")) {
  ## reordering subfunction: uses a column nwRowOrderCol to rearrange
  ## the rows of the adat to their original positions
  reorderAdat <- function(x) {
    #adat <- adat[sort(adat$nwRowOrderCol),]
    dplyr::arrange(x, nwRowOrderCol)
  }
  
  apt_data <- getAptData(adat)
  out      <- list()
  out$hyb  <- adat
  
  # add a field to remember sample order ?
  out$hyb %<>% 
    tibble::rowid_to_column("nwRowOrderCol") %>% 
    createChildAttributes(adat, verbose=verbose)

  # 1 hyb normalize... assumed given here
  
  # 2 median normalize (internally) Calibrators and Buffers
  out$imed <- medianNormalize(out$hyb,
                              by = c(split_by, "SampleType"),
                              do_field  = "SampleType",
                              # do_regexp = "Calibrator|Buffer|Blank",
                              do_regexp = "Calibrator", # let's not median normalize blanks... (byt pltscale?)
                              apt.data  = apt_data,
                              do_round  = do_round,
                              verbose   = verbose) %>%
              createChildAttributes(out$hyb, verbose=verbose)
  
  # 3 plate normalization
  out$plt <- split(out$imed, out$imed[[split_by]]) %>%
    plateNormalize(., calref, do_round = do_round) %>%
    do.call(rbind, .) %>%
    reorderAdat() %>% 
    createChildAttributes(out$imed, verbose=verbose)

  # 4 optional calibration
  # calibrate.adats.list does not retain the original order (how would it know it?)
  if (do_calibration) {
    out$cal <- split(out$plt, out$plt[[split_by]]) %>%
      calibrate.adats(., ref = calref,
                      parent.adat = adat,
                      do_round = do_round,
                      verbose = verbose) %>%
      reorderAdat()
    tmp <- out$cal
  } else {
    tmp <- out$plt
  }
  
  # 5 ANML QC|Sample to reference
  tmp <- singleSampleANML(tmp,
                          pop_ref = popref,
                          do_field  = "SampleType",
                          do_regexp = "QC|Sample",
                          effect_cut = cutoff,  
                          do_round  = do_round,
                          verbose   = verbose)
  out$anml <- tmp$MedDat
  # remove artificial nwRowOrderCols
  out %<>% purrr::map(~dplyr::select(.x, -nwRowOrderCol))
  
  # return list of adats and Mahalanobis's distances
  list(adats=out, EffectSize=tmp$EffectSize)
}

## For PX-like Normalization scheme #3: one subset used in computations, applied to everything
# soma_subset is named list of only one element: a vector of SOMAmer names 
# (one of the entries in soma_groups, usually soma_groups$S1, but passed as soma_groups["S1"])
UnormCompSubset <- function(adat,
                            calref,
                            popref,
                            soma_subset,
                            split_by = "PlateId",
                            hyb_by = "PlateId",
                            cutoff = 2.5,
                            do_round = TRUE,
                            verbose = getOption("verbose")) {
  
  subset_n <- names(soma_subset)
  if (length(soma_subset) != 1 | is.null(subset_n)) {
    rlang::signal(
      "`soma_subset` should have only one (named) element: a vector of SOMAmer names",
      "error")
  }
  
  # normalize only the subset (we'll later apply the scale factor to everything)
  calref_ss <- calref[getSeqId(soma_subset[[1]], trim.version = TRUE)]
  popref_ss <- popref %>% dplyr::filter(SeqId %in% getSeqId(soma_subset[[1]], trim.version = TRUE))
  
  normed_tmp <-
    UnormInGroups(adat,
               calref = calref_ss,
               popref = popref_ss,
               soma_groups = soma_subset,
               split_by = "PlateId",
               hyb_by = "PlateId",
               cutoff = cutoff,
               do_round = TRUE,
               verbose = getOption("verbose")) 
  out <- normed_tmp
  # fix one by one
  # what needs to change
  snames <- getAptamers(adat)
  soma_chg <- setdiff(grep("^Hyb", snames, value = TRUE, invert = TRUE), 
          soma_subset[[1]])
  # hyb is fine
  # imed
  norm_n <- grep("^NormScale", names(normed_tmp$adats$imed)) # should be length 1
  out$adats$imed[,soma_chg] %<>% 
    multiply_by(normed_tmp$adats$imed[,norm_n]) 
  if (do_round) out$adats$imed[,soma_chg] %<>% round(1)
  # plt
  plt_n <- grep("^PlateScale_Scalar", names(normed_tmp$adats$plt)) # should be length 1
  out$adats$plt[,soma_chg] <-  
    multiply_by(out$adats$imed[,soma_chg], normed_tmp$adats$plt[,plt_n]) 
  if (do_round) out$adats$plt[,soma_chg] %<>% round(1)
  # anml
  # only change the QC|Sample
  sam_chg <- grep("QC|Sample", normed_tmp$adats$SampleType)
  out$adats$anml[sam_chg,soma_chg] <-  
    multiply_by(out$adats$plt[sam_chg,soma_chg], normed_tmp$adats$anml[sam_chg,norm_n]) 
  if (do_round) out$adats$anml[,soma_chg] %<>% round(1)
  # ANML_FractionUsed is allright. 
  # We keep the Effects for restricted subset only, too (the rest of the valueas are NA)
  out
}

