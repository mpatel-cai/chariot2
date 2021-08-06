map_to_value <-
  function(x,
           map_assignment,
           other) {


    stopifnot(is.character(x))

    x <- stringr::str_replace_na(x)

    levels        <- names(map_assignment)
    names(levels) <- map_assignment

    # Any unaccounted for x values?
    unique_x <- unique(x)
    missing_levels <-
      unique_x[!(unique_x %in% levels)]

    if (length(missing_levels)>0) {
      cli::cli_alert_warning("{length(missing_levels)} value{?s} not mapped: {missing_levels}. Mapping to `other` value '{other}'.")
      names(missing_levels) <- rep(other, length(missing_levels))

      levels <-
        c(levels,
          missing_levels)

    } else {

      cli::cli_alert_success("All {length(unique_x)} unique value{?s} mapped.")
    }

    suppressWarnings(
      forcats::fct_recode(factor(x), !!!levels) %>%
        as.character())
  }
