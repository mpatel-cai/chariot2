#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param map_assignment PARAM_DESCRIPTION
#' @param other PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_replace_na}}
#'  \code{\link[cli]{cli_alert}}
#'  \code{\link[forcats]{fct_recode}}
#' @rdname map_to_value
#' @export 
#' @importFrom stringr str_replace_na
#' @importFrom cli cli_alert_warning cli_alert_success
#' @importFrom forcats fct_recode
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
