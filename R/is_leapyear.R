#' Check if a given year is a leap year
#'
#' This function determines whether the provided year is a leap year
#' based on the rules of the Gregorian calendar.
#'
#' @param year A numeric or numeric-string scalar input representing a year.
#'
#' @return TRUE if it is a leap year, FALSE otherwise.
#'
#' @examples
#' is_leapyear(2000) # TRUE
#' is_leapyear("1992") # TRUE
#' is_leapyear("1900") # FALSE
#'
#' @export
is_leapyear <- function(year) {
    if (length(year) != 1) stop("Input must be a single value.")

    if (is.character(year)) {
        if (!grepl("^\\d+$", year)) stop("Input must be a numeric string.")
        year <- as.numeric(year)
    }

    if (is.null(year) || is.na(year)) stop("Year cannot be NULL or NA.")
    if (!is.numeric(year)) stop("Input must be numeric.")
    if (year %% 1 != 0) stop("Year must be an integer.")
    if (year <= 0) stop("Year must be a positive integer.")

    if (year %% 400 == 0) {
        return(TRUE)
    } else if (year %% 100 == 0) {
        return(FALSE)
    } else if (year %% 4 == 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
