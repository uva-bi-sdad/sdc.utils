#' Change acronyms (multiple capital letters) to title case
#' @param str List of colnames to convert.
#' @export
#' @examples
#' str_fix_acronyms(c("FIPSCode", "NoAcronymHere", "Multi-APNFlag"))
#' [1] "FipsCode"      "NoAcronymHere" "Multi-ApnFlag"
str_fix_acronyms <- function (str = c("FIPSCode", "NoAcronymHere", "Multi-APNFlag"))
{
  out <- list()
  for(i in 1:length(str)) {
    acro <- stringr::str_match(str[i], "([:upper:][:upper:]([:upper:]+)?)[:lower:]")[2]
    if (!is.na(acro)) {
      acro2 <- str_sub(acro, 1, nchar(acro) - 1)
      acro3 <- str_to_title(acro2)
      out <- c(out, str_replace(str[i], acro2, acro3))
    } else {
      out <- c(out, str[i])
    }
  }
  unlist(out)
}
