whichLeapday <- function(year) {
  if (!is.numeric(year)) {
    stop("You must specify a numerical input.")
  }
  mod4 <- year %% 4
  mod100 <- year %% 100
  mod400 <- year %% 400
  
  leapdayIndex <- ((mod4
                    == 0 & mod100 != 0) | mod400
                   == 0)
  out <- year[leapdayIndex]
  return(out)
}
