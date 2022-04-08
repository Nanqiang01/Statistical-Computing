isLeapday <- function(year) {
  mod4 <- year %% 4
  mod100 <- year %% 100
  mod400 <- year %% 400
  LeapdayIndex <- ((mod4
  == 0 & mod100 != 0) |
    mod400
    == 0)
  if((mod4  == 0 & mod100 != 0) |  mod400 == 0)
  {
    out <- TRUE
  }
  else
  {
    out <- FALSE
  }
  return(out)
  return(LeapdayIndex)
}