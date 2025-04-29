rswald <- function(t, a, m, Ter){
  ans <- a/sqrt(2*pi*(t-Ter)^3)*
    exp(-(a-m*(t-Ter))^2/(2*(t-Ter)))
}