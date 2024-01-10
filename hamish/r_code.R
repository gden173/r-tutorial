### Assumptions concerning weightbridge distribution ###
# We make the same assumptions of balance distribution as Usuda:
# First we assume that weight of 1t of class M3 are used. MPE for those: 1 000 000 mg so 1 kg
# As in Usuda, we assume 1/3 of MPE + Normal distribution
maximum_permissible_error_1 <- 0.001
delta <- maximum_permissible_error_1 / 3
# We assume Npass, as in Usuda = 0.999Ntotal
sigma <- delta / 3.3
sigma
### The Wheat industry in Australia ###
### Exports
### Bulk exports
price_per_tonne <- 400
Q <- 15000000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
y1
# # # # # # # # Loss with deviation # # # # # # #
epsilon1 <- maximum_permissible_error_1 / 10
f2 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-((x - epsilon1)^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta - epsilon1, delta - epsilon1)
y2
loss1 <- y2[[1]] - y1[[1]]
loss1
# So we would have a loss of 64 223.23 dollars over the whole economy
### Manufactured export
price_per_tonne <- 200
Q <- 500000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
y1
# # # # # # # # Loss with deviation # # # # # # #
epsilon1 <- maximum_permissible_error_1 / 10
f2 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-((x - epsilon1)^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta - epsilon1, delta - epsilon1)
loss2 <- y2[[1]] - y1[[1]]
loss2
loss1 + loss2


### MPE of higher magnitude ###
### Bulk export
price_per_tonne <- 400
Q <- 15000000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
y1
# # # Loss due to the higher MPE
maximum_permissible_error_2 <- 2 * maximum_permissible_error_1
delta2 <- maximum_permissible_error_2 / 3
sigma2 <- delta2 / 3.3
f2 <- function(x) {
  out <- Q * (1 / (sigma2 * sqrt(2 * pi))) * exp((-((x)^2)) / (2 * (sigma2^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta2, delta2)
y2
loss <- y2[[1]] - y1[[1]]
loss
### Manufactured exports
price_per_tonne <- 200
Q <- 500000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
y1
# # # # # # # Loss due to the higher maximum_permissible_error_
maximum_permissible_error_2 <- 2 * maximum_permissible_error_1
delta2 <- maximum_permissible_error_2 / 3
sigma2 <- delta2 / 3.3
f2 <- function(x) {
  out <- Q * (1 / (sigma2 * sqrt(2 * pi))) * exp((-((x)^2)) / (2 * (sigma2^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta2, delta2)
loss1 <- y2[[1]] - y1[[1]]
loss1 + loss
### The Sugar industry in Australia ###
### Exports of raw sugar
price_per_tonne <- 400
Q <- 3300000



f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}


y1 <- integrate(f1, -delta, delta)
y1
# # # # # # # # Loss with deviation # # # # # # #
epsilon1 <- maximum_permissible_error_1 / 10
f2 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-((x - epsilon1)^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta - epsilon1, delta - epsilon1)
loss <- y2[[1]] - y1[[1]]
loss
### Exports of refined sugar
price_per_tonne <- 5000
Q <- 500000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
y1
# # # # # # # # Loss with deviation
epsilon1 <- maximum_permissible_error_1 / 10
f2 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-((x - epsilon1)^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta - epsilon1, delta - epsilon1)
loss1 <- y2[[1]] - y1[[1]]
loss1
loss1 + loss
### maximum_permissible_errorof higher magnitude ###
price_per_tonne <- 400
Q <- 3300000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
y1
# # # Loss due to the higher maximum_permissible_error_
maximum_permissible_error_2 <- 2 * maximum_permissible_error_1
delta2 <- maximum_permissible_error_2 / 3
sigma2 <- delta2 / 3.3
f2 <- function(x) {
  out <- Q * (1 / (sigma2 * sqrt(2 * pi))) * exp((-((x)^2)) / (2 * (sigma2^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta2, delta2)
y2
loss <- y2[[1]] - y1[[1]]
loss
# refined sugar
price_per_tonne <- 5000


Q <- 500000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
# # # Loss due to the higher maximum_permissible_error_
maximum_permissible_error_2 <- 2 * maximum_permissible_error_1
delta2 <- maximum_permissible_error_2 / 3
sigma2 <- delta2 / 3.3
f2 <- function(x) {
  out <- Q * (1 / (sigma2 * sqrt(2 * pi))) * exp((-((x)^2)) / (2 * (sigma2^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta2, delta2)
y2
loss1 <- y2[[1]] - y1[[1]]
loss1 + loss
### Coal industry ###
# Not the same permissible error than in the other industries
maximum_permissible_error <- 0.005
delta <- maximum_permissible_error / 3
# We assume Npass, as in Usuda = 0.999Ntotal
sigma <- delta / 3.3
sigma
price_per_tonne <- 85
Q <- 261000000
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
# # # # # # # # Loss with deviation
epsilon1 <- maximum_permissible_error / 10
f2 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-((x - epsilon1)^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta - epsilon1, delta - epsilon1)
y2
loss <- y2[[1]] - y1[[1]]
loss



### maximum_permissible_errorof higher magnitude ###
f1 <- function(x) {
  out <- Q * (1 / (sigma * sqrt(2 * pi))) * exp((-(x^2)) / (2 * (sigma^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y1 <- integrate(f1, -delta, delta)
y1

maximum_permissible_error_2 <- 2 * maximum_permissible_error
delta2 <- maximum_permissible_error_2 / 3
sigma2 <- delta2 / 3.3
f2 <- function(x) {
  out <- Q * (1 / (sigma2 * sqrt(2 * pi))) * exp((-((x)^2)) / (2 * (sigma2^2))) * price_per_tonne * abs(0 - x)
  return(out)
}
y2 <- integrate(f2, -delta2, delta2)
y2
loss <- y2[[1]] - y1[[1]]
loss
