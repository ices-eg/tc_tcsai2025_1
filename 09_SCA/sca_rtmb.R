
## Read data
C <-
  as.matrix(read.table("09_SCA/nscod_catage.dat",
    header = TRUE,
    check.names = FALSE, row.names = 1
  ))
I <- as.matrix(read.table("09_SCA/nscod_survey.dat", header = TRUE,
                          check.names = FALSE, row.names = 1))
M <- as.matrix(read.table("09_SCA/nscod_natmort.dat", header = TRUE,
                          check.names = FALSE, row.names = 1))
data <- list(C = C, I = I, M = M)

## Set initial parameter values

parlist <- list(
  logNa = rep(8, ncol(C)),
  logNt = rep(8, nrow(C)),
  logFa = rep(0, ncol(C) - 1),
  logFt = rep(0, nrow(C)),
  logQ = rep(-5, ncol(I))
)




# FLa4a

library(FLa4a)
data(ple4)
data(ple4.indices)
library(plotly)


fmod <- ~ factor(age) + factor(year)

fmod <- ~ s(replace(age, age > 9, 9), k = 4) + s(year, k = 20)

fmod <- ~ s(age, k = 4) + s(year, k = 20) # + s(as.numeric(year - age), k = 10)

fmod <- ~ te(age, year, k = c(4, 20))

fmod <- ~ s(age, k = 3, by = breakpts(year, 1990))


fit <- sca(ple4, ple4.indices[1], fmod)

plot.fit(fit)



plot.fit <- function(fit) {
  xyz <-
    list(
      x = as.numeric(rownames(harvest(fit))),
      y = as.numeric(colnames(harvest(fit))),
      z = harvest(fit)@.Data[, , 1, 1, 1, 1, drop = TRUE]
    )

  Fatage <- xyz$z
  age <- xyz$x
  year <- xyz$y

  plot_ly(
    z = ~Fatage, x = ~year, y = ~age,
    type = "surface", opacity = .80,
    contours = list(
      y = list(show = TRUE),
      x = list(show = TRUE)
    )
  )
}

plot.fit(fit)
wireframe(harvest(fit), drape = TRUE, colorkey = TRUE, scales = list(arrows = FALSE))


fit <- sca(fmod = ~ factor(age), ple4, ple4.indices)
d_s <- residuals(fit, ple4, ple4.indices)
plot(d_s)

fit <- sca(fmod = ~ s(age, k = 6) + s(year, k = 40), ple4, ple4.indices)
d_s <- residuals(fit, ple4, ple4.indices)
plot(d_s)
