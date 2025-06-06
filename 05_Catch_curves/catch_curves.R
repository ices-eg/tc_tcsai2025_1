
library(tidyr)
library(dplyr)
library(ggplot2)

haddock <- read.csv("05_Catch_curves/haddock_catches.csv", header = TRUE, check.names = FALSE)


head(haddock)
names(haddock) <- c("year", 0:8)
head(haddock)

had_data <-
  pivot_longer(
    haddock,
    cols = -year,
    names_to = "age",
    values_to = "catch"
  ) |>
  mutate(
    age = as.numeric(age),
    yearclass = year - age,
    logcatch = log(catch)
  )

ggplot(had_data, aes(x = age, y = logcatch)) +
  geom_line() +
  facet_wrap(~ yearclass) +
  labs(
    x = "Age",
    y = "Catch",
    title = "Haddock catches by cohort"
  ) +
  theme_minimal()

tries <- expand.grid(
  amin = c(0,1,2,3),
  amax = c(7,8)
)

fits <-
  lapply(1:nrow(tries), function(i) {
    lm(
      logcatch ~ age * factor(yearclass),
      data = had_data[had_data$age %in% tries$amin[i]:tries$amax[i], ]
    )
  })

tries$R2 <- sapply(fits, function(x) summary(x)$r.squared)
tries[order(tries$R2, decreasing = TRUE), ]

had_data_37 <-
  had_data |>
  filter(age %in% 3:7 & yearclass %in% 1972:2013)

ggplot(had_data_37, aes(x = age, y = logcatch)) +
  geom_line() +
  facet_wrap(~yearclass) +
  labs(
    x = "Age",
    y = "Catch",
    title = "Haddock catches by cohort"
  ) +
  theme_minimal()



fit <- lm(
  logcatch ~ age * factor(yearclass),
  data = had_data_37
)

newdata <-
  expand.grid(
    yearclass = unique(had_data_37$yearclass)
  )

intercept <- predict(fit, newdata = data.frame(newdata, age = 0) )
slope <- predict(fit, newdata = data.frame(newdata, age = 1)) - intercept

newdata <- newdata |>
  mutate(
    intercept = intercept,
    slope = slope,
    z = -slope
  )

ggplot(newdata, aes(x = yearclass, y = z)) +
  geom_line() +
  labs(
    x = "Year class",
    y = "Z",
    title = "Mortality rate by year class"
  ) +
  theme_minimal()
