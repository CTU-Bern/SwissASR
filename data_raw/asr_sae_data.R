# generate demo data

set.seed(1234)

n <- 20

last_report <- as.Date("2020-11-01")

# little function for easier sampling
msample <- function(x, ...) sample(x, n, TRUE, ...)

sae_data <- data.frame(sae_date = last_report + msample(-20:20),
    record_id = msample(1:300),
    age = round(runif(n, 45, 75)),
    sex = factor(msample(c(c("M", "F")))),
    country = factor(msample("CH")),
    site = factor(msample(letters[1:15])),
    sae = factor(msample(c("headache", "diarhea",
    "spontaneous unconsciousness", "death", "poor sleep"))),
    intervention = factor(msample(c("grp1", "grp2"))),
    outcome = factor(msample(c("resolved", "fatal", "improved",
    "sequel", "unknown"))),
    comment = msample(c("", "fribble", "foo", "bar", "foobar")),
    trt = msample(c("", "trt1", "trt2", "trt3", "trt4")),
    class = msample(c("SAE", "SUSAR", "SADR")),
    expected = msample(c(TRUE, FALSE)),
    devdef = msample(c(TRUE, FALSE)),
    devdefcouldlead = msample(c(TRUE, FALSE)),
    devattr = msample(c(TRUE, FALSE)),
    devint = msample(c(TRUE, FALSE)),
    safetymeasure = msample(c(TRUE, FALSE))
)

sae_data <- sae_data[order(sae_data$sae_date), ]
sae_data$sae_n <- 1:nrow(sae_data)
sae_data$sae_trtstop <- as.Date(ifelse(sae_data$outcome %in% c("resolved", "fatal") &
  sae_data$trt != "", sae_data$sae_date + runif(2:10, 1), NA), origin = "1970-01-01")
sae_data$sae_trtstart <- as.Date(ifelse(sae_data$trt != "", sae_data$sae_date + 1, NA),
  origin = "1970-01-01")
sae_data$related <- rbinom(n, 1, .25)

asr_sae <- sae_data

usethis::use_data(asr_sae, overwrite = TRUE)
