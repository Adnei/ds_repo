require('ggplot2')
require('dplyr')
require('caret')
require('lubridate')
N_EXPERIMENTS <- 30

dataset <- read.table("soja_milho_modificado.csv", sep="|", dec=",", head=T, stringsAsFactors=F)

conf_mtx_stats <- data.frame(
  tfp = c(rep(NA,N_EXPERIMENTS)),
  tvp = c(rep(NA, N_EXPERIMENTS))
)

for (exp_idx in c(1:N_EXPERIMENTS)){
  training_sample <- dataset %>%
    slice_sample(prop=0.25) %>%
    arrange(obs)

  #Removing the training sample from the original dataset
  #testing.df <- dataset %>% filter(! obs %in% training_sample$obs)

  training_sample$data <- dmy(training_sample$data)

  #Identifying harvest time
  actual <- month(training_sample$data) >= 1 & month(training_sample$data) <= 5
  predicted <- (training_sample$ano == 2014 & training_sample$soja >= 67) | (training_sample$ano == 2015 & training_sample$soja >= 58 & training_sample$soja <= 62)

  conf_mtx <- confusionMatrix(data=factor(predicted), reference=factor(actual))
  # | VN | FN |
  # | FP | VP |
  conf_mtx

  fp <- conf_mtx$table[2,1]
  vp <- conf_mtx$table[2,2]
  conf_mtx_stats$tfp[exp_idx] <- fp / length(actual)
  conf_mtx_stats$tvp[exp_idx] <- vp / length(actual)
}

sort_stats <- conf_mtx_stats %>% arrange(tvp)
