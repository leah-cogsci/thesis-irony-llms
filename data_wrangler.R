### use pacman::p_load instead of multiple install_packages() & library() calls
if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(lme4, lmerTest, car, afex, emmeans, sciplot, ggplot2, dplyr)

setwd("C:/Users/eles/Desktop/Master/Thesis/R Code")

### loading data of all models

gpt2 <- get(load("res_gpt2_leah.RData"))

llama8b <- get(load("res_llama3.1_8b_leah.RData"))

llama70b <- get(load("res_llama3.1_70b_leah.RData"))

llama1b <- get(load("res_llama3.2_1b_leah.RData"))

llama3b <- get(load("res_llama3.2_3b_leah.RData"))

dat_all <- rbind(gpt2, llama8b, llama70b, llama1b, llama3b) #concat all models for baseline exp

#drop cols
dat_all_filtered <- dat_all %>% select(-Stimulus_simple, -IA_irony_region, -IA_res_region)

dat_all_filtered$context <- "unbiased" #add context for later model


### load prime results

gpt2_prime <- get(load("res_prime_gpt2.RData"))

llama8b_prime <- get(load("res_prime_llama3.1_8b.RData"))

llama70b_prime <- get(load("res_prime_llama3.1_70b.RData"))

llama1b_prime <- get(load("res_prime_llama3.2_1b.RData"))

llama3b_prime <- get(load("res_prime_llama3.2_3b.RData"))

dat_all_prime <- rbind(gpt2_prime, llama8b_prime, llama70b_prime, llama1b_prime, llama3b_prime)

#drop cols
dat_prime_filtered <- dat_all_prime %>% select(-subtokens_entropy, -prompt_complete, -pre)

colnames(dat_prime_filtered)[13:14] <- c("IA_res_num", "IA_res_word") #rename bias cols

dat_prime_filtered$context <- "biased"  #add context for later model


#check that the colnames are identical 
## THIS NEEDS TO BE TRUE SO THAT I CAN USE RBIND

identical(colnames(dat_all_filtered), colnames(dat_prime_filtered)) 

#save as files
save(dat_all_filtered, file = "dat_all_filtered.RData")
save(dat_prime_filtered, file = "dat_prime_filtered.RData")