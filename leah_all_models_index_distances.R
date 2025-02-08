library(lme4)
library(lmerTest)
library(car)
library(afex)
library(emmeans)
library(writexl)

setwd("C:/Users/eles/Desktop/Master/Thesis/R Code")
#load all data
gpt2 <- get(load("res_gpt2_leah.RData"))

llama8b <- get(load("res_llama3.1_8b_leah.RData"))

llama70b <- get(load("res_llama3.1_70b_leah.RData"))

llama1b <- get(load("res_llama3.2_1b_leah.RData"))

llama3b <- get(load("res_llama3.2_3b_leah.RData"))

all_models <- rbind(gpt2, llama8b, llama70b, llama1b, llama3b) #concat data


#check for res
subset(all_models, pos  == IA_res_num, 
       select = c("pos", "IA_res_num", "word", "IA_res_word", "Type", "Topic"))

#check for irony
subset(all_models, pos  == IA_irony_num, 
       select = c("pos", "IA_irony_num", "word", "IA_irony_word", "Type", "Topic"))

#calculate distance from irony IA
all_models$index_dist_irony <- all_models$pos - all_models$IA_irony_num

#same for res IA
all_models$index_dist_res <- all_models$pos - all_models$IA_res_num


#IRONY ANALYSIS
dat_crit_irony <- subset(all_models, index_dist_irony >= -2 & index_dist_irony <= 2)
dat_crit_irony$index_dist_irony <- factor(dat_crit_irony$index_dist_irony) #make categorical
dat_crit_irony$Type[dat_crit_irony$Type == "dramatic_irony"] <- "unint_irony"
dat_crit_irony$Type <- factor(dat_crit_irony$Type, levels = c("unint_irony", "reg_irony", "literal"))
#check if critical word has punctuation
dat_crit_irony$punct <- grepl("[[:punct:]]", dat_crit_irony$word)
#split surprisal array for critical words with punctuation
dat_crit_irony$surp_pure <- sapply(dat_crit_irony$subtokens_surprisal, function(x) x[1])
dat_crit_irony$region <- "irony"

test <- dat_crit_irony[order(dat_crit_irony$Type, dat_crit_irony$surprisal, decreasing=TRUE), c("word", "surprisal", "Type")]

#RES ANALYSIS
dat_crit_res <- subset(all_models, index_dist_res >= -2 & index_dist_res <= 2)
dat_crit_res$index_dist_res <- factor(dat_crit_res$index_dist_res) #make categorical
dat_crit_res$Type[dat_crit_res$Type == "dramatic_irony"] <- "unint_irony"
dat_crit_res$Type <- factor(dat_crit_res$Type, levels = c("unint_irony", "reg_irony", "literal"))
#check if critical word has punctuation
dat_crit_res$punct <- grepl("[[:punct:]]", dat_crit_res$word)
#split surprisal array for critical words with punctuation
dat_crit_res$surp_pure <- sapply(dat_crit_res$subtokens_surprisal, function(x) x[1])
dat_crit_res$region <- "resolution"


#Sum not Treatment
options(contrasts = c("contr.Sum", "contr.poly"))

# #irony IAs
surps_irony <- lmer(surp_pure ~ Type * model * index_dist_irony + (1+Type|Topic), data = dat_crit_irony)
summary(surps_irony)

sur.emm <- emmeans(surps_irony, ~ Type + model + index_dist_irony)
pairs_result <- pairs(sur.emm, simple = "Type")

pairs_df <- as.data.frame(pairs_result)

write_xlsx(pairs_df, "pairs_comparison_irony.xlsx")



#res IAs
surps_res <- lmer(surp_pure ~ Type * model * index_dist_res + (1 + Type|Topic), data = dat_crit_res)
summary(surps_res)

sur.emm <- emmeans(surps_res, ~ Type + model)
pairs(sur.emm, simple = "Type")

#plot everything
p1 <-
  afex_plot(
    surps_irony,
    x = "index_dist_irony",        ## predictor shown in x coordinate, such as "region"
    trace = "Type",  ## predictor shown in different lines
    panel = "model", ## predictor shown in different panels
    # error = "within",
    id = c("Topic"),
    dodge = 0.5,
    mapping = c("color", "linetype", "shape"),
    error_arg = list(width = 0.1),
    error_ci = T, # standard errors
    legend_title = NULL,
    data_color = "darkgrey",
    #data_geom = geom_boxplot,
    # data_geom = geom_violin,
    #data_geom = ggbeeswarm::geom_beeswarm,
    #data_geom = geom_quasirandom,
    data_alpha = 0.3,
    data_arg = list(
      width = 0.4,
      colour = "grey",
      notch = T
    )
  )

p1 + 
  ggplot2::theme_minimal() 

### RESOLUTION

p2 <-
  afex_plot(
    surps_res,
    x = "index_dist_res",        ## predictor shown in x coordinate, such as "region"
    trace = "Type",  ## predictor shown in different lines
    panel = "model", ## predictor shown in different panels
    # error = "within",
    id = c("Topic"),
    dodge = 0.5,
    mapping = c("color", "linetype", "shape"),
    error_arg = list(width = 0.1),
    error_ci = T, # standard errors
    legend_title = NULL,
    data_color = "darkgrey",
    #data_geom = geom_boxplot,
    # data_geom = geom_violin,
    #data_geom = ggbeeswarm::geom_beeswarm,
    #data_geom = geom_quasirandom,
    data_alpha = 0.3,
    data_arg = list(
      width = 0.4,
      colour = "grey",
      notch = T
    )
  )

p2 + 
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Suprisals res IA +/- 2")
