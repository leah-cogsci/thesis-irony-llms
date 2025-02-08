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

#IRONY ANALYSIS
dat_crit_irony <- subset(all_models, pos  == IA_irony_num)
dat_crit_irony$Type[dat_crit_irony$Type == "dramatic_irony"] <- "unint_irony"
dat_crit_irony$Type <- factor(dat_crit_irony$Type, levels = c("unint_irony", "reg_irony", "literal"))
#check if critical word has punctuation
dat_crit_irony$punct <- grepl("[[:punct:]]", dat_crit_irony$word)
#split surprisal array for critical words with punctuation
dat_crit_irony$surp_pure <- sapply(dat_crit_irony$subtokens_surprisal, function(x) x[1])
dat_crit_irony$region <- "irony"

#RES ANALYSIS
dat_crit_res <- subset(all_models, pos  == IA_res_num)
dat_crit_res$Type[dat_crit_res$Type == "dramatic_irony"] <- "unint_irony"
dat_crit_res$Type <- factor(dat_crit_res$Type, levels = c("unint_irony", "reg_irony", "literal"))
#check if critical word has punctuation
dat_crit_res$punct <- grepl("[[:punct:]]", dat_crit_res$word)
#split surprisal array for critical words with punctuation
dat_crit_res$surp_pure <- sapply(dat_crit_res$subtokens_surprisal, function(x) x[1])
dat_crit_res$region <- "resolution"


#concat data
dat_crit_all <- rbind(dat_crit_irony, dat_crit_res)

#Sum not Treatment
options(contrasts = c("contr.Sum", "contr.poly"))



#irony IAs
surps_irony <- lmer(surp_pure ~ Type * model + (1 + Type|Topic), data = dat_crit_irony)
summary(surps_irony)

sur.emm <- emmeans(surps_irony, ~ Type + model)
irony_pairs_result <- pairs(sur.emm, simple = "Type")

irony_pairs_df <- as.data.frame(irony_pairs_result)

write_xlsx(irony_pairs_df, "irony_pairs.xlsx")

#res IAs
surps_res <- lmer(surp_pure ~ Type * model + (1 + Type|Topic), data = dat_crit_res)
summary(surps_res)

sur.emm <- emmeans(surps_res, ~ Type + model)
res_pairs_result <- pairs(sur.emm, simple = "Type")

res_pairs_df <- as.data.frame(res_pairs_result)

write_xlsx(res_pairs_df, "res_pairs.xlsx")

#total analysis
surps_total <- lmer(surp_pure ~ Type * model * region +(1|Topic), data=dat_crit_all)
summary(surps_total)

sur.emm <- emmeans(surps_total, ~ Type + model + region)
pairs(sur.emm, simple = "Type")

#plot everything
p3 <-
  afex_plot(
    surps_total,
    x = "region",        ## predictor shown in x coordinate, such as "region"
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

p3 + 
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Surprisal per target region")

### IRONY 

p2 <-
  afex_plot(
    surps_irony,
    x = "Type",        ## predictor shown in x coordinate, such as "region"
    #trace = "Type",  ## predictor shown in different lines
    #panel = "model", ## predictor shown in different panels
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
  ggplot2::ggtitle("Surprisals irony IA all models")

#### RESOLUTION

p1 <-
  afex_plot(
    surps_res,
    x = "Type",        ## predictor shown in x coordinate, such as "region"
    #trace = "Type",  ## predictor shown in different lines
    #panel = "model", ## predictor shown in different panels
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
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Surprisals res IA all models")
