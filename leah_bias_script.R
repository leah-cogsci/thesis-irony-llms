### use pacman::p_load instead of multiple install_packages() & library() calls
if (!require(pacman)) {
  install.packages("pacman")
  }

pacman::p_load(lme4, lmerTest, car, afex, emmeans, sciplot, ggplot2, dplyr, writexl)

setwd("C:/Users/eles/Desktop/Master/Thesis/R Code")

### loading data of all models

dat_reg <- get(load("dat_all_filtered.RData"))

dat_prime <- get(load("dat_prime_filtered.RData"))

### concat data 

dat_all <- rbind(dat_reg, dat_prime)
dat_all$model[dat_all$model == "Meta-Llama-3.1-8B"] <- "Llama-3.1-8B" #consolidate naming
dat_all$index <- dat_all$pos - dat_all$IA_irony_num #add index

### check that everything works correctly
table(dat_all$model)

# critical regions subset

dat_crit1 <- subset(dat_all, pos == IA_irony_num)
dat_crit2 <- subset(dat_all, pos == IA_res_num) #res for bias is prime

### and concat 

dat_crit <- rbind(dat_crit1, dat_crit2)

# dat_crit <- subset(dat_all, index >= -2 & index <= 2)
# dat_crit$index <- factor(dat_crit$index)
# dat_crit$index <- relevel(dat_crit$index, ref = "0")
# levels(dat_crit$index)
dat_crit$model <- factor(dat_crit$model)
dat_crit$model <- relevel(dat_crit$model, ref = "Meta-Llama-3.1-70B")
levels(dat_crit$model)

dat_crit$context <- factor(dat_crit$context, levels = c("unbiased", "biased"))
levels(dat_crit$context)

dat_crit$Type[dat_crit$Type == "dramatic_irony"] <- "unint_irony"
dat_crit$Type <- factor(dat_crit$Type, levels = c("literal", "reg_irony", "unint_irony"))
levels(dat_crit$Type)
dat_crit$punct <- grepl("[[:punct:]]", dat_crit$word)
dat_crit$surp_pure <- sapply(dat_crit$subtokens_surprisal, function(x) x[1])

### Treatment coding per default
options(contrasts = c("contr.Treatment", "contr.poly"))


# leah1 <- lmer(surprisal ~ Type * region * model + punct + (1+Type+punct|Topic), data = dat_crit)
# summary(leah1)

# leah2 <- lmer(surp_pure ~ Type * index * model + (1+Type|Topic), data = dat_crit)
# summary(leah2)

contrasts(dat_crit$model) <- contr.Sum(levels(dat_crit$model))
contrasts(dat_crit$context) <- contr.Sum(levels(dat_crit$context))

#look only at target irony area for both
dat_crit_0 <- subset(dat_crit, index == "0")

leah3 <- lmer(surp_pure ~ Type * model * context + (1+Type|Topic), data = dat_crit_0)
summary(leah3)
coe <- coefficients(summary(leah3))

df_coe <- as.data.frame(coe)
df_coe$Term <- rownames(df_coe)      
df_coe <- df_coe[, c("Term", setdiff(names(df_coe), "Term"))] #reorder
rownames(df_coe) <- NULL 

write_xlsx(df_coe, "model_summary.xlsx")

#check for grouped effects
sur.emm <- emmeans(leah3, ~ Type + model + context)
bias_emmeans <- pairs(sur.emm, simple = "Type")

bias_emmeans_df <- as.data.frame(bias_emmeans)

write_xlsx(bias_emmeans_df, "bias_posthoc.xlsx")

p1 <-
  afex_plot(
    leah3,
    x = "model",
    trace = "Type",
    panel = "context",
    # error = "within",
    #id = c("RECORDING_SESSION_LABEL", "item_id"),
    id = c("Topic"),
    #id = c("item_id"),
    dodge = 0.2,
    #col_trace = c("red", "blue"),
    mapping = c("color", "linetype", "shape"),
    error_arg = list(width = 0.1),
    error_ci = F, # standard errors
    legend_title = NULL,
    data_color = "darkgrey",
    #data_geom = geom_boxplot,
    #data_geom = geom_violin,
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
  ggplot2::theme_minimal() #+ scale_colour_manual(values = c("red", "blue"))


