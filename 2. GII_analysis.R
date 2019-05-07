# Figures & Tables

#######################################################################################
# Figure 1
setwd("C:/Users/Joanna/Dropbox/Cohen/Gender Inequality Index/Figures")

fig1_data <- table(data$country, data$pool)
fig1_data <- prop.table(fig1_data, 1)
fig1_data <- as_tibble(fig1_data, .name_repair = ~ c("country", "type", "prop"))

fig1_data$type <- factor(fig1_data$type, levels = c("One $ Manager", "Manage $ Together", "Keep Some $ Separate", "Keep All $ Separate"))

fig1_data <- fig1_data %>% 
  left_join(GII, by = "country")
fig1_data$country <- factor(fig1_data$country)


fig1 <- fig1_data %>%
  ggplot(aes(index, prop, color = type)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(mapping=aes(label=country), size = 3) +
  scale_x_reverse() +
  facet_wrap( ~ type) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(size = 12,   face = "bold"),
        strip.text = element_text(size = 10,   face = "bold"),
        axis.text  = element_text(size = 10)) +
#  ggtitle("Proportion of Couples with each Income Organizational Approach \nby Gender Inequality Index") +
  labs(x = "Gender Inequality Index", y = "Proportion") +
  scale_colour_manual(values=c("#116A66", "#CD3278", "#5D478B", "#CD661D"))

ggsave("issp_figure 1.png", width = 16, height = 16, units = "cm", dpi = 300)

#######################################################################################
# Table 1
## Load package
library("tableone")

# Create a variable list which we want in Table 1
listVars <- c("sex", "age", "parent", "employ", "homemaker", "degree", "hswrk", "respmom", "famlife")

# Define categorical variables
catVars <- c("sex", "parent", "employ", "homemaker", "degree", "respmom")

# Descriptive Statistics
tab1 <- CreateTableOne(vars = listVars, data = data, factorVars = catVars)
print(tab1, quote = F)

#######################################################################################
# Table 2
data %>%
  group_by(country) %>%
  summarize(meanIndex = mean(index))

tab2a <- table(data$country, data$relinc)
prop.table(tab2a, 1)

tab2b <- table(data$country, data$marst)
prop.table(tab2b, 1)

table(data$country)

#######################################################################################
# Table 3
setwd("C:/Users/Joanna/Dropbox/Repositories/ISSP_Income-Pooling")
write.csv(data, file = "ISSPdata.csv")


library(nnet)
mn1 <- multinom(pool ~ marst + relinc, data = data)
mn2 <- multinom(pool ~ marst + relinc | country, data = data)

# https://stackoverflow.com/questions/21082396/multinomial-logistic-multilevel-models-in-r

install.packages("devtools")
library(devtools)
find_rtools() # Error in find_rtools() : could not find function "find_rtools"
install.packages("pkgbuild")
library(pkgbuild)
find_rtools()

library(brms)

bm1 <- brm (pool ~ (1 | index), # this doesn't work
           data=data, family="categorical",
           prior=c(set_prior ("normal (0, 8)")))

bm2 <- brm (
  pool   ~ relinc,
  data   = data, 
  family = "categorical",
  prior  = c(set_prior ("normal (0, 8)")),
  seed   = 123 # Adding a seed makes results reproducible.
  )

m1 <- brm(
  Allowed ~ D.z,
  data = lrsmall,
  prior = m1priors,
  family = "bernoulli",
  seed = 123 # Adding a seed makes results reproducible.
) 

bm3 <- brm (pool ~ marst + (1 | index),
           data=data, family="categorical",
           prior=c(set_prior ("normal (0, 8)")))

library(mlogit)
mlog1 <- mlogit(pool ~ relinc + marst | index, data = data)


# rpl <- mlogit(mode ~ price+ catch | income, Fishing, varying = 2:9,
  #            shape = 'wide', rpar = c(price= 'n', catch = 'n'),
   #           correlation = TRUE, halton = NA,
    #          R = 10, tol = 10, print.level = 0)