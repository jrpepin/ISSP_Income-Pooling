# Set-up

## Set working directory
setwd(repoDir)

## Figures & Tables
library("foreign")
library("plyr")
library("tableone")
library("readxl")
library("cowplot")
library("tidyverse")
library("ggplot2")
#######################################################################################
# Figure 1
fig1_data <- table(data$country, data$pool)
fig1_data <- prop.table(fig1_data, 1)
fig1_data <- as_tibble(fig1_data, .name_repair = ~ c("country", "type", "prop"))

fig1_data$type <- factor(fig1_data$type, levels = c("One $ Manager", "Manage $ Together", "Keep Some $ Separate", "Keep All $ Separate"))
fig1_data$type <- revalue(fig1_data$type, c("One $ Manager"="Pooled: One $ Manager", "Manage $ Together"="Pooled: Manage $ Together"))

# Create codes dataset
codes <- data %>%
  distinct(country, code)

fig1_data <- fig1_data %>% 
  left_join(GII,  by = "country") %>%
  left_join(codes, by = "country")
fig1_data$code <- factor(fig1_data$code)

fig1 <- fig1_data %>%
  ggplot(aes(index, prop, color = type)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(mapping=aes(label=code), size = 3, position="jitter") +
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
fig1

ggsave("figures/issp_figure 1.png", width = 16, height = 16, units = "cm", dpi = 300)

#######################################################################################
# Table 1

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
  group_by(code) %>%
  summarize(meanIndex = mean(index))

tab2a <- table(data$code, data$relinc)
prop.table(tab2a, 1)

tab2b <- table(data$code, data$marst)
prop.table(tab2b, 1)

table(data$code)

#######################################################################################
# Table 3 & 4
write.dta("data/ISSPdata.dta")

#######################################################################################
# Figures

## Figure 2
fig2_data <- read_excel("figures/fig2.xlsx")

fig2_data <- fig2_data %>%
  gather(type, prop, -relinc, -index)

fig2_data$relinc <- ordered(fig2_data$relinc, levels = c("Men Primary-Earners", "Equal-Earners", "Women Primary-Earners"))

#  ggtitle("Predictive Probability of Each Organizational Approach \nby Couple Level Relative Income Status and code Level Gender Inequality")
fig2 <- fig2_data %>%
  ggplot(aes(index, prop, fill = type)) +
  facet_wrap(~ relinc) +
  geom_area(size=.2, colour="black", alpha = .90) +
  theme_minimal() +
  labs(x = "Gender Inequality Index", y = "Proportion") +
  scale_fill_manual(values=c("#CD661D", "#5D478B", "#CD3278", "#116A66")) +
  scale_x_reverse( lim=c(.61,.05)) +
    theme(legend.position="none",
        plot.title = element_text(size = 12,   face = "bold"),
        strip.text = element_text(size = 11,   face = "bold"),
        axis.text  = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

fig2 <- ggdraw(fig2) + draw_label(" Keep All $ Separate",    x = 0.22, y = 0.82, fontface='bold', size = 10, colour = "white")
fig2 <- ggdraw(fig2) + draw_label("Keep Some $ Separate",    x = 0.24, y = 0.74, fontface='bold', size = 10, colour = "white")
fig2 <- ggdraw(fig2) + draw_label("Manage $ Together",       x = 0.22, y = 0.63, fontface='bold', size = 10, colour = "white")
fig2 <- ggdraw(fig2) + draw_label("One $ Manager",           x = 0.20, y = 0.22, fontface='bold', size = 10, colour = "white")

fig2

ggsave("figures/issp_figure 2.png", fig2, width = 16, height = 8, units = "cm", dpi = 300)

## Figure 3
fig3_data <- read_excel("figures/fig3.xlsx")

fig3_data <- fig3_data %>%
  gather(type, prop, -marst, -index)

fig3_data$marst <- ordered(fig3_data$marst, levels = c("Married", "Cohab"))
fig3_data$marst <- revalue(fig3_data$marst, c("Cohab"="Cohabiting"))


#  ggtitle("Predictive Probability of Each Organizational Approach \nby Couple Level Marital Status and code Level Gender Inequality")
fig3 <- fig3_data %>%
  ggplot(aes(index, prop, fill = type)) +
  facet_wrap(~ marst) +
  geom_area(size=.2, colour="black", alpha = .90) +
  theme_minimal() +
  labs(x = "Gender Inequality Index", y = "Proportion") +
  scale_fill_manual(values=c("#CD661D", "#5D478B", "#CD3278", "#116A66")) +
  scale_x_reverse( lim=c(.61,.05)) +
  theme(legend.position="none",
        plot.title = element_text(size = 12,   face = "bold"),
        strip.text = element_text(size = 11,   face = "bold"),
        axis.text  = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

fig3 <- ggdraw(fig3) + draw_label(" Keep All $ Separate",    x = 0.23, y = 0.82, fontface='bold', size = 10, colour = "white")
fig3 <- ggdraw(fig3) + draw_label("Keep Some $ Separate",    x = 0.25, y = 0.74, fontface='bold', size = 10, colour = "white")
fig3 <- ggdraw(fig3) + draw_label("Manage $ Together",       x = 0.23, y = 0.63, fontface='bold', size = 10, colour = "white")
fig3 <- ggdraw(fig3) + draw_label("One $ Manager",           x = 0.21, y = 0.22, fontface='bold', size = 10, colour = "white")

fig3

ggsave("figures/issp_figure 3.png", fig3, width = 16, height = 8, units = "cm", dpi = 300)