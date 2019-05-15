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
# Table 3 & 4
require(foreign)
write.dta(data, "C:/Users/Joanna/Dropbox/Repositories/ISSP_Income-Pooling/ISSPdata.dta")

#######################################################################################
# Figures
library(readxl)
library(cowplot)

## Figure 2
fig2_data <- read_excel("C:/Users/Joanna/Dropbox/Repositories/ISSP_Income-Pooling/figures/fig2.xlsx")

fig2_data <- fig2_data %>%
  gather(type, prop, -relinc, -index)

fig2_data$relinc <- ordered(fig2_data$relinc, levels = c("Men Primary-Earners", "Equal-Earners", "Women Primary-Earners"))

#  ggtitle("Predictive Probability of Each Organizational Approach \nby Couple Level Relative Income Status and Country Level Gender Inequality")
fig2 <- fig2_data %>%
  ggplot(aes(index, prop, fill = type)) +
  facet_wrap(~ relinc) +
  geom_area(size=1, colour="black", alpha = .90) +
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

ggsave("issp_figure 2.png", fig2, width = 16, height = 8, units = "cm", dpi = 300)

