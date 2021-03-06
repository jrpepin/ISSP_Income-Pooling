#------------------------------------------------------------------------------------
# GENDER INEQUALITY INDEX PROJECT
# GII_03_analysis.R
# Joanna Pepin and Philip Cohen
#------------------------------------------------------------------------------------

# This file creates the figures and tables for the analysis.
# It also creates Stata datafiles for additional analyses.

#####################################################################################
# Figure 1
#####################################################################################

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
 # geom_text(mapping=aes(label=code), size = 3, position="jitter") + # alternate way of making labels readable.
  geom_text_repel(mapping=aes(label=code), size = 3) +
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
#######################################################################################

data %>%
  group_by(code) %>%
  summarise(meanIndex = mean(index))

data %>%
  summarise(meanIndex = mean(index))

tab2a <- table(data$code, data$relinc)
prop.table(tab2a, 1)

tab2b <- table(data$code, data$marst)
prop.table(tab2b, 1)

table(data$code)

#######################################################################################
# Table 2
#######################################################################################

# Create a variable list which we want in Table 2
listVars <- c("sex", "age", "parent", "employ", "homemaker", "degree", "hswrk", "respmom", "famlife")

# Define categorical variables
catVars <- c("sex", "parent", "employ", "homemaker", "degree", "respmom")

# Descriptive Statistics
tab1 <- CreateTableOne(vars = listVars, data = data, factorVars = catVars)
print(tab1, quote = F)

#######################################################################################
# Table 3 & 4
setwd(outDir)
write.dta(data, "ISSPdata.dta") # Use this data in Stata to create Tables 3 & 4
setwd(repoDir)
#######################################################################################

#######################################################################################
# Figure 2
#######################################################################################

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
  scale_x_reverse( lim=c(61,5)) +
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

#######################################################################################
# Figure 3
#######################################################################################

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
  scale_x_reverse( lim=c(61,5)) +
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

#######################################################################################
# Analyses for reviewers
#######################################################################################

# Create Appendix Table A--------------------------------------------------------------
table(data$country, data$pool)

# Create Figure A----------------------------------------------------------------------
figA_data <- data %>%
  distinct(code, index, percohab)

figA <- figA_data %>%
  ggplot(aes(index, percohab)) +
  geom_text(mapping=aes(label=code), size = 3, position="jitter") +
  geom_smooth(method = "lm", se = FALSE) +
  stat_cor(method="pearson") +
  scale_x_reverse() +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(size = 12,   face = "bold"),
        strip.text = element_text(size = 10,   face = "bold"),
        axis.text  = element_text(size = 10)) +
  ggtitle("Correlation between GII and % Cohabiting") +
  labs(x = "Gender Inequality Index", y = "Proportion Cohabiting")
figA

ggsave("figures/issp_figure A.png", width = 16, height = 16, units = "cm", dpi = 300)

# Figure B--------------------------------------------------------------------------
figB_data <- table(data$country, data$pool)
figB_data <- prop.table(figB_data, 1)
figB_data <- as_tibble(figB_data, .name_repair = ~ c("country", "type", "prop"))

figB_data$type <- factor(figB_data$type, levels = c("One $ Manager", "Manage $ Together", "Keep Some $ Separate", "Keep All $ Separate"))
figB_data$type <- revalue(figB_data$type, c("One $ Manager"="Pooled: One $ Manager", "Manage $ Together"="Pooled: Manage $ Together"))

# Create codes dataset
codes <- data %>%
  distinct(country, code)

figB_data <- figB_data %>% 
  left_join(FLFP,  by = "country") %>%
  left_join(codes, by = "country")
figB_data$code <- factor(figB_data$code)

figB <- figB_data %>%
  filter(code != "IN") %>%
  ggplot(aes(flfp, prop, color = type)) +
  geom_smooth(method = "lm", se = FALSE) +
  # geom_text(mapping=aes(label=code), size = 3, position="jitter") + # alternate way of making labels readable.
  geom_text_repel(mapping=aes(label=code), size = 3) +
  facet_wrap( ~ type) +
  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(size = 12,   face = "bold"),
        strip.text = element_text(size = 10,   face = "bold"),
        axis.text  = element_text(size = 10)) +
  ggtitle("Proportion of Couples with each Income Organizational Approach \nby Female Labor Force Participation") +
  labs(x       = "Gender Inequality flfp", 
       y       = "Proportion",
       caption = "IN excluded because it is an outlier (FLFP = 21.27)") +
  scale_colour_manual(values=c("#116A66", "#CD3278", "#5D478B", "#CD661D"))
figB

ggsave("figures/issp_figure B.png", width = 16, height = 16, units = "cm", dpi = 300)

# Create Figure C--------------------------------------------------------------------
figC_data <- read_excel("figures/figC.xlsx")

figC_data <- figC_data %>%
  gather(type, prop, -relinc, -flfp)

figC_data$relinc <- ordered(figC_data$relinc, levels = c("Men Primary-Earners", "Equal-Earners", "Women Primary-Earners"))

figC <- figC_data %>%
  ggplot(aes(flfp, prop, fill = type)) +
  facet_wrap(~ relinc) +
  geom_area(size=.2, colour="black", alpha = .90) +
  theme_minimal() +
  labs(x = "National Female Labor Force Participation Rate", y = "Proportion") +
  scale_fill_manual(values=c("#CD661D", "#5D478B", "#CD3278", "#116A66")) +
  theme(legend.position="none",
        plot.title = element_text(size = 12,   face = "bold"),
        strip.text = element_text(size = 11,   face = "bold"),
        axis.text  = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle("Predicted Probability of Each Organizational Approach \nby Couple Level Relative Income Status and National FLFP")

figC <- ggdraw(figC) + draw_label(" Keep All $ Separate",    x = 0.22, y = 0.72, fontface='bold', size = 10, colour = "white")
figC <- ggdraw(figC) + draw_label("Keep Some $ Separate",    x = 0.24, y = 0.65, fontface='bold', size = 10, colour = "white")
figC <- ggdraw(figC) + draw_label("Manage $ Together",       x = 0.22, y = 0.50, fontface='bold', size = 10, colour = "white")
figC <- ggdraw(figC) + draw_label("One $ Manager",           x = 0.20, y = 0.22, fontface='bold', size = 10, colour = "white")

figC

ggsave("figures/issp_figure C.png", figC, width = 16, height = 8, units = "cm", dpi = 300)

# Create Figure D-------------------------------------------------------------------------
figD_data <- read_excel("figures/figD.xlsx")

figD_data <- figD_data %>%
  gather(type, prop, -marst, -flfp)

figD_data$marst <- ordered(figD_data$marst, levels = c("Married", "Cohab"))
figD_data$marst <- revalue(figD_data$marst, c("Cohab"="Cohabiting"))


figD <- figD_data %>%
  ggplot(aes(flfp, prop, fill = type)) +
  facet_wrap(~ marst) +
  geom_area(size=.2, colour="black", alpha = .90) +
  theme_minimal() +
  labs(x = "National Female Labor Force Participation Rate", y = "Proportion") +
  scale_fill_manual(values=c("#CD661D", "#5D478B", "#CD3278", "#116A66")) +
  theme(legend.position="none",
        plot.title = element_text(size = 12,   face = "bold"),
        strip.text = element_text(size = 11,   face = "bold"),
        axis.text  = element_text(size = 11),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ggtitle("Predicted Probability of Each Organizational Approach \nby Couple Level Marital Status and National FLFP")


figD <- ggdraw(figD) + draw_label(" Keep All $ Separate",    x = 0.23, y = 0.72, fontface='bold', size = 10, colour = "white")
figD <- ggdraw(figD) + draw_label("Keep Some $ Separate",    x = 0.25, y = 0.67, fontface='bold', size = 10, colour = "white")
figD <- ggdraw(figD) + draw_label("Manage $ Together",       x = 0.23, y = 0.47, fontface='bold', size = 10, colour = "white")
figD <- ggdraw(figD) + draw_label("One $ Manager",           x = 0.21, y = 0.22, fontface='bold', size = 10, colour = "white")

figD

ggsave("figures/issp_figure D.png", figD, width = 16, height = 8, units = "cm", dpi = 300)