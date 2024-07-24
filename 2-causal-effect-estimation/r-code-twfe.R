################
##### INIT #####
################

#Library packages
install.packages(c("bacondecomp", "did", "eatATA", "haven", "lfe", "panelView", "tidyverse"))
library(bacondecomp)
library(did)
library(eatATA)
library(haven)
library(lfe)
library(panelView)
library(tidyverse)

# Set working directory

# The .csv file "data-final-assessment.csv" can be found here: 
# https://drive.google.com/file/d/1P8PhUxUfsEiehUR--7Kd3lBr9XUCJuje/view?usp=sharing

#####################
##### LOAD DATA #####
#####################

castle = read_csv("data-final-assessment.csv")

plot_order <- c("lead10","lead9","lead8","lead7","lead6",
                "lead5","lead4","lead3","lead2","lead1",
                "lag1","lag2","lag3","lag4","lag5","lag6",
                "lag7","lag8","lag9","lag10")


dropped_vars <- c("type", "cv_change")

#lintrend <- castle %>%
#  select(starts_with("trend")) %>% 
#  colnames %>% 
#  # remove due to colinearity
#  subset(.,! . %in% dropped_vars)

#region <- castle %>%
#  select(starts_with("r20")) %>% 
#  colnames %>% 
#  # remove due to colinearity
#  subset(.,! . %in% dropped_vars) 

#castle$Country <- sapply(castle$Country, unclass)
#for (i in 1:length(castle[, 1])) {
#  if (is.na(castle[i, "year"])) {
#    castle[i, "year"] == 3000
#  }
#}


#########################
##### STANDARD TWFE #####
#########################

# Standard TWFE estimation
dd_reg <- felm(Turnout_idea ~ CV_idea | year + cid,
               weights = castle$Population, 
               data = castle)
summary(dd_reg)


# Now cluster the standard errors by state (always go for this option!)
dd_reg <- felm(Turnout_idea ~ CV_idea | year + cid | 0 | cid, 
               weights = castle$Population, 
               data = castle)
summary(dd_reg)

# Store the magnitudes of the main ATT estimate's 95% confidence interval bounds
main_ATT = summary(dd_reg)$coefficients["CV_idea", "Estimate"]
ATT_CI_LB = abs(main_ATT - qnorm(0.975)*summary(dd_reg)$coefficients["CV_idea", "Cluster s.e."])
ATT_CI_UB = abs(main_ATT + qnorm(0.975)*summary(dd_reg)$coefficients["CV_idea", "Cluster s.e."])


###############################
##### EVENT STUDIES & PTA #####
###############################

#Event study regression
event_study_formula <- as.formula(
  paste("Turnout_idea ~ ",
        paste(
          paste(paste("lead", 1:10, sep = ""), collapse = " + "),
          paste(paste("lag", 1:10, sep = ""), collapse = " + "), sep = " + "),
        "| year + Country | 0 | cid"
  ),
)
event_study_reg <- felm(event_study_formula, 
                        weights = castle$Population, 
                        data = castle)
summary(event_study_reg)

#Plotting results
leadslags_plot <- tibble(
  sd = c(event_study_reg$cse[plot_order], 0),
  mean = c(coef(event_study_reg)[plot_order], 0),
  label = c(-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8,9,10,0)
)
leadslags_plot %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-qnorm(0.95)*sd, 
             ymax = mean+qnorm(0.95)*sd)) +
  geom_hline(yintercept = ATT_CI_LB, color = "red") +
  geom_hline(yintercept = -ATT_CI_LB, color = "red") +
  geom_pointrange() +
  theme_minimal() +
  xlab("Years before and after CV is adopted") +
  ylab("Turnout") +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_vline(xintercept = 0,
             linetype = "dashed")

