#install.packages("DIDmultiplegt")
#install.packages('rgl')
options(rgl.useNULL = TRUE)
library(rgl)
library(DIDmultiplegt)
library(tidyverse)

# The .csv file "data-final-assessment.csv" can be found here
# https://drive.google.com/file/d/1P8PhUxUfsEiehUR--7Kd3lBr9XUCJuje/view?usp=drive_link


df = read_csv("data-final-assessment.csv")

did_multiplegt(mode='old', df=df, Y='Turnout_idea', G='cid', T='year', D='CV_idea', 
               placebo=5, dynamic=5, brep=30, parallel=TRUE)#, cluster='cid'

did_multiplegt(mode='dyn', df=df, outcome='Turnout_idea', group='cid', time='interval', 
               treatment='CV_idea', placebo=3, effects=5, bootstrap=30, cluster='cid')
