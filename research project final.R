#Ian Mallory


#setup
rm(list=ls())
setwd("C:\\Users\\ianma\\Documents\\UCSD\\poli 5\\research project")

library(reshape)
library(reshape2)
library(stringr)
library(readxl)
library(RCurl)
library(stargazer)
library(ggplot2)
library(ggiraphExtra)
library(sandwich)
library(lmtest)
library(car)
library(corrplot)
library(ggpubr)

###########################################################################################
#imports information to be used later to create robust standard errors

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)


#importing and cleaning public opinions data
public_opinions_2019 <- read.csv("~/UCSD/poli 5/research project/public_opinions_2019.csv")

public_opinions_2019 = public_opinions_2019[(public_opinions_2019$GeoType=='County'),]


#importing and cleaning employment data
employment_data <- read.csv("~/UCSD/poli 5/research project/employment_data.csv")

employment_data$GEOID = as.numeric(as.character(employment_data[,1]))

#merging employment data with public opinion data

employment_public_opinion = merge(public_opinions_2019, employment_data, by='GEOID')



#import and clean county GDP data

excel_gdp_per_capita2 <- read_excel("~/UCSD/poli 5/research project/excel_gdp_per_capita2.xlsx", col_types = c("text", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "numeric"))

excel_gdp_per_capita2$GEOID = as.numeric(excel_gdp_per_capita2$GEOID)

#merge county GDP data with above data

master_dataframe = merge(employment_public_opinion, excel_gdp_per_capita2, by='GEOID')



#import and clean education data

final_education <- read_excel("~/UCSD/poli 5/research project/final_education.xlsx")

final_education$GEOID = as.numeric(final_education$GEOID)

#merge master dataframe with education data

master_dataframe = merge(master_dataframe, final_education, by = 'GEOID')


#import and clean political data
`2016_US_County_Level_Presidential_Results` <- read.csv("~/UCSD/poli 5/research project/2016_US_County_Level_Presidential_Results.csv")

presidential_results = subset(`2016_US_County_Level_Presidential_Results`, select = c('per_dem', 'per_gop', 'combined_fips'))

presidential_results$GEOID = presidential_results$combined_fips

#merge master dataframe with political data

master_dataframe = merge(master_dataframe, presidential_results, by = 'GEOID')


#creating new variable: proportion workforce in mining/fossil fuel

mining_jobtitle = as.character(master_dataframe$Description[6])
prop_mining_fossil = c()
unique_counties = unique(master_dataframe$GEOID)

for(i in 1:length(unique_counties)){
  total_workforce_subset = subset(master_dataframe, master_dataframe$GEOID==unique_counties[i] & master_dataframe$Description=='Total employment (number of jobs)')
  total_workforce = as.numeric(as.character(total_workforce_subset$X2018[1]))
  mining_subset = subset(master_dataframe, master_dataframe$GEOID==unique_counties[i] & master_dataframe$Description==mining_jobtitle)
  total_mining_fossil = as.numeric(as.character(mining_subset$X2018[1]))
  prop_mining_fossil1 = total_mining_fossil/total_workforce
  prop_mining_fossil = c(prop_mining_fossil, prop_mining_fossil1)
}

#reducing master data frame to just description being total employment. This is necessary to to add prop_mining_fossil
master_dataframe = master_dataframe[(master_dataframe$Description == 'Total employment (number of jobs)'),]

master_dataframe$prop_mining_fossil = prop_mining_fossil

master_dataframe_map = master_dataframe


#dropping missing data

is_nas = c()
for(i in 1:length(master_dataframe$prop_mining_fossil)){
  is_na = is.na(master_dataframe$prop_mining_fossil[i])
  is_nas = c(is_nas, is_na)
}
master_dataframe$is_nas = is_nas

master_dataframe = master_dataframe[master_dataframe$is_nas==FALSE,]

is_nas2 = c()
for(i in 1:length(master_dataframe$GDP_pc)){
  is_na = is.na(master_dataframe$GDP_pc[i])
  is_nas2 = c(is_nas2, is_na)
}
master_dataframe$is_nas2 = is_nas2

master_dataframe = master_dataframe[master_dataframe$is_nas2==FALSE,]



#dropping unnecessary variables

master_dataframe = subset(master_dataframe, select = c('GEOID', 'GeoFIPS', 'GeoName.x', 'Description', 
                          'X2018', 'TotalPop', 'discuss', 'reducetax', 'CO2limits', 
                          'localofficials', 'governor', 'congress', 'president', 'corporations', 
                          'citizens', 'regulate', 'supportRPS', 'drilloffshore', 'drillANWR', 
                          'fundrenewables', 'rebates', 'mediaweekly', 'prienv', 'happening', 
                          'human', 'consensus', 'worried', 'personal', 'harmUS', 'futuregen', 
                          'timing', 'regulate', 'regulateOppose', 'CO2limitsOppose', 'prienv', 'GDP_pc',
                          'Per_bach', 'per_dem', 'per_gop', 'prop_mining_fossil'))

#change Per_bach variable to numeric
master_dataframe$Per_bach = as.numeric(master_dataframe$Per_bach)


###########################################################################################


#summary statistics:
master_dataframe_summary = subset(master_dataframe, select = c('happening', 'human', 'regulate', 'drillANWR',
                                                               'GDP_pc', 'Per_bach', 'per_gop', 'prop_mining_fossil'))
stargazer::stargazer(master_dataframe_summary, type = "html", out="C:\\Users\\ianma\\Documents\\UCSD\\poli 5\\research project\\summary.html")

###########################################################################################

###regressions
##first calculates cook distances. Then it removes outliers and re-runs the regression


#happening regression

happening_reg = lm(happening ~ prop_mining_fossil + GDP_pc + Per_bach + per_gop, data = master_dataframe)

cooks = cooks.distance(happening_reg)
new_dataframe = master_dataframe
new_dataframe$cooks = cooks
no_cooks_outliers_happening = master_dataframe[(new_dataframe$cooks < 4*mean(new_dataframe$cooks)),]

happening_reg_cooked = lm(happening ~ prop_mining_fossil + GDP_pc +Per_bach + per_gop, data = no_cooks_outliers_happening)
robust_se1 <- as.vector(summary(happening_reg_cooked,robust = T)$coefficients[,"Std. Error"])
stargazer(happening_reg_cooked, se = list(robust_se1), type = "html", report = 'vctp')

################################

#human regression

human_reg = lm(human ~ prop_mining_fossil + GDP_pc + Per_bach + per_gop, data = master_dataframe)

cooks = cooks.distance(human_reg)
new_dataframe = master_dataframe
new_dataframe$cooks = cooks
no_cooks_outliers_human = new_dataframe[(new_dataframe$cooks < 4*mean(new_dataframe$cooks)),]

human_reg_cooked = lm(human ~ prop_mining_fossil + GDP_pc + Per_bach + per_gop, data = no_cooks_outliers_human)
robust_se2 <- as.vector(summary(human_reg_cooked,robust = T)$coefficients[,"Std. Error"])
stargazer(human_reg_cooked, se = list(robust_se2), type = "html", report = 'vctp')

################################

#regulate regression

regulate_reg = lm(regulate ~ prop_mining_fossil + GDP_pc + Per_bach + per_gop, data = master_dataframe)

cooks = cooks.distance(regulate_reg)
new_dataframe = master_dataframe
new_dataframe$cooks = cooks
no_cooks_outliers_regulate = new_dataframe[(new_dataframe$cooks < 4*mean(new_dataframe$cooks)),]

regulate_reg_cooked = lm(regulate ~ prop_mining_fossil + GDP_pc + Per_bach + per_gop, data = no_cooks_outliers_regulate)
robust_se3 <- as.vector(summary(regulate_reg_cooked,robust = T)$coefficients[,"Std. Error"])
stargazer(regulate_reg_cooked, se = list(robust_se3), type = "html", report = 'vctp')

################################

#drillANWR regression

drillANWR_reg = lm(drillANWR ~ prop_mining_fossil + GDP_pc + Per_bach + per_gop, data = master_dataframe)

cooks = cooks.distance(drillANWR_reg)
new_dataframe = master_dataframe
new_dataframe$cooks = cooks
no_cooks_outliers_drillANWR = new_dataframe[(new_dataframe$cooks < 4*mean(new_dataframe$cooks)),]

drillANWR_reg_cooked = lm(drillANWR ~ prop_mining_fossil + GDP_pc + Per_bach + per_gop, data = no_cooks_outliers_drillANWR)
robust_se4 <- as.vector(summary(drillANWR_reg_cooked,robust = T)$coefficients[,"Std. Error"])
stargazer(drillANWR_reg_cooked, se = list(robust_se4), type = "html", report = 'vctp')


################################

################################

#regression matrix

stargazer::stargazer(happening_reg_cooked, human_reg_cooked, regulate_reg_cooked,
                     drillANWR_reg_cooked, se=list(robust_se1, robust_se2, robust_se3, robust_se4), 
                     type='html', report='vctp')



###########################################################################################



###visualizations used to check regression assumptions

#multicolinearity
ind_var_subset = subset(master_dataframe, select = c('prop_mining_fossil', 'GDP_pc', 'Per_bach', 'per_gop'))
m = cor(ind_var_subset)
corrplot(m, method='color', type='upper', addCoef.col = "black")

#residual distributions
resid1 = ggplot(no_cooks_outliers_happening, aes(x=resid(happening_reg_cooked))) + 
  geom_histogram(binwidth = .25) + xlab('residual') + ggtitle('residual distribution - happening')
resid2 = ggplot(no_cooks_outliers_human, aes(x=resid(human_reg_cooked))) + 
  geom_histogram(binwidth = .25) + xlab('residual') + ggtitle('residual distribution - human')
resid3 = ggplot(no_cooks_outliers_regulate, aes(x=resid(regulate_reg_cooked))) + 
  geom_histogram(binwidth = .25) + xlab('residual') + ggtitle('residual distribution - regulate')
resid4 = ggplot(no_cooks_outliers_drillANWR, aes(x=resid(drillANWR_reg_cooked))) + 
  geom_histogram(binwidth = .25) + xlab('residual') + ggtitle('residual distribution - drillANWR')

resid_plots = ggarrange(resid1, resid2, resid3, resid4)

#residual plots

sresid1 = ggplot(no_cooks_outliers_happening, aes(x=prop_mining_fossil, y=resid(happening_reg_cooked))) + 
  geom_point() + xlab('prop_mining_fossil') + ggtitle('residual distribution - happening')
sresid2 = ggplot(no_cooks_outliers_human, aes(x=prop_mining_fossil, y=resid(human_reg_cooked))) + 
  geom_point() + xlab('prop_mining_fossil') + ggtitle('residual distribution - human')
sresid3 = ggplot(no_cooks_outliers_regulate, aes(x=prop_mining_fossil, y=resid(regulate_reg_cooked))) + 
  geom_point() + xlab('prop_mining_fossil') + ggtitle('residual distribution - regulate')
sresid4 = ggplot(no_cooks_outliers_drillANWR, aes(x=prop_mining_fossil, y=resid(drillANWR_reg_cooked))) + 
  geom_point() + xlab('prop_mining_fossil') + ggtitle('residual distribution - drillANWR')

resid_plots2 = ggarrange(sresid1, sresid2, sresid3, sresid4)

###visualizations

#added variable plots

#code for functions source: https://stackoverflow.com/questions/59150905/is-there-a-ggplot2-analogue-to-the-avplots-function-in-r

#avplot - happening

avPlots.invis <- function(MODEL, ...) {
  
  ff <- tempfile();
  png(filename = ff);
  OUT <- car::avPlots(MODEL, ...);
  dev.off()
  unlink(ff);
  OUT; }

ggAVPLOTS  <- function(MODEL, YLAB = NULL) {
  
  #Extract the information for AV plots
  AVPLOTS <- avPlots.invis(MODEL);
  K       <- length(AVPLOTS);
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K);
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]]);
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'black') + ggtitle('Added Variable Plot - Happening') +# geom_text(aes(label=no_outliers$GeoName.x)) +
      geom_smooth(method = 'lm', se = FALSE, 
                  color = 'red', formula = y ~ x, linetype = 'dashed') +
      xlab(paste0('Predictor Residual \n (', 
                  names(DATA)[1], ' | others)')) +
      ylab(paste0('Response Residual \n (',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')); 
      }
  
  #Return output object
  GGPLOTS; }

library(gridExtra);
PLOTS <- ggAVPLOTS(happening_reg_cooked);
K     <- length(PLOTS);
NCOL  <- ceiling(sqrt(K));
AVPLOTS <- do.call("arrangeGrob", c(PLOTS, ncol = NCOL, top = 'Added Variable Plots'));
ggsave('AV Plots - Trucking.jpg', width = 10, height = 10);
avplot_happening = PLOTS[1]


#avplot - human

ggAVPLOTS  <- function(MODEL, YLAB = NULL) {
  
  #Extract the information for AV plots
  AVPLOTS <- avPlots.invis(MODEL);
  K       <- length(AVPLOTS);
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K);
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]]);
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'black') + ggtitle('Added Variable Plot - Human') +# geom_text(aes(label=no_outliers$GeoName.x)) +
      geom_smooth(method = 'lm', se = FALSE, 
                  color = 'red', formula = y ~ x, linetype = 'dashed') +
      xlab(paste0('Predictor Residual \n (', 
                  names(DATA)[1], ' | others)')) +
      ylab(paste0('Response Residual \n (',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')); 
  }
  
  #Return output object
  GGPLOTS; }

library(gridExtra);
PLOTS <- ggAVPLOTS(human_reg_cooked);
K     <- length(PLOTS);
NCOL  <- ceiling(sqrt(K));
AVPLOTS <- do.call("arrangeGrob", c(PLOTS, ncol = NCOL, top = 'Added Variable Plots'));
ggsave('AV Plots - Trucking.jpg', width = 10, height = 10);
avplot_happening = PLOTS[1]


#avplot - regulate

ggAVPLOTS  <- function(MODEL, YLAB = NULL) {
  
  #Extract the information for AV plots
  AVPLOTS <- avPlots.invis(MODEL);
  K       <- length(AVPLOTS);
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K);
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]]);
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'black') + ggtitle('Added Variable Plot - Regulate') +# geom_text(aes(label=no_outliers$GeoName.x)) +
      geom_smooth(method = 'lm', se = FALSE, 
                  color = 'red', formula = y ~ x, linetype = 'dashed') +
      xlab(paste0('Predictor Residual \n (', 
                  names(DATA)[1], ' | others)')) +
      ylab(paste0('Response Residual \n (',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')); 
  }
  
  #Return output object
  GGPLOTS; }

library(gridExtra);
PLOTS <- ggAVPLOTS(regulate_reg_cooked);
K     <- length(PLOTS);
NCOL  <- ceiling(sqrt(K));
AVPLOTS <- do.call("arrangeGrob", c(PLOTS, ncol = NCOL, top = 'Added Variable Plots'));
ggsave('AV Plots - Trucking.jpg', width = 10, height = 10);
avplot_happening = PLOTS[1]

#avplot - drillANWR

ggAVPLOTS  <- function(MODEL, YLAB = NULL) {
  
  #Extract the information for AV plots
  AVPLOTS <- avPlots.invis(MODEL);
  K       <- length(AVPLOTS);
  
  #Create the added variable plots using ggplot
  GGPLOTS <- vector('list', K);
  for (i in 1:K) {
    DATA         <- data.frame(AVPLOTS[[i]]);
    GGPLOTS[[i]] <- ggplot2::ggplot(aes_string(x = colnames(DATA)[1], 
                                               y = colnames(DATA)[2]), 
                                    data = DATA) +
      geom_point(colour = 'black') + ggtitle('Added Variable Plot - drillANWR') +# geom_text(aes(label=no_outliers$GeoName.x)) +
      geom_smooth(method = 'lm', se = FALSE, 
                  color = 'red', formula = y ~ x, linetype = 'dashed') +
      xlab(paste0('Predictor Residual \n (', 
                  names(DATA)[1], ' | others)')) +
      ylab(paste0('Response Residual \n (',
                  ifelse(is.null(YLAB), 
                         paste0(names(DATA)[2], ' | others'), YLAB), ')')); 
  }
  
  #Return output object
  GGPLOTS; }

library(gridExtra);
PLOTS <- ggAVPLOTS(drillANWR_reg_cooked);
K     <- length(PLOTS);
NCOL  <- ceiling(sqrt(K));
AVPLOTS <- do.call("arrangeGrob", c(PLOTS, ncol = NCOL, top = 'Added Variable Plots'));
ggsave('AV Plots - Trucking.jpg', width = 10, height = 10);
avplot_happening = PLOTS[1]

