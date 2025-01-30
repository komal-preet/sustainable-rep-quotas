# project: Sustainable Representation through Electoral Quotas: Evidence from India
# authors: Komal Preet Kaur and Andrew Q. Philips
# date: 2024-01-08
# task: Fig 2-4 in main manuscript and Fig 5-24 in SI


#* load packages ----
library(pacman)
p_load(tidyverse, cregg, xtable, viridis, survey) #download archieve for cregg: https://cran.r-project.org/src/contrib/Archive/cregg/) dependencies: survey and ggstance packages in R


#* source plot function ----
source("./mm_plot_fun.R")

#* load cleaned dataset ----
dat <- read_rds("clean_conjoint_india_all.rds") 

#* recording a few variables because cregg package gets confused between attributes having same levels. ---- 
dat$`Local gender quota` <- recode(dat$`Local gender quota`, 
                                   "Yes" = "Used local gender quota", 
                                   "No" = "No local gender quota",
                                   "Not Applicable" = "Not Applicable")

dat$`Local caste quota` <- recode(dat$`Local caste quota`, 
                                  "Yes" = "Used local caste quota", 
                                  "No" = "No local caste quota",
                                  "Not Applicable" = "NA")

dat$`Local political experience` <- recode(dat$`Local political experience`, 
                                           Yes = "Local political experience", 
                                           No = "No local political experience")


dat$vignette_info <- recode(dat$vignette_info, 
                            both = "Gender & caste quota", 
                            caste = "Caste quota",
                            control = "Control",
                            gen = "Gender quota")

# creating levels for attributes so they appear in the tables and figures in the order we like
levels(dat$vignette) <- c("Gender & caste quota", 
                          "Caste quota","Control",
                          "Gender quota")
dat$vignette <- factor(dat$vignette, 
                       levels = c("Control", 
                                  "Gender quota",
                                  "Caste quota",
                                  "Gender & caste quota"))
dat$vignette_info <- factor(dat$vignette_info, 
                            levels = c("Control", 
                                       "Gender quota",
                                       "Caste quota",
                                       "Gender & caste quota"))


#* pick variables of choice ----
c_dat <- dat %>% 
  select(responseid, ipaddress, conjoint, picked, Gender, Caste ,`Local gender quota`, `Local caste quota`, Religion, `Dynastic family`, `Local political experience`, `Age`, `Party type`, `Caste reservation on MLA seat`, vignette, vignette_info, r_gender, r_caste, r_religion, r_age, r_edu, q45, voter_id, pol_interest,interact_poli,interact_pol_level, voted_nat_elect, voted_state_elect, dep_pm, party_support, women_mp, women_res_approv, caste_res_approv, wom_unfit_patriarchal, wom_unfit_dirtypol,wom_discrim, caste_discrim, gen_intelligent, indians_fair, women_res50, correct_vignette, r_income) 

# recode respondent's caste variable to make a binary variable of attributes Non-SCST and SCST 
c_dat$r_caste1 <- NA
c_dat$r_caste1[c_dat$r_caste == "General"] <- "Non-SC/ST"
c_dat$r_caste1[c_dat$r_caste == "SC"] <- "SC/ST"
c_dat$r_caste1[c_dat$r_caste == "ST"] <- "SC/ST"
c_dat$r_caste1 <- as.factor(c_dat$r_caste1)

##########################
#* Fig 2 in main manuscript  and Fig 5 in SI - basic MM ---- 
##########################

# for basic marginal means the folllowing reflect our general approach:
f1 = picked ~ Gender + Caste +`Local gender quota` + `Local caste quota` + `Local political experience` + `Dynastic family` + `Party type` + `Caste reservation on MLA seat` +  Religion +  `Age` 

mm <- cregg::cj(c_dat, f1, 
                id = ~responseid, 
                estimate = "mm")
mm <- filter(mm, 
             !level %in% c("NA", "Not Applicable"))

# Fig 5 in SI 
mm_fullplot_alldata <- ggplot(mm, aes(x = level, y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ .,
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_single(gg_input_single = mm_fullplot_alldata)
#ggsave(path = "figs", file = "mm_fullplot_alldata.jpg", width = 11, height = 4)


# Fig 2 in main manuscript  
mm <- filter(mm, 
             feature %in% c("Gender", 
                            "Caste", 
                            "Local gender quota", 
                            "Local caste quota"))
mm_plot <- ggplot(mm, aes(x = level, y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ .,
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_single(gg_input_single = mm_plot)
#ggsave(path = "figs", file = "mm_plot.jpg", width = 11, height = 4)

#########################
##* Fig 4 in main manuscript and Fig 6 in SI - MM by vignette ----
##########################

mm_by_vignette_alldata <- cregg::cj(c_dat, f1, 
                                    id = ~responseid, 
                                    by = ~vignette_info,
                                    estimate = "mm")

mm_by_vignette_alldata <- filter(mm_by_vignette_alldata, 
                                 !level %in% c("NA", "Not Applicable"))

# Fig 6 in SI 
mm_by_vignette_plot_alldata <- ggplot(mm_by_vignette_alldata, 
                                      aes(x = level, 
                                          y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")
theme_mine_single(gg_input_single = mm_by_vignette_plot_alldata)
#ggsave(path = "figs", file = "mm_by_vignette_plot_alldata.jpg", width = 11, height = 4)

# Fig 4 in main manuscript 
mm_by_vignette_alldata <- filter(mm_by_vignette_alldata, 
                                 feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_alldata_smallplot <- ggplot(mm_by_vignette_alldata, 
                                           aes(x = level, 
                                               y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_single(gg_input_single = mm_by_vignette_alldata_smallplot)
#ggsave(path = "figs", file = "mm_by_vignette_alldata_smallplot.jpg", width = 11, height = 4)

#########################
##* Fig 5 in the main manuscript - intersectional MM  ----
##########################

c_dat$GenderXCaste <- with(c_dat, 
                           interaction(Gender, Caste), 
                           drop=T)
c_dat$GenderXlocal_gen_quota <- with(c_dat, 
                                     interaction(Gender, `Local gender quota`), 
                                     drop=T)
c_dat$GenderXlocal_caste_quota <- with(c_dat, 
                                       interaction(Gender, `Local caste quota`), 
                                       drop=T)
c_dat$GenderXlocal_gen_quotaXlocal_caste_quota <- with(c_dat, 
                                                       interaction(Gender, `Local gender quota`,`Local caste quota`), 
                                                       drop=T)

mm_genderx_vignette <- cregg::cj(c_dat, 
                                 picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                 id=~responseid, 
                                 estimate = "mm", 
                                 by=~vignette_info)

mm_genderx_vignette <-  mm_genderx_vignette  %>%
  filter(!estimate == "NA") %>% 
  filter(!grepl("Not Applicable|NA", level)) %>% 
  mutate(feature = case_when(feature == "GenderXCaste" ~ "Gender x Caste",
                             feature == "GenderXlocal_gen_quota" ~ "Gender x Local gender quota",
                             feature == "GenderXlocal_caste_quota" ~ "Gender x Local caste quota", 
                             feature == "GenderXlocal_gen_quotaXlocal_caste_quota" ~ "Gender x Local caste quota  x Local caste quota"))

mm_genderx_vignette$feature <- factor(mm_genderx_vignette$feature, 
                                      levels = c("Gender x Caste", 
                                                 "Gender x Local gender quota",
                                                 "Gender x Local caste quota", 
                                                 "Gender x Local caste quota  x Local caste quota"))


mm_genderx_vignette_plot <- ggplot(mm_genderx_vignette, 
                                   aes(x = level, 
                                       y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") +  
  theme(panel.spacing.x = unit(.25, "cm")) +
  theme(panel.spacing.y = unit(.25, "cm"))

theme_mine_single(gg_input_single =mm_genderx_vignette_plot)
#ggsave(path = "figs", file = "fig5.jpg", width = 13, height = 5)

#####################################################
#* Fig 7 in SI - MM analysis by vignette across age ----
#####################################################

table(c_dat$r_age)

mm_by_vignette_r_age_1824 <- cregg::cj(subset(c_dat, r_age=="18-24"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm")
mm_by_vignette_r_age_1824 <- filter(mm_by_vignette_r_age_1824, 
                                    !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_1824$`Respondent Age` <- "18-24"


mm_by_vignette_r_age_2534 <- cregg::cj(subset(c_dat, r_age=="25-34"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm")
mm_by_vignette_r_age_2534 <- filter(mm_by_vignette_r_age_2534, 
                                    !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_2534$`Respondent Age` <- "25-34"


mm_by_vignette_r_age_3544 <- cregg::cj(subset(c_dat, r_age=="35-44"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm")
mm_by_vignette_r_age_3544 <- filter(mm_by_vignette_r_age_3544, 
                                    !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_3544$`Respondent Age` <- "35-44"

mm_by_vignette_r_age_4554 <- cregg::cj(subset(c_dat, r_age=="45-54"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm")
mm_by_vignette_r_age_4554 <- filter(mm_by_vignette_r_age_4554, 
                                    !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_4554$`Respondent Age` <- "45-54"

mm_by_vignette_r_age <- rbind(mm_by_vignette_r_age_1824, mm_by_vignette_r_age_2534, mm_by_vignette_r_age_3544, mm_by_vignette_r_age_4554)
mm_by_vignette_r_age_small <- filter(mm_by_vignette_r_age, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))
mm_by_vignette_r_age_smallplot <- ggplot(mm_by_vignette_r_age_small,
                                         aes(x = level,
                                             y = estimate,
                                             colour=`Respondent Age`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")

theme_mine_quad(gg_input_quad = mm_by_vignette_r_age_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Respondent Age`)) 

# ggsave(path = "figs", file = "mm_by_vignette_r_age_smallplot_alldata.jpg", width = 11, height = 5)

#####################################################
#* Fig 8 in SI - MM analysis by vignette across men and women respondents ----
#####################################################

mm_by_vignette_r_male <- cregg::cj(subset(c_dat, r_gender=="Male"), f1, 
                                   id = ~responseid, 
                                   by = ~vignette_info,
                                   estimate = "mm")
mm_by_vignette_r_male <- filter(mm_by_vignette_r_male, 
                                !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_male$`Respondent Gender` <- "Male"

mm_by_vignette_r_female <- cregg::cj(subset(c_dat, r_gender=="Female"), f1, 
                                     id = ~responseid, 
                                     by = ~vignette_info,
                                     estimate = "mm")
mm_by_vignette_r_female <- filter(mm_by_vignette_r_female, 
                                  !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_female$`Respondent Gender` <- "Female"
mm_by_vignette_r_gender <- rbind(mm_by_vignette_r_male, mm_by_vignette_r_female)
mm_by_vignette_r_gender_small <- filter(mm_by_vignette_r_gender, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))
mm_by_vignette_r_gender_smallplot <- ggplot(mm_by_vignette_r_gender_small,
                                            aes(x = level,
                                                y = estimate,
                                                colour=`Respondent Gender`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")
theme_mine_double(gg_input_double = mm_by_vignette_r_gender_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Respondent Gender`)) 

#ggsave(path = "figs", file = "mm_by_vignette_r_gender_smallplot_alldata.jpg", width = 11, height = 5)

#####################################################
#* Fig 9 in SI - MM analysis by vignette across caste groups ----
#####################################################

mm_by_vignette_r_gen <- cregg::cj(subset(c_dat, r_caste1=="Non-SC/ST"), f1, 
                                  id = ~responseid, 
                                  by = ~vignette_info,
                                  estimate = "mm")

mm_by_vignette_r_gen <- filter(mm_by_vignette_r_gen, 
                               !level %in% c("NA", "Not Applicable"))

mm_by_vignette_r_gen$`Respondent Caste` <- "Non-SC/ST"

mm_by_vignette_r_scst <- cregg::cj(subset(c_dat, r_caste1=="SC/ST"), f1, 
                                   id = ~responseid, 
                                   by = ~vignette_info,
                                   estimate = "mm")

mm_by_vignette_r_scst <- filter(mm_by_vignette_r_scst, 
                                !level %in% c("NA", "Not Applicable"))

mm_by_vignette_r_scst$`Respondent Caste` <- "SC/ST"

mm_by_vignette_r_caste <- rbind(mm_by_vignette_r_gen, mm_by_vignette_r_scst)

mm_by_vignette_r_caste_small <- filter(mm_by_vignette_r_caste, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_r_caste_smallplot <- ggplot(mm_by_vignette_r_caste_small,
                                           aes(x = level,
                                               y = estimate,
                                               colour=`Respondent Caste`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")

theme_mine_double(gg_input_double = mm_by_vignette_r_caste_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Respondent Caste`)) 

#ggsave(path = "figs", file = "mm_by_vignette_r_caste_smallplot_alldata.jpg", width = 11, height = 5)





#* code five-point Likert scale to three-point scale for attitudinal questions before obtaining marginal means ----
c_dat <- 
  c_dat %>% 
  #qus: approve of women's reservation in politics?
  mutate(women_res_approv_cat3 = case_when(women_res_approv == "Strongly agree" ~ "Approve",
                                           women_res_approv == "Somewhat agree" ~ "Approve",
                                           women_res_approv == "Strongly disagree" ~ "Disapprove",
                                           women_res_approv == "Somewhat disagree" ~ "Disapprove",
                                           women_res_approv == "Neither agree nor disagree" ~ "Neutral"),
         #qus: approve of caste's reservation in politics?
         caste_res_approv_cat3 =  case_when(caste_res_approv == "Strongly agree" ~ "Approve",
                                            caste_res_approv == "Somewhat agree" ~ "Approve",
                                            caste_res_approv == "Strongly disagree" ~ "Disapprove",
                                            caste_res_approv == "Somewhat disagree" ~ "Disapprove",
                                            caste_res_approv == "Neither agree nor disagree" ~ "Neutral"),
         #women are not discriminated against in politics.
         wom_discrim_cat3 = case_when(wom_discrim == "Strongly agree" ~ "Disagree", #reverse the scale
                                      wom_discrim == "Somewhat agree" ~ "Disagree",
                                      wom_discrim == "Strongly disagree" ~ "Agree",
                                      wom_discrim == "Somewhat disagree" ~ "Agree",
                                      wom_discrim == "Neither agree nor disagree" ~ "Neutral"),
         #people are not discriminated against based on caste in politics.
         caste_discrim_cat3 = case_when(caste_discrim == "Strongly agree" ~ "Disagree", #reverse the scale
                                        caste_discrim == "Somewhat agree" ~ "Disagree",
                                        caste_discrim == "Strongly disagree" ~ "Agree",
                                        caste_discrim == "Somewhat disagree" ~ "Agree",
                                        caste_discrim == "Neither agree nor disagree" ~ "Neutral"), 
         # Women are unsuitable for politics and that their scope should remain limited to work that their father or husband approves of.
         wom_unfit_patriarchal_cat3 = case_when(wom_unfit_patriarchal == "Strongly agree" ~ "Agree",
                                                wom_unfit_patriarchal == "Somewhat agree" ~ "Agree",
                                                wom_unfit_patriarchal == "Neither agree nor disagree" ~ "Neutral",
                                                wom_unfit_patriarchal == "Somewhat disagree" ~ "Disagree",
                                                wom_unfit_patriarchal == "Strongly disagree" ~ "Disagree"),
         # Women are simply unsuitable for politics because politics is a dirty business.
         wom_unfit_dirtypol_cat3 = case_when(wom_unfit_dirtypol == "Strongly agree" ~ "Agree",
                                             wom_unfit_dirtypol == "Somewhat agree" ~ "Agree",
                                             wom_unfit_dirtypol == "Neither agree nor disagree" ~ "Neutral",
                                             wom_unfit_dirtypol == "Somewhat disagree" ~ "Disagree",
                                             wom_unfit_dirtypol == "Strongly disagree" ~ "Disagree"), 
         # People from the Forward (General) caste are more intelligent on average.
         gen_intelligent_cat3 = case_when(gen_intelligent == "Strongly agree" ~ "Agree",
                                          gen_intelligent == "Somewhat agree" ~ "Agree",
                                          gen_intelligent == "Neither agree nor disagree" ~ "Neutral",
                                          gen_intelligent == "Somewhat disagree" ~ "Disagree",
                                          gen_intelligent == "Strongly disagree" ~ "Disagree"),
         fairness_cat3 = case_when(indians_fair == "Strongly agree" ~ "Agree",
                                   indians_fair == "Somewhat agree" ~ "Agree",
                                   indians_fair == "Neither agree nor disagree" ~ "Neutral",
                                   indians_fair == "Somewhat disagree" ~ "Disagree",
                                   indians_fair == "Strongly disagree" ~ "Disagree"))

likert_map <- c("Strongly agree" = 5, "Somewhat agree" = 4, "Neither agree nor disagree" = 3, "Somewhat disagree" = 2, "Strongly disagree" = 1)      

# create a binary variable for sexism
c_dat <- c_dat %>% 
  mutate(wom_unfit_patriarchal_num = likert_map[wom_unfit_patriarchal],
         wom_unfit_dirtypol_num = likert_map[wom_unfit_dirtypol],
         sexism = (wom_unfit_patriarchal_num+wom_unfit_dirtypol_num)/2, 
         sexism_cat = if_else(sexism <=2.5, "Low", "High"))



#####################################################
#* Fig 10 in SI - MM analysis by vignette based on the attitudes towards caste discrimination   ----
#####################################################

mm_by_vignette_no_caste_discrim <- cregg::cj(subset(c_dat, caste_discrim_cat3 =="Disagree"), f1, 
                                             id = ~responseid, 
                                             by = ~vignette_info,
                                             estimate = "mm")

mm_by_vignette_no_caste_discrim <- filter(mm_by_vignette_no_caste_discrim, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_no_caste_discrim$`People face discrimination based on caste` <- "Disagree"

mm_by_vignette_caste_discrim_neutral <- cregg::cj(subset(c_dat, caste_discrim_cat3 =="Neutral"), f1, 
                                                  id = ~responseid, 
                                                  by = ~vignette_info,
                                                  estimate = "mm")

mm_by_vignette_caste_discrim_neutral <- filter(mm_by_vignette_caste_discrim_neutral, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_caste_discrim_neutral$`People face discrimination based on caste` <- "Neutral"

mm_by_vignette_y_caste_discrim <- cregg::cj(subset(c_dat, caste_discrim_cat3 =="Agree"), f1, 
                                            id = ~responseid, 
                                            by = ~vignette_info,
                                            estimate = "mm")
mm_by_vignette_y_caste_discrim <- filter(mm_by_vignette_y_caste_discrim, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_y_caste_discrim$`People face discrimination based on caste` <- "Agree"

mm_by_vignette_y_caste_discrim <- rbind(mm_by_vignette_y_caste_discrim, mm_by_vignette_no_caste_discrim, mm_by_vignette_caste_discrim_neutral)

mm_by_vignette_y_caste_discrim_small <- filter(mm_by_vignette_y_caste_discrim, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_y_caste_discrim_smallplot <- ggplot(mm_by_vignette_y_caste_discrim_small,
                                                   aes(x = level,
                                                       y = estimate,
                                                       colour=`People face discrimination based on caste`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_triple(gg_input_triple = mm_by_vignette_y_caste_discrim_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`People face discrimination based on caste`)) 

#ggsave(path = "figs", file = "mm_by_vignette_caste_discrim_smallplot_alldata_nowt.jpg", width = 11, height = 5)


#####################################################
#* Fig 11 in SI -  MM analysis by vignette based on the attitudes towards caste reservation   ----
#####################################################
mm_by_vignette_caste_res_disapprov <- cregg::cj(subset(c_dat, caste_res_approv_cat3 =="Disapprove"), f1, 
                                                id = ~responseid, 
                                                by = ~vignette_info,
                                                estimate = "mm")
mm_by_vignette_caste_res_disapprov <- filter(mm_by_vignette_caste_res_disapprov, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_caste_res_disapprov$`Attitudes towards Caste Reservation` <- "Disapprove"

mm_by_vignette_caste_res_neutral <- cregg::cj(subset(c_dat, caste_res_approv_cat3 =="Neutral"), f1, 
                                              id = ~responseid, 
                                              by = ~vignette_info,
                                              estimate = "mm")
mm_by_vignette_caste_res_neutral <- filter(mm_by_vignette_caste_res_neutral, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_caste_res_neutral$`Attitudes towards Caste Reservation` <- "Neutral"

mm_by_vignette_caste_res_approv <- cregg::cj(subset(c_dat, caste_res_approv_cat3 =="Approve"), f1, 
                                             id = ~responseid, 
                                             by = ~vignette_info,
                                             estimate = "mm")
mm_by_vignette_caste_res_approv <- filter(mm_by_vignette_caste_res_approv, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_caste_res_approv$`Attitudes towards Caste Reservation` <- "Approve"

mm_by_vignette_caste_res_attitude <- rbind(mm_by_vignette_caste_res_approv, mm_by_vignette_caste_res_disapprov, mm_by_vignette_caste_res_neutral)

mm_by_vignette_caste_res_attitude_small <- filter(mm_by_vignette_caste_res_attitude, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_caste_res_attitude_smallplot <- ggplot(mm_by_vignette_caste_res_attitude_small,
                                                      aes(x = level,
                                                          y = estimate,
                                                          colour=`Attitudes towards Caste Reservation`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_triple(gg_input_triple = mm_by_vignette_caste_res_attitude_smallplot)  + geom_point(size = 1, position = position_dodge(0.3), aes(shape=`Attitudes towards Caste Reservation`)) 

#ggsave(path = "figs", file = "mm_by_vignette_caste_res_attitude_smallplot_alldata.jpg", width = 11, height = 5)

#####################################################
#* Fig 12 in SI -  MM analysis by vignette based on the attitudes towards women's reservation   ----
#####################################################

mm_by_vignette_wom_res_disapprov <- cregg::cj(subset(c_dat, 
                                                     women_res_approv_cat3 =="Disapprove"), 
                                              f1, 
                                              id = ~responseid, 
                                              by = ~vignette_info,
                                              estimate = "mm")

mm_by_vignette_wom_res_disapprov <- filter(mm_by_vignette_wom_res_disapprov, 
                                           !level %in% c("NA", "Not Applicable"))
mm_by_vignette_wom_res_disapprov$`Attitudes towards Women's Reservation` <- "Disapprove"

mm_by_vignette_wom_res_neutral <- cregg::cj(subset(c_dat, women_res_approv_cat3 =="Neutral"), f1, 
                                            id = ~responseid, 
                                            by = ~vignette_info,
                                            estimate = "mm")

mm_by_vignette_wom_res_neutral <- filter(mm_by_vignette_wom_res_neutral, 
                                         !level %in% c("NA", "Not Applicable"))
mm_by_vignette_wom_res_neutral$`Attitudes towards Women's Reservation` <- "Neutral"

mm_by_vignette_wom_res_approv <- cregg::cj(subset(c_dat, women_res_approv_cat3 =="Approve"), f1, 
                                           id = ~responseid, 
                                           by = ~vignette_info,
                                           estimate = "mm")

mm_by_vignette_wom_res_approv <- filter(mm_by_vignette_wom_res_approv, !level %in% c("NA", "Not Applicable"))

mm_by_vignette_wom_res_approv$`Attitudes towards Women's Reservation` <- "Approve"

mm_by_vignette_wom_res_attitude <- rbind(mm_by_vignette_wom_res_approv, mm_by_vignette_wom_res_disapprov, mm_by_vignette_wom_res_neutral)

mm_by_vignette_wom_res_attitude_small <- filter(mm_by_vignette_wom_res_attitude, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_wom_res_attitude_smallplot <- ggplot(mm_by_vignette_wom_res_attitude_small,
                                                    aes(x = level,
                                                        y = estimate,
                                                        colour=`Attitudes towards Women's Reservation`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_triple(gg_input_triple = mm_by_vignette_wom_res_attitude_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Attitudes towards Women's Reservation`)) 

#ggsave(path = "figs", file = "mm_by_vignette_wom_res_attitude_smallplot_alldata.jpg", width = 11, height = 5)

#####################################################
#* Fig 13 in SI  - MM analysis by vignette based on the attitudes towards women's discrimination   ----
#####################################################

mm_by_vignette_no_wom_discrim <- cregg::cj(subset(c_dat, wom_discrim_cat3 =="Disagree"), f1, 
                                           id = ~responseid, 
                                           by = ~vignette_info,
                                           estimate = "mm")
mm_by_vignette_no_wom_discrim <- filter(mm_by_vignette_no_wom_discrim, 
                                        !level %in% c("NA", "Not Applicable"))
mm_by_vignette_no_wom_discrim$`Women face discrimination` <- "Disagree"

mm_by_vignette_wom_discrim_neutral <- cregg::cj(subset(c_dat, wom_discrim_cat3 =="Neutral"), f1, 
                                                id = ~responseid, 
                                                by = ~vignette_info,
                                                estimate = "mm")

mm_by_vignette_wom_discrim_neutral <- filter(mm_by_vignette_wom_discrim_neutral, 
                                             !level %in% c("NA", "Not Applicable"))
mm_by_vignette_wom_discrim_neutral$`Women face discrimination` <- "Neutral"

mm_by_vignette_y_wom_discrim <- cregg::cj(subset(c_dat, wom_discrim_cat3 =="Agree"), f1, 
                                          id = ~responseid, 
                                          by = ~vignette_info,
                                          estimate = "mm")

mm_by_vignette_y_wom_discrim <- filter(mm_by_vignette_y_wom_discrim, !level %in% c("NA", "Not Applicable"))

mm_by_vignette_y_wom_discrim$`Women face discrimination` <- "Agree"

mm_by_vignette_y_wom_discrim <- rbind(mm_by_vignette_y_wom_discrim, mm_by_vignette_no_wom_discrim, mm_by_vignette_wom_discrim_neutral)

mm_by_vignette_y_wom_discrim_small <- filter(mm_by_vignette_y_wom_discrim, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_y_wom_discrim_smallplot <- ggplot(mm_by_vignette_y_wom_discrim_small,
                                                 aes(x = level,
                                                     y = estimate,
                                                     colour=`Women face discrimination`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_triple(gg_input_triple = mm_by_vignette_y_wom_discrim_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Women face discrimination`)) 

#ggsave(path = "figs", file = "mm_by_vignette_wom_discrim_smallplot_alldata_nowt.jpg", width = 11, height = 5)

#####################################################
#* Fig 14-16 in SI use survey weights, the code for that is shown towards the end of the script   ----
#####################################################

#####################################################
#* Fig 17 in SI - MM analysis by vignette based on sexist attitudes   ----
#####################################################

mm_by_vignette_low_sexism <- cregg::cj(subset(c_dat, sexism_cat =="Low"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm")

mm_by_vignette_low_sexism <- filter(mm_by_vignette_low_sexism, !level %in% c("NA", "Not Applicable"))

mm_by_vignette_low_sexism$`Sexist Attitudes` <- "Low"

mm_by_vignette_high_sexism <- cregg::cj(subset(c_dat, sexism_cat=="High"), f1, 
                                        id = ~responseid, 
                                        by = ~vignette_info,
                                        estimate = "mm")

mm_by_vignette_high_sexism <- filter(mm_by_vignette_high_sexism, !level %in% c("NA", "Not Applicable"))

mm_by_vignette_high_sexism$`Sexist Attitudes` <- "High"

mm_by_vignette_sexism <- rbind(mm_by_vignette_high_sexism, mm_by_vignette_low_sexism)

mm_by_vignette_sexism_small <- filter(mm_by_vignette_sexism, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_sexism_smallplot <- ggplot(mm_by_vignette_sexism_small,
                                          aes(x = level,
                                              y = estimate,
                                              colour=`Sexist Attitudes`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")

theme_mine_double(gg_input_double = mm_by_vignette_sexism_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Sexist Attitudes`)) 

#ggsave(path = "figs", file = "mm_by_vignette_sexism_smallplot_alldata_nowt.jpg", width = 11, height = 5)

#####################################################
#* Fig 3 in main manuscript - diff in MM  ----
#####################################################
mm_by_vignette_alldata <- cregg::cj(c_dat, f1, 
                                    id = ~responseid, 
                                    by = ~vignette_info,
                                    estimate = "mm_diff")

mm_by_vignette_alldata <- filter(mm_by_vignette_alldata, !level %in% c("NA", "Not Applicable"))

# mm_by_vignette_plot_alldata <- ggplot(mm_by_vignette_alldata, 
#                                       aes(x = level, 
#                                           y = estimate)) +
#   coord_flip() +
#   facet_grid(feature ~ vignette_info, 
#              scales  ="free_y", 
#              space = "free",
#              switch = "y")
# theme_mine_single_contrast(gg_input_single_contrast = mm_by_vignette_plot_alldata)
# ggsave(path = "figs", file = "mm_by_vignette_plot_alldata_cont_nowt.jpg", width = 11, height = 4)


mm_by_vignette_alldata_small <- filter(mm_by_vignette_alldata, 
                                       feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_alldata_smallplot <- ggplot(mm_by_vignette_alldata_small, 
                                           aes(x = level, 
                                               y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_single_contrast(gg_input_single_contrast = mm_by_vignette_alldata_smallplot)
#ggsave(path = "figs", file = "mm_by_vignette_alldata_smallplot_cont_nowt.jpg", width = 11, height = 4)

#####################################################
#* Fig 18 in SI - diff in MM only for those who got the attention checks correct ----
#####################################################

mm_by_vignette_by_check <- cregg::cj(subset(c_dat, correct_vignette=="c"), f1, 
                                     id = ~responseid, 
                                     by = ~vignette_info,
                                     estimate = "mm_diff")

mm_by_vignette_by_check <- filter(mm_by_vignette_by_check, !level %in% c("NA", "Not Applicable"))

mm_by_vignette_by_check_small <- filter(mm_by_vignette_by_check, 
                                        feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_by_check_smallplot <- ggplot(mm_by_vignette_by_check_small, 
                                            aes(x = level, 
                                                y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_single_contrast(gg_input_single_contrast = mm_by_vignette_by_check_smallplot)
#ggsave(path = "figs", file = "mm_by_vignette_by_check_smallplot_cont_nowt.jpg", width = 11, height = 4)

#####################################################
#* Fig 19 - Intersectional MM contrast by vignette across men and women respondents ----
#####################################################
mm_genderx_vignette_r_male <- cregg::cj(subset(c_dat, r_gender=="Male"),
                                        picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                        id=~responseid, 
                                        estimate = "mm_diff", 
                                        by=~vignette_info)
mm_genderx_vignette_r_male <-  mm_genderx_vignette_r_male  %>%
  filter(!estimate == "NA") %>% 
  filter(!grepl("Not Applicable|NA", level)) %>% 
  mutate(feature = case_when(feature == "GenderXCaste" ~ "Gender x Caste",
                             feature == "GenderXlocal_gen_quota" ~ "Gender x Local gender quota",
                             feature == "GenderXlocal_caste_quota" ~ "Gender x Local caste quota", 
                             feature == "GenderXlocal_gen_quotaXlocal_caste_quota" ~ "Gender x Local caste quota  x Local caste quota"))

mm_genderx_vignette_r_male$'Respondent Gender' <- "Male"

mm_genderx_vignette_r_female <- cregg::cj(subset(c_dat, r_gender=="Female"),
                                          picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                          id=~responseid, 
                                          estimate = "mm_diff", 
                                          by=~vignette_info)

mm_genderx_vignette_r_female <-  mm_genderx_vignette_r_female  %>%
  filter(!estimate == "NA") %>% 
  filter(!grepl("Not Applicable|NA", level)) %>% 
  mutate(feature = case_when(feature == "GenderXCaste" ~ "Gender x Caste",
                             feature == "GenderXlocal_gen_quota" ~ "Gender x Local gender quota",
                             feature == "GenderXlocal_caste_quota" ~ "Gender x Local caste quota", 
                             feature == "GenderXlocal_gen_quotaXlocal_caste_quota" ~ "Gender x Local caste quota  x Local caste quota"))
#mm_genderx_vignette_r_female <- filter(mm_genderx_vignette_r_female, !level %in% c("NA", "Not Applicable"))
mm_genderx_vignette_r_female$'Respondent Gender' <- "Female"


mm_genderx_vignette_r_gender <- rbind(mm_genderx_vignette_r_female, mm_genderx_vignette_r_male)
mm_genderx_vignette_r_genderplot <- ggplot(mm_genderx_vignette_r_gender,
                                           aes(x = level,
                                               y = estimate,
                                               colour=`Respondent Gender`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")
theme_mine_double_contrast(gg_input_double_contrast = mm_genderx_vignette_r_genderplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Respondent Gender`)) 
ggsave(path = "figs", file = "mm_genderx_vignette_r_genderplot_alldata_cont_nowt.jpg", width = 13, height = 5)

#####################################################
#* Fig 20 in SI - MM analysis by vignette across age ----
#####################################################

mm_by_vignette_r_age_1824 <- cregg::cj(subset(c_dat, r_age=="18-24"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm_diff")
mm_by_vignette_r_age_1824 <- filter(mm_by_vignette_r_age_1824, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_1824$`Respondent Age` <- "18-24"


mm_by_vignette_r_age_2534 <- cregg::cj(subset(c_dat, r_age=="25-34"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm_diff")
mm_by_vignette_r_age_2534 <- filter(mm_by_vignette_r_age_2534, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_2534$`Respondent Age` <- "25-34"


mm_by_vignette_r_age_3544 <- cregg::cj(subset(c_dat, r_age=="35-44"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm_diff")
mm_by_vignette_r_age_3544 <- filter(mm_by_vignette_r_age_3544, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_3544$`Respondent Age` <- "35-44"

mm_by_vignette_r_age_4554 <- cregg::cj(subset(c_dat, r_age=="45-54"), f1, 
                                       id = ~responseid, 
                                       by = ~vignette_info,
                                       estimate = "mm_diff")
mm_by_vignette_r_age_4554 <- filter(mm_by_vignette_r_age_4554, !level %in% c("NA", "Not Applicable"))
mm_by_vignette_r_age_4554$`Respondent Age` <- "45-54"

mm_by_vignette_r_age <- rbind(mm_by_vignette_r_age_1824, mm_by_vignette_r_age_2534, mm_by_vignette_r_age_3544, mm_by_vignette_r_age_4554)


mm_by_vignette_r_age_small <- filter(mm_by_vignette_r_age, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))
mm_by_vignette_r_age_smallplot <- ggplot(mm_by_vignette_r_age_small,
                                         aes(x = level,
                                             y = estimate,
                                             colour=`Respondent Age`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")

theme_mine_quad_contrast(gg_input_quad_contrast = mm_by_vignette_r_age_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Respondent Age`)) 
#ggsave(path = "figs", file = "mm_by_vignette_r_age_smallplot_alldata_cont_nowt.jpg", width = 11, height = 5)

#####################################################
#* Fig 21 - Intersectional MM contrast by vignette across caste ----
#####################################################
mm_genderx_vignette_r_gen <- cregg::cj(subset(c_dat, r_caste1=="Non-SC/ST"),
                                       picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                       id=~responseid, 
                                       estimate = "mm_diff", 
                                       by=~vignette_info)
mm_genderx_vignette_r_gen <-  mm_genderx_vignette_r_gen  %>%
  filter(!estimate == "NA") %>% 
  filter(!grepl("Not Applicable|NA", level)) %>% 
  mutate(feature = case_when(feature == "GenderXCaste" ~ "Gender x Caste",
                             feature == "GenderXlocal_gen_quota" ~ "Gender x Local gender quota",
                             feature == "GenderXlocal_caste_quota" ~ "Gender x Local caste quota", 
                             feature == "GenderXlocal_gen_quotaXlocal_caste_quota" ~ "Gender x Local caste quota  x Local caste quota"))
mm_genderx_vignette_r_gen$'Respondent Caste' <- "Non-SC/ST"

mm_genderx_vignette_r_scst <- cregg::cj(subset(c_dat, r_caste1=="SC/ST"),
                                        picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                        id=~responseid, 
                                        estimate = "mm_diff", 
                                        by=~vignette_info)

mm_genderx_vignette_r_scst <-  mm_genderx_vignette_r_scst  %>%
  filter(!estimate == "NA") %>% 
  filter(!grepl("Not Applicable|NA", level)) %>% 
  mutate(feature = case_when(feature == "GenderXCaste" ~ "Gender x Caste",
                             feature == "GenderXlocal_gen_quota" ~ "Gender x Local gender quota",
                             feature == "GenderXlocal_caste_quota" ~ "Gender x Local caste quota", 
                             feature == "GenderXlocal_gen_quotaXlocal_caste_quota" ~ "Gender x Local caste quota  x Local caste quota"))
mm_genderx_vignette_r_scst$'Respondent Caste' <- "SC/ST"
mm_genderx_vignette_r_caste <- rbind(mm_genderx_vignette_r_gen, mm_genderx_vignette_r_scst)
mm_genderx_vignette_r_casteplot <- ggplot(mm_genderx_vignette_r_caste,
                                          aes(x = level,
                                              y = estimate,
                                              colour=`Respondent Caste`)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y")
theme_mine_double_contrast(gg_input_double_contrast = mm_genderx_vignette_r_casteplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Respondent Caste`)) 
#ggsave(path = "figs", file = "mm_genderx_vignette_r_casteplot_alldata_cont_nowt.jpg", width = 13, height = 5)


#########################
##* Fig 22 in SI - Test of Hypothesis H17 ----
##########################

c_dat$conjoint <- as.numeric(c_dat$conjoint)
c_dat$`Waves` <- as.factor(ifelse(c_dat$conjoint <= 6 & !is.na(c_dat$conjoint),
                                  "First 6 Waves", "Last 6 Waves"))
mm_bywave <- cregg::cj(c_dat, f1, 
                       id = ~responseid, 
                       by = ~`Waves`,
                       estimate = "mm")
mm_bywave <- filter(mm_bywave, !level %in% c("NA", "Not Applicable"))


mm_bywave_plot <- ggplot(mm_bywave, aes(x = level,
                                        y = estimate,
                                        colour = `Waves`)) +
  coord_flip() +
  facet_grid(feature ~ .,
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_double(gg_input_double = mm_bywave_plot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Waves`))
#ggsave(path = "figs", file = "mm_byconjointhalves_fullplot_alldata.jpg", width = 11, height = 4)

#########################
##* Fig 23 in SI - Tests of Hypothesis H18 ----
##########################
mm_wom_res33 <- cregg::cj(subset(c_dat, women_res50=="0"), f1, 
                          id = ~responseid, 
                          estimate = "mm")
mm_wom_res33 <- filter(mm_wom_res33, !level %in% c("NA", "Not Applicable"))
mm_wom_res33$`Women's Reservation` <- "33%"

mm_wom_res50 <- cregg::cj(subset(c_dat, women_res50=="1"), f1, 
                          id = ~responseid, 
                          estimate = "mm")
mm_wom_res50 <- filter(mm_wom_res50, !level %in% c("NA", "Not Applicable"))
mm_wom_res50$`Women's Reservation` <- "50%"

mm_wom_res <- rbind(mm_wom_res33, mm_wom_res50)
mm_wom_res_small <- filter(mm_wom_res, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))
mm_wom_res_smallplot <- ggplot(mm_wom_res_small,
                               aes(x = level,
                                   y = estimate,
                                   colour=`Women's Reservation`)) +
  coord_flip() +
  facet_grid(feature ~ ., 
             scales  ="free_y", 
             space = "free",
             switch = "y")
theme_mine_double(gg_input_double = mm_wom_res_smallplot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Women's Reservation`)) 
#ggsave(path = "figs", file = "mm_fullplot_womres_smallplot_alldata_nowt.jpg", width = 11, height = 5)

#########################
##* Fig 24 in SI Tests of Hypothesis H19 ----
##########################

mm_genderx_vignette_res33 <- cregg::cj(subset(c_dat, women_res50=="0"), 
                                       picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                       id=~responseid, 
                                       estimate = "mm")
mm_genderx_vignette_res50 <- cregg::cj(subset(c_dat, women_res50=="1"), 
                                       picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                       id=~responseid, 
                                       estimate = "mm")
# remove NAs. Label:
mm_genderx_vignette_res33 <- mm_genderx_vignette_res33 %>%
  filter(!estimate == "NA") %>%
  filter(!grepl("Not Applicable|NA", level))

mm_genderx_vignette_res50 <- mm_genderx_vignette_res50 %>% 
  filter(!estimate == "NA") %>%
  filter(!grepl("Not Applicable|NA", level))

mm_genderx_vignette_res33$`Women's Reservation` <- "33%"
mm_genderx_vignette_res50$`Women's Reservation` <- "50%"

# combine:
mm_genderx_vignette <- rbind(mm_genderx_vignette_res33,
                             mm_genderx_vignette_res50)

mm_genderx_vignette <-  mm_genderx_vignette  %>%
  dplyr::mutate(feature = case_when(feature == "GenderXCaste" ~ "Gender x Caste",
                                    feature == "GenderXlocal_gen_quota" ~ "Gender x Local gender quota",
                                    feature == "GenderXlocal_caste_quota" ~ "Gender x Local caste quota", 
                                    feature == "GenderXlocal_gen_quotaXlocal_caste_quota" ~ "Gender x Local caste quota  x Local caste quota"))

mm_genderx_vignette$feature <- factor(mm_genderx_vignette$feature, 
                                      levels = c("Gender x Caste", "Gender x Local gender quota", "Gender x Local caste quota", "Gender x Local caste quota  x Local caste quota"))

mm_genderx_vignette_plot <- ggplot(mm_genderx_vignette, 
                                   aes(x = level, 
                                       y = estimate,
                                       colour = `Women's Reservation`)) +
  coord_flip() +
  facet_grid(feature ~ ., 
             scales  ="free_y", 
             space = "free",
             switch = "y")
theme_mine_double(gg_input_double = mm_genderx_vignette_plot)  + geom_point(size = 1,  position = position_dodge(0.3), aes(shape=`Women's Reservation`)) 
#ggsave(path = "figs", file = "mm_fullplot_womres_alldata.jpg", width = 13, height = 5)






#####################################################
#* Analysis by survey weights for Fig 14-16 in SI ----
#####################################################

# collapse on respondent level data
respondents <- c_dat %>% 
  select(responseid, ipaddress, r_age, r_religion, r_edu, r_income, r_gender, r_caste, voter_id,voted_nat_elect, voted_state_elect,interact_poli,interact_pol_level,pol_interest,dep_pm,party_support,women_res_approv,caste_res_approv,women_mp,wom_unfit_patriarchal,wom_unfit_dirtypol,gen_intelligent,wom_discrim, caste_discrim,indians_fair,women_res50, vignette) %>% 
  #na.omit() %>% 
  group_by(responseid) %>% 
  distinct()

# drop respondents with the missing variables we'll weight on:
respondents.noNA <- respondents %>% drop_na(r_gender, r_religion, r_caste)
# We'll also need to drop any sample strata that are absent from our population strata:
respondents.noNA <- subset(respondents.noNA, r_religion != "Prefer not to say")

# let's weight on the following:
# Gender (target pop: 48.4% female, 51.6% male. Source: 2022 data from https://data.worldbank.org/indicator/SP.POP.TOTL.FE.ZS?locations=IN)
prop.table(table(respondents.noNA$r_gender))
# Religion (target pop: 79.8% Hindu, 14.2% Muslim, 2.3% Christian, 1.7% Sikh, 2% other group. Source: 2011 data from https://www.pewresearch.org/short-reads/2021/09/21/key-findings-about-the-religious-composition-of-india/ )
prop.table(table(respondents.noNA$r_religion))
# Caste (https://www.pewresearch.org/decoded/2021/06/29/measuring-caste-in-india/). Data are from 2020
prop.table(table(respondents.noNA$r_caste)) # General includes OBC, which hasn't been included on census recently so we're going to use these 3 categories (2011 census number): 16.6% SC, 8.6% STs, 74.8% Gen/OBC 

# now produce weights:
respondents.unweighted <- svydesign(ids =~1, data = respondents.noNA)
gender.dist <- data.frame(r_gender = c("Male","Female"),
                          Freq = nrow(respondents.noNA)*c(.516,.484))
religion.dist <- data.frame(r_religion = c("Hindu","Muslim","Christian",
                                           "Sikh","Other"), # H, M, C, S, Oth
                            Freq = nrow(respondents.noNA)*c(.798, .142, .023,
                                                            .017, .02))
caste.dist <- data.frame(r_caste = c("General", "SC", "ST"),
                         Freq = nrow(respondents.noNA)*c(0.748, 0.166, 0.086))
data.rake <- rake(design = respondents.unweighted,
                  sample.margins = list(~r_gender, ~r_religion, ~r_caste),
                  population.margins = list(gender.dist, religion.dist,
                                            caste.dist))


# calculate weight effect. If far from 1, might suggest more trimming is needed
max(weights(data.rake))/min(weights(data.rake))
summary(weights(data.rake))
hist(weights(data.rake))
quantile(weights(data.rake), probs = c(.95)) # we'll trim upper limit and lower limit 5%
data.rake <- trimWeights(data.rake, 
                         lower = quantile(weights(data.rake), probs = c(.05)),
                         upper = quantile(weights(data.rake), probs = c(.95)))
max(weights(data.rake))/min(weights(data.rake))
summary(weights(data.rake))

# now merge weights with respondents: 
respondents.noNA$weights <- weights(data.rake)

# This produces a list of the trimmed weights used:
stack(table(respondents.noNA$weights))

# This indicates the population percentages for gender when weights are applied:
svytable(~respondents.noNA$r_gender, data.rake)
prop.table(svytable(~respondents.noNA$r_gender, data.rake)) # so not perfect to the population (likely due to trim?) but previously was 70% male dataset. Post-weighting now balanced to 52%, very close to population value of 51.6%

# next merge respondents.noNA (just weights and responseID) back into c_dat:
respondents.noNA <- respondents.noNA %>% 
  select(responseid, weights)

dat <- merge(c_dat, respondents.noNA, by = "responseid", all.x = TRUE, all.y = TRUE)

# assert it works:
cbind(dat$responseid[1:100], dat$weights[1:100])
# and drop if we don't have the weight for the respondent
dat <- dat %>% drop_na(weights)
dev.off()


# pick variables of choice 
c_dat <- dat %>% 
  select(responseid, ipaddress, conjoint, picked, Gender, Caste ,`Local gender quota`, `Local caste quota`, Religion, `Dynastic family`, `Local political experience`, `Age`, `Party type`, `Caste reservation on MLA seat`, vignette, vignette_info, r_gender, r_caste, r_religion, r_age, r_edu, q45, voter_id, pol_interest,interact_poli,interact_pol_level, voted_nat_elect, voted_state_elect, dep_pm, party_support, women_mp, women_res_approv, caste_res_approv, wom_unfit_patriarchal, wom_unfit_dirtypol,wom_discrim, caste_discrim, gen_intelligent, indians_fair, women_res50, correct_vignette, r_income, weights) 

#* recode respondent's caste variable to make a binary variable of attributes Non-SCST and SCST 
c_dat$r_caste1 <- NA
c_dat$r_caste1[c_dat$r_caste == "General"] <- "Non-SC/ST"
c_dat$r_caste1[c_dat$r_caste == "SC"] <- "SC/ST"
c_dat$r_caste1[c_dat$r_caste == "ST"] <- "SC/ST"
c_dat$r_caste1 <- as.factor(c_dat$r_caste1)
c_dat$r_caste1 <- NA

#####################################################
##* Fig 14 in SI - replication of main results with survey weights ----
##################################################### 
mm <- cregg::cj(c_dat, f1, 
                id = ~responseid, 
                estimate = "mm",
                weights = ~ weights)

mm <- filter(mm, !level %in% c("NA", "Not Applicable"))

mm <- filter(mm, feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_plot <- ggplot(mm, aes(x = level, y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ .,
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_single(gg_input_single = mm_plot)
#ggsave(path = "figs", file = "mm_plot-weights.jpg", width = 11, height = 4)

#########################
##* Fig 15 in SI - MM by vignette with survey weights----
##########################
mm_by_vignette_alldata <- cregg::cj(c_dat, f1, 
                                    id = ~responseid, 
                                    by = ~vignette_info,
                                    estimate = "mm",
                                    weights = ~weights)

mm_by_vignette_alldata <- filter(mm_by_vignette_alldata, !level %in% c("NA", "Not Applicable"))

mm_by_vignette_alldata <- filter(mm_by_vignette_alldata, 
                                 feature %in% c("Gender", "Caste", "Local gender quota", "Local caste quota"))

mm_by_vignette_alldata_smallplot <- ggplot(mm_by_vignette_alldata, 
                                           aes(x = level, 
                                               y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") 

theme_mine_single(gg_input_single = mm_by_vignette_alldata_smallplot)
#ggsave(path = "figs", file = "fig15.jpg", width = 11, height = 4)

#########################
##* Fig 16 in SI - intersectional MM by vignette with survey weights ----
##########################
c_dat$GenderXCaste <- with(c_dat, 
                           interaction(Gender, Caste), 
                           drop=T)
c_dat$GenderXlocal_gen_quota <- with(c_dat, 
                                     interaction(Gender, `Local gender quota`), 
                                     drop=T)
c_dat$GenderXlocal_caste_quota <- with(c_dat, 
                                       interaction(Gender, `Local caste quota`), 
                                       drop=T)
c_dat$GenderXlocal_gen_quotaXlocal_caste_quota <- with(c_dat, 
                                                       interaction(Gender, `Local gender quota`,`Local caste quota`), 
                                                       drop=T)

mm_genderx_vignette <- cregg::cj(c_dat, 
                                 picked ~ GenderXCaste+ GenderXlocal_gen_quota + GenderXlocal_caste_quota + GenderXlocal_gen_quotaXlocal_caste_quota, 
                                 id=~responseid, 
                                 estimate = "mm", 
                                 by=~vignette_info,
                                 weights = ~weights)

mm_genderx_vignette <-  mm_genderx_vignette  %>%
  filter(!estimate == "NA") %>% 
  filter(!grepl("Not Applicable|NA", level)) %>% 
  mutate(feature = case_when(feature == "GenderXCaste" ~ "Gender x Caste",
                             feature == "GenderXlocal_gen_quota" ~ "Gender x Local gender quota",
                             feature == "GenderXlocal_caste_quota" ~ "Gender x Local caste quota", 
                             feature == "GenderXlocal_gen_quotaXlocal_caste_quota" ~ "Gender x Local caste quota  x Local caste quota"))

mm_genderx_vignette$feature <- factor(mm_genderx_vignette$feature, 
                                      levels = c("Gender x Caste", "Gender x Local gender quota", "Gender x Local caste quota", "Gender x Local caste quota  x Local caste quota"))


mm_genderx_vignette_plot <- ggplot(mm_genderx_vignette, 
                                   aes(x = level, 
                                       y = estimate)) +
  coord_flip() +
  facet_grid(feature ~ vignette_info, 
             scales  ="free_y", 
             space = "free",
             switch = "y") +  theme(panel.spacing.x = unit(.25, "cm")) +
  theme(panel.spacing.y = unit(.25, "cm"))

theme_mine_single(gg_input_single =mm_genderx_vignette_plot)
#ggsave(path = "figs", file = "mm_genderx_vignette_plot_alldata-weights.jpg", width = 13, height = 5)
