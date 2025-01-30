# project: Sustainable Representation through Electoral Quotas: Evidence from India
# authors: Komal Preet Kaur and Andrew Q. Philips
# date: 2024-01-06
# task: Fig 1,2, and 4 in SI and Tables 1-4 in SI 

#* load packages ----
library(pacman)
p_load(haven, janitor, tidyverse, excluder, xtable, ggpubr, dotwhisker) 

#* load data ----
c_dat <- read_rds("clean_conjoint_india_all.rds") 

#* get respondent-level data ----
respondents <- c_dat %>% 
  select(responseid, ipaddress, r_age, r_religion, r_edu, r_income, r_gender, r_caste, voter_id,voted_nat_elect, voted_state_elect,interact_poli,interact_pol_level,pol_interest,dep_pm,party_support,women_res_approv,caste_res_approv,women_mp,wom_unfit_patriarchal,wom_unfit_dirtypol,gen_intelligent,wom_discrim, caste_discrim,indians_fair, women_res50, vignette, q45, correct_vignette, vignette_info) %>% 
  group_by(responseid) %>% 
  distinct()

#* code five-point Likert scale to three-point scale for four questions ----
respondents <- 
  respondents %>% 
  #qus: do you approve of women's reservation in politics?
  mutate(women_res_approv_cat3 = case_when(women_res_approv == "Strongly agree" ~ "Approve",
                                           women_res_approv == "Somewhat agree" ~ "Approve",
                                           women_res_approv == "Strongly disagree" ~ "Disapprove",
                                           women_res_approv == "Somewhat disagree" ~ "Disapprove",
                                           women_res_approv == "Neither agree nor disagree" ~ "Neutral"),
         #qus: do you approve of caste's reservation in politics?
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
                                        caste_discrim == "Neither agree nor disagree" ~ "Neutral"))
##########################
#* Fig 1 in SI - create a graph on respondents' views on gender and caste reservation  ----
##########################

# summary stats for qus: do you approve of women's reservation in politics
women_res_approv <- respondents %>% 
  tabyl(women_res_approv_cat3) %>% 
  adorn_pct_formatting()
women_res_approv

#summary stats for qus: do you approve of caste-based reservation in politics
caste_res_approv <- respondents %>% 
  tabyl(caste_res_approv_cat3) %>% 
  adorn_pct_formatting()
caste_res_approv

women_res_approv$Reservation <- 'Women Reservation'
caste_res_approv$Reservation <- 'Caste Reservation'

women_res_approv <- women_res_approv %>% rename('Category' = women_res_approv_cat3) 
caste_res_approv <- caste_res_approv %>% rename('Category' = caste_res_approv_cat3) 

res_approv <- rbind(women_res_approv, caste_res_approv)
res_approv$percent <- gsub('%', "", res_approv$percent)
res_approv$percent <- as.numeric(res_approv$percent)

res_approv_fulldata <- ggbarplot(res_approv, "Category", "percent",
                                 fill = "Reservation", color = "Reservation", 
                                 palette = "grey", position = position_dodge(0.75), 
                                 lab.pos = "out", ylab = "Percent", 
                                 legend = "bottom", xlab=F, 
                                 ggtheme = theme_minimal(), label = T, 
                                 lab.size = 3, order = c("Disapprove", "Neutral", "Approve")) + font("ylab", size=13) 
res_approv_fulldata
#ggsave(path = "figs", file = "fig1SI.jpg", width = 8, height = 5)

##########################
#* Fig 2 in SI - create a graph on respondents' views on gender and caste reservation  ----
##########################
wom_discrim <- respondents %>% 
  tabyl(wom_discrim_cat3) %>% 
  adorn_pct_formatting()
wom_discrim

caste_discrim <- respondents %>% 
  tabyl(caste_discrim_cat3) %>% 
  adorn_pct_formatting()
caste_discrim


wom_discrim$Discrimination <- 'Gender Discrimination'
caste_discrim$Discrimination <- 'Caste Discrimination'

wom_discrim <- wom_discrim %>% rename ('Category' = wom_discrim_cat3) 
caste_discrim <- caste_discrim %>% rename ('Category' = caste_discrim_cat3) 

discrimination <- rbind(wom_discrim, caste_discrim)
discrimination$percent <- gsub('%', "", discrimination$percent)
discrimination$percent <- as.numeric(discrimination$percent)

discrimination_fulldata <- ggbarplot(discrimination, "Category", "percent",
                                     fill = "Discrimination", color = "Discrimination", 
                                     palette = "grey", position = position_dodge(0.75), 
                                     lab.pos = "out", ylab = "Percent", 
                                     legend = "bottom", xlab=F, 
                                     ggtheme = theme_minimal(), label = T, 
                                     lab.size = 3, order = c("Disagree", "Neutral", "Agree")) + font("ylab", size=13) 
discrimination_fulldata
#ggsave(path = "figs", file = "fig2SI.jpg", width = 8, height = 5)

##########################
#* Table 1 in SI  ----
##########################
gender_caste <- round(prop.table(table(respondents$r_gender, respondents$r_caste)),2)
gender_caste
#print(xtable(gender_caste, type = "latex"), file = "gender_caste.tex")


##########################
#* Table 2 in SI  ----
##########################

r_age <- respondents %>% 
  tabyl(r_age) %>% 
  adorn_pct_formatting()
r_age
#print(xtable(r_age, type = "latex"), file = "r_age.tex")

##########################
#* Table 3 in SI  ----
##########################
r_religion <- respondents %>% 
  tabyl(r_religion) %>% 
  adorn_pct_formatting()
r_religion
print(xtable(r_religion, type = "latex"), file = "r_religion.tex")

##########################
#* Table 4 in SI  ----
##########################

vignette_info_pc <- respondents %>% 
  tabyl(vignette_info) %>% 
  adorn_pct_formatting()
vignette_info_pc
#print(xtable(vignette_info_pc, type = "latex"), file = "vignette_info_pc.tex")


##########################
#* Fig 4 in SI  ----
##########################
# recode DVs to dichotomous: Agree or Disagree/neutral
respondents <- 
  respondents %>% 
  #qus: do you approve of women's reservation in politics?
  mutate(women_res_approv_cat2 = case_when(women_res_approv == "Strongly agree" ~ 1,
                                           women_res_approv == "Somewhat agree" ~ 1,
                                           women_res_approv == "Strongly disagree" ~ 0,
                                           women_res_approv == "Somewhat disagree" ~ 0,
                                           women_res_approv == "Neither agree nor disagree" ~ 0),
         #qus: do you approve of caste's reservation in politics?
         caste_res_approv_cat2 =  case_when(caste_res_approv == "Strongly agree" ~ 1,
                                            caste_res_approv == "Somewhat agree" ~ 1,
                                            caste_res_approv == "Strongly disagree" ~ 0,
                                            caste_res_approv == "Somewhat disagree" ~ 0,
                                            caste_res_approv == "Neither agree nor disagree" ~ 0),
         #women are not discriminated against in politics.
         wom_discrim_cat2 = case_when(wom_discrim == "Strongly agree" ~ 0, #reverse the scale
                                      wom_discrim == "Somewhat agree" ~ 0,
                                      wom_discrim == "Strongly disagree" ~ 1,
                                      wom_discrim == "Somewhat disagree" ~ 1,
                                      wom_discrim == "Neither agree nor disagree" ~ 0),
         #people are not discriminated against based on caste in politics.
         caste_discrim_cat2 = case_when(caste_discrim == "Strongly agree" ~ 0, #reverse the scale
                                        caste_discrim == "Somewhat agree" ~ 0,
                                        caste_discrim == "Strongly disagree" ~ 1,
                                        caste_discrim == "Somewhat disagree" ~ 1,
                                        caste_discrim == "Neither agree nor disagree" ~ 0))
table(respondents$women_res_approv_cat2) # e.g.


# recode the variable relative to actual % of women MP and make it a scale like <5 points off, <10 points off, 10-20 points off
summary(respondents$women_mp) # guess at % of women in politics
# express relative to actual % of women in Lok Sabha (14.94% as of Dec 2022)
respondents$womenmp.index <- respondents$women_mp - 14.94
# next we'll recode as the following:
# 5 = 3 points or less off
# 4 = 3-10 points off
# 3 = 10-20 points off
# 2 = 20-30 points off
# 1 = more than 30 points off 
#
respondents <- 
  respondents %>% 
  #qus: do you approve of women's reservation in politics?
  mutate(womenmp.index = case_when(abs(women_mp - 14.94) <= 3 ~ 5,
                                   abs(women_mp - 14.94) > 3 & abs(women_mp - 14.94) <= 10 ~ 4,
                                   abs(women_mp - 14.94) > 10 & abs(women_mp - 14.94) <= 20 ~ 3,
                                   abs(women_mp - 14.94) > 20 & abs(women_mp - 14.94) <= 30 ~ 2,
                                   abs(women_mp - 14.94) > 30 ~ 1))
table(respondents$womenmp.index)

#  IVs?
table(respondents$r_age) 
table(respondents$r_religion)
table(respondents$pol_interest)
table(respondents$party_support)
#create binary vars for respondent attributes needed for logit 
respondents <- 
  respondents %>% 
  mutate(hindu = case_when(r_religion == "Hindu" ~ 1,
                           r_religion != "Hindu" ~ 0),
         female = case_when(r_gender == "Female" ~ 1,
                            r_gender == "Male" ~ 0),
         general.caste = case_when(r_caste == "General" ~ 1,
                                   r_caste != "General" ~ 0),
         political.interest = case_when(pol_interest == "Not interinterested" ~ 0,
                                        pol_interest == "Somewhat interested" ~ 1,
                                        pol_interest == "Very interested" ~ 2),
         support.BJP = case_when(party_support == "Bharatiya Janata Party (BJP)" ~ 1,
                                 party_support != "Bharatiya Janata Party (BJP)" ~ 0),
         support.INC = case_when(party_support == "Indian National Congress (INC)" ~ 1,
                                 party_support != "Indian National Congress (INC)" ~ 0))
table(respondents$hindu)
table(respondents$political.interest)

respondents$r_age <- as.numeric(respondents$r_age)
respondents$r_income <- as.numeric(respondents$r_income)

# we also want the Q on political knowledge:
table(respondents$dep_pm) # correct ans: there isn't a deputy PM
respondents <- 
  respondents %>% 
  mutate(know.dep.pm = case_when(dep_pm == "None of the above" ~ 1,
                                 dep_pm != "None of the above" ~ 0))
table(respondents$know.dep.pm)

# ----------- Respondent view on Gender discrimination --------
gen.discrim <- glm(wom_discrim_cat2 ~ female + general.caste + womenmp.index + hindu + r_age + r_income + political.interest + know.dep.pm + support.BJP + support.INC, data = respondents, family = binomial) # wom_discrim_cat2 DV = 1 if disagree or strongly disagree that women are not discriminated against in politics
summary(gen.discrim)


# ----------- Respondent view on Caste discrimination --------
caste.discrim <- glm(caste_discrim_cat2 ~ female + general.caste + womenmp.index + hindu + r_age + r_income + political.interest + know.dep.pm + support.BJP + support.INC, data = respondents, family = binomial) # caste_discrim_cat2 = 1 if disagree or strongly disagree that people are not discriminated against based on caste in politics
summary(caste.discrim)


# ----------- Respondent view of approve Gender reservation in politics --------
wom.approve.res <- glm(women_res_approv_cat2 ~ female + general.caste + womenmp.index + hindu + r_age + r_income + political.interest + know.dep.pm + support.BJP + support.INC, data = respondents, family = binomial) # women_res_approv_cat2 DV = 1 if approve of women's reservation in politics
summary(wom.approve.res)

# ----------- Respondent view of approve Caste reservation in politics --------
caste.approve.res <- glm(caste_res_approv_cat2 ~ female + general.caste + womenmp.index + hindu + r_age + r_income + political.interest + know.dep.pm + support.BJP + support.INC, data = respondents, family = binomial) # caste_res_approv_cat2 DV = 1 if approve of caste reservation in politics
summary(caste.approve.res)

length(gen.discrim$residuals)
length(caste.discrim$residuals)
length(wom.approve.res$residuals)
length(caste.approve.res$residuals)

# now let's plot these: 
# for the approve of caste/gender reservations:
dwplot(list(wom.approve.res,caste.approve.res),
       vline = geom_vline(xintercept = 0) #,
) %>% 
  relabel_predictors(
    c(
      female = "Female",
      r_age = "Age",
      hindu = "Hindu",
      general.caste = "General Caste",
      r_income = "Income",
      political.interest = "Political Interest",
      know.dep.pm = "Knows Dep. PM",
      support.BJP = "BJP",
      support.INC = "INC",
      womenmp.index = "Guess at % of Women MPs"
    )
  ) +
  theme_minimal() +
  ggtitle("Approve of Reservation") + xlab("Log-odds coefficient") +
  scale_colour_grey(labels = c("Women's Reservation", "Caste Reservation"))
#ggsave(path = "figs", file = "logit-approveofreservation.jpg", width = 8, height = 4)




dwplot(list(gen.discrim,caste.discrim),
       vline = geom_vline(xintercept = 0)
) %>% 
  relabel_predictors(
    c(
      female = "Female",
      r_age = "Age",
      hindu = "Hindu",
      general.caste = "General Caste",
      r_income = "Income",
      political.interest = "Political Interest",
      know.dep.pm = "Knows Dep. PM",
      support.BJP = "BJP",
      support.INC = "INC",
      womenmp.index = "Guess at % of Women MPs"
    )
  ) +
  theme_minimal() +
  ggtitle("Think Discrimination Still Exists") + xlab("Log-odds coefficient") +
  scale_colour_grey(labels = c("Caste Discrimination", "Gender Discrimination"))
#ggsave(path = "figs", file = "logit-discrimination.jpg", width = 8, height = 4)



