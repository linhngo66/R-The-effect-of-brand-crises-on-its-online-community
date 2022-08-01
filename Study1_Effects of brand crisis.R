library(dplyr)
library(tidyverse)
library(data.table)
library(sentimentr)
library(textclean)
library(tm)
library(stringr)
library(magrittr)
library(car)
library(stargazer)

dir <- "C:/Users/Admin/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/Team5_text.csv"
comment <- read.csv(dir, stringsAsFactors = FALSE, encoding="UTF-8")

dir_comp <- "C:/Users/Admin/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/companies_and_crises.csv"
company <- read.csv(dir_comp)

dir_team <- "C:/Users/Admin/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/Team5.csv"                       
team <- read.csv(dir_team)

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ################## Prep ################
#Find the min and max created_utc in table Team
team$created_utc <- as.POSIXct(team$created_utc)

max_date <- max(team$created_utc)
min_date <- min(team$created_utc)

#Filter table Comment based on the timeframe in table Team
comment <- rename(comment, created_utc = date)
comment$created_utc <- as.POSIXct(comment$created_utc)

comment_time <- comment %>%
  filter(created_utc <= max_date & created_utc >= min_date)

#Since table Team only include comments from 2 subreddit, we take table Comment as base
#Only use table Team to choose the timeframe and scandaldate
subreddit <- unique(comment$subreddit)
subreddit_team <- unique(team$subreddit)

#Merge the scandal date and comments together
team_scandaldate <- as.data.frame(unique(team$reference_scandaldate))
comment_merge <- comment_time %>% 
  merge(., team_scandaldate, by = NULL) %>%
  rename("reference_scandaldate" = "unique(team$reference_scandaldate)")

#Drop rows with empty comments
comment_full <- comment_merge %>%
  filter(selftext != "")

comment_full$reference_scandaldate <- as.POSIXct(comment_full$reference_scandaldate)

#Add treatment variable: Comment made Before (0) or After (1) the crisis
comment_treat <- comment_full %>%
  group_by(subreddit, reference_scandaldate) %>%
  mutate(flag_treatment = ifelse(created_utc >= reference_scandaldate, 1, 0))

#Export 
#comment_treat$selftext <- enc2utf8(comment_treat$selftext)
#final_dir <- "C:/Users/523203tn/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/text_final.csv"
#fwrite(comment_treat, final_dir, bom = T)

################## Sentiment analysis################

clean_text <- function (text) {
  rm_cmt <- "Your submission was automatically removed because v.redd.it is not an approved site. This has done to reduce spam on the sub. If this was an error or you have a question regarding this new rule you can \\[message the mods\\]\\( am a bot, and this action was performed automatically. Please \\[contact the moderators of this subreddit\\]\\(\\/message\\/compose\\/\\?to skeptical if you have any questions or concerns\\.\\*"
  
  text %>%
    replace_url() %>%
    replace_curly_quote() %>%
    replace_contraction() %>%
    replace_html() %>%
    replace_emoji() %>%
    replace_emoticon() %>%
    gsub("<[a-zA-Z0-9]+>", " ", .) %>% #remove emojis not translated
    gsub(">", " ", .) %>% #remove symbols translated from html 
    gsub("#", " ", .) %>% #remove hash
    gsub("\\-{3,}\\s*\\^.*", " ", .) %>% #remove end note generated by bot
    gsub("r/[a-zA-Z0-9]+\\S", " ", .) %>% #remove subreddit names
    gsub("\\n", ".", .) %>%
    gsub(rm_cmt, "", .) %>%
    replace_kern() %>%
    replace_non_ascii() %>%
    replace_internet_slang() %>%
    replace_rating() %>%
    stripWhitespace()
}

text <- data.frame(selftext = unique(comment_treat$selftext))
text$id <- text$selftext

cmt <- c(text$selftext)
cmt <- clean_text(cmt)
text$selftext <- cmt

stm <- get_sentences(text$selftext)
max(stm$element_id)
stm <- sentiment_by(stm)
text$sentiment <- stm$ave_sentiment

#merge score to the big dataset
comment_smt <- comment_treat %>%
  left_join(., text, by = c("selftext" = "id")) %>%
  select(-c("selftext")) %>%
  rename(selftext = selftext.y) %>%
  .[, c(6, 1:5, 7)]

#remove empty rows
comment_final <- comment_smt %>%
  filter(selftext != "", 
         !is.na(selftext), 
         selftext != "[deleted]", 
         selftext != "[removed]")

#final_dir <- "C:/Users/523203tn/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/smt_analysis_final.csv"
#write.csv(comment_final, final_dir, row.names = F)

################## Add control variables ###########
company$news_date <- as.POSIXct(company$news_date)
company$subreddit <- company$subreddit %>%
  tolower() %>%
  as.factor()

#Create dummy variable for related issue of each crisis
tinder <- company %>%
  filter(company %like% "Tinder" & news_date %in% unique(comment_final$reference_scandaldate)) %>%
  rename(reference_scandaldate = news_date)

issue <- c("Poor employment conditions",
           "Human rights abuses and corporate complicity",
           "Fraud",
           "Products (health and environmental issues)",
           "Social discrimination",
           "Discrimination in employment",
           "Violation of national legislation",
           "Anti-competitive practices")

tinder[, issue] <- NA

for (i in (1:length(tinder$related_issues))) {
  related_issue <- str_split(tinder$related_issues[i], ";")
  
  for (x in related_issue[[1]]) {
    tinder[i, x] <- 1
  }
  
  x <- issue[which(!issue %in% related_issue[[1]])]
  tinder[i,x] <- 0
}

tinder[, issue] <- lapply(tinder[, issue], as.factor)

#Add control variables
cmt_control <- comment_final %>%
  left_join(., tinder, by = c("subreddit", "reference_scandaldate")) %>%
  select(-c("RepRisk_ID", "RepRisk_story_ID", "related_topic_tags", "related_countries_codes", "company", "year",
            "related_issues", "related_UNGC_principles", "selftext", "created_utc")) %>%
  setnames(., new = c("emp_cond", "human_rights", "fraud", "products", "social_disc", "emp_disc", "legis", "comp"), old = issue)

cmt_control$subreddit <- as.factor(cmt_control$subreddit)
cmt_control$flag_treatment <- as.factor(cmt_control$flag_treatment)
cmt_control$novelty <- as.factor(cmt_control$novelty)
cmt_control$reach <- as.factor(cmt_control$reach)
cmt_control$severity <- as.factor(cmt_control$severity)

################## H1: Effect on sentiment ########
h1 <- select(cmt_control, -c("id"))

#Check the direct effect of all DV, find perfect multicolinearity in legis
testmod <- lm(sentiment ~ ., data = h1[,c(1,3:15)])
summary(testmod)
car ::vif(testmod)
alias(testmod)

#Drop legis, fix multicolinearity, insignificant effect of treatment
testmod1 <- lm(sentiment ~ ., data = h1[, c(1, 3:13, 15)])
summary(testmod1)

#Include interaction effect between flag_treatment and subreddit --> better model
testmod11 <- lm(sentiment ~ . + flag_treatment:subreddit, data = h1[, c(1, 3:13, 15)])
summary(testmod11)

anova(testmod11, testmod1)

unique(h1$subreddit)

testmod12 <- lm(sentiment ~ flag_treatment + subreddit, data = h1)
summary(testmod12)

testmod13 <- lm(sentiment ~ flag_treatment, data = h1)
summary(testmod13)
anova(testmod11, testmod13)

mod1_dir <- "C:/Users/Admin/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/mod1.html"
stargazer(testmod11, type = "html", out = mod1_dir)
################## H2: Effect on the number of comments ######
#Count the number of comments in each subreddit before and after the crisis
n_cmt <- cmt_control %>%
  group_by(subreddit, reference_scandaldate, flag_treatment) %>%
  count(subreddit, name = "n_cmt")

#Add control variables
h2_control <- n_cmt %>%
  left_join(., tinder, by = c("subreddit", "reference_scandaldate")) %>%
  select(-c("RepRisk_ID", "RepRisk_story_ID", "related_topic_tags", "related_countries_codes", "company", "year",
            "related_issues", "related_UNGC_principles")) %>%
  setnames(., new = c("emp_cond", "human_rights", "fraud", "products", "social_disc", "emp_disc", "legis", "comp"), old = issue)

#Model building
testmod2 <- lm(n_cmt ~ . + flag_treatment:subreddit, data = h2_control[,c(1, 3:15)])
summary(testmod2)

testmod22 <- lm(n_cmt ~ ., data = h2_control[,c(1, 3:15)])
summary(testmod22)
anova(testmod22, testmod2)

testmod23 <- lm(n_cmt ~ . + flag_treatment:subreddit, data = h2_control[, c(1, 3:13, 15)])
summary(testmod23)
anova(testmod23, testmod22)

#Direct effect only: Do not include legis (perfect multicolinearity) and reference_scandal date
testmod24 <- lm(n_cmt ~ ., data = h2_control[, c(1, 3:13, 15)])
#Including interaction effect
mod2 <- testmod23
anova(testmod24, testmod23)
summary(mod1)

mod2_dir <- "C:/Users/523203tn/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/mod2.html"
stargazer(mod2, type = "html", out = mod2_dir, report = "vc*p")
