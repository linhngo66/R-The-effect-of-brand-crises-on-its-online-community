library(ggplot2)
library(psych)
#cmt_control stats
summary(cmt_control)

#Model 1: 788645 obs with duplicated cmt
#124255 --> 71695 comments
#Model 2: 124 obs 

############## Model 1 ##########
#Stat1
cmt_smt <- comment_time %>%
  filter(selftext != "") %>%
  left_join(., text, by = c("selftext" = "id")) %>%
  select(-c("selftext", "created_utc")) %>%
  filter(selftext.y != "", 
         !is.na(selftext.y), 
         selftext.y != "[deleted]", 
         selftext.y != "[removed]")

stat1 <- cmt_smt %>%
  group_by(subreddit) %>%
  summarise(
    n_cmt = n_distinct(id),
    mean_smt = mean(sentiment),
    sd_smt = sd(sentiment),
    median_smt = median(sentiment),
    min_smt = min(sentiment),
    max_smt = max(sentiment),
    first_qu = quantile(sentiment, 0.25),
    third_qu = quantile(sentiment, 0.75))

stat1[, 3:9] <- round(stat1[, 3:9], digits = 2)
write.csv(stat1, 
          file = "C:/Users/523203tn/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/stat1.csv",
          row.names = F)

round(summary(cmt_smt$sentiment), digits = 2)
round(sd(cmt_smt$sentiment), digit = 2)

hist(cmt_smt$sentiment) #normal distribution of cmt sentiment

ggplot(cmt_smt, aes(x = sentiment)) +
  geom_histogram(binwidth = .2, colour="black", fill="#1f78b4") +
  xlim(-2,2) +
  ggtitle("Distribution of comment sentiment") +
  theme(axis.line = element_line(colour = "black"), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))

ggplot(cmt_smt, aes(x = sentiment)) +
  geom_density() +
  xlim(-2,2)

#Stat2
stat2_gr <- h1 %>%
  group_by(reference_scandaldate, flag_treatment) %>%
  summarise(avg_smt = mean(sentiment))

stat2_gr$reference_scandaldate <- as.factor(stat2_gr$reference_scandaldate)

#Graph for stat2
ggplot(data=stat2_gr, aes(x=reference_scandaldate, y=avg_smt, fill=flag_treatment)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
  xlab("Date of crisis") + ylab("Average sentiment") + 
  ggtitle("Average sentiment before and after each crisis") +
  scale_fill_brewer(palette="Paired", labels = c("Before", "After"), name = "Before/After Crisis") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5))
  

#Table for stat2
stat2 <- dcast(stat2_gr, reference_scandaldate ~ flag_treatment, value = avg_smt)
write.csv(stat2, 
          file = "C:/Users/523203tn/OneDrive - Erasmus University Rotterdam/Documents/RSM/IBA 3/Bachelor Project/stat2.csv",
          row.names = F)
############# Model 2 ##########
summary(h2_control)
#DV: subreddit: number of cmts in each subreddit
#relationship: the number of cmts before and after each crisis
#relationship: the number of cmts in each subreddit before and after crisis
stat3_1 <- h2_control %>%
  group_by(subreddit, flag_treatment) %>%
  summarise(n_cmt = sum(n_cmt))

ggplot(data=stat3_1, aes(x=subreddit, y=n_cmt, fill=flag_treatment)) +
  geom_bar(stat="identity", position=position_dodge(), width = 0.7) +
  xlab("Subreddit") + ylab("Number of comments") +
  ggtitle("Number of comments in each subreddit before and after crisis") +
  scale_y_continuous(labels = scales :: comma) +
  scale_fill_brewer(palette="Paired", labels = c("Before", "After"), name = "Before/After Crisis") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5))
