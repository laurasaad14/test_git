####################################
## EDA:  Exploratory Data Analysis #
####################################

########################
### Experiment setup ###
########################

experimentName <- "SoA1"
whoami <- Sys.info()[["user"]]
if (whoami == "trafton") {
    workingDirectory <- "~/Documents/graphics/SenseOfAgency/"
} else if (whoami == "saad-admin") {
  workingDirectory <- "~/SenseofAgency/"
}

source(paste0(workingDirectory, "R/helper.R"))
graphSaveDirectory <- paste0(workingDirectory, "graphs/", experimentName, "/")
dataDirectory <- paste0(workingDirectory, "data/raw/", experimentName, "/")
setwd(workingDirectory)
VerifyPathIsSafe(graphSaveDirectory)
VerifyPathIsSafe(dataDirectory)

source(paste0(workingDirectory, "R/GetData", experimentName, ".R"))

########################
### very simple hist ###
########################

# converting responses to numeric for visualization
SoA1.df <- mutate(SoA1.df, Response_num = as.numeric(unlist(Response)))
# removing responses to attn check from Response_num variable
SoA1.df <- mutate(SoA1.df, Response_num = if_else(Question=="Please select 3 for this question",NA,Response_num))
# Please select 3 for this question

# overall histogram
p <- ggplot(SoA1.df, aes(x=Response_num))
p <- p + geom_histogram()
print(p)
ggsave(paste0(graphSaveDirectory, "overallHist.pdf"))

# histograms per condition
h_1 <- ggplot(SoA1.df, aes(x=Response_num)) + geom_histogram()
h_1 <- h_1 + facet_wrap(vars(SoA1.df$Condition)) +
  xlab("Likert Response (1 = No Control, 6= Total Control)") + ylab("count") + 
  ggtitle("Response by Condition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1,6, by = 1)) +
  scale_y_continuous(breaks = seq(0,15, by = 3))
print(h_1)

h_2 <- ggplot(SoA1.df, aes(x=Response_num)) + geom_histogram() +
  facet_wrap(vars(SoA1.df$Condition,SoA1.df$Subject)) + 
  scale_x_continuous(breaks = seq(1,6, by = 1)) +
  scale_y_continuous(breaks = seq(0,15, by = 3)) +
  xlab("Likert Response (1 = No Control, 6= Total Control)") + ylab("count") +
  ggtitle("Response by Condition and Participant") +
  theme(plot.title = element_text(hjust = 0.5))
print(h_2)


#################################
## everyone answered questions ##
#################################

cat(fill=TRUE)
cat("Number of questions that each P answered:", fill=TRUE)
print(rowSums(table(SoA1.long$Subject, SoA1.long$Response)))

cat("Slightly funky different numbers for P; fixed next round I think...", fill=TRUE)
totalItems <- 75  ## have to change it for every experiment
cat("Participants that did not answer", totalItems, "questions:", fill=TRUE)
rowSums(table(SoA1.long$Subject, SoA1.long$Response))[which(rowSums(table(SoA1.long$Subject, SoA1.long$Response)) != totalItems)]

##############################
## Average Ranking (graphs) ##
##############################

### no ranking graphs in SoA1

############################
### tidy ggplot of above ###
############################

### probably reasonable way of doing this -- have to do it for each question and/or trial though
SoA1.long %>%
  filter(Question == "How much control did you feel over your actions as you put the item in the cart?") %>%
  filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Video") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap (~Trial) +
  ggtitle("Action Control")
ggsave(paste0(graphSaveDirectory, "ActionControl.pdf"))

SoA1.long %>%
  filter(Question == "How much control did you feel over the item you wanted going into the cart?") %>%
  filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Video") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap (~Trial) +
  ggtitle("Outcome Control")
ggsave(paste0(graphSaveDirectory, "OutcomeControl.pdf"))

SoA1.long %>%
  filter(Question == "To what degree did you feel like you were in control during the above shopping trip?") %>%
  filter(Trial == 1 | Trial == 5) %>%  ## probably want to separate these...
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Condition, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Video") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap (~Trial) +
  ggtitle("Overall Control")
ggsave(paste0(graphSaveDirectory, "OverallControl.pdf"))

################################################
## plots comparing questions within condition ##
################################################

# PAPO expect to see high responses across the board
SoA1.long %>%
  filter(Condition == "PAPO") %>%
  filter(Trial == 1 | Trial == 5) %>%
  filter(Question == "To what degree did you feel like you were in control during the above shopping trip?" | Question == "How much control did you feel over the item you wanted going into the cart?" | Question == "How much control did you feel over your actions as you put the item in the cart?") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Question, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Question") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap (~Trial) +
  ggtitle("Predictable Action Predictable Outcome") + scale_x_discrete(labels=c("How much control did you feel over your actions as you put the item in the cart?" = "Action",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?" = "Overall",
                                                                                  "How much control did you feel over the item you wanted going into the cart?" = "Outcome"), 
                                                                         limits=c("How much control did you feel over your actions as you put the item in the cart?",
                                                                                  "How much control did you feel over the item you wanted going into the cart?",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?"))
ggsave(paste0(graphSaveDirectory, "PAPO_resp.pdf")) 

# PAUO expect to see high action, low outcome, low overall

SoA1.long %>%
  filter(Condition == "PAUO") %>%
  filter(Question == "To what degree did you feel like you were in control during the above shopping trip?" | Question == "How much control did you feel over the item you wanted going into the cart?" | Question == "How much control did you feel over your actions as you put the item in the cart?") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Question, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Question") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  #facet_wrap (~Question) +
  ggtitle("Predictable Action Unpredictable Outcome") + scale_x_discrete(labels=c("How much control did you feel over your actions as you put the item in the cart?" = "Action",
                                              "To what degree did you feel like you were in control during the above shopping trip?" = "Overall",
                                              "How much control did you feel over the item you wanted going into the cart?" = "Outcome"), 
                                              limits=c("How much control did you feel over your actions as you put the item in the cart?",
                                                       "How much control did you feel over the item you wanted going into the cart?",
                                                       "To what degree did you feel like you were in control during the above shopping trip?"))
ggsave(paste0(graphSaveDirectory, "PAUO_resp.pdf")) 


# UAPO expect to see low action, high outcome, mid overall
SoA1.long %>%
  filter(Condition == "UAPO") %>%
  filter(Question == "To what degree did you feel like you were in control during the above shopping trip?" | Question == "How much control did you feel over the item you wanted going into the cart?" | Question == "How much control did you feel over your actions as you put the item in the cart?") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Question, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Question") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  #facet_wrap (~Question) +
  ggtitle("Unpredictable Action Predictable Outcome") + scale_x_discrete(labels=c("How much control did you feel over your actions as you put the item in the cart?" = "Action",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?" = "Overall",
                                                                                  "How much control did you feel over the item you wanted going into the cart?" = "Outcome"), 
                                                                         limits=c("How much control did you feel over your actions as you put the item in the cart?",
                                                                                  "How much control did you feel over the item you wanted going into the cart?",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?"))
ggsave(paste0(graphSaveDirectory, "UAPO_resp.pdf")) 

##########
## same thing by trial
# PAPO expect to see high responses across the board
SoA1.long %>%
  filter(Condition == "PAPO") %>%
  filter(Trial == 1 | Trial == 5) %>%
  filter(Question == "To what degree did you feel like you were in control during the above shopping trip?" | Question == "How much control did you feel over the item you wanted going into the cart?" | Question == "How much control did you feel over your actions as you put the item in the cart?") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Question, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Question") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap (~Trial) +
  ggtitle("Predictable Action Predictable Outcome by Trial") + scale_x_discrete(labels=c("How much control did you feel over your actions as you put the item in the cart?" = "Action",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?" = "Overall",
                                                                                  "How much control did you feel over the item you wanted going into the cart?" = "Outcome"), 
                                                                         limits=c("How much control did you feel over your actions as you put the item in the cart?",
                                                                                  "How much control did you feel over the item you wanted going into the cart?",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?"))
ggsave(paste0(graphSaveDirectory, "PAPO_respbytrial.pdf")) 

# PAUO expect to see high action, low outcome, low overall

SoA1.long %>%
  filter(Condition == "PAUO") %>%
  filter(Trial == 1 | Trial == 5) %>%
  filter(Question == "To what degree did you feel like you were in control during the above shopping trip?" | Question == "How much control did you feel over the item you wanted going into the cart?" | Question == "How much control did you feel over your actions as you put the item in the cart?") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Question, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Question") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap (~Trial) +
  ggtitle("Predictable Action Unpredictable Outcome by Trial") + scale_x_discrete(labels=c("How much control did you feel over your actions as you put the item in the cart?" = "Action",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?" = "Overall",
                                                                                  "How much control did you feel over the item you wanted going into the cart?" = "Outcome"), 
                                                                         limits=c("How much control did you feel over your actions as you put the item in the cart?",
                                                                                  "How much control did you feel over the item you wanted going into the cart?",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?"))
ggsave(paste0(graphSaveDirectory, "PAUO_respbytrial.pdf")) 


# UAPO expect to see low action, high outcome, mid overall
SoA1.long %>%
  filter(Condition == "UAPO") %>%
  filter(Trial == 1 | Trial == 5) %>%
  filter(Question == "To what degree did you feel like you were in control during the above shopping trip?" | Question == "How much control did you feel over the item you wanted going into the cart?" | Question == "How much control did you feel over your actions as you put the item in the cart?") %>%
  mutate(Response = as.numeric(Response)) %>%
  ggplot(aes(x=Question, y=Response)) +
  stat_summary(fun = "mean", geom = "bar", fill="gray") +  ##    shape = 24,
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width=.2) +
  xlab("Question") +
  ylim(0, 7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap (~Trial) +
  ggtitle("Unpredictable Action Predictable Outcome by Trial") + scale_x_discrete(labels=c("How much control did you feel over your actions as you put the item in the cart?" = "Action",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?" = "Overall",
                                                                                  "How much control did you feel over the item you wanted going into the cart?" = "Outcome"), 
                                                                         limits=c("How much control did you feel over your actions as you put the item in the cart?",
                                                                                  "How much control did you feel over the item you wanted going into the cart?",
                                                                                  "To what degree did you feel like you were in control during the above shopping trip?"))
ggsave(paste0(graphSaveDirectory, "UAPO_respbytrial.pdf")) 


#################################################
# checking UAPO for add to cart button presses ##
#################################################

SoA1.df %>%
  filter(Condition == "UAPO") %>%
  filter(Trial == 1 | Trial == 5) %>% 
  ggplot(aes(x=AddToCartButtonPushes)) + geom_histogram() + xlim(-0.5,0.5) +
  ggtitle("Add to Cart Button Pushes in Unpredictable Action Condition") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + xlab("Number of Button Presses")
ggsave(paste0(graphSaveDirectory, "UAPO_buttonpresscount.pdf")) 

############################
## chosen item per person ##
############################
# checking to see if each p picks an item in the same location on every trial

SoA1.df %>%
  group_by(Subject, Trial) %>%
  ggplot(aes(x=ChosenItem)) + geom_histogram() + facet_wrap(~Subject) + 
  xlab("Location of Chosen Grocery Item") + ggtitle("Location of Item Choices Per Participant")




## cor matrix of all vars 
#round(cor(dplyr::select(MPScore.wide, !c(Subject, Video))), digits=2)

### pairwise correlations of all variables can be fun
## require(GGally)
## ggpairs(dplyr::select(SoA1.wide, !c(Subject, Video)))
## ggsave(paste0(graphSaveDirectory, "SoApairs.pdf"))

### add any overall means here if needed
## Means.df <- MPScore.wide %>%
##     group_by(Video) %>%
##     summarise(RSIV = mean(TTCMItemMean),
##               InnerLife = mean(PCInnerLife),
##               Reflect = mean(PCReflect),
##               DirectC = mean(PCDirectC)) %>%
##     arrange(RSIV)

## print(Means.df)

