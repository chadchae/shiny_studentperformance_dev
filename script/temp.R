library(dplyr)
data <-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdPz6D3keSL9E6toKu-AR1dmdL_yy00kX3GI41okczGCNegTY6FFKO7RIoEhnTDAYG_irvzf9lVQIb/pub?output=csv")

data <- data %>% replace(is.na(.), 0)
whatifdata <- data
#is.na(data)

# Credential data create
#credentials <- data %>% select(E.mail.address, ID)
#credentials$admin <- FALSE
#colnames(credentials) <- c("user", "password", "admin")
#write.csv(credentials, 'credentials.csv')

# 성적계산
data$ScorewithoutP <- ((data$Attendence1 + 
                        data$Attendence2 + 
                        data$Attendence3 + 
                        data$Attendence4 + 
                        data$Attendence5 +
                        data$Attendence6 + 
                        data$Attendence7 + 
                        data$Attendence8 + 
                        data$Attendence9 + 
                        data$Attendence10) / 10 * 0.1) + 
    ((data$RefEssayCh1 + 
                     data$ChapSumCh1 + 
                     data$ProposalPractice + 
                     data$CITITraining + 
                     data$ThreeIdea + 
                     data$Method + 
                     data$BasicAnalysis) / 7 * 0.3) +
    (data$FinalPaper * 0.1) + 
    (data$Midterm * 0.25) + 
    (data$Finalexam * 0.25)

data$Grade1 <- ifelse(data$ScorewithoutP >= 93, "A", 
                 ifelse(data$ScorewithoutP >= 90, "A-", 
                        ifelse(data$ScorewithoutP >= 87, "B+", 
                               ifelse(data$ScorewithoutP >= 84, "B", 
                                      ifelse(data$ScorewithoutP >= 80, "B-", 
                                             ifelse(data$ScorewithoutP >= 77, "C+", 
                                                    ifelse(data$ScorewithoutP >= 70, "C", 
                                                           ifelse(data$ScorewithoutP >= 60, "D", "F Grade"))))))))
data$ScorewithPandC <- ceiling(data$ScorewithoutP + (data$Participation*0.1))
data$Grade2 <- ifelse(data$ScorewithPandC >= 93, "A", 
                      ifelse(data$ScorewithPandC >= 90, "A-", 
                             ifelse(data$ScorewithPandC >= 87, "B+", 
                                    ifelse(data$ScorewithPandC >= 84, "B", 
                                           ifelse(data$ScorewithPandC >= 80, "B-", 
                                                  ifelse(data$ScorewithPandC >= 77, "C+", 
                                                         ifelse(data$ScorewithPandC >= 70, "C", 
                                                                ifelse(data$ScorewithPandC >= 60, "D", "F Grade"))))))))


# 성적 시뮬레이션
whatifdata[whatifdata == 0] <- 100

whatifdata$ScorewithoutP <- ((whatifdata$Attendence1 + 
                                  whatifdata$Attendence2 + 
                                  whatifdata$Attendence3 + 
                                  whatifdata$Attendence4 + 
                                  whatifdata$Attendence5 +
                                  whatifdata$Attendence6 + 
                                  whatifdata$Attendence7 + 
                                  whatifdata$Attendence8 + 
                                  whatifdata$Attendence9 + 
                                  whatifdata$Attendence10) / 10 * 0.1) + 
    ((whatifdata$RefEssayCh1 + 
          whatifdata$ChapSumCh1 + 
          whatifdata$ProposalPractice + 
          whatifdata$CITITraining + 
          whatifdata$ThreeIdea + 
          whatifdata$Method + 
          whatifdata$BasicAnalysis) / 7 * 0.3) +
    (whatifdata$FinalPaper * 0.1) + 
    (whatifdata$Midterm * 0.25) + 
    (whatifdata$Finalexam * 0.25)
whatifdata$Grade1 <- ifelse(whatifdata$ScorewithoutP >= 93, "A", 
                      ifelse(whatifdata$ScorewithoutP >= 90, "A-", 
                             ifelse(whatifdata$ScorewithoutP >= 87, "B+", 
                                    ifelse(whatifdata$ScorewithoutP >= 84, "B", 
                                           ifelse(whatifdata$ScorewithoutP >= 80, "B-", 
                                                  ifelse(whatifdata$ScorewithoutP >= 77, "C+", 
                                                         ifelse(whatifdata$ScorewithoutP >= 70, "C", 
                                                                ifelse(whatifdata$ScorewithoutP >= 60, "D", "F Grade"))))))))
whatifdata$ScorewithPandC <- ceiling(whatifdata$ScorewithoutP + (whatifdata$Participation*0.01))
whatifdata$Grade2 <- ifelse(whatifdata$ScorewithPandC >= 93, "A", 
                      ifelse(whatifdata$ScorewithPandC >= 90, "A-", 
                             ifelse(whatifdata$ScorewithPandC >= 87, "B+", 
                                    ifelse(whatifdata$ScorewithPandC >= 84, "B", 
                                           ifelse(whatifdata$ScorewithPandC >= 80, "B-", 
                                                  ifelse(whatifdata$ScorewithPandC >= 77, "C+", 
                                                         ifelse(whatifdata$ScorewithPandC >= 70, "C", 
                                                                ifelse(whatifdata$ScorewithPandC >= 60, "D", "F Grade"))))))))

table(whatifdata$Grade1)
table(whatifdata$Grade2)
