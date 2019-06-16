
library(dplyr)
library(afex)
library(tidyr)
library(ggplot2)

all <- read.table(file = "testlist_all.csv", sep = ",", header = TRUE)
head(all) #查看資料

all.select <- dplyr::select(all,
                            no, Condition, Correct, Response.Time)
head(all.select)

##整理所有人的正確率
all.acc<-all.select %>%
    group_by(no, Condition) %>%
    summarize(acc.sum = sum(Correct)/40)
all.acc #查看資料
mean(all.acc$acc.sum) #所有人的平均正確率

##判斷condition屬於哪個set size and duration time
#condition 1 = set size 4 = duration time 100
#condition 2 = set size 8 = duration time 100
#condition 3 = set size 4 = duration time 500
#condition 4 = set size 8 = duration time 500


##Accuracy in different condition
acc.summary <- all.acc %>%
    mutate(set.size = ifelse(Condition == 1 |Condition == 3, 4, 8),
           duration.time = ifelse(Condition == 1 |Condition == 2, 100, 500))%>%
    group_by(set.size, duration.time) %>%
    summarize(acc.condition = mean(acc.sum))
acc.summary


##拿掉答錯的trails
all.correct <- all.select %>%
    filter(Correct == 1) %>%
    mutate(set.size = ifelse((Condition == 1) |(Condition == 3), "4", "8"),
           duration.time = ifelse((Condition == 1) |(Condition == 2), "100ms", "500ms"))%>%
    select(no, set.size, duration.time, Correct, Condition, Response.Time)

head(all.correct) #查看資料

##拿掉界外值
no.outliers <- all.correct %>%
    filter(Response.Time <= mean(Response.Time) + sd(Response.Time)*3) %>%
    filter(Response.Time >= mean(Response.Time) - sd(Response.Time)*3) 
head(no.outliers) #查看資料


##檢查男女
#id.data <- no.outliers %>% mutate(., id = 1:nrow(.))
#sex.test <- id.data %>% group_by(Sex, Subject) %>% summarize(mean = mean(targetSquare.RT))
#head(sex.test)
#aov.sex <- aov(mean ~ Sex, data = sex.test)
#summary(aov.sex)

##檢查location
#location.test <- id.data %>% group_by(dis_loc, Subject) %>% 
#    summarize(mean = mean(targetSquare.RT))
#head(location.test)
#aov.loc <- aov(mean ~ dis_loc, data = location.test)
#summary(aov.loc)

##整理不同set.size & duration.time的情況 for table1 summary
table1 <- no.outliers %>%
    group_by(set.size, duration.time) %>%
    summarize(mean.RT = mean(Response.Time), sd = sd(Response.Time))
table1


#準備做anova
all.forAnova <- no.outliers%>%
    group_by(.,no, set.size, duration.time)%>%
    summarize(rt = mean(Response.Time), sd=sd(Response.Time), N=n())
head(all.forAnova)

model2 <- aov_car(rt ~ set.size*duration.time + Error(no/(set.size*duration.time)), 
                  data = all.forAnova)
summary(model2)

##多重比較
library(emmeans)
est.means <- emmeans(model2, specs = "set.size")
tukey.results <- contrast(est.means, method = "tukey")
tukey.results


##資料視覺化
library(ggplot2)

drawing <- all.forAnova %>% group_by(.,set.size, duration.time) %>%
    summarize(newrt = mean(rt), newsd = sd(sd/sqrt(16)), N=n())
drawing
drawing$newsd


Means.Within.SE <-ggplot(data = drawing, aes(x = set.size, y = newrt, group = duration.time))+
    geom_point(aes(colour = duration.time))+
    #scale_y_continuous(breaks = seq(100,800,50))+
    #scale_x_continuous(breaks = seq(50,950,300))+
    geom_line(aes(colour = duration.time), size=1.1)+
    geom_ribbon(aes(ymin=newrt-newsd, ymax=newrt+newsd, fill = duration.time), alpha=.25)+
    ylab("Response Time (ms)")+xlab("Set Size")+
    theme(legend.position = "right")
Means.Within.SE

