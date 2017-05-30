
library(tidyverse)

all_data <- read_csv("~/censoc/dred.csv")
all_data$matched_factor <- factor(all_data$matched, labels = c("Unmatched", "Matched"))

#[sample(1:nrow(all_data), 1000),]

p <- ggplot(all_data %>% filter(matched==1, age<110)) + 
  geom_density(position="stack", alpha=0.6, adjust =2.5, aes(x = age, fill = "Matched")) +
  geom_density(data = all_data %>% filter(matched==0, age<110), 
               position="stack", alpha=0.6, adjust = 3, aes(x = age, fill = "Unmatched"))+
  scale_colour_manual(name="",values=c("blue", "red")) + 
  scale_fill_manual(name="",values=c("blue", "red"))+
  theme_bw() + ggtitle("Age distribution of matched and unmatched samples")
ggsave(p, "./fig/age_distribution.pdf")

#m <- print(p)
#plot(m$data[[1]]$x, m$data[[1]]$y)
#lines(m$data[[2]]$x, m$data[[2]]$y)

p <- ggplot(all_data %>% filter(matched==1, income>0, income<30000)) + 
  geom_density(position="stack", alpha=1, adjust =2, aes(x = log(income), fill = "Matched")) +
  geom_density(data = all_data %>% filter(matched==0, income>0, income<30000), 
               position="stack", alpha=0.6, adjust = 3.5, aes(x = log(income), fill = "Unmatched"))+
  scale_colour_manual(name="",values=c("#5e3c99", "#f1a340")) + 
  scale_fill_manual(name="",values=c("#5e3c99", "#f1a340"))+
  theme_bw() #+ ggtitle("Log-income distribution of matched \nand unmatched samples")

ggsave("./fig/lincome_distribution.pdf")

linc <- print(p)
save(linc, file = "./fig/linc.RData")

ggplot(all_data %>% filter(income>0, income<30000, age <70, age>14), 
       aes(y = log(income), x = census_age_group, fill=matched_factor))+
   geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(name="",values=c("red", "blue"))+
  xlab("Age")+ggtitle("Log-income by age")+ theme_bw() + ylim(c(2, 10))
ggsave("./fig/lincome_box.pdf")  

p <- ggplot(all_data %>% filter(matched==1, income>0, income<30000, age <70, age>14)) + 
  geom_density(position="stack", alpha=1, adjust =2, aes(x = educ, fill = "Matched")) +
  geom_density(data = all_data %>% filter(matched==0, income>0, income<30000, age <70, age>14), 
               position="stack", alpha=0.6, adjust = 3.5, aes(x = educ, fill = "Unmatched"))+
  scale_colour_manual(name="",values=c("#5e3c99", "#f1a340")) + 
  scale_fill_manual(name="",values=c("#5e3c99", "#f1a340"))+
  theme_bw() #+ ggtitle("Log-income distribution of matched \nand unmatched samples")
ggsave("./fig/educ.pdf")

educ <- print(p)
save(educ, file = "./fig/educ.RData")

p <- ggplot(all_data %>% filter(matched==1, income>0, income<30000, age <50, age>=40)) + 
  geom_density(position="stack", alpha=1, adjust =2, aes(x = educ, fill = "Matched")) +
  geom_density(data = all_data %>% filter(matched==0, income>0, income<30000, age <50, age>=40), 
               position="stack", alpha=0.6, adjust = 3.5, aes(x = educ, fill = "Unmatched"))+
  scale_colour_manual(name="",values=c("#5e3c99", "#f1a340")) + 
  scale_fill_manual(name="",values=c("#5e3c99", "#f1a340"))+
  theme_bw() #+ ggtitle("Log-income distribution of matched \nand unmatched samples")
ggsave("./fig/educ_40_50.pdf")

educ_40_50 <- print(p)
save(educ_40_50, file = "./fig/educ_40_50.RData")

ggplot(all_data %>% filter(income>0, income<30000, age <70, age>14), 
       aes(y = educ, x = census_age_group, fill=matched_factor))+
  geom_boxplot(outlier.shape=NA)+
  scale_fill_manual(name="",values=c("red", "blue"))+
  xlab("Age")+ggtitle("Education by age")+ theme_bw() 
ggsave("./fig/educ_box.pdf") 

##

props <- all_data %>% 
  group_by(matched_factor, census_age_group) %>% 
  summarise(prop_zero_income = sum(income==0, na.rm=T)/n(), 
            prop_no_income = sum(is.na(income))/n(), 
            prop_rural = sum(rural==TRUE, na.rm=T)/n(), 
            prop_owned = sum(own_rent=="Owned", na.rm=T)/n())

ggplot(props %>% filter(!(census_age_group %in% c("0-4", "5-9", "10-14", "70-74", "75+"))), 
       aes(x=census_age_group, y = prop_zero_income, fill = matched_factor)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(name="",values=c("red", "blue"))+ 
  xlab("Age")+ggtitle("Proportion with zero income")+ ylab("Proportion")+
  theme_bw() 
ggsave("./fig/zero_income_bar.pdf") 

ggplot(props %>% filter(!(census_age_group %in% c("0-4", "5-9", "10-14", "70-74", "75+"))), 
       aes(x=census_age_group, y = prop_no_income, fill = matched_factor)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(name="",values=c("red", "blue"))+ 
  xlab("Age")+ggtitle("Proportion income missing")+ ylab("Proportion")+
  theme_bw() 
ggsave("./fig/income_missing_bar.pdf") 

ggplot(props %>% filter(!(census_age_group %in% c("0-4", "5-9", "10-14", "70-74", "75+"))), 
       aes(x=census_age_group, y = prop_rural, fill = matched_factor)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(name="",values=c("red", "blue"))+ 
  xlab("Age")+ggtitle("Proportion living in rural area")+ ylab("Proportion")+
  theme_bw() 
ggsave("./fig/rural_bar.pdf") 


ggplot(props %>% filter(!(census_age_group %in% c("0-4", "5-9", "10-14", "70-74", "75+"))), 
       aes(x=census_age_group, y = prop_owned, fill = matched_factor)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(name="",values=c("red", "blue"))+ 
  xlab("Age")+ggtitle("Proportion who own home")+ ylab("Proportion")+
  theme_bw() 
ggsave("./fig/own_home_bar.pdf") 


###

props_race <- all_data %>% filter(!is.na(race)) %>%
  group_by(matched_factor, census_age_group, race) %>% 
  summarise(n = n()) %>%
  group_by(matched_factor, census_age_group) %>%
  mutate(prop = n / sum(n))

ggplot(props_race %>% filter(race=="White", !(census_age_group %in% c("0-4", "5-9", "10-14", "70-74", "75+"))),aes(x=census_age_group, y = prop, fill = matched_factor))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(name="",values=c("red", "blue"))+ 
  xlab("Age")+ggtitle("Proportion who are white")+ ylab("Proportion")+
  theme_bw() 
ggsave("./fig/white_bar.pdf") 
