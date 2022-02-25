library(ggplot2)
library(gridExtra)
library(car)
library(dplyr)
library(tidyr)
library(lubridate)
install.packages("chron")
library(chron)

social_data= read.csv("cleaned_social_data.csv")
attach(social_data)
###Selecting randomly rows in data for easy manipulation
df=social_data[sample(nrow(social_data), "8000"), ]


#First cast Post.Created.Time to chron time 
df$Post.Created.Time <- chron(times = df$Post.Created.Time)

#Then: define bins and dummies for those bins
#Bin 1: from 22:00:01 to 06:00:00 -> Post.Created.TimeNight
df$Post.Created.TimeNight <- ifelse((df$Post.Created.Time >= '22:00:01' |
                                                df$Post.Created.Time <= '06:00:00'), 1, 0)

#Bin 2: from 06:00:01 to 10:00:00 -> Post.Created.TimeMorning
df$Post.Created.TimeMorning <- ifelse((df$Post.Created.Time >= '06:00:01' &
                                                  df$Post.Created.Time <= '10:00:00'), 1, 0)

#Bin 3: from 10:00:01 to 14:00:00 -> Post.Created.TimeMidday
df$Post.Created.TimeMidday <- ifelse((df$Post.Created.Time >= '10:00:01' &
                                                 df$Post.Created.Time <= '14:00:00'), 1, 0)

#Bin 4: from 14:00:01 to 18:00:00 -> Post.Created.TimeAfternoon
df$Post.Created.TimeAfternoon <- ifelse((df$Post.Created.Time >= '14:00:01' &
                                                    df$Post.Created.Time <= '18:00:00'), 1, 0)

#Bin 5: from 18:00:01 to 22:00:00 -> Post.Created.TimeEvening
df$Post.Created.TimeEvening <- ifelse((df$Post.Created.Time >= '18:00:01' &
                                                 df$Post.Created.Time <= '22:00:00'), 1, 0)

#Create factor variable with day of the week for post created 
df$Post.Created.Day <- weekdays(as.Date(df$Post.Created.Date))
#Create binary column for weekend: 1 if 'Saturday' or 'Sunday', 0 else
df$Post.Created.Weekend <- ifelse((df$Post.Created.Day == 'Saturday' |
                                              df$Post.Created.Day == 'Sunday' ), 1, 0)
attach(df)
########## Engagement vs Posting Moment
reg=lm(Engagement~Post.Created.TimeEvening+Post.Created.TimeAfternoon+
     Post.Created.Weekend+Post.Created.TimeAfternoon*Post.Created.Weekend+ hasSponsor+Type 
     +Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic +WC, data=df)
summary(reg)


reg1=lm(Engagement~Post.Created.TimeEvening+Post.Created.TimeAfternoon+
         Post.Created.Weekend+Post.Created.TimeAfternoon*Post.Created.Weekend+ hasSponsor+Type 
       +Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic +WC+informal+swear+netspeak+assent+nonflu+filler, data=df)
      
summary(reg1)

reg2=lm(Engagement~Post.Created.TimeEvening+Post.Created.TimeAfternoon+
          Post.Created.Weekend+Post.Created.TimeAfternoon*Post.Created.Weekend+ hasSponsor+Type 
        +Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic +WC+informal+swear+netspeak+assent+nonflu+filler+
          work+leisure+money+relig+death, data=df)

summary(reg2)


reg3=lm(Engagement~Post.Created.TimeEvening+Post.Created.TimeAfternoon+
          Post.Created.Weekend+Post.Created.TimeAfternoon*Post.Created.Weekend+ hasSponsor+Type 
        +Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic +WC+informal+swear+netspeak+assent+nonflu+filler+
          work+leisure+money+relig+death+affect+posemo+negemo+anx+anger+sad+social+family+friend+female+
          male+cogproc+insight+cause+discrep+tentat+percept+see+hear+feel+bio+
          body+health+sexual+ingest+drives+affiliation+achieve+power+reward+risk+
          focuspast+focuspresent+focusfuture+relativ+space+time, data=df)

summary(reg3)

reg4=lm(Engagement~Post.Created.TimeEvening+Post.Created.TimeAfternoon+
          Post.Created.Weekend+Post.Created.TimeAfternoon*Post.Created.Weekend+ hasSponsor+Type 
        +Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic +WC+informal+swear+netspeak+assent+nonflu+filler+
          work+leisure+money+relig+death+affect+anger+sad+social+family+friend+female+
          male+cogproc+insight+cause+see+hear+feel+bio+
          body+health+sexual+ingest+drives+affiliation+achieve+power+reward+risk+relativ+space+time, data=df)


summary(reg4)


#####Trying Models out
###Engagement vs Extrinsic factors
reg0=lm(Engagement~factor(Page.Name)+factor(User.Name)+factor(Page.Category)+Page.Created+hasSponsor+Likes.at.Posting)
summary(reg0)          
          
reg01= lm(Engagement~ Post.Created+hasSponsor+Likes.at.Posting)
summary(reg01)

###Engagement vs Psychological Process 
reg1=lm(Engagement~affect+posemo+negemo+anx+anger+sad+social+family+friend+female+
          male+cogproc+insight+cause+discrep+tentat+percept+see+hear+feel+bio+
          body+health+sexual+ingest+drives+affiliation+achieve+power+reward+risk+
          focuspast+focuspresent+focusfuture+relativ+space+time)
summary(reg1)
###Engagement vs Linguistic Dimensions
reg2=lm(Engagement~fnctn+pronoun+ppron+we+you+shehe+they+ipron+article+prep+auxverb+adverb+conj+negate+verb+adj+compare
        +interrog+number+quant)
summary(reg2)
###Engagement vs word count:if long text attract more people than short text
reg3=lm(Engagement~WC,data=df)
summary(reg3)

###Engagement vs  Language Variables
reg4=lm(Engagement~Analytic+Clout+Authentic+Tone+WPS+Sixltr+Dic)
summary(reg4)

###Engagement vs Personelconcerns
reg5=lm(Engagement~work+leisure+money+relig+death, data=df)
summary(reg5)
###Engagement vs Informal language
reg6=lm(Engagement~informal+swear+netspeak+assent+nonflu+filler,data=df)
summary(reg6)



##Data Visualization
##Total Interaction Vs Type
p1<-ggplot(df, aes(x=Type, y=Total.Interactions, fill=Type)) +geom_bar(stat="identity")+theme_minimal()
p1
### Engagement VS Type
p2<-ggplot(df, aes(x=Type, y=Engagement, fill=Type)) +geom_bar(stat="identity")+theme_minimal()
p2
### Total Views VS Type
p3<-ggplot(df, aes(x=Type, y=Total.Views , fill=Type)) +geom_bar(stat="identity")+theme_minimal()
p3

####   
attach(df)
###Best so far
summary(lm(Engagement~Post.Views+home+Total.Views.For.All.Crossposts+leisure+Post.Created.Weekend+Post.Created.TimeAfternoon +
              Authentic+family+money+hasSponsor+WC+Type+Likes.at.Posting+Post.Created.TimeAfternoon*Post.Created.Weekend))



