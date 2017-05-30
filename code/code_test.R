library(lubridate)
library(tidyverse)
library(reshape2)
library(ggplot2)

mydata<-read_csv(file="/srv/shiny-server/renew/dataset/renew.txt",
                 col_types =list(
                   COMCODE1 = col_character(),
                   VEH_USE_CN = col_character(),
                   RISKCODE = col_character(),
                   GRP_RENEW_IND = col_character(),
                   核保年月 = col_character(),
                   生效月份 = col_character(),
                   是否贷款车 = col_character(),
                   是否短期单 = col_character(),
                   渠道代码 = col_character(),
                   PREMIUM = col_double(),
                   延迟天数 = col_double(),
                   保单数量 = col_integer()
                 ))

smalldata<-mydata %>%
  
  filter(substr(生效月份,1,4) %in% c('2016','2017') )



ggplot(data=smalldata,aes(x=生效月份,weight=保单数量))+geom_bar()


       mydata %>%
          
         filter(as.numeric(生效月份)<=201705) %>%
  
         mutate(保单年=substr(生效月份,1,4),
                保单月=substr(生效月份,5,7)
                   
                   )  %>%
         group_by(保单年,保单月) %>%
         summarise(
                保单数量=sum(保单数量,na.rm=TRUE),
                延迟天数=-1*sum(延迟天数,na.rm=TRUE),
                保费收入=sum(PREMIUM,na.rm=TRUE),
                平均续保=延迟天数/保单数量
         )  %>%
  
         ggplot(aes(x=保单月,y=平均续保,colour=保单年,group=保单年))+geom_line()


       
       paste0(year(today()), month(today()),'')
       
       as.character(today())
       
       ymd(20170501)<paste(substr(as.character(today()),1,4),substr(as.character(today()),6,7),sep='')
       
       head(mydata)
       
       
 smalldata<-      mydata %>%
         mutate(保单年=substr(生效月份,1,4),
                保单月=substr(生效月份,5,7))  %>%
         
         filter(as.numeric(保单月)<month(today()),as.numeric(保单年)<=year(today())) %>%
         group_by(VEH_USE_CN,保单年) %>%
         summarise(
           保单数量=sum(保单数量,na.rm=TRUE),
            
            提前签单天数=-1*sum(延迟天数,na.rm=TRUE)/sum(保单数量,na.rm=TRUE))
 
 
 smalldata
 
 rename(smalldata,项目=VEH_USE_CN)
 str(smalldata)
      ??melt
    
 a<-melt(data=smalldata)   
 a<-melt(data=smalldata,id=c("VEH_USE_CN","保单年"))
 b<-  as.data.frame.array(acast(a,VEH_USE_CN~variable+保单年))
 c<-data.frame(项目=row.names(b),b)
 
 d<-as_tibble(c)
 
 str(c)
       
 rownames(c)
 
 ??format
 
 str(mydata)
 
 round(c,2)
 
 comma(c)
 
 prettyNum(c,big.mark=",")
 
 ggplot2::comma( )
 
 options(digits=7)
 
 as_tibble(c)
 
 
 xnam <- paste0("x", 1:25)
 (fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
 
formula( paste("veh_use_cn","~policy_year"))
 
 get("a")
 
 rm(list=ls())
 