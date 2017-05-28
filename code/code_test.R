
require(tidyverse)

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

## rm(mydata)

ggplot(data=smalldata,aes(x=生效月份,weight=保单数量))+geom_bar()


smalldata<-mydata %>%
          
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
         )  

