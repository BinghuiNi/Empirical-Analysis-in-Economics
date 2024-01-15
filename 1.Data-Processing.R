
#####互联网对财政透明度的影响
#####地级市层面2013-2019

#####1.Setting#####
Ps <- c("readstata13","xlsx","foreign","tidyr","dplyr","plm","stargazer")
lapply(Ps,library,character.only=T);rm(Ps)

#####2.数据整合#####
#财政透明度
transp <- read.xlsx("C:/Users/nibh/Desktop/地方财政/论文/地市级-政府财政透明度（2013-2021年）.xlsx",sheetIndex=1)
transp <- within(transp, {provcd <- 所属省份;city<- 城市;transp <- 财政透明度;year <- 年份;rm(所属省份,城市,财政透明度,年份)})
transp <- filter(transp,year<=2019)

#互联网用户数，2020年数据缺失严重（舍）
internet <- read.xlsx("C:/Users/nibh/Desktop/地方财政/论文/各地级市互联网宽帶接入用户.xlsx",sheetIndex=1,colIndex=c(2,4:10))
colnames(internet) <- c("city",2019:2013)
internet <- gather(internet,key="year",value="user","2019":"2013")
internet$year <- as.numeric(internet$year)

#控制变量，源于年鉴
control <- read.xlsx("C:/Users/nibh/Desktop/地方财政/论文/2013-2020控制变量.xlsx",sheetIndex=2)
control1 <- select(control,城市,年份,地区生产总值.年末人口,年末总人口数.万人.,当年实际使用外资金额.万美元.,房地产占比,
                   赤字占GDP,高等教育学生数总)
colnames(control1) <- c("city","year","GDP_pc","population","fdi","restate_rate","deficit_ratio","college_stu")

#合并
ds <- merge(x=transp,y=internet,by=c("city","year"))
ds <- merge(x=ds,y=control1,by=c("city","year"),all.x=T)

#####3.数据处理#####
ds2 <- na.omit(ds)

ds2$lnuser <- log(ds2$user)
ds2$lnGDP_pc <- log(ds2$GDP_pc)
ds2$lnpop <- log(ds2$population)
ds2$lnFDI <- log(ds2$fdi)
ds2$restate <- I(ds2$restate_rate*100)
ds2$deficit <- I(ds2$deficit_ratio*100)
ds2$lnstudent <- log(ds2$college_stu)
ds3 <- select(ds2,transp,lnuser,lnGDP_pc,lnpop,lnFDI,restate,deficit,lnstudent)
stargazer(ds3, title="Descriptive Statistic",type = "text",digits=3,no.space=TRUE,summary=T,median=T,
          out="C:/Users/nibh/Desktop/地方财政/论文/Descriptive Statistic.doc")

ds2$neast <- ifelse(ds2$provcd %in% c("北京市","天津市","河北省","辽宁省","上海市","江苏省","浙江省","福建省","山东省","广东省","广西省","海南省"),0,1)

#####4.宽带中国DID#####
kdzg <- read.dta13("C:/Users/nibh/Desktop/地方财政/论文/“宽带中国“试点名单匹配（2000-2021年）.dta", convert.factors = FALSE)
kdzg <- kdzg %>%
  select(year=年份,city=地区,did=宽带中国试点) %>%
  filter(year>=2013 & year<=2019)
#是否是实验组
kdzg$treat <- ifelse(kdzg$city %in% unique(filter(kdzg,did==1)$city),1,0)
#开通高铁的年份，没开通为0
kdzg$first.treat <- 0
for (i in 2019:2013) {
  kdzg$first.treat <- ifelse(kdzg$city %in% filter(kdzg,did==1 & year==i)$city,i,kdzg$first.treat)
}
ds3 <- merge(x=ds2,y=kdzg,by=c("city","year"),all.x=T)
ds3 <- select(ds3,city,year,provcd,transp,lnuser,lnGDP_pc,lnpop,lnFDI,restate,deficit,lnstudent,neast,did,treat,first.treat)
ds3$did <- ifelse(is.na(ds3$did),0,ds3$did)
ds3$treat <- ifelse(is.na(ds3$treat),0,ds3$treat)
ds3$first.treat <- ifelse(is.na(ds3$first.treat),0,ds3$first.treat)
ds3 <- pdata.frame(ds3,index = c("city","year"))
write.dta(ds3,file="C:/Users/nibh/Desktop/地方财政/论文/ds3.dta")
