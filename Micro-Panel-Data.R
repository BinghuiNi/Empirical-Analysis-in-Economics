########高等教育对代际收入流动的影响
########个体层面，匹配父代收入
########2010、2012、2014、2016、2018、2020年CFPS面板数据

########Setting#####
options(digits = 3,scipen = 200)#确定3位小数点
pkg <- c("foreign","readstata13","xlsx","stargazer","dplyr","plm","DescTools","tidyr","reshape2")
lapply(pkg,library,character.only=T);rm(pkg)

#fileNames <- Sys.glob("D:/微观数据库/CFPS中国家庭追踪/CFPS2020面板数据/*.dta",dirmark=T)
#for (i in 1:length(fileNames)) {
#  a <- read.dta13(fileNames[i])
#  var.NAs <- colSums(is.na(a))
#  var.obs <- nrow(a)
#  var.labels <- attr(a,"var.labels")
#  data.key <- data.frame(var.labels,var.NAs,round((1-var.NAs/var.obs)*100,3))
#  colnames(data.key) <- c("VarLables","Number of Missing","PercentageofObs%")
#  write.xlsx(data.key,paste(fileNames[i],".xlsx"))
#};rm(list=ls())

#高等教育毛入学率-画图
rate <- c(0.0976,0.125,0.133,0.15,0.17,0.19,0.21,0.22,0.23,0.233,0.242,0.265,
          0.269,0.3,0.345,0.375,0.40,0.427,0.457,0.481,0.516,0.545,0.578,0.596)
plot(rate,type="o",col="blue",ylim=0:1,axes=F,ann=F)
axis(side=1,at=1:24,lab=1999:2022)
axis(side=2,las=1,at=c(0,0.25,0.5,1))
box()
title(main="图1. 高等教育毛入学率",col.main="red",font.main=4)
title(xlab="Years",ylab="Rate",col.lab=rgb(0,0.5,1))

#############################################数据处理（一）父子/女关系匹配########################################################
#########2010年
fr <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2010/cfps2010famconf_家庭关系202008.dta", convert.factors = FALSE)
fr1 <- fr %>%
  select(pid, pid_f) %>%
  filter(pid_f != -8 & pid_f != 77) #pid是个体，往上找父亲pid_f，fr1得到父子/父女关系
fr <- fr %>%
  filter(tb2_a_p == 1) %>%           #筛选性别为男，pid是父亲，往下找孩子pid_c
  select(pid_c1, pid_c2, pid_c3, pid_c4, pid_c5, pid_c6, pid_c7, pid_c8, pid_c9, pid_c10, pid)
#利用宽面板转长面板的方法，把pid和10个孩子id匹配
fr2 <- reshape(fr, direction = "long",
               varying = c('pid_c1', 'pid_c2', 'pid_c3', 'pid_c4', 'pid_c5', 'pid_c6', 'pid_c7', 'pid_c8', 'pid_c9', 'pid_c10'),
               v.names = "pid_c", idvar = c('pid'))
fr2 <- fr2 %>%
  filter(pid_c != 79 & pid_c != -8 & pid_c != 77) %>%     #删掉没有孩子pid_c=79、缺失值-8、离家出走77
  select(pid_f = pid, pid = pid_c)        #原来是pid和pid_C，为了后续匹配，两边都升一级变成pid_f和pid
fr3 <- unique(rbind(fr1,fr2))             #21838组

#########2012年
fr <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2012/cfps2012famconf_092015家庭关系.dta", convert.factors = FALSE)
fr1 <- fr %>%
  select(pid, pid_f) %>%
  filter(pid_f != -8 & pid_f != 77) #pid是个体，往上找父亲pid_f，fr1得到父子/父女关系
fr <- fr %>%
  filter(tb2_a_p == 1) %>%           #筛选性别为男，pid是父亲，往下找孩子pid_c
  select(pid_c1, pid_c2, pid_c3, pid_c4, pid_c5, pid_c6, pid_c7, pid_c8, pid_c9, pid_c10, pid)
#利用宽面板转长面板的方法，把pid和10个孩子id匹配
fr <- distinct(fr,pid,.keep_all = T)
fr2 <- reshape(fr, direction = "long",
               varying = c('pid_c1', 'pid_c2', 'pid_c3', 'pid_c4', 'pid_c5', 'pid_c6', 'pid_c7', 'pid_c8', 'pid_c9', 'pid_c10'),
               v.names = "pid_c", idvar = c('pid'))
fr2 <- fr2 %>%
  filter(pid_c != 79 & pid_c != -8 & pid_c != 77) %>%     #删掉没有孩子pid_c=79、缺失值-8、离家出走77
  select(pid_f = pid, pid = pid_c)        #原来是pid和pid_C，为了后续匹配，两边都升一级变成pid_f和pid
fr4 <- unique(rbind(fr1,fr2))             #21572组
fr3 <- unique(rbind(fr3,fr4))             #和2010年unique，24268组

#########2014年
fr <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2014/cfps2014famconf_170630.dta", convert.factors = FALSE)
fr1 <- fr %>%
  select(pid, pid_f) %>%
  filter(pid_f != -8 & pid_f != 77) #pid是个体，往上找父亲pid_f，fr1得到父子/父女关系
fr <- fr %>%
  filter(tb2_a_p == 1) %>%           #筛选性别为男，pid是父亲，往下找孩子pid_c
  select(pid_c1, pid_c2, pid_c3, pid_c4, pid_c5, pid_c6, pid_c7, pid_c8, pid_c9, pid_c10, pid)
#利用宽面板转长面板的方法，把pid和10个孩子id匹配
fr <- distinct(fr,pid,.keep_all = T)
fr2 <- reshape(fr, direction = "long",
               varying = c('pid_c1', 'pid_c2', 'pid_c3', 'pid_c4', 'pid_c5', 'pid_c6', 'pid_c7', 'pid_c8', 'pid_c9', 'pid_c10'),
               v.names = "pid_c", idvar = c('pid'))
fr2 <- fr2 %>%
  filter(pid_c != 79 & pid_c != -8 & pid_c != 77) %>%     #删掉没有孩子pid_c=79、缺失值-8、离家出走77
  select(pid_f = pid, pid = pid_c)        #原来是pid和pid_C，为了后续匹配，两边都升一级变成pid_f和pid
fr4 <- unique(rbind(fr1,fr2))             #23610组
fr3 <- unique(rbind(fr3,fr4))             #和2010、2012年unique，26844组

#########2016年
fr <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2016/cfps2016famconf_201804.dta",convert.factors = FALSE)
fr1 <- fr %>%
  select(pid, pid_f) %>%
  filter(pid_f != -8 & pid_f != 77) #pid是个体，往上找父亲pid_f，fr1得到父子/父女关系
fr <- fr %>%
  filter(tb2_a_p == 1) %>%           #筛选性别为男，pid是父亲，往下找孩子pid_c
  select(pid_c1, pid_c2, pid_c3, pid_c4, pid_c5, pid_c6, pid_c7, pid_c8, pid_c9, pid_c10, pid)
#利用宽面板转长面板的方法，把pid和10个孩子id匹配
fr <- distinct(fr,pid,.keep_all = T)
fr2 <- reshape(fr, direction = "long",
               varying = c('pid_c1', 'pid_c2', 'pid_c3', 'pid_c4', 'pid_c5', 'pid_c6', 'pid_c7', 'pid_c8', 'pid_c9', 'pid_c10'),
               v.names = "pid_c", idvar = c('pid'))
fr2 <- fr2 %>%
  filter(pid_c != 79 & pid_c != -8 & pid_c != 77) %>%     #删掉没有孩子pid_c=79、缺失值-8、离家出走77
  select(pid_f = pid, pid = pid_c)        #原来是pid和pid_C，为了后续匹配，两边都升一级变成pid_f和pid
fr4 <- unique(rbind(fr1,fr2))             #25929组
fr3 <- unique(rbind(fr3,fr4))             #和2010、2012、2014年unique，29648组

#########2018年
fr <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2018/cfps2018famconf_202008家庭关系.dta", convert.factors = FALSE)
fr1 <- fr %>%
  select(pid, pid_f=pid_a_f) %>%
  filter(pid_f != -8 & pid_f != 77) #pid是个体，往上找父亲pid_f，fr1得到父子/父女关系
fr <- fr %>%
  filter(tb2_a_p == 1) %>%           #筛选性别为男，pid是父亲，往下找孩子pid_c
  select(pid_c1=pid_a_c1, pid_c2=pid_a_c2, pid_c3=pid_a_c3, pid_c4=pid_a_c4, 
         pid_c5=pid_a_c5, pid_c6=pid_a_c6, pid_c7=pid_a_c7, pid_c8=pid_a_c8, 
         pid_c9=pid_a_c9, pid_c10=pid_a_c10, pid)
#利用宽面板转长面板的方法，把pid和10个孩子id匹配
fr <- distinct(fr,pid,.keep_all = T)
fr2 <- reshape(fr, direction = "long",
               varying = c('pid_c1', 'pid_c2', 'pid_c3', 'pid_c4', 'pid_c5', 'pid_c6', 'pid_c7', 'pid_c8', 'pid_c9', 'pid_c10'),
               v.names = "pid_c", idvar = c('pid'))
fr2 <- fr2 %>%
  filter(pid_c != 79 & pid_c != -8 & pid_c != 77) %>%     #删掉没有孩子pid_c=79、缺失值-8、离家出走77
  select(pid_f = pid, pid = pid_c)        #原来是pid和pid_C，为了后续匹配，两边都升一级变成pid_f和pid
fr4 <- unique(rbind(fr1,fr2))             #57252组
fr3 <- unique(rbind(fr3,fr4))             #和2010、2012、2014、2016年unique，共62214组（2020年无家庭关系数据库）

#################################################数据处理（二）个人库#############################################
#########2010年
ps0 <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2010/cfps2010adult_202008.dta",convert.factors = F)
ps01 <- ps0 %>%                             
  select(pid,provcd,urban,age=qa1age,male=gender,marriage=qe1,
         edu=cfps2010edu_best,income,happy=qk802,health=qp3) %>% 
  filter(income!=-8)                                                   
#匹配子女信息
mg01 <- merge(x=fr3,y=ps01,by="pid",all=F)
#匹配父亲收入
mg10 <- merge(x=mg01,y=ps01[,c(1,8)],by.x="pid_f",by.y="pid",all=F)  #3977组父亲、子女均有收入信息的样本
mg10$year <- 2010

#########2012年
ps0 <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2012/cfps2012adult_201906成人库.dta",convert.factors = F)
ps01 <- ps0 %>%                             
  select(pid,provcd,urban=urban12,age=qv201b,male=cfps2012_gender_best,marriage=qe104,
         edu=sw1r,income,happy=qn12012,health=qp201) %>% 
  filter(income!=-8)                                                   
#匹配子女信息
mg01 <- merge(x=fr3,y=ps01,by="pid",all=F)
#匹配父亲收入
mg12 <- merge(x=mg01,y=ps01[,c(1,8)],by.x="pid_f",by.y="pid",all=F)  #7694组父亲、子女均有收入信息的样本
mg12$year <- 2012

#########2014年
ps0 <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2014/cfps2014adult_201906.dta",convert.factors = F)
ps01 <- ps0 %>%                             
  select(pid,provcd=provcd14,urban=urban14,age=cfps2014_age,male=cfps_gender,marriage=qea0,
         edu=cfps2014edu,income,happy=qm2012,health=qp201) %>% 
  filter(income!=-8)                                                   
#匹配子女信息
mg01 <- merge(x=fr3,y=ps01,by="pid",all=F)
#匹配父亲收入
mg14 <- merge(x=mg01,y=ps01[,c(1,8)],by.x="pid_f",by.y="pid",all=F)  #2994组父亲、子女均有收入信息的样本
mg14$year <- 2014

#########2016年
ps0 <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2016/cfps2016adult_201906.dta",convert.factors = F)
ps01 <- ps0 %>%                             
  select(pid,provcd=provcd16,urban=urban16,age=cfps_age,male=cfps_gender,marriage=cfps2014_marriage,
         edu=cfps_latest_edu,income,happy=qm2014,health=qp201) %>% 
  filter(income!=-8)                                                   
#匹配子女信息
mg01 <- merge(x=fr3,y=ps01,by="pid",all=F)
#匹配父亲收入
mg16 <- merge(x=mg01,y=ps01[,c(1,8)],by.x="pid_f",by.y="pid",all=F)  #819组父亲、子女均有收入信息的样本
mg16$year <- 2016

#########2018年
ps0 <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2018/cfps2018person_202012个人库.dta",convert.factors = F)
ps01 <- ps0 %>%                             
  select(pid,provcd=provcd18,urban=urban18,age,male=gender,marriage=qea0,
         edu=cfps2018edu,income,happy=qm2016,health=qp201) %>% 
  filter(income!=-8)                                                   
#匹配子女信息
mg01 <- merge(x=fr3,y=ps01,by="pid",all=F)
#匹配父亲收入
mg18 <- merge(x=mg01,y=ps01[,c(1,8)],by.x="pid_f",by.y="pid",all=F)  #1814组父亲、子女均有收入信息的样本
mg18$year <- 2018

#########2020年
ps0 <- read.dta13("D:/微观数据库/CFPS中国家庭追踪/CFPS2020/cfps2020person_202112.dta",convert.factors = F)
ps01 <- ps0 %>%                             
  select(pid,provcd=provcd20,urban=urban20,age,male=gender,marriage=qea0,
         edu=cfps2020edu,income,happy=qm2016,health=qp201) %>% 
  filter(income!=-8)                                                   
#匹配子女信息
mg01 <- merge(x=fr3,y=ps01,by="pid",all=F)
#匹配父亲收入
mg20 <- merge(x=mg01,y=ps01[,c(1,8)],by.x="pid_f",by.y="pid",all=F)  #1325组父亲、子女均有收入信息的样本
mg20$year <- 2020

#################################################数据处理（三）合成面板数据+变量处理#############################################
ds <- rbind(mg10,mg12,mg14,mg16,mg18,mg20);rm(fr,fr1,fr2,fr3,fr4,mg01,mg10,mg12,mg14,mg16,mg18,mg20,ps0,ps01)
ds$provcd <- ifelse(ds$provcd==-9,trunc(ds$pid/10000000),ds$provcd)
ds <- pdata.frame(ds,index = c("pid","year"))           #2010-2020年6期，共18623组父亲、子女均有收入信息的样本
#重命名
ds <- within(ds, {income_c <- income.x; income_f<- income.y; rm(income.x,income.y)})

#处理控制变量
ds$marriage <- ifelse(is.na(ds$marriage) | ds$marriage!=2,0,1)
ds$happy <- ifelse(ds$happy<0 | is.na(ds$happy),5,ds$happy)
#健康状况2010-2016是1-7的赋分，2018和2020是‘1.非常健康 2.很健康 3.比较健康4. 一般 5.不健康’
ds$health <- ifelse(ds$year %in% c(2010,2012,2014,2016),ifelse(ds$health %in% c(1,2),0,1),ifelse(ds$health==5,0,1))
#学历2010-2016是1. 文盲/半文盲 2. 小学 3. 初中 4. 高中 5. 大专 6. 大学本科 7. 硕士 8. 博士
#2018和2020是0.文盲/半文盲 3.小学 4.初中 5.高中/中专/技校/职高 6.大专 7.大学本科 8.硕士 9.博士 10.从未上过学
for (i in c(2010,2012,2014,2016,2018,2020)) {
  print(table(filter(ds,year==i)$edu))
}#硕士47个观测值，博士1个观测值
#生产细分高等教育high 1.大专 2.本科 3.硕士 4.博士
ds$high <- ifelse(ds$year %in% c(2010,2012,2014,2016),
                  ifelse(ds$edu==5,1,ifelse(ds$edu %in% c(6,7,8),2,0)),
                  ifelse(ds$edu==6,1,ifelse(ds$edu %in% c(7,8,9),2,0)))
ds$high <- ifelse(is.na(ds$high),0,ds$high)
#是否高等教育
ds$edu <- ifelse(ds$year %in% c(2010,2012,2014,2016),ifelse(ds$edu %in% c(5,6,7,8),1,0),ifelse(ds$edu %in% c(6,7,8,9),1,0))

ds$income_f <- ifelse(ds$income_f<0,NA,ds$income_f)
ds$income_c <- ifelse(ds$income_c<0,NA,ds$income_c)
ds$age <- ifelse(ds$age<0,NA,ds$age)
ds$urban <- ifelse(ds$urban<0,NA,ds$urban)
ds1 <- na.omit(ds)          #16292组

#处理子女收入的outlier
boxplot(ds1$income_c,main="Figure1. Unprocessed Individual Income")
bench1 <- quantile(ds1$income_c,0.75)+1.5*IQR(ds1$income_c)
#赋值benchmark1,截断
ds1$income_c[ds1$income_c>bench1] <- bench1
boxplot(ds1$income_c,main="Figure2. Processed Individual Income")

#处理父亲收入的outlier
boxplot(ds1$income_f,main="Figure3. Unprocessed Father's Income")
bench2 <- quantile(ds1$income_f,0.75,na.rm=T)+1.5*IQR(ds1$income_f,na.rm=T)
ds1$income_f[ds1$income_f>bench2] <- bench2
boxplot(ds1$income_f,main="Figure4. Processed Father's Income")

#将需要的变量移至最前
ds1 <- ds1 %>% select(income_c,income_f,edu,age,male,urban,marriage,happy,health,everything())

#地区收入分布
#1.东部：北京、天津、河北、辽宁、上海、江苏、浙江、福建、山东、广东、广西、海南
#2.中部：山西、内蒙古、吉林、黑龙江、安徽、江西、河南、湖北、湖南
#3.西部：重庆、四川、贵州、云南、西藏、陕西、甘肃、宁夏、青海、新疆
ds1$east <- ifelse(ds1$provcd %in% c(11,12,13,21,31,32,33,35,37,44,45,46),1,
                   ifelse(ds1$provcd %in% c(14,15,22,23,34,36,41,42,43),2,3))
x <- ds1[order(ds1$income_c),] # sort by father's income
x$east <- factor(x$east) # it must be a factor
x$color[x$east==1] <- "red"
x$color[x$east==2] <- "lightblue"
x$color[x$east==3] <- "lightgreen"
dotchart(as.numeric(x$income_c),labels="",cex=.7,groups= x$east,
               main="图2. 东中西部样本收入",
               xlab="Income", gcolor="black", color=x$color)

#################################################数据处理（四）回归-随机效应模型############################################
#导出描述性统计
stargazer(ds1, title = "Table 1. Descriptive Statistic",type = "text",digits=3,digit.separate=3,
          keep=c("income_c","income_f","edu","age","male","urban","marriage","happy","health"),
          no.space=TRUE,summary=T,median=T,
          out="C:/Users/nibh/Desktop/应用统计/论文-高等教育对代际收入流动/Descriptive Statistic.doc")

#####回归-随机效应模型####
re0 <- lm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
          +age+I(age^2)+marriage+happy+health+male+urban+year,data=ds1)
re1 <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu,
           data=ds1,effect = "individual", model="random")
re2 <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
           +age+I(age^2)+marriage+happy+health+male+urban,data=ds1,effect = "individual", model="random")
#导出回归结果
stargazer(re0,re1,re2, title="表2. 基准回归结果",type = "text",digits=3,digit.separate=3,
          no.space=TRUE,single.row=F, align=F,
          out="C:/Users/nibh/Desktop/应用统计/论文-高等教育对代际收入流动/Regression Results.doc")

######################################数据处理（五）异质性分析#################################################
#####分性别
male <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
          +age+I(age^2)+marriage+happy+health+urban,data=filter(ds1,male==1),effect = "individual", model="random")
female <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
           +age+I(age^2)+marriage+happy+health+urban,data=filter(ds1,male==0),effect = "individual", model="random")

stargazer(re2,male,female,urban,rural, title="Table 3. Heterogeneity Analysis",type = "text",digits=3,digit.separate=3,
          no.space=TRUE,single.row=F,
          out="C:/Users/nibh/Desktop/应用统计/论文-高等教育对代际收入流动/Heterogeneity Analysis.doc")

#####分城乡
urban <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
            +age+I(age^2)+marriage+happy+health+male,data=filter(ds1,urban==1),effect = "individual", model="random")
rural <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
              +age+I(age^2)+marriage+happy+health+male,data=filter(ds1,urban==0),effect = "individual", model="random")

stargazer(re2,urban,rural, title="Table 3. Heterogeneity Analysis",type = "text",digits=3,digit.separate=3,
          no.space=TRUE,single.row=F,
          out="C:/Users/nibh/Desktop/应用统计/论文-高等教育对代际收入流动/Heterogeneity Analysis.doc")

#####细分高等教育类型
college <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
            +age+I(age^2)+marriage+happy+health+male+urban,data=filter(ds1,high %in% c(0,1)),effect = "individual", model="random")
university <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu
               +age+I(age^2)+marriage+happy+health+male+urban,data=filter(ds1,high %in% c(0,2)),effect = "individual", model="random")
stargazer(re2,college,university, title="Table 3. Heterogeneity Analysis",type = "text",digits=3,digit.separate=3,
          no.space=TRUE,single.row=F,
          out="C:/Users/nibh/Desktop/应用统计/论文-高等教育对代际收入流动/Heterogeneity Analysis.doc")

######################################数据处理（六）内生性-IV#################################################
#合并iv，做随机效应模型+IV
ds2 <- read.xlsx("C:/Users/nibh/Desktop/应用统计/论文-高等教育对代际收入流动/每十万人口高等教育学校在校生数（人）.xlsx",sheetIndex=2)
colnames(ds2) <- c("provcd",2010,2012,2014,2016,2018,2020) 
ds2 <- melt(ds2,id.vars ="provcd",variable.name = "year", 
       value.name = "colg")
ds3 <- merge(x=ds1,y=ds2,by=c("provcd","year"),all.x=T)
iv <- plm(log(income_c+1)~log(income_f+1)+log(income_f+1)*edu+age+I(age^2)+marriage+happy+health+male+urban | 
            log(income_f+1)+log(income_f+1)*colg+age+I(age^2)+marriage+happy+health+male+urban,data=ds3,effect = "individual", model="random")
stargazer(re2,iv, title="Table 3. Heterogeneity Analysis",type = "text",digits=3,digit.separate=3,
          no.space=TRUE,single.row=F,
          out="C:/Users/nibh/Desktop/应用统计/论文-高等教育对代际收入流动/iv稳健性.doc")