
#####高铁开通对地区房价
#####地级市层面2005-2019

#####1.Setting#####
Ps <- c("xlsx","foreign","tidyr","dplyr","did","stargazer")
lapply(Ps,library,character.only=T);rm(Ps)

#####2.数据整合#####
#高铁开通
hw <- read.xlsx("C:/Users/nibh/Desktop/城市经济/论文/2001-2022全国地级市高铁开通情况.xls",sheetIndex=1)
hw <- hw %>% 
  select(province,city,year,provincecode,citycode,did=G.series.是否开通高铁.) %>% 
  filter(year>=2005 & year<=2019)
#是否是实验组
hw$treat <- ifelse(hw$city %in% unique(filter(hw,did==1)$city),1,0)
#开通高铁的年份，没开通为0
hw$first.treat <- 0
for (i in 2019:2005) {
  hw$first.treat <- ifelse(hw$city %in% filter(hw,did==1 & year==i)$city,i,hw$first.treat)
}

#房价
hp <- read.xlsx("C:/Users/nibh/Desktop/城市经济/论文/全国所有298个地级市商品房平均销售价格(1998-2020).xls",sheetIndex=1)
hp <- select(hp,city=region,t2005:t2019)
hp$city <- paste(hp$city,"市",sep="")
hp <- reshape(hp, direction = "long",
               varying = c('t2005','t2006','t2007','t2008','t2009','t2010','t2011','t2012','t2013','t2014','t2015','t2016',
                           't2017','t2018','t2019'),
               v.names = "houseprice",idvar = 'city',timevar="year",times=2005:2019)
#控制变量
ct <- read.xlsx("C:/Users/nibh/Desktop/城市经济/论文/2005-2019控制变量.xlsx",sheetIndex=1)
ct1 <- ct %>% 
  select(year=年份,city=市,population=年末户籍人口_万人_全市,densty=人口密度_人每平方公里_全市,
         citywoker=城镇单位从业人员期末人数_人_全市,         GDP=地区生产总值_当年价格_亿元_全市,GDP_pc=人均地区生产总值_元_全市,
         growth=地区生产总值增长率_百分比_全市,         first=第一产业占地区生产总值的比重_全市,
         second=第二产业占地区生产总值的比重_全市,third=第三产业占地区生产总值的比重_全市,houseinv=住宅开发投资完成额_万元_全市,
         restate=房地产开发投资完成额_万元_全市,FDI=当年实际使用外资金额_万美元_全市,salary=在岗职工平均工资_元_全市,
         highsch=普通高等学校_所_全市,middlesch=普通中学_所_全市,primarysch=普通小学_所_全市,book=公共图书馆图书藏量_千册_全市,
         hospital=医院数_个_全市,bed=医院床位数_张_全市,sewagetreat=污水处理厂集中处理率_百分比_全市,
         garbagetreat=生活垃圾无害化处理率_百分比_全市,revenue=地方一般公共预算收入_万元_全市,
         expenditure=地方一般公共预算支出_万元_全市,edu=教育支出_万元_全市,bkloan=年末金融机构人民币各项贷款余额_万元_全市,
         bksaving=年末金融机构人民币各项存款余额_万元_全市,saving=住户存款余额_万元_全市)
#合并
ds <- merge(x=hw,y=hp,by=c("city","year"),all=F)
ds <- merge(x=ds,y=ct1,by=c("city","year"),all=F)

#####3.多期DID#####
ds1 <- select(ds,citycode,year,province,provincecode,first.treat,did,treat,growth,third,
              sewagetreat,garbagetreat)
ds1$lnhp <- log(ds$houseprice+1)
ds1$lnpop <- log(ds$population+1)
ds1$citywoker <- I(ds$citywoker/ds$population/10000)
ds1$GDPpc <- log(ds$GDP_pc)
ds1$houseinv <- I(ds$houseinv/ds$GDP/10000)
ds1$restate <- I(ds$restate/ds$GDP/100000000)
ds1$FDI <- log(ds$FDI)
ds1$salary <-log(ds$salary)
ds1$highsch <- I(ds$highsch/ds$population)
ds1$middlesch <- I(ds$middlesch/ds$population)
ds1$primarysch <- I(ds$primarysch/ds$population)
ds1$book <- I(ds$book/ds$population)
ds1$hospital <- I(ds$hospital/ds$population)
ds1$bed <- I(ds$bed/ds$population/100)
ds1$edu <- I(ds$edu/ds$expenditure)
ds1$finance <- I(ds$bkloan/ds$bksaving)
ds1$credit <- I(ds$bkloan/ds$GDP/1000000)
ds1$first <- I(ds$first/100)
ds1$second <- I(ds$second/100)
#生成1-297的城市编码
ds1$cityid <- as.factor(ds1$citycode)
ds1$cityid <- as.numeric(ds1$cityid)
ds2 <- select(ds1,citycode,cityid,year,province,provincecode,first.treat,did,treat,lnhp,lnpop,GDPpc,restate,bed,edu,finance,first,second)
ds2 <- na.omit(ds2)
write.dta(ds2,file="C:/Users/nibh/Desktop/城市经济/论文/ds1.dta")
