*************REGRESSION--DIFFERENCE-IN-DIFFERENCES*****************************************************
use "C:\Users\nibh\Desktop\城市经济\论文\ds1.dta", clear
cd "C:\Users\nibh\Desktop\城市经济\论文"
outreg2 using "Decriptive statistics.doc", replace sum(log) dec(3) keep(lnhp did GDPpc first second finance restate lnpop edu bed) title(Table 1. Descriptive statistics)
xtset citycode year
reg lnhp did
est store m1
reg lnhp did GDPpc lnpop first second restate finance bed edu
est store m2
reghdfe lnhp did,absorb(provincecode year)
est store m3
reghdfe lnhp did GDPpc lnpop first second restate finance bed edu,absorb(provincecode year)
est store m4
reghdfe lnhp did,absorb(provincecode year) vce(cluster provincecode)
est store m5
reghdfe lnhp did GDPpc lnpop first second restate finance bed edu,absorb(provincecode year) vce(cluster provincecode)
est store m6
outreg2 [m*] using "MDID.doc",replace tstat bdec(3) tdec(2) title(Table 2. Regression Result)
****************COMMON TREND/PARALLEL TEST*****************************************************
use "C:\Users\nibh\Desktop\城市经济\论文\ds1.dta", clear
gen time=year-first_treat if treat==1
replace time=0 if time==.
replace time=-7 if time <=-7
replace time=1 if time>=1
tab time ,gen(j)
forvalues i=1/9{
replace j`i'=0 if treat==0
}
drop j1
xtset citycode year
reghdfe lnhp j* GDPpc lnpop first second restate finance bed edu,absorb(provincecode year)
coefplot, baselevels keep(j*) vertical yline(0) ytitle("Effect of High-speed Railway") xtitle("Before-After launched") addplot(line @b @at) ciopts(recast(rcap)) scheme(s1mono) levels(95) coeflabels(j2="-6" j3="-5" j4="-4" j5="-3" j6="-2" j7="-1" j8="0" j9="1") xline(7,lp(shortdash))
***********************TIME PLACEBO TEST*************************************
use "C:\Users\nibh\Desktop\城市经济\论文\ds1.dta", clear
xtset citycode year
//Treat before 4 years
gen ft_4=first_treat-4
replace ft_4=0 if treat==0
gen did_4=1 
replace did_4=0 if treat==0 | year<ft_4
xtreg lnhp did_4 GDPpc first second finance restate lnpop edu bed i.year,fe
est store bef4
//Treat before 3 years
gen ft_3=first_treat-3
replace ft_3=0 if treat==0
gen did_3=1 
replace did_3=0 if treat==0 | year<ft_3
xtreg lnhp did_3 GDPpc first second finance restate lnpop edu bed i.year,fe
est store bef3
outreg2 [bef*] using "Time Placebo .doc",replace tstat bdec(3) tdec(2) title(Table 3. Time Placebo Test)
****************************INDIVIDUAL PLACEBO TEST*******************************
clear
mat b = J(500,1,0)
mat se = J(500,1,0)
mat p = J(500,1,0)
forvalues i = 1/500{
use "C:\Users\nibh\Desktop\城市经济\论文\ds1.dta", clear
//citycode是正规6位编码，cityid是1~297
xtset cityid year
keep if year==2019
//实际213个城市开通高铁，这里随机生成213个假设开通高铁的城市matchid
sample 213,count
keep cityid
save matchid.dta,replace
merge 1:m cityid using "C:\Users\nibh\Desktop\城市经济\论文\ds1.dta"
gen treat1=(_merge==3)
save matchid`i'.dta,replace
use "C:\Users\nibh\Desktop\城市经济\论文\ds1.dta", clear
bsample 1,strata(cityid)
keep year
save matchyear.dta,replace
mkmat year,matrix(sampleyear)
use matchid`i'.dta,replace
xtset cityid year
gen timea=0
foreach j of numlist 1/297{
replace timea=1 if (cityid == `j' & year >= sampleyear[`j',1])
}
gen did1=treat1*year
qui xtreg lnhp did1 GDPpc first second finance restate lnpop edu bed i.year,fe
mat b[`i',1] = _b[did1]
mat se[`i',1] = _se[did1]
scalar df_r = e(N) - e(df_m) -1
mat p[`i',1] = 2*ttail(df_r,abs(_b[did1]/_se[did1]))
}
svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)
drop if pvalue1 == .
label var pvalue1 P-value
label var coef1 Estimated-Coeffienct
twoway (scatter pvalue1 coef1, xlabel(-0.035(0.01)0.035, grid) yline(0.1,lp(shortdash)) xline(0.031,lp(shortdash)) xtitle(估计系数) msymbol(smcircle_hollow) mcolor(red) legend(on))(kdensity coef1,yaxis(2) legend(on) title(安慰剂检验)),ytitle("p值",axis(1)) ytitle("核密度",axis(2))

******************************************************************************************
use "C:\Users\nibh\Desktop\城市经济\论文\ds1.dta", clear
cd "C:\Users\nibh\Desktop\城市经济\论文"
xtset citycode year
gen region = 0
replace region =1 if province =="北京市"| province == "天津市" | province =="河北省" | province =="辽宁省" | province == "上海市" | province == "江苏省" | province == "浙江省" | province == "福建省" | province == "山东省" | province == "广东省" | province == "海南省"

replace region =2 if province == "山西省" | province == "内蒙古自治区" | province == "吉林省" | province == "黑龙江省"  | province == "安徽省" | province == "江西省" | province == "河南省" | province == "湖北省" | province == "湖南省" | province == "广西壮族自治区"

replace region =3 if province == "重庆市" | province == "四川省" | province == "贵州省" | province == "云南省" | province == "西藏自治区" | province == "陕西省" | province == "甘肃省" | province == "青海省" | province == "宁夏回族自治区" | province == "新疆维吾尔自治区"


reghdfe lnhp did GDPpc lnpop first second restate finance bed edu if region==1 | region==2,absorb(provincecode year) vce(cluster provincecode)
est store m7
reghdfe lnhp did GDPpc lnpop first second restate finance bed edu if region==3,absorb(provincecode year) vce(cluster provincecode)
est store m8
outreg2 [m7 m8] using "hete.doc",replace tstat bdec(3) tdec(2) title(Table 2. Regression Result)
