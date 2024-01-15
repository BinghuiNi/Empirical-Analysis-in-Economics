use "C:\Users\nibh\Desktop\地方财政\论文\ds3.dta", clear
cd "C:\Users\nibh\Desktop\地方财政\论文"
xtset city year

reghdfe transp lnuser,absorb(city)
est store m1
reghdfe transp lnuser lnGDP_pc lnpop lnFDI restate deficit lnstudent,absorb(city)
est store m2
reghdfe transp lnuser,absorb(year)
est store m3
reghdfe transp lnuser lnGDP_pc lnpop lnFDI restate deficit lnstudent,absorb(year)
est store m4
reghdfe transp lnuser,absorb(city year)
est store m5
reghdfe transp lnuser lnGDP_pc lnpop lnFDI restate deficit lnstudent,absorb(city year)
est store m6

outreg2 [m*] using "FE.doc",replace tstat bdec(3) tdec(2) title(Table 2. Regression Result)

*************分东部省份和非东部省份进行异质性分析，以东部为基准
gen neast_lnuser=neast*lnuser
reghdfe transp neast_lnuser lnuser lnGDP_pc lnpop lnFDI restate deficit lnstudent,absorb(city year)
est store m7
outreg2 [m6 m7] using "Heterogeneity Analysis.doc",replace tstat bdec(3) tdec(2) title(Table 3. Heterogeneity Analysis)

**************内生性--宽带中国多期DID
reghdfe transp did lnGDP_pc lnpop lnFDI restate deficit lnstudent,absorb(city year)
est store m7
outreg2 [m6 m7] using "DID.doc",replace tstat bdec(3) tdec(2) title(Table 4. )