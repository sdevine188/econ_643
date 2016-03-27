* read in data
clear
sysuse lifeexp.dta

* see if any notes for safewater variable, bc no label
notes safewater

summarize lexp
histogram lexp, width(5) start(50) frequency

summarize gnppc
summarize gnppc, detail
tabstat gnppc, statistics(mean median ) 
describe gnppc
describe safewater

label variable safewater "% of pop with access to water"

* summarize with filter condition
summarize safewater if region == 1
