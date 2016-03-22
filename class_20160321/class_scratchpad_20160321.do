pwd
cd C:\Users\Stephen\Desktop\stata\econ_643\class_20160321
ls

* open data
use "firstsurvey_chapter4.dta"

summarize
describe
* can see that several variables are likert scale responses

* try to run correlation on survey responses, but they're likert type scale
corr prison conserv
* you can see the likert value "labels" and add them to data values
numlabel _all, add

describe prison

sum conserv if gender == 1
sum conserv if gender == 2
corr prison conserv if gender == 1
corr prison conserv if gender == 2
corr prison conserv 

* equivalent of head command
list in 1/5

* create new variable female
generate female = 1 if gender == 2
replace female = 0 if gender == 1

* dummy for college grad
generate college = 1 if education == 16
replace college = 0 if education != 16
 
* cross tab
tab female conserv
