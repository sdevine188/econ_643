* print and set working directory
pwd
cd "c:\users\stephen\desktop"

* get summary stats on two variables of interest
tabstat deaths, statistics(mean median sd iqr)
summarize deaths
summarize Tpopdeth

tabstat Ruspeed, statistics(mean median sd iqr)
summarize Ruspeed

* get correlation and covariance
correlate deaths Ruspeed
correlate deaths Ruspeed, covariance
