#匯入股市資料
library(odbc)
DATABASE <- "stock_tick_1min_price_0"
USERID <- "nchufin412-tickreader"
PASSWRD <- "nchumei412"
COMPANY <- "2330" 
conn <- dbConnect(odbc::odbc()
                  ,driver = "SQL Server"
                  ,server = "140.120.55.58"
                  ,database = DATABASE
                  ,UID = USERID
                  ,PWD = PASSWRD
                  ,Port = 1433)

df_tables = odbcListObjects(conn, catalog=DATABASE, schema="dbo")

df_tick = dbGetQuery(conn, paste0("
SELECT *
FROM [",DATABASE,"].[dbo].[",COMPANY,"]
ORDER BY [date]
"))
View(df_tick)  #另開一個資料檔案 方便查資料
array(df_tick) #矩陣轉為陣列 避免運算上出現非數值的問題

head(df_tick)
nrow(df_tick)
ncol(df_tick)
data <- df_tick

#名詞定義 
#Denote {Pi,t,t = 1,...,T,i = 1,...,N} as the minute stock prices of the ith day in which t = 1, t = T − 1, and t = T are observing times at 09:00:30, 13:24:30, and 13:30, respectively.

###1.unit root check
install.packages("urca")
library(urca)
## (a)~(c)三題均選定i=20240614 (i是股票日期)
#選取資料範圍 進行ADF單根檢定 虛無假設為y具有單根 對立假設為y不具有單根
##(a) minute stock prices 每分鐘股價
minute_stock= as.numeric(data[1025,-1])  #先定義每分鐘股價
#進行不含有截距項和時間趨勢的單根檢定 
#結果出現p-value=0.01687 拒絕虛無假設 代表每分鐘股價不具有單根
minute_stock1.df=ur.df(y=minute_stock,type="none", selectlags="BIC")
summary(minute_stock1.df)
#對資料進行含有截距項的單根檢定 結果為 p-value: 0.0277 拒絕虛無假設 每分鐘股價不具有單根
minute_stock2.df <- ur.df(y=minute_stock, type="drift",selectlags="BIC")
summary(minute_stock2.df)
#對資料進行含有時間趨勢的單根檢定 結果為 p-value: 0.00631 拒絕虛無假設 每分鐘股價不具有單根
minute_stock3.df <- ur.df(y=minute_stock, type="trend", selectlags="AIC")
summary(minute_stock3.df)

##(b) minute compound returns ri,t = 100∗(ln(Pi,t)−ln(Pi,t−1))
min_cr=as.numeric(100*(diff(log(minute_stock))))  #帶入 每分鐘複利報酬公式 並把資料從矩陣轉為數值矩陣
#不含有時間趨勢與截距項的單根檢定 p-value: < 2.2e-16 拒絕虛無假設 每分鐘複合報酬不具有單根
min_cr1.df=ur.df(y=min_cr,type="none",selectlags="BIC")    
summary(min_cr1.df) 
#含有截距項的單根檢定 結果為p-value: <2.2e-16 拒絕虛無假設 每分鐘複合(利)報酬不具有單根
min_cr2.df <- ur.df(y=min_cr, type="drift",selectlags="BIC")  
summary(min_cr2.df)
#含有時間趨勢的單根檢定 結果為p-value: < 2.2e-16 拒絕虛無假設 每分鐘複利報酬不具有單根
min_cr3.df <- ur.df(y=min_cr, type="trend", selectlags="AIC")  
summary(min_cr3.df)

##(c) minute cumulative return 
min_cumreturn=cumsum(min_cr)  #定義每分鐘累績報酬
#不含有時間趨勢與截距項的單根檢定 p-value: 3.948e-05 顯著拒絕虛無假設 每分鐘累績報酬不具有單根
min_cumreturn1.df=ur.df(y=min_cumreturn,type="none", selectlags="BIC")  
summary(min_cumreturn1.df)
#含有截距項的單根檢定 p-value: 5.171e-05 顯著拒絕虛無假設 每分鐘累績報酬不具有單根
min_cumreturn2.df <- ur.df(y=min_cumreturn, type="drift",selectlags="BIC")
summary(min_cumreturn2.df)
#含有時間趨勢的單根檢定 p-value: 1.127e-06 顯著拒絕虛無假設 每分鐘累績報酬不具有單根
min_cumreturn3.df <- ur.df(y=min_cumreturn, type="trend", selectlags="AIC")
summary(min_cumreturn3.df)

##(d) daily price 
closeP=as.numeric(data[,267])  #定義每天的收盤價 並移除所有資料中的NA值
closeP=closeP[!is.na(closeP)]
View(closeP)
#不含有時間趨勢與截距項的單根檢定 p-value: 0.02901 顯著拒絕虛無假設 每天收盤價格不具有單根
daily_price1.df=ur.df(y=closeP,type="none", selectlags="BIC")
summary(daily_price1.df)
#含有截距項的單根檢定 p-value: 0.2291 其值不顯著 無法拒絕虛無假設 考慮截距項的情況下 具有單根
daily_price2.df <- ur.df(y=closeP, type="drift",selectlags="BIC")
summary(daily_price2.df)
#含有時間趨勢的單根檢定 p-value: 0.2719 其值不顯著 無法拒絕虛無假設 考慮時間趨勢的情況下 具有單根
daily_price3.df <- ur.df(y=closeP, type="trend", selectlags="AIC")
summary(daily_price3.df)

##(e) daily compound return  
daily_cr=as.numeric(100*(diff(log(closeP))))  #定義每天複利報酬 並把數據從矩陣轉為數值矩陣
#不含有時間趨勢與截距項的單根檢定  p-value: < 2.2e-16  拒絕虛無假設 每天複利報酬不具有單根
daily_cr1.df=ur.df(y=daily_cr,type="none",selectlags="BIC")    
summary(day_cr1.df) 
#含有截距項的單根檢定 結果為p-value: < 2.2e-16 拒絕虛無假設 每天複利報酬不具有單根
daily_cr2.df <- ur.df(y=daily_cr, type="drift",selectlags="BIC")  
summary(day_cr2.df)
#含有時間趨勢的單根檢定 結果為p-value: < 2.2e-16 拒絕虛無假設 每天複利報酬不具有單根
daily_cr3.df <- ur.df(y=daily_cr, type="trend", selectlags="AIC")  
summary(daily_cr3.df)


###2 Mean change check
install.packages("strucchange")
library(strucchange)
#資料(或股價的隨機過程)含有mean change的迴歸式 dt為dummy variable  deltat為平均值變化的程度
yt=mu+dt*deltat+ytc

#(a)~(c)和第一題一樣 使用同一天的資料(i=20240614)
##(a) minute stock prices
minute_stock= as.numeric(data[1025,-1]) #定義每分鐘股價(同第一題)
bp=breakpoints(minute_stock~1)  #使用此函數去尋找平均值變動的點 再畫出折線圖 把資料視覺化
summary(bp)
plot(bp)
#根據上面summary的結果 可知道當m=1~5時 個別在部分觀察值中出現折點 下方為對應折點的值
#且根據折線圖來看 明顯存在五個拗折點 故當i=2024-06-14時 每分鐘股價存在mean change

##(b) minute compound returns  ri,t = 100∗(ln(Pi,t)−ln(Pi,t−1))
min_cr=as.numeric(100*(diff(log(minute_stock))))  #定義複利報酬 使用compound return公式
bp1=breakpoints(min_cr~1)  #使用此函數去尋找平均值變動的點 再畫出折線圖 
summary(bp1)
plot(bp1)
#當m=1~5時 個別在部分觀察值中出現折點 下方為對應折點的值 故每分鐘複利報酬存在mean change

##(c) minute cumulative return 
min_cumreturn=cumsum(min_cr)  #定義每分鐘累績報酬
bp2=breakpoints(min_cumreturn~1)  #使用此函數去尋找平均值變動的點 畫出折線圖 
summary(bp2)
plot(bp2)
#當m=1~5時 個別在部分觀察值中出現折點 下方為對應折點的值 故每分鐘複利報酬存在mean change

##(d) daily price
closeP=as.numeric(data[,267])  #定義每天的收盤價 並移除所有資料中的NA值
closeP=closeP[!is.na(closeP)]
bp3=breakpoints(closeP~1)  #尋找平均值變動的點 畫出折線圖
summary(bp3)
plot(bp3)

##(e) daily compound return 
daily_cr=as.numeric(100*(diff(log(closeP)))) #定義每天複利報酬
bp4=breakpoints(daily_cr~1)  #尋找平均值變動的點 畫出折線圖 把資料視覺化
summary(bp4)
plot(bp4)


###3  Stationarity check
#單根檢定的目的即為確定資料是否為穩定態 而單根檢定有很多種方式 第一題使用ADF檢定 
#本題想要確認資料是否為定態 直觀來看應使用KPSS檢定 但ADF和KPSS兩者檢定的虛無假設為相反 為一體兩面的檢定方法
#ADF的虛無假設:資料含有單根 即資料為非穩定態(nonstationary)
#KPSS的虛無假設:資料為穩定態(stationary)
#由兩者虛無假設來看 只需進行其中一種檢定即可 故延續第一題的結果
##(a) minute stock prices
minute_stock= as.numeric(data[1025,-1])  #先定義每分鐘股價
#進行不含有截距項和時間趨勢的單根檢定 
#結果出現p-value=0.01687 拒絕虛無假設 代表每分鐘股價不具有單根 資料呈現穩定態
minute_stock1.df=ur.df(y=minute_stock,type="none", selectlags="BIC")
summary(minute_stock1.df)
#對資料進行含有截距項的單根檢定 結果為 p-value: 0.0277 拒絕虛無假設 每分鐘股價不具有單根 資料呈現穩定態
minute_stock2.df <- ur.df(y=minute_stock, type="drift",selectlags="BIC")
summary(minute_stock2.df)
#對資料進行含有時間趨勢的單根檢定 結果為 p-value: 0.00631 拒絕虛無假設 每分鐘股價不具有單根 資料呈現穩定態
minute_stock3.df <- ur.df(y=minute_stock, type="trend", selectlags="AIC")
summary(minute_stock3.df)

##(b) minute compound returns  ri,t = 100∗(ln(Pi,t)−ln(Pi,t−1))
min_cr=as.numeric(100*(diff(log(minute_stock))))  #帶入compound return公式 並把資料從矩陣轉為數值矩陣
#不含有時間趨勢與截距項的單根檢定 p-value: < 2.2e-16 拒絕虛無假設 每分鐘複合報酬不具有單根 資料呈現穩定態
min_cr1.df=ur.df(y=min_cr,type="none",selectlags="BIC")    
summary(min_cr1.df) 
#含有截距項的單根檢定 結果為p-value: <2.2e-16 拒絕虛無假設 每分鐘複合(利)報酬不具有單根 資料呈現穩定態
min_cr2.df <- ur.df(y=min_cr, type="drift",selectlags="BIC")  
summary(min_cr2.df)
#含有時間趨勢的單根檢定 結果為p-value: < 2.2e-16 拒絕虛無假設 每分鐘複利報酬不具有單根 資料呈現穩定態
min_cr3.df <- ur.df(y=min_cr, type="trend", selectlags="AIC")  
summary(min_cr3.df)

##(c) minute cumulative return 
min_cumreturn=cumsum(min_cr)  #定義每分鐘累績報酬
#不含有時間趨勢與截距項的單根檢定 p-value: 3.948e-05 顯著拒絕虛無假設 每分鐘累績報酬不具有單根 資料呈現穩定態
min_cumreturn1.df=ur.df(y=min_cumreturn,type="none", selectlags="BIC")  
summary(min_cumreturn1.df)
#含有截距項的單根檢定 p-value: 5.171e-05 顯著拒絕虛無假設 每分鐘累績報酬不具有單根 資料呈現穩定態
min_cumreturn2.df <- ur.df(y=min_cumreturn, type="drift",selectlags="BIC")
summary(min_cumreturn2.df)
#含有時間趨勢的單根檢定 p-value: 1.127e-06 顯著拒絕虛無假設 每分鐘累績報酬不具有單根 資料呈現穩定態
min_cumreturn3.df <- ur.df(y=min_cumreturn, type="trend", selectlags="AIC")
summary(min_cumreturn3.df)

##(d) daily price 
closeP=as.numeric(data[,267])  #定義每天的收盤價 並移除所有資料中的NA值
closeP=closeP[!is.na(closeP)]
View(closeP)
#不含有時間趨勢與截距項的單根檢定 p-value: 0.02901 顯著拒絕虛無假設 每天收盤價格不具有單根 資料呈現穩定態
daily_price1.df=ur.df(y=closeP,type="none", selectlags="BIC")
summary(daily_price1.df)
#含有截距項的單根檢定 p-value: 0.2291 其值不顯著 無法拒絕虛無假設 考慮截距項的情況下 具有單根 資料呈現非穩定態
daily_price2.df <- ur.df(y=closeP, type="drift",selectlags="BIC")
summary(daily_price2.df)
#含有時間趨勢的單根檢定 p-value: 0.2719 其值不顯著 無法拒絕虛無假設 考慮時間趨勢的情況下 具有單根 資料呈現非穩定態
daily_price3.df <- ur.df(y=closeP, type="trend", selectlags="AIC")
summary(daily_price3.df)

##(e) daily compound return  
daily_cr=as.numeric(100*(diff(log(closeP))))  #定義每天複利報酬 並把數據從矩陣轉為數值矩陣
#不含有時間趨勢與截距項的單根檢定  p-value: < 2.2e-16  拒絕虛無假設 每天複利報酬不具有單根 資料呈現穩定態
daily_cr1.df=ur.df(y=daily_cr,type="none",selectlags="BIC")    
summary(day_cr1.df) 
#含有截距項的單根檢定 結果為p-value: < 2.2e-16 拒絕虛無假設 每天複利報酬不具有單根 資料呈現穩定態
daily_cr2.df <- ur.df(y=daily_cr, type="drift",selectlags="BIC")  
summary(day_cr2.df)
#含有時間趨勢的單根檢定 結果為p-value: < 2.2e-16 拒絕虛無假設 每天複利報酬不具有單根 資料呈現穩定態
daily_cr3.df <- ur.df(y=daily_cr, type="trend", selectlags="AIC")  
summary(daily_cr3.df)


###4 Statistical inferences
##(a) test H0:E(Pi,t)=P0 假設P0為每分鐘平均股價(如果P0為常數 則下面將因不知確切值無法運算)
#test statastic
t=length(2:265)   #交易筆數從第二筆到最後一筆
minute_stock= as.numeric(data[1025,-1]) 
#已知資料為時間序列型態 必定存在自我相關 故在回答題目的問題前 只需檢驗是否存在變異數異質性的問題
#可以同時檢驗自我相關和變異數異質性的檢定>>使用Newey-west test
#以下為統計檢定
Pbar=sum(y)/(t-2)
SE=((t-2)*(sum((minute_stock-Pbar)^2,i=2:265))/(t-3))^0.5
P0=sum(minute_stock)/(ncol(data))
test_statistic=(Pbar-P0)/SE
accept=-1.96~~1.96  #檢定值0.0576464落在接受域 不拒絕虛無假設(取alpha=0.05)
#代表每分鐘股價的期望值等於平均股價

##(b) test H0:E(ri,t)=0  (minute compound returns)
min_cr=as.numeric(100*(diff(log(minute_stock))))  #先定義每分鐘複利報酬
#題目虛無假設為每分鐘複利報酬的平均值是否為零 這邊使用Lobato test 
#(雖然和Ljung-Box test一樣都是檢驗資料的自我相關 但Lobato可以額外檢驗時間序列模型是否為線性)
#Lobato的檢定統計量
brownbridge_square=(B(r)-B(1))^2
S=2*pi*fy(0)*integrate(brownbridge_square,0,1)
T=T*(ytbar-mu)^2/S
install.packages("tseries")
library(tseries)
lobato_test <- tseries::jarque.bera.test(min_cr)
print(lobato_test)
#結果為p-value < 2.2e-16 拒絕虛無假設 統計結果顯示每分鐘複利報酬的平均期望值不為零 
#以財務理論來看 如果一檔股票的平均報酬率等於零 相當於無風險報酬 代表投資該公司的風險只有市場風險
#本題結果為拒絕虛無假設 代表這間公司的股票具有個別風險

##(c) test H0:E(CRi,t)=0 (minute cumulative return) 
min_cumreturn=cumsum(min_cr)  #先定義每分鐘累積複利報酬
lobato_test1<- tseries::jarque.bera.test(min_cumreturn) #使用Lobato test檢驗
print(lobato_test1)
#p-value = 8.932e-09 統計結果顯示每分鐘累積複利報酬的平均期望值不為零
#根據財務理論 本題結果為拒絕虛無假設 代表這間公司的股票具有個別風險

##(d) test  H0:E(Pi)=P0 假設P0為每天平均股價
#test statastic
t1=nrow(data)   #交易筆數從第二筆到最後一筆
closeP=as.numeric(data[,267])  #定義每天的收盤價 並移除所有資料中的NA值
closeP=closeP[!is.na(closeP)]
Pbar2=sum(closeP)/(t-2)
SE2=((t-2)*(sum((closeP)^2,i=1:1026))/(t-2))^0.5
P02=sum(closeP)/(nrow(data))
test_statistic2=(Pbar2-P02)/SE2
accept=-1.96~~1.96  #檢定值0.08891669落在接受域 不拒絕虛無假設(取alpha=0.05)
#代表每天收盤股價的期望值等於平均股價

##(e) test  H0:E(ri)=0 (daily compound return)
daily_cr=as.numeric(100*(diff(log(closeP))))  #定義每天複利報酬
lobato_test2<- tseries::jarque.bera.test(daily_cr)  #使用Lobato test
print(lobato_test2)
#p-value < 2.2e-16 統計結果顯示每天複利報酬的平均期望值不為零
#根據財務理論 本題結果為拒絕虛無假設 代表這間公司的股票具有個別風險

###5  Build up time series models (build up an ARMA (p,q))
##(a) stochastic process with {Pi,t,t = 2,...,T − 1}
minute_stock=as.numeric(data[1025,-1])   #先定義每分鐘股價
#用Ljung-Box來檢測殘差值
Box.test(minute_stock,lag=20,type = "Ljung-Box")
#結果為p-value < 2.2e-16 拒絕虛無假設 代表數據有自我相關性 使用時間序列模型估計數據
#選擇時間序列模型 因為ARIMA是ARMA的特例 以下使用SIC/AIC選擇模型
library(forecast)
auto_minstoc<-auto.arima(minute_stock)
summary(auto_minstoc)
#結果顯示每天收盤價格適合使用ARIMA(2,1,0)模型


##(b) stochastic process with {ri,t,t = 2,...,T − 1}
min_cr=as.numeric(100*(diff(log(minute_stock))))  #定義minute compound return公式
#用Ljung-Box來檢測殘差值
Box.test(min_cr,lag=20,type = "Ljung-Box")
#結果為p-value = 0.2144 不拒絕虛無假設 代表數據沒有自我相關性 無法使用時間序列模型估計數據
#故結果為每分鐘複利報酬不適合使用ARMA模型

##(c) stochastic process with {CRi,t,t = 2,...,T − 1}
min_cumreturn=cumsum(min_cr)  #先定義每分鐘累積複利報酬
#用Ljung-Box來檢測殘差值
Box.test(min_cumreturn,lag=20,type = "Ljung-Box")
#結果為p-value < 2.2e-16 拒絕虛無假設 代表數據有自我相關性 使用時間序列模型估計數據
#選擇時間序列模型
library(forecast)
auto_cumreturn<-auto.arima(min_cumreturn)
summary(auto_cumreturn)
#結果顯示每天收盤價格適合使用ARIMA(2,1,0)模型


##(d)  stochastic process with {Pi,i = 1,...,N}
closeP=as.numeric(data[,267])  #定義每天的收盤價 並移除所有資料中的NA值
closeP=closeP[!is.na(closeP)]
#用Ljung-Box來檢測殘差值
Box.test(closeP,lag=20,type = "Ljung-Box")
#結果為p-value < 2.2e-16 拒絕虛無假設 代表數據有自我相關性 可以使用時間序列模型 
#選擇時間序列模型
library(forecast)
auto_closeP<-auto.arima(closeP)
summary(auto_closeP)
#結果顯示每天收盤價格適合使用ARIMA(2,1,0)with drift模型
win.graph()
plot(forecast(auto_closeP,h=20))  #查看模型折線圖

##(e)  stochastic process with {ri,i = 2,...,N}
daily_cr=as.numeric(100*(diff(log(closeP))))  #定義每天複利報酬
#用Ljung-Box來檢測殘差值
Box.test(daily_cr,lag=20,type = "Ljung-Box")
#結果為p-value=0.5245 不拒絕虛無假設 代表數據有不具有自我相關性 不適合使用時間序列模型 
Box.test(daily_cr,lag=10,type = "Ljung-Box")
#結果為p-value = 0.1059 不拒絕虛無假設 代表數據有不具有自我相關性 
#故每天複利報酬不適合使用ARMA模型


###6 build up an ARMA(p,q) + GARCH(p,q)
##(a)  stochastic process with {Pi,t,t = 2,...,T −1}
minute_stock=as.numeric(data[1025,-1])   #先定義每分鐘股價
#建立模型之前 需要先確定每分鐘股價是否為平穩 
#根據第一題(a) 得到結論為拒絕虛無假設 資料不存在單根 為穩定態
#由第五題的(a)結果可知 每天收盤價格適合使用ARIMA(2,1,0)模型 
auto_minstoc<-auto.arima(minute_stock)
install.packages("TSA")
library(TSA)
minstoc_arima=arima(minute_stock, order=c(2,1,0))   #建立ARIMA模型
summary(minstoc_arima)
minstoc_arima_residuals<- residuals(minstoc_arima) #求出ARIMA模型的殘差
install.packages("fGarch")
library(fGarch)
minstoc_garch=garchFit(~garch(1, 1),minstoc_arima_residuals, trace = FALSE)
summary(minstoc_garch)
auto_minstoc<- diff(minute_stock)  #進行差分


##(b)  stochastic process with {ri,t,t = 2,...,T − 1}
min_cr=as.numeric(100*(diff(log(minute_stock))))  #定義minute compound return公式
#題目要求ARIMA+GARCH 但根據上面第5題(b) 每分鐘複利報酬不適用於ARMA模型 
#故本題無法使用ARIMA+GARCH

##(c) stochastic process with {CRi,t,t = 2,...,T − 1}
min_cumreturn=cumsum(min_cr)  #先定義每分鐘累積複利報酬
#建立模型之前 需要先確定每分鐘累積複利報酬是否為平穩 
#根據第一題(c) 得到結論為拒絕虛無假設 資料不存在單根 為穩定態
#由第五題的(c)結果可知 每天收盤價格適合使用ARIMA(2,1,0)模型 
auto_cumreturn<-auto.arima(min_cumreturn)  #第五題的(c)結果
install.packages("TSA")
library(TSA)
cumreturn_arima<-arima(min_cumreturn,order=c(2,1,0))  #建立ARIMA模型
summary(cumreturn_arima)   
cumreturn_arima_residuals<- residuals(cumreturn_arima) #求出ARIMA模型的殘差
install.packages("fGarch")
library(fGarch)
cumreturn_garch=garchFit(~garch(1, 1),cumreturn_arima_residuals, trace = FALSE)
summary(cumreturn_garch)
auto_min_cumreturn<- diff(min_cumreturn)  #進行差分


##(d)  stochastic process with {Pi,i = 1,...,N}
closeP=as.numeric(data[,267])  #定義每天的收盤價 並移除所有資料中的NA值
closeP=closeP[!is.na(closeP)]
#建立模型之前 需要先確定每分鐘累積複利報酬是否為平穩 
#根據第一題(d) 得到結論為拒絕虛無假設 資料不存在單根 為穩定態
#由第五題的(d)結果可知 每天收盤價格適合使用ARIMA(2,1,0)drift模型 
install.packages("TSA")
library(TSA)
closeP_arima<-arima(closeP,order=c(2,1,0))  #建立ARIMA模型
summary(closeP_arima)   
closeP_arima_residuals<- residuals(closeP_arima) #求出ARIMA模型的殘差
install.packages("fGarch")
library(fGarch)
closeP_garch=garchFit(~garch(1, 1),closeP_arima_residuals, trace = FALSE)
summary(closeP_garch)
auto_closeP<- diff(closeP)  #進行差分

##(e)  stochastic process with {ri,i = 2,...,N}
daily_cr=as.numeric(100*(diff(log(closeP))))  #定義每天複利報酬
#題目要求ARIMA+GARCH 但根據上面第5題(e) 每天複利報酬不適用於ARMA模型 
#故本題無法使用ARIMA+GARCH

