Please set our directory in the main program cryptoSignal.R
The program can have difficulties in running on Windows. My current setup was on bash on Windows. i7 core with 8GB of RAM.
The program consists of detecting the best signals in Crypto market.

<p align="center">
   <h1> Crypto Market Cross-Sectional regression signals </h1>
</p>
Cyril de Lavergne cdldl@connect.ust.hk

<h3>Abstract</h3>

We compare signals such as 100 technical indicators on 1000 crypto currency available on most broker platform via cross sectional regression. We compare the performance of each signal predictions to the daily expected returns at t+1, cumulative returns of expected returns between t+1 and t+7, the Sharpe ratio between and t+1 and t+7 and the expected volatility between t+1 and t+7 in the crypto market. We demonstrate that each prediction has different ideal signals with varying Sharpe ratio and that the volatility is in general easier to predict that other signals. Some signals obtain a Sharpe Ratio above 6 before transactions costs and spread. The strategy remains to be implemented while including transaction costs, spread and liquidity of such available products.

Keywords: Crypto Market, Big Data, Cross-sectional regression, Data Mining, Trading Strategy

<h3>1 Previous Work</h3>

Northfield US Fundamental Risk model (Northinfo., 2015) used fundamental risk factor to predict US stock market and manage risk across factors. They demonstrate that a quantitative model can allow asset managers to control their risk exposure based on their factors. A factor is namely a signal that we use to trade on. It could be dividend yield, size or even value of one stock. A cross-sectional regression is used in their model to monitor the whole stock market. A cross-sectional regression is a time independent regression and check the overall situation of the market. For instance, we regress dividend yields of all stocks at time t against one predictor. One predictor could be the expected returns at time t+1.

<h3> 2 Characteristics of the dataset </h3>

Data was taken from Kaggle website (Vent J., 2017) that features every crypto historical data since 2013. Crypto markets is opened 24 hours 7days out of 7, hence unlike Stock market data, the Sharpe ratio should be compounded by sqrt(365) and not sqrt(252) to convert daily signals to annual. In addition, the dataset contains 700,000rows with Open, high, low, close and volume information. 

<h3> 3 Overview of the system </h3>

We make extensive use of data.table (Dowle, M., 2017) library to manipulate such big dataset. Technical indicators are created by parallel programming. I recommend any user to parallelize the cross-sectional regression to maximize the efficiency for further research.

Most technical indicators in the R library TTR (Ulrich, J., 2017) were regressed against predictors quoted in abstract.

<h3> 4 Results</h3>

<h4>ForRet</h4>

The predominant indicators for predicting returns at time t+1 are:
 
 ![alt text](https://github.com/cdldl/Crypto-Signal/blob/master/forRet.png)
 
This means that we would hold a position of one day. In that scenario, we should carefully consider transaction costs and spread before making a trade. 

The feature ret7 can provide an annual Sharpe Ratio of 6.29 before transaction costs and spread.
The mean p-values of these indicators are available once you go through the code. The lowest p-value means that the value is statistically significant. In this paper, a p-value lower than 0.5 has more chances to be accurate.
The mean R Squared is 0.5457959.

<h4> ForRet7</h4>
The best indicators of the cumulative returns of the next 7days are:

 ![alt text](https://github.com/cdldl/Crypto-Signal/blob/master/forRet7.png)

The mean R squared is 0.4951624.

<h4>ForSharpe7</h4>

The best indicators to obtain the best Sharpe Ratio over the next 7days are:

![alt text](https://github.com/cdldl/Crypto-Signal/blob/master/forSharpe7.png)

The mean R squared is 0.4439898.

<h4> ForVol </h4>

The best indicators to obtain the best prediction of the volatility over the next 7days are:

![alt text](https://github.com/cdldl/Crypto-Signal/blob/master/forVol.png)

Please ignore r.squared in the image above.
The mean R squared of this indicator is: 0.6513996.

<h4> 5 Conclusion </h4>

To conclude, we have shown that a strategy based on historical data can be used in crypto markets. Indicators can have a varying Sharpe Ratio and the R squared demonstrated that future volatility is in general easier to predict. Hence, we would predict the future returns and volatility independently and rank these factors to have an optimal signal. If we were to trade crypto markets we would also carefully consider correlation between the best signals in this paper to control risk as in the Northfield US Fundamental Risk model.  We would probably minimize the expected shortfall decomposition of these signals or maximize the CRRA indicator that skew the most returns upwards and lower tail (kurtosis) risk (to see in our next paper). 

<h4> 6 References </h4>

1.	Vent J., (2017). All Crypto Currencies. Retrieved from https://www.kaggle.com/jessevent/all-crypto-currencies 
2.	Dowle, M. (2017) Data.table. Retrieved from https://cran.r-project.org/web/packages/data.table/data.table.pdf  
3.	Ulrich, J. (2017). TTR. Retrieved from https://cran.r-project.org/web/packages/TTR/TTR.pdf  


