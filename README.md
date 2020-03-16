# SYPA
This repository houses the python and R code underlying the analysis of Jake Schneider's Harvard Master's Thesis, called a Second Year Policy Analysis (SYPA).

**Project Overview** <br>
<br>
Just like with investments made in companies on the stock market, investments made in foreign countries can—and often do—diverge from economic fundamentals. Spurious factors such as bias, false perception and lack of true information about business climates in nations around the world can prejudice international investors from making capital investments in markets that might otherwise deserve it. On the converse, other times over-confidence in “established” economies can bid up more investment in that country than is truly deserved. The result is that some countries are over-invested in Foreign Direct Investment (FDI), while others are under-invested. Utilizing various statistical models, including multivariate regression, tree-based approaches and deep neural networks (DNN), I introduce a new methodology for analyzing over- and under-invested markets around the world and thereby suggest which nations are most over- and under-invested for foreign direct investments. The key findings from the empirical analysis demonstrate, first and foremost, that dislocations of FDI inflows from fundamental value are ubiquitous, and, second, that the most over-valued country for FDI inflows in the world is the United States, consistent with the over-valuation of the American bond and currency markets. Finally, this report concludes with public policy recommendations for international investors and national governments.

**Key Charts** <br>

Figure 1: Bar Chart of Over- and Under-Invested Countries Based Upon the DNN Model

![GitHub Logo](https://github.com/jschneids13/SYPA/blob/master/3_Outputs/Results/Predicted%20FDI%20by%20Country%2C%20Avg%20Value%202015-2019.jpg)

Source: World Bank, UN Comtrade Database, Bloomberg and Author’s Calculations.

Figure 2: Security Market Line for FDI Markets

![GitHub Logo](https://github.com/jschneids13/SYPA/blob/master/3_Outputs/Results/Actual%20Vs%20Predicted%20FDI%20by%20Country%2C%20Avg%20Value%202015-2019.jpg)

Source: World Bank, UN Comtrade Database, Bloomberg and Author’s Calculations.

Figure 3: FDI Inflows World Map, 2015 - 2019

![GitHub Logo](https://github.com/jschneids13/SYPA/blob/master/3_Outputs/Results/FDI%20Valuation%20Map.png)
 
Source: World Bank, UN Comtrade Database, Bloomberg and Author’s Calculations.


**Key Findings:** <br>
1.	Dislocations of FDI inflows from fundamental value occur and are ubiquitous. Only one country (Benin) received FDI inflows within 1% of predicted values and every other country deviated by as much as 212% over-valuation and infinite under-valuation.
2.	The United States’ was the most-overvalued country for FDI inflows. This is consistent with the premium that the US commands on other economic assets, including on the international debt and currency markets.
<br>

**Public Policy Recommendations:** <br>

*For Investors* <br>

Investors and national governments alike might wonder how best to utilize this information. For investors, these country rankings ought not to be thought of a concrete investment strategy in of itself. Instead, this machine learning approach to FDI valuation is an important first step in the process of asset allocation that quickly and accurately cuts through the universe of available information to provide a foundation for investment decisions. Building from this starting point, investors should then consider their individual investment constraints, including the allocation time horizon, specifics of the market and necessities of the location for the investment when making their final decision on where to outlay scarce capital. 

For instance, an MNC considering an FDI investment in a solar plant might consider Kuwait and Qatar (ranked 9 and 10 respectively for most under-invested) as opposed to other locations that also receive a commensurate amount of sun, such as Egypt (the fifth most over-invested market according to the algorithm). However, after cutting through these initial location decisions using the algorithm’s predictions, the investor should then use bespoke, qualitative investment research to decide where further to invest. Continuing with our previous example, when deciding between Kuwait and Qatar, which are both ranked nearly identically according to this author’s algorithm, the MNC should then consider the context of each location and its applicability to the MNC’s investment needs. Perhaps the MNC would benefit from port access for its supply chains, thus making Qatar’s capital of Doha, with its extensive port system, more attractive. In contrast, perhaps the MNC has a company mandate to invest in stronger political institutions, thus making Kuwait a more viable option. Thus, this example demonstrates that the predictions from this algorithm should not be seen as dogma, but instead should be considered within the larger context of the investor’s needs.

*For National Governments*

On the converse, national governments can stand to benefit tremendously from this research as well primarily through answering the question: “How can our national markets become more attractive to foreign investors given what information that we know about how investors make their capital allocation decisions.” To answer this, the author draws upon the individual criteria that the lasso algorithm found to be most predictive of investment decisions. 

These five most predictive variables (according to lasso) are: 
1.	Battle Related Deaths, 
2.	Current Health Expenditures per Capita, 
3.	Cereal Yield, 
4.	Presence of Peacekeepers, and 
5.	Secure Internet Servers

As all of these variables had a positive relationship with FDI inflows, some naïve public policy analysts might suggest that national governments should seek to focus primarily on increasing these variables. However, this strategy would have shortcomings for several reasons. First, although these are the critical variables for the lasso model, it is not clear that these are the most significant variables for the DNN model that was employed to produce the country predictions. Second, even if these were the most critical variables, the magnitude on each variable is so small that simply focusing on these 5 individual indicators would likely do little to tip the skills on the predictions for how much FDI inflows that country should receive. Finally (and most importantly), narrow public policy recommendations that singularly focus on a few indicators are rarely successful because real-life policy is never so simple. One cannot expect to merely increase a few ticks of five variables and expect a dramatically better outcome.

These admonishments serve as the backdrop for the overriding policy recommendation to national governments attempting to improve their attractiveness for FDI: rather than focus on a few disparate factors, attempt to increase all variables a small amount in synchronicity. As the apocryphal economic quote states, “A rising tide lifts all ships,” so too should national governments attempt to lift all ships through wide-ranging reforms. 

Through the academic research and this author’s own empirics, it is clear what types of variables are critical for attracting FDI inflows: political, economic and risk variables. Thus, national governments should seek to slowly and incrementally improve their economic and political institutions through broad-based market reforms in order to strengthen their country-level fundamentals. This shoring up fundamentals will then translate into a higher predicted FDI scores, greater investment from international investors, and improved national prosperity.







