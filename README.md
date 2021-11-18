# fllsdshbrd

app.r generates R shiny interface to upload and view analysis results to tickers in buys.csv file. The dashboard is a demonstration of using [tidyquant](https://cran.r-project.org/web/packages/tidyquant/tidyquant.pdf) to read and mutate stock tickers. The application might be useful to the retail investor seeking a snapshot view to purchased stocks peformance as a portfolio and their current market value.

buy.csv is the file that contains the ticker information for analysis.  Although a simple file in format, the following characteristics of the data columns in the file must be met for the application to function as expected.

  - __Description__: text, Company Name 
  - __Action__:  text, One of "Buy", "Sell", or "Deposit" (not a dividend)
  - __Symbol__: text, Yahoo symbol format of stock/ticker
  - __Date__: text, mm/dd/yyyy, Transaction date of Buy, Sell, or Deposit
  - __Quantity__: numeric, Number of shares/units purchased
  - __Currency__: text, Currency of purchase one of "CAD" or "USD"
  - __BuyPrice__: numeric, Share purchase price

| Description  | Action  | Symbol | Date      | Quantity | Currency | BuyPrice |
|--------------|---------|--------|-----------|----------|----------|----------|
| Magna Int.   | BUY     | MG.TO  | 5/14/2014 | 25       | CAD      | 45.71    |
| Atlas        | BUY     | ATCO   | 3/28/2018 | 75       | USD      | 5.43     |
| Contribution | DEPOSIT |        | 5/01/2014 |          | CAD      | 1700.00  |


---
Usage
* Pre req is install of R, R Studio and shiny.
* Open app.r in R Studio and run the application (app.r)
* Edit buys.csv to capture your stock purchases and investment contributions
* Upload the edited buys.csv to the application
* Review the results and __marvel__ at your stock picking prowess
---
