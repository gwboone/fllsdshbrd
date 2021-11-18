# fllsdshbrd

app.r generates R shiny interface to upload and view analysis results to tickers in buys.csv file. The dashboard is a demonstration of using [tidyquant](https://cran.r-project.org/web/packages/tidyquant/tidyquant.pdf) to read and mutate stock tickers. The application might be useful to the retail investor seeking a snapshot view to purchased stocks peformance as a portfolio and their current market value.

buy.csv is the file that contains the ticker information for analysis.  Although a simple file in format, the following characteristics of the data columns in the file must be met for the application to function as expected.

  >__Description__:  Company Name [text]
  >__Action__:  one of "Buy", "Sell", or "Deposit" (not a dividend) [text]
  >__Symbol__: Ticker [yahoo format of a stock/ticker symbol]
  >__Date__: Date of Buy, Sell, Cash Deposit [mm/dd/yyyy]
  >__Quantity__: Number of shares purchased [numeric]
  >__Currency__: Currency of purchase one of "CAD" or "USD" [text]
  >__BuyPrice__: Share purchase price [numeric]

---
Usage
* Pre req is install of R, R Studio and shiny.
* Open app.r in R Studio and run the application (app.r)
* Edit buys.csv to capture your stock purchases and investment contributions
* Upload the edited buys.csv to the application
* Review the results and __marvel__ at your stock picking prowess
---
