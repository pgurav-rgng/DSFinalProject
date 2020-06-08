library(quantmod)
library(shiny)
library(dygraphs)
library(prophet)
library(DT)
library(tidyquant)
library(data.table)
library(xts)
library(dplyr)

shinyServer(function(input, output) {
    output$dygraph <- renderDygraph({
        output$text1 <- renderText({paste("Output: ", input$symb)})
        validate(
            need(input$symb != "", "")
        )
        if(input$ptype == 1){
            tryCatch({
                data <- getSymbols(
                    input$symb,
                    src = "yahoo",
                    from = "2008-01-01",
                    to = "2019-01-01",
                    auto.assign = FALSE
                )
                mydf <- data.frame(data[,4])
                setDT(mydf, keep.rownames = TRUE)
                colnames(mydf)<- c("ds", "y")
                mydf$ds<-ymd(mydf$ds)
                dygraph(mydf,main = "Stock Data from Jan 2010 to Jan 2018") %>% dyRangeSelector(dateWindow =c("2010-01-01", "2018-01-01"))
            },
            error=function(e) {
                output$text1 <- renderText({paste(input$symb, " Error occurred")})
                return(NULL) 
            })
        }
        else if (input$ptype == 2){
            tryCatch({
                data <- getSymbols(
                input$symb,
                src = "yahoo",
                from = "2008-01-01",
                to = "2019-01-01",
                auto.assign = FALSE
                )
                mydf <- data.frame(data[,4])
                mydf <- copy(mydf)
                setDT(mydf, keep.rownames = TRUE)
                colnames(mydf)<- c("ds", "y")
                mydf$ds<-ymd(mydf$ds)
                #Leave one year out and use the remaining data for prediction
                df2017 <- mydf %>%
                    filter(ds >= as.Date("2010-01-01") & ds <= as.Date("2017-12-31"))
                ### Log Transformed data
                ds<-df2017$ds
                y<-log(df2017$y)
                df2017log<-data.frame(ds,y)
                head(df2017log)
                
                ### Plot of Forecast for Next 365 Days
                m=prophet(df2017log, daily.seasonality = TRUE)
                future<-make_future_dataframe(m, periods=365)
                forecast <-predict(m,future)
                dyplot.prophet(m,forecast,main = "Fb.Prophet prediction Conf. Intervals on Log scale for 2018-19 ") 
            },
            error=function(e) {
                output$text1 <- renderText({paste(input$symb, " Error occurred")})
                return(NULL) 
            })
        } 
        else if (input$ptype == 3){
            tryCatch({
                data <- getSymbols(
                    input$symb,
                    src = "yahoo",
                    from = "2008-01-01",
                    to = "2019-01-01",
                    auto.assign = FALSE
                )
                
                mydf <- data.frame(data[,4])
                mydf <- copy(mydf)
                setDT(mydf, keep.rownames = TRUE)
                colnames(mydf)<- c("ds", "y")
                mydf$ds<-ymd(mydf$ds)
                #head(mydf)
                
                #Leave one year out and use the remaining data for prediction
                df2017 <- mydf %>%
                    filter(ds >= as.Date("2010-01-01") & ds <= as.Date("2017-12-31"))
                #qplot(ds,y, data=df2017, main="Historical Price Chart")
                
                ### Log Transformed data
                ds<-df2017$ds
                y<-log(df2017$y)
                df2017log<-data.frame(ds,y)
                head(df2017log)
                
                ### Plot of Forecast for Next 365 Days
                m=prophet(df2017log, daily.seasonality = TRUE)
                future<-make_future_dataframe(m, periods=365)
                forecast <-predict(m,future)
                dyplot.prophet(m,forecast) 
                #prophet_plot_components(m, forecast)
                
                #Plot historical data overlapped with Predicted data
                date <- df2017$ds
                val <-df2017$y
                df1=data.frame(date,val)
                
                date <- forecast$ds
                val <-exp(forecast$yhat)
                df2=data.frame(date,val)
                
                #Merge two datasets, training data set and Prediction
                dfA<-rbind(df1,df2)
                
                #This graph is prepared to show how predicted data compares with actual values
                df2018 <- mydf %>% filter(ds >= as.Date("2010-01-01") & ds <= as.Date("2018-12-31"))

                date <- df2018$ds
                val <-df2018$y
                df3=data.frame(date,val)
                
                df2T<- as.xts(x=df2,df2$date)
                
                df3T<- as.xts(x=df3,df3$date)
                
                dygraph1 <- cbind(df2T, df3T)
                dw<-c("2010-01-01", "2018-12-31")
                dygraph(dygraph1, main = "Predicted versus Actual reality till Jan 2019") %>% dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>% dyRangeSelector(dateWindow =dw) %>% dyLegend(show = "never", hideOnMouseOut = FALSE)

              },
            error=function(e) {
                output$text1 <- renderText({paste(input$symb, "Error")})
                return(NULL) 
            })
        }
    }) 
})
