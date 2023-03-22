#добавление таблиц 
library(googlesheets4)
library(tidyverse)
library(dplyr)
library(dtplyr)
# аунтификация 
gs4_auth_configure(path = 'D:/Обучение/GA/GOOLTB/googltabl.json')
gs4_auth(email = 'asimonovnissan@gmail.com')
httr::oauth_app(appname = "client_id",
                key =  "338543304501-7bcighd0dgh57f6ta4cp13sjoevr66g2.apps.googleusercontent.com",
                secret = "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")
# Добавляем таблицу с продажами 
sales <- as_sheets_id('https://docs.google.com/spreadsheets/d/1I5Ebye3_g0NmpV2qyGCqJiLDTNHUKPUxkG3_J_HzrrQ/edit#gid=0',
                    '1I5Ebye3_g0NmpV2qyGCqJiLDTNHUKPUxkG3_J_HzrrQ/edit#gid=0')
sales <-  range_read(sales, sheet = 'Данные по заказам и пользователям')
# проверяем таблицу с продажами 
head(sales)
print(sales)
# убираем значение NA  делаем 0
sales$Revenue[is.na(sales$Revenue)] <- 0
sales$`Product SKU`[is.na(sales$`Product SKU`)] <- 0
sales$`Product Name`[is.na(sales$`Product Name`)] <- 0
print(sales)
str(sales)
sales$Date <- as.Date(sales$Date)
sales$`First entry` <- as.Date(sales$`First entry`)
str(sales)
#добавляем таблицу продукты 
tab12 <- as_sheets_id('https://docs.google.com/spreadsheets/d/1I5Ebye3_g0NmpV2qyGCqJiLDTNHUKPUxkG3_J_HzrrQ/edit#gid=0',
                      '1I5Ebye3_g0NmpV2qyGCqJiLDTNHUKPUxkG3_J_HzrrQ/edit#gid=0')
produkt <- range_read(tab12, 'Продукты')
#добавляем таблицу расходы на рекламу
consumption <- range_read(tab12,'Данные по расходам')
# групируем рассход по каналам 
library(aggregation)
#расход на рекламу сумма по каналам 
channel_flow <- aggregate( Расход ~ Канал, data = consumption, sum)
#доход от рекламы 
sales_channel <- tibble(sales$Source,sales$Revenue)
sales_channel_sum <-  aggregate(sales$Revenue~sales$Source, data = sales_channel, sum )
sales_channel_sum <- sales_channel_sum [-6,]
channel_flow <- mutate(channel_flow,sales_channel_sum$`sales$Revenue`)
colnames(channel_flow)[3] <- 'Доход'
# расчёт доход минус расход и добавление в таблицу по рекламе 
revenue <- (channel_flow$Доход - channel_flow$Расход)
channel_flow <- mutate(channel_flow, revenue)
# жилаемая прибыль 25%
living_profit <- (channel_flow$Доход*40)/100
channel_flow <- mutate(channel_flow, living_profit)
#расчёт выручки 
revenue_total <- (channel_flow$revenue - channel_flow$living_profit)
channel_flow <- mutate(channel_flow,revenue_total)
print(channel_flow) # расходы на рекламу 
# проведение retense  анализа 
unit <- range_read(tab12,'Unit')
colnames(unit) <- as.character(unlist(unit[1,],use.names = FALSE ))
unit <- unit[-1,]
unit$total_user <- c(12,13,12)
str(unit)

