rm(list = ls())
graphics.off()

#################################################################################

# 有用到的
library(geosphere)
library(magrittr)
library(dplyr)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)

#################################################################################

# 分好群資料
moun3B = read.csv("C:/Users/user/Desktop/gg3B0.csv")
head(moun3B)
moun = moun3B[-c(6, 8)]
moun$labels = as.factor(moun$labels + 1)
head(moun)  # moun

#################################################################################

# 縣市座標
city <- data.frame(name = c("臺北"  , "桃園"  , "新竹"  , "苗栗"  , "臺中"  , "南投"  , "彰化"  , "雲林"  , "嘉義"  , "臺南"  , "高雄"  , "屏東"  , "宜蘭"  , "花蓮"  , "臺東"),
                   long = c(121.5176, 121.3144, 120.9716, 120.8224, 120.6853, 120.6875, 120.5383, 120.5406, 120.4411, 120.2176, 120.3025, 120.4860, 121.7583, 121.6012, 121.1233), 
                   lati = c(25.0486 , 24.9889 , 24.8016 , 24.5699 , 24.1355 , 23.9052 , 24.0815 , 23.7072 , 23.4790 , 22.9981 , 22.6394 , 22.6688 , 24.7552 , 23.9932 , 22.7936))
city

library(geosphere)
# 計算兩點空間距離 (單位：m)
# distGeo(city[1, 2:3], city[2, 2:3])

#################################################################################

# 與山的距離計算
# (法一：直接變新的column)
head(moun)
city[1, 2:3]
moun[1, 2:3]
distGeo(city[3, 2:3], moun[40749, 2:3])

library(magrittr)
library(dplyr)
# taipei = lapply(1:40749, FUN = function(x){floor(distGeo(city[1, 2:3], moun[x, 1:2]))}) %>% do.call(rbind, .)
# taipei

dist_city = NULL
for(i in 1:15){
  a = lapply(1:40749, FUN = function(x){floor(distGeo(city[i, 2:3], moun[x, 2:3]))}) %>% do.call(rbind, .)
  dist_city = c(dist_city, a)
  dist_city = matrix(dist_city, ncol = 15)
}
dist_city = data.frame(dist_city)
colname = c(city$name)
names(dist_city) <- colname
dist_city

#???為啥dist_city會有40755列???
##
dist_city[1:5,]
dist_city[40748:40755,]
floor(distGeo(city[1:15, 2:3], moun[2, 2:3]))
##先不管了找不到

dist_moun = cbind(moun, dist_city[1:40749, ])
dist_moun
# write.csv(dist_moun, file = "mouncitydist.csv")

# (法二：及時連動)
#但幾累


#################################################################################

dist_moun = read.csv("C:/Users/user/Desktop/mouncitydist.csv")
head(dist_moun)

shiny_moun = dist_moun[-2]
names(shiny_moun) <- c("id", "經度", "緯度", "平均海拔", "平均速度", "爬山時長(小時)", "是否過夜", "等級", colname)
shiny_moun

# 將爬山時長單位變秒
shiny_moun$`爬山時長(小時)` <- round((shiny_moun$`爬山時長(小時)`/3600), 2)
shiny_moun

# 把等級變回numeric先(?)
shiny_moun$等級 = as.numeric(shiny_moun$等級)
head(shiny_moun)

#################################################################################

# 定義篩選條件
# 與居住地距離
summary(dist_moun[9:23])
boxplot(dist_moun[9:23])
# 與居住地距離：先設方圓15km內 (= 15000m)

# 時長、海拔
# 只留用戶篩選範圍的點

# 等級
# 只推比自己大or等於的等級
# 所以如果等級4就看不到等級123的了(?)

#################################################################################

# shiny_moun

library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)

# dashboardHeader(): 儀表板標頭
# dashboardSidebar(): 側邊選單
# dashboardBody(): 主頁面

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "山那路線推薦",
                  
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 10, color = "olive",
                                        "目前爬山等級"),
                               taskItem(value = 60, color = "yellow",
                                        "行程規劃進度"),
                               taskItem(value = 2, color = "red",
                                        "百岳攻頂紀錄")),
                  
                  dropdownMenu(type = "notifications",
                               notificationItem(text = "個人資料已更新",
                                                icon = icon("edit")),
                               notificationItem(text = "48個新好友",
                                                icon = icon("user-friends")),
                               notificationItem(text = "已完成一項任務",
                                                icon = icon("tasks"))),
                  
                  dropdownMenu(type = "messages",
                               messageItem(from = "新用戶",
                                           message = "你好嗎？考完期末考了嗎？",
                                           icon = icon("hand-sparkles"),
                                           time = "2021-06-26"),
                               messageItem(from = "幫助",
                                           message = "還沒得到山神的力量嗎？點擊了解詳情",
                                           icon = icon("bible"),
                                           time = "2021-06-26"))
                  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("出發！", tabName = "id1",
               icon = icon("dashboard")),
      menuItem("推薦路線", tabName = "id2",
               icon = icon("thumbs-up")),
      menuItem("路線地圖", tabName = "id3",
               icon = icon("mountain"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "id1",
              fluidPage(
                column(box(selectInput(inputId = "地區", label = "所在地區", choices = c("臺北" = "臺北",
                                                                                         "桃園" = "桃園",
                                                                                         "新竹" = "新竹",
                                                                                         "苗栗" = "苗栗",
                                                                                         "臺中" = "臺中",
                                                                                         "南投" = "南投",
                                                                                         "彰化" = "彰化",
                                                                                         "雲林" = "雲林",
                                                                                         "嘉義" = "嘉義",
                                                                                         "臺南" = "臺南",
                                                                                         "高雄" = "高雄",
                                                                                         "屏東" = "屏東",
                                                                                         "宜蘭" = "宜蘭",
                                                                                         "花蓮" = "花蓮",
                                                                                         "臺東" = "臺東")), height = 340),
                       box(sliderInput(inputId = "時長", label = "預計爬山時長 (小時)", min = 1, max = 20, value = c(2, 5)),
                           sliderInput(inputId = "海拔", label = "可負荷之海拔高度範圍 (公尺)", min = 0, max = 5000, value = c(1000, 3000))),
                       
                       box(selectInput(inputId = "等級", label = "個人等級", choices = c("健行登山" = 1, "新手入門" = 2,
                                                                                         "不錯會爬" = 3, "我是專家" = 4))),
                       width = 8)
                )
              ),
      
      tabItem(tabName = "id2", 
              column(box(numericInput("n", label = "推薦筆數", value = 10)), width = 5),
              fluidRow(column(width = 12,
                              DT::dataTableOutput("recommend"), style = "height:5000px; overflow-y: scroll;overflow-x: scroll;"))
              ),
      
      tabItem(tabName = "id3", 
              fluidPage(leafletOutput("map"))
              )
      
    )
  )
  
)

# shinyApp(ui = ui, server = server)





server <- function(input, output){
  
  output$recommend <- DT::renderDataTable({
    showtable <- data.frame(shiny_moun[which(shiny_moun$平均海拔 >= input$海拔[1] & shiny_moun$平均海拔 <= input$海拔[2] &
                                             (shiny_moun$`爬山時長(小時)`) >= input$時長[1] & (shiny_moun$`爬山時長(小時)`) <= input$時長[2] &
                                             shiny_moun[input$地區] <= 15000 &
                                             shiny_moun$等級 >= input$等級), ])
    
    
    head(showtable[order(showtable$等級), 2:8], input$n)
    
  })
  
  
  output$map <- renderLeaflet({
    showtable <- data.frame(shiny_moun[which(shiny_moun$平均海拔 >= input$海拔[1] & shiny_moun$平均海拔 <= input$海拔[2] &
                                             (shiny_moun$`爬山時長(小時)`) >= input$時長[1] & (shiny_moun$`爬山時長(小時)`) <= input$時長[2] &
                                             shiny_moun[input$地區] <= 15000 &
                                             shiny_moun$等級 >= input$等級), ])
    
    leaflet() %>% 
      addTiles() %>%
      addMarkers(lng = showtable$經度[1:input$n], lat = showtable$緯度[1:input$n]) %>%
      setView(lng = city[which(city$name == input$地區), 2], lat = city[which(city$name == input$地區), 3], zoom = 11)

  })
  
}

shinyApp(ui = ui, server = server)

#################################################################################

# 嗨和和這裡下面不用跑
##ssss
head(shiny_moun)
shiny_moun[40748:40749, ]
distGeo(city[, 2:3], moun[40748, 2:3])

length(which(shiny_moun["臺北"] <= 2000))
##ssss
##tttt
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = shiny_moun$經度[1:2], lat = shiny_moun$緯度[1:2]) %>%
  setView(lng = city[which(city$name == "臺北"), 2], lat = city[which(city$name == "臺北"), 3], zoom = 10)
##
leaflet() %>% 
  #addProviderTiles("Stamen.Toner") %>%
  addTiles() %>%
  #setView(lng = 120.284586, lat = 22.73445, zoom = 10) %>%
  addMarkers(lng = showtable$經度[1:input$n], lat = showtable$緯度[1:input$n], popup = as.character(showtable$X))
##tttt








