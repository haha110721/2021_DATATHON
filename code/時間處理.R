# 2020.1月資料
rjan = read.csv("C:/Users/user/Desktop/20201路.csv")
head(rjan, 10)

##########################################################

# 時間
library(lubridate)
time = dmy_hms(rjan[, 6])  # 變成as.POSIXct格式
head(time, 10)

# 取出年月日
date = as.character(time, format = "%Y%m%d")
head(date, 10)
# 取出時分秒
hms = as.character(time, format = "%H%M%S")
head(hms, 10)

# 跟原始資料併一起成新欄位
rjan$date = date
rjan$hms = hms
head(rjan, 10)

##########################################################

# 上傳時間
variable.names(rjan)
time_upd = dmy_hms(rjan[, 7])
head(time_upd, 10)

# 取出上傳時間年月日
date_upd = as.character(time_upd, format = "%Y%m%d")
head(date_upd, 10)
# 取出上傳時間時分秒
hms_upd = as.character(time_upd, format = "%H%M%S")
head(hms_upd, 10)

# 跟原始資料併一起成新欄位
rjan$date_upd = date_upd
rjan$hms_upd = hms_upd
head(rjan, 10)

