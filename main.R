# 依赖库
# 数据处理加工
library(tidyverse)
# 检查同民函数
library(conflicted)
# 方便地做日期/时间操作，各种标准化时间和时区的处理
suppressPackageStartupMessages(library(lubridate))
# 绘图
suppressPackageStartupMessages(library(scales))
library(ggplot2)
library(ggridges)
# package 顺序
# search()


# 读取数据
# 不变成属性数据,按字符串读入
#数据集中提供了三个文件，如下所述：
# 伤害记录：.csv格式的伤害记录文件包含有关两个赛季的常规赛期间发生的下肢受伤的信息。可以使用PlayerKey，GameID和PlayKey字段将伤害链接到玩家历史记录中的特定记录。
injury_record <- data.table::fread("../data/InjuryRecord.csv", stringsAsFactors = F)
# 有关比赛和比赛的详细信息包括球员分配的名册位置，体育场类型，场地类型，天气，比赛类型，比赛位置和位置组。
player_tracking <- data.table::fread("../data/PlayerTrackData.csv", stringsAsFactors = F)
# 玩家跟踪数据：描述以10 Hz录制的播放过程中每个玩家的位置，方向，速度和方向的玩家级别数据（即，每秒记录10个观测值）。
play_list <- data.table::fread("../data/PlayList.csv", stringsAsFactors = F)

# 绘图设置
theme_jason <- function(legend_pos="top", base_size=13, font=NA){
  
  # 文本内容设置
  # 正文
  txt <- element_text(size = base_size, colour = "black", face = "plain")
  # 粗体
  bold_txt <- element_text(size = base_size, colour = "black", face = "bold")
  
  # 使用theme_minimal（）主题
  theme_minimal(base_size = base_size, base_family = font)+
    theme(text = txt,
          # 轴线标题和文本设置
          axis.title.x = element_text(size = base_size+4, hjust = 1),
          axis.title.y = element_text(size = base_size+4, hjust = 1),
          # 绘图上的网格线
          panel.grid.major = element_line(linetype = 2),
          panel.grid.minor = element_line(linetype = 2),
          # 标题和副标题文本
          plot.title = element_text(size =  base_size+8, colour = "grey25", face = "bold"),
          plot.subtitle = element_text(size =  base_size+5, colour = "grey44"),
          
          # 清理
          legend.key = element_blank(),
          # 参数用于分页
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size =  base_size+1, colour = "grey35")) +
    
    #轴线
    theme(
      #图例依赖于函数中的参数而无标题
      legend.position = legend_pos,
      legend.title = element_blank(),
      legend.background = element_rect(fill = NULL, size = 0.5,linetype = 2)
      
    )
}

# 多种基本色 丰富图表
plot_cols <- c("#99CC00", "#FFCC99", "#00FFCC", "#CCFF66", "#AA44CC", "#559911", "#22AA88", "#7700AA")

# 数据清洗
  # 球场类型数据清洗
play_list %>% 
  count(StadiumType) %>% 
  rename(Count = n) %>% 
  mutate(Count = comma(Count))

# 包括清洗错别字情况
# 户外的
outdoor <- c('Outdoor', 'Outdoors', 'Cloudy', 'Heinz Field', 
             'Outdor', 'Ourdoor', 'Outside', 'Outddors', 
             'Outdoor Retr Roof-Open', 'Oudoor', 'Bowl')

# 室内关闭的
indoor_closed <- c('Indoors', 'Indoor', 'Indoor, Roof Closed', 'Indoor, Roof Closed',
                   'Retractable Roof', 'Retr. Roof-Closed', 'Retr. Roof - Closed', 'Retr. Roof Closed')

# 室内开放的
indoor_open <- c('Indoor, Open Roof', 'Open', 'Retr. Roof-Open', 'Retr. Roof - Open')

# 圆顶关闭的
dome_closed <- c('Dome', 'Domed, closed', 'Closed Dome', 'Domed', 'Dome, closed')

# 圆顶开放的
dome_open <- c('Domed, Open', 'Domed, open')

convert_stadiums <- function(x) {
  if(x %in% outdoor) {
    "outdoor"
  } else if(x %in% indoor_closed) {
    "indoor closed"
  } else if(x %in% indoor_open) {
    "indoor open"
  } else if(x %in% dome_closed) {
    "dome_closed"
  } else if( x %in% dome_open) {
    "dome_open"
  } else {
    "unknown"
  }
  
}    

play_list <- play_list %>% 
  mutate(StadiumType = mapply(convert_stadiums, StadiumType))

  # 天气类型数据清洗
play_list %>% 
  count(Weather) %>% 
  rename(Count = n) %>% 
  mutate(Count = comma(Count)) %>% 

rain <- c('30% Chance of Rain', 'Rainy', 'Rain Chance 40%', 'Showers', 'Cloudy, 50% change of rain', 'Rain likely, temps in low 40s.',
          'Cloudy with periods of rain, thunder possible. Winds shifting to WNW, 10-20 mph.',
          'Scattered Showers', 'Cloudy, Rain', 'Rain shower', 'Light Rain', 'Rain')

overcast <- c('Party Cloudy', 'Cloudy, chance of rain',
              'Coudy', 
              'Cloudy and cold', 'Cloudy, fog started developing in 2nd quarter',
              'Partly Clouidy', 'Mostly Coudy', 'Cloudy and Cool',
              'cloudy', 'Partly cloudy', 'Overcast', 'Hazy', 'Mostly cloudy', 'Mostly Cloudy',
              'Partly Cloudy', 'Cloudy')

clear <- c('Partly clear', 'Sunny and clear', 'Sun & clouds', 'Clear and Sunny',
           'Sunny and cold', 'Sunny Skies', 'Clear and Cool', 'Clear and sunny',
           'Sunny, highs to upper 80s', 'Mostly Sunny Skies', 'Cold',
           'Clear and warm', 'Sunny and warm', 'Clear and cold', 'Mostly sunny',
           'T: 51; H: 55; W: NW 10 mph', 'Clear Skies', 'Clear skies', 'Partly sunny',
           'Fair', 'Partly Sunny', 'Mostly Sunny', 'Clear', 'Sunny')

snow <- c('Cloudy, light snow accumulating 1-3"', 'Heavy lake effect snow', 'Snow')

none <- c('N/A Indoor', 'Indoors', 'Indoor', 'N/A (Indoors)', 'Controlled Climate')

convert_weather <- function(x) {
  if(x %in% rain) {
    "rain"
  } else if(x %in% overcast) {
    "overcast"
  } else if(x %in% clear) {
    "clear"
  } else if(x %in% snow) {
    "snow"
  } else if( x %in% none) {
    "indoors"
  } else {
    "unknown"
  }
  
}    

play_list <- play_list %>% 
  mutate(Weather = mapply(convert_weather, Weather))

  #伤害记录数据清洗
p <- injury_record %>% 
  count(BodyPart) %>% 
  ggplot(aes(x= reorder(BodyPart,n), y= n)) +
  geom_col(fill = plot_cols[1], colour = plot_cols[1], alpha = 0.6) +
  labs(x= "Body Part", y= "Num Injuries") +
  ggtitle("NOT ALL NFL BODY PARTS ARE CREATED EQUAL", subtitle = "86% of the injury records made up by knees and ankles") +
  coord_flip() +
  theme_jason()
gridExtra::grid.arrange(p)

