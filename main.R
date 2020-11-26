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

# 数据转化
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
  mutate(Count = comma(Count))

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

# 绘图设置
theme_jason <- function(legend_pos="top", base_size=13, font=NA){
  
  # 文本内容设置
  # 正文
  txt <- element_text(size = base_size, colour = "black", face = "plain",family = "STXihei")
  # 粗体
  bold_txt <- element_text(size = base_size, colour = "black", face = "bold",family = "STXihei")
  
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

#伤害记录数据分析
p <- injury_record %>% 
  count(BodyPart) %>% 
  ggplot(aes(x= reorder(BodyPart,n), y= n)) +
  geom_col(fill = plot_cols[1], colour = plot_cols[1], alpha = 0.6) +
  labs(x= "身体部位", y= "受伤次数") +
  ggtitle("NFL运动员各身体部位受伤次数", subtitle = "86%的受伤发生在是由膝盖和脚踝") +
  coord_flip() +
  theme_jason()

p <- injury_record %>% 
  count(Surface) %>% 
  ggplot(aes(x= Surface, y= n)) +
  geom_col(fill = plot_cols[1], colour = plot_cols[1], alpha = 0.6) +
  labs(x= "场地类型", y= "受伤次数") +
  ggtitle("不同的场地类型与受伤次数", subtitle = "人造场地的伤害要比自然场地稍微多一些") +
  coord_flip() +
  theme_jason()

# gridExtra::grid.arrange(p, p2)

p <- injury_record %>% 
  ggplot(aes(x=BodyPart, fill = Surface, colour = Surface)) +
  geom_bar(stat = "count", position = "fill", alpha = 0.6) +
  scale_fill_manual(values = plot_cols[c(2,3)]) +
  scale_colour_manual(values = plot_cols[c(2,3)]) +
  scale_y_continuous(labels = percent) +
  ggtitle("受伤部位和场地存在一定关系", subtitle = "60%的踝关节和膝关节损伤发生在人造场地上") +
  theme_jason() +
  theme(axis.title.y = element_blank())

#gridExtra::grid.arrange(p)

# 运动员追踪数据清洗
player_tracking <- player_tracking %>% 
  mutate(isInjured = PlayKey %in% injury_record$PlayKey) 

player_tracking_summary <- player_tracking %>% 
  group_by(PlayKey) %>% 
  summarise(min_time = min(time, na.rm = T),
            max_time = max(time, na.rm = T),
            avg_speed = mean(s, na.rm = T),
            sd_speed = sd(s, na.rm = T)) %>% 
  mutate(play_time = max_time - min_time) %>% ungroup()

# 左连接查询
player_tracking_summary <- player_tracking_summary %>% 
  left_join(play_list, by = "PlayKey")

# 运动员运动数据分析
p <- player_tracking_summary %>% 
  ggplot(aes(x= avg_speed)) + 
  geom_density(alpha = 0.5, fill = plot_cols[1], colour = plot_cols[1]) +
  geom_vline(xintercept = mean(player_tracking_summary$avg_speed), linetype = 2, colour = plot_cols[8]) +
  annotate("text", y= 0.4, x= mean(player_tracking_summary$avg_speed) + 1.4, label = paste0("", round(mean(player_tracking_summary$avg_speed), 2)), size=5, colour = plot_cols[8]) +
  labs(x= "平均速度", y= "概率密度") +
  ggtitle("NFL运动员的平均速度稍有偏差", subtitle = "每次比赛的平均速度是每秒1.33码") +
  theme_jason()
# gridExtra::grid.arrange(p)

p <- player_tracking_summary %>% 
  ggplot(aes(x= FieldType, y= sd_speed, fill = FieldType, colour = FieldType)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = plot_cols) +
  scale_colour_manual(values = plot_cols, guide = "none") +
  labs(x= "场地类型", y= "速度") +
  ggtitle("场地对运动员速度的影响", subtitle = "在人造场地中运动员速度明显快于自然场地") +
  coord_flip() +
  theme_jason()

gridExtra::grid.arrange(p)
