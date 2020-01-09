# Tạo video biểu đồ động bằng R
# Nguyên tắc là tạo nhiều biểu đồ theo 1 tiêu chí nào đó, thông thường là thời gian
# Mỗi biểu đồ tại 1 thời điểm là 1 khung hình (Frame)
# Xếp chồng các frame đó lại với nhau ta được 1 video
# https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da
# https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3

# PAckage ggplot2, gganimat
rm(list = ls())
library(gganimate)
library(tidyverse)
library(janitor)
library(scales)
library(gifski)
library(ggflags)

# Lưu dữ liệu tên quốc gia để tạo 
national_name <- read.csv("C:\\Users\\hbcuong\\Desktop\\hoc R\\Make a Animate Chart\\national_name.csv", header = T, stringsAsFactors = F)


# Vẽ biểu đồ thể hiện GDP của các nước qua các năm
dat <- read.csv("C:\\Users\\hbcuong\\Desktop\\hoc R\\Make a Animate Chart\\GDP by Country.csv", stringsAsFactors = F)
gdp <- dat %>% select(3:4, 7:15) 
gdp <- gdp[1:217,]
gdp_tidy <- gdp %>% 
  mutate_at(vars(contains("YR")), as.numeric) %>% 
  gather(year,value, 3:11) %>% 
  janitor::clean_names() %>% # Hàm này để đưa về tên biến chuẩn, ví dụ 'a b' thành a_b
  mutate(year = as.numeric(stringr::str_sub(year,2,5))) %>% 
  left_join(national_name, by = c("country_name" = "Country")) %>% 
  mutate(Abb = str_to_lower(Abb))

# Chỉ lấy top 10 nước có GDP cao nhất mỗi năm
gdp_formatted <- gdp_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1], # Tạo ra 1 giá trị tương đối so với thằng đứng đầu mỗi năm, để biểu đồ cân cối
         Value_lbl = paste0(" ", round(value/1e9))) %>%
  group_by(country_name) %>% 
  filter(rank <=10) %>%
  ungroup()

# Tạo biểu đồ tĩnh
staticplot <-  gdp_formatted %>% 
  ggplot(aes(rank, group = country_name, fill = as.factor(country_name), 
             color = as.factor(country_name))) +
  geom_tile(aes(y = value/2, # Biểu đồ thể hiện mật độ
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  geom_flag(y = -1, aes(country = Abb), size = 50) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

# Tạo animation
anim <- staticplot + 
  transition_states(year, transition_length = 4, state_length = 1) + # Hàm transition_states: biến đổi trạng thái
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")

# Rendering
# Dạng gif
animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))

# Video MP4
for_mp4 <- animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer())
anim_save("C:\\Users\\user\\Desktop\\hoc R\\Make a Animate Chart\\animation.mp4", animation = for_mp4 )