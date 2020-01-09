# Tạo bản đồ bằng gói rworldmap và chèn cờ vào biểu đồ chart bằng gói ggflags
# https://www.r-bloggers.com/all-around-the-world-maps-and-flags-in-r/
# https://github.com/rensa/ggflags
# https://github.com/ellisp/ggflags

rm(list = ls())
library(ggflags)
library(rworldmap)
library(tidyverse)


# Lấy tên quốc gia và cách viết tắt tên của mỗi nước
text <- read.csv("http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry", stringsAsFactors = F)
names(text) <- c("info")
text <- text %>% mutate(id = cumsum(str_detect(info, "%%")))
text <- text %>% filter(!(info %in% c("%%", "Type: language")))

text <- cbind(text, str_split(text$info, pattern = ":", simplify = T) %>% as.data.frame()) %>% 
  select(2, 3, 4) %>% filter(V1 %in% c("Subtag", "Description")) %>% mutate_if(is.factor, as.character) %>% 
  distinct(id, V1, .keep_all = T) %>%  spread(key = V1, value = V2) %>% 
  transmute(Country = str_trim(Description, side = "both"), Abb = str_trim(Subtag, side = "both"))
write.csv(text, "C:\\Users\\hbcuong\\Desktop\\hoc R\\Make a Animate Chart\\national_name.csv", row.names = F)


# Lấy dữ liệu football chứa tên quốc gia
file_name <- "C:\\Users\\hbcuong\\Desktop\\hoc R\\Make a Animate Chart\\dat_flag.csv"
df <- read.csv(file_name, header = TRUE, stringsAsFactors = FALSE)

countries_lab <- as.data.frame(table(df$Origin))
colnames(countries_lab) <- c("country", "value")
matched <- rworldmap::joinCountryData2Map(countries_lab, joinCode="ISO2", nameJoinColumn="country")

# make png of the map
png(file = "labWorldMap.png", width = 1024, height = 768)
par(mai=c(0,0,0.2,0))
rworldmap::mapCountryData(matched, nameColumnToPlot="value", mapTitle= "",
               catMethod = "logFixedWidth", colourPalette = "heat",
               oceanCol="lightblue", missingCountryCol="white",
               addLegend = FALSE, lwd = 1)
dev.off()


# make bar chart of lab members
countries_lab %>%
  mutate(code = tolower(country)) %>% # hàm ggflags match dưới dạng chữ thường
  ggplot(aes(x = reorder(country, value), y = value)) +
  geom_bar(stat = "identity") +
  ggflags::geom_flag(y = countries_lab$value + 2, aes(country = code), size = 4) +  ## Quan trọng là hàm geom_flags trong ggflags
  
  # Có thể điều chỉnh vị trí của cờ với đối số y, ở đây y = -1, y = ...
  scale_y_continuous(expand = c(0.1, 1)) +
  xlab("Country") +
  ylab("Members") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  coord_flip()
ggsave("plot.png", plot = last_plot())


# hàm scale_country khá hay
