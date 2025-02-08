# 拆分整理数据
path <- "C:\\Users\\18752\\Desktop\\GBD\\cry-12-1-1\\cry-12-1-1\\多处骨折，脱位，坠伤，伤口，韧带损伤和肌肉拉伤\\数据"
files <- "C:\\Users\\18752\\Desktop\\GBD\\cry-12-1-1\\cry-12-1-1\\多处骨折，脱位，坠伤，伤口，韧带损伤和肌肉拉伤\\结果"
setwd(path)
# 设置新文件夹的路径 -- 整理好数据存放地址
folder_path <- path
# 设置要解压的文件夹路径  -- 下载的压缩包所在位置
zip_folder <- paste0(path) 
# 设置解压后的目标文件夹  -- 文件解压到哪里
output_folder <- path 
# 获取所有 ZIP 文件的路径
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = TRUE)
# 批量解压
for (zip_file in zip_files) {
  unzip(zip_file, exdir = output_folder)
}

# 加载必要的库
library(data.table)
# 获取所有 CSV 文件的路径
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
# 读取并拼接所有 CSV 文件
all_data <- rbindlist(lapply(file_list, fread))
all_data[] <- lapply(all_data, function(x) {
  if (is.character(x)) {
    gsub(",", "_", x)
  } else {
    x
  }
})

head(all_data)
fwrite(all_data, "alldata.csv")
sex <- all_data[location_name == 'Global' &
                  age_name %in% c('All ages', 'Age-standardized')]
fwrite(sex, "sex.csv")
age <- all_data[location_name == 'Global' &
                  sex_name == 'Both' &
                  !age_name %in% c('All ages', 'Age-standardized')]
fwrite(age, "age.csv")
global <- all_data[location_name == 'Global' &
                     sex_name == 'Both' &
                     age_name %in% c('All ages', 'Age-standardized')]
fwrite(global, "global.csv")
sdi <- all_data[location_name %in% c('High SDI','High-middle SDI','Middle SDI','Low-middle SDI','Low SDI') &
                  sex_name == 'Both' &
                  age_name %in% c('All ages', 'Age-standardized')]
fwrite(sdi, "SDI.csv")

places <- data.table::fread(file="E:\\RStudio\\0\\country.csv",encoding="UTF-8")
country <- unique(places$location_name)
country <- all_data[location_name %in% country &
                      sex_name == 'Both' &
                      age_name %in% c('All ages', 'Age-standardized')]
fwrite(country, "country.csv")
region <- unique(places$region_name)
region <- all_data[location_name %in% region &
                      sex_name == 'Both' &
                      age_name %in% c('All ages', 'Age-standardized')]
fwrite(region, "region.csv")

Global <- all_data[location_name == 'Global']
fwrite(Global, "Globals.csv")

#下载包: 
# install.packages('gbd')  BiocManager::install('gbd')  devtools::install_github('gbd')
# install.packages("ggplot2", dependencies = TRUE, type = "binary")
# install.packages("ggsci", dependencies = TRUE, type = "binary")
# install.packages("cowplot", dependencies = TRUE, type = "binary")
#加载包
library(ggplot2)
library(ggsci)
library(cowplot)
library(dplyr)
setwd(path)
library(RColorBrewer)
options(scipen = 999)

duiying = read.csv("E:\\RStudio\\0\\对应关系.csv")
country<-data.table::fread(file="country.csv",encoding="UTF-8")
country = merge(country, duiying,by.x = 'location_name',by.y = 'new',all.x = T)
country$location_name <- ifelse(!is.na(country$old), country$old, country$location_name)
write.csv(country,'country.csv',quote = FALSE)


library(ggpubr)
#sex亚组分析
#读取数据
Sex<-data.table::fread(file="sex.csv",encoding="UTF-8")
Sex$measure_name <- factor(Sex$measure_name, levels=sort(unique(Sex$measure_name)))
Sex$sex_name <- factor(Sex$sex_name, levels=sort(unique(Sex$sex_name)))

# 按照条件筛选后获取指定列
A1 <- Sex[age_name == 'Age-standardized' &
            metric_name == 'Rate' &
            year=='2021' ,
          .(sex_name,measure_name,val,upper,lower)]
A1_plot <- ggplot(A1, aes(x = sex_name, y = val, fill = sex_name)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin = lower), position = position_dodge(0.9), width = 0.5) +
  ylab("Age-standardized rate") + xlab("Sex") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major = element_line(colour = "grey", size = 0.5)) +
  facet_wrap(vars(measure_name), nrow = 2, scales = "free_y", strip.position = "top") +
  theme(strip.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # 添加 x 轴标签
  scale_fill_brewer(palette = "Set2")
A1_plot
file_path <- paste0(files, "\\21sex-ASR.png")
ggsave(file_path, plot = A1_plot, height = 8, width = 10)

B1 <- Sex[age_name == 'All ages' &
            metric_name == 'Number' &
            year=='2021' ,
          .(sex_name,measure_name,val,upper,lower)]
B1_plot <- ggplot(B1, aes(x = sex_name, y = val, fill = sex_name)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin = lower), position = position_dodge(0.9), width = 0.5) +
  ylab("Number of cases") + xlab("Sex") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major = element_line(colour = "grey", size = 0.5)) +
  facet_wrap(vars(measure_name), nrow = 2, scales = "free_y", strip.position = "top") +
  theme(strip.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # 添加 x 轴标签
  scale_fill_brewer(palette = "Set2")
B1_plot
file_path <- paste0(files, "\\21sex-number.png")
ggsave(file_path, plot = B1_plot, height = 8, width = 10)

combined_plot <- ggarrange(A1_plot, B1_plot, ncol = 2, common.legend = FALSE)
combined_plot
# 保存组合后的图形到文件
file_path <- paste0(files, "\\21combined_sex_plots.png")
ggsave(file_path, plot = combined_plot, height = 8, width = 20)


#age 亚组分析
AGE<-data.table::fread(file="age.csv",encoding="UTF-8")
AGE$measure_name <- factor(AGE$measure_name, levels=sort(unique(AGE$measure_name)))
AGE$age_name <- factor(AGE$age_name, levels=c('0-6 days','7-27 days','1-5 months','6-11 months','12-23 months','2-4 years',
                                              '<5 years', '5-9 years','10-14 years','15-19 years','20-24 years','25-29 years',
                                              '30-34 years','35-39 years','40-44 years','45-49 years','50-54 years','55-59 years','60-64 years',
                                              '65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90-94 years','95+ years'))

A2 <- AGE[sex_name == 'Both' &
            metric_name == 'Rate' &
            year=='2021' ,
          .(age_name,measure_name, val,upper,lower)]
A2 <-ggplot(A2,aes(x=age_name, y=val,fill=age_name)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin=lower),position=position_dodge(.9),width = 0.5)+
  ylab(label="Age-standardized rate") + xlab(label="Age")+
  theme_classic()+
  theme( legend.position =" none ",
         panel.grid.major = element_line(colour = "grey", size = 0.5))+
  theme(axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))#子图标签细节控制
A2
file_path <- paste0(files, "\\21age-ASR.png")
ggsave(file_path, plot = A2, height = 8, width = 10)

B2 <- AGE[sex_name == 'Both' &
            metric_name == 'Number' &
            year=='2021' ,
          .(age_name,measure_name, val,upper,lower)]
B2 <-ggplot(B2,aes(x=age_name, y=val,fill=age_name)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin=lower),position=position_dodge(.9),width = 0.5)+
  ylab(label="Number of cases") + xlab(label="Age")+
  theme_classic()+
  theme( legend.position =" none ",
         panel.grid.major = element_line(colour = "grey", size = 0.5))+
  theme(axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))#子图标签细节控制
B2
file_path <- paste0(files, "\\21age-number.png")
ggsave(file_path, plot = B2, height = 8, width = 10)

combined_plot <- ggarrange(A2, B2, ncol = 2, common.legend = FALSE)
combined_plot
# 保存组合后的图形到文件
file_path <- paste0(files, "\\21combined_age_plots.png")
ggsave(file_path, plot = combined_plot, height = 8, width = 20)


#SDI
SDI<-data.table::fread(file="SDI.csv",encoding="UTF-8")
SDI$measure_name <- factor(SDI$measure_name, levels=sort(unique(SDI$measure_name)))
SDI$location_name <- factor(SDI$location_name, levels=c('High SDI','High-middle SDI','Middle SDI','Low-middle SDI','Low SDI'))

A3 <- SDI[age_name == 'Age-standardized' &
            sex_name == 'Both' &
            metric_name == 'Rate' &
            year=='2021' ,
          .(location_name,measure_name, val,upper,lower)]

A3 <-ggplot(A3,aes(x=location_name, y=val,fill=location_name)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin=lower),position=position_dodge(.9),width = 0.5)+
  ylab(label="Age-standardized rate") + xlab(label="SDI") +
  theme_classic()+
  theme( legend.position =" none ",
         panel.grid.major = element_line(colour = "grey", size = 0.5)) + 
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",            # 控制y轴自由缩放,"free","free_y","free_x"
             strip.position = "top")+      # 控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+#子图标签细节控制
  scale_fill_brewer(palette = "Set2")
A3
file_path <- paste0(files, "\\21SDI-ASR.png")
ggsave(file_path, plot = A3, height = 8, width = 10)

B3 <- SDI[age_name == 'All ages' &
            sex_name == 'Both' &
            metric_name == 'Number' &
            year=='2021' ,
          .(location_name,measure_name, val,upper,lower)]
B3 <-ggplot(B3,aes(x=location_name, y=val,fill=location_name)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin=lower),position=position_dodge(.9),width = 0.5)+
  ylab(label="Number of cases") + xlab(label="SDI")+
  theme_classic()+
  theme( legend.position =" none ",
         panel.grid.major = element_line(colour = "grey", size = 0.5))+ 
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",
             strip.position = "top")+
  theme(strip.text = element_text(size = 10))+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+#子图标签细节控制
  scale_fill_brewer(palette = "Set2")
B3
file_path <- paste0(files, "\\21SDI-number.png")
ggsave(file_path, plot = B3, height = 8, width = 10)

combined_plot <- ggarrange(A3, B3, ncol = 2, common.legend = FALSE)
combined_plot
# 保存组合后的图形到文件
file_path <- paste0(files, "\\21combined_SDI_plots.png")
ggsave(file_path, plot = combined_plot, height = 8, width = 20)



#GBD region亚组分析
GBD<-data.table::fread(file="region.csv",encoding="UTF-8")
GBD$measure_name <- factor(GBD$measure_name, levels=unique(GBD$measure_name))

A4 <- GBD[age_name == 'Age-standardized' &
            sex_name == 'Both' &
            metric_name == 'Rate' &
            year=='2021' ,
          .(location_name,measure_name, val,upper,lower)]

A4 <-ggplot(A4,aes(x=location_name, y=val,fill=location_name)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin=lower),position=position_dodge(.9),width = 0.5)+
  ylab(label="Age-standardized rate") + xlab(label="GBD region") +
  theme_classic()+
  theme( legend.position =" none ",
         panel.grid.major = element_line(colour = "grey", size = 0.5)) + 
  theme(axis.text.x = element_text(size = 9, angle = 60, hjust = 1))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",
             strip.position = "top")+
  theme(strip.text = element_text(size = 10))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))#子图标签细节控制
A4
file_path <- paste0(files, "\\21region-ASR.png")
ggsave(file_path, plot = A4, height = 8, width = 15)

B4 <- GBD[age_name == 'All ages' &
            sex_name == 'Both' &
            metric_name == 'Number' &
            year=='2021' ,
          .(location_name,measure_name, val,upper,lower)]
B4 <-ggplot(B4,aes(x=location_name, y=val,fill=location_name)) +
  geom_bar(stat = "identity",position = position_dodge(0.9),width = 0.5) +
  geom_errorbar(aes(ymax = upper, ymin=lower),position=position_dodge(.9),width = 0.5)+
  ylab(label="Number of cases") + xlab(label="GBD region")+
  theme_classic()+
  theme( legend.position =" none ",
         panel.grid.major = element_line(colour = "grey", size = 0.5))+ 
  theme(axis.text.x = element_text(size = 9, angle = 60, hjust = 1))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",
             strip.position = "top")+
  theme(strip.text = element_text(size = 10))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))#子图标签细节控制
B4
file_path <- paste0(files, "\\21region-number.png")
ggsave(file_path, plot = B4, height = 8, width = 15)

combined_plot <- ggarrange(A4, B4, ncol = 2, common.legend = FALSE)
combined_plot
# 保存组合后的图形到文件
file_path <- paste0(files, "\\21combined_region_plots.png")
ggsave(file_path, plot = combined_plot, height = 10, width = 30)



#country亚组分析
#install.packages('ggmap', dependencies = TRUE, type = "binary")
#install.packages('rgdal', dependencies = TRUE, type = "binary")
#BiocManager::install('rgdal')
#install.packages('maps', dependencies = TRUE, type = "binary")
#install.packages('dplyr', dependencies = TRUE, type = "binary")
#install.packages('data.table', dependencies = TRUE, type = "binary")
library(ggmap)
#library(rgdal)
library(maps)
library(dplyr)
library(data.table)

#读取数据
AR<-data.table::fread(file="country.csv",encoding="UTF-8")
measures = sort(unique(AR$measure_name))

for (measure in measures) {
  #Incidence rate
  ASDR_2021 <- subset(AR, year==2021 &
                        age_name=='Age-standardized' &
                        metric_name== 'Rate' &
                        measure_name==measure) ## 获取2021年AR年龄校正后发病率
  ASDR_2021 <- data.frame(location = ASDR_2021$location_name,
                          val = ASDR_2021$val,
                          upper = ASDR_2021$upper,
                          lower = ASDR_2021$lower)
  ASDR_2021$val <- round(ASDR_2021$val,1) ###保留一位小数点
  ASDR_2021$lower <- round(ASDR_2021$lower,1) ###保留一位小数点
  ASDR_2021$upper <- round(ASDR_2021$upper,1) ###保留一位小数点

  ####  map for ASR
  worldData <- map_data('world')
  country_asr <- ASDR_2021
  country_asr$location <- as.character(country_asr$location)
  ###以下代码的目的是让country_asr$location的国家名称与worldData的国家名称一致
  ### 这样才能让数据映射到地图上
  country_asr$location[country_asr$location == 'United States of America'] = 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
  country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
  country_asr$location[country_asr$location == "North Macedonia"] = 'North Macedonia'
  country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
  country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
  country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
  country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
  country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
  country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
  country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
  country_asr$location[country_asr$location == "C么te d'Ivoire"] = 'Ivory Coast'
  country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
  country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
  country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
  worldData <- map_data('world')
  total <- full_join(worldData,country_asr,by = c('region'='location'))
  p <- ggplot()
  total <- total %>% mutate(val2 = cut(val,
                                       breaks = pretty(val, n = 8), # 减少到5个分组
                                       include.lowest = TRUE,
                                       right = TRUE))
  A4 <- p + geom_polygon(data = total,
                         aes(x = long, y = lat, group = group, fill = val2),
                         colour = "black", size = 0.2) +
    # scale_fill_brewer(palette = "set1") +
    theme_map() +
    labs(title = paste0("    Age-standardized ", measure, " rate")) +
    theme(
      legend.position = 'right',
      legend.title = element_blank(),
      legend.margin = margin(10, 10, 10, 10),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA)   # 设置绘图背景为白色
    )
  A4
  file_path <- file.path(files, paste0('21country_rate_',measure,'.png'))
  ggsave(file_path, plot = A4, height = 10, width = 20)
}


for (measure in measures) {
  #Incidence rate
  ASDR_2021 <- subset(AR, year==2021 &
                        age_name=='All ages' &
                        metric_name== 'Number' &
                        measure_name==measure) ## 获取2021年AR年龄校正后发病率
  ASDR_2021 <- data.frame(location = ASDR_2021$location_name,
                          val = ASDR_2021$val,
                          upper = ASDR_2021$upper,
                          lower = ASDR_2021$lower)
  ASDR_2021$val <- round(ASDR_2021$val,1) ###保留一位小数点
  ASDR_2021$lower <- round(ASDR_2021$lower,1) ###保留一位小数点
  ASDR_2021$upper <- round(ASDR_2021$upper,1) ###保留一位小数点

  ####  map for ASR
  worldData <- map_data('world')
  country_asr <- ASDR_2021
  country_asr$location <- as.character(country_asr$location)
  ###以下代码的目的是让country_asr$location的国家名称与worldData的国家名称一致
  ### 这样才能让数据映射到地图上
  country_asr$location[country_asr$location == 'United States of America'] = 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
  country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
  country_asr$location[country_asr$location == "North Macedonia"] = 'North Macedonia'
  country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
  country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
  country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
  country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
  country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
  country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
  country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
  country_asr$location[country_asr$location == "C么te d'Ivoire"] = 'Ivory Coast'
  country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
  country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
  country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
  worldData <- map_data('world')
  total <- full_join(worldData,country_asr,by = c('region'='location'))
  p <- ggplot()
  total <- total %>% mutate(val2 = cut(val,
                                       breaks = pretty(val, n = 10), # 自动生成间隔
                                       include.lowest = TRUE,
                                       right = TRUE))
  A4 <- p + geom_polygon(data = total,
                         aes(x = long, y = lat, group = group, fill = val2),
                         colour = "black", size = 0.2) +
    scale_fill_brewer(palette = "Set1") +
    theme_map() +
    labs(title = paste0("    Number of ", measure, " cases")) +
    theme(legend.position = 'right',
          legend.title = element_blank(),
          legend.margin = margin(10, 10, 10, 10),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA))   # 设置绘图背景为白色
  A4
  file_path <- file.path(files, paste0('21country_number_',measure,'.png'))
  ggsave(file_path, plot = A4, height = 10, width = 20)
}


####第一步 不同年龄段患病人数金字塔####
#####分组年龄，一般为5岁一个年龄组
age1 <- c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years",
          "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
          "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
          "75-79 years","80-84","85-89","90-94","95+ years")   ###20个年龄组

#提取患病人数的数据
measures <- unique(Global$measure_name)
for (measure in measures) {
  data1<- subset(Global,Global$year==2021& #提取2021年
                   (Global$age_name %in% age1 ) & #提取不同年龄段
                   Global$sex_name!="Both"& #把男女分开
                   Global$metric_name== 'Number' & #提取10人数
                   Global$measure_name==measure) #提取患病率数据
  
  #提取分析的数据
  data1<-data1[,c("sex_name","age_name","val","upper","lower")]
  #年龄数据的处理，替代year 文字
  data1$age_name<-gsub(" years","",data1$age_name)
  #因子化,按年龄排序
  data1$age_name <- factor(data1$age_name, levels = c("<5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  # 按照 sex_name 和 age_name 进行排序
  data1 <- data1[order(data1$sex_name, data1$age_name),]
  #提取整数
  data1$val<-round(data1$val,0)
  data1$Sex<-as.factor(data1$sex_name)
  #设置颜色
  custom_colors <- c("Male" = "steelblue", "Female" = "#e31a1c")
  p1<-ggplot(data1, aes(x = factor(age_name,levels = unique(age_name)),
                        y = ifelse(Sex == "Male", val, -val), 
                        fill = Sex)) +
    scale_fill_manual(values = custom_colors) +
    geom_bar(stat = 'identity')+  
    coord_flip()+
    labs(x = 'Age', y = paste0('The Numbers of ',measure))+  
    geom_text( 
      aes(label=val,                                   # 显示数值
          hjust = ifelse(Sex == "Male", -0.4, 1.1)    # 数值的位置
      ),
      size=5)+   ####字体大小
    scale_y_continuous(                                 
      labels = abs,                                           
      expand = expansion(mult = c(0.2, 0.2)) )                 
  
  # 年龄段折线图
  data2<- subset(Global,Global$year==2021& #提取2021年
                   (Global$age_name %in% age1 ) & #提取不同年龄段
                   Global$sex_name!="Both"& #把男女分开
                   Global$metric_name== 'Rate' & #提取10人患病人数
                   Global$measure_name==measure) #提取患病率数据
  #提取分析的数据
  data2<-data2[,c("sex_name","age_name","val","upper","lower")]
  #年龄数据的处理，替代year 文字
  data2$age_name<-gsub(" years","",data2$age_name)
  
  #因子化,按年龄排序
  data2$age_name <- factor(data2$age_name, levels = c("<5", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  # 按照 sex_name 和 age_name 进行排序
  data2 <- data2[order(data2$sex_name, data2$age_name),]
  #取小2位数点
  data2$val<-round(data2$val,2)
  #性别进行因子化
  data2$Sex<-as.factor(data2$sex_name)
  p2 <- ggplot(data = data2,aes(x=data2$age_name,y=data2$val,color=Sex, group = Sex))+
    geom_line()+
    geom_point(size=1)+
    labs(x = 'Age', y = paste0('The Rate of ',measure))+  
    scale_fill_manual(values=c("steelblue","#e31a1c"))+
    scale_color_manual(values=c("steelblue","#e31a1c"))+
    geom_ribbon(aes(ymin=data2$lower,ymax=data2$upper,fill=Sex),alpha=0.1,color=NA)
  
  combined_plot <- ggarrange(p1,p2,ncol = 2, common.legend = FALSE)
  ggsave(paste0(files,'\\21sex_age_',measure,'.png'), plot = combined_plot, width = 20, height = 8)
}

# 绘制year与sex双轴图
for (measure in measures) {
  #提取所需要的数据
  data1<- subset(Global,
                 Global$sex_name!="Both"& #提取所有性别
                   (Global$metric_name %in% c('Number','Rate') ) & #提取人数及10万人患病人数
                   Global$measure_name ==measure) #提取患病率
  #筛选所需要的数据
  data2 <- data1[,c("sex_name","age_name","metric_name","year","val","upper","lower")] 
  # 绘制图表
  p1 <- ggplot() +
    geom_bar(data = subset(data2, age_name =="All ages"), 
             mapping=aes(x = year, y = val, fill = sex_name), 
             stat = "identity", 
             position = position_dodge(width = 0.8), 
             width = 0.7)+
    geom_errorbar(data = subset(data2, age_name =="All ages"),  
                  mapping=aes(x = year, ymin = lower, ymax = upper, 
                              group =sex_name ), 
                  position = position_dodge(width = 0.8), 
                  width = 0.25, 
                  color = "black")+
    geom_line(data = subset(data2, age_name =="Age-standardized"),
              aes(x = year, y = val*10000, color = sex_name),
              size = 1) +
    geom_ribbon(data = subset(data2, age_name =="Age-standardized"),
                aes(x = year, ymin = lower*10000, ymax = upper*10000,
                    fill = sex_name),
                alpha = 0.2) +
    scale_y_continuous(
      name = paste0(measure," Number"),
      # limits = c(0, 40000),
      sec.axis = sec_axis(~ . * 0.0001, name = paste0("Age-standardized ",measure," rate per 100,000"))
    ) +
    scale_fill_manual(values = c("#ffaa00", "#1240ab","blue", "red"), name = "Number") +
    scale_color_manual(values = c("#ffaa00", "#1240ab"), name = "Rate") +
    labs(x = "Year") 
  ggsave(paste0(files,'\\21sex_year_',measure,'.png'), plot = p1, width = 10, height = 8)
}



#趋势性分析

#表格
#1990-2021
library(dplyr)         ## 读取需要的R包
library(flextable)
library(officer)

EC <- read.csv('SDI.csv',header = T)  ## 读取我们的数据
measures = sort(unique(EC$measure_name))

for (measure in measures) {
  ## 1990发病人数
  EC_1990 <- EC %>%
    filter(year == 1990,
           age_name == 'All ages',
           metric_name == 'Number',
           sex_name == 'Both',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_1990$val <- round(EC_1990$val,0)  ###取整
  EC_1990$lower <- round(EC_1990$lower,0)###取整
  EC_1990$upper <- round(EC_1990$upper,0) ###取整
  EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021发病人数
  EC_2021 <- EC %>%
    filter(year == 2021,
           age_name == 'All ages',
           metric_name == 'Number',
           sex_name == 'Both',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_2021$val <- round(EC_2021$val,0)  ###取整
  EC_2021$lower <- round(EC_2021$lower,0)###取整
  EC_2021$upper <- round(EC_2021$upper,0) ###取整
  EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

  ## 1990 ASR
  ASR_1990 <- EC %>%
    filter(year == 1990,
           age_name == 'Age-standardized',
           metric_name == 'Rate',
           sex_name == 'Both',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_1990$val <- round(ASR_1990$val,2)  ###取整
  ASR_1990$lower <- round(ASR_1990$lower,2)###取整
  ASR_1990$upper <- round(ASR_1990$upper,2) ###取整
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021 ASR
  ASR_2021 <- EC %>%
    filter(year == 2021,
           age_name == 'Age-standardized',
           metric_name == 'Rate',
           sex_name == 'Both',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_2021$val <- round(ASR_2021$val,2)  ###取整
  ASR_2021$lower <- round(ASR_2021$lower,2)###取整
  ASR_2021$upper <- round(ASR_2021$upper,2) ###取整
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接
  
  ##### EAPC
  EAPC <- EC %>%
    filter(age_name=='Age-standardized',
           metric_name== 'Rate',
           sex_name == 'Both',
           measure_name== measure) %>%
    select(location_name, year, val)

  country <- unique(EC_1990$location_name)
  EAPC_cal <- data.frame(location_name=country,
                         EAPC=rep(0,times=length(country)),
                         UCI=rep(0,times=length(country)),
                         LCI=rep(0,times=length(country)))
  for (i in 1:length(country)){  ###总共23个地区，所以循环23次
    country_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
    a <- subset(EAPC, EAPC$location_name==country_cal)  ##取对应地区的数据子集
    a$y <- log(a$val)  ##根据EAPC计算方法计算y值
    mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的上限值
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的下限值
    EAPC_cal[i,2] <- estimate
    EAPC_cal[i,4] <- low
    EAPC_cal[i,3] <- high
  }

  EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
  EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
  EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '')
  EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')

  ### 数据整合
  EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
  ASR_1990 <- ASR_1990[,c(1,5)]
  EC_2021 <- EC_2021[,c(1,5)]
  ASR_2021 <- ASR_2021[,c(1,5)]
  EAPC_cal <- EAPC_cal[,c(1,5)]
  Incidence <- merge(EC_1990,ASR_1990,by='location_name')
  Incidence <- merge(Incidence,EC_2021,by='location_name')
  Incidence <- merge(Incidence,ASR_2021,by='location_name')
  Incidence <- merge(Incidence,EAPC_cal,by='location_name')
  SDIIncidence<-Incidence
  dir_path <- file.path(files, paste0("eapc\\",measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\SDI_eapc.csv')
  write.csv(SDIIncidence, file_path ,quote = FALSE)
}


EC <- read.csv('sex.csv',header = T)  ## 读取我们的数据

for (measure in measures) {
  ## 1990发病人数
  EC_1990 <- EC %>%
    filter(year == 1990,
           age_name == 'All ages',
           metric_name == 'Number',
           measure_name == measure) %>%
    select(sex_name, val, upper, lower)  # 只取需要的变量
  EC_1990$val <- round(EC_1990$val,0)  ###取整
  EC_1990$lower <- round(EC_1990$lower,0)###取整
  EC_1990$upper <- round(EC_1990$upper,0) ###取整
  EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021发病人数
  EC_2021 <- EC %>%
    filter(year == 2021,
           age_name == 'All ages',
           metric_name == 'Number',
           measure_name == measure) %>%
    select(sex_name, val, upper, lower)  # 只取需要的变量
  EC_2021$val <- round(EC_2021$val,0)  ###取整
  EC_2021$lower <- round(EC_2021$lower,0)###取整
  EC_2021$upper <- round(EC_2021$upper,0) ###取整
  EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

  ## 1990 ASR
  ASR_1990 <- EC %>%
    filter(year == 1990,
           age_name == 'Age-standardized',
           metric_name == 'Rate',
           measure_name == measure) %>%
    select(sex_name, val, upper, lower)  # 只取需要的变量
  ASR_1990$val <- round(ASR_1990$val,2)  ###取整
  ASR_1990$lower <- round(ASR_1990$lower,2)###取整
  ASR_1990$upper <- round(ASR_1990$upper,2) ###取整
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021 ASR
  ASR_2021 <- EC %>%
    filter(year == 2021,
           age_name == 'Age-standardized',
           metric_name == 'Rate',
           measure_name == measure) %>%
    select(sex_name, val, upper, lower)  # 只取需要的变量
  ASR_2021$val <- round(ASR_2021$val,2)  ###取整
  ASR_2021$lower <- round(ASR_2021$lower,2)###取整
  ASR_2021$upper <- round(ASR_2021$upper,2) ###取整
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接
  
  ##### EAPC
  EAPC <- EC %>%
    filter(age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name== measure) %>%
    select(sex_name, year, val)

  country <- EC_1990$sex_name
  EAPC_cal <- data.frame(sex_name=country,
                         EAPC=rep(0,times=length(country)),
                         UCI=rep(0,times=length(country)),
                         LCI=rep(0,times=length(country)))
  for (i in 1:length(country)){  ###总共23个地区，所以循环23次
    country_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
    a <- subset(EAPC, EAPC$sex_name==country_cal)  ##取对应地区的数据子集
    a$y <- log(a$val)  ##根据EAPC计算方法计算y值
    mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的上限值
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的下限值
    EAPC_cal[i,2] <- estimate
    EAPC_cal[i,4] <- low
    EAPC_cal[i,3] <- high
  }

  EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
  EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
  EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '')
  EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')

  ### 数据整合
  EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
  ASR_1990 <- ASR_1990[,c(1,5)]
  EC_2021 <- EC_2021[,c(1,5)]
  ASR_2021 <- ASR_2021[,c(1,5)]
  EAPC_cal <- EAPC_cal[,c(1,5)]
  Incidence <- merge(EC_1990,ASR_1990,by='sex_name')
  Incidence <- merge(Incidence,EC_2021,by='sex_name')
  Incidence <- merge(Incidence,ASR_2021,by='sex_name')
  Incidence <- merge(Incidence,EAPC_cal,by='sex_name')
  SexIncidence<-Incidence
  dir_path <- file.path(files, paste0("eapc\\",measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\sex_eapc.csv')
  write.csv(SexIncidence, file_path ,quote = FALSE)
}



EC <- read.csv('age.csv',header = T)  ## 读取我们的数据

for (measure in measures) {
  ## 1990发病人数
  EC_1990 <- EC %>%
    filter(year==1990,
           metric_name== 'Number',
           measure_name == measure) %>%
    select(age_name, val, upper, lower)  # 只取需要的变量
  EC_1990$val <- round(EC_1990$val,0)  ###取整
  EC_1990$lower <- round(EC_1990$lower,0)###取整
  EC_1990$upper <- round(EC_1990$upper,0) ###取整
  EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021发病人数
  EC_2021 <- EC %>%
    filter(year == 2021,
           metric_name == 'Number',
           measure_name == measure) %>%
    select(age_name, val, upper, lower)  # 只取需要的变量
  EC_2021$val <- round(EC_2021$val,0)  ###取整
  EC_2021$lower <- round(EC_2021$lower,0)###取整
  EC_2021$upper <- round(EC_2021$upper,0) ###取整
  EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

  ## 1990 ASR
  ASR_1990 <- EC %>%
    filter(year == 1990,
           metric_name == 'Rate',
           measure_name == measure) %>%
    select(age_name, val, upper, lower)  # 只取需要的变量
  ASR_1990$val <- round(ASR_1990$val,2)  ###取整
  ASR_1990$lower <- round(ASR_1990$lower,2)###取整
  ASR_1990$upper <- round(ASR_1990$upper,2) ###取整
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021 ASR
  ASR_2021 <- EC %>%
    filter(year == 2021,
           metric_name == 'Rate',
           measure_name == measure) %>%
    select(age_name, val, upper, lower)  # 只取需要的变量
  ASR_2021$val <- round(ASR_2021$val,2)  ###取整
  ASR_2021$lower <- round(ASR_2021$lower,2)###取整
  ASR_2021$upper <- round(ASR_2021$upper,2) ###取整
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接
  
  ##### EAPC
  EAPC <- EC %>%
    filter(metric_name== 'Rate',
           measure_name== measure) %>%
    select(age_name, year, val)
  EAPC = EAPC[!EAPC$val == 0,]
  country <- unique(EAPC$age_name)
  EAPC_cal <- data.frame(age_name=country,
                         EAPC=rep(0,times=length(country)),
                         UCI=rep(0,times=length(country)),
                         LCI=rep(0,times=length(country)))
  for (i in 1:length(country)){  ###总共23个地区，所以循环23次
    country_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
    a <- subset(EAPC, EAPC$age_name==country_cal)  ##取对应地区的数据子集
    a$y <- log(a$val)  ##根据EAPC计算方法计算y值
    mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的上限值
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的下限值
    EAPC_cal[i,2] <- estimate
    EAPC_cal[i,4] <- low
    EAPC_cal[i,3] <- high
  }

  EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
  EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
  EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '')
  EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')

  ### 数据整合
  EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
  ASR_1990 <- ASR_1990[,c(1,5)]
  EC_2021 <- EC_2021[,c(1,5)]
  ASR_2021 <- ASR_2021[,c(1,5)]
  EAPC_cal <- EAPC_cal[,c(1,5)]
  Incidence <- merge(EC_1990,ASR_1990,by='age_name')
  Incidence <- merge(Incidence,EC_2021,by='age_name')
  Incidence <- merge(Incidence,ASR_2021,by='age_name')
  Incidence <- merge(Incidence,EAPC_cal,by='age_name')
  AgeIncidence<-Incidence
  dir_path <- file.path(files, paste0("eapc\\",measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\age_eapc.csv')
  write.csv(AgeIncidence, file_path ,quote = FALSE)
}


#Global
EC <- read.csv('global.csv',header = T)  ## 读取我们的数据

for (measure in measures) {
  ## 1990发病人数
  EC_1990 <- EC %>%
    filter(year==1990,
           age_name=='All ages',
           metric_name== 'Number',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_1990$val <- round(EC_1990$val,0)  ###取整
  EC_1990$lower <- round(EC_1990$lower,0)###取整
  EC_1990$upper <- round(EC_1990$upper,0) ###取整
  EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021发病人数
  EC_2021 <- EC %>%
    filter(year == 2021,
           age_name=='All ages',
           metric_name== 'Number',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_2021$val <- round(EC_2021$val,0)  ###取整
  EC_2021$lower <- round(EC_2021$lower,0)###取整
  EC_2021$upper <- round(EC_2021$upper,0) ###取整
  EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

  ## 1990 ASR
  ASR_1990 <- EC %>%
    filter(year == 1990,
           age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_1990$val <- round(ASR_1990$val,2)  ###取整
  ASR_1990$lower <- round(ASR_1990$lower,2)###取整
  ASR_1990$upper <- round(ASR_1990$upper,2) ###取整
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021 ASR
  ASR_2021 <- EC %>%
    filter(year == 2021,
           age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_2021$val <- round(ASR_2021$val,2)  ###取整
  ASR_2021$lower <- round(ASR_2021$lower,2)###取整
  ASR_2021$upper <- round(ASR_2021$upper,2) ###取整
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接
  
  ##### EAPC
  EAPC <- EC %>%
    filter(metric_name== 'Rate',
           measure_name== measure) %>%
    select(location_name, year, val)

  country <- unique(EC_1990$location_name)
  EAPC_cal <- data.frame(location_name=country,
                         EAPC=rep(0,times=length(country)),
                         UCI=rep(0,times=length(country)),
                         LCI=rep(0,times=length(country)))
  for (i in 1:length(country)){  ###总共23个地区，所以循环23次
    country_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
    a <- subset(EAPC, EAPC$location_name==country_cal)  ##取对应地区的数据子集
    a$y <- log(a$val)  ##根据EAPC计算方法计算y值
    mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的上限值
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的下限值
    EAPC_cal[i,2] <- estimate
    EAPC_cal[i,4] <- low
    EAPC_cal[i,3] <- high
  }

  EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
  EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
  EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '')
  EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')

  ### 数据整合
  EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
  ASR_1990 <- ASR_1990[,c(1,5)]
  EC_2021 <- EC_2021[,c(1,5)]
  ASR_2021 <- ASR_2021[,c(1,5)]
  EAPC_cal <- EAPC_cal[,c(1,5)]
  Incidence <- merge(EC_1990,ASR_1990,by='location_name')
  Incidence <- merge(Incidence,EC_2021,by='location_name')
  Incidence <- merge(Incidence,ASR_2021,by='location_name')
  Incidence <- merge(Incidence,EAPC_cal,by='location_name')
  globalIncidence<-Incidence
  dir_path <- file.path(files, paste0("eapc\\",measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\global_eapc.csv')
  write.csv(globalIncidence, file_path,quote = FALSE)
}



#GBD region
EC <- read.csv('region.csv',header = T)  ## 读取我们的数据
measures = unique(EC$measure_name)
for (measure in measures) {
  ## 1990发病人数
  EC_1990 <- EC %>%
    filter(year==1990,
           age_name=='All ages',
           metric_name== 'Number',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_1990$val <- round(EC_1990$val,0)  ###取整
  EC_1990$lower <- round(EC_1990$lower,0)###取整
  EC_1990$upper <- round(EC_1990$upper,0) ###取整
  EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021发病人数
  EC_2021 <- EC %>%
    filter(year == 2021,
           age_name=='All ages',
           metric_name== 'Number',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_2021$val <- round(EC_2021$val,0)  ###取整
  EC_2021$lower <- round(EC_2021$lower,0)###取整
  EC_2021$upper <- round(EC_2021$upper,0) ###取整
  EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

  ## 1990 ASR
  ASR_1990 <- EC %>%
    filter(year == 1990,
           age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_1990$val <- round(ASR_1990$val,2)  ###取整
  ASR_1990$lower <- round(ASR_1990$lower,2)###取整
  ASR_1990$upper <- round(ASR_1990$upper,2) ###取整
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021 ASR
  ASR_2021 <- EC %>%
    filter(year == 2021,
           age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_2021$val <- round(ASR_2021$val,2)  ###取整
  ASR_2021$lower <- round(ASR_2021$lower,2)###取整
  ASR_2021$upper <- round(ASR_2021$upper,2) ###取整
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接
  
  ##### EAPC
  EAPC <- EC %>%
    filter(metric_name== 'Rate',
           measure_name== measure) %>%
    select(location_name, year, val)

  country <- unique(EC_1990$location_name)
  EAPC_cal <- data.frame(location_name=country,
                         EAPC=rep(0,times=length(country)),
                         UCI=rep(0,times=length(country)),
                         LCI=rep(0,times=length(country)))
  for (i in 1:length(country)){  ###总共23个地区，所以循环23次
    country_cal <- as.character(EAPC_cal[i,1]) ### 依次取对应的地区
    a <- subset(EAPC, EAPC$location_name==country_cal)  ##取对应地区的数据子集
    a$y <- log(a$val)  ##根据EAPC计算方法计算y值
    mod_simp_reg<-lm(y~year,data=a) ##根据EAPC计算方法做线性回归方程
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100 ##根据EAPC计算方法取方程beta值来计算EAPC
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的上限值
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    ### 计算EAPC的95%可信区间的下限值
    EAPC_cal[i,2] <- estimate
    EAPC_cal[i,4] <- low
    EAPC_cal[i,3] <- high
  }

  EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
  EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
  EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '')
  EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')

  ### 数据整合
  EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
  ASR_1990 <- ASR_1990[,c(1,5)]
  EC_2021 <- EC_2021[,c(1,5)]
  ASR_2021 <- ASR_2021[,c(1,5)]
  EAPC_cal <- EAPC_cal[,c(1,5)]
  Incidence <- merge(EC_1990,ASR_1990,by='location_name')
  Incidence <- merge(Incidence,EC_2021,by='location_name')
  Incidence <- merge(Incidence,ASR_2021,by='location_name')
  Incidence <- merge(Incidence,EAPC_cal,by='location_name')
  globalIncidence<-Incidence
  dir_path <- file.path(files, paste0("eapc\\",measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\region_eapc.csv')
  write.csv(globalIncidence, file_path,quote = FALSE)
}


#country
EC <- read.csv('country.csv',header = T)  ## 读取我们的数据
measures = unique(EC$measure_name)
for (measure in measures) {
  ## 1990发病人数
  EC_1990 <- EC %>%
    filter(year==1990,
           age_name=='All ages',
           metric_name== 'Number',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_1990$val <- round(EC_1990$val,0)  ###取整
  EC_1990$lower <- round(EC_1990$lower,0)###取整
  EC_1990$upper <- round(EC_1990$upper,0) ###取整
  EC_1990$Num_1990 <- paste(EC_1990$lower,EC_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_1990$Num_1990 <- paste(EC_1990$Num_1990,')',sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste('(',EC_1990$Num_1990,sep = '')  ##95%UI前后加括号
  EC_1990$Num_1990 <- paste(EC_1990$val,EC_1990$Num_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021发病人数
  EC_2021 <- EC %>%
    filter(year == 2021,
           age_name=='All ages',
           metric_name== 'Number',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  EC_2021$val <- round(EC_2021$val,0)  ###取整
  EC_2021$lower <- round(EC_2021$lower,0)###取整
  EC_2021$upper <- round(EC_2021$upper,0) ###取整
  EC_2021$Num_2021 <- paste(EC_2021$lower,EC_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  EC_2021$Num_2021 <- paste(EC_2021$Num_2021,')',sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste('(',EC_2021$Num_2021,sep = '')  ##95%UI前后加括号
  EC_2021$Num_2021 <- paste(EC_2021$val,EC_2021$Num_2021,sep = ' ') ##数据和95%UI用空格键连接

  ## 1990 ASR
  ASR_1990 <- EC %>%
    filter(year == 1990,
           age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_1990$val <- round(ASR_1990$val,2)  ###取整
  ASR_1990$lower <- round(ASR_1990$lower,2)###取整
  ASR_1990$upper <- round(ASR_1990$upper,2) ###取整
  ASR_1990$ASR_1990 <- paste(ASR_1990$lower,ASR_1990$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_1990$ASR_1990 <- paste(ASR_1990$ASR_1990,')',sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste('(',ASR_1990$ASR_1990,sep = '')  ##95%UI前后加括号
  ASR_1990$ASR_1990 <- paste(ASR_1990$val,ASR_1990$ASR_1990,sep = ' ') ##数据和95%UI用空格键连接

  ## 2021 ASR
  ASR_2021 <- EC %>%
    filter(year == 2021,
           age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(location_name, val, upper, lower)  # 只取需要的变量
  ASR_2021$val <- round(ASR_2021$val,2)  ###取整
  ASR_2021$lower <- round(ASR_2021$lower,2)###取整
  ASR_2021$upper <- round(ASR_2021$upper,2) ###取整
  ASR_2021$ASR_2021 <- paste(ASR_2021$lower,ASR_2021$upper,sep = '-') ## 用-连接95%UI上下数值
  ASR_2021$ASR_2021 <- paste(ASR_2021$ASR_2021,')',sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste('(',ASR_2021$ASR_2021,sep = '')  ##95%UI前后加括号
  ASR_2021$ASR_2021 <- paste(ASR_2021$val,ASR_2021$ASR_2021,sep = ' ') ##数据和95%UI用空格键连接
  
  ##### EAPC
  EAPC <- EC %>%
    filter(metric_name== 'Rate',
           measure_name== measure) %>%
    select(location_name, year, val)

  country <- unique(EC_1990$location_name)
  EAPC_cal <- data.frame(location_name=country,
                         EAPC=rep(0,times=length(country)),
                         UCI=rep(0,times=length(country)),
                         LCI=rep(0,times=length(country)))
  for (i in 1:length(country)) {  ### 总共23个地区，所以循环23次
    country_cal <- as.character(EAPC_cal[i, 1])  ### 依次取对应的地区
    a <- subset(EAPC, EAPC$location_name == country_cal)  ## 取对应地区的数据子集
    a <- a[a$val > 0, ]

    if (nrow(a) > 0) {  # 使用 if 语句
      a$y <- log(a$val)  ## 根据EAPC计算方法计算y值

      mod_simp_reg <- lm(y ~ year, data = a)  ## 根据EAPC计算方法做线性回归方程
      estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1]) - 1) * 100  ## 计算EAPC

      low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100
      high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 2]) - 1) * 100

      EAPC_cal[i, 2] <- estimate
      EAPC_cal[i, 4] <- low
      EAPC_cal[i, 3] <- high
    } else {
      next  # 停止并返回错误信息
    }
  }

  EAPC_cal$EAPC <- round(EAPC_cal$EAPC,2)  ##保留2位小数点
  EAPC_cal$UCI <- round(EAPC_cal$UCI,2)
  EAPC_cal$LCI <- round(EAPC_cal$LCI,2)
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$LCI,EAPC_cal$UCI,sep = '-')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC_CI,')',sep = '')
  EAPC_cal$EAPC_CI <- paste('(',EAPC_cal$EAPC_CI,sep = '')
  EAPC_cal$EAPC_CI <- paste(EAPC_cal$EAPC,EAPC_cal$EAPC_CI,sep = ' ')

  ### 数据整合
  EC_1990 <- EC_1990[,c(1,5)]  ###取地区和整合好的变量
  ASR_1990 <- ASR_1990[,c(1,5)]
  EC_2021 <- EC_2021[,c(1,5)]
  ASR_2021 <- ASR_2021[,c(1,5)]
  EAPC_cal <- EAPC_cal[,c(1,5)]
  Incidence <- merge(EC_1990,ASR_1990,by='location_name')
  Incidence <- merge(Incidence,EC_2021,by='location_name')
  Incidence <- merge(Incidence,ASR_2021,by='location_name')
  Incidence <- merge(Incidence,EAPC_cal,by='location_name')
  globalIncidence<-Incidence
  dir_path <- file.path(files, paste0("eapc\\",measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\country_eapc.csv')
  write.csv(globalIncidence, file_path,quote = FALSE)
}




#趋势分析
library(ggsci)
library(cowplot)

#图片
data1<-data.table::fread(file="global.csv",encoding="UTF-8")
plots <- list()
#总体
for (measure in measures) {
  DR <- data1 %>%
    filter(age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select("location_name","year","val","upper","lower")
  DN <- data1 %>%
    filter(age_name=='All ages',
           metric_name== 'Number',
           measure_name == measure) %>%
    select("location_name","year","val","upper","lower")
  colnames(DN)[3] <- "numberval"
  colnames(DN)[4] <- "numberupper"
  colnames(DN)[5] <- "numberlower"

  Incidence <- merge(DR,DN,by=c("location_name","year"))
  coeff=max(Incidence$numberval)/max(Incidence$val)
  pal_jco("default")(10)

  P1<-ggplot(Incidence, aes(x =year))+#总的控制#
    geom_bar(aes(y=numberval), color="#dfc27d",fill="#dfc27d",stat="identity",position = "dodge",width = 0.8,alpha=0.5)+#画条形图，width可以设置条形图宽度#
    geom_line(aes(y=val*coeff),size=0.8,color="#dfc27d")+#画第二条Y轴的线图#
    geom_point(aes(y=val*coeff),size=2,color="#dfc27d")+#画第二条Y轴的线图#
    theme_classic()+#主题#
    labs(title = measure) +
    theme(panel.grid.major = element_line(colour = "grey", size = 0.5))+
    theme(axis.text.x=element_text(angle = 45,hjust=0.5,vjust=0.5,color="black",size=12),
          axis.text.y=element_text(color="black",size=12),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),legend.title =element_blank(),
          legend.position="top",
          legend.spacing.y = unit(0.5, "cm"),legend.text=element_text(size=8))+#设置字体颜色#
    scale_y_continuous(sec.axis = sec_axis(~./coeff,name="Age-standardized  rate (per 100,000)"))+#设置y轴刻度及标签#
    labs(y="    Number of  cases",x="Year")+#设置xy轴的标题#
    guides(fill=guide_legend(nrow=3,ncol = 1))+####设置填充和颜色图例的位置
    guides(shape=guide_legend(nrow=3,ncol=1))####设置形状图例的位置
  plots[[as.character(measure)]] <- P1
  file_path <- file.path(files, paste0('90-21_global_',measure,'.png'))
  ggsave(file_path,P1,width = 10,height = 8)
}
# 图片拼接
combined_plot <- ggarrange(plots[[1]], plots[[2]],plots[[3]],nrow = 2,ncol = 2, common.legend = FALSE)
file_path <- paste0(files, "\\combined_90-21_global_plots.png")
ggsave(file_path, plot = combined_plot, height = 8, width = 20)




#Sex亚组分析
SEX<-data.table::fread(file="sex.csv",encoding="UTF-8")
SEX$measure_name <- factor(SEX$measure_name, levels=sort(unique(SEX$measure_name)))
SEX$sex_name <- factor(SEX$sex_name, levels=c('Both',"Female","Male"))

A7 <- SEX[age_name == 'Age-standardized' &
            metric_name == 'Rate' ,
          .(sex_name,measure_name, year, val,upper,lower)]
A7<-ggplot(A7,aes(x=year, y=val,colour=sex_name, group=sex_name))+geom_line(size = 1.3)+geom_point(size = 1.5)+ylab(label="Age-standardized rate") + xlab(label="Year")+
  theme_classic()+
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+
  guides(colour=guide_legend(title=NULL))+#子图标签细节控制
  scale_fill_brewer(palette = "Set2")
A7
file_path <- paste0(files, "\\90-21_sex_ASR.png")
ggsave(file_path, plot = A7, height = 8, width = 10)

B7 <- SEX[age_name == 'All ages' &
            metric_name == 'Number' ,
          .(sex_name,measure_name, year, val,upper,lower)]
B7<-ggplot(B7,aes(x=year, y=val,colour=sex_name, group=sex_name))+geom_line(size = 1.3)+geom_point(size = 1.5)+ylab(label="Number of cases") + xlab(label="Year")+
  theme_classic()+
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+
  guides(colour=guide_legend(title=NULL))+#子图标签细节控制
  scale_fill_brewer(palette = "Set2")
B7
file_path <- paste0(files, "\\90-21_sex_number.png")
ggsave(file_path, plot = B7, height = 8, width = 10)

combined_plot <- ggarrange(A7, B7, ncol = 2, common.legend = FALSE)
combined_plot
# 保存组合后的图形到文件
file_path <- paste0(files, "\\combined_90-21_sex_number_plots.png")
ggsave(file_path, plot = combined_plot, height = 8, width = 20)


#Age亚组分析
AGE<-data.table::fread(file="age.csv",encoding="UTF-8")
AGE$measure_name <- factor(AGE$measure_name, levels=sort(unique(AGE$measure_name)))
AGE$age_name <- factor(AGE$age_name, levels=c('0-6 days','7-27 days','1-5 months','6-11 months','12-23 months','2-4 years','<5 years', '5-9 years','10-14 years','15-19 years','20-24 years',
                                              '25-29 years','30-34 years','35-39 years','40-44 years','45-49 years','50-54 years','55-59 years','60-64 years',
                                              '65-69 years','70-74 years','75-79 years','80-84 years','85-89 years','90-94 years','95+ years'))

A8 <- AGE[metric_name == 'Rate' ,
          .(age_name,measure_name, year, val,upper,lower)]
A8<-ggplot(A8,aes(x=year, y=val,colour=age_name, group=age_name))+geom_line(size = 1.3)+geom_point(size = 1.5)+ylab(label="Age-standardized rate") + xlab(label="Year")+
  theme_classic()+
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+
  guides(colour=guide_legend(title=NULL))#子图标签细节控制
A8
file_path <- paste0(files, "\\90-21_Age_ASR.png")
ggsave(file_path, plot = A8, height = 8, width = 10)

B8 <- AGE[metric_name == 'Number' ,
          .(age_name,measure_name, year, val,upper,lower)]
B8<-ggplot(B8,aes(x=year, y=val,colour=age_name, group=age_name))+geom_line(size = 1.3)+geom_point(size = 1.5)+ylab(label="Number of cases") + xlab(label="Year")+
  theme_classic()+
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+
  guides(colour=guide_legend(title=NULL))#子图标签细节控制
B8
file_path <- paste0(files, "\\90-21_Age_number.png")
ggsave(file_path, plot = B8, height = 8, width = 10)

combined_plot <- ggarrange(A8, B8, ncol = 2, common.legend = FALSE)
combined_plot
# 保存组合后的图形到文件
file_path <- paste0(files, "\\combined_90-21_Age_number_plots.png")
ggsave(file_path, plot = combined_plot, height = 8, width = 20)



#SDI亚组分析
SDI<-data.table::fread(file="SDI.csv",encoding="UTF-8")
SDI$measure_name <- factor(SDI$measure_name, levels=sort(unique(SDI$measure_name)))
SDI$location_name <- factor(SDI$location_name, levels=c("High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"))

A9 <- SDI[age_name == 'Age-standardized' &
            sex_name == 'Both' &
            metric_name == 'Rate' ,
          .(location_name,measure_name, year, val,upper,lower)]
A9<-ggplot(A9,aes(x=year, y=val,colour=location_name, group=location_name))+geom_line(size = 1.3)+geom_point(size = 1.5)+ylab(label="Age-standardized rate") + xlab(label="Year")+
  theme_classic()+
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+
  guides(colour=guide_legend(title=NULL))+
  scale_fill_brewer(palette = "Set2")
A9
file_path <- paste0(files, "\\90-21_SDI_ASR.png")
ggsave(file_path, plot = A9, height = 8, width = 10)

B9 <- SDI[age_name == 'All ages' &
            sex_name == 'Both' &
            metric_name == 'Number' ,
          .(location_name,measure_name, year, val,upper,lower)]
B9<-ggplot(B9,aes(x=year, y=val,colour=location_name, group=location_name))+geom_line(size = 1.3)+geom_point(size = 1.5)+ylab(label="Number of cases") + xlab(label="Year")+
  theme_classic()+
  theme(panel.grid.major = element_line(colour = "grey", size = 0.5))+
  facet_wrap(vars(measure_name ),nrow = 2,
             scales = "free_y",#控制 y轴尺度,"free","free_y","free_x"
             strip.position = "top")+#控制标签位置,"top,"left","right"
  theme(strip.text = element_text(size = 10))+
  guides(colour=guide_legend(title=NULL))+
  scale_fill_brewer(palette = "Set2")
B9
file_path <- paste0(files, "\\90-21_SDI_number.png")
ggsave(file_path, plot = B9, height = 8, width = 10)

combined_plot <- ggarrange(A9, B9, ncol = 2, common.legend = FALSE)
combined_plot
# 保存组合后的图形到文件
file_path <- paste0(files, "\\combined_90-21_SDI_number_plots.png")
ggsave(file_path, plot = combined_plot, height = 8, width = 20)



# region 聚类分析
library(dplyr)
library(ggplot2)
library(ggsci)
library(factoextra)
EC <- read.csv('region.csv',header = T)  ## 读取我们的数据
measures = unique(EC$measure_name)
clusters <- list()
for (measure in measures) {
  ### 计算发病率的EAPC数据
  EAPC <- subset(EC, EC$age_name=='Age-standardized' & 
                   EC$metric_name== 'Rate' &
                   EC$measure_name==measure)
  
  EAPC <- EAPC[,c("location_name","year","val")]
  
  country_name <- subset(EC,EC$year==1990 & 
                           EC$age_name=='Age-standardized' & 
                           EC$metric_name== 'Rate' &
                           EC$measure_name==measure) ###获取国家名称
  country <- country_name$location_name  ###获取国家名称
  EAPC_cal <- data.frame(location_name=country,EAPC=rep(0,times=length(country)),UCI=rep(0,times=length(country)),LCI=rep(0,times=length(country)))
  for (i in 1:length(country)){
    country_cal <- as.character(EAPC_cal[i,1])
    a <- subset(EAPC, EAPC$location_name==country_cal)
    a$y <- log(a$val)
    mod_simp_reg<-lm(y~year,data=a)
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    EAPC_cal[i,2] <- estimate
    EAPC_cal[i,4] <- low
    EAPC_cal[i,3] <- high
  }
  EAPC_cluster <- EAPC_cal[,c(1,2)]
  names(EAPC_cluster)[2] <- paste0("eapc_",measure)
  clusters[[as.character(measure)]] <- EAPC_cluster
}

if (length(clusters) > 1) {
  cluster <- clusters[[1]]
  for (i in 2:length(clusters)) {
    cluster <- merge(cluster, clusters[[i]], by = 'location_name')
  }
} else {
  cluster <- if (length(clusters) == 1) clusters[[1]] else NULL
}

data <- cluster
rownames(data) <- data[,1]
data <- data[,-1]
## 画聚类图形
df <- scale(data)
# Hierarchical clustering
res.hc <- hclust(dist(df,method="euclidean"),method = "complete")
###画图
J8<-fviz_dend(res.hc, cex = 0.5, k = 4, color_labels_by_k = TRUE,
              k_colors=c("#2878b5","#c82423","#54B345","mediumpurple4"),
              horiz = T, rect = TRUE)
J8
file_path <- paste0(files, "\\region_cluster.png")
ggsave(file_path, plot = J8, height = 8, width = 10)






#install.packages("dplyr", dependencies = TRUE, type = "binary")
#install.packages("ggplot2", dependencies = TRUE, type = "binary"
#install.packages("ggsci", dependencies = TRUE, type = "binary")
#install.packages("factoextra2", dependencies = TRUE, type = "binary")
library(dplyr)
library(ggplot2)
library(ggsci)
library(factoextra)
#country亚组分析
## map for EAPC
#读取数据
AR<-data.table::fread(file="country.csv",encoding="UTF-8")
measures = sort(unique(AR$measure_name))
for (measure in measures) {
  EAPC <- AR %>%
    filter(age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(location_name, year, val)  # 只取需要的变量
  country <- EAPC$location_name  ###获取国家名称
  EAPC_cal <- data.frame(location=country,EAPC=rep(0,times=length(country)),UCI=rep(0,times=length(country)),LCI=rep(0,times=length(country)))
  for (i in 1:length(country)){
    country_cal <- as.character(EAPC_cal[i,1])
    a <- subset(EAPC, EAPC$location==country_cal)
    a$y <- log(a$val)
    mod_simp_reg<-lm(y~year,data=a)
    estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1])-1)*100
    low <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]-1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    high <- (exp(summary(mod_simp_reg)[["coefficients"]][2,1]+1.96*summary(mod_simp_reg)[["coefficients"]][2,2])-1)*100
    EAPC_cal[i,2] <- estimate
    EAPC_cal[i,4] <- low
    EAPC_cal[i,3] <- high
  }
  country_asr <- EAPC_cal
  country_asr$location <- as.character(country_asr$location)
  country_asr$location[country_asr$location == 'United States of America'] = 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
  country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
  country_asr$location[country_asr$location == "North Macedonia"] = 'North Macedonia'
  country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
  country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
  country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
  country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
  country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
  country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
  country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
  country_asr$location[country_asr$location == "C么te d'Ivoire"] = 'Ivory Coast'
  country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
  country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
  country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
  worldData <- map_data('world')
  total <- full_join(worldData,country_asr,by = c('region'='location'))
  p <- ggplot()
  total <- total %>% mutate(val2 = cut(EAPC, breaks = pretty(EAPC, n = 8), # 自动生成间隔
                                       include.lowest = TRUE,
                                       right = TRUE))
  # 增加颜色数量
  C1 <- p + geom_polygon(data=total,
                         aes(x=long, y=lat, group = group, fill=val2),
                         colour="black", size = .2) +
    scale_fill_brewer(palette = "Set3") +  # 添加第12种颜色
    theme_map() +
    labs(title = paste0("Age-standardized ", measure, " Rate")) +
    theme(legend.position = 'right') + theme(legend.title=element_blank(),
                                             panel.background = element_rect(fill = "white", colour = NA),
                                             plot.background = element_rect(fill = "white", colour = NA))  # 设置绘图背景为白色)
  C1
  file_path <- file.path(files, paste0('EAPCmap_',measure,'.png'))
  ggsave(file_path,C1,width = 15,height = 8)
}



### case change
####   case change MAP
for (measure in measures) {
  case_2021 <- subset(AR, AR$year==2021 &
                        AR$age_name=='All ages' &
                        AR$metric_name== 'Number' &
                        AR$measure_name==measure) ## 获取2021年EC发病数
  case_1990 <- subset(AR, AR$year==1990 &
                        AR$age_name=='All ages' &
                        AR$metric_name== 'Number' &
                        AR$measure_name==measure) ## 获取1990年EC发病数
  case_1990 <- case_1990[,c("location_name","val")]
  case_2021 <- case_2021[,c("location_name","val")]
  names(case_1990) <- c('location','case_1990')
  names(case_2021) <- c('location','case_2021')
  country_asr <- merge(case_1990, case_2021, by='location')
  country_asr$val <- (country_asr$case_2021-country_asr$case_1990)/country_asr$case_1990*100  ### 获取我们的结果
  country_asr$location <- as.character(country_asr$location)
  country_asr$location[country_asr$location == 'United States of America'] = 'USA'
  country_asr$location[country_asr$location == 'Russian Federation'] = 'Russia'
  country_asr$location[country_asr$location == 'United Kingdom'] = 'UK'
  country_asr$location[country_asr$location == 'Congo'] = 'Republic of Congo'
  country_asr$location[country_asr$location == "Iran (Islamic Republic of)"] = 'Iran'
  country_asr$location[country_asr$location == "Democratic People's Republic of Korea"] = 'North Korea'
  country_asr$location[country_asr$location == "Taiwan (Province of China)"] = 'Taiwan'
  country_asr$location[country_asr$location == "Republic of Korea"] = 'South Korea'
  country_asr$location[country_asr$location == "United Republic of Tanzania"] = 'Tanzania'
  country_asr$location[country_asr$location == "C?te d'Ivoire"] = 'Saint Helena'
  country_asr$location[country_asr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
  country_asr$location[country_asr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
  country_asr$location[country_asr$location == "Czechia"] = 'Czech Republic'
  country_asr$location[country_asr$location == "Republic of Moldova"] = 'Moldova'
  country_asr$location[country_asr$location == "Viet Nam"] = 'Vietnam'
  country_asr$location[country_asr$location == "Lao People's Democratic Republic"] = 'Laos'
  country_asr$location[country_asr$location == "Syrian Arab Republic"] = 'Syria'
  country_asr$location[country_asr$location == "North Macedonia"] = 'Macedonia'
  country_asr$location[country_asr$location == "Micronesia (Federated States of)"] = 'Micronesia'
  country_asr$location[country_asr$location == "North Macedonia"] = 'North Macedonia'
  country_asr$location[country_asr$location == "Trinidad and Tobago"] = 'Trinidad'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Trinidad",])
  country_asr$location[country_asr$location == "Trinidad"] = 'Tobago'
  country_asr$location[country_asr$location == "Cabo Verde"] = 'Cape Verde'
  country_asr$location[country_asr$location == "United States Virgin Islands"] = 'Virgin Islands'
  country_asr$location[country_asr$location == "Antigua and Barbuda"] = 'Antigu'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Antigu",])
  country_asr$location[country_asr$location == "Antigu"] = 'Barbuda'
  country_asr$location[country_asr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Kitts",])
  country_asr$location[country_asr$location == "Saint Kitts"] = 'Nevis'
  country_asr$location[country_asr$location == "C么te d'Ivoire"] = 'Ivory Coast'
  country_asr$location[country_asr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
  country_asr <- rbind(country_asr,country_asr[country_asr$location == "Saint Vincent",])
  country_asr$location[country_asr$location == "Saint Vincent"] = 'Grenadines'
  country_asr$location[country_asr$location == "Eswatini"] = 'Swaziland'
  country_asr$location[country_asr$location == "Brunei Darussalam"] = 'Brunei'
  worldData <- map_data('world')
  total <- full_join(worldData,country_asr,by = c('region'='location'))
  p <- ggplot()
  total <- total %>% mutate(val2 = cut(val, breaks = c(-200,-150,-100,-50,0,50,100,150,200),
                                       labels = c("150% to 200% decrease","100% to 150% decrease","50% to 100% decrease","0% to 50% decrease", "0% to 50% increase","50% to 100% increase","100% to 150% increase","150% to 200% increase"),
                                       ### breaks需要根据自己的实际结果来调整
                                       include.lowest = T,right = T))
  B1 <- p + geom_polygon(data=total,
                         aes(x=long, y=lat, group = group,fill=val2),
                         colour="black",size = .2) +
    scale_fill_manual(values = c("#006400", "#66CD00", "#FFE4C4", "#FF7256", "#FF4040", "#CD3333", "#8B2323"))+
    theme_void()+labs(title=paste0(".    Change in the number of ",measure," cases"))+
    theme(legend.position = 'right')+ theme(legend.title=element_blank(),
                                            panel.background = element_rect(fill = "white", colour = NA),
                                            plot.background = element_rect(fill = "white", colour = NA))  # 设置绘图背景为白色)
  file_path <- paste0(files, "\\", measure,"_cases.png")
  ggsave(file_path, plot = B1, height = 8, width = 10)
}



#ES模型
setwd(path)
library(forecast)
#install.packages("fpp2")
library(fpp2)

measures = unique(EC$measure_name)
global<-data.table::fread(file="sex.csv",encoding="UTF-8")
global <- global[sex_name == 'Male']

for (measure in measures) {
  A1 <- global %>%
    filter(age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  train<- A1$val
  train<-ts(train, start=1990)
  fc1 <- holt(train, damped=TRUE, phi = 0.9, h=29)
  dir_path <- file.path(files,paste0('ES模型ASR\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\male.csv')
  write.csv(fc1,file_path)
}

for (measure in measures) {
  A1 <- global %>%
    filter(age_name == 'All ages',
           metric_name == 'Number',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  train<- A1$val
  train<-ts(train, start=1990)
  fc1 <- holt(train, damped=TRUE, phi = 0.9, h=29)
  dir_path <- file.path(files,paste0('ES模型number\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\male.csv')
  write.csv(fc1,file_path)
}


global<-data.table::fread(file="sex.csv",encoding="UTF-8")
global <- global[sex_name == 'Female']
for (measure in measures) {
  A1 <- global %>%
    filter(age_name=='Age-standardized',
           metric_name== 'Rate',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  train<- A1$val
  train<-ts(train, start=1990)
  fc1 <- holt(train, damped=TRUE, phi = 0.9, h=29)
  dir_path <- file.path(files,paste0('ES模型ASR\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\female.csv')
  write.csv(fc1,file_path)
}

for (measure in measures) {
  A1 <- global %>%
    filter(age_name == 'All ages',
           metric_name == 'Number',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  train<- A1$val
  train<-ts(train, start=1990)
  fc1 <- holt(train, damped=TRUE, phi = 0.9, h=29)
  dir_path <- file.path(files,paste0('ES模型number\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\female.csv')
  write.csv(fc1,file_path)
}

setwd(files)
library(cowplot)
plots <- list()
for (measure in measures) {
  manNum<-data.table::fread(file=paste0(files,'\\ES模型number\\',measure,'\\male.csv'),encoding="UTF-8")
  manNum$sex = 'Male'
  womanNum<-data.table::fread(file=paste0(files,'\\ES模型number\\',measure,'\\female.csv'),encoding="UTF-8")
  womanNum$sex = 'Female'
  manASR<-data.table::fread(file=paste0(files,'\\ES模型ASR\\',measure,'\\male.csv'),encoding="UTF-8")
  manASR$sex = 'Male'
  womanASR<-data.table::fread(file=paste0(files,'\\ES模型ASR\\',measure,'\\female.csv'),encoding="UTF-8")
  womanASR$sex = 'Female'
  Num = rbind(manNum,womanNum)
  ASR = rbind(manASR,womanASR)
  year1 <- 2022:2050
  
  Num<-Num %>% filter(V1 %in% year1)
  ASR<-ASR %>% filter(V1 %in% year1)
  
  ratio <-max(ASR$Point.Forecast)/max(Num$Point.Forecast)
  
  head(Num)
  p1 <- ggplot(Num, aes(V1, Point.Forecast, fill = factor(sex))) +  # sex是分组变量
    geom_col(position = 'dodge', width = 0.8) +
    labs(title = NULL, x = 'Year', y = 'Number of cases') +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, size = 8, color = 'black'),
          axis.text.y = element_text(size = 8, color = 'black'),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          strip.background.x = element_rect(fill = 'skyblue3'),
          title = element_text(size = 10, hjust = 0.5),
          legend.position = 'right') +
    geom_line(data = ASR,  # 绘制连接ASR数据点的直线
              aes(x = V1, y = Point.Forecast / ratio, color = factor(sex)),
              size = 1) +  # 可以设置线的粗细
    geom_point(data = ASR,  # 在ASR数据点上添加点
               aes(x = V1, y = Point.Forecast / ratio, color = factor(sex)),
               size = 1.5) +  # 设置点的大小
    scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~ . * ratio,
                                                             name = " Age-standardized  rate")) +
    theme_classic() +
    labs(title = measure)+
    theme(legend.title = element_blank()) +
    scale_fill_manual(values = c('#ff7f00', '#00acb3'))  # 指定颜色
  plots[[as.character(measure)]] <- p1
  ggsave(paste0('ES_', measure, '.png'), p1, width = 10, height = 8)
}
combined_plot <- ggarrange(plots[[1]], plots[[2]],plots[[3]], ncol = 2,nrow = 2, common.legend = FALSE)
ggsave('ES_measure_plots.png', plot = combined_plot, width = 20, height = 8)



setwd(path)
#ARIMA
library(ggplot2)
library(forecast)
measures = unique(global$measure_name)

global <- data.table::fread(file = "sex.csv",encoding = "UTF-8")
global <- global[sex_name == 'Male']

for (measure in measures) {
  A1 <- global %>%
    filter(age_name == 'Age-standardized',
           metric_name == 'Rate',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  fit.arima <- auto.arima(A1$val)
  summary(fit.arima)
  b1<-forecast(fit.arima,h=29)
  dir_path <- file.path(files,paste0('ARIMA模型ASR\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\male.csv')
  write.csv(b1,file_path)
}

for (measure in measures) {
  A1 <- global %>%
    filter(age_name == 'All ages',
           metric_name == 'Number',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  fit.arima <- auto.arima(A1$val)
  summary(fit.arima)
  b1<-forecast(fit.arima,h=29)
  dir_path <- file.path(files,paste0('ARIMA模型number\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\male.csv')
  write.csv(b1,file_path)
}


global<-data.table::fread(file="sex.csv",encoding="UTF-8")
global <- global[sex_name == 'Female']
for (measure in measures) {
  A1 <- global %>%
    filter(age_name == 'Age-standardized',
           metric_name == 'Rate',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  fit.arima <- auto.arima(A1$val)
  summary(fit.arima)
  b1<-forecast(fit.arima,h=29)
  dir_path <- file.path(files,paste0('ARIMA模型ASR\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\female.csv')
  write.csv(b1,file_path)
}

for (measure in measures) {
  A1 <- global %>%
    filter(age_name == 'All ages',
           metric_name == 'Number',
           measure_name == measure) %>%
    select(year,val,upper,lower)  # 只取需要的变量
  fit.arima <- auto.arima(A1$val)
  summary(fit.arima)
  b1<-forecast(fit.arima,h=29)
  dir_path <- file.path(files,paste0('ARIMA模型number\\', measure))
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  file_path <- file.path(dir_path, '\\female.csv')
  write.csv(b1,file_path)
}

setwd(files)
library(cowplot)
plots <- list()
for (measure in measures) {
  manNum<-data.table::fread(file=paste0(files,'\\ARIMA模型number\\',measure,'\\male.csv'),encoding="UTF-8")
  manNum$sex = 'Male'
  womanNum<-data.table::fread(file=paste0(files,'\\ARIMA模型number\\',measure,'\\female.csv'),encoding="UTF-8")
  womanNum$sex = 'Female'
  manASR<-data.table::fread(file=paste0(files,'\\ARIMA模型ASR\\',measure,'\\male.csv'),encoding="UTF-8")
  manASR$sex = 'Male'
  womanASR<-data.table::fread(file=paste0(files,'\\ARIMA模型ASR\\',measure,'\\female.csv'),encoding="UTF-8")
  womanASR$sex = 'Female'
  manNum$V1 = 2022:2050
  womanNum$V1 = 2022:2050
  manASR$V1 = 2022:2050
  womanASR$V1 = 2022:2050


  Num = rbind(manNum,womanNum)
  ASR = rbind(manASR,womanASR)

  ratio <-max(ASR$Point.Forecast)/max(Num$Point.Forecast)

  head(Num)
  p1 <- ggplot(Num,aes(V1,Point.Forecast))+
    geom_col(aes(fill=factor(sex)),position = 'dodge',width = 0.8)+
    labs(title = NULL,x='Year',y='Number of cases') +
    theme_bw() +
    labs(title = measure)+
    theme(plot.title=element_text(hjust=0.5),
          axis.text.x=element_text(angle=45,size=8,color='black'),
          axis.text.y=element_text(size=8,color='black'),
          axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          strip.background.x = element_rect(fill = 'skyblue3'),
          title = element_text(size = 10, hjust = 0.5),
          legend.position = 'right') +
    geom_line(data = ASR,  # 绘制连接ASR数据点的直线
              aes(x = V1, y = Point.Forecast / ratio, color = factor(sex)),
              size = 1) +  # 可以设置线的粗细
    geom_point(data = ASR,  # 在ASR数据点上添加点
               aes(x = V1, y = Point.Forecast / ratio, color = factor(sex)),
               size = 1.5) +  # 设置点的大小
    scale_y_continuous(expand=c(0,0),sec.axis = sec_axis(.~.*ratio,
                                                         name="Age-standardized rate"))+
    theme_classic()+ theme(legend.title=element_blank())+
    scale_fill_manual(values = c('#984ea3', '#80b1d3'))  # 指定颜色
  plots[[as.character(measure)]] <- p1
  file_path <- file.path(files, paste0('ARIMA_',measure,'.png'))
  ggsave(file_path,p1,width = 10,height = 8)
}
combined_plot <- ggarrange(plots[[1]], plots[[2]],plots[[3]],  ncol = 2,nrow = 2, common.legend = FALSE)
ggsave('ARIMA_measure_plots.png', plot = combined_plot, width = 20, height = 8)


# 返回到 R 的启动目录
setwd("E:\\RStudio")  # ~ 表示用户的主目录
