library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
# library("ggpubr")
library(emmeans)
library(lmerTest)
library(stringi)
library(stringr)
# install.packages("xlsx")
# library(xlsx)



# прописываем папку откуда беруться данные (path) и куда будут сохраняться полученные значения pvalue
path <- 'C:/Users/Temga/Desktop/LMEM/df_for_LMEM'
out_path <-'C:/Users/Temga/Desktop/LMEM/p_values_LMEM/'

#### prepare table with info ####
# sensors.csv - таблица где прописано соответсвие между порядковым номером сеносра и его обозначением (planar1)
sensor_info <- fread('C:/Users/Temga/Desktop/LMEM/sensors.csv')
files <- data.table(full_filename=list.files(path, pattern = '*.csv', full.names = T))
files$short_filename <- list.files(path, pattern = '*.csv', full.names = F)
# files$short_filename <- gsub('planar1','',files$short_filename)

files[, sensor:=stri_extract_first_regex(short_filename,"[0-9]+")]
files[, interval:=str_extract(short_filename,'[0-9]+_[0-9]+.csv')]
files[,interval:=gsub('.csv','',interval)]
files$sensor <- as.integer(files$sensor)
files <- merge.data.table(files,sensor_info,by = c('sensor'))

files[1]

files$effect <- NULL

## load subj_list ##
# список испытуемых
subj_list <- fread('C:/Users/Temga/Desktop/LMEM/subj_list.csv')


#### make large table with p_values ####

temp <- fread(files[sensor==0]$full_filename)
temp$V1 <- NULL
cols <- colnames(temp)[grep('[0-9]',colnames(temp))]

######## for trial_type #############
p_vals <- data.table()
for (i in files$sensor){
  temp <- fread(files[sensor==i]$full_filename)
  temp$V1 <- NULL
  
  subj_list
  
  subj_list$subj_list
  
  temp <- temp[subject %in% subj_list$subj_list]
  
  temp$subject <- as.factor(temp$subject)
  temp$round <- as.factor(temp$round)
  temp$stim_type <-as.factor(temp$stim_type)
  temp$stage_of_learning <-as.factor(temp$stage_of_learning)
  temp$part_of_exp <-as.factor(temp$part_of_exp)
  temp$real_stim <- as.factor(temp$real_stim)
  
  for (j in cols){
    m <- lmer(get(j) ~ stim_type + (1|subject), data = temp)
    m
    an <- anova(m)
    an
    an <- data.table(an,keep.rownames = TRUE)
    an_cols <- c('rn','Pr(>F)') 
    an <- an[, ..an_cols]
    an$`Pr(>F)` <- format(an$`Pr(>F)`, digits = 3)
    an$interval <- j
    an$interval <- gsub('ERP','',an$interval)
    an <- dcast(an,formula = interval~rn,value.var = 'Pr(>F)')
    an$sensor <- i
    an$sensor_name <- files[sensor==i]$sensor_name
    anр
    p_vals <- rbind(p_vals,an)
  }
}

p_vals

write.csv(p_vals,paste0(out_path, "p_vals_factor_significance_MEG.csv"))

