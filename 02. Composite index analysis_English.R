###-----------------------------------------------------------####
###-----------------------------------------------------------####
####-------------------ETI: City-level index construction----------------------####
###-----------------------------------------------------------####
###-----------------------------------------------------------####
rm(list=ls())
library(dplyr)
library(dplyr)
library(zoo)
# Open the file selection dialog
file_path <- file.choose()
# Import the CSV file
Data <- read.csv(file_path)
###-----------------------------
# Index list:
###-----------------------------

#---------------
#01. System Performance score
#---------------

####A. Energy system structure:
#A_1_Energy mix: share of coal
#A_2_Electricity structure: energy consumption for power generation / electricity consumption
#A_3_Energy intensity: energy consumption / GDP
#A_4_Energy consumption: 1. electricity consumption per capita; 2. fossil energy consumption per capita;

####B. Environmental sustainability:
#B_1_Carbon intensity:
#B_2_Carbon emissions per capita:
#B_3_Air pollution: PM2.5

#---------------
#02. Transition Readiness score
#---------------

####C. Economic development:
#C_1_Economic growth: 1. GDP per capita; 2. GDP growth
#C_2_Economic structure: 1. share of mining employment in secondary industry; 2. share of tertiary industry;

####D. Capital & investment:
#D_1_Capital stock: 1. average annual balance of net fixed assets per capita; 2. share of urban construction land in the municipal area; 3. deposits per capita;
#D_2_Investment: 1. fixed-asset investment per capita; 2. actually utilized foreign capital per capita;
#D_3_Fiscal capacity: public finance income per capita;

####E. Technology capability:
#E_1_Innovation capability: 1. new economy: share of internet access subscribers; 2. green invention and utility model patents per capita
#E_2_Technology expenditure: science and technology expenditure per capita
#E_3_Adaptive technology: 1. SO2 removal rate; 2. industrial wastewater treatment rate; 3. harmless treatment rate of household waste; 4. comprehensive utilization rate of industrial solid waste;

####F. Human capital:
#F_1_R&D and new economy: 1. share of R&D personnel; 2. information technology personnel;
#F_2_Education and training capacity: 1. share of education employees; 2. secondary vocational teachers; 3. higher education teachers;
#F_3_Quality of education: 1. education expenditure per capita; 2. share of college students;


###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###
############################################### Step_2: Data processing ###################################################
###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###

#------------------------------------
#------------------------------------
####A. Initial data check
#------------------------------------
#------------------------------------

# #Initial number of cities: 281
# Using the benchmark indicator, city counts are 219 for >10 observations, 249 for >5 observations, 261 for >1 observation, and 232 cities with data in 2010.
# Check the number of values
# 1. Add count column and remove missing values
Data_unit <- Data %>%
  mutate(Units = 1) %>%
  filter(!is.na(A_1_能源结构))
# 2. Count observations for each city
city_count <- Data_unit %>%
  group_by(城市) %>%
  summarise(n_obs = n())
# 3. Count cities under different thresholds
n_total  <- length(unique(Data$城市))                     # total number of cities
n_gt1    <- sum(city_count$n_obs > 1)                     # >1
n_gt5    <- sum(city_count$n_obs > 5)                     # >5
n_gt10   <- sum(city_count$n_obs > 10)                    # >10
# 4. Count cities with data in 2010
n_2010 <- Data_unit %>%
  filter(年份 == 2010) %>%
  summarise(n_city = n_distinct(城市)) %>%
  pull(n_city)
# 5. Print results
cat("总城市数：", n_total, "\n")
cat(">1期城市数：", n_gt1, "\n")
cat(">5期城市数：", n_gt5, "\n")
cat(">10期城市数：", n_gt10, "\n")
cat("2010年有数据城市数：", n_2010, "\n")
# 6. Clean up
rm(city_count, Data_unit)

#------------------------------------
#------------------------------------
####B. Index construction
#------------------------------------
#------------------------------------

#------------------------------------
####Step B_0. Indicator selection --------------------This step is no longer required after the 09/27 version
#------------------------------------

# #Remove columns without data (i.e., columns where all values are NA)
# Data_temp <- Data[,colSums(is.na(Data))<nrow(Data)]
# 
# sapply(Data_temp, class)
# 
# # #Change to numeric
# # for (i in 4:length(Data_temp)){
# #   Data_temp[,i]<-as.numeric(Data_temp[,i])
# # }
# 
# ##The last three variables are not used for now
# Data_temp=Data_temp[,1:(ncol(Data_temp)-3)] 
# 
# #Define new variable
# Data_2=Data_temp
# 
# #Remove redundant variables
# rm(Data_temp) 

#------------------------------------
####Step B_0. Time range selection
#------------------------------------

Data_2=Data

#01. Time variable selection (data updated to 2022)

Data_2=filter(Data_2,年份<2023) 


#------------------------------------
####Step B_1. Spatial scope selection
#------------------------------------

# Laiwu City was abolished in 2019 (already removed from the initial data, so this step is skipped)

# Disabled code: filter out Laiwu City if needed.

#------------------------------------
####Step B_2. Data imputation; use the simplest approach for now (if one value is available, use it for imputation; if no value is available, use the mean; future versions may use same-province data for imputation)
#------------------------------------


Data_select <- Data_2
# Fill missing data using linear interpolation, with boundary values extended; note that na.approx returns a single value for columns with only one observation, so a loop is used for adjustment.
n_country=length(unique(Data_select$城市))
n_year=length(unique(Data_select$年份))
n_column=ncol(Data_select)


# Step 1: linear interpolation
for (i in 1:n_country) {
  for (j in 4:n_column) {
    if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j]))==(n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=mean(as.numeric(unlist(Data_select[(i*n_year-n_year+1):(i*n_year),j])),na.rm = T) } 
    else { if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j])) < (n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=na.approx(Data_select[(i*n_year-n_year+1):(i*n_year),j],rule=2) }  } 
  }
}


# Step 2: replace indicators with all NA values by the annual mean
Data_temp=Data_select

Data_mean=Data_temp %>% group_by(年份) %>% summarise_all("mean",na.rm = TRUE) 


# Fill missing data
n_country=length(unique(Data_temp$城市))
n_year=length(unique(Data_temp$年份))
n_column=ncol(Data_temp)


for (i in 1:n_country) {
  for (j in 4:n_column) {
    if (sum(is.na(Data_temp[(i*n_year-n_year+1):(i*n_year),j]))==(n_year)) {Data_temp[(i*n_year-n_year+1):(i*n_year),j]=as.numeric(unlist(Data_mean[,j])) } 
  }
}

Data_2=Data_temp


####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####--Sequence transformation--######----------------------------------------------------------####################Important 01####################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################


#-------------
#01. Energy system
#-------------

# Energy consumption
Data_2$A_4_能源消费_化石能源=log(Data_2$A_4_能源消费_化石能源*1000)
Data_2$A_4_能源消费_电力=log(Data_2$A_4_能源消费_电力) 

# Carbon emissions
Data_2$B_2_人均碳排放=log(Data_2$B_2_人均碳排放*1000)

#-------------
#02. Readiness
#-------------

#------
#C. Economic variables
#------

Data_2$C_1_Economic_Devlelopment_人均GDP=log(Data_2$C_1_Economic_Devlelopment_人均GDP)

# #------
# #D
# #------
# Disabled code: log-transform average annual balance of net fixed assets per capita.
# Disabled code: log-transform deposits per capita.
# Disabled code: log-transform fixed-asset investment per capita.
# Disabled code: log-transform actually utilized foreign capital per capita.
# Disabled code: log-transform public finance income per capita.
# 
# #------
# #E
# #------

# Disabled code: log-transform science and technology expenditure per capita.
# 
# #------
# #F
# #------
# 
# Disabled code: log-transform education expenditure per capita.



#------------------------------------
####Step B_3. Bound selection
#------------------------------------
# Note: for now, use the simplest selection rule, i.e., the top 2.5% and bottom 2.5%.

Quan_mat=matrix(0,2,length(Data_2))

#97.5% and 2.5%
for (i in 4:length(Data_2)){
  Quan_mat[1,i]<-quantile(na.omit(Data_2[,i]), 0.975)
  Quan_mat[2,i]<-quantile(na.omit(Data_2[,i]), 0.025)
}

colnames(Quan_mat)=colnames(Data_2)

####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################    
####-Fine-tune upper and lower bounds-######----------------------------------------------------------####################Important 02####################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################

#-------------
#01. Energy system
#-------------


# Electricity structure
Quan_mat[1,5]=2.099740e-03 # level of Liupanshui City in 2015

# Energy intensity
#Quan_mat[1,6]=1.780830e-02 # level of Huaibei City in 2015


# #Fossil energy consumption
#Quan_mat[1,8]=11.559974 # level of Jiayuguan City in 2016
#Quan_mat[2,8]=4.310962 # level of Xiaogan City in 2003

# Electricity consumption
#Quan_mat[1,7]=6.678341 # level of Karamay City in 2015
#Quan_mat[2,7]=1.0424888 # level of Shantou City in 2015

# Carbon intensity
#Quan_mat[1,9]=5.760205e-02 # level of Liupanshui City in 2015:5.760205e-06

# Carbon emissions per capita
Quan_mat[1,10]=5.745011  # level of Jiayuguan City in 2015
#Quan_mat[2,10]=0.011175388  # level of Zhangjiajie City in 2015

# Air pollution
Quan_mat[1,11]=66.48037 # level of Pingdingshan City in 2015
#Quan_mat[2,11]=20 # 35 is the national threshold for the excellent category

#-------------
#02. Readiness
#-------------
#GDP Growth
Quan_mat[1,13]=28.60  #2005年呼和浩特市水平
#Quan_mat[2,13]=0


####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####-Export upper and lower bounds-######----------------------------------------------------------####################Important 03####################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################


# #Use bounds set in the iScience article:
# 
# Bound_select_benchmark <- read_excel("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index_Update/3_code/data/Bound_select_benchmark.xlsx")
# 
# Quan_mat=Bound_select_benchmark




#------------------------------------
####Step B_4. Normalization of indicator values ------------------------------------------------------------------------------------Adjust this section to change city coverage (city counts are 180 for >10 observations, 234 for >5 observations, 228 cities with data in 2010, and 282 cities in total)
#------------------------------------

# Select positive indicators (the previous "E_1_Innovation capability_Innovation and entrepreneurship index" has been removed) and extract indices
Positive_index=match(c("C_1_Economic_Devlelopment_人均GDP","C_1_Economic_Devlelopment_GDP增速" ,"C_2_Economic_Stucture_第三产业" ,  "D_1_资本存量_人均固定资产净余额" ,"D_1_资本存量_城市建设用地占比","D_1_资本存量_人均存款余额"  ,"D_2_投资_人均固定资产投资", "D_2_投资_人均当年使用外资", "D_3_财政能力_人均GDP",  "E_1_创新能力_新经济"   ,"E_1_创新能力_发明专利" , "E_2_科学支出","E_3_适应性技术能力_二氧化硫去除率",  "E_3_适应性技术能力_工业废水处理率"  ,"E_3_适应性技术能力_生活垃圾无害化处理率"   ,    "E_3_适应性技术能力_工业固体废物综合利用率"   ,  "F_1_科研能力_科研人员比例" ,  "F_1_科研能力_信息技术人员比例" , "F_2_师资培训水平_教育人员比例" , "F_2_师资培训水平_中职" ,   "F_2_师资培训水平_普高" , "F_3_教育水平_人均教育事业费用支出", "F_3_教育水平_大学生比例" ) ,names(Data_2))

# Select negative indicators and extract indices
Negative_index=match(c( "A_1_能源结构" ,"A_2_电力结构" ,"A_3_能源强度","A_4_能源消费_化石能源","A_4_能源消费_电力","B_1_碳强度" ,"B_2_人均碳排放"  ,"B_3_空气污染","C_2_Economic_Stucture_第二产业_采矿业人员占比" ) ,names(Data_2))


# Use indicator columns only
Data_Normal = Data_2;

# Positive indicators
for (i in Positive_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[2,i])/(Quan_mat[1,i]-Quan_mat[2,i]))*100
  }
}

# Negative indicators
for (i in Negative_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[1,i])/(Quan_mat[2,i]-Quan_mat[1,i]))*100
  }
}


# Top coding and threshold treatment (values above 100 are set to 100, and values below 0 are set to 0)

Data_Normal[Data_Normal<0] <- 0
Data_Normal[Data_Normal>100] <- 100

# Add identifier columns back
Data_Normal[,1:3]=Data_2[,1:3]

# Remove redundant variables
rm(Data_2,Data_temp,Data_mean,Data_select)


#------------------------------------
####Step B_5. Construct national and city indexes
#------------------------------------

##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############
##################-------------------------Select city scope---------------------------###############
##################------------------Baseline results, large sample (281 cities)-------------------###############
##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############

# Initial number of cities: 281
length(unique(Data_Normal$城市))

Data_select=Data_Normal

# #----------------------------------------------#
# #Code for selecting city scope based on A_1_Energy mix; city counts are 180 for >10 observations, 234 for >5 observations, 228 cities with data in 2010, and 282 cities in total.
# #----------------------------------------------#
# 
# Data_select = cbind(Data,temp=matrix(1, nrow(temp), 1)) # Construct a marker sequence with value 1
# names(Data_select)[length(Data_select)] <- "Units"                       # Name the marker column as Units
# Disabled code: use A_1_Energy mix as the benchmark to check available data.
# Disabled code: group by city and select cities above the threshold (>10 observations).

# #Number of cities
# Disabled code: count the number of unique selected cities.
# 
# #Select city indicators (Y indicator variables)
# Disabled code: filter normalized data to selected cities.


##################----------------------------------------------------------------###############
##################--------------------1. Construct national-level index--------------------------###############
##################----------------------------------------------------------------###############


#----------Construct national index------------#------------------------------------(one index for each year)

# Calculate annual aggregate data
Data_temp<-Data_select%>% group_by(年份)%>% summarise_all("mean",na.rm = TRUE)   

# #-----A Simple average-----
Results_National_1_detail=mutate(Data_temp, National_1 = rowMeans(Data_temp[,3:ncol(Data_temp)]))
# 
# #Construct simplified national-level results
Results_National_1=cbind(Results_National_1_detail[,1],Results_National_1_detail[,length(Results_National_1_detail)])
# 
# # #Save data
write.csv(Results_National_1_detail, "Results_National_1_detail.csv", fileEncoding = "GBK",row.names = F)


#-----B Theoretical framework aggregation----- (adjust as needed)

# Generate dimension scores
Results_National_2_A=(rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4
Results_National_2_B=(rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_National_2_C=(rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_National_2_D=(rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_National_2_E=(rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_National_2_F=(rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

# Calculate sub-indexes
Results_National_2_Energy=(Results_National_2_A+Results_National_2_B)/2
Results_National_2_Readiness=(Results_National_2_C+Results_National_2_D+Results_National_2_E+Results_National_2_F)/4

# Calculate overall index
Results_National_2_ETI=(Results_National_2_Energy+Results_National_2_Readiness)/2

# Merge data
Results_National_2_detail=cbind(Data_temp,Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI)


# Rename columns
colnames(Results_National_2_detail)[(ncol(Results_National_2_detail)-8):ncol(Results_National_2_detail)]=c("National_2_A","National_2_B","National_2_C","National_2_D","National_2_E","National_2_F","National_2_Energy","National_2_Readiness","National_2_ETI")

# #Construct simplified national-level results
# Results_National_2=cbind(Results_National_2_detail[,1],Results_National_2_detail[,length(Results_National_2_detail)])

# Remove redundant variables
rm(Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI,Data_temp)

#-----------------
# Save data
#-----------------
write.csv(Results_National_2_detail, "Results_National_2_detail_2.csv", fileEncoding = "GBK",row.names = F)


##################----------------------------------------------------------------###############
##################--------------------2.1. City-level median---------------------------###############
##################----------------------------------------------------------------###############

#----------Construct city index------------#------------------------------------(one index for each 5-year period)

# Define city-level data for easier processing
Data_select_城市 = Data_select

# Add period labels for median calculation
Data_select_城市=Data_select_城市 %>% 
  mutate(mark = case_when(
    年份 %in% 2003:2007 ~ 1, 
    年份 %in% 2008:2013 ~ 2,
    年份 %in% 2014:2016 ~ 3,
    年份 %in% 2017:2019 ~ 4,
    年份 %in% 2020:2022 ~ 5
  ))

# Calculate median
Data_temp<-Data_select_城市%>% group_by(城市,mark)%>% summarise_all("median",na.rm = TRUE)   


#-----B Theoretical framework aggregation----- (adjust as needed)

# Calculate median
Data_temp<-Data_select_城市%>% group_by(城市,mark)%>% summarise_all("median",na.rm = TRUE)   

# Generate dimension scores
Results_City_2_A= (rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4 #########---------------------------The -1 is used to facilitate variable selection; without -1, the city variable would be selected.
Results_City_2_B= (rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_City_2_C= (rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_City_2_D= (rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_City_2_E= (rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_City_2_F= (rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

# Calculate sub-indexes
Results_City_2_Energy=(Results_City_2_A+Results_City_2_B)/2
Results_City_2_Readiness=(Results_City_2_C+Results_City_2_D+Results_City_2_E+Results_City_2_F)/4

# Calculate index
Results_City_2_ETI=(Results_City_2_Energy+Results_City_2_Readiness)/2

# Merge data
Results_City_2_detail=cbind(Data_temp,Results_City_2_A,Results_City_2_B,Results_City_2_C,Results_City_2_D,Results_City_2_E,Results_City_2_F,Results_City_2_Energy,Results_City_2_Readiness,Results_City_2_ETI)

# Rename columns
colnames(Results_City_2_detail)[(ncol(Results_City_2_detail)-8):ncol(Results_City_2_detail)]=c("City_2_A","City_2_B","City_2_C","City_2_D","City_2_E","City_2_F","City_2_Energy","City_2_Readiness","City_2_ETI")

# #Construct simplified city-level results
# Results_City_2=cbind(Results_City_2_detail[,1:4],Results_City_2_detail[,length(Results_City_2_detail)])

# #-----------------
# #Convert Chinese city names to English
# #-----------------
# 
# for (i in 1:dim(Results_City_2_detail)[1]) {
# Disabled code: convert Chinese city names to English names using city_name_list.
# }

# Save data
write.csv(Results_City_2_detail, "Results_City_2_detail.csv", fileEncoding = "GBK",row.names = F)

##################----------------------------------------------------------------###############
##################--------------------2.2. City-level annual results--------------------------###############
##################----------------------------------------------------------------###############



#----------Construct city index------------#------------------------------------(annual index)

# Define city-level data for easier processing
Data_select_城市 = Data_select



#-----B Theoretical framework aggregation----- (adjust as needed)

# Use annual city-level data
Data_temp<-Data_select_城市

# Generate dimension scores
Results_City_2_A= (rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4 #########---------------------------The -1 is used to facilitate variable selection; without -1, the city variable would be selected.
Results_City_2_B= (rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_City_2_C= (rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_City_2_D= (rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_City_2_E= (rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_City_2_F= (rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

# Calculate sub-indexes
Results_City_2_Energy=(Results_City_2_A+Results_City_2_B)/2
Results_City_2_Readiness=(Results_City_2_C+Results_City_2_D+Results_City_2_E+Results_City_2_F)/4

# Calculate index
Results_City_2_ETI=(Results_City_2_Energy+Results_City_2_Readiness)/2

# Merge data
Results_City_2_detail=cbind(Data_temp,Results_City_2_A,Results_City_2_B,Results_City_2_C,Results_City_2_D,Results_City_2_E,Results_City_2_F,Results_City_2_Energy,Results_City_2_Readiness,Results_City_2_ETI)

# Rename columns
colnames(Results_City_2_detail)[(ncol(Results_City_2_detail)-8):ncol(Results_City_2_detail)]=c("City_2_A","City_2_B","City_2_C","City_2_D","City_2_E","City_2_F","City_2_Energy","City_2_Readiness","City_2_ETI")

# #Construct simplified city-level results
# Results_City_2=cbind(Results_City_2_detail[,1:4],Results_City_2_detail[,length(Results_City_2_detail)])

# #-----------------
# #Convert Chinese city names to English
# #-----------------
# 
# for (i in 1:dim(Results_City_2_detail)[1]) {
# Disabled code: convert Chinese city names to English names using city_name_list.
# }

# Save data
write.csv(Results_City_2_detail, "Results_City_2_disagreegate.csv", fileEncoding = "GBK",row.names = F)
