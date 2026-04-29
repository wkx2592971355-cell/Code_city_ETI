###-----------------------------------------------------------####
###-----------------------------------------------------------####
####-------------------ETI: 城级数据构建----------------------####
###-----------------------------------------------------------####
###-----------------------------------------------------------####
rm(list=ls())
library(dplyr)
library(dplyr)
library(zoo)
# 弹出文件选择对话框
file_path <- file.choose()
# 导入 CSV 文件
Data <- read.csv(file_path)
###-----------------------------
#指数列表： 
###-----------------------------

#---------------
#01. System Performance score
#---------------

####A. Energy System Structure：
#A_1_能源结构: 煤炭占比
#A_2_电力结构: 发电能耗/用电量
#A_3_能源强度: 能耗/GDP
#A_4_能源消费：1. 人均电力消费； 2. 人均能源消费； 

####B. Environmental Sustainability：
#B_1_碳强度: 
#B_2_人均碳排放: 
#B_3_空气污染: PM 2.5

#---------------
#02. System Performance score
#---------------

####C. Economic Development：  
#C_1_Economic_Growth: 1. 人均GDP； 2. GDP Growth
#C_2_Economic_Structure: 1. 第二产业采矿比例; 2. 第三产业;

####D. Capital & investment：  
#D_1_资本存量: 1. 人均固定资产净值年平均余额_全市_万元 2. 城市建设用地占城市建设用地占市辖区面积比重_百分比；3. 人均存款余额;
#D_2_投资: 1. 人均固定资产投资总额_全市_万元; 2. 人均当年实际使用外资；
#D_3_财政能力: 人均财政收入；

####E. Technology capability：
#E_1_创新能力: 1. 新经济：互联网接入比例; 2. 人均绿色发明+绿色专利
#E_2_科学支出: 人均科技支出
#E_3_适应性技术能力: 1. 二氧化硫去除率； 2 工业废水去除率;  3 生活垃圾无害化处理率;  4 一般工业固体废物综合利用率;

####F. Human Capital（Quality of Education）：
#F_1_教育水平: 大学人口占比
#F_2_师资培训水平:  1 教育从业比例； 2 中职； 3 普高教师；
#F_3_科研能力：1. R&D人员比例 2. 信息技术； 3. 人均科学教育支出； 


###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###
############################################### Step_2: 数据处理################################################### 
###-------------------------------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------------------------------###

#------------------------------------
#------------------------------------
####A. 初步查验数据
#------------------------------------
#------------------------------------

# #初始城市数量---------------281个
# 以Carbon_Intensity为基准,大于阈值10的城市集为219，大于阈值5的城市集为249个，大于阈值1的城市集为261个；2010年有数据的为232
# Check the number of values
# 1. 添加计数列 + 去缺失
Data_unit <- Data %>%
  mutate(Units = 1) %>%
  filter(!is.na(A_1_能源结构))
# 2. 计算每个城市的观测数
city_count <- Data_unit %>%
  group_by(城市) %>%
  summarise(n_obs = n())
# 3. 不同阈值下的城市数量
n_total  <- length(unique(Data$城市))                     # 总城市数
n_gt1    <- sum(city_count$n_obs > 1)                     # >1
n_gt5    <- sum(city_count$n_obs > 5)                     # >5
n_gt10   <- sum(city_count$n_obs > 10)                    # >10
# 4. 2010年有数据的城市数
n_2010 <- Data_unit %>%
  filter(年份 == 2010) %>%
  summarise(n_city = n_distinct(城市)) %>%
  pull(n_city)
# 5. 输出结果
cat("总城市数：", n_total, "\n")
cat(">1期城市数：", n_gt1, "\n")
cat(">5期城市数：", n_gt5, "\n")
cat(">10期城市数：", n_gt10, "\n")
cat("2010年有数据城市数：", n_2010, "\n")
# 6. 清理
rm(city_count, Data_unit)

#------------------------------------
#------------------------------------
####B. 指数构建
#------------------------------------
#------------------------------------

#------------------------------------
####Step B_0. 指数的选择 --------------------09_27版本之后，不需要这一步骤了
#------------------------------------

# #移除没有数据的列（即该列的所值均为NA）
# Data_temp <- Data[,colSums(is.na(Data))<nrow(Data)]
# 
# sapply(Data_temp, class)
# 
# # #Change to numeric
# # for (i in 4:length(Data_temp)){
# #   Data_temp[,i]<-as.numeric(Data_temp[,i])
# # }
# 
# ##暂时不用到最后三个数据
# Data_temp=Data_temp[,1:(ncol(Data_temp)-3)] 
# 
# #定义新变量
# Data_2=Data_temp
# 
# #去除冗余变量
# rm(Data_temp) 

#------------------------------------
####Step B_0. 时间范围的选择 
#------------------------------------

Data_2=Data

#01. 时间变量的选择（数据更新到2022）

Data_2=filter(Data_2,年份<2023) 


#------------------------------------
####Step B_1. 空间范围的选择 
#------------------------------------

#2019年取消了莱芜市（初始数据就去除掉了，所以这步省去）

#Data_2=filter(Data_2,!城市=="莱芜市") 

#------------------------------------
####Step B_2. 数据填充；暂时使用最简单的方式（如果有一个数据，就用这个数据填充；如果没有数据，就放均值；未来可以用同省数据填充等方式）
#------------------------------------


Data_select <- Data_2
#填充数据，标准为线性填充，边界值由边界值延展；注意其中na.approx函数对只有一个数据的列只会return一个数值，所以这边要写loop调整；
n_country=length(unique(Data_select$城市))
n_year=length(unique(Data_select$年份))
n_column=ncol(Data_select)


#步骤一：线性填充
for (i in 1:n_country) {
  for (j in 4:n_column) {
    if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j]))==(n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=mean(as.numeric(unlist(Data_select[(i*n_year-n_year+1):(i*n_year),j])),na.rm = T) } 
    else { if (sum(is.na(Data_select[(i*n_year-n_year+1):(i*n_year),j])) < (n_year-1)) { Data_select[(i*n_year-n_year+1):(i*n_year),j]=na.approx(Data_select[(i*n_year-n_year+1):(i*n_year),j],rule=2) }  } 
  }
}


#步骤二：将某一指标全为NA的，用均值代替
Data_temp=Data_select

Data_mean=Data_temp %>% group_by(年份) %>% summarise_all("mean",na.rm = TRUE) 


#填充数据
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
####--序列变换--######----------------------------------------------------------####################重要01####################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################


#-------------
#01. Energy system
#-------------

#能源消费
Data_2$A_4_能源消费_化石能源=log(Data_2$A_4_能源消费_化石能源*1000)
Data_2$A_4_能源消费_电力=log(Data_2$A_4_能源消费_电力) 

#碳排放
Data_2$B_2_人均碳排放=log(Data_2$B_2_人均碳排放*1000)

#-------------
#02. Readiness
#-------------

#------
#C. 经济变量
#------

Data_2$C_1_Economic_Devlelopment_人均GDP=log(Data_2$C_1_Economic_Devlelopment_人均GDP)

# #------
# #D
# #------
# Data_2$D_1_资本存量_人均固定资产净余额=log(Data_2$D_1_资本存量_人均固定资产净余额)
# Data_2$D_1_资本存量_人均存款余额=log(Data_2$D_1_资本存量_人均存款余额)
# Data_2$D_2_投资_人均固定资产投资=log(Data_2$D_2_投资_人均固定资产投资)
# Data_2$D_2_投资_人均当年使用外资=log(Data_2$D_2_投资_人均当年使用外资)
# Data_2$D_3_财政能力_人均GDP=log(Data_2$D_3_财政能力_人均GDP)
# 
# #------
# #E
# #------

# Data_2$E_2_科学支出=log(Data_2$E_2_科学支出)
# 
# #------
# #F
# #------
# 
# Data_2$F_3_教育水平_人均教育事业费用支出=log(Data_2$F_3_教育水平_人均教育事业费用支出)



#------------------------------------
####Step B_3. 上下限的选择（bound selection）
#------------------------------------
#注：暂时我们用最简单的选法，即最高的2.5%和最低的2.5%

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
####-上下界精调-######----------------------------------------------------------####################重要02####################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################

#-------------
#01. Energy system
#-------------


#电力结构
Quan_mat[1,5]=2.099740e-03 #2015年六盘水市水平

#能源强度
#Quan_mat[1,6]=1.780830e-02 #2015年淮北市水平


# #化石能源消费
#Quan_mat[1,8]=11.559974 #2016年嘉峪关市水平
#Quan_mat[2,8]=4.310962 #2003年孝感水平

#电力消费
#Quan_mat[1,7]=6.678341 #2015年克拉玛依市水平
#Quan_mat[2,7]=1.0424888 #2015年汕头市水平

#碳强度
#Quan_mat[1,9]=5.760205e-02 #2015年六盘水市水平:5.760205e-06

#人均碳排放
Quan_mat[1,10]=5.745011  #2015年嘉峪关市水平
#Quan_mat[2,10]=0.011175388  #2015年张家界市水平

#空气污染
Quan_mat[1,11]=66.48037 #2015年平顶山市水平
#Quan_mat[2,11]=20 #35为国家定义的优

#-------------
#02. Readiness
#-------------
#GDP Growth
Quan_mat[1,13]=28.60  #2005年呼和浩特市水平
#Quan_mat[2,13]=0


####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####-输出上下界-######----------------------------------------------------------####################重要03####################
####------------######                                                          ##############################################
####------------######                                                          ##############################################
####------------######                                                          ##############################################


# #用iScience文章设置上下界： 
# 
# Bound_select_benchmark <- read_excel("C:/Users/TJSEM/Dropbox/00000_1_Idea List/Idea_7_Energy_transition_Index_Update/3_code/data/Bound_select_benchmark.xlsx")
# 
# Quan_mat=Bound_select_benchmark




#------------------------------------
####Step B_4. 数据标准化（normalization of indicator values）------------------------------------------------------------------------------------调整这里来调整数据覆盖城市范围（#遴选大于阈值10的城市集为180，大于阈值5的城市集为234；2010年有数据的为228；总城市数量包含282个）
#------------------------------------

#选择正向指标（已删去"E_1_创新能力_创新创业指数" ），并提取编号
Positive_index=match(c("C_1_Economic_Devlelopment_人均GDP","C_1_Economic_Devlelopment_GDP增速" ,"C_2_Economic_Stucture_第三产业" ,  "D_1_资本存量_人均固定资产净余额" ,"D_1_资本存量_城市建设用地占比","D_1_资本存量_人均存款余额"  ,"D_2_投资_人均固定资产投资", "D_2_投资_人均当年使用外资", "D_3_财政能力_人均GDP",  "E_1_创新能力_新经济"   ,"E_1_创新能力_发明专利" , "E_2_科学支出","E_3_适应性技术能力_二氧化硫去除率",  "E_3_适应性技术能力_工业废水处理率"  ,"E_3_适应性技术能力_生活垃圾无害化处理率"   ,    "E_3_适应性技术能力_工业固体废物综合利用率"   ,  "F_1_科研能力_科研人员比例" ,  "F_1_科研能力_信息技术人员比例" , "F_2_师资培训水平_教育人员比例" , "F_2_师资培训水平_中职" ,   "F_2_师资培训水平_普高" , "F_3_教育水平_人均教育事业费用支出", "F_3_教育水平_大学生比例" ) ,names(Data_2))

#选择负向指标，并提取编号
Negative_index=match(c( "A_1_能源结构" ,"A_2_电力结构" ,"A_3_能源强度","A_4_能源消费_化石能源","A_4_能源消费_电力","B_1_碳强度" ,"B_2_人均碳排放"  ,"B_3_空气污染","C_2_Economic_Stucture_第二产业_采矿业人员占比" ) ,names(Data_2))


# 只取指标列
Data_Normal = Data_2;

#正向指标
for (i in Positive_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[2,i])/(Quan_mat[1,i]-Quan_mat[2,i]))*100
  }
}

#负向指标
for (i in Negative_index){
  for (j in 1:nrow(Data_2)){
    Data_Normal[j,i]<-(( Data_2[j,i]-Quan_mat[1,i])/(Quan_mat[2,i]-Quan_mat[1,i]))*100
  }
}


#top coding，阈值处理（超过100的为100，小于0的为0）

Data_Normal[Data_Normal<0] <- 0
Data_Normal[Data_Normal>100] <- 100

#加回表头
Data_Normal[,1:3]=Data_2[,1:3]

#去除冗余变量
rm(Data_2,Data_temp,Data_mean,Data_select)


#------------------------------------
####Step B_5. 国家&城市指数的构建
#------------------------------------

##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############
##################-------------------------选择城市范围---------------------------###############
##################------------------基准结果，大样本(281个城市)-------------------###############
##################----------------------------------------------------------------###############
##################----------------------------------------------------------------###############

#初始城市数量---------------281个
length(unique(Data_Normal$城市))

Data_select=Data_Normal

# #----------------------------------------------#
# #按照A_1_能源结构变量为准则，选择城市范围的code# #大于阈值（10）的城市集城市数量----------------------------------------------- #遴选大于阈值10的城市集为180，大于阈值5的城市集为234；2010年有数据的为228；总共有282个城市。
# #----------------------------------------------#
# 
# Data_select = cbind(Data,temp=matrix(1, nrow(temp), 1)) #构建为1的标记序列
# names(Data_select)[length(Data_select)] <- "Units"                       #命名为Units
# Data_select <- subset(Data_select, !is.na(Data_select$A_1_能源结构)) #以Carbon_Intensity为基准，查看available的数据
# Data_select<-Data_select %>% group_by(城市)%>% filter(sum(Units)>10)     #遴选大于阈值（10）的城市集

# #Number of cities
# length(unique(Data_select$城市))
# 
# #选择城市指标（Y指标变量）
# Data_select<- filter(Data_Normal,城市 %in% Data_select$城市)


##################----------------------------------------------------------------###############
##################--------------------1.国家层面指数构建--------------------------###############
##################----------------------------------------------------------------###############


#----------国家指数的构建------------#------------------------------------（每1年一个指数）

#计算年度加总数据
Data_temp<-Data_select%>% group_by(年份)%>% summarise_all("mean",na.rm = TRUE)   

# #-----A 简单平均-----
Results_National_1_detail=mutate(Data_temp, National_1 = rowMeans(Data_temp[,3:ncol(Data_temp)]))
# 
# #构建精简国家层面结果
Results_National_1=cbind(Results_National_1_detail[,1],Results_National_1_detail[,length(Results_National_1_detail)])
# 
# # #保存数据
write.csv(Results_National_1_detail, "Results_National_1_detail.csv", fileEncoding = "GBK",row.names = F)


#-----B 理论框架加总----- （根据具体情况，调整）

#生成子指标
Results_National_2_A=(rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4
Results_National_2_B=(rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_National_2_C=(rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_National_2_D=(rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_National_2_E=(rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_National_2_F=(rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

#计算子指数
Results_National_2_Energy=(Results_National_2_A+Results_National_2_B)/2
Results_National_2_Readiness=(Results_National_2_C+Results_National_2_D+Results_National_2_E+Results_National_2_F)/4

#计算总指数
Results_National_2_ETI=(Results_National_2_Energy+Results_National_2_Readiness)/2

#合并数据
Results_National_2_detail=cbind(Data_temp,Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI)


#修改名称
colnames(Results_National_2_detail)[(ncol(Results_National_2_detail)-8):ncol(Results_National_2_detail)]=c("National_2_A","National_2_B","National_2_C","National_2_D","National_2_E","National_2_F","National_2_Energy","National_2_Readiness","National_2_ETI")

# #构建精简国家层面结果
# Results_National_2=cbind(Results_National_2_detail[,1],Results_National_2_detail[,length(Results_National_2_detail)])

#去除冗余变量
rm(Results_National_2_A,Results_National_2_B,Results_National_2_C,Results_National_2_D,Results_National_2_E,Results_National_2_F,Results_National_2_Energy,Results_National_2_Readiness,Results_National_2_ETI,Data_temp)

#-----------------
#保存数据
#-----------------
write.csv(Results_National_2_detail, "Results_National_2_detail_2.csv", fileEncoding = "GBK",row.names = F)


##################----------------------------------------------------------------###############
##################--------------------2.1.城市层面media---------------------------###############
##################----------------------------------------------------------------###############

#----------城市指数的构建------------#------------------------------------（每5年一个指数）

#定义城市数据，方便操作
Data_select_城市 = Data_select

#标识数据，以便计算中位数
Data_select_城市=Data_select_城市 %>% 
  mutate(mark = case_when(
    年份 %in% 2003:2007 ~ 1, 
    年份 %in% 2008:2013 ~ 2,
    年份 %in% 2014:2016 ~ 3,
    年份 %in% 2017:2019 ~ 4,
    年份 %in% 2020:2022 ~ 5
  ))

#计算中位数
Data_temp<-Data_select_城市%>% group_by(城市,mark)%>% summarise_all("median",na.rm = TRUE)   


#-----B 理论框架加总----- （根据具体情况，调整）

#计算中位数
Data_temp<-Data_select_城市%>% group_by(城市,mark)%>% summarise_all("median",na.rm = TRUE)   

#生成子指标
Results_City_2_A= (rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4 #########---------------------------负一是为了遴选变量的方便；不包含负一，会选到地方这个变量
Results_City_2_B= (rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_City_2_C= (rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_City_2_D= (rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_City_2_E= (rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_City_2_F= (rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

#计算子指数
Results_City_2_Energy=(Results_City_2_A+Results_City_2_B)/2
Results_City_2_Readiness=(Results_City_2_C+Results_City_2_D+Results_City_2_E+Results_City_2_F)/4

#计算指数
Results_City_2_ETI=(Results_City_2_Energy+Results_City_2_Readiness)/2

#合并数据
Results_City_2_detail=cbind(Data_temp,Results_City_2_A,Results_City_2_B,Results_City_2_C,Results_City_2_D,Results_City_2_E,Results_City_2_F,Results_City_2_Energy,Results_City_2_Readiness,Results_City_2_ETI)

#修改名称
colnames(Results_City_2_detail)[(ncol(Results_City_2_detail)-8):ncol(Results_City_2_detail)]=c("City_2_A","City_2_B","City_2_C","City_2_D","City_2_E","City_2_F","City_2_Energy","City_2_Readiness","City_2_ETI")

# #构建精简城市层面结果
# Results_City_2=cbind(Results_City_2_detail[,1:4],Results_City_2_detail[,length(Results_City_2_detail)])

# #-----------------
# #将中文城市名称转化为英文
# #-----------------
# 
# for (i in 1:dim(Results_City_2_detail)[1]) {
#   Results_City_2_detail$城市[i]<-city_name_list$English_Name[city_name_list$Chinese_Name==Results_City_2_detail$城市[i]]
# }

#保存数据
write.csv(Results_City_2_detail, "Results_City_2_detail.csv", fileEncoding = "GBK",row.names = F)

##################----------------------------------------------------------------###############
##################--------------------2.1.城市层面每年--------------------------###############
##################----------------------------------------------------------------###############



#----------城市指数的构建------------#------------------------------------（每5年一个指数）

#定义城市数据，方便操作
Data_select_城市 = Data_select



#-----B 理论框架加总----- （根据具体情况，调整）

#计算中位数
Data_temp<-Data_select_城市

#生成子指标
Results_City_2_A= (rowMeans(select(Data_temp[,-1], starts_with("A_1")))+rowMeans(select(Data_temp[,-1], starts_with("A_2")))+rowMeans(select(Data_temp[,-1], starts_with("A_3")))+rowMeans(select(Data_temp[,-1], starts_with("A_4"))))/4 #########---------------------------负一是为了遴选变量的方便；不包含负一，会选到地方这个变量
Results_City_2_B= (rowMeans(select(Data_temp[,-1], starts_with("B_1")))+rowMeans(select(Data_temp[,-1], starts_with("B_2")))+rowMeans(select(Data_temp[,-1], starts_with("B_3"))))/3
Results_City_2_C= (rowMeans(select(Data_temp[,-1], starts_with("C_1")))+rowMeans(select(Data_temp[,-1], starts_with("C_2"))))/2
Results_City_2_D= (rowMeans(select(Data_temp[,-1], starts_with("D_1")))+rowMeans(select(Data_temp[,-1], starts_with("D_2")))+rowMeans(select(Data_temp[,-1], starts_with("D_3"))))/3
Results_City_2_E= (rowMeans(select(Data_temp[,-1], starts_with("E_1")))+rowMeans(select(Data_temp[,-1], starts_with("E_2")))+rowMeans(select(Data_temp[,-1], starts_with("E_3"))))/3
Results_City_2_F= (rowMeans(select(Data_temp[,-1], starts_with("F_1")))+rowMeans(select(Data_temp[,-1], starts_with("F_2")))+rowMeans(select(Data_temp[,-1], starts_with("F_3"))))/3

#计算子指数
Results_City_2_Energy=(Results_City_2_A+Results_City_2_B)/2
Results_City_2_Readiness=(Results_City_2_C+Results_City_2_D+Results_City_2_E+Results_City_2_F)/4

#计算指数
Results_City_2_ETI=(Results_City_2_Energy+Results_City_2_Readiness)/2

#合并数据
Results_City_2_detail=cbind(Data_temp,Results_City_2_A,Results_City_2_B,Results_City_2_C,Results_City_2_D,Results_City_2_E,Results_City_2_F,Results_City_2_Energy,Results_City_2_Readiness,Results_City_2_ETI)

#修改名称
colnames(Results_City_2_detail)[(ncol(Results_City_2_detail)-8):ncol(Results_City_2_detail)]=c("City_2_A","City_2_B","City_2_C","City_2_D","City_2_E","City_2_F","City_2_Energy","City_2_Readiness","City_2_ETI")

# #构建精简城市层面结果
# Results_City_2=cbind(Results_City_2_detail[,1:4],Results_City_2_detail[,length(Results_City_2_detail)])

# #-----------------
# #将中文城市名称转化为英文
# #-----------------
# 
# for (i in 1:dim(Results_City_2_detail)[1]) {
#   Results_City_2_detail$城市[i]<-city_name_list$English_Name[city_name_list$Chinese_Name==Results_City_2_detail$城市[i]]
# }

#保存数据
write.csv(Results_City_2_detail, "Results_City_2_disagreegate.csv", fileEncoding = "GBK",row.names = F)
