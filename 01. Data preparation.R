library(dplyr)

################################################
# 1 基础变量
################################################

Pop <- Data_raw_1$年末户籍人口_万人_全市
GDP <- Data_raw_1$地区生产总值_当年价格_亿元_全市 * 10000

################################################
# 2 新建 data_select
################################################

data_select <- data.frame(
  城市 = Data_raw_1$城市,
  年份 = Data_raw_1$year,
  城市代码 = Data_raw_1$city_code
)

################################################
# A 能源系统（缺失 → NA）
################################################

data_select$A_1_能源结构 <- 
  Data_raw_1$能源结构
data_select$A_2_电力结构 <- 
  Data_raw_1$发电能耗/Data_raw_1$全年用电总量_市辖区_万千瓦时
data_select$A_3_能源强度 <- 
  Data_raw_1$Scope1_化石能源燃烧_Total/GDP* 10000
data_select$A_4_能源消费_化石能源 <-
  Data_raw_1$Scope1_化石能源燃烧_Total/Pop
data_select$A_4_能源消费_电力 <- 
  Data_raw_1$全年用电总量_市辖区_万千瓦时/ Pop

################################################
# B 碳排放
################################################

data_select$B_1_碳强度 <- 
  Data_raw_1$各城市排放量/GDP* 10000
data_select$B_2_人均碳排放 <- 
  Data_raw_1$各城市排放量/Pop

data_select$B_3_空气污染 <- Data_raw_1$PM2.5

################################################
# C Economic Development
################################################

data_select$C_1_Economic_Devlelopment_人均GDP <- GDP * .0001  / Pop

data_select$C_1_Economic_Devlelopment_GDP增速 <- 
  Data_raw_1$地区生产总值增长率_百分比_全市

data_select$C_2_Economic_Stucture_第二产业_采矿业人员占比 <- 
  Data_raw_1$第二产业_采矿业从业人员_人_全市* 10000 / Pop
######因为我的数据四舍五入到万，这里为了能对上所以* 10000

data_select$C_2_Economic_Stucture_第三产业 <- 
  Data_raw_1$第三产业占地区生产总值的比重_全市

################################################
# D Capital & Investment
################################################

data_select$D_1_资本存量_人均固定资产净余额 <- 
  Data_raw_1$固定资产净值年平均余额_全市_万元 / Pop

data_select$D_1_资本存量_城市建设用地占比 <-
  Data_raw_1$城市建设用地占市区面积比重_百分比_市辖区*100

data_select$D_1_资本存量_人均存款余额 <- 
  Data_raw_1$年末金融机构人民币各项存款余额_全市_万元 / Pop

data_select$D_2_投资_人均固定资产投资 <- 
  Data_raw_1$固定资产投资_万元_全市 / Pop

data_select$D_2_投资_人均当年使用外资 <- 
  Data_raw_1$外商直接投资额_百万美元 * 100 / Pop

data_select$D_3_财政能力_人均GDP <- 
  Data_raw_1$地方一般公共预算收入_万元_全市 / Pop

################################################
# E Technology capability
################################################

data_select$E_1_创新能力_新经济 <- 
  Data_raw_1$互联网宽带接入用户数_万户_全市 / Pop

data_select$E_1_创新能力_发明专利 <- 
  (
    Data_raw_1$当年申请的绿色发明数量 / Pop +
      Data_raw_1$当年申请的绿色实用新型数量 / Pop
  )/2

data_select$E_2_科学支出 <- 
  Data_raw_1$科学技术支出_万元_全市 / Pop

################################################
# E3 适应性技术能力
################################################

data_select$E_3_适应性技术能力_二氧化硫去除率 <- 
  Data_raw_1$工业二氧化硫去除量_吨_全市 /
  (Data_raw_1$工业二氧化硫去除量_吨_全市 +
     Data_raw_1$工业二氧化硫排放量_吨_全市)

data_select$E_3_适应性技术能力_工业废水处理率 <- 
  Data_raw_1$污水处理厂集中处理率_百分比_全市

data_select$E_3_适应性技术能力_生活垃圾无害化处理率 <- 
  Data_raw_1$生活垃圾无害化处理率_百分比_全市

data_select$E_3_适应性技术能力_工业固体废物综合利用率 <- 
  Data_raw_1$一般工业固体废物综合利用率_百分比_全市

################################################
# F Human Capital
################################################

data_select$F_1_科研能力_科研人员比例 <- 
  Data_raw_1$科研综合技术服务业从业人员数_全市_人2 / Pop

data_select$F_1_科研能力_信息技术人员比例 <- 
  Data_raw_1$第三产业_信息传输计算机服务和软件业_人_全市* 10000 / Pop
######因为我的数据四舍五入到万，这里为了能对上所以* 10000

data_select$F_2_师资培训水平_教育人员比例 <- 
  Data_raw_1$第三产业_教育_人_全市* 10000 / Pop
######因为我的数据四舍五入到万，这里为了能对上所以* 10000

data_select$F_2_师资培训水平_中职 <- 
  Data_raw_1$中等职业教育学校专任教师数_人_全市 / Pop

data_select$F_2_师资培训水平_普高 <- 
  Data_raw_1$普通高等学校专任教师数_人_全市 / Pop

data_select$F_3_教育水平_人均教育事业费用支出 <- 
  Data_raw_1$教育支出_万元_全市 / Pop

data_select$F_3_教育水平_大学生比例 <- 
  Data_raw_1$普通本专科在校学生人数 / Pop

################################################
# 排序
################################################

data_select <- data_select %>% 
  arrange(城市, 年份)

str(data_select)
#  保存
write.csv(data_select, "data_select.csv", row.names = FALSE)
#存为excel
library(writexl)
write_xlsx(data_select, "data_select.xlsx")
