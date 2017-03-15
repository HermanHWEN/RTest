library(stringr)
library(memoise)
library(xlsx)
library
# 百度
baidu_key_fun=memoise(function(baidu_path,header0,seq0,quote0){
  data1=read.csv2(baidu_path,header0,seq0,quote0,
                  skip = 7,encoding = 'UTF-8',fileEncoding ='GB2312')
  data=data1[c("日期","关键词","推广单元","推广计划",
               "展现","点击","消费","点击率","平均点击价格","平均排名")]
  data$展现=as.numeric(data$展现)
  data$点击=as.numeric(data$点击)
  data$消费=as.numeric(data$消费)
  data$平均排名=as.character(data$平均排名)
  data$平均排名[data$平均排名=="-"]=0
  data$平均排名=as.numeric(data$平均排名)
  data$总排名=data$展现*data$平均排名
  data_sum=aggregate(data[c("展现","点击","消费","总排名")],
                     by=list(关键词=data$关键词,
                                推广单元=data$推广单元,
                                推广计划=data$推广计划),
                     FUN="sum")
  data_sum$点击率=with(data_sum,点击/展现)
  data_sum$平均点击单价=with(data_sum,消费/点击)
  data_sum$平均排名=with(data_sum,总排名/展现)
  data_sum$总排名=NULL
  return(data_sum)
})

baidu_acc_fun=memoise(function(baidu_path,header0,sep0,quote0,short=TRUE){
  data1=read.csv2(baidu_path,header0,sep0,quote0,
                  stringsAsFactors=FALSE,encoding = 'UTF-8',fileEncoding ='GB2312')
  name=c("关键词出价","关键词计算机质量度","关键词移动质量度",
         "关键词匹配模式","关键词访问URL","关键词移动访问URL")
  if(short==TRUE){name=sub("关键词","",name)}
  data=data1[c("推广计划名称","推广单元名称","关键词名称",name)]
  names(data)=c("推广计划","推广单元","关键词","关键词出价",
                "关键词计算机质量度","关键词移动质量度","关键词匹配模式",
                "关键词访问URL","关键词移动访问URL")
  data=data[!(is.na(data$关键词访问URL) & is.na(data$关键词移动访问URL)),]
  return(data)
})

# 搜狗
sougou_key_fun=memoise(function(sougou_path,header0,sep0,quote0){
  data1=read.csv2(sougou_path,header0,seq0,quote0
                  ,encoding = 'UTF-8',fileEncoding ='GB2312')
  data=data1[c("日期","推广计划","推广组","关键词",
               "展示数","点击数","消耗","点击率","点击均价","关键词平均排名")]
  names(data)=c("日期","推广计划","推广单元","关键词",
                "展现","点击","消费","点击率","平均点击价格","平均排名")
  data=data[-1,]
  data$展现=as.numeric(data$展现)
  data$点击=as.numeric(data$点击)
  data$消费=as.numeric(data$消费)
  data$平均排名=as.character(data$平均排名)
  data$平均排名[data$平均排名=="--"]=0
  data$平均排名=as.numeric(data$平均排名)
  data$总排名=data$展现*data$平均排名
  data_sum=aggregate(data[c("展现","点击","消费","总排名")],
                     by=list(关键词=data$关键词,
                                推广单元=data$推广单元,
                                推广计划=data$推广计划),
                     FUN="sum")
  data_sum$点击率=with(data_sum,点击/展现)
  data_sum$平均点击单价=with(data_sum,消费/点击)
  data_sum$平均排名=with(data_sum,总排名/展现)
  data_sum$总排名=NULL
  return(data_sum)
})
# 加载搜狗账户
sougou_acc_fun=memoise(function(sougou_path,header0,sep0,quote0){
  data1=read.csv2(sougou_path,header0,sep0,quote0,
                  stringsAsFactors=FALSE,encoding = 'UTF-8',fileEncoding ='GB2312')
  
  data=data1[c("推广计划名称","推广组名称","关键词名称",
               "匹配方式","出价","访问url","移动访问url","计算机质量度",
               "移动质量度")]
  names(data)=c("推广计划","推广单元","关键词",
                "关键词匹配模式","关键词出价","关键词访问URL",
                "关键词移动访问URL","关键词质量度","关键词移动质量度")
  data=data[!(is.na(data$关键词访问URL) & is.na(data$关键词移动访问URL)),]
  return(data)
})

# 加载360关键词
ssl_key_fun=memoise(function(sll_path,header0,seq0,quote0){
  data1=read.csv2(sll_path,header0,seq0,quote0,
                  encoding = 'UTF-8',fileEncoding ='GB2312')
  data=data1[c("推广计划","推广组","关键词",
               "展示次数","点击次数","总费用","点击率",
               "平均每次点击费用","平均排名.计算机.")]
  names(data)=c("推广计划","推广单元","关键词",
                "展现","点击","消费","点击率","平均点击价格","平均排名")
  data$展现=as.numeric(data$展现)
  data$点击=as.numeric(data$点击)
  data$消费=as.numeric(data$消费)
  data$总排名=data$展现*data$平均排名
  data_sum=aggregate(data[c("展现","点击","消费","总排名")],
                     by=list(关键词=data$关键词,
                                推广单元=data$推广单元,
                                推广计划=data$推广计划),
                     FUN="sum")
  data_sum$点击率=with(data_sum,点击/展现)
  data_sum$平均点击单价=with(data_sum,消费/点击)
  data_sum$平均排名=with(data_sum,总排名/展现)
  data_sum$总排名=NULL
  return(data_sum)
})

# 加载360账户
sll_acc_fun=memoise(function(sll_path,header0,sep0,quote0){
  data1=read.csv2(sll_path,header0,sep0,quote0,
                  stringsAsFactors=FALSE,encoding = 'UTF-8',fileEncoding ='GB2312')
  
  data=data1[c("推广计划名称","推广组名称","关键词",
               "关键词匹配模式","关键词出价","关键词链接网址",
               "关键词移动链接网址","关键词质量度","关键词移动质量度")]
  names(data)=c("推广计划","推广单元","关键词",
                "关键词匹配模式","关键词出价","关键词访问URL",
                "关键词移动访问URL","关键词质量度","关键词移动质量度")
  # data=data1[c("推广计划名称","推广组名称","关键词","关键词匹配模式","关键词出价","关键词链接网址")]
  # names(data)=c("推广计划","推广单元","关键词","关键词匹配模式","关键词出价","关键词访问URL")
  data=data[!(is.na(data$关键词访问URL) & is.na(data$关键词移动访问URL)),]
  # data=data[!(is.na(data$关键词访问URL) ),]
  return(data)
})
# 加载神马的关键词
shenma_key_fun=memoise(function(shenma_path,header0,seq0,quote0){
  data1=read.csv2(shenma_path,header0,seq0,quote0,
                  encoding = 'UTF-8',fileEncoding ='GB2312')
  data1=read.csv(shenma_path,header = T)
  data=data1[c("时间","推广计划","推广单元","关键词",
               "展现量","点击量","消费","点击率",
               "平均点击价格","平均排名")]
  names(data)=c("日期","推广计划","推广单元","关键词",
                "展现","点击","消费","点击率","平均点击价格","平均排名")
  data$展现=as.numeric(data$展现)
  data$点击=as.numeric(data$点击)
  data$消费=as.numeric(data$消费)
  data$平均排名=as.numeric(data$平均排名)
  data$总排名=data$展现*data$平均排名
  data_sum=aggregate(data[c("展现","点击","消费","总排名")],
                     by=list(关键词=data$关键词,
                                推广单元=data$推广单元,
                                推广计划=data$推广计划),
                     FUN="sum")
  data_sum$点击率=with(data_sum,点击/展现)
  data_sum$平均点击单价=with(data_sum,消费/点击)
  data_sum$平均排名=with(data_sum,总排名/展现)
  data_sum$总排名=NULL
  return(data_sum)
})

# 加载神马账户
shenma_acc_fun=memoise(function(shenma_path,header0,sep0,quote0){
  data1=read.csv2(sll_path,header0,sep0,quote0,
                  stringsAsFactors=FALSE,encoding = 'UTF-8',fileEncoding ='GB2312')

  data=data1[c("推广计划","推广组","关键词",
               "关键词匹配模式","关键词出价","关键词访问URL","关键词质量度")]
  names(data)=c("推广计划","推广单元","关键词",
                "关键词匹配模式","关键词出价","关键词移动访问URL",
                "关键词移动质量度")
  return(data)
})

ECS_order_fun=memoise(function(order_path,belong_str){
  
  data1=read.xlsx2(order_path,sheetIndex = 1)
  data=data1[grep(belong_str,data1$商品归属商户),]
  data$有效性 = rep("有效",nrow(data))
  data$有效性[grep("取消订单|*退单",data$订单状态)] = "无效"
  return(data)
})
classify_fun_3=memoise(function(data,per_pay_level=0.8,per_order_level=0.8){
  order_pay=order(data$消费,decreasing=T)
  per_pay=rep(NA,length(data))
  per_pay[order_pay]=cumsum(data$消费[order_pay]/sum(data$消费[order_pay]))
  level_num_pay=min(data$消费[per_pay<=per_pay_level])
  
  data$消费层级=rep("低消费",nrow(data))
  data$消费层级[data$消费>level_num_pay]="高消费"
  
  order_order=order(data$ECS订单,decreasing=T)
  per_order=rep(NA,length(data))
  per_order[order_order]=cumsum(data$消费[order_order]/sum(data$消费[order_order]))
  level_num_order=min(data$ECS订单[per_order<=per_order_level])
  data$订单层级=rep("低订单",nrow(data))
  data$订单层级[data$ECS订单>level_num_order]="高订单"
  
  data=within(data,{
    建议=NA
    建议[消费层级=="低消费" & 订单层级=="高订单"]="保持排名，拓词"
    建议[消费层级=="低消费" & 订单层级=="低订单"]="提排名，观测"
    建议[消费层级=="高消费" & 订单层级=="低订单"]="控成本，观测"
    建议[消费层级=="高消费" & 订单层级=="高订单"]="控成本，保持排名"
  })
  return(data)
})

count_order=memoise(function(string0,ECS_data){
  count_N=sapply(ECS_data,FUN=grep,x=string0)
  count_N=length(unlist(count_N))
  return(count_N)
})