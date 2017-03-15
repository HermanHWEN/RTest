library(shiny)
library(shinydashboard)
options(shiny.maxRequestSize=30*1024^2)

function(input,output){
  options(shiny.sanitize.errors = TRUE)
  # 百度
  dataInput_baidu=reactive({
    keyword_file <- input$file1_baidu
    Acc_file <- input$file2_baidu
    ECS_file=input$file3_baidu

    if (is.null(keyword_file) | is.null(Acc_file))
      return("NULL")
    else{
      baidu_key=baidu_key_fun(keyword_file$datapath, input$header1_baidu,
                              input$sep1_baidu,input$quote1_baidu)
      baidu_acc=baidu_acc_fun(Acc_file$datapath,input$header2_baidu,
                              input$sep2_baidu,input$quote2_baidu,FALSE)
      baidu_merge=merge(baidu_key,baidu_acc,x.all=T,
                        id=c("推广计划","推广单元","关键词"))
      if(!is.null(ECS_file)){
        ECS_data=ECS_order_fun(ECS_file$datapath,input$belong_str_baidu)
        ECS_data=ECS_data[str_detect(ECS_data$广告编码,input$code_str_baidu),]
        baidu_merge$ECS订单移动=sapply(baidu_merge$关键词移动访问URL,count_order,ECS_data=ECS_data$广告编码)
        baidu_merge$ECS订单PC=sapply(baidu_merge$关键词访问URL,count_order,ECS_data=ECS_data$广告编码)
        baidu_merge$ECS订单=with(baidu_merge,ECS订单移动+ECS订单PC)
        baidu_merge=classify_fun_3(data = baidu_merge,
                                   input$per_pay_level_baidu,input$per_order_level_baidu)
      }
      return(baidu_merge)
    }
  })
  
  # 百度下载
  output$downloadData_baidu <- downloadHandler(
    filename = function() { 
      paste("outputdata_baidu", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dataInput_baidu(), file,fileEncoding='GB2312')
    }
  )
  output$keyword_baidu=renderTable({
    return(tryCatch(head(dataInput_baidu()),
             warning=function(w) stop(safeError("警告:输入正确的文件")),
             error=function(e) stop(safeError("错误:输入正确的文件"))))
  })
  
  # 搜狗
  dataInput_sougou=reactive({
    # 将输入的字段换名称方便更改
    keyword_file <- input$file1_sougou
    Acc_file <- input$file2_sougou
    ECS_file=input$file3_sougou
    # 关键词输入内容
    header1=input$header1_sougou
    sep1=input$sep1_sougou
    quote1=input$quote1_sougou
    # 账户输入内容
    header2=input$header2_sougou
    sep2=input$sep2_sougou
    quote2=input$quote2_sougou
    # 分界水平
    per_pay_level=input$per_pay_level_sougou
    per_order_level=input$per_order_level_sougou
    
    if (is.null(keyword_file) | is.null(Acc_file))
      return("NULL")
    else{
      data_key=sougou_key_fun(keyword_file$datapath, header1,
                              sep1,quote1)
      data_acc=sougou_acc_fun(Acc_file$datapath,header2,
                              sep2,quote2,FALSE)
      data_merge=merge(data_key,data_acc,x.all=T,
                       id=c("推广计划","推广单元","关键词"))
      if(!is.null(ECS_file)){
        ECS_data=ECS_order_fun(ECS_file$datapath,input$belong_str_baidu)
        ECS_data=ECS_data[str_detect(ECS_data$广告编码,input$code_str_baidu),]
        data_merge$ECS订单移动=sapply(data_merge$关键词移动访问URL,count_order,ECS_data=ECS_data$广告编码)
        data_merge$ECS订单PC=sapply(data_merge$关键词访问URL,count_order,ECS_data=ECS_data$广告编码)
        data_merge$ECS订单=with(baidu_merge,ECS订单移动+ECS订单PC)
        data_merge=classify_fun_3(data = data_merge,
                                  input$per_pay_level_baidu,input$per_order_level_baidu)
      }
      return(data_merge)
    }
  })
  
  # 搜狗下载
  output$downloadData_sougou <- downloadHandler(
    filename = function() { 
      paste("outputdata_baidu", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dataInput_sougou(), file,fileEncoding='GB2312')
    }
  )
  output$keyword_sougou=renderTable({
    return(tryCatch(head(dataInput_sougou()),
                    warning=function(w) stop(safeError("警告:输入正确的文件")),
                    error=function(e) stop(safeError("错误:输入正确的文件"))))
  })
  # 神马
  dataInput_shenma=reactive({
    # 将输入的字段换名称方便更改
    keyword_file <- input$file1_shenma
    Acc_file <- input$file2_shenma
    ECS_file=input$file3_shenma
    # 关键词输入内容
    header1=input$header1_shenma
    sep1=input$sep1_shenma
    quote1=input$quote1_shenma
    # 账户输入内容
    header2=input$header2_shenma
    sep2=input$sep2_shenma
    quote2=input$quote2_shenma
    # 分界水平
    per_pay_level=input$per_pay_level_shenma
    per_order_level=input$per_order_level_shenma
    
    if (is.null(keyword_file) | is.null(Acc_file))
      return("NULL")
    else{
      data_key=shenma_key_fun(keyword_file$datapath, header1,
                              sep1,quote1)
      data_acc=shenma_acc_fun(Acc_file$datapath,header2,
                              sep2,quote2,FALSE)
      data_merge=merge(data_key,data_acc,x.all=T,
                       id=c("推广计划","推广单元","关键词"))
      if(!is.null(ECS_file)){
        ECS_data=ECS_order_fun(ECS_file$datapath,input$belong_str_baidu)
        ECS_data=ECS_data[str_detect(ECS_data$广告编码,input$code_str_baidu),]
        data_merge$ECS订单移动=sapply(data_merge$关键词移动访问URL,count_order,ECS_data=ECS_data$广告编码)
        data_merge$ECS订单PC=sapply(data_merge$关键词访问URL,count_order,ECS_data=ECS_data$广告编码)
        data_merge$ECS订单=with(baidu_merge,ECS订单移动+ECS订单PC)
        data_merge=classify_fun_3(data = data_merge,
                                  input$per_pay_level_baidu,input$per_order_level_baidu)
      }
      return(data_merge)
    }
  })
  
  # 神马下载
  output$downloadData_shenma <- downloadHandler(
    filename = function() { 
      paste("outputdata_baidu", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dataInput_shenma(), file,fileEncoding='GB2312')
    }
  )
  output$keyword_shenma=renderTable({
    return(tryCatch(head(dataInput_shenma()),
                    warning=function(w) stop(safeError("警告:输入正确的文件")),
                    error=function(e) stop(safeError("错误:输入正确的文件"))))
  })
  # 360
  dataInput_shenma=reactive({
    # 将输入的字段换名称方便更改
    keyword_file <- input$file1_360
    Acc_file <- input$file2_360
    ECS_file=input$file3_360
    # 关键词输入内容
    header1=input$header1_360
    sep1=input$sep1_360
    quote1=input$quote1_360
    # 账户输入内容
    header2=input$header2_360
    sep2=input$sep2_360
    quote2=input$quote2_360
    # 分界水平
    per_pay_level=input$per_pay_level_360
    per_order_level=input$per_order_level_360
    
    if (is.null(keyword_file) | is.null(Acc_file))
      return("NULL")
    else{
      data_key=sll_key_fun(keyword_file$datapath, header1,
                              sep1,quote1)
      data_acc=sll_acc_fun(Acc_file$datapath,header2,
                              sep2,quote2,FALSE)
      data_merge=merge(data_key,data_acc,x.all=T,
                       id=c("推广计划","推广单元","关键词"))
      if(!is.null(ECS_file)){
        ECS_data=ECS_order_fun(ECS_file$datapath,input$belong_str_baidu)
        ECS_data=ECS_data[str_detect(ECS_data$广告编码,input$code_str_baidu),]
        data_merge$ECS订单移动=sapply(data_merge$关键词移动访问URL,count_order,ECS_data=ECS_data$广告编码)
        data_merge$ECS订单PC=sapply(data_merge$关键词访问URL,count_order,ECS_data=ECS_data$广告编码)
        data_merge$ECS订单=with(baidu_merge,ECS订单移动+ECS订单PC)
        data_merge=classify_fun_3(data = data_merge,
                                  input$per_pay_level_baidu,input$per_order_level_baidu)
      }
      return(data_merge)
    }
  })
  
  # 360下载
  output$downloadData_360 <- downloadHandler(
    filename = function() { 
      paste("outputdata_baidu", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(dataInput_360(), file,fileEncoding='GB2312')
    }
  )
  output$keyword_360=renderTable({
    return(tryCatch(head(dataInput_360()),
                    warning=function(w) stop(safeError("警告:输入正确的文件")),
                    error=function(e) stop(safeError("错误:输入正确的文件"))))
  })
}