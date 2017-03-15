library(shiny)
library(shinydashboard)
# dashboardPage(
#   dashboardHeader(title = "你好!"),
#   dashboardSidebar(),
#   dashboardBody()
# )

tabItem_mode=function(medium){
  tab_mode=tabItem(
    tabName = paste0('tab_keyword_',medium),
    fileIncsv_mode(paste0("file1_",medium),
                   "关键词数据",
                   paste0("header1_",medium),
                   paste0("sep1_",medium),
                   paste0("quote1_",medium)),
    fileIncsv_mode(paste0("file2_",medium),
                   "账户信息表",
                   paste0("header2_",medium),
                   paste0("sep2_",medium),
                   paste0("quote2_",medium)),
    fileInxls_mode(paste0("file3_",medium),
                   paste0("belong_str_",medium),
                   paste0("code_str_",medium)),
    fluidRow(
      box(
        # dateRangeInput(
        #   "date_range",
        #   label = "选择日期（此功能没用）",
        #   start = as.Date("2016-12-01"), end = as.Date("2016-12-31")
        # ),
        # selectInput(
        #   "cannal",
        #   "选择渠道（此功能没用）",
        #   choices = c("百度PC账户","百度WAP账户","神马账户","搜狗账户","360账户")
        # ),
        sliderInput(
          paste0("per_pay_level_",medium),
          "总费用占比：",
          min=0,max=1,value = 0.8
        ),
        sliderInput(
          paste0("per_order_level_",medium),
          "总订单占比：",
          min=0,max=1,value = 0.8
        )
      )
    ),
    downloadButton(paste0('downloadData_',medium), '下载'),
    fluidRow(
      box(
        title = "关键词数据",width=12,solidHeader=TRUE,status="primary",
        tableOutput(paste0("keyword_",medium))
      )
    )
  )
  return(tab_mode)
}


fileIncsv_mode=function(file_name,title_name,header_name,sep_name,quote_name){
  file_input=box(
    fileInput(file_name,
              title_name,
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
              )
    ),
    tags$hr(),
    checkboxInput(header_name, '表头', TRUE),
    fluidRow(
      box(
        radioButtons(sep_name, '分隔符',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ',')),
      box(
        radioButtons(quote_name, '应用符号',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')))
  )
  return(file_input)
}

fileInxls_mode=function(file_name,belong_str,code_str){
  file_input=box(
    fileInput(file_name,
              "订单数据",
              accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv',
                '.xls'
              )
    ),
    tags$hr(),
    fluidRow(
      box(
        textInput(belong_str, '商品归属',value = '输入...')
      ),
      box(
        textInput(code_str, '代码识别特征',value = '输入...')
      )
    )
  )
  return(file_input)
}

dashboardPage(
  skin="black",
  # 页面抬头
  dashboardHeader(title = "联通四象限"),
  # 左侧栏
  dashboardSidebar(
    sidebarMenu(
      menuItem('百度关键词数据',tabName='tab_keyword_baidu',icon=icon("table")),
      menuItem('搜狗关键词数据',tabName='tab_keyword_sougou',icon=icon("table")),
      menuItem('神马关键词数据',tabName='tab_keyword_shenma',icon=icon("table")),
      menuItem('360关键词数据',tabName='tab_keyword_360',icon=icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem_mode("baidu"),
      tabItem_mode("sougou"),
      tabItem_mode("shenma"),
      tabItem_mode("360")
    )
  )
)