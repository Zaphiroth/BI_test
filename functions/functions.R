summary_table <- function(data,
                          total_data,
                          group_var){
  
  # data <- salesdata_m4
  # total_data <- growth_table_mkt
  # group_var <- "display name1"
  
  growth_table_out <- data %>%
    group_by_at(c(group_var, "date")) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    arrange_at(c(group_var, "date")) %>%
    group_by_at(group_var) %>%
    mutate(row_number = row_number(),
           yoy_value = map_dbl(row_number, function(x) ifelse(x<=12,
                                                              0,
                                                              value[x-12])),
           growth = value/yoy_value-1) %>%
    ungroup() %>%
    left_join(total_data, by = "date") %>%
    mutate(share = value.x/value.y,
           share_yoy = yoy_value.x/yoy_value.y,
           `share_delta%` = share - share_yoy,
           EI = growth.x/growth.y,
           `Sales(Mn)` = value.x/1000000) %>%
    select(date,
           `Display Name` = group_var,  
           `Sales(Mn)`,
           `Growth%` = growth.x,
           `Share%` = share,
           `share_delta%`,
           EI)
  
  return(growth_table_out)
}

work_out <- function(data,
                     input_metric, 
                     current_time_point,
                     input_period,
                     input_market,
                     input_tc_group,
                     input_measure,
                     input_index,
                     input_region,
                     input_province,
                     input_city,
                     input_channel = "all",
                     measure_list = c("sales", "unit"),
                     input_molecule,
                     output_type){
  
  
  #' @metric：MAT/YTD/RQ/MTH
  #' @current_time_point
  #' @channel 
  
  # data  = raw_data
  # input_metric = "MAT"
  # current_time_point = "201903"
  # input_period = 12
  # input_market = "LIP"
  # input_tc_group = "C001"
  # input_measure = "sales"
  # input_index = "MS%"  # multiple choice
  # input_region = "非4+7城市组"
  # input_province = "江苏"
  # input_city = "南京市"
  # input_channel = "City"
  # input_molecule = "恩替卡韦"
  # output_type = "brand group"
  # measure_list <- c("sales", "units")
  
  
  
  salesdata <- data %>%
    filter(market == input_market) 
  
  if (tolower(input_tc_group) != "all") {
    salesdata <- salesdata %>%
      filter(atc == input_tc_group)
  }
  
  if (tolower(input_region) != "all") {
    salesdata <- salesdata %>%
      filter(`4+7城市` == input_region)
  }
  
  if (tolower(input_province) != "all") {
    salesdata <- salesdata %>%
      filter(province == input_province)
  }
  
  if (tolower(input_city) != "all") {
    salesdata <- salesdata %>%
      filter(city == input_city)
  }
  
  if (tolower(input_channel) != "all") {
    salesdata <- salesdata %>%
      filter(channel == input_channel)
  }
  
  if (tolower(input_molecule) != "all") {
    salesdata <- salesdata %>%
      filter(`通用名` == input_molecule)
  }
  
  
  salesdata_m <- salesdata %>%
    filter(date <= current_time_point)
  
  if (tolower(output_type) == "brand group") {
    
    salesdata_m <- salesdata_m %>%
      left_join(mapping_table, by = c("pack id")) 
    
    recog_var <- "display name3"
    recog_var_m <- c("display name1", "display name2", "display name3")
  } else {
    recog_var <- "商品名"
    recog_var_m <- "商品名"
  }
  
  
  salesdata_m1 <- salesdata_m %>%
    group_by_at(c("date", recog_var_m)) %>%
    summarise_at(measure_list, ~sum(., na.rm = T)) %>%
    ungroup() %>%
    melt(id.vars = c("date", recog_var_m), 
         variable.name = "measure",
         value.name = "value") %>%
    mutate(measure = ifelse(measure != "sales",
                            "unit",
                            "sales"))
  
  
  salesdata_m2 <- expand.grid(name = unique(unlist(salesdata_m1[recog_var])),
                              date = unique(salesdata_m1$date),
                              measure = unique(salesdata_m1$measure),
                              stringsAsFactors = F)
  colnames(salesdata_m2)[1] <- recog_var
  
  salesdata_m3 <- salesdata_m2 %>%
    left_join(salesdata_m1, by = c(recog_var, "date", "measure")) %>%
    mutate(value = ifelse(is.na(value), 0, value),
           year = substr(date, 1, 4)) %>%
    group_by_at(c(recog_var_m, "measure", "year", "date")) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()
  
  salesdata_m4 <- salesdata_m3 %>%
    group_by_at(c(recog_var_m, "measure", "year")) %>%
    arrange_at(c(recog_var_m, "measure", "year", "date")) %>%
    mutate(ytd = cumsum(value)) %>%
    ungroup() %>%
    group_by_at(c(recog_var_m, "measure")) %>%
    arrange_at(c(recog_var_m, "measure", "date")) %>%
    mutate(mat = roll_sum(c(rep(NA, 11), value), 12),
           mth = value,
           rq = roll_sum(c(rep(NA, 2), value), 3)) %>%
    ungroup() %>%
    select(-year, -value) %>%
    melt(id.vars = c(recog_var_m, "measure", "date"),
         variable.name = "metric",
         value.name = "value") %>%
    mutate(value = ifelse(is.na(value),
                          0,
                          value))
  
  salesdata_m4 <- salesdata_m4 %>%
    filter(metric == tolower(input_metric))
  
  if (tolower(output_type) == "brand group") {
    
    price_measure <- salesdata_m4 %>%
      dcast(`display name3`+date+metric ~measure,
            value.var = "value",
            fun.aggregate = sum) %>%
      mutate(price = ifelse(unit == 0,
                            0,
                            sales/unit)) %>%
      select(`Display Name` = "display name3",
             date,
             price)
    growth_table_mkt <- salesdata_m4 %>%
      group_by(date) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      arrange(date) %>%
      mutate(row_number = row_number(),
             yoy_value = map_dbl(row_number, function(x) ifelse(x<=12,
                                                                0,
                                                                value[x-12])),
             growth = value/yoy_value-1,
             `Sales(Mn)` = value/1000000) %>%
      mutate(`Display Name` = "Market",
             share = 1,
             `share_delta%` = 0,
             EI = 100) %>%
      ungroup() %>%
      select(`Display Name`, date, value, growth, share, `share_delta%`, EI, yoy_value, `Sales(Mn)`)
    
    growth_table_mkt_m <- growth_table_mkt %>%
      select(date,
             `Display Name`,
             `Sales(Mn)`, 
             `Growth%` = growth,
             `Share%` = share,
             `share_delta%`,
             EI)
    
    growth_table_display1 <- summary_table(salesdata_m4,
                                           growth_table_mkt,
                                           "display name1")
    
    growth_table_display2 <- summary_table(salesdata_m4,
                                           growth_table_mkt,
                                           "display name2")
    
    growth_table_display3 <- summary_table(salesdata_m4,
                                           growth_table_mkt,
                                           "display name3")
    growth_table <- bind_rows(growth_table_mkt_m,
                              growth_table_display1,
                              growth_table_display2,
                              growth_table_display3)
    
  } else {
    
    price_measure <- salesdata_m4 %>%
      dcast(`商品名`+date+metric ~measure,
            value.var = "value",
            fun.aggregate = sum) %>%
      mutate(price = ifelse(unit == 0,
                            0,
                            sales/unit)) %>%
      select(`Display Name` = `商品名`,
             date,
             price)
    
    growth_table_mkt <- salesdata_m4 %>%
      group_by(date) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      arrange(date) %>%
      mutate(row_number = row_number(),
             yoy_value = map_dbl(row_number, function(x) ifelse(x<=12,
                                                                0,
                                                                value[x-12])),
             growth = value/yoy_value-1,
             `Sales(Mn)` = value/1000000) %>%
      mutate(`Display Name` = "Total",
             share = 1,
             `share_delta%` = 0,
             EI = 100) %>%
      ungroup() %>%
      select(`Display Name`, date, value, growth, share, `share_delta%`, EI, yoy_value, `Sales(Mn)`)
    
    growth_table_mkt_m <- growth_table_mkt %>%
      select(date,
             `Display Name`,
             `Sales(Mn)`, 
             `Growth%` = growth,
             `Share%` = share,
             `share_delta%`,
             EI)
    
    growth_table_display1 <- summary_table(salesdata_m4,
                                           growth_table_mkt,
                                           "商品名")
    
    growth_table <- bind_rows(growth_table_mkt_m,
                              growth_table_display1)
  }
  
  
  
  if (tolower(input_measure) == "price") {
    out <- price_measure
  } else {
    out <- growth_table
  }
  
  return(out)
  
}
