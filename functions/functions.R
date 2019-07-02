
summary_table <- function(data,
                          total_data,
                          mapping_table,
                          mapping_table_2 = NULL,
                          group_var){
  
  # data <- salesdata_m4
  # total_data <- growth_table_mkt
  # mapping_table <- mapping_data
  # mapping_table_2 <- product_cn_mapping
  # group_var <- recog_var_m
  
  
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
           `Sales(Mn)` = value.x/1000000) 
  
  if (is.null(mapping_table_2)) {
    
    group_var_m <- paste(group_var, "en", sep = "_")
    colnames(mapping_table) <- str_replace_all(colnames(mapping_table), "_cn", "")
    cn_mapping_col <- colnames(mapping_table)[grepl(group_var, colnames(mapping_table))]
    cn_mapping <- mapping_table %>%
      select(one_of(cn_mapping_col)) %>%
      distinct()
    
    growth_table_out_m <- growth_table_out %>%
      left_join(cn_mapping) %>%
      select(date,
             `Display Name En` = group_var_m, 
             `Display Name Cn` = group_var,
             `Sales(Mn)`,
             `Growth%` = growth.x,
             `Share%` = share,
             `share_delta%`,
             EI)
    
    
    
  } else {
    colnames(mapping_table) <- str_replace_all(colnames(mapping_table), "_cn", "")
    cn_mapping_col <- colnames(mapping_table)[grepl("display name", colnames(mapping_table))]
    cn_mapping <- mapping_table %>%
      select(one_of(cn_mapping_col)) %>%
      distinct()
    
    if (any(grepl("product", group_var))) {
      growth_table_out_m <- growth_table_out %>%
        left_join(mapping_table_2) %>%
        left_join(cn_mapping) %>%
        select(date,
               `Brand Cn` = product,
               `Brand En` = product_en,
               `Manufacture Cn` = manufacture,
               `Manufacture En` = manufacture_en,
               `Display Name1 Cn` = `display name1`, 
               `Display Name1 En` = `display name1_en`,
               `Display Name2 Cn` = `display name2`, 
               `Display Name2 En` = `display name2_en`,
               `Display Name3 Cn` = `display name3`, 
               `Display Name3 En` = `display name3_en`,
               `Sales(Mn)`,
               `Growth%` = growth.x,
               `Share%` = share,
               `share_delta%`,
               EI)
    } else {
      growth_table_out_m <- growth_table_out %>%
        left_join(mapping_table_2) %>%
        left_join(cn_mapping) %>%
        select(date,
               `Molecule Cn` = molecule,
               `Molecule En` = molecule_en,
               `Display Name1 Cn` = `display name1`, 
               `Display Name1 En` = `display name1_en`,
               `Sales(Mn)`,
               `Growth%` = growth.x,
               `Share%` = share,
               `share_delta%`,
               EI)
    }
    
  }
  
  
  return(growth_table_out_m)
}

work_out <- function(data,
                     mapping_data,
                     input_metric, 
                     current_time_point,
                     input_period,
                     input_market,
                     input_atc2,
                     input_atc3,
                     input_measure,
                     input_region,
                     input_province,
                     input_city,
                     input_channel,
                     input_molecule,
                     output_type,
                     mapping_table_list){
  
  
  # data  = raw_data
  # mapping_data = mapping_table
  # input_metric = "MAT"
  # current_time_point = "201903"
  # input_period = 24
  # input_market = "HTN"
  # input_atc2 = "C07"
  # input_atc3 = "C07A"
  # input_measure = "sales"
  # input_region = "4+7Group"
  # input_province = "上海市"
  # input_city = "上海"
  # input_channel = "City"
  # input_molecule = "艾司洛尔"
  # output_type = "by group"   # 三个选项 by group/by molecule/by brand
  # mapping_table_list = mapping_table_list  # display name的顺序表
  
  # data = raw_data
  # mapping_data = mapping_data
  # input_metric = "MAT"
  # current_time_point = "201701"
  # input_period = 12
  # input_market = "HTN"
  # input_atc2 = "ALL"
  # input_atc3 = "ALL"
  # input_measure = "RMB"
  # input_region = "all"
  # input_province = "all"
  # input_city = "all"
  # input_channel = "all"
  # input_molecule = "all"
  # output_type = "by brand"
  # mapping_table_list = en_cn
  
  colname_data <- colnames(data)[!grepl("_en",colnames(data))]
  colname_data <- str_replace_all(colname_data, "_cn", "")
  colnames(data) <- str_replace_all(colnames(data), "_cn", "")
  
  
  colname_mapping <- colnames(mapping_data)[!grepl("_en",colnames(mapping_data))]
  colname_mapping <- str_replace_all(colname_mapping, "_cn", "")
  colnames(mapping_data) <- str_replace_all(colnames(mapping_data), "_cn", "")
  
  measure_list <- intersect(c("value", "unit", "volume"), colnames(data))
  
  mapping_data_m <- mapping_data %>%
    select(one_of(colname_mapping))
  
  salesdata <- data %>%
    filter(market == input_market) 
  
  if (tolower(input_atc2) != "all") {
    salesdata <- salesdata %>%
      filter(atc2 == input_atc2)
  }
  
  if (tolower(input_atc3) != "all") {
    salesdata <- salesdata %>%
      filter(atc3 == input_atc3)
  }
  
  if (tolower(input_region) != "all") {
    salesdata <- salesdata %>%
      filter(region == input_region)
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
      filter(molecule %in% input_molecule)
  }
  
  product_cn_mapping <- salesdata %>%
    select(product, product_en, manufacture, manufacture_en) %>%
    distinct()
  
  molecule_cn_mapping <- salesdata %>%
    select(molecule, molecule_en) %>%
    distinct()
  
  salesdata_m <- salesdata %>%
    select(one_of(colname_data)) %>%
    mutate(date = str_replace_all(period_code, "M", "")) %>%
    filter(date <= current_time_point)
  
  if (tolower(output_type) == "by group") {
    
    recog_var <- "display name3"
    recog_var_m <- c("display name1", "display name2", "display name3")
  } else  if (tolower(output_type) == "by molecule"){
    recog_var <- "molecule"
    recog_var_m <- c("molecule", "display name1")
  } else {
    recog_var <- "product"
    recog_var_m <- c("product", "manufacture", "display name1", "display name2", "display name3")
  }
  
  salesdata_m <- salesdata_m %>%
    left_join(mapping_data_m, by = c("pack_id")) 
  
  salesdata_m1 <- salesdata_m %>%
    group_by_at(c("date", recog_var_m)) %>%
    summarise_at(measure_list, ~sum(., na.rm = T)) %>%
    ungroup() %>%
    melt(id.vars = c("date", recog_var_m), 
         variable.name = "measure",
         value.name = "value") %>%
    mutate(measure = ifelse(measure != "value",
                            "unit",
                            "value"))
  
  if (tolower(output_type) == "by group") {
    names_list <- unique(unlist(mapping_data_m[recog_var]))
  } else {
    names_list <- unique(unlist(salesdata_m1[recog_var]))
  }
  
  salesdata_m2 <- expand.grid(name = names_list,
                              date = unique(c(substr(str_replace_all(ymd(paste(current_time_point, "01",sep = "")) 
                                                                     -months(1:(input_period-1)), "-", ""), 1, 6),
                                              current_time_point)),
                              measure = unique(salesdata_m1$measure),
                              stringsAsFactors = F)
  
  colnames(salesdata_m2)[1] <- recog_var
  
  if (tolower(output_type) == "by group") {
    
    add_table <- mapping_data_m %>%
      select(one_of(recog_var_m)) %>%
      distinct()
    
  } else {
    add_table <- salesdata_m1 %>%
      select(one_of(recog_var_m)) %>%
      distinct()
  }
  
  salesdata_m2 <- salesdata_m2 %>%
    left_join(add_table, by = recog_var)
  
  salesdata_m3 <- salesdata_m2 %>%
    left_join(salesdata_m1, by = c(recog_var_m, "date", "measure")) %>%
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
  
  if (tolower(output_type) == "by group") {
    
    price_measure <- salesdata_m4 %>%
      dcast(`display name3`+date+metric ~measure,
            value.var = "value",
            fun.aggregate = sum) %>%
      mutate(price = ifelse(unit == 0,
                            0,
                            value/unit)) %>%
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
      mutate(`Display Name En` = "Market") %>%
      select(date,
             `Display Name En`, 
             `Display Name Cn` = `Display Name`,
             `Sales(Mn)`, 
             `Growth%` = growth,
             `Share%` = share,
             `share_delta%`,
             EI)
    
    growth_table_display1 <- summary_table(salesdata_m4,
                                           growth_table_mkt,
                                           mapping_data,
                                           NULL,
                                           "display name1")
    
    growth_table_display2 <- summary_table(salesdata_m4,
                                           growth_table_mkt,
                                           mapping_data,
                                           NULL,
                                           "display name2")
    
    growth_table_display3 <- summary_table(salesdata_m4,
                                           growth_table_mkt,
                                           mapping_data,
                                           NULL,
                                           "display name3")
    growth_table <- bind_rows(growth_table_mkt_m,
                              growth_table_display1,
                              growth_table_display2,
                              growth_table_display3) %>%
      mutate(`Display Name En` = factor(`Display Name En`, levels = mapping_table_list$col_en)) %>%
      arrange(`Display Name En`)
    
  } else {
    
    formula <- as.formula(paste(recog_var, "+date+metric ~measure", sep = ""))
    
    price_measure <- salesdata_m4 %>%
      dcast(formula,
            value.var = "value",
            fun.aggregate = sum) %>%
      mutate(price = ifelse(unit == 0,
                            0,
                            value/unit)) %>%
      select(`Display Name` = recog_var,
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
    
    if (tolower(output_type) == "by brand") {
      
      growth_table_mkt_m <- growth_table_mkt %>%
        mutate(`Brand En` = "Total",
               `Manufacture En` = "Total",
               `Manufacture Cn` = "Total",
               `Display Name1 En` = "Total",
               `Display Name1 Cn` = "Total",
               `Display Name2 En` = "Total",
               `Display Name2 Cn` = "Total",
               `Display Name3 En` = "Total",
               `Display Name3 Cn` = "Total") %>%
        select(date,
               `Brand En`, 
               `Brand Cn` = `Display Name`,
               `Manufacture En`,
               `Manufacture Cn`,
               `Display Name1 En`,
               `Display Name1 Cn`,
               `Display Name2 En`,
               `Display Name2 Cn`,
               `Display Name3 En`,
               `Display Name3 Cn`,
               `Sales(Mn)`, 
               `Growth%` = growth,
               `Share%` = share,
               `share_delta%`,
               EI)
      
      growth_table_display1 <- summary_table(salesdata_m4,
                                             growth_table_mkt,
                                             mapping_data,
                                             product_cn_mapping,
                                             recog_var_m) 
      
    } else {
      
      growth_table_mkt_m <- growth_table_mkt %>%
        mutate(`Molecule En` = "Total",
               `Display Name1 En` = NA,
               `Display Name1 Cn` = NA) %>%
        select(date,
               `Molecule En`,
               `Molecule Cn` = `Display Name`,
               `Display Name1 En`,
               `Display Name1 Cn`,
               `Sales(Mn)`, 
               `Growth%` = growth,
               `Share%` = share,
               `share_delta%`,
               EI)
      
      growth_table_display1 <- summary_table(salesdata_m4,
                                             growth_table_mkt,
                                             mapping_data,
                                             molecule_cn_mapping,
                                             recog_var_m)
    }
    
    
    
    growth_table <- bind_rows(growth_table_mkt_m,
                              growth_table_display1)
  }
  
  # growth_table[] <- lapply(growth_table, function(x) {
  #   x[str_squish(x) == "NA"] <- ""
  #   x[str_squish(x) == "NaN"] <- ""
  #   x[str_squish(x) == "Inf"] <- 1
  #   x
  # })
  
  growth_table[is.na(growth_table)] <- 0
  growth_table[growth_table == Inf] <- 1
  
  if (tolower(input_measure) == "price") {
    out <- price_measure
  } else {
    out <- growth_table
  }
  
  out <- distinct(out)
  
  return(out)
  
  
  
}
