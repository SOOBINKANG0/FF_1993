## install.package
rm(list = ls())
pkg = c("data.table", "tidyverse", "RPostgres", "DBI", "writexl", 'glue', "frenchdata", "nanoparquet")

for(i in 1:length(pkg)){
  
  if(!require(pkg[i], character.only = T)){
    install.packages(pkg[i], dependencies = TRUE)
    require(pkg[i], character.only = T)
  }
}; rm(i, pkg)
dir.create("Results")
##
dat = read_parquet("dat.parquet")
dat = dat %>% mutate(ri_rf = ret - RF)

##------------------------------
## Table 1
##------------------------------
report_dat <- dat %>% filter(month(date) == 7)
table1 <- report_dat %>%
  group_by(date, port_size, port_bm) %>%
  summarise(
    avg_firm_size = mean(june_me, na.rm = TRUE) / 1e6,
    portfolio_bm = sum(be * 1e6, na.rm = TRUE) / sum(dec_me, na.rm = TRUE),
    n_firms = n(),
    .groups = "drop"
  )

table1 <- table1 %>%
  group_by(port_size, port_bm) %>%
  summarise(
    avg_firm_size = mean(avg_firm_size, na.rm = TRUE),
    avg_bm = mean(portfolio_bm, na.rm = TRUE),
    avg_n_firms = mean(n_firms, na.rm = TRUE),
    .groups = "drop"
  )

table1_size <- table1 %>%
  select(port_size, port_bm, avg_firm_size) %>%
  pivot_wider(
    names_from = port_bm,
    values_from = avg_firm_size,
    names_prefix = "BM"
  )

table1_size = table1 %>% select(port_size, port_bm, avg_firm_size) %>% 
  pivot_wider(names_from = port_bm, values_from = avg_firm_size, names_prefix = "BE/ME")

table1_bm = table1 %>% select(port_size, port_bm, avg_bm) %>% 
  pivot_wider(names_from = port_bm, values_from = avg_bm, names_prefix = "BE/ME")

table1_num = table1 %>% select(port_size, port_bm, avg_n_firms) %>% 
  pivot_wider(names_from = port_bm, values_from = avg_n_firms, names_prefix = "BE/ME")

res = rbind(table1_size, table1_bm, table1_num)
# write_xlsx(res, "Results/table1.xlsx")
rm(table1_size, table1_bm, table1_num, table1, res)

##------------------------------
## Table 2
##------------------------------
rf_monthly <- dat %>%
  select(date, RF) %>%
  distinct()

temp <- dat %>%
  filter(
    !is.na(port_size),
    !is.na(port_bm),
    !is.na(ri_rf),
    !is.na(june_me),
    june_me > 0
  ) %>%
  group_by(date, port_size, port_bm) %>%
  summarise(
    ret_port = weighted.mean(ri_rf, w = june_me, na.rm = TRUE),
    .groups = "drop"
  )

table2_avg_ret = temp %>% group_by(port_size, port_bm) %>% summarise(avg_ret = mean(ret_port)) %>% 
  pivot_wider(names_from = port_bm, values_from = avg_ret, names_prefix = "BM")
table2_avg_ret = round(table2_avg_ret,4)*100
table2_avg_ret$port_size = table2_avg_ret$port_size /100

table2_sd = temp %>% group_by(port_size, port_bm) %>% summarise(sd = sd(ret_port)) %>% 
  pivot_wider(names_from = port_bm, values_from = sd, names_prefix = "BM")
table2_sd = round(table2_sd,4)*100
table2_sd$port_size = table2_sd$port_size/100

table2_tstat = temp %>% group_by(port_size, port_bm) %>% summarise(tstat = t.test(ret_port)$statistic) %>% 
  pivot_wider(names_from = port_bm, values_from = tstat, names_prefix = "BM")

res = rbind(table2_avg_ret, table2_sd, table2_tstat)
# write_xlsx(res, "Results/table2.xlsx")

rm(table2_avg_ret, table2_sd, table2_tstat, res)

##------------------------------
## Table 4
##------------------------------
table4_b = matrix(NA, nrow = 5, ncol = 5)
table4_t = matrix(NA, nrow = 5, ncol = 5)
table4_R = matrix(NA, nrow = 5, ncol = 5)
table4_s = matrix(NA, nrow = 5, ncol = 5)

for(i in 1:5){
  for(j in 1:5){
  temp_loop = temp %>% filter(port_size == i, port_bm == j) %>% 
    select(date, ret_port) %>% left_join(dat %>% select(date, Mkt.RF) %>% distinct(),
                                         by = "date",
                                         relationship = "many-to-one")
    
    res = summary(lm(data = temp_loop, formula = ret_port ~ Mkt.RF))
    
    table4_b[i,j] <- round(res$coefficients["Mkt.RF", "Estimate"],2)
    table4_t[i,j] <- round(res$coefficients["Mkt.RF", "t value"],2)
    table4_R[i,j] <- round(res$r.squared,2)
    table4_s[i,j] <- round(res$sigma*100,2)
  }
}

res = as.data.frame(rbind(table4_b, table4_t, table4_R, table4_s))
# write_xlsx(res, "Results/table4.xlsx")
rm(list = ls(pattern = "table4"))

##------------------------------
## Table 5
##------------------------------
table5_s = matrix(NA, nrow = 5, ncol = 5)
table5_st = matrix(NA, nrow = 5, ncol = 5)
table5_h = matrix(NA, nrow = 5, ncol = 5)
table5_ht = matrix(NA, nrow = 5, ncol = 5)
table5_R = matrix(NA, nrow = 5, ncol = 5)
table5_se = matrix(NA, nrow = 5, ncol = 5)

for(i in 1:5){
  for(j in 1:5){
    temp_loop = temp %>% filter(port_size == i, port_bm == j) %>% 
      select(date, ret_port) %>% left_join(dat %>% select(date, Mkt.RF, SMB, HML) %>% distinct(),
                                           by = "date",
                                           relationship = "many-to-one")
    
    res = summary(lm(data = temp_loop, formula = ret_port ~ SMB + HML))
    
    table5_s[i,j] <- res$coefficients["SMB","Estimate"] %>% round(2)
    table5_st[i,j] <- res$coefficients["SMB","t value"]  %>% round(2)
    table5_h[i,j] <- res$coefficients["HML","Estimate"]  %>% round(2)
    table5_ht[i,j] <- res$coefficients["HML","t value"]  %>% round(2)
    table5_R[i,j]  <- round(res$r.squared,2)
    table5_se[i,j] <- round(res$sigma*100,2)
  }
}
res = as.data.frame(rbind(table5_s, table5_st, table5_h, table5_ht, table5_R, table5_se))
# write_xlsx(res, "Results/table5.xlsx")
rm(list = ls(pattern = "table5"))
##------------------------------
## Table 6
##------------------------------
table6_b = matrix(NA, nrow = 5, ncol = 5)
table6_bt = matrix(NA, nrow = 5, ncol = 5)
table6_s = matrix(NA, nrow = 5, ncol = 5)
table6_st = matrix(NA, nrow = 5, ncol = 5)
table6_h = matrix(NA, nrow = 5, ncol = 5)
table6_ht = matrix(NA, nrow = 5, ncol = 5)
table6_R = matrix(NA, nrow = 5, ncol = 5)
table6_se = matrix(NA, nrow = 5, ncol = 5)

for(i in 1:5){
  for(j in 1:5){
    temp_loop = temp %>% filter(port_size == i, port_bm == j) %>% 
      select(date, ret_port) %>% left_join(dat %>% select(date, Mkt.RF, SMB, HML) %>% distinct(),
                                           by = "date",
                                           relationship = "many-to-one")
    
    res = summary(lm(data = temp_loop, formula = ret_port ~ Mkt.RF + SMB + HML))
    
    table6_b[i,j] <- res$coefficients["Mkt.RF","Estimate"] %>% round(2)
    table6_bt[i,j] <- res$coefficients["Mkt.RF","t value"]  %>% round(2)
    table6_s[i,j] <- res$coefficients["SMB","Estimate"] %>% round(2)
    table6_st[i,j] <- res$coefficients["SMB","t value"]  %>% round(2)
    table6_h[i,j] <- res$coefficients["HML","Estimate"]  %>% round(2)
    table6_ht[i,j] <- res$coefficients["HML","t value"]  %>% round(2)
    table6_R[i,j]  <- round(res$r.squared,2)
    table6_se[i,j] <- round(res$sigma*100,2)
  }
}
res = rbind(table6_b, table6_bt, table6_s, table6_st, table6_h, table6_ht, table6_R, table6_se) %>% 
  as.data.frame()
# write_xlsx(res, "Results/table6.xlsx")
rm(list = ls(pattern = "table6"))

##------------------------------
## Table 9a
##------------------------------

table9_b = matrix(NA, nrow = 5, ncol = 5)
table9_bt = matrix(NA, nrow = 5, ncol = 5)
table9_s = matrix(NA, nrow = 5, ncol = 5)
table9_st = matrix(NA, nrow = 5, ncol = 5)
table9_h = matrix(NA, nrow = 5, ncol = 5)
table9_ht = matrix(NA, nrow = 5, ncol = 5)


## change formula in lm() and change variable _s, _h, _b
for(i in 1:5){
  for(j in 1:5){
    temp_loop = temp %>% filter(port_size == i, port_bm == j) %>% 
      select(date, ret_port) %>% left_join(dat %>% select(date, Mkt.RF, SMB, HML) %>% distinct(),
                                           by = "date",
                                           relationship = "many-to-one")
    
    res = summary(lm(data = temp_loop, formula = ret_port ~ Mkt.RF))
    
    table9_s[i,j] <- round(res$coefficients["(Intercept)","Estimate"]*100 ,2)
    table9_st[i,j] <- round(res$coefficients["(Intercept)","t value"],2)
    
  }
}

for(i in 1:5){
  for(j in 1:5){
    temp_loop = temp %>% filter(port_size == i, port_bm == j) %>% 
      select(date, ret_port) %>% left_join(dat %>% select(date, Mkt.RF, SMB, HML) %>% distinct(),
                                           by = "date",
                                           relationship = "many-to-one")
    
    res = summary(lm(data = temp_loop, formula = ret_port ~ SMB+HML))
    
    table9_h[i,j] <- round(res$coefficients["(Intercept)","Estimate"]*100 ,2)
    table9_ht[i,j] <- round(res$coefficients["(Intercept)","t value"],2)
    
  }
}

for(i in 1:5){
  for(j in 1:5){
    temp_loop = temp %>% filter(port_size == i, port_bm == j) %>% 
      select(date, ret_port) %>% left_join(dat %>% select(date, Mkt.RF, SMB, HML) %>% distinct(),
                                           by = "date",
                                           relationship = "many-to-one")
    
    res = summary(lm(data = temp_loop, formula = ret_port ~ Mkt.RF+SMB+HML))
    
    table9_b[i,j] <- round(res$coefficients["(Intercept)","Estimate"]*100 ,2)
    table9_bt[i,j] <- round(res$coefficients["(Intercept)","t value"],2)
    
  }
}

res1 = cbind(table9_s, table9_st)
res2 = cbind(table9_h, table9_ht)
res3 = cbind(table9_b, table9_bt)
res = rbind(res1, res2,res3 ) %>% as.data.frame()
# write_xlsx(res, "Results/table9a.xlsx")