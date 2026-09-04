##
id = "Your ID"
pw = "Your PW"

## install.package
rm(list = ls()[ !(ls() %in% c("id", "pw")) ])
pkg = c("data.table", "tidyverse", "RPostgres", "DBI", 'glue', "frenchdata", "nanoparquet")

for(i in 1:length(pkg)){
  
  if(!require(pkg[i], character.only = T)){
    install.packages(pkg[i], dependencies = TRUE)
    require(pkg[i], character.only = T)
  }
}; rm(i, pkg)

## DB connection
wrds <- dbConnect(
  Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  dbname = "wrds",
  port = 9737,
  sslmode = "require",
  user = id,
  password = pw
)

## Parameter Setting
table_name_crsp <- "crsp.msf_v2"
s_date <- "1961-01-01"
e_date <- "1993-12-31"

##--------------------------
## CRSP
##--------------------------

## Function define
get_data <- function(conn, s_date, e_date, table_name,
                     exchanges = c("N", "A", "Q"), 
                     share_types = c("NS")) {
  
  tbl_identifier <- DBI::SQL(table_name)
  
  query <- glue::glue_sql("
    SELECT 
      a.permno, 
      a.mthcaldt AS date,
      a.mthprc AS prc,
      a.mthcap::numeric * 1000 AS mthcap,
      a.mthret AS ret,
      a.mthprcvol AS volume, 
      a.shrout::numeric * 1000 AS shrout,
      a.primaryexch AS exch
    FROM {tbl_identifier} AS a
    WHERE a.mthcaldt BETWEEN {s_date} AND {e_date}
      AND a.sharetype IN ({share_types*})
      AND a.securitytype = 'EQTY'
      AND a.securitysubtype = 'COM'
      AND a.usincflg = 'Y'
      AND a.issuertype IN ('ACOR', 'CORP')
      AND a.primaryexch IN ({exchanges*})
      AND a.conditionaltype IN ('RW', 'NW')
      AND a.tradingstatusflg = 'A'
  ", .con = conn)
  
  DBI::dbGetQuery(conn, query)
}

## call DB via function
crsp_data <- get_data(conn = wrds, 
                      s_date = s_date, 
                      e_date = e_date, 
                      table_name = table_name_crsp)

crsp_data <- crsp_data %>% mutate(ym = format(date, "%Y%m"))
crsp_raw <- crsp_data ## saving original data

##-------------------------------------------
## CCM linking table (Reference: Tidyfinance)
##-------------------------------------------
ccm_linking_table_db <- tbl(wrds, I("crsp.ccmxpf_lnkhist"))

## linking table load
ccm_linking_table <- ccm_linking_table_db |>
  filter(
    linktype %in% c("LU", "LC") & linkprim %in% c("P", "C")
  ) |>
  select(permno = lpermno, gvkey, linkdt, linkenddt) |>
  collect() |>
  mutate(linkenddt = replace_na(linkenddt, as.Date("9999-12-31"))) ## currently listed

## separated permno gvkey into 1 key in linking table
ccm_linking_table <- ccm_linking_table %>%
  mutate(
    linkdt = as.Date(linkdt),
    linkenddt = as.Date(linkenddt),
    linkenddt = replace_na(linkenddt, as.Date("9999-12-31")) ## currently listed  
  ) %>%
  arrange(permno, gvkey, linkdt, linkenddt) %>%
  mutate(
    linkdt_num = as.numeric(linkdt),
    linkenddt_num = as.numeric(linkenddt)
  ) %>%
  group_by(permno, gvkey) %>%
  mutate(
    prev_max_end = lag(cummax(linkenddt_num)), ## find max date
    
    new_spell = case_when( 
      is.na(prev_max_end) ~ 1L,
      linkdt_num <= prev_max_end + 1 ~ 0L,
      TRUE ~ 1L
    ),
    
    spell = cumsum(new_spell) ## make 1 integrated linkenddt 
  ) %>%
  group_by(permno, gvkey, spell) %>%
  summarise(
    linkdt = as.Date(min(linkdt_num), origin = "1970-01-01"),
    linkenddt = as.Date(max(linkenddt_num), origin = "1970-01-01"),
    .groups = "drop"
  ) %>%
  select(permno, gvkey, linkdt, linkenddt)

## left_join gvkey to CRSP
ccm_links <- crsp_data |>
  inner_join(
    ccm_linking_table,
    join_by(permno),
    relationship = "many-to-many"
  ) |>
  filter(
    !is.na(gvkey) &
      (date >= linkdt & date <= linkenddt)
  ) |>
  select(permno, gvkey, date)

crsp_data <- crsp_data |>
  left_join(ccm_links, join_by(permno, date))

##--------------------------
## Fama french
##--------------------------
temp = frenchdata::get_french_data_list()
temp = frenchdata::download_french_data('Fama/French 3 Factors')
temp = as.data.frame(temp[3]$subsets$data[1])

temp = temp %>% mutate(Mkt.RF = Mkt.RF/100,
                       SMB = SMB/100,
                       HML = HML/100,
                       RF = RF/100)
temp = temp %>% rename(ym = date)
temp$ym = as.character(temp$ym)

crsp_data = left_join(crsp_data, temp, by = "ym")  ## left join 3 factor to crsp
crsp_data = crsp_data %>% select(-ym) %>% relocate(gvkey, .after = permno)

##--------------------------
## compustat
##--------------------------
get_comp <- function(conn, s_date, e_date, 
                     target_vars = c('seq', 'ceq', 'at', 'lt','ib', 
                                     'txditc','txdb','itcb',
                                     'pstkrv', 'pstkl', 'oancf')) {
  
  id_vars <- c("gvkey", "fyear", "fyr", "datadate")
  comp_vars <- paste(paste0("a.", unique(c(id_vars, target_vars))), collapse = ",")
  
  query <- glue_sql("
    SELECT 
      {DBI::SQL(comp_vars)}
    FROM comp.funda AS a
    WHERE a.datadate BETWEEN {s_date} AND {e_date}
      AND a.indfmt = 'INDL'
      AND a.datafmt = 'STD'
      AND a.popsrc = 'D'
      AND a.consol = 'C'
      AND a.curcd = 'USD'
  ", .con = conn)
  
  dbGetQuery(conn, query)
}

comp <- get_comp(
  conn = wrds, 
  s_date = s_date, 
  e_date = e_date
)

comp_raw <- comp  ## save original compustat data

## make Book Value of Equity
comp <- comp |>
  mutate(
    be = coalesce(seq, ceq + pstkl, at - lt) +
      coalesce(txditc, txdb + itcb, 0) -
      coalesce(pstkrv, pstkl, 0),
    be = if_else(be <= 0, NA, be))

## leftjoin with CCM linking table to compustat
comp <- comp %>%
  mutate(datadate = as.Date(datadate)) %>%
  inner_join(
    ccm_linking_table,
    by = "gvkey",
    relationship = "many-to-many"
  ) %>%
  filter(
    datadate >= linkdt,
    datadate <= linkenddt
  )

## compustat left only one last observation
comp <- comp %>%
  mutate(
    datadate = as.Date(datadate),
    year = year(datadate)
  ) %>%
  distinct(permno, gvkey, datadate, .keep_all = TRUE) %>%
  arrange(permno, gvkey, year, datadate) %>%
  group_by(permno, gvkey, year) %>%
  slice_tail(n = 1) %>%
  ungroup()

## time t accounting period match into t+1 timing 
comp = comp %>% select(permno, gvkey, datadate, be, linkdt, linkenddt) %>% filter(!is.na(be) == T) %>% 
  mutate(year = year(datadate)) %>% mutate(
    formation_year = year + 1L
  )

##
comp <- comp %>%
  filter(
    !is.na(be),
    be > 0
  ) %>%
  arrange(permno, formation_year, datadate) %>%
  group_by(permno, formation_year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(permno, formation_year, gvkey, datadate, be)

##
dd = c("crsp_data", "wrds", "id", 'pw', 's_date','e_date', "comp", "crsp_raw",'comp_raw', "table_name_crsp")
rm(list= ls()[!ls() %in% dd])
##---------------
## merge
##---------------

## size 5 by 5
size_temp6 = crsp_data %>% 
  filter(month(date) == 6, !is.na(mthcap), mthcap > 0) %>% 
  select(permno, date, mthcap, exch, gvkey) %>%
  mutate(
    year = year(date),
    month = month(date)
  )

## NYSE break point
size_breakpoints <- size_temp6 %>% 
  filter(exch == "N") %>% 
  group_by(date) %>% 
  summarise(
    size1 = quantile(mthcap, 0.2, na.rm = TRUE),
    size2 = quantile(mthcap, 0.4, na.rm = TRUE),
    size3 = quantile(mthcap, 0.6, na.rm = TRUE),
    size4 = quantile(mthcap, 0.8, na.rm = TRUE),
    .groups = "drop"
  )

## decile portfolio assign
size_temp6 <- size_temp6 %>% 
  left_join(size_breakpoints, by = "date", relationship = "many-to-one") %>%
  mutate(
    port_size = case_when(
      mthcap <= size1 ~ 1L,
      mthcap <= size2 ~ 2L,
      mthcap <= size3 ~ 3L,
      mthcap <= size4 ~ 4L,
      mthcap >  size4 ~ 5L,
      TRUE ~ NA_integer_
    )
  ) %>% 
  select(permno, gvkey, year, month, port_size)

## portfolio pormation t+1 July
crsp_data <- crsp_data %>%
  mutate(
    year = year(date),
    month = month(date),
    year_for_join = if_else(month >= 7, year, year - 1L)
  ) %>%
  left_join(
    size_temp6 %>%
      select(permno, gvkey, year, port_size) %>%
      rename(year_for_join = year),
    by = c("permno", "year_for_join"),
    relationship = "many-to-one"
  ) %>%
  select(-year_for_join) %>% select(-gvkey.y)
crsp_data = crsp_data %>% rename(gvkey = gvkey.x)

#################################

## make BM in t-1 Dec.
temp2 <- crsp_data %>% 
  select(permno, date, exch, mthcap) %>% 
  filter(
    month(date) == 12,
    !is.na(mthcap),
    mthcap > 0
  ) %>%
  mutate(
    formation_year = year(date) + 1L,
    dec_me = mthcap
  )

temp2 <- left_join(
  temp2,
  comp %>% select(permno, formation_year, be),
  by = c("permno", "formation_year"),
  relationship = "many-to-one"
) %>% 
  mutate(
    bm = be * 1e6 / dec_me
  )

bm_breakpoints <- temp2 %>% 
  filter(
    exch == "N",
    !is.na(bm),
    bm > 0,
    !is.na(mthcap),
    mthcap > 0
  ) %>% 
  group_by(date) %>% 
  summarise(
    bm1 = quantile(bm, 0.2, na.rm = TRUE),
    bm2 = quantile(bm, 0.4, na.rm = TRUE),
    bm3 = quantile(bm, 0.6, na.rm = TRUE),
    bm4 = quantile(bm, 0.8, na.rm = TRUE),
    n_nyse = n(),
    .groups = "drop"
  )

temp2 <- temp2 %>% 
  left_join(
    bm_breakpoints,
    by = "date",
    relationship = "many-to-one"
  ) %>%
  mutate(
    port_bm = case_when(
      bm <= bm1 ~ 1L,
      bm <= bm2 ~ 2L,
      bm <= bm3 ~ 3L,
      bm <= bm4 ~ 4L,
      bm >  bm4 ~ 5L,
      TRUE ~ NA_integer_
    )
  ) %>% 
  select(-bm1, -bm2, -bm3, -bm4, -n_nyse)

temp2 = temp2 %>% 
  select(permno, formation_year, be, dec_me, bm, port_bm) %>%
  distinct()

crsp_data <- crsp_data %>%
  select(-any_of(c("port_bm", "bm", "be", "dec_me"))) %>%
  mutate(
    year_for_join = if_else(month >= 7, year, year - 1L)
  ) %>%
  left_join(
    temp2 %>%
      rename(year_for_join = formation_year),
    by = c("permno", "year_for_join"),
    relationship = "many-to-one"
  ) %>%
  select(-year_for_join)

rm(temp, temp2, size_temp6, size_breakpoints, bm_breakpoints)

## filter check
crsp_data = crsp_data %>% group_by(permno) %>% arrange(date) %>% 
  mutate(june_me = lag(mthcap,1)) %>% ungroup() %>% 
  filter(date > "1963-06-30" & date < "1992-01-01") %>% filter(!is.na(port_bm) == T) %>% 
  filter(!is.na(port_size) == T) 
write_parquet(crsp_data, "dat.parquet")