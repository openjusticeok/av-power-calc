library(ojodb)
library(rdpower)
library(tictoc)

source("ojo_crim_cases.R")

oscn_districts <- c("ADAIR",
                    "CANADIAN",
                    "CLEVELAND",
                    "COMANCHE",
                    "ELLIS",
                    "GARFIELD",
                    "LOGAN",
                    "OKLAHOMA",
                    "PAYNE",
                    "PUSHMATAHA",
                    "ROGER MILLS",
                    "ROGERS",
                    "TULSA")

id_dcn <- function(df) {
  df |>
    mutate(id = str_remove_all(id, "\""),
           district = str_extract(id, "(?<=district: ).*?(?=,)"),
           case_number = str_extract(id,  "(?<=case_number: ).*?(?=\\})")) |>
    mutate(case_number = paste0(str_sub(case_number, 1, 8),
                                str_pad(str_extract(case_number, "(?<=-)\\d{1,5}$"),
                                        width = 5,
                                        side = "left",
                                        pad = 0))) |>
    arrange(district, case_number) |>
    select(district, case_number, everything()) |>
    ungroup() |>
    select(-id)
}

# tic()
# cases_raw <- ojo_tbl("case") |>
#   filter(case_type %in% c("CF", "CM"),
#          year(date_filed) %in% 2010:2022,
#          district %in% !!oscn_districts) |>
#   select(id, date_filed) |>
#   left_join(ojo_tbl("count"),
#             by = c("id" = "case_id")) |>
#   group_by(case_id = id.x, date_filed,
#            disp_date = disposition_date, party) |>
#   count(disposition) |>
#   collect()
# 
# cases <- cases_raw |>
#   mutate(disposition = replace_na(disposition, "UNKNOWN") |>
#            str_remove(",.*") |>
#            str_squish() |>
#            str_to_lower() |>
#            str_replace_all(" ", "_")
#          ) |>
#   filter(!is.na(disp_date)) |>
#   mutate(disposition = if_else(disposition %in% c("conviction",
#                                                   "dismissed",
#                                                   "deferred",
#                                                   "dismissed_with_costs"),
#                                disposition,
#                                "other")) |>
#   pivot_wider(id_cols = c("case_id", "date_filed", "disp_date", "party"),
#               names_from = "disposition",
#               values_from = "n",
#               values_fill = 0) |>
#   rename(id = case_id) |>
#   id_dcn() |>
#   mutate(has_costs = conviction + dismissed_with_costs + deferred > 0) |>
#   mutate(party = party |>
#            str_remove_all("\"| ") |>
#            str_extract("(?<=name:)\\d{1,20}"))
# toc()
# 
# BRRR::skrrrahh(12)
# 
# write_csv(cases, "data/cases-sample.csv")

cases <- read_csv("data/cases-sample.csv")

#  Fines and fees reduced at conviction####
# tic()
# m <- ojo_crim_cases(districts = oscn_districts,
#                     case_types = c("CM", "CF"),
#                     file_years = 2010:2022) |>
#   select(id, district, case_number) |>
#   distinct() |>
#   ojo_add_minutes() |>
#   filter(!is.na(amount),
#          date >= "2015-01-01") |>
#   mutate(fee_cat = if_else(code == "FINE", "FINE", "FEE")) |>
#   group_by(district, case_number, date, party) |>
#   summarize(n_fees = n(),
#             total_fees = sum(amount)) |>
#   collect()
# toc()
# 
# BRRR::skrrrahh(11)
# 
# tic()
# fines <- ojo_crim_cases(districts = oscn_districts,
#                         case_types = c("CM", "CF"),
#                         file_years = 2016:2022) |>
#   select(id, district, case_number) |>
#   distinct() |>
#   left_join(ojo_tbl("minute"),
#             by = c("id" = "case_id")) |>
#   filter(code == "FINE") |>
#   collect()
# toc()
# 
# write_csv(fines, "data/fines-sample.csv")

fines <- read_csv("data/fines-sample.csv")

case_fine <- fines |>
  filter(!is.na(amount)) |> 
  group_by(district,
           case_number,
           date,
           party = party |>
             str_remove_all("\"| ") |>
             str_extract("(?<=name:)\\d{1,20}")) |> 
  summarize(fine = sum(amount),
            n_fines = n())

ggplot(case_fine) +
  geom_point(aes(date, fine))

cf_fine_sample <- case_fine |>
  mutate(days_since_covid = as.numeric(date) - 18337,
         case_type = str_sub(case_number, 1, 2)) |> 
  filter(case_type == "CF") |> 
  ungroup() |> 
  as.data.frame()

cf_fine_sample |> 
  count(days_since_covid > 0)

rdpower(data = cf_fine_sample[c("fine", 
                                "days_since_covid")],
        covs = cf_fine_sample[c("n_fines")],
        cluster = cf_fine_sample$district,
        tau = 300)

# H1: Fines and fees reduced at hearings #####
tic()
waive <- ojo_crim_cases(districts = oscn_districts,
                        case_types = c("CF", "CM"),
                        file_years = 2010:2022) |>
  select(id, district, case_number) |>
  distinct() |>
  left_join(ojo_tbl("minute"),
            by = c("id" = "case_id")) |>
  filter(code == "CTFREE",
         date >= "2016-01-01") |>
  select(district, case_number, date, code, description) |> 
  collect()
toc()

w <- waive |> 
  select(district, case_number, date, code, description) |> 
  filter(str_detect(description, "WAIVE|REDUCE") & str_detect(description, "FINES|COSTS")) |> 
  filter(!str_detect(description, "(DEF|DEFENDANT) WAIVE|ER (IS|WAS) WAIVE|WAIVE HIS RIGHT|WAIVE HER RIGHT")) |> 
  mutate(waive_str = str_extract(description, ".{30}WAIVE.{30}")) |> 
  filter(str_detect(waive_str, "COST|\\$"),
         !str_detect(waive_str, "TER (WAS |)WAIVED|WAIVER")) 

w |> 
  count(mo = floor_date(date, "month"),
        district) |> 
  ggplot(aes(mo, n, fill = district)) +
  geom_col() +
  labs(title = "Number of waivers of court costs")

# write_csv(w, "../av-power-calc/data/fee-waiver-data.csv")

w_mo <- w |> 
  count(mo = floor_date(date, "month")) |> 
  mutate(days_post_covid = as.numeric(mo) - 17713) |> 
  #filter(mo > ymd("202-05-01")) |> 
  as.data.frame()

rdpower(data = w_mo[c("n", "days_post_covid")])

# Total fines and fees ####
ff <- read_csv("data/2022-10-11-ff-sample.csv")

cases_ff <- cases |> 
  filter(has_costs == TRUE) |>
  left_join(read_csv("data/2022-10-11-ff-sample.csv")) |> 
  filter(n_fees >= 12)

cases_ff |> 
  filter(!is.na(total_fees)) |> 
  count(month = floor_date(disp_date, "month")) |> 
  ggplot(aes(month, n)) +
  geom_col()

sample_ff <- cases_ff |>
  filter(!is.na(total_fees),
         deferred + conviction > 0) |>
  mutate(disp_month = floor_date(disp_date, "month"),
         n_charges = deferred + conviction,
         days_post_covid = as.numeric(disp_month) - 18337,
         case_type = str_sub(case_number, 1, 2)) |>
  as.data.frame()

rdpower(data = sample_ff[c("total_fees", 
                           "days_post_covid")],
        covs = sample_ff[c("n_charges")],
        cluster = sample_ff$district,
        tau = 700)

# FTP warrants ####
# tic()
# ftp <- ojo_crim_cases(districts = oscn_districts,
#                       case_types = c("CM", "CF"),
#                       file_years = 2010:2022) |>
#   select(id, district, case_number) |>
#   distinct() |>
#   left_join(ojo_tbl("minute"),
#             by = c("id" = "case_id")) |>
#   filter(code %like% "BW%") |>
#   collect()
# toc()
# 
# ftp |> 
#   count(district)
# 
# tic()
# tul <- ojo_crim_cases(districts = "TULSA",
#                       case_types = c("CF"),
#                       file_years = 2010:2022) |>
#   select(id, district, case_number) |>
#   distinct() |>
#   left_join(ojo_tbl("minute"),
#             by = c("id" = "case_id")) |>
#   filter(code %in% c("BWIFAP",
#                      "BWIFP",
#                      "CTBWFTA",
#                      "CTFREE", 
#                      "CTPASS")) |> 
#   collect()
# toc()
# 
# tul_cost |> 
#   bind_rows(ftp) |> 
#   count(month = floor_date(date, "quarter")) |> 
#   ggplot(aes(month, n)) +
#   geom_col() +
#   xlim(ymd("2016-01-01"), ymd("2022-10-01"))
# 
# warrants <- ftp |> 
#   bind_rows(tul_cost) |> 
#   ungroup() |> 
#   filter(str_detect(description, "FAIL"),
#          str_detect(description, "COST|FINE|PAY"),
#          str_detect(description, "WARRANT")) |> 
#   filter(!str_detect(description, "RECALL")) |> 
#   select(district, case_number, code, date, description)
# 
# warrants |> 
#   count(year = year(date),
#         district) |> 
#   ggplot(aes(year, n, color = district)) +
#   geom_line() +
#   xlim(2016, 2022)
# 
# write_csv(warrants, "data/warrants-data.csv")

warrants <- read_csv("data/warrants-data.csv")

warrants_mo <- warrants |> 
  count(district, month = floor_date(date, "month")) |> 
  mutate(since_covid = as.numeric(month) - as.numeric(ymd("2020-03-01"))) |> 
  filter(year(month) >= 2016, month <= ymd("2022-01-01")) |> 
  as.data.frame()

rdpower(data = warrants_mo[c("n", 
                             "since_covid")],
        cluster = warrants_mo$district)

warrants_mo |> 
  ggplot(aes(month, n)) +
  geom_point()

# Number of payments received by court
# pays <- ojo_crim_cases(districts = oscn_districts,
#                        case_types = c("CM", "CF"),
#                        file_years = 2010:2022) |>
#   select(id, district, case_number) |>
#   distinct() |>
#   left_join(ojo_tbl("minute"),
#             by = c("id" = "case_id")) |>
#   filter(code == "ACCOUNT",
#          date >= "2016-01-01") |>
#   collect()

# write_rds(p, "data/pays-data.rds")

pays <- read_rds("data/pays-data.rds")

p <- pays |> 
  select(district, case_number, date, code, description) |> 
  filter(str_detect(description, "RECEIPT")) |> 
  mutate(amount = str_extract(description, "PAID:.*?LINE") |> 
           str_remove_all("[^\\d\\.]") |> 
           str_remove("\\.$") |> 
           as.numeric()) |> 
  filter(amount > 0,
         !str_detect(description, "TAX COM|INTERCEPT|FORFEIT|CASH BOND"),
         date < ymd("2022-04-01"))

pay_mo <- p |> 
  group_by(district, month = floor_date(date, "quarter")) |> 
  summarize(n = n(),
            amount = sum(amount)) |>
  mutate(since_covid = as.numeric(month) - as.numeric(ymd("2020-03-01")),
         quarter = quarter(month)) |>
  as.data.frame()

pay_mo |> 
  ggplot(aes(month, amount, color = district)) +
  geom_line()

# write_csv(pay_mo, "data/pay-summary-data.csv")

rdpower(data = pay_mo[c("n",
                        "since_covid")],
        covs = pay_mo$moy,
        cluster = pay_mo$district)

# Total court collections
rdpower(data = pay_mo[c("amount",
                        "since_covid")],
        covs = pay_mo$moy,
        cluster = pay_mo$district)


