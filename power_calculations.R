library(tidyverse)
library(rdpower)

# Hypothesis 1: Total fines at conviction ####
fines <- read_csv("data/fines-data.csv")

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

# H1: Total fines on felony cases
cf_fine_sample <- case_fine |>
  mutate(since_covid = as.numeric(date) - 18337,
         case_type = str_sub(case_number, 1, 2)) |> 
  filter(case_type == "CF") |> 
  ungroup() |> 
  as.data.frame()

rdpower(data = cf_fine_sample[c("fine", 
                                "since_covid")],
        covs = cf_fine_sample[c("n_fines")],
        cluster = cf_fine_sample$district,
        tau = 300)

# H1: Total fines on misdemeanor cases
cm_fine_sample <- case_fine |>
  mutate(since_covid = as.numeric(date) - 18337,
         case_type = str_sub(case_number, 1, 2)) |> 
  filter(case_type == "CM") |> 
  ungroup() |> 
  as.data.frame()

cm_fine_sample |> 
  count(floor_date(date, "mo")) |> 
  view()

rdpower(data = cm_fine_sample[c("fine", 
                                "since_covid")],
        covs = cm_fine_sample[c("n_fines")],
        cluster = cm_fine_sample$district,
        tau = 300)

# H1: Total fines and fees on felony ####
total_fees <- read_csv("data/total-fees.csv")

cf_ff <- total_fees |> 
  filter(str_detect(case_number, "CF")) |> 
  mutate(across(contains("date"), mdy)) |> 
  as.data.frame()

rdpower(data = cf_ff[c("total_fees", 
                           "since_covid")],
        covs = cf_ff[c("n_charges")],
        cluster = cf_ff$district,
        tau = 700)

cm_ff <-total_fees |> 
  filter(str_detect(case_number, "CM")) |> 
  mutate(across(contains("date"), mdy)) |> 
  as.data.frame()

rdpower(data = cm_ff[c("total_fees", 
                           "since_covid")],
        covs = cm_ff[c("n_charges")],
        cluster = cm_ff$district,
        tau = 500)

# H1: Fee waivers
waive <- read_csv("data/fee-waiver-data.csv") |> 
  filter(date <= ymd("2020-09-30")) |> 
  count(month = floor_date(date, "month")) |> 
  mutate(since_covid = as.numeric(month) - 18377) |> 
  as.data.frame()

ggplot(waive, aes(month, n)) +
  geom_line() +
  labs(title= "Number of fee waivers by month")

rdpower(data = waive[c("n", "since_covid")])

# H2: FTP warrants ####
warrants <- read_csv("data/warrants-data.csv")

warrants_mo <- warrants |> 
  filter(date <= ymd("2020-09-30")) |> 
  count(district, month = floor_date(date, "month")) |> 
  mutate(since_covid = as.numeric(month) - as.numeric(ymd("2020-03-01"))) |> 
  filter(year(month) >= 2016, month <= ymd("2022-01-01")) |> 
  as.data.frame()

ggplot(warrants_mo, aes(month, n, color = district)) +
  geom_line()

rdpower(data = warrants_mo[c("n", 
                             "since_covid")],
        cluster = warrants_mo$district)

# H3: Number of payments received by court
pay_mo <- read_csv("data/pay-summary-data.csv") |> 
  as.data.frame()

ggplot(pay_mo, aes(month, n, color = district)) +
  geom_line() +
  labs(title= "Number of payments by month")

rdpower(data = pay_mo[c("n",
                        "since_covid")],
        covs = pay_mo$moy,
        cluster = pay_mo$district)

# H4: Total court collections
ggplot(pay_mo, aes(month, amount, color = district)) +
  geom_line() +
  labs(title= "Total amount of payments by month")

rdpower(data = pay_mo[c("amount",
                        "since_covid")],
        covs = pay_mo$moy,
        cluster = pay_mo$district)

