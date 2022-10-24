library(ojodb)
library(rdpower)

#cases <- read_csv("data/2022-10-11-cases-sample.csv")
#  Total fines at conviction ####
cf_fine_sample <- read_csv("data/felony-fines.csv") |> 
  as.data.frame()

rdpower(data = cf_fine_sample[c("fine", 
                                "days_since_covid")],
        covs = cf_fine_sample[c("n_fines")],
        cluster = cf_fine_sample$district,
        tau = 300)

# Total fines and fees at conviction ####
sample_ff <- read_csv("data/total-fees.csv") |> 
  as.data.frame()

rdpower(data = sample_ff[c("total_fees", 
                           "days_post_covid")],
        covs = sample_ff[c("n_charges")],
        cluster = sample_ff$district,
        tau = 700)

# FTP warrants ####
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
pay_mo <- read_csv("data/pay-summary-data.csv") |> 
  as.data.frame()

rdpower(data = pay_mo[c("n",
                        "since_covid")],
        covs = pay_mo$moy,
        cluster = pay_mo$district)

# Total court collections
rdpower(data = pay_mo[c("amount",
                        "since_covid")],
        covs = pay_mo$moy,
        cluster = pay_mo$district)

# Rates of new charges





