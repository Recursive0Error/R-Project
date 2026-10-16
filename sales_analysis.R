# =====================================================================
# Statistical Data Analysis using R - FA-2 Activity
# Project : Retail Sales Analysis (sales_data.csv, 20,000 orders, INR)
# Packages: dplyr, tidyr, ggplot2, lubridate, scales
# =====================================================================
# install.packages(c("dplyr","tidyr","ggplot2","lubridate","scales"))
library(dplyr); library(tidyr); library(ggplot2); library(lubridate); library(scales)

dir.create("figs", showWarnings = FALSE)

# ---------------------------------------------------------------------
# 0. LOAD AND PREPARE DATA
# ---------------------------------------------------------------------
df <- read.csv("sales_data.csv", stringsAsFactors = FALSE)
df$order_date <- as.Date(df$order_date)
df$month <- month(df$order_date)
df$year  <- year(df$order_date)
str(df); summary(df)
colSums(is.na(df))                       # only rating has missing values (~1%)

# Rating imputation (median) - used only where rating is analysed
df$rating[is.na(df$rating)] <- median(df$rating, na.rm = TRUE)

# Derived variables
df$margin_order <- df$profit / df$sales
df$band <- cut(df$discount, breaks = c(-0.001, 0, 0.10, 0.20, 0.30),
               labels = c("0%", "1-10%", "11-20%", "21-30%"))
df$age_group <- cut(df$age, breaks = c(17, 25, 35, 45, 55, 65),
                    labels = c("18-25", "26-35", "36-45", "46-55", "56-65"))

# ---------------------------------------------------------------------
# PROBLEM 1: Discount vs quantity per order and profit per order
# ---------------------------------------------------------------------
p1 <- df %>% group_by(band) %>%
  summarise(orders = n(), avg_qty = mean(quantity), avg_profit = mean(profit),
            margin = sum(profit) / sum(sales))
print(p1)

# Share of loss-making orders at each discount level
loss <- df %>% group_by(discount) %>%
  summarise(orders = n(), pct_loss = 100 * mean(profit < 0))
print(loss)

kruskal.test(quantity ~ band, data = df)           # does basket size differ?
cor.test(df$discount, df$quantity, method = "spearman")
cor.test(df$discount, df$profit)                   # Pearson

ggplot(p1, aes(band, avg_qty)) + geom_col(fill = "#4C78A8") +
  labs(title = "Average quantity per order", x = "Discount band", y = "Quantity") +
  theme_minimal()
ggsave("figs/p1_qty.png", width = 5, height = 3.5)
ggplot(p1, aes(band, avg_profit)) + geom_col(fill = "#F58518") +
  labs(title = "Average profit per order (INR)", x = "Discount band", y = "Profit") +
  theme_minimal()
ggsave("figs/p1_profit.png", width = 5, height = 3.5)
ggplot(loss, aes(factor(discount), pct_loss)) + geom_col(fill = "#E45756") +
  labs(title = "Share of loss-making orders by discount", x = "Discount", y = "% of orders") +
  theme_minimal()
ggsave("figs/p1_loss.png", width = 5.5, height = 3.5)

# ---------------------------------------------------------------------
# PROBLEM 2: Channel profit margin and channel x category pivot
# ---------------------------------------------------------------------
p2 <- df %>% group_by(channel) %>%
  summarise(orders = n(), sales = sum(sales), profit = sum(profit),
            margin = 100 * sum(profit) / sum(sales))
print(p2)

p2b <- df %>% group_by(channel, category) %>%
  summarise(margin = 100 * sum(profit) / sum(sales), .groups = "drop")
print(pivot_wider(p2b, names_from = category, values_from = margin))

kruskal.test(margin_order ~ channel, data = df)

ggplot(p2b, aes(category, channel, fill = margin)) + geom_tile() +
  geom_text(aes(label = round(margin, 2))) +
  scale_fill_gradient(low = "#F7FCB9", high = "#31A354") +
  labs(title = "Profit margin (%) : channel x category", x = "", y = "") +
  theme_minimal()
ggsave("figs/p2_heatmap.png", width = 8, height = 3.5)

# ---------------------------------------------------------------------
# PROBLEM 3: Products with high sales but low profit margin
# ---------------------------------------------------------------------
p3 <- df %>% group_by(product, category) %>%
  summarise(sales = sum(sales), profit = sum(profit), .groups = "drop") %>%
  mutate(margin = 100 * profit / sales)
med_s <- median(p3$sales); med_m <- median(p3$margin)
p3$flag <- ifelse(p3$sales >= med_s & p3$margin < med_m, "High sales, low margin", "Other")
print(p3 %>% arrange(desc(sales)))
print(filter(p3, flag != "Other"))

ggplot(p3, aes(sales / 1e6, margin, colour = category)) +
  geom_point(size = 3) + scale_x_log10() +
  geom_vline(xintercept = med_s / 1e6, linetype = "dashed") +
  geom_hline(yintercept = med_m, linetype = "dashed") +
  geom_text(data = filter(p3, flag != "Other"), aes(label = product),
            size = 3, vjust = 1.8, colour = "firebrick") +
  labs(title = "Sales vs profit margin by product",
       x = "Total sales (INR million, log scale)", y = "Profit margin (%)") +
  theme_minimal()
ggsave("figs/p3_scatter.png", width = 8, height = 5)

# ---------------------------------------------------------------------
# PROBLEM 4: Payment mix by age group and channel
# ---------------------------------------------------------------------
tab_age  <- table(df$age_group, df$payment_method)
tab_chan <- table(df$channel,   df$payment_method)
round(prop.table(tab_age,  1) * 100, 1)
round(prop.table(tab_chan, 1) * 100, 1)

chi_age  <- chisq.test(tab_age);  print(chi_age)
chi_chan <- chisq.test(tab_chan); print(chi_chan)
cramers_v <- function(t, chi) sqrt(unname(chi$statistic) / (sum(t) * (min(dim(t)) - 1)))
cramers_v(tab_age, chi_age); cramers_v(tab_chan, chi_chan)

mix <- bind_rows(
  as.data.frame(prop.table(tab_age,  1) * 100) %>% mutate(type = "Age group"),
  as.data.frame(prop.table(tab_chan, 1) * 100) %>% mutate(type = "Channel"))
names(mix)[1:3] <- c("group", "payment", "pct")
ggplot(mix, aes(group, pct, fill = payment)) + geom_col() +
  facet_wrap(~type, scales = "free_x") +
  labs(title = "Payment method mix (%)", x = "", y = "% of orders", fill = "Payment") +
  theme_minimal()
ggsave("figs/p4_payment.png", width = 10, height = 4)

# ---------------------------------------------------------------------
# PROBLEM 5: Seasonality and the Oct-Nov festive peak by category
# ---------------------------------------------------------------------
monthly <- df %>% group_by(category, year, month) %>%
  summarise(sales = sum(sales), .groups = "drop") %>%
  mutate(festive = month %in% c(10, 11))

fest <- monthly %>% group_by(category, festive) %>%
  summarise(avg_month_sales = mean(sales), .groups = "drop") %>%
  pivot_wider(names_from = festive, values_from = avg_month_sales) %>%
  mutate(festive_index = `TRUE` / `FALSE`)
print(fest)

for (ct in unique(monthly$category)) {          # Welch t-test per category
  x <- filter(monthly, category == ct)
  cat(ct, ": p =", t.test(sales ~ festive, data = x)$p.value, "\n")
}

season <- df %>% group_by(category, month) %>%
  summarise(sales = sum(sales) / 3, .groups = "drop") %>%
  group_by(category) %>% mutate(index = sales / mean(sales))
ggplot(season, aes(month, index, colour = category)) +
  annotate("rect", xmin = 9.5, xmax = 11.5, ymin = -Inf, ymax = Inf, alpha = .2, fill = "orange") +
  geom_line() + geom_point() + scale_x_continuous(breaks = 1:12) +
  labs(title = "Seasonality by category (average of 2023-25)",
       x = "Month", y = "Sales index (category average month = 1)") +
  theme_minimal()
ggsave("figs/p5_season.png", width = 9, height = 4)

# ---------------------------------------------------------------------
# PROBLEM 6: Repeat vs one-time buyers
# ---------------------------------------------------------------------
cust <- df %>% group_by(customer_id) %>%
  summarise(orders = n(), spend = sum(sales), profit = sum(profit)) %>%
  mutate(type = ifelse(orders > 1, "Repeat", "One-time"))
mean(cust$orders > 1)                            # repeat-buyer share
table(cust$orders)

df <- left_join(df, select(cust, customer_id, type), by = "customer_id")
df %>% group_by(type) %>%
  summarise(orders = n(), aov = mean(sales), median_aov = median(sales),
            avg_discount = mean(discount), avg_qty = mean(quantity))
cust %>% group_by(type) %>%
  summarise(customers = n(), avg_orders = mean(orders),
            avg_spend = mean(spend), avg_profit = mean(profit))

t.test(sales ~ type, data = df)                  # order value
wilcox.test(sales ~ type, data = df)
t.test(profit ~ type, data = cust)               # lifetime profit per customer

ggplot(cust, aes(factor(orders))) + geom_bar(fill = "#4C78A8") +
  labs(title = "Customers by number of orders", x = "Orders per customer", y = "Customers") +
  theme_minimal()
ggsave("figs/p6_orders.png", width = 6, height = 3.5)
