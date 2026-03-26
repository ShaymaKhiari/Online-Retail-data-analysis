library(readxl)
df <- read_excel("Online Retail Cleaned.xlsx")


library(tidyverse)
library(readxl)
library(FactoMineR)
library(factoextra)
library(cluster)
library(arules)
library(arulesViz)





df <- read_excel("Online Retail Cleaned.xlsx")


df_clean <- df %>%
  filter(!is.na(`Customer ID`), Quantity > 0, Price > 0) %>%
  mutate(TotalSales = Quantity * Price)

set.seed(123)
df_sample <- df_clean %>% sample_frac(0.1)


ggplot(df_sample, aes(x = Price, y = Quantity)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Bivariate Analysis: Price vs Quantity",
       subtitle = "Log scale used for better visibility") +
  theme_minimal()


rfm_data <- df_sample %>%
  group_by(`Customer ID`) %>%
  summarise(
    Recency = as.numeric(as.Date("2012-01-01") - max(as.Date(InvoiceDate))),
    Frequency = n(),
    Monetary = sum(TotalSales)
  ) %>%
  column_to_rownames("Customer ID")


pca_res <- PCA(rfm_data, scale.unit = TRUE, graph = FALSE)
fviz_pca_var(pca_res, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

set.seed(123)
km_res <- kmeans(scale(rfm_data), centers = 3, nstart = 25)
fviz_cluster(km_res, data = scale(rfm_data), ellipse.type = "norm", main = "Customer Segmentation")


library(arules)



transactions_list <- transactions_list[sapply(transactions_list, length) > 0]


transactions <- transactions(transactions_list)

summary(transactions)

rules <- apriori(transactions, 
                 parameter = list(supp = 0.01, conf = 0.5, minlen = 2))


inspect(head(sort(rules, by = "lift"), 5))



