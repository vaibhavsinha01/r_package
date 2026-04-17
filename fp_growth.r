# -------------------------------
# ECLAT Algorithm (arules)
# -------------------------------

# Install & load package
install.packages("arules")
library(arules)

# -------------------------------
# 1. Create dataset
# -------------------------------
transactions_list <- list(
  c("Laptop","Mouse","Keyboard"),
  c("Laptop","Mouse"),
  c("Mouse","Keyboard"),
  c("Laptop","Keyboard"),
  c("Laptop","Mouse","Keyboard"),
  c("Mouse")
)

trans <- as(transactions_list, "transactions")

inspect(trans)

# -------------------------------
# 2. Apply ECLAT (support = 40%)
# -------------------------------
freq_eclat_40 <- eclat(
  trans,
  parameter = list(supp = 0.4)
)

# -------------------------------
# 3. Frequent itemsets
# -------------------------------
inspect(freq_eclat_40)

# -------------------------------
# 4. Most frequent item
# -------------------------------
itemFrequency(trans)

# Expected:
# Mouse = 0.83 (most frequent)

# -------------------------------
# 5. Generate rules (from itemsets)
# -------------------------------
rules <- ruleInduction(freq_eclat_40, trans, confidence = 0.6)

inspect(rules)

# -------------------------------
# 6. Support for {Laptop, Mouse}
# -------------------------------
# 3 out of 6 transactions
3/6   # 0.5

# -------------------------------
# 7. Confidence for Laptop → Mouse
# -------------------------------
# = support(Laptop,Mouse) / support(Laptop)
# = 3/4 = 0.75
3/4

# -------------------------------
# 8. Increase support to 60%
# -------------------------------
freq_eclat_60 <- eclat(
  trans,
  parameter = list(supp = 0.6)
)

inspect(freq_eclat_60)

# -------------------------------
# 9. (Optional) Simple visualization
# -------------------------------
install.packages("arulesViz")
library(arulesViz)

plot(freq_eclat_40, method = "itemFrequency")

# -------------------------------
# 10. Insights
# -------------------------------
# - Mouse is the most frequent item
# - Laptop → Mouse has high confidence (75%)
# - Strong co-occurrence: Laptop & Mouse
# - Good bundling opportunity for accessories