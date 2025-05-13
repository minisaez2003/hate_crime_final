# STEP 1.1: INSTALLING DATA

df <- table13

#STEP 1.2: CLEANING THE DATA
library(dplyr)

# Start from your original dataframe
df_clean <- df

# 1. Convert 'Population' to numeric
df_clean <- df_clean %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))

# 2. Create 'Total_Complaints' by summing quarterly columns
df_clean <- df_clean %>%
  mutate(
    Total_Complaints = rowSums(select(., X1st.quarter, X2nd.quarter, X3rd.quarter, X4th.quarter), na.rm = TRUE)
  )

# 3. Drop rows with missing Population or Total_Complaints
df_clean <- df_clean %>%
  filter(!is.na(Population) & !is.na(Total_Complaints))

# 4. Convert 'Gender' to a factor
df_clean <- df_clean %>%
  mutate(Gender = as.factor(Gender))

# 5. (Optional) Reset row names to default
rownames(df_clean) <- NULL

# STEP 1.3: SPLITTING DATA

