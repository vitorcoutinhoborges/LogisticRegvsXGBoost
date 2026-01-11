# Setup -------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(embed)   
library(tictoc)  
library(rsample)

# Paths e Ingestão --------------------------------------------------------
#setwd("C:/Users/Vitor Borges/Desktop/personal/portfolio/Linkedin/p1_risk/data")

app_train <- read_csv("application_train.csv")
bureau    <- read_csv("bureau.csv")

# Feature Engineering: Bureau Aggregation ---------------------------------
bureau_agg <- bureau %>%
  group_by(SK_ID_CURR) %>%
  summarise(
    nb_prolongamentos   = sum(CNT_CREDIT_PROLONG, na_rm = TRUE),
    total_divida_ativa  = sum(AMT_CREDIT_SUM_DEBT, na_rm = TRUE),
    media_overdue       = mean(AMT_CREDIT_MAX_OVERDUE, na_rm = TRUE),
    .groups = "drop"
  )

df_final <- app_train %>%
  left_join(bureau_agg, by = "SK_ID_CURR") %>%
  mutate(TARGET = as.factor(TARGET)) %>% 
  select(TARGET, AMT_INCOME_TOTAL, AMT_CREDIT, DAYS_BIRTH, 
         nb_prolongamentos, total_divida_ativa)

# Data Partitioning -------------------------------------------------------
set.seed(123)
data_split <- initial_split(df_final, prop = 0.75, strata = TARGET)
train_data <- training(data_split)
test_data  <- testing(data_split)

# Preprocessing Pipeline (Regulatory Compliance) --------------------------
risk_recipe <- recipe(TARGET ~ ., data = train_data) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_num2factor(
    nb_prolongamentos, 
    levels = as.character(unique(train_data$nb_prolongamentos))
  ) %>%
  step_discretize(all_numeric_predictors(), -nb_prolongamentos, num_breaks = 5) %>%
  step_woe(all_predictors(), outcome = vars(TARGET))

# Model Specifications ----------------------------------------------------
lr_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

xgb_spec <- boost_tree(trees = 500, tree_depth = 3, learn_rate = 0.05) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Workflow Integration ----------------------------------------------------
risk_wf <- workflow() %>% add_recipe(risk_recipe) %>% add_model(lr_spec)
perf_wf <- workflow() %>% add_recipe(risk_recipe) %>% add_model(xgb_spec)

# Training ----------------------------------------------------------------
tic("Training Models")
risk_fit <- fit(risk_wf, data = train_data)
perf_fit <- fit(perf_wf, data = train_data)
toc()

# Prediction & Performance Evaluation -------------------------------------
results_lr <- test_data %>%
  select(TARGET) %>%
  bind_cols(predict(risk_fit, test_data, type = "prob")) %>%
  mutate(model = "Logistic Regression (WoE)")

results_xgb <- test_data %>%
  select(TARGET) %>%
  bind_cols(predict(perf_fit, test_data, type = "prob")) %>%
  mutate(model = "XGBoost")

# Metrics Calculation (Gini & AUC) ----------------------------------------
metrics_summary <- bind_rows(results_lr, results_xgb) %>%
  group_by(model) %>%
  roc_auc(truth = TARGET, .pred_1, event_level = "second") %>%
  mutate(Gini = (.estimate * 2) - 1)

print(metrics_summary)

# Visualization: ROC Curve Comparison -------------------------------------
bind_rows(results_lr, results_xgb) %>%
  group_by(model) %>%
  roc_curve(truth = TARGET, .pred_1, event_level = "second") %>%
  autoplot() +
  labs(
    title = "Model Comparison: Probability of Default (PD)",
    subtitle = "Benchmark: Interpretable Logistic Regression vs. XGBoost",
    x = "1 - Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")




library(dplyr)
library(forcats)


## WOE Age Plot

woe_plot_final <- woe_plot_data %>%
  mutate(age_label = case_when(
    value == "bin1" ~ "> 56 anos",
    value == "bin2" ~ "47 - 56 anos",
    value == "bin3" ~ "39 - 47 anos",
    value == "bin4" ~ "32 - 39 anos",
    value == "bin5" ~ "< 32 anos",
    TRUE ~ value
  ))

ggplot(woe_plot_final, aes(x = fct_reorder(age_label, woe), y = woe)) +
  geom_col(fill = "#2c3e50", alpha = 0.8) +
  geom_label(aes(label = round(woe, 2)), vjust = 1.5, size = 3.5, fill = "white", fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Impacto da Idade no Risco de Crédito (WoE)",
    subtitle = "Quanto maior o peso (WoE), menor a probabilidade de Default",
    x = "Faixas Etárias",
    y = "Weight of Evidence (WoE)",
    caption = "Nota: Valores negativos indicam grupos com risco acima da média."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"))




library(dplyr)
library(forcats)

# 1. Criar a legenda amigável para os bins
# Baseado nos limites que extraímos (32, 40, 47, 56 anos)
woe_plot_final <- woe_plot_data %>%
  mutate(age_label = case_when(
    value == "bin1" ~ "> 56 anos",
    value == "bin2" ~ "47 - 56 anos",
    value == "bin3" ~ "39 - 47 anos",
    value == "bin4" ~ "32 - 39 anos",
    value == "bin5" ~ "< 32 anos",
    TRUE ~ value
  ))

# 2. Gerar o Gráfico com os labels novos
ggplot(woe_plot_final, aes(x = fct_reorder(age_label, woe), y = woe)) +
  geom_col(fill = "#2c3e50", alpha = 0.8) +
  geom_label(aes(label = round(woe, 2)), vjust = 1.5, size = 3.5, fill = "white", fontface = "bold") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Impacto da Idade no Risco de Crédito (WoE)",
    subtitle = "Quanto maior o peso (WoE), menor a probabilidade de Default",
    x = "Faixas Etárias",
    y = "Weight of Evidence (WoE)",
    caption = "Nota: Valores negativos indicam grupos com risco acima da média."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )



## PLOT Caixa de Vidro vs. Caixa Negra
library(vip)
library(patchwork)
library(tidyverse)

lr_data <- tidy(risk_fit) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = str_replace(term, "woe_", ""),
    impacto = ifelse(estimate > 0, "Aumenta Risco", "Reduz Risco")
  )

p1 <- ggplot(lr_data, aes(x = estimate, y = fct_reorder(term, abs(estimate)), fill = impacto)) +
  geom_col() +
  scale_fill_manual(values = c("Reduz Risco" = "#27ae60", "Aumenta Risco" = "#c0392b")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Regressão Logística: O Caminho",
    subtitle = "Coeficientes indicam a DIREÇÃO do risco",
    x = "Impacto (Coeficiente Beta)",
    y = NULL,
    fill = "Sentido do Risco"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- perf_fit %>%
  extract_fit_parsnip() %>%
  vi() %>%
  mutate(Variable = str_replace(Variable, "woe_", "")) %>%
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance))) +
  geom_col(fill = "#2c3e50") +
  labs(
    title = "XGBoost: O Supercomputador",
    subtitle = "Importância indica a MAGNITUDE global",
    x = "Importância (Gain/Inpurity)",
    y = NULL
  ) +
  theme_minimal()

(p1 | p2) + 
  plot_annotation(
    title = "Interpretabilidade: Caixa de Vidro vs. Caixa Negra",
    subtitle = "Enquanto a Regressão diz para onde ir, o XGBoost diz apenas o que é relevante.",
    caption = "Análise PD - Modelo de Risco IFRS9",
    theme = theme(plot.title = element_text(size = 18, face = "bold"))
  )


p2
