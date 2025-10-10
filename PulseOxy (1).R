# At R startup (before loading tidyverse)
memory.limit(size = 128000)   # sets to 128 GB
library(tidyverse)

devices <- read_csv("devices.csv")
pulseox <- read_csv("pulseoximeter.csv")
bloodgas <- read_csv("bloodgas.csv")
patient <- read_csv("patient.csv")

pulseox <- pulseox %>% mutate(saturation = as.numeric(saturation))
bloodgas <- bloodgas %>% mutate(so2 = as.numeric(so2))

bg <- bloodgas %>%
  filter(!is.na(so2)) %>%
  group_by(encounter_id, patient_id) %>%
  summarise(saO2_mean = mean(so2), saO2_median = median(so2))

px <- pulseox %>%
  filter(!is.na(saturation)) %>%
  group_by(encounter_id) %>%
  summarise(spO2_mean = mean(saturation), spO2_median = median(saturation))

merged <- bg %>%
  inner_join(px, by="encounter_id") %>%
  left_join(patient %>% select(patient_id, race, assigned_sex, ethnicity), by="patient_id") %>%
  mutate(delta_mean = spO2_mean - saO2_mean) %>%
  filter(between(saO2_mean,50,100), between(spO2_mean,50,100), !is.na(race))

summary <- merged %>%
  group_by(race) %>%
  summarise(
    n = n(),
    bias_mean = mean(delta_mean),
    MAE = mean(abs(delta_mean)),
    RMSE = sqrt(mean(delta_mean^2))
  ) %>%
  arrange(desc(n))

ggplot(merged, aes(x=race, y=delta_mean, fill=race)) +
  geom_boxplot() + geom_hline(yintercept=0, linetype="dashed") +
  labs(y="Bias (SpO₂ − SaO₂, %)", x="Race", title="Pulse Oximeter Bias by Race") +
  theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=1))

ggplot(merged, aes(x=saO2_mean, y=spO2_mean, color=race)) +
  geom_point(alpha=0.6) +
  geom_abline(linetype="dashed") +
  geom_vline(xintercept=88, linetype="dotted", color="red") +
  geom_hline(yintercept=92, linetype="dotted", color="blue") +
  labs(x="SaO₂ (%)", y="SpO₂ (%)", title="SpO₂ vs SaO₂ by Race") +
  theme_bw()


# ================================================================
# Pulse oximetry accuracy by monk_fingernail (Device type = 2 only)
# ================================================================

library(data.table)
library(ggplot2)

# --- 1. Load data efficiently ---
bloodgas   <- fread("bloodgas.csv", select = c("patient_id","encounter_id","so2"))
pulseox    <- fread("pulseoximeter.csv", select = c("encounter_id","device","saturation"))
encounter  <- fread("encounter.csv", select = c("patient_id","encounter_id","monk_fingernail"))
patient    <- fread("patient.csv", select = c("patient_id","assigned_sex","ethnicity","race"))
devices    <- fread("devices.csv", select = c("device_number","device_type"))

# --- 2. Identify devices of type = 2 ---
device_ids_type2 <- devices[device_type == 2, unique(device_number)]

# --- 3. Keep only pulse oximeter readings from those devices ---
pulseox <- pulseox[device %in% device_ids_type2]

# --- 4. Convert numeric fields ---
bloodgas[, so2 := as.numeric(so2)]
pulseox[, saturation := as.numeric(saturation)]

# --- 5. Summarize within encounters ---
bg <- bloodgas[!is.na(so2), .(
  saO2_mean   = mean(so2),
  saO2_median = median(so2)
), by = .(encounter_id, patient_id)]

px <- pulseox[!is.na(saturation), .(
  spO2_mean   = mean(saturation),
  spO2_median = median(saturation)
), by = encounter_id]

# --- 6. Merge all tables together ---
merged <- merge(bg, px, by = "encounter_id")
merged <- merge(merged, encounter, by = c("patient_id","encounter_id"), all.x = TRUE)
merged <- merge(merged, patient, by = "patient_id", all.x = TRUE)

# --- 7. Clean and filter ---
merged <- merged[between(saO2_mean, 50, 100) & between(spO2_mean, 50, 100)]
merged <- merged[!is.na(monk_fingernail) & monk_fingernail != ""]
merged[, monk_fingernail := as.factor(trimws(monk_fingernail))]

# --- 8. Compute bias (SpO₂ − SaO₂) ---
merged[, delta_mean := spO2_mean - saO2_mean]

# --- 9. Summarize by monk_fingernail category ---
summary <- merged[, .(
  n_encounters = .N,
  bias_mean  = mean(delta_mean, na.rm=TRUE),
  bias_median = median(delta_mean, na.rm=TRUE),
  MAE = mean(abs(delta_mean), na.rm=TRUE),
  RMSE = sqrt(mean(delta_mean^2, na.rm=TRUE)),
  over_3pts_pct  = mean(delta_mean > 3)*100,
  under_3pts_pct = mean(delta_mean < -3)*100
), by = monk_fingernail][order(-n_encounters)]

fwrite(summary, "summary_bias_by_monk_fingernail_device2.csv")
fwrite(merged,  "merged_analysis_by_monk_fingernail_device2.csv")

print(summary)

# --- 10. Visualizations ---
theme_set(theme_bw(base_size = 13))

## (A) Boxplot
p1 <- ggplot(merged, aes(x = monk_fingernail, y = delta_mean, fill = monk_fingernail)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pulse Oximeter Bias by Monk Fingernail Tone (Device Type 2)",
    x = "Monk Fingernail Category",
    y = "Bias (SpO2 - SaO2, %)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("boxplot_bias_by_monk_fingernail_device2.pdf", plot = p1, width = 8, height = 6)


## (B) Bar chart
p2 <- ggplot(summary, aes(x = monk_fingernail, y = bias_mean, fill = monk_fingernail)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Mean Oximeter Bias by Monk Fingernail Tone (Device Type 2)",
    x = "Monk Fingernail Category",
    y = "Mean Bias (SpO2 - SaO2, %)"
  ) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("bar_bias_by_monk_fingernail_device2.pdf", plot = p2, width = 8, height = 6)


## (C) Scatter
p3 <- ggplot(merged, aes(x = saO2_mean, y = spO2_mean, color = monk_fingernail)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed") +
  geom_vline(xintercept = 88, color = "red", linetype = "dotted") +
  geom_hline(yintercept = 92, color = "blue", linetype = "dotted") +
  labs(
    title = "SpO2 vs SaO2 by Monk Fingernail Tone (Device Type 2)",
    x = "SaO2 (Blood Gas, %)",
    y = "SpO2 (Pulse Oximeter, %)",
    color = "Fingernail Tone"
  )

ggsave("scatter_SpO2_vs_SaO2_by_monk_fingernail_device2.pdf", plot = p3, width = 7, height = 6)


## (D) Bland–Altman plots for top tones
top_groups <- summary$monk_fingernail[1:min(4, nrow(summary))]
for (tone in top_groups) {
  sub <- merged[monk_fingernail == tone]
  mean_val <- (sub$spO2_mean + sub$saO2_mean) / 2
  diff_val <- sub$delta_mean
  bias <- mean(diff_val)
  sd <- sd(diff_val)
  
  p <- ggplot(sub, aes(x = mean_val, y = diff_val)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = bias, linetype = "dashed", color = "red") +
    geom_hline(yintercept = bias + 1.96*sd, linetype = "dotted") +
    geom_hline(yintercept = bias - 1.96*sd, linetype = "dotted") +
    labs(
      title = paste0("Bland–Altman Plot — ", tone, " (Device Type 2)"),
      subtitle = sprintf("Bias = %.2f%%, LoA [%.2f, %.2f], n=%d",
                         bias, bias - 1.96*sd, bias + 1.96*sd, nrow(sub)),
      x = "Mean of SpO2 & SaO2 (%)",
      y = "Difference (SpO2 - SaO2)"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(sprintf("bland_altman_%s_device2.pdf", tone), plot = p, width = 7, height = 5)
}

cat("\n✅ Analysis complete. Outputs saved as:\n",
    "- summary_bias_by_monk_fingernail_device2.csv\n",
    "- merged_analysis_by_monk_fingernail_device2.csv\n",
    "- boxplot_bias_by_monk_fingernail_device2.pdf\n",
    "- bar_bias_by_monk_fingernail_device2.pdf\n",
    "- scatter_SpO2_vs_SaO2_by_monk_fingernail_device2.pdf\n",
    "- bland_altman_<tone>_device2.pdf\n")


# ================================================================
# Pulse oximetry bias by monk_fingernail × sex (Device type = 2)
# ================================================================

library(data.table)
library(ggplot2)

# --- 1. Load data efficiently ---
bloodgas   <- fread("bloodgas.csv", select = c("patient_id","encounter_id","so2"))
pulseox    <- fread("pulseoximeter.csv", select = c("encounter_id","device","saturation"))
encounter  <- fread("encounter.csv", select = c("patient_id","encounter_id","monk_fingernail"))
patient    <- fread("patient.csv", select = c("patient_id","assigned_sex","ethnicity","race"))
devices    <- fread("devices.csv", select = c("device_number","device_type"))

# --- 2. Identify devices of type = 2 ---
device_ids_type2 <- devices[device_type == 2, unique(device_number)]

# --- 3. Keep only pulse oximeter readings from those devices ---
pulseox <- pulseox[device %in% device_ids_type2]

# --- 4. Convert numeric fields ---
bloodgas[, so2 := as.numeric(so2)]
pulseox[, saturation := as.numeric(saturation)]

# --- 5. Summarize within encounters ---
bg <- bloodgas[!is.na(so2), .(
  saO2_mean   = mean(so2),
  saO2_median = median(so2)
), by = .(encounter_id, patient_id)]

px <- pulseox[!is.na(saturation), .(
  spO2_mean   = mean(saturation),
  spO2_median = median(saturation)
), by = encounter_id]

# --- 6. Merge all tables together ---
merged <- merge(bg, px, by = "encounter_id")
merged <- merge(merged, encounter, by = c("patient_id","encounter_id"), all.x = TRUE)
merged <- merge(merged, patient, by = "patient_id", all.x = TRUE)

# --- 7. Clean and filter ---
merged <- merged[between(saO2_mean, 50, 100) & between(spO2_mean, 50, 100)]
merged <- merged[!is.na(monk_fingernail) & monk_fingernail != ""]
merged <- merged[!is.na(assigned_sex) & assigned_sex != ""]
merged[, monk_fingernail := as.factor(trimws(monk_fingernail))]
merged[, assigned_sex := as.factor(trimws(assigned_sex))]

# --- 8. Compute bias (SpO₂ − SaO₂) ---
merged[, delta_mean := spO2_mean - saO2_mean]

# --- 9. Summarize by monk_fingernail × sex ---
summary <- merged[, .(
  n_encounters = .N,
  bias_mean  = mean(delta_mean, na.rm=TRUE),
  bias_median = median(delta_mean, na.rm=TRUE),
  MAE = mean(abs(delta_mean), na.rm=TRUE),
  RMSE = sqrt(mean(delta_mean^2, na.rm=TRUE))
), by = .(monk_fingernail, assigned_sex)][order(monk_fingernail, assigned_sex)]

fwrite(summary, "summary_bias_by_monk_fingernail_sex_device2.csv")
fwrite(merged,  "merged_analysis_by_monk_fingernail_sex_device2.csv")

print(summary)

# --- 10. Visualizations ---
theme_set(theme_bw(base_size = 13))

## (A) Boxplot stratified by sex
p1 <- ggplot(merged, aes(x = monk_fingernail, y = delta_mean, fill = assigned_sex)) +
  geom_boxplot(outlier.alpha = 0.3, position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pulse Oximeter Bias by Monk Fingernail Tone and Sex (Device Type 2)",
    x = "Monk Fingernail Category",
    y = "Bias (SpO2 - SaO2, %)",
    fill = "Sex"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("boxplot_bias_by_monk_fingernail_sex_device2.pdf", plot = p1, width = 9, height = 6)


## (B) Mean bias bar chart
p2 <- ggplot(summary, aes(x = monk_fingernail, y = bias_mean, fill = assigned_sex)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Mean Oximeter Bias by Monk Fingernail Tone and Sex (Device Type 2)",
    x = "Monk Fingernail Category",
    y = "Mean Bias (SpO2 - SaO2, %)",
    fill = "Sex"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("bar_bias_by_monk_fingernail_sex_device2.pdf", plot = p2, width = 9, height = 6)


## (C) Scatter plot colored by tone, shaped by sex
p3 <- ggplot(merged, aes(x = saO2_mean, y = spO2_mean,
                         color = monk_fingernail, shape = assigned_sex)) +
  geom_point(alpha = 0.6) +
  geom_abline(linetype = "dashed") +
  geom_vline(xintercept = 88, color = "red", linetype = "dotted") +
  geom_hline(yintercept = 92, color = "blue", linetype = "dotted") +
  labs(
    title = "SpO2 vs SaO2 by Fingernail Tone and Sex (Device Type 2)",
    x = "SaO2 (Blood Gas, %)",
    y = "SpO2 (Pulse Oximeter, %)",
    color = "Fingernail Tone",
    shape = "Sex"
  )

ggsave("scatter_SpO2_vs_SaO2_by_monk_fingernail_sex_device2.pdf", plot = p3, width = 8, height = 6)


## (D) Bland–Altman plots (still grouped by tone)
top_groups <- summary$monk_fingernail[1:min(4, length(unique(summary$monk_fingernail)))]
for (tone in top_groups) {
  sub <- merged[monk_fingernail == tone]
  mean_val <- (sub$spO2_mean + sub$saO2_mean) / 2
  diff_val <- sub$delta_mean
  bias <- mean(diff_val)
  sd <- sd(diff_val)
  
  p <- ggplot(sub, aes(x = mean_val, y = diff_val, color = assigned_sex)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = bias, linetype = "dashed", color = "red") +
    geom_hline(yintercept = bias + 1.96*sd, linetype = "dotted") +
    geom_hline(yintercept = bias - 1.96*sd, linetype = "dotted") +
    labs(
      title = paste0("Bland–Altman Plot — ", tone, " (Device Type 2)"),
      subtitle = sprintf("Bias = %.2f%%, LoA [%.2f, %.2f], n=%d",
                         bias, bias - 1.96*sd, bias + 1.96*sd, nrow(sub)),
      x = "Mean of SpO2 & SaO2 (%)",
      y = "Difference (SpO2 - SaO2)",
      color = "Sex"
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(sprintf("bland_altman_%s_sex_device2.pdf", tone), plot = p, width = 7, height = 5)
}

cat("\n✅ Analysis complete. Outputs saved as:\n",
    "- summary_bias_by_monk_fingernail_sex_device2.csv\n",
    "- merged_analysis_by_monk_fingernail_sex_device2.csv\n",
    "- boxplot_bias_by_monk_fingernail_sex_device2.pdf\n",
    "- bar_bias_by_monk_fingernail_sex_device2.pdf\n",
    "- scatter_SpO2_vs_SaO2_by_monk_fingernail_sex_device2.pdf\n",
    "- bland_altman_<tone>_sex_device2.pdf\n")


# ================================================================
# Pulse oximetry bias by monk_fingernail adjusted for sex
# (Device type = 2 only)
# ================================================================

library(data.table)
library(ggplot2)
library(broom)       # for tidy model summaries
library(dplyr)

# --- 1. Load data efficiently ---
bloodgas   <- fread("bloodgas.csv", select = c("patient_id","encounter_id","so2"))
pulseox    <- fread("pulseoximeter.csv", select = c("encounter_id","device","saturation"))
encounter  <- fread("encounter.csv", select = c("patient_id","encounter_id","monk_fingernail"))
patient    <- fread("patient.csv", select = c("patient_id","assigned_sex","ethnicity","race"))
devices    <- fread("devices.csv", select = c("device_number","device_type"))

# --- 2. Identify devices of type = 2 ---
device_ids_type2 <- devices[device_type == 2, unique(device_number)]

# --- 3. Keep only pulse oximeter readings from those devices ---
pulseox <- pulseox[device %in% device_ids_type2]

# --- 4. Convert numeric fields ---
bloodgas[, so2 := as.numeric(so2)]
pulseox[, saturation := as.numeric(saturation)]

# --- 5. Summarize within encounters ---
bg <- bloodgas[!is.na(so2), .(
  saO2_mean   = mean(so2),
  saO2_median = median(so2)
), by = .(encounter_id, patient_id)]

px <- pulseox[!is.na(saturation), .(
  spO2_mean   = mean(saturation),
  spO2_median = median(saturation)
), by = encounter_id]

# --- 6. Merge all tables together ---
merged <- merge(bg, px, by = "encounter_id")
merged <- merge(merged, encounter, by = c("patient_id","encounter_id"), all.x = TRUE)
merged <- merge(merged, patient, by = "patient_id", all.x = TRUE)

# --- 7. Clean and filter ---
merged <- merged[between(saO2_mean, 50, 100) & between(spO2_mean, 50, 100)]
merged <- merged[!is.na(monk_fingernail) & monk_fingernail != ""]
merged <- merged[!is.na(assigned_sex) & assigned_sex != ""]
merged[, monk_fingernail := as.factor(trimws(monk_fingernail))]
merged[, assigned_sex := as.factor(trimws(assigned_sex))]

# --- 8. Compute bias (SpO2 − SaO2) ---
merged[, delta_mean := spO2_mean - saO2_mean]

# --- 9. Fit linear model: tone effect adjusted for sex ---
lm_fit <- lm(delta_mean ~ monk_fingernail + assigned_sex, data = merged)
summary(lm_fit)

# --- 10. Extract tidy results with confidence intervals ---
lm_results <- tidy(lm_fit, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write.csv(lm_results, "lm_bias_by_tone_adjusted_for_sex_device2.csv", row.names = FALSE)
print(lm_results)

# --- 11. Optional ANOVA: overall tone effect controlling for sex ---
anova_tone <- anova(lm_fit)
write.csv(anova_tone, "anova_tone_effect_device2.csv")
print(anova_tone)

# --- 12. Visualization of adjusted bias by tone ---
# (Predicted mean bias per tone, adjusted for sex)
pred_df <- merged %>%
  group_by(monk_fingernail, assigned_sex) %>%
  summarise(mean_bias = mean(delta_mean), .groups = "drop")

p_adj <- ggplot(pred_df, aes(x = monk_fingernail, y = mean_bias, fill = assigned_sex)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Adjusted Pulse Oximeter Bias by Monk Fingernail Tone (controlling for sex)",
    x = "Monk Fingernail Category",
    y = "Mean Bias (SpO2 - SaO2, %)",
    fill = "Sex"
  ) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("adjusted_bias_by_tone_controlling_sex_device2.pdf", plot = p_adj, width = 9, height = 6)

# --- 13. Optional diagnostic plots ---
pdf("lm_diagnostics_bias_by_tone_device2.pdf", width = 7, height = 7)
par(mfrow = c(2,2))
plot(lm_fit)
dev.off()

cat("\n✅ Analysis complete. Outputs saved as:\n",
    "- lm_bias_by_tone_adjusted_for_sex_device2.csv (coefficients, CIs, p-values)\n",
    "- anova_tone_effect_device2.csv (overall tone test)\n",
    "- adjusted_bias_by_tone_controlling_sex_device2.pdf (visualization)\n",
    "- lm_diagnostics_bias_by_tone_device2.pdf (residual diagnostics)\n")


# ================================================================
# Pulse oximetry bias-adjusted calculator (Device type = 2)
# ================================================================

# --- If not already loaded ---
library(data.table)
library(broom)

# --- (1) Load the fitted model or re-fit if needed ---
lm_fit <- lm(delta_mean ~ monk_fingernail + assigned_sex, data = merged)

# --- (2) Define the prediction function ---
predict_corrected_SaO2 <- function(tone, sex, SpO2_measured, model = lm_fit) {
  # Ensure tone and sex match model factor levels
  tone <- factor(tone, levels = levels(model$model$monk_fingernail))
  sex  <- factor(sex,  levels = levels(model$model$assigned_sex))
  
  # Create a new data frame for prediction
  newdat <- data.frame(monk_fingernail = tone, assigned_sex = sex)
  
  # Predict bias (SpO2 − SaO2) for this combination
  predicted_bias <- predict(model, newdata = newdat)
  
  # Compute corrected SaO2
  corrected_SaO2 <- SpO2_measured - predicted_bias
  
  # Return both raw bias and corrected value
  return(list(
    predicted_bias = predicted_bias,
    corrected_SaO2 = corrected_SaO2
  ))
}

# --- (3) Example Usage ---
# Suppose a patient has tone = "E", sex = "Female", and pulse oximeter reading = 94%
result <- predict_corrected_SaO2(tone = "E", sex = "Female", SpO2_measured = 94)

cat(sprintf("Predicted bias: %.2f%%\n", result$predicted_bias))
cat(sprintf("Bias-adjusted (corrected) SaO₂: %.2f%%\n", result$corrected_SaO2))

# ================================================================
# Shiny App: Pulse Oximeter Bias Correction (Device Type 2)
# ================================================================

library(shiny)
library(ggplot2)
library(dplyr)

# ---- Load the fitted linear model (or re-fit if needed) ----
# You must have `lm_fit` saved from your previous analysis:
# lm_fit <- lm(delta_mean ~ monk_fingernail + assigned_sex, data = merged)
saveRDS(lm_fit, "lm_fit_device2.rds")

# Load model from file
lm_fit <- readRDS("lm_fit_device2.rds")

# Extract model levels for dropdown menus
tone_levels <- levels(lm_fit$model$monk_fingernail)
sex_levels  <- levels(lm_fit$model$assigned_sex)

# ---- Define UI ----
ui <- fluidPage(
  titlePanel("Pulse Oximeter Bias Correction (Device Type 2)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tone", "Monk Fingernail Tone:",
                  choices = tone_levels, selected = tone_levels[1]),
      selectInput("sex", "Sex:",
                  choices = sex_levels, selected = sex_levels[1]),
      numericInput("spo2", "Pulse Oximeter Reading (SpO₂, %):", value = 95, min = 50, max = 100),
      actionButton("predict", "Calculate Corrected SaO₂"),
      hr(),
      h5("Outputs"),
      verbatimTextOutput("result_text")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Correction Results", plotOutput("bias_plot")),
        tabPanel("Model Info", verbatimTextOutput("model_summary"))
      )
    )
  )
)

# ---- Define Server ----
server <- function(input, output, session) {
  
  # Reactive prediction when button pressed
  pred <- eventReactive(input$predict, {
    newdat <- data.frame(
      monk_fingernail = factor(input$tone, levels = tone_levels),
      assigned_sex = factor(input$sex, levels = sex_levels)
    )
    
    predicted_bias <- predict(lm_fit, newdata = newdat)
    corrected_SaO2 <- input$spo2 - predicted_bias
    
    list(
      tone = input$tone,
      sex = input$sex,
      spo2 = input$spo2,
      bias = predicted_bias,
      corrected = corrected_SaO2
    )
  })
  
  # Display results
  output$result_text <- renderText({
    req(pred())
    sprintf("Input SpO₂: %.1f%%\nPredicted bias: %.2f%%\nCorrected SaO₂: %.2f%%",
            pred()$spo2, pred()$bias, pred()$corrected)
  })
  
  # Bias visualization by tone (using model predictions)
  output$bias_plot <- renderPlot({
    tone_df <- expand.grid(
      monk_fingernail = tone_levels,
      assigned_sex = sex_levels
    )
    tone_df$predicted_bias <- predict(lm_fit, newdata = tone_df)
    
    ggplot(tone_df, aes(x = monk_fingernail, y = predicted_bias, fill = assigned_sex)) +
      geom_col(position = position_dodge(width = 0.8)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title = "Predicted Bias (SpO₂ − SaO₂) by Fingernail Tone and Sex",
        x = "Monk Fingernail Tone",
        y = "Predicted Bias (%)",
        fill = "Sex"
      ) +
      theme_bw(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Optional: Model summary tab
  output$model_summary <- renderPrint({
    summary(lm_fit)
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)


