# 🩸 Pulse Oximeter Bias Correction Model (Finger Measurement)

### **Project Overview**
This project quantifies and corrects **bias in pulse oximeter readings** across different **skin tone categories** and **sex**.  
Using matched *pulse oximeter (SpO₂)* and *blood gas (SaO₂)* data, we developed a statistical model to estimate and correct systematic over- or underestimation of true oxygen saturation. This regression model is built on data collected from healthy volunteers. Applying this model to patient pulse oximetry data shall consider other confounding factors contributing to altered fingertip perfusion (Reynold's Syndrome, Diabetes Mellitus, other damages) and reduced light permeability (nail painting). 

---

## 🧬 Background
Pulse oximeters estimate arterial oxygen saturation (SpO₂) using light absorption, but studies show they can be less accurate in individuals with darker skin pigmentation.  

To address this, we used the **Monk Fingernail Tone scale (A–H)** as a standardized measure of skin tone and fitted a model that quantifies the average **bias**:
The bias is defined as  

$Bias = SpO_2 - SaO_2$

<p align="center">
  <img src="https://github.com/user-attachments/assets/2320f7a6-3671-44ba-8444-89ac74ec6908" width="648" height="432" />
  <img src="https://github.com/user-attachments/assets/c55742d4-52f4-4690-ba81-fef0ef39901a" width="576" height="432" />
</p>


Figure 1. Monk Skin Tone (MST) Orbs and Swatches 

**Citation**: Monk, Ellis. Monk Skin Tone Scale. 2019, skintone.google.




---

## 🧮 Model Development

### **Data Inputs**
| Dataset | Description |
|----------|--------------|
| `bloodgas.csv` | Reference blood-gas oxygen saturation (SaO₂) per encounter |
| `pulseoximeter.csv` | Device-measured pulse oximeter readings (SpO₂) |
| `encounter.csv` | Encounter-level info including `monk_fingernail` tone |
| `patient.csv` | Patient demographics including `assigned_sex`, `ethnicity`, `race` |
| `devices.csv` | Device metadata — used to restrict analysis to `device_type == 2` |

---

### **Processing Steps**
1. **Filter to Device Type 2**  
   Only samples recorded with device type 2 were analyzed to maintain consistency.
2. **Merge Datasets**  
   Linked encounters across blood-gas, pulse-oximeter, patient, and encounter tables by `encounter_id` and `patient_id`.
3. **Compute Bias**
   $Bias = SpO_2 - SaO_2$
   
5. **Fit Linear Model**
   $Bias = \beta_0 + \beta_1(Monk Fingernail Tone) + \beta_2(Sex) + \varepsilon$
   
   - **β₀** – baseline bias (reference tone & sex)  
   - **β₁, β₂** – effects of tone and sex on bias  
   - **ε (epsilon)** – residual variation (measurement noise, unmodeled effects)
7. **Evaluate Model Fit**  
   - Residual and Q–Q plots confirm linear model assumptions.  
   - Bias increases slightly (~1–2%) for darker tones (E–F).  
   - Sex has minimal additional effect after adjusting for tone.

---

## 🧠 Model Interpretation

### **What ε Represents**
> The ε (epsilon) term captures the random error — the part of bias that the model cannot explain, caused by patient-specific and measurement variability.

When ε values are small and centered around zero, the model fits well.

---

## 🧩 Shiny App — Interactive Bias Correction

An interactive **Shiny app** lets users enter patient characteristics and receive a **bias-corrected oxygen saturation** estimate.

### **Features**
- Input:
  - Monk Fingernail Tone (A–H)
  - Sex (Male/Female)
  - Pulse Oximeter Reading (SpO₂)
- Output:
  - Predicted Bias (SpO₂ − SaO₂)
  - Corrected SaO₂
- Visualization:
  - Predicted bias across tones and sex
  - Model summary and coefficients
  
<p align="center">
  <img src="https://github.com/user-attachments/assets/3e30ad8c-42c0-4210-8d28-7c4c534b4cca" width="500" height="333" />
  <img src="https://github.com/user-attachments/assets/a2baa6e8-deb7-422d-80e9-a7122cb41330" width="450" height="333" />
</p>
---

### **How It Works**
1. The app loads the trained model (`lm_fit_device2.rds`).
2. When a user enters tone, sex, and SpO₂:
   - The model predicts the **expected bias** for that combination.
   - It then subtracts that bias from the input SpO₂:  $$\text{Corrected SaO₂} = \text{Measured SpO₂} - \widehat{\text{Bias}}$$

3. The corrected SaO₂ is displayed along with a bias visualization.

---

### **Example**
| Input | |
|:--|:--|
| Tone | E |
| Sex | Female |
| Measured SpO₂ | 94% |

**Predicted bias:** +1.2%  
**Corrected SaO₂:** 92.8%

---

## ⚙️ Installation & Usage

### **1️⃣ Prerequisites**
- R (≥ 4.2)
- Packages:
  ```r
  install.packages(c("data.table", "ggplot2", "broom", "shiny", "dplyr"))
  ```

### **2️⃣ Fit the model**
Run the analysis script (from the provided R pipeline):
```r
lm_fit <- lm(delta_mean ~ monk_fingernail + assigned_sex, data = merged)
saveRDS(lm_fit, "lm_fit_device2.rds")
```

### **3️⃣ Launch the Shiny app**
```r
shiny::runApp("app.R")
```

Then open the web interface (typically <http://127.0.0.1:xxxx>) to use the correction tool.

---

## 📊 Example Outputs
| File | Description |
|------|--------------|
| `summary_bias_by_monk_fingernail_sex_device2.csv` | Mean bias per tone and sex |
| `bland_altman_<tone>_sex_device2.pdf` | Agreement plots by tone |
| `adjusted_bias_by_tone_controlling_sex_device2.pdf` | Model-adjusted mean bias |
| `lm_bias_by_tone_adjusted_for_sex_device2.csv` | Regression coefficients and confidence intervals |
| `lm_diagnostics_bias_by_tone_device2.pdf` | Model diagnostics |
| `app.R` | Interactive correction app |

---

## 📈 Interpretation Summary
- **Average bias:** ~+0.5% for lighter tones, increasing to ~+1–2% for darker tones  
- **Sex differences:** minimal after controlling for tone  
- **Clinical implication:** Device Type 2 slightly overestimates oxygen saturation at darker tones, but the correction model compensates for this difference

---

## 📚 Citation
If you use this model or code, please cite:

> *“Bias-corrected pulse oximetry model using blood gas validation and skin-tone adjustment.”*  
> UCSF Computational Health Sciences Institute, 2025.

---

## 🧑‍💻 Contact
**Authors:** Reuben Sarwal, Amirtha Maria, Cecilia Wu, Jiajie Gu

**Institution:** UCSF School of Pharmacy, MS AICD3 Program  

**Emails:** reuben.sarwal@ucsf.edu, amirtha.maria@ucsf.edu, Cecilia.Wu@ucsf.edu, Jiajie.Gu@ucsf.edu   
