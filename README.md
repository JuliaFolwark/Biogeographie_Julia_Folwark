# Abschlussbericht: Artenverbreitungsmodellierung

Dieses Repository enthält alle **Daten, Skripte und Ergebnisse** des Abschlussberichts zur **Artenverbreitungsmodellierung (SDM)** für fünf (sieben unvollständig) Gefäßpflanzenarten.
Die Analysen umfassen **Datenbereinigung, Umweltvariablen-Extraktion, Subsampling, Ensemble-Modellierung und Thresholding**.

---

## 📁 **Struktur des Repositories**

### **1. Daten (Data)**
- **GBIF-Daten:**
  - Bereinigte Vorkommensdaten für alle Arten (z. B. `Soldanella_minima_GBIF_WCVP.csv`, `Lepidium_latifolium_GBIF_WCVP.csv`).
  - **Pseudo-Abwesenheitsdaten** (z. B. `Phleum_paniculatum_Absence.csv`).
- **Umweltvariablen (ENVIREM):**
  - Extrahiere Umweltwerte für alle Arten (z. B. `Ruta_graveolens_ENV.csv`).
  - Enthält Variablen wie `annualPET`, `aridityIndexThornthwaite`, `climaticMoistureIndex`.
- **Subsampling-Ergebnisse:**
  - Daten nach PCA-basiertem Subsampling und Convex-Hull-Peeling (z. B. `Soldanella_minima_Subsampled.csv`).

### **2. Abbildungen (Figures)**
- **Arealkarten:**
  - WCVP-Referenzkarten (z. B. `R_Karte_Soldanella_minima.png`).
  - Visualisierungen der Verbreitungsgebiete nach Bereinigung (z. B. `convex_s_minima.png`).
- **SDM-Ergebnisse:**
  - **Kontinuierliche Ensemble-Vorhersagen** (z. B. `Continuous_Ensemble_Soldanella_minima.png`).
  - **Binäre Karten** (z. B. `Binary_Presence_Soldanella_minima.png`).
  - **Geglättete Binärkarten** (z. B. `Smoothed_Binary_Presence_Soldanella_minima.png`).
  - **Vergleiche mit WCVP-Polygonen** (z. B. `Smoothed_Binary_Polygon_Soldanella_minima.png`).

### **3. Skripte (Scripts)**
- **Datenaufbereitung:**
  - `01_Data_Cleaning.R` – Bereinigung der GBIF-Daten mit *CoordinateCleaner*.
  - `02_ENVIREM_Extraction.R` – Extraktion von Umweltvariablen für alle Arten.
- **Modellierung:**
  - `03_Subsampling.R` – PCA-basiertes Subsampling und Generierung von Pseudo-Abwesenheiten.
  - `04_SDM_Modelling.R` – Ensemble-Modellierung und Thresholding.
  - `05_SDM_Evaluation.R` – **Evaluation der Modelle für alle Arten** (AUC, TSS, Kappa, Confusion Matrix).
    - **Hinweis:** Im Bericht wurden nur die Ergebnisse für *Soldanella minima* gezeigt, aber **alle Evaluationsergebnisse für die sieben Arten sind hier verfügbar** (z. B. `SDM_Evaluation_All_Species.csv`).

### **4. Ergebnisse (Results)**
- **Ensemble-Modelle:**
  - Kontinuierliche Vorhersagen (`ENS_CUST_*.tif`).
  - Binäre Karten (`ENS_BIN_*.tif`).
- **Evaluationsmetriken:**
  - **Vollständige Ergebnisse der SDM-Evaluation für alle Arten** (z. B. `Evaluation_Metrics_All_Species.csv`).
  - Enthält Metriken wie **AUC, TSS, Kappa, Sensitivität, Spezifität** für jedes Modell und jede Art.

---

## 🔍 **Wichtige Hinweise**
- **SDM-Evaluation für alle Arten:**
  Im Bericht wurden aus Platzgründen nur die Evaluationsergebnisse für *Soldanella minima* dargestellt. **In diesem Repository finden sich jedoch die vollständigen Evaluationsdaten für fünf Arten** (siehe `Results/SDM_Evaluation_All_Species.csv`).
  - Enthaltene Arten:
    - *Soldanella minima*
    - *Ruta graveolens*
    - *Lepidium graminifolium*
    - *Rumex palustris*
    - *Lepidium latifolium*
  - unvollständig: 
    - *Phleum paniculatum*
    - *Linaria arvensis*

- **Reproduzierbarkeit:**
  Alle Skripte sind so dokumentiert, dass die Analysen **schrittweise nachvollzogen** werden können.
  - **Abhängigkeiten:** Die Skripte erfordern R-Pakete wie `sf`, `terra`, `dplyr`, `gbif.range`, `CoordinateCleaner`, und `sdm`.
  - **Datenquellen:**
    - GBIF-Daten: [https://www.gbif.org](https://www.gbif.org)
    - WCVP/POWO: [https://powo.science.kew.org](https://powo.science.kew.org)
    - ENVIREM: [https://envirem.github.io](https://envirem.github.io)

---
