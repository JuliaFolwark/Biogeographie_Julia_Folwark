### ------------------------------------------------------------- ###
###   SDM – automatische Paketprüfung, -installation und Setup     ###
### ------------------------------------------------------------- ###

# 🧩 1. Grundpakete installieren (sdm muss zuerst da sein)
base_packages <- c("sdm", "readr", "terra", "dplyr",
                   "rasterVis", "latticeExtra", "sf", "sp",
                   "ggplot2", "parallel", "foreach", "doParallel")

installed_packages <- rownames(installed.packages())

for (pkg in base_packages) {
  if (!pkg %in% installed_packages) {
    message(paste("📦 Installiere Basis-Package:", pkg))
    install.packages(pkg, dependencies = TRUE)
  } else {
    message(paste("✅ Bereits installiert:", pkg))
  }
}

# 🧠 2. SDM laden und seine internen Modellabhängigkeiten nachinstallieren
library(sdm)

message("\n🚀 Starte sdm::installAll() – automatische Installation aller Modellabhängigkeiten…")
tryCatch({
  installAll()
}, error = function(e) {
  message("⚠️  sdm::installAll() konnte nicht vollständig ausgeführt werden: ", e$message)
})

# ------------------------------------------------------------- #
# 🧩 3. Zusätzliche Modellierungspakete prüfen (Fallback)
# einige Pakete sind evtl. nicht durch installAll() abgedeckt
required_packages <- c(
  "gbm", "rpart", "dismo", "fda", "mgcv", "glmnet", "earth", 
  "maxlike", "mda", "polspline", "randomForest", "e1071"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    message(paste("📥 Installiere zusätzliches Modell-Package:", pkg))
    install.packages(pkg, dependencies = TRUE)
  } else {
    message(paste("✅ Modell-Package bereits installiert:", pkg))
  }
}

# ------------------------------------------------------------- #
# 🧩 4. Alle Pakete laden (keine Fehlermeldung falls schon aktiv)
all_packages <- unique(c(base_packages, required_packages))
invisible(lapply(all_packages, library, character.only = TRUE))

# ------------------------------------------------------------- #
# 🧩 5. Prüfung auf maxent.jar (Java-Modell)
if ("dismo" %in% all_packages) {
  maxent_path <- system.file("java", package = "dismo")
  jar_file <- file.path(maxent_path, "maxent.jar")
  
  if (!file.exists(jar_file)) {
    message("\n⚠️  'maxent.jar' wurde nicht gefunden.")
    message("   Lade es herunter von:")
    message("   https://biodiversityinformatics.amnh.org/open_source/maxent/")
    message("   und kopiere es nach:", maxent_path)
  } else {
    message("\n✅ 'maxent.jar' gefunden unter:", jar_file)
  }
}

# ------------------------------------------------------------- #
message("\n🎉 SDM-Umgebung vollständig vorbereitet und einsatzbereit!")