# =============================================================================
# 01_label_raw_csv.R
# -----------------------------------------------------------------------------
# Turns ONE raw LimeSurvey CSV export into a labelled data.frame.
#
# This is the auto-generated LimeSurvey "R syntax" (variable names + value
# labels), taken verbatim from Test.R. Every survey batch is the SAME survey
# exported the same way (identical 417-column layout), so this labelling is
# reusable for every file - the ONLY thing that changes between batches is the
# input file, which is why the hard-coded file name has been turned into the
# `file_path` argument.
#
# It labels the real question columns (positions 1-219 and the 54 vignette
# attributes at 364-417) and deliberately LEAVES the ~144 timing columns
# (positions 220-363) with their raw names - the cleaning step in
# 02_clean_one_survey.R relies on those raw names to find the timing variables.
#
# Required packages (dplyr/tidyr/stringr) are loaded once in config.R.
#
# Usage:
#   labelled <- label_raw_csv("raw-survey-data/survey_196938_R_data_file.csv")
# =============================================================================

label_raw_csv <- function(file_path) {

  # Read the semicolon-separated LimeSurvey export. All options are kept exactly
  # as in the original LimeSurvey syntax (quoting, NA strings, UTF-8-BOM
  # encoding, and the default check.names = TRUE, which produces the raw timing
  # column names the cleaning step matches on).
data <- read.csv(file_path, quote = "'\"", na.strings=c("", "\"\""),  sep = ";", stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")

# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
attributes(data)$variable.labels[3] <- "lastpage"
names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "startlanguage"
names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "seed"
names(data)[5] <- "seed"
# LimeSurvey Field type: A
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "token"
names(data)[6] <- "token"
# LimeSurvey Field type: DATETIME23.2
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "startdate"
names(data)[7] <- "startdate"
# LimeSurvey Field type: DATETIME23.2
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "datestamp"
names(data)[8] <- "datestamp"
# LimeSurvey Field type: F
data[, 9] <- as.numeric(data[, 9])
attributes(data)$variable.labels[9] <- "Wie lautet die Postleitzahl des Sitzes Ihres Unternehmens?"
names(data)[9] <- "Q0"
# LimeSurvey Field type: F
data[, 10] <- as.numeric(data[, 10])
attributes(data)$variable.labels[10] <- "Sind Sie Eigentümer/Eigentümerin bzw. Geschäftsführer/Geschäftsführerin des Unternehmens?"
data[, 10] <- factor(data[, 10], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[10] <- "Q0a"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "Was ist Ihre Rolle innerhalb des Unternehmens?"
names(data)[11] <- "Q0b"
# LimeSurvey Field type: F
data[, 12] <- as.numeric(data[, 12])
attributes(data)$variable.labels[12] <- "[Architektur- / Bauingenieurbüro / sonstiges Ingenieurbüro] Welcher Kategorie bzw. welchen Kategorien ordnen Sie Ihr Unternehmen zu?"
data[, 12] <- factor(data[, 12], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[12] <- "Q2_SQ001"
# LimeSurvey Field type: F
data[, 13] <- as.numeric(data[, 13])
attributes(data)$variable.labels[13] <- "[Energieberatungsbüro] Welcher Kategorie bzw. welchen Kategorien ordnen Sie Ihr Unternehmen zu?"
data[, 13] <- factor(data[, 13], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[13] <- "Q2_SQ002"
# LimeSurvey Field type: F
data[, 14] <- as.numeric(data[, 14])
attributes(data)$variable.labels[14] <- "[Sanitär-, Heizungs-, Klimatechnik] Welcher Kategorie bzw. welchen Kategorien ordnen Sie Ihr Unternehmen zu?"
data[, 14] <- factor(data[, 14], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[14] <- "Q2_SQ003"
# LimeSurvey Field type: F
data[, 15] <- as.numeric(data[, 15])
attributes(data)$variable.labels[15] <- "[Schornsteinfeger] Welcher Kategorie bzw. welchen Kategorien ordnen Sie Ihr Unternehmen zu?"
data[, 15] <- factor(data[, 15], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[15] <- "Q2_SQ004"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
attributes(data)$variable.labels[16] <- "[Sonstiges] Welcher Kategorie bzw. welchen Kategorien ordnen Sie Ihr Unternehmen zu?"
names(data)[16] <- "Q2_other"
# LimeSurvey Field type: F
data[, 17] <- as.numeric(data[, 17])
attributes(data)$variable.labels[17] <- "Wie viele Personen sind in Ihrem Betrieb/Unternehmen in Deutschland beschäftigt (inkl. Ihnen selbst)?"
names(data)[17] <- "Q3"
# LimeSurvey Field type: A
data[, 18] <- as.character(data[, 18])
attributes(data)$variable.labels[18] <- "Welchen Umsatz hat Ihre Firma im vergangenen Geschäftsjahr erwirtschaftet (insgesamt in allen Geschäftsfeldern)?"
data[, 18] <- factor(data[, 18], levels=c("AO02","AO03","AO04","AO05","AO06","AO07"),labels=c("unter 100.000 €", "100.000 € bis unter 250.000 €", "250.000 € bis unter 500.000 €", "500.000 € bis unter 1 Million €", "1 Million € bis unter 3 Millionen €", "über 3 Millionen €"))
names(data)[18] <- "Q4"
# LimeSurvey Field type: A
data[, 19] <- as.character(data[, 19])
attributes(data)$variable.labels[19] <- "In welchem Umkreis bieten Sie Ihre Energiedienstleistung(en) an?"
data[, 19] <- factor(data[, 19], levels=c("AO01","AO02","AO04","AO05"),labels=c("bis 20 km", "bis 50 km", "bis 100 km", "bundesweit"))
names(data)[19] <- "Q6"
# LimeSurvey Field type: A
data[, 20] <- as.character(data[, 20])
attributes(data)$variable.labels[20] <- "[Sonstiges] In welchem Umkreis bieten Sie Ihre Energiedienstleistung(en) an?"
names(data)[20] <- "Q6_other"
# LimeSurvey Field type: A
data[, 21] <- as.character(data[, 21])
attributes(data)$variable.labels[21] <- "Ist innerhalb Ihres Einzugsgebietes eine Fernwärmeversorgung vorhanden?"
data[, 21] <- factor(data[, 21], levels=c("AO01","AO02","AO03","AO04"),labels=c("Ja, eine Fernwärmeversorgung ist vorhanden.", "Nein, eine Fernwärmeversorgung ist nicht vorhanden. Eine Versorgung ist jedoch in den nächsten Jahren geplant.", "Nein, eine Fernwärmeversorgung ist nicht vorhanden und diese wird in den nächsten Jahren auch nicht geplant.", "Weiß nicht"))
names(data)[21] <- "Q6a"
# LimeSurvey Field type: A
data[, 22] <- as.character(data[, 22])
attributes(data)$variable.labels[22] <- "Im Folgenden werden wir Ihnen nacheinander sechs Szenarien präsentieren. Jedes Szenario beschreibt einen Haushalt mit spezifischen Eigenschaften hinsichtlich der Wohnsituation und des bestehenden Heizsystems.  Bitte lesen Sie jedes Szenario sorgfältig durch und geben Sie anschließend eine Empfehlung ab, welches Heizsystem Sie dem jeweiligen Haushalt empfehlen würden.  Bitte beachten Sie dabei Folgendes:   	Alle für Ihre Empfehlung relevanten Informationen sind im jeweiligen Szenariotext enthalten. 	Die abgebildeten Bilder dienen ausschließlich der Visualisierung und enthalten keine zusätzlichen Informationen, die über den Text hinausgehen. 	Bitte geben Sie Ihre Empfehlung nach Ihrer besten fachlichen Einschätzung ab. Falls Sie keine Einschätzung abgeben können, erläutern Sie bitte kurz die Gründe hierfür. 	Bitte bearbeiten Sie jedes Szenario unabhängig voneinander."
names(data)[22] <- "G02Q120"
# LimeSurvey Field type: A
data[, 23] <- as.character(data[, 23])
attributes(data)$variable.labels[23] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_2}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_3}. Die bestehende {TOKEN:ATTRIBUTE_4} {TOKEN:ATTRIBUTE_5}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_6} gebaut und {TOKEN:ATTRIBUTE_9} Das Gebäude wird über {TOKEN:ATTRIBUTE_8} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 23] <- factor(data[, 23], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung"))
names(data)[23] <- "Vignette1ohneFW"
# LimeSurvey Field type: A
data[, 24] <- as.character(data[, 24])
attributes(data)$variable.labels[24] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_2}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_3}. Die bestehende {TOKEN:ATTRIBUTE_4} {TOKEN:ATTRIBUTE_5}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_6} gebaut und {TOKEN:ATTRIBUTE_9} Das Gebäude wird über {TOKEN:ATTRIBUTE_8} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[24] <- "Vignette1ohneFW_other"
# LimeSurvey Field type: A
data[, 25] <- as.character(data[, 25])
attributes(data)$variable.labels[25] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_11}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_12}. Die bestehende {TOKEN:ATTRIBUTE_13} {TOKEN:ATTRIBUTE_14}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_15} gebaut und {TOKEN:ATTRIBUTE_18} Das Gebäude wird über {TOKEN:ATTRIBUTE_17} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 25] <- factor(data[, 25], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung"))
names(data)[25] <- "Vignette2ohneFW"
# LimeSurvey Field type: A
data[, 26] <- as.character(data[, 26])
attributes(data)$variable.labels[26] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_11}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_12}. Die bestehende {TOKEN:ATTRIBUTE_13} {TOKEN:ATTRIBUTE_14}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_15} gebaut und {TOKEN:ATTRIBUTE_18} Das Gebäude wird über {TOKEN:ATTRIBUTE_17} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[26] <- "Vignette2ohneFW_other"
# LimeSurvey Field type: A
data[, 27] <- as.character(data[, 27])
attributes(data)$variable.labels[27] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_20}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_21}. Die bestehende {TOKEN:ATTRIBUTE_22} {TOKEN:ATTRIBUTE_23}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_24} gebaut und {TOKEN:ATTRIBUTE_27} Das Gebäude wird über {TOKEN:ATTRIBUTE_26} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 27] <- factor(data[, 27], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung"))
names(data)[27] <- "Vignette3ohneFW"
# LimeSurvey Field type: A
data[, 28] <- as.character(data[, 28])
attributes(data)$variable.labels[28] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_20}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_21}. Die bestehende {TOKEN:ATTRIBUTE_22} {TOKEN:ATTRIBUTE_23}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_24} gebaut und {TOKEN:ATTRIBUTE_27} Das Gebäude wird über {TOKEN:ATTRIBUTE_26} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[28] <- "Vignette3ohneFW_other"
# LimeSurvey Field type: A
data[, 29] <- as.character(data[, 29])
attributes(data)$variable.labels[29] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_29}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_30}. Die bestehende {TOKEN:ATTRIBUTE_31} {TOKEN:ATTRIBUTE_32}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_33} gebaut und {TOKEN:ATTRIBUTE_36} Das Gebäude wird über {TOKEN:ATTRIBUTE_35} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 29] <- factor(data[, 29], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung"))
names(data)[29] <- "Vignette4ohneFW"
# LimeSurvey Field type: A
data[, 30] <- as.character(data[, 30])
attributes(data)$variable.labels[30] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_29}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_30}. Die bestehende {TOKEN:ATTRIBUTE_31} {TOKEN:ATTRIBUTE_32}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_33} gebaut und {TOKEN:ATTRIBUTE_36} Das Gebäude wird über {TOKEN:ATTRIBUTE_35} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[30] <- "Vignette4ohneFW_other"
# LimeSurvey Field type: A
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_38}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_39}. Die bestehende {TOKEN:ATTRIBUTE_40} {TOKEN:ATTRIBUTE_41}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_42} gebaut und {TOKEN:ATTRIBUTE_45} Das Gebäude wird über {TOKEN:ATTRIBUTE_44} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 31] <- factor(data[, 31], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung"))
names(data)[31] <- "Vignette5ohneFW"
# LimeSurvey Field type: A
data[, 32] <- as.character(data[, 32])
attributes(data)$variable.labels[32] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_38}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_39}. Die bestehende {TOKEN:ATTRIBUTE_40} {TOKEN:ATTRIBUTE_41}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_42} gebaut und {TOKEN:ATTRIBUTE_45} Das Gebäude wird über {TOKEN:ATTRIBUTE_44} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[32] <- "Vignette5ohneFW_other"
# LimeSurvey Field type: A
data[, 33] <- as.character(data[, 33])
attributes(data)$variable.labels[33] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_47}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_48}. Die bestehende {TOKEN:ATTRIBUTE_49} {TOKEN:ATTRIBUTE_50}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_51} gebaut und {TOKEN:ATTRIBUTE_54} Das Gebäude wird über {TOKEN:ATTRIBUTE_53} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 33] <- factor(data[, 33], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung"))
names(data)[33] <- "Vignette6ohneFW"
# LimeSurvey Field type: A
data[, 34] <- as.character(data[, 34])
attributes(data)$variable.labels[34] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_47}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_48}. Die bestehende {TOKEN:ATTRIBUTE_49} {TOKEN:ATTRIBUTE_50}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_51} gebaut und {TOKEN:ATTRIBUTE_54} Das Gebäude wird über {TOKEN:ATTRIBUTE_53} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[34] <- "Vignette6ohneFW_other"
# LimeSurvey Field type: A
data[, 35] <- as.character(data[, 35])
attributes(data)$variable.labels[35] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_2}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_3}. Die bestehende {TOKEN:ATTRIBUTE_4} {TOKEN:ATTRIBUTE_5}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_6} gebaut und {TOKEN:ATTRIBUTE_9} Das Gebäude wird über {TOKEN:ATTRIBUTE_8} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 35] <- factor(data[, 35], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung", "Fernwärme"))
names(data)[35] <- "Vignette1mitFW"
# LimeSurvey Field type: A
data[, 36] <- as.character(data[, 36])
attributes(data)$variable.labels[36] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_2}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_3}. Die bestehende {TOKEN:ATTRIBUTE_4} {TOKEN:ATTRIBUTE_5}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_6} gebaut und {TOKEN:ATTRIBUTE_9} Das Gebäude wird über {TOKEN:ATTRIBUTE_8} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[36] <- "Vignette1mitFW_other"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
attributes(data)$variable.labels[37] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_11}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_12}. Die bestehende {TOKEN:ATTRIBUTE_13} {TOKEN:ATTRIBUTE_14}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_15} gebaut und {TOKEN:ATTRIBUTE_18} Das Gebäude wird über {TOKEN:ATTRIBUTE_17} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 37] <- factor(data[, 37], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung", "Fernwärme"))
names(data)[37] <- "Vignette2mitFW"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
attributes(data)$variable.labels[38] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_11}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_12}. Die bestehende {TOKEN:ATTRIBUTE_13} {TOKEN:ATTRIBUTE_14}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_15} gebaut und {TOKEN:ATTRIBUTE_18} Das Gebäude wird über {TOKEN:ATTRIBUTE_17} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[38] <- "Vignette2mitFW_other"
# LimeSurvey Field type: A
data[, 39] <- as.character(data[, 39])
attributes(data)$variable.labels[39] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_20}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_21}. Die bestehende {TOKEN:ATTRIBUTE_22} {TOKEN:ATTRIBUTE_23}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_24} gebaut und {TOKEN:ATTRIBUTE_27} Das Gebäude wird über {TOKEN:ATTRIBUTE_26} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 39] <- factor(data[, 39], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung", "Fernwärme"))
names(data)[39] <- "Vignette3mitFW"
# LimeSurvey Field type: A
data[, 40] <- as.character(data[, 40])
attributes(data)$variable.labels[40] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_20}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_21}. Die bestehende {TOKEN:ATTRIBUTE_22} {TOKEN:ATTRIBUTE_23}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_24} gebaut und {TOKEN:ATTRIBUTE_27} Das Gebäude wird über {TOKEN:ATTRIBUTE_26} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[40] <- "Vignette3mitFW_other"
# LimeSurvey Field type: A
data[, 41] <- as.character(data[, 41])
attributes(data)$variable.labels[41] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_29}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_30}. Die bestehende {TOKEN:ATTRIBUTE_31} {TOKEN:ATTRIBUTE_32}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_33} gebaut und {TOKEN:ATTRIBUTE_36} Das Gebäude wird über {TOKEN:ATTRIBUTE_35} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 41] <- factor(data[, 41], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung", "Fernwärme"))
names(data)[41] <- "Vignette4mitFW"
# LimeSurvey Field type: A
data[, 42] <- as.character(data[, 42])
attributes(data)$variable.labels[42] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_29}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_30}. Die bestehende {TOKEN:ATTRIBUTE_31} {TOKEN:ATTRIBUTE_32}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_33} gebaut und {TOKEN:ATTRIBUTE_36} Das Gebäude wird über {TOKEN:ATTRIBUTE_35} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[42] <- "Vignette4mitFW_other"
# LimeSurvey Field type: A
data[, 43] <- as.character(data[, 43])
attributes(data)$variable.labels[43] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_38}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_39}. Die bestehende {TOKEN:ATTRIBUTE_40} {TOKEN:ATTRIBUTE_41}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_42} gebaut und {TOKEN:ATTRIBUTE_45} Das Gebäude wird über {TOKEN:ATTRIBUTE_44} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 43] <- factor(data[, 43], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung", "Fernwärme"))
names(data)[43] <- "Vignette5mitFW"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
attributes(data)$variable.labels[44] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_38}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_39}. Die bestehende {TOKEN:ATTRIBUTE_40} {TOKEN:ATTRIBUTE_41}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_42} gebaut und {TOKEN:ATTRIBUTE_45} Das Gebäude wird über {TOKEN:ATTRIBUTE_44} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[44] <- "Vignette5mitFW_other"
# LimeSurvey Field type: A
data[, 45] <- as.character(data[, 45])
attributes(data)$variable.labels[45] <- "Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_47}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_48}. Die bestehende {TOKEN:ATTRIBUTE_49} {TOKEN:ATTRIBUTE_50}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_51} gebaut und {TOKEN:ATTRIBUTE_54} Das Gebäude wird über {TOKEN:ATTRIBUTE_53} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
data[, 45] <- factor(data[, 45], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("Wärmepumpe", "Pelletheizung", "Hybridheizung", "Gasheizung", "Ölheizung", "Fernwärme"))
names(data)[45] <- "Vignette6mitFW"
# LimeSurvey Field type: A
data[, 46] <- as.character(data[, 46])
attributes(data)$variable.labels[46] <- "[Sonstiges]   Stellen Sie sich vor, Sie haben einen Kundentermin im Hause Schmidt ({TOKEN:ATTRIBUTE_47}) in Ihrem Einzugsgebiet. Gemeinsam hat das Ehepaar Schmidt ein Einkommen von {TOKEN:ATTRIBUTE_48}. Die bestehende {TOKEN:ATTRIBUTE_49} {TOKEN:ATTRIBUTE_50}. Das abgebildete Haus wurde im Jahr {TOKEN:ATTRIBUTE_51} gebaut und {TOKEN:ATTRIBUTE_54} Das Gebäude wird über {TOKEN:ATTRIBUTE_53} beheizt.  Welche Heiztechnologie würden Sie dem Ehepaar Schmidt empfehlen?"
names(data)[46] <- "Vignette6mitFW_other"
# LimeSurvey Field type: A
data[, 47] <- as.character(data[, 47])
attributes(data)$variable.labels[47] <- "Im letzten Szenario haben Sie als Heiztechnologie eine {Vignette6ohneFW.shown} empfohlen. Welche Investitionskosten erwarten Sie für Anschaffung und Einbau der Anlage ohne mögliche Förderung zu berücksichtigen?"
data[, 47] <- factor(data[, 47], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("unter 10.000€", "10.000€ - 19.999€", "20.000€ - 29.999€", "30.000€ - 39.999€", "40.000€ - 49.999€", "über 50.000€"))
names(data)[47] <- "KostenVignetteohneFW"
# LimeSurvey Field type: A
data[, 48] <- as.character(data[, 48])
attributes(data)$variable.labels[48] <- "Im letzten Szenario haben Sie als Heiztechnologie eine {Vignette6mitFW.shown} empfohlen. Welche Investitionskosten erwarten Sie für Anschaffung und Einbau der Anlage ohne mögliche Förderung zu berücksichtigen?"
data[, 48] <- factor(data[, 48], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("unter 10.000€", "10.000€ - 19.999€", "20.000€ - 29.999€", "30.000€ - 39.999€", "40.000€ - 49.999€", "über 50.000€"))
names(data)[48] <- "KostenVignettemitFW"
# LimeSurvey Field type: A
data[, 49] <- as.character(data[, 49])
attributes(data)$variable.labels[49] <- "Im letzten Szenario haben Sie als Heiztechnologie {Vignette6mitFW.shown} empfohlen. Welche Investitionskosten erwarten Sie für Anschaffung und Einbau der Anlage ohne mögliche Förderung zu berücksichtigen?"
data[, 49] <- factor(data[, 49], levels=c("AO01","AO02","AO03","AO04","AO05","AO06"),labels=c("unter 10.000€", "10.000€ - 19.999€", "20.000€ - 29.999€", "30.000€ - 39.999€", "40.000€ - 49.999€", "über 50.000€"))
names(data)[49] <- "KostenVignetteFW"
# LimeSurvey Field type: F
data[, 50] <- as.numeric(data[, 50])
attributes(data)$variable.labels[50] <- "[Gas] Wenn Sie schätzen müssten, was bezahlten Haushalte unter Ihren Kunden im Schnitt für eine kWh des jeweiligen Energieträgers in den ersten zwei Monaten des Jahres 2026?   Falls es keine Haushalte unter Ihren Kunden gibt, die den jeweiligen Energieträger nutzen, lassen Sie das entsprechende Feld bitte einfach frei. "
names(data)[50] <- "Q15_SQ001"
# LimeSurvey Field type: F
data[, 51] <- as.numeric(data[, 51])
attributes(data)$variable.labels[51] <- "[Wärmepumpenstrom] Wenn Sie schätzen müssten, was bezahlten Haushalte unter Ihren Kunden im Schnitt für eine kWh des jeweiligen Energieträgers in den ersten zwei Monaten des Jahres 2026?   Falls es keine Haushalte unter Ihren Kunden gibt, die den jeweiligen Energieträger nutzen, lassen Sie das entsprechende Feld bitte einfach frei. "
names(data)[51] <- "Q15_SQ002"
# LimeSurvey Field type: F
data[, 52] <- as.numeric(data[, 52])
attributes(data)$variable.labels[52] <- "[Fernwärme] Wenn Sie schätzen müssten, was bezahlten Haushalte unter Ihren Kunden im Schnitt für eine kWh des jeweiligen Energieträgers in den ersten zwei Monaten des Jahres 2026?   Falls es keine Haushalte unter Ihren Kunden gibt, die den jeweiligen Energieträger nutzen, lassen Sie das entsprechende Feld bitte einfach frei. "
names(data)[52] <- "Q15_SQ003"
# LimeSurvey Field type: F
data[, 53] <- as.numeric(data[, 53])
attributes(data)$variable.labels[53] <- "[Öl          ⓘ            Sie können von folgender Grundregel ausgehen:     1l Heizöl entspricht etwa 10kWh.    ] Wenn Sie schätzen müssten, was bezahlten Haushalte unter Ihren Kunden im Schnitt für eine kWh des jeweiligen Energieträgers in den ersten zwei Monaten des Jahres 2026?   Falls es keine Haushalte unter Ihren Kunden gibt, die den jeweiligen Energieträger nutzen, lassen Sie das entsprechende Feld bitte einfach frei. "
names(data)[53] <- "Q15_SQ004"
# LimeSurvey Field type: F
data[, 54] <- as.numeric(data[, 54])
attributes(data)$variable.labels[54] <- "[Pellets          ⓘ            Sie können von folgender Grundregel ausgehen:     1kg Pellets entspricht etwa 5kWh.    ] Wenn Sie schätzen müssten, was bezahlten Haushalte unter Ihren Kunden im Schnitt für eine kWh des jeweiligen Energieträgers in den ersten zwei Monaten des Jahres 2026?   Falls es keine Haushalte unter Ihren Kunden gibt, die den jeweiligen Energieträger nutzen, lassen Sie das entsprechende Feld bitte einfach frei. "
names(data)[54] <- "Q15_SQ005"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "[Gas] Stellen Sie sich vor, Sie würden Kunden eine neue Heizung empfehlen und müssten unterschiedliche Technologien vergleichen. Wie werden sich die Energiepreise in den nächsten fünf Jahren Ihrer Meinung nach entwickeln im Vergleich zu Anfang des Jahres 2026? Falls der jeweilige Energieträger in Ihrem Einzugsgebiet nicht verfügbar sein sollte, lassen Sie die jeweilige Zeile bitte einfach aus. Der Preis für die folgenden Energieträger in fünf Jahren ist..."
data[, 55] <- factor(data[, 55], levels=c("AO07","AO01","AO03","AO05","AO08"),labels=c("mehr als 10% niedriger", "bis zu 10 % niedriger", "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher"))
names(data)[55] <- "Q17_SQ001"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
attributes(data)$variable.labels[56] <- "[Wärmepumpenstrom] Stellen Sie sich vor, Sie würden Kunden eine neue Heizung empfehlen und müssten unterschiedliche Technologien vergleichen. Wie werden sich die Energiepreise in den nächsten fünf Jahren Ihrer Meinung nach entwickeln im Vergleich zu Anfang des Jahres 2026? Falls der jeweilige Energieträger in Ihrem Einzugsgebiet nicht verfügbar sein sollte, lassen Sie die jeweilige Zeile bitte einfach aus. Der Preis für die folgenden Energieträger in fünf Jahren ist..."
data[, 56] <- factor(data[, 56], levels=c("AO07","AO01","AO03","AO05","AO08"),labels=c("mehr als 10% niedriger", "bis zu 10 % niedriger", "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher"))
names(data)[56] <- "Q17_SQ002"
# LimeSurvey Field type: A
data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "[Fernwärme] Stellen Sie sich vor, Sie würden Kunden eine neue Heizung empfehlen und müssten unterschiedliche Technologien vergleichen. Wie werden sich die Energiepreise in den nächsten fünf Jahren Ihrer Meinung nach entwickeln im Vergleich zu Anfang des Jahres 2026? Falls der jeweilige Energieträger in Ihrem Einzugsgebiet nicht verfügbar sein sollte, lassen Sie die jeweilige Zeile bitte einfach aus. Der Preis für die folgenden Energieträger in fünf Jahren ist..."
data[, 57] <- factor(data[, 57], levels=c("AO07","AO01","AO03","AO05","AO08"),labels=c("mehr als 10% niedriger", "bis zu 10 % niedriger", "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher"))
names(data)[57] <- "Q17_SQ003"
# LimeSurvey Field type: A
data[, 58] <- as.character(data[, 58])
attributes(data)$variable.labels[58] <- "[Öl] Stellen Sie sich vor, Sie würden Kunden eine neue Heizung empfehlen und müssten unterschiedliche Technologien vergleichen. Wie werden sich die Energiepreise in den nächsten fünf Jahren Ihrer Meinung nach entwickeln im Vergleich zu Anfang des Jahres 2026? Falls der jeweilige Energieträger in Ihrem Einzugsgebiet nicht verfügbar sein sollte, lassen Sie die jeweilige Zeile bitte einfach aus. Der Preis für die folgenden Energieträger in fünf Jahren ist..."
data[, 58] <- factor(data[, 58], levels=c("AO07","AO01","AO03","AO05","AO08"),labels=c("mehr als 10% niedriger", "bis zu 10 % niedriger", "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher"))
names(data)[58] <- "Q17_SQ004"
# LimeSurvey Field type: A
data[, 59] <- as.character(data[, 59])
attributes(data)$variable.labels[59] <- "[Pellets] Stellen Sie sich vor, Sie würden Kunden eine neue Heizung empfehlen und müssten unterschiedliche Technologien vergleichen. Wie werden sich die Energiepreise in den nächsten fünf Jahren Ihrer Meinung nach entwickeln im Vergleich zu Anfang des Jahres 2026? Falls der jeweilige Energieträger in Ihrem Einzugsgebiet nicht verfügbar sein sollte, lassen Sie die jeweilige Zeile bitte einfach aus. Der Preis für die folgenden Energieträger in fünf Jahren ist..."
data[, 59] <- factor(data[, 59], levels=c("AO07","AO01","AO03","AO05","AO08"),labels=c("mehr als 10% niedriger", "bis zu 10 % niedriger", "ungefähr gleich", "bis zu 10 % höher", "mehr als 10% höher"))
names(data)[59] <- "Q17_SQ005"
# LimeSurvey Field type: F
data[, 60] <- as.numeric(data[, 60])
attributes(data)$variable.labels[60] <- "Haben Sie schon von der CO₂-Bepreisung für Heizbrennstoffe (z. B. Gas, Heizöl) in Deutschland gehört?"
data[, 60] <- factor(data[, 60], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[60] <- "Q18"
# LimeSurvey Field type: F
data[, 61] <- as.numeric(data[, 61])
attributes(data)$variable.labels[61] <- "[Der Preis liegt aktuell zwischen ca.] Wie hoch ist der CO₂-Preis für Heizbrennstoffe (z. B. Gas, Heizöl) in Deutschland aktuell? Bitte geben Sie eine geschätzte Spanne für den aktuellen Preis an. Eine grobe Schätzung ist ausreichend."
names(data)[61] <- "Q18b_SQ001"
# LimeSurvey Field type: F
data[, 62] <- as.numeric(data[, 62])
attributes(data)$variable.labels[62] <- "[und ca.] Wie hoch ist der CO₂-Preis für Heizbrennstoffe (z. B. Gas, Heizöl) in Deutschland aktuell? Bitte geben Sie eine geschätzte Spanne für den aktuellen Preis an. Eine grobe Schätzung ist ausreichend."
names(data)[62] <- "Q18b_SQ002"
# LimeSurvey Field type: F
data[, 63] <- as.numeric(data[, 63])
attributes(data)$variable.labels[63] <- "[Wärmepumpe] Denken Sie an Ihre Aufträge im Jahr 2025: Bitte schätzen Sie grob den Anteil an tatsächlich installierten Heizsystemen für Raumwärme, der auf die folgenden Technologien entfiel. Eine grobe Schätzung ist ausreichend."
names(data)[63] <- "Q11b_WP"
# LimeSurvey Field type: F
data[, 64] <- as.numeric(data[, 64])
attributes(data)$variable.labels[64] <- "[Pelletheizung] Denken Sie an Ihre Aufträge im Jahr 2025: Bitte schätzen Sie grob den Anteil an tatsächlich installierten Heizsystemen für Raumwärme, der auf die folgenden Technologien entfiel. Eine grobe Schätzung ist ausreichend."
names(data)[64] <- "Q11b_PEL"
# LimeSurvey Field type: F
data[, 65] <- as.numeric(data[, 65])
attributes(data)$variable.labels[65] <- "[Fernwärme] Denken Sie an Ihre Aufträge im Jahr 2025: Bitte schätzen Sie grob den Anteil an tatsächlich installierten Heizsystemen für Raumwärme, der auf die folgenden Technologien entfiel. Eine grobe Schätzung ist ausreichend."
names(data)[65] <- "Q11b_FW"
# LimeSurvey Field type: F
data[, 66] <- as.numeric(data[, 66])
attributes(data)$variable.labels[66] <- "[Gasheizung] Denken Sie an Ihre Aufträge im Jahr 2025: Bitte schätzen Sie grob den Anteil an tatsächlich installierten Heizsystemen für Raumwärme, der auf die folgenden Technologien entfiel. Eine grobe Schätzung ist ausreichend."
names(data)[66] <- "Q11b_GAS"
# LimeSurvey Field type: F
data[, 67] <- as.numeric(data[, 67])
attributes(data)$variable.labels[67] <- "[Hybridheizung         ⓘ        Eine Hybridheizung verbindet ein umweltfreundliches, erneuerbares System (wie eine Wärmepumpe) mit einem konventionellen Wärmeerzeuger (wie einer Gas- oder Ölheizung).   ] Denken Sie an Ihre Aufträge im Jahr 2025: Bitte schätzen Sie grob den Anteil an tatsächlich installierten Heizsystemen für Raumwärme, der auf die folgenden Technologien entfiel. Eine grobe Schätzung ist ausreichend."
names(data)[67] <- "Q11b_HYB"
# LimeSurvey Field type: F
data[, 68] <- as.numeric(data[, 68])
attributes(data)$variable.labels[68] <- "[Ölheizung] Denken Sie an Ihre Aufträge im Jahr 2025: Bitte schätzen Sie grob den Anteil an tatsächlich installierten Heizsystemen für Raumwärme, der auf die folgenden Technologien entfiel. Eine grobe Schätzung ist ausreichend."
names(data)[68] <- "Q11b_OEL"
# LimeSurvey Field type: F
data[, 69] <- as.numeric(data[, 69])
attributes(data)$variable.labels[69] <- "[Sonstiges] Denken Sie an Ihre Aufträge im Jahr 2025: Bitte schätzen Sie grob den Anteil an tatsächlich installierten Heizsystemen für Raumwärme, der auf die folgenden Technologien entfiel. Eine grobe Schätzung ist ausreichend."
names(data)[69] <- "Q11b_SON"
# LimeSurvey Field type: A
data[, 70] <- as.character(data[, 70])
attributes(data)$variable.labels[70] <- ""
names(data)[70] <- "Q11bRandWP"
# LimeSurvey Field type: A
data[, 71] <- as.character(data[, 71])
attributes(data)$variable.labels[71] <- ""
names(data)[71] <- "Q11bRandPEL"
# LimeSurvey Field type: A
data[, 72] <- as.character(data[, 72])
attributes(data)$variable.labels[72] <- ""
names(data)[72] <- "Q11bRandFW"
# LimeSurvey Field type: A
data[, 73] <- as.character(data[, 73])
attributes(data)$variable.labels[73] <- ""
names(data)[73] <- "Q11bRandGAS"
# LimeSurvey Field type: A
data[, 74] <- as.character(data[, 74])
attributes(data)$variable.labels[74] <- ""
names(data)[74] <- "Q11bRandOEL"
# LimeSurvey Field type: A
data[, 75] <- as.character(data[, 75])
attributes(data)$variable.labels[75] <- ""
names(data)[75] <- "Q11bRandHYB"
# LimeSurvey Field type: A
data[, 76] <- as.character(data[, 76])
attributes(data)$variable.labels[76] <- ""
names(data)[76] <- "Q11bRandSON"
# LimeSurvey Field type: A
data[, 77] <- as.character(data[, 77])
attributes(data)$variable.labels[77] <- ""
names(data)[77] <- "Q11bScoreWP"
# LimeSurvey Field type: A
data[, 78] <- as.character(data[, 78])
attributes(data)$variable.labels[78] <- ""
names(data)[78] <- "Q11bScorePEL"
# LimeSurvey Field type: A
data[, 79] <- as.character(data[, 79])
attributes(data)$variable.labels[79] <- ""
names(data)[79] <- "Q11bScoreFW"
# LimeSurvey Field type: A
data[, 80] <- as.character(data[, 80])
attributes(data)$variable.labels[80] <- ""
names(data)[80] <- "Q11bScoreGAS"
# LimeSurvey Field type: A
data[, 81] <- as.character(data[, 81])
attributes(data)$variable.labels[81] <- ""
names(data)[81] <- "Q11bScoreOEL"
# LimeSurvey Field type: A
data[, 82] <- as.character(data[, 82])
attributes(data)$variable.labels[82] <- ""
names(data)[82] <- "Q11bScoreHYB"
# LimeSurvey Field type: A
data[, 83] <- as.character(data[, 83])
attributes(data)$variable.labels[83] <- ""
names(data)[83] <- "Q11bScoreSON"
# LimeSurvey Field type: A
data[, 84] <- as.character(data[, 84])
attributes(data)$variable.labels[84] <- ""
names(data)[84] <- "Q11bTop1"
# LimeSurvey Field type: A
data[, 85] <- as.character(data[, 85])
attributes(data)$variable.labels[85] <- ""
names(data)[85] <- "Q11bTop2"
# LimeSurvey Field type: A
data[, 86] <- as.character(data[, 86])
attributes(data)$variable.labels[86] <- "Von welchem Hersteller verkaufen Sie am meisten {if(Q11bTop1==\'WP\',\'Wärmepumpen\',if(Q11bTop1==\'PEL\',\'Pelletheizungen\',if(Q11bTop1==\'FW\',\'Fernwärme-Hausstationen\',if(Q11bTop1==\'GAS\',\'Gasheizungen\',if(Q11bTop1==\'HYB\',\'Hybridheizungen\',if(Q11bTop1==\'OEL\',\'Ölheizungen\',if(Q11bTop1==\'SON\',\'sonstige Heizungen\',\'\')))))))}?"
names(data)[86] <- "Q11c1b"
# LimeSurvey Field type: A
data[, 87] <- as.character(data[, 87])
attributes(data)$variable.labels[87] <- "Angenommen, Ihnen würde die Wahl zwischen dem Erhalt einer Zahlung heute oder einer Zahlung in 12 Monaten gegeben. Wir werden Ihnen nun fünf Situationen vorstellen. Die Zahlung heute ist in jeder dieser Situationen gleich. Die Zahlung in 12 Monaten ist in jeder Situation unterschiedlich. Für jede dieser Situationen möchten wir wissen, welche Sie wählen würden. Bitte nehmen Sie an, dass es keine Inflation gibt, d. h. zukünftige Preise sind die gleichen wie die heutigen Preise.   Bitte berücksichtigen Sie Folgendes: Würden Sie lieber 100 Euro heute oder 154 Euro in 12 Monaten erhalten?"
data[, 87] <- factor(data[, 87], levels=c("A","B"),labels=c("100€ heute", "154€ in 12 Monaten"))
names(data)[87] <- "Time1"
# LimeSurvey Field type: A
data[, 88] <- as.character(data[, 88])
attributes(data)$variable.labels[88] <- "Würden Sie lieber 100 Euro heute oder 185 Euro in 12 Monaten erhalten?"
data[, 88] <- factor(data[, 88], levels=c("A","B"),labels=c("100 Euro heute", "185 Euro in 12 Monaten"))
names(data)[88] <- "Time2A"
# LimeSurvey Field type: A
data[, 89] <- as.character(data[, 89])
attributes(data)$variable.labels[89] <- "Würden Sie lieber 100 Euro heute oder 125 Euro in 12 Monaten erhalten?"
data[, 89] <- factor(data[, 89], levels=c("A","B"),labels=c("100 Euro heute", "125 Euro in 12 Monaten"))
names(data)[89] <- "Time2B"
# LimeSurvey Field type: A
data[, 90] <- as.character(data[, 90])
attributes(data)$variable.labels[90] <- "Würden Sie lieber 100 Euro heute oder 202 Euro in 12 Monaten erhalten?"
data[, 90] <- factor(data[, 90], levels=c("A","B"),labels=c("100 Euro heute", "202 Euro in 12 Monaten"))
names(data)[90] <- "Time3AA"
# LimeSurvey Field type: A
data[, 91] <- as.character(data[, 91])
attributes(data)$variable.labels[91] <- "Würden Sie lieber 100 Euro heute oder 169 Euro in 12 Monaten erhalten?"
data[, 91] <- factor(data[, 91], levels=c("A","B"),labels=c("100 Euro heute", "169 Euro in 12 Monaten"))
names(data)[91] <- "Time3AB"
# LimeSurvey Field type: A
data[, 92] <- as.character(data[, 92])
attributes(data)$variable.labels[92] <- "Würden Sie lieber 100 Euro heute oder 139 Euro in 12 Monaten erhalten?"
data[, 92] <- factor(data[, 92], levels=c("A","B"),labels=c("100 Euro heute", "139 Euro in 12 Monaten"))
names(data)[92] <- "Time3BA"
# LimeSurvey Field type: A
data[, 93] <- as.character(data[, 93])
attributes(data)$variable.labels[93] <- "Würden Sie lieber 100 Euro heute oder 112 Euro in 12 Monaten erhalten?"
data[, 93] <- factor(data[, 93], levels=c("A","B"),labels=c("100 Euro heute", "112 Euro in 12 Monaten"))
names(data)[93] <- "Time3BB"
# LimeSurvey Field type: A
data[, 94] <- as.character(data[, 94])
attributes(data)$variable.labels[94] <- "Würden Sie lieber 100 Euro heute oder 210 Euro in 12 Monaten erhalten?"
data[, 94] <- factor(data[, 94], levels=c("A","B"),labels=c("100 Euro heute", "210 Euro in 12 Monaten"))
names(data)[94] <- "Time4AAA"
# LimeSurvey Field type: A
data[, 95] <- as.character(data[, 95])
attributes(data)$variable.labels[95] <- "Würden Sie lieber 100 Euro heute oder 193 Euro in 12 Monaten erhalten?"
data[, 95] <- factor(data[, 95], levels=c("A","B"),labels=c("100 Euro heute", "193 Euro in 12 Monaten"))
names(data)[95] <- "Time4AAB"
# LimeSurvey Field type: A
data[, 96] <- as.character(data[, 96])
attributes(data)$variable.labels[96] <- "Würden Sie lieber 100 Euro heute oder 177 Euro in 12 Monaten erhalten?"
data[, 96] <- factor(data[, 96], levels=c("A","B"),labels=c("100 Euro heute", "177 Euro in 12 Monaten"))
names(data)[96] <- "Time4ABA"
# LimeSurvey Field type: A
data[, 97] <- as.character(data[, 97])
attributes(data)$variable.labels[97] <- "Würden Sie lieber 100 Euro heute oder 161 Euro in 12 Monaten erhalten?"
data[, 97] <- factor(data[, 97], levels=c("A","B"),labels=c("100 Euro heute", "161 Euro in 12 Monaten"))
names(data)[97] <- "Time4ABB"
# LimeSurvey Field type: A
data[, 98] <- as.character(data[, 98])
attributes(data)$variable.labels[98] <- "Würden Sie lieber 100 Euro heute oder 146 Euro in 12 Monaten erhalten?"
data[, 98] <- factor(data[, 98], levels=c("A","B"),labels=c("100 Euro heute", "146 Euro in 12 Monaten"))
names(data)[98] <- "Time4BAA"
# LimeSurvey Field type: A
data[, 99] <- as.character(data[, 99])
attributes(data)$variable.labels[99] <- "Würden Sie lieber 100 Euro heute oder 132 Euro in 12 Monaten erhalten?"
data[, 99] <- factor(data[, 99], levels=c("A","B"),labels=c("100 Euro heute", "132 Euro in 12 Monaten"))
names(data)[99] <- "Time4BAB"
# LimeSurvey Field type: A
data[, 100] <- as.character(data[, 100])
attributes(data)$variable.labels[100] <- "Würden Sie lieber 100 Euro heute oder 119 Euro in 12 Monaten erhalten?"
data[, 100] <- factor(data[, 100], levels=c("A","B"),labels=c("100 Euro heute", "119 Euro in 12 Monaten"))
names(data)[100] <- "Time4BBA"
# LimeSurvey Field type: A
data[, 101] <- as.character(data[, 101])
attributes(data)$variable.labels[101] <- "Würden Sie lieber 100 Euro heute oder 106 Euro in 12 Monaten erhalten?"
data[, 101] <- factor(data[, 101], levels=c("A","B"),labels=c("100 Euro heute", "106 Euro in 12 Monaten"))
names(data)[101] <- "Time4BBB"
# LimeSurvey Field type: A
data[, 102] <- as.character(data[, 102])
attributes(data)$variable.labels[102] <- "Würden Sie lieber 100 Euro heute oder 215 Euro in 12 Monaten erhalten?"
data[, 102] <- factor(data[, 102], levels=c("A","B"),labels=c("100 Euro heute", "215 Euro in 12 Monaten"))
names(data)[102] <- "Time5AAAA"
# LimeSurvey Field type: A
data[, 103] <- as.character(data[, 103])
attributes(data)$variable.labels[103] <- "Würden Sie lieber 100 Euro heute oder 206 Euro in 12 Monaten erhalten?"
data[, 103] <- factor(data[, 103], levels=c("A","B"),labels=c("100 Euro heute", "206 Euro in 12 Monaten"))
names(data)[103] <- "Time5AAAB"
# LimeSurvey Field type: A
data[, 104] <- as.character(data[, 104])
attributes(data)$variable.labels[104] <- "Würden Sie lieber 100 Euro heute oder 197 Euro in 12 Monaten erhalten?"
data[, 104] <- factor(data[, 104], levels=c("A","B"),labels=c("100 Euro heute", "197 Euro in 12 Monaten"))
names(data)[104] <- "Time5AABA"
# LimeSurvey Field type: A
data[, 105] <- as.character(data[, 105])
attributes(data)$variable.labels[105] <- "Würden Sie lieber 100 Euro heute oder 189 Euro in 12 Monaten erhalten?"
data[, 105] <- factor(data[, 105], levels=c("A","B"),labels=c("100 Euro heute", "189 Euro in 12 Monaten"))
names(data)[105] <- "Time5AABB"
# LimeSurvey Field type: A
data[, 106] <- as.character(data[, 106])
attributes(data)$variable.labels[106] <- "Würden Sie lieber 100 Euro heute oder 181 Euro in 12 Monaten erhalten?"
data[, 106] <- factor(data[, 106], levels=c("A","B"),labels=c("100 Euro heute", "181 Euro in 12 Monaten"))
names(data)[106] <- "Time5ABAA"
# LimeSurvey Field type: A
data[, 107] <- as.character(data[, 107])
attributes(data)$variable.labels[107] <- "Würden Sie lieber 100 Euro heute oder 173 Euro in 12 Monaten erhalten?"
data[, 107] <- factor(data[, 107], levels=c("A","B"),labels=c("100 Euro heute", "173 Euro in 12 Monaten"))
names(data)[107] <- "Time5ABAB"
# LimeSurvey Field type: A
data[, 108] <- as.character(data[, 108])
attributes(data)$variable.labels[108] <- "Würden Sie lieber 100 Euro heute oder 150 Euro in 12 Monaten erhalten?"
data[, 108] <- factor(data[, 108], levels=c("A","B"),labels=c("100 Euro heute", "150 Euro in 12 Monaten"))
names(data)[108] <- "Time5BAAA"
# LimeSurvey Field type: A
data[, 109] <- as.character(data[, 109])
attributes(data)$variable.labels[109] <- "Würden Sie lieber 100 Euro heute oder 143 Euro in 12 Monaten erhalten?"
data[, 109] <- factor(data[, 109], levels=c("A","B"),labels=c("100 Euro heute", "143 Euro in 12 Monaten"))
names(data)[109] <- "Time5BAAB"
# LimeSurvey Field type: A
data[, 110] <- as.character(data[, 110])
attributes(data)$variable.labels[110] <- "Würden Sie lieber 100 Euro heute oder 136 Euro in 12 Monaten erhalten?"
data[, 110] <- factor(data[, 110], levels=c("A","B"),labels=c("100 Euro heute", "136 Euro in 12 Monaten"))
names(data)[110] <- "Time5BABA"
# LimeSurvey Field type: A
data[, 111] <- as.character(data[, 111])
attributes(data)$variable.labels[111] <- "Würden Sie lieber 100 Euro heute oder 129 Euro in 12 Monaten erhalten?"
data[, 111] <- factor(data[, 111], levels=c("A","B"),labels=c("100 Euro heute", "129 Euro in 12 Monaten"))
names(data)[111] <- "Time5BABB"
# LimeSurvey Field type: A
data[, 112] <- as.character(data[, 112])
attributes(data)$variable.labels[112] <- "Würden Sie lieber 100 Euro heute oder 122 Euro in 12 Monaten erhalten?"
data[, 112] <- factor(data[, 112], levels=c("A","B"),labels=c("100 Euro heute", "122 Euro in 12 Monaten"))
names(data)[112] <- "Time5BBAA"
# LimeSurvey Field type: A
data[, 113] <- as.character(data[, 113])
attributes(data)$variable.labels[113] <- "Würden Sie lieber 100 Euro heute oder 116 Euro in 12 Monaten erhalten?"
data[, 113] <- factor(data[, 113], levels=c("A","B"),labels=c("100 Euro heute", "116 Euro in 12 Monaten"))
names(data)[113] <- "Time5BBAB"
# LimeSurvey Field type: A
data[, 114] <- as.character(data[, 114])
attributes(data)$variable.labels[114] <- "Würden Sie lieber 100 Euro heute oder 109 Euro in 12 Monaten erhalten?"
data[, 114] <- factor(data[, 114], levels=c("A","B"),labels=c("100 Euro heute", "109 Euro in 12 Monaten"))
names(data)[114] <- "Time5BBBA"
# LimeSurvey Field type: A
data[, 115] <- as.character(data[, 115])
attributes(data)$variable.labels[115] <- "Würden Sie lieber 100 Euro heute oder 103 Euro in 12 Monaten erhalten?"
data[, 115] <- factor(data[, 115], levels=c("A","B"),labels=c("100 Euro heute", "103 Euro in 12 Monaten"))
names(data)[115] <- "Time5BBBB"
# LimeSurvey Field type: A
data[, 116] <- as.character(data[, 116])
attributes(data)$variable.labels[116] <- "Angenommen, Sie hätten die Wahl zwischen einer sicheren Zahlung und einer Verlosung. Wir werden Ihnen nun fünf Situationen vorstellen. Die Verlosung ist in jeder dieser Situationen gleich aufgebaut: Sie haben eine 50-prozentige Chance, 300 Euro zu erhalten, und eine 50-prozentige Chance, nichts zu erhalten. Die sichere Zahlung variiert zwischen den Situationen. Für jede dieser Situationen möchten wir wissen, welche Option Sie bevorzugen. Bitte beachten Sie, dass die Wahrscheinlichkeiten in allen Situationen identisch sind.  Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 160 Euro?"
data[, 116] <- factor(data[, 116], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 160 Euro"))
names(data)[116] <- "Risk1"
# LimeSurvey Field type: A
data[, 117] <- as.character(data[, 117])
attributes(data)$variable.labels[117] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 240 Euro?"
data[, 117] <- factor(data[, 117], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 240 Euro"))
names(data)[117] <- "Risk2A"
# LimeSurvey Field type: A
data[, 118] <- as.character(data[, 118])
attributes(data)$variable.labels[118] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 80 Euro?"
data[, 118] <- factor(data[, 118], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 80 Euro"))
names(data)[118] <- "Risk2B"
# LimeSurvey Field type: A
data[, 119] <- as.character(data[, 119])
attributes(data)$variable.labels[119] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 280 Euro?"
data[, 119] <- factor(data[, 119], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 280 Euro"))
names(data)[119] <- "Risk3AA"
# LimeSurvey Field type: A
data[, 120] <- as.character(data[, 120])
attributes(data)$variable.labels[120] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 200 Euro?"
data[, 120] <- factor(data[, 120], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 200 Euro"))
names(data)[120] <- "Risk3AB"
# LimeSurvey Field type: A
data[, 121] <- as.character(data[, 121])
attributes(data)$variable.labels[121] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 120 Euro?"
data[, 121] <- factor(data[, 121], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 120 Euro"))
names(data)[121] <- "Risk3BA"
# LimeSurvey Field type: A
data[, 122] <- as.character(data[, 122])
attributes(data)$variable.labels[122] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 40 Euro?"
data[, 122] <- factor(data[, 122], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "Sichere Zahlung von 40 Euro"))
names(data)[122] <- "Risk3BB"
# LimeSurvey Field type: A
data[, 123] <- as.character(data[, 123])
attributes(data)$variable.labels[123] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 300 Euro?"
data[, 123] <- factor(data[, 123], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 300 Euro"))
names(data)[123] <- "Risk4AAA"
# LimeSurvey Field type: A
data[, 124] <- as.character(data[, 124])
attributes(data)$variable.labels[124] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 260 Euro?"
data[, 124] <- factor(data[, 124], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 260 Euro"))
names(data)[124] <- "Risk4AAB"
# LimeSurvey Field type: A
data[, 125] <- as.character(data[, 125])
attributes(data)$variable.labels[125] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 220 Euro?"
data[, 125] <- factor(data[, 125], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 220 Euro"))
names(data)[125] <- "Risk4ABA"
# LimeSurvey Field type: A
data[, 126] <- as.character(data[, 126])
attributes(data)$variable.labels[126] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 180 Euro?"
data[, 126] <- factor(data[, 126], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 180 Euro"))
names(data)[126] <- "Risk4ABB"
# LimeSurvey Field type: A
data[, 127] <- as.character(data[, 127])
attributes(data)$variable.labels[127] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 140 Euro?"
data[, 127] <- factor(data[, 127], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 140 Euro"))
names(data)[127] <- "Risk4BAA"
# LimeSurvey Field type: A
data[, 128] <- as.character(data[, 128])
attributes(data)$variable.labels[128] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 100 Euro?"
data[, 128] <- factor(data[, 128], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 100 Euro"))
names(data)[128] <- "Risk4BAB"
# LimeSurvey Field type: A
data[, 129] <- as.character(data[, 129])
attributes(data)$variable.labels[129] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 60 Euro?"
data[, 129] <- factor(data[, 129], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 60 Euro"))
names(data)[129] <- "Risk4BBA"
# LimeSurvey Field type: A
data[, 130] <- as.character(data[, 130])
attributes(data)$variable.labels[130] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 20 Euro?"
data[, 130] <- factor(data[, 130], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 20 Euro"))
names(data)[130] <- "Risk4BBB"
# LimeSurvey Field type: A
data[, 131] <- as.character(data[, 131])
attributes(data)$variable.labels[131] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 310 Euro?"
data[, 131] <- factor(data[, 131], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 310 Euro"))
names(data)[131] <- "Risk5AAAA"
# LimeSurvey Field type: A
data[, 132] <- as.character(data[, 132])
attributes(data)$variable.labels[132] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 290 Euro?"
data[, 132] <- factor(data[, 132], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 290 Euro"))
names(data)[132] <- "Risk5AAAB"
# LimeSurvey Field type: A
data[, 133] <- as.character(data[, 133])
attributes(data)$variable.labels[133] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 270 Euro?"
data[, 133] <- factor(data[, 133], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 270 Euro"))
names(data)[133] <- "Risk5AABA"
# LimeSurvey Field type: A
data[, 134] <- as.character(data[, 134])
attributes(data)$variable.labels[134] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 250 Euro?"
data[, 134] <- factor(data[, 134], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 250 Euro"))
names(data)[134] <- "Risk5AABB"
# LimeSurvey Field type: A
data[, 135] <- as.character(data[, 135])
attributes(data)$variable.labels[135] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 230 Euro?"
data[, 135] <- factor(data[, 135], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 230 Euro"))
names(data)[135] <- "Risk5ABAA"
# LimeSurvey Field type: A
data[, 136] <- as.character(data[, 136])
attributes(data)$variable.labels[136] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 210 Euro?"
data[, 136] <- factor(data[, 136], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 210 Euro"))
names(data)[136] <- "Risk5ABAB"
# LimeSurvey Field type: A
data[, 137] <- as.character(data[, 137])
attributes(data)$variable.labels[137] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 190 Euro?"
data[, 137] <- factor(data[, 137], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 190 Euro"))
names(data)[137] <- "Risk5ABBA"
# LimeSurvey Field type: A
data[, 138] <- as.character(data[, 138])
attributes(data)$variable.labels[138] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 170 Euro?"
data[, 138] <- factor(data[, 138], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 170 Euro"))
names(data)[138] <- "Risk5ABBB"
# LimeSurvey Field type: A
data[, 139] <- as.character(data[, 139])
attributes(data)$variable.labels[139] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 150 Euro?"
data[, 139] <- factor(data[, 139], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 150 Euro"))
names(data)[139] <- "Risk5BAAA"
# LimeSurvey Field type: A
data[, 140] <- as.character(data[, 140])
attributes(data)$variable.labels[140] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 130 Euro?"
data[, 140] <- factor(data[, 140], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 130 Euro"))
names(data)[140] <- "Risk5BAAB"
# LimeSurvey Field type: A
data[, 141] <- as.character(data[, 141])
attributes(data)$variable.labels[141] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 110 Euro?"
data[, 141] <- factor(data[, 141], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 110 Euro"))
names(data)[141] <- "Risk5BABA"
# LimeSurvey Field type: A
data[, 142] <- as.character(data[, 142])
attributes(data)$variable.labels[142] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 90 Euro?"
data[, 142] <- factor(data[, 142], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 90 Euro"))
names(data)[142] <- "Risk5BABB"
# LimeSurvey Field type: A
data[, 143] <- as.character(data[, 143])
attributes(data)$variable.labels[143] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 70 Euro?"
data[, 143] <- factor(data[, 143], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 70 Euro"))
names(data)[143] <- "Risk5BBAA"
# LimeSurvey Field type: A
data[, 144] <- as.character(data[, 144])
attributes(data)$variable.labels[144] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 50 Euro?"
data[, 144] <- factor(data[, 144], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 50 Euro"))
names(data)[144] <- "Risk5BBAB"
# LimeSurvey Field type: A
data[, 145] <- as.character(data[, 145])
attributes(data)$variable.labels[145] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 30 Euro?"
data[, 145] <- factor(data[, 145], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 30 Euro"))
names(data)[145] <- "Risk5BBBA"
# LimeSurvey Field type: A
data[, 146] <- as.character(data[, 146])
attributes(data)$variable.labels[146] <- "Was würden Sie bevorzugen: Eine Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro oder eine sichere Zahlung von 10 Euro?"
data[, 146] <- factor(data[, 146], levels=c("A","B"),labels=c("Verlosung mit einer 50-prozentigen Chance auf 300 Euro und einer 50-prozentigen Chance auf 0 Euro", "sichere Zahlung von 10 Euro"))
names(data)[146] <- "Risk5BBBB"
# LimeSurvey Field type: F
data[, 147] <- as.numeric(data[, 147])
attributes(data)$variable.labels[147] <- "[Energieberatung für Wohngebäude] Welche der folgenden Leistungen bieten Sie eigenständig auf dem Markt an?"
data[, 147] <- factor(data[, 147], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[147] <- "Q8_SQ001"
# LimeSurvey Field type: F
data[, 148] <- as.numeric(data[, 148])
attributes(data)$variable.labels[148] <- "[Energieberatung für Nicht-Wohngebäude (Berechnung nach DIN 18599)] Welche der folgenden Leistungen bieten Sie eigenständig auf dem Markt an?"
data[, 148] <- factor(data[, 148], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[148] <- "Q8_SQ002"
# LimeSurvey Field type: F
data[, 149] <- as.numeric(data[, 149])
attributes(data)$variable.labels[149] <- "[Energiebezogene (Sachverständigen-)Gutachten, z.B. für die Beantragung von Fördermitteln] Welche der folgenden Leistungen bieten Sie eigenständig auf dem Markt an?"
data[, 149] <- factor(data[, 149], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[149] <- "Q8_SQ003"
# LimeSurvey Field type: F
data[, 150] <- as.numeric(data[, 150])
attributes(data)$variable.labels[150] <- "[Energieausweis] Welche der folgenden Leistungen bieten Sie eigenständig auf dem Markt an?"
data[, 150] <- factor(data[, 150], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[150] <- "Q8_SQ004"
# LimeSurvey Field type: F
data[, 151] <- as.numeric(data[, 151])
attributes(data)$variable.labels[151] <- "[Beratung zu Förderprogrammen ] Welche der folgenden Leistungen bieten Sie eigenständig auf dem Markt an?"
data[, 151] <- factor(data[, 151], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[151] <- "Q8_SQ006"
# LimeSurvey Field type: F
data[, 152] <- as.numeric(data[, 152])
attributes(data)$variable.labels[152] <- "[Sonstige] Welche der folgenden Leistungen bieten Sie eigenständig auf dem Markt an?"
data[, 152] <- factor(data[, 152], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[152] <- "Q8_SQ005"
# LimeSurvey Field type: F
data[, 153] <- as.numeric(data[, 153])
attributes(data)$variable.labels[153] <- "Wie viele Haushalte haben Sie im letzten Jahr ungefähr zum Heizungstausch beraten? Eine grobe Schätzung ist ausreichend."
names(data)[153] <- "Q10a"
# LimeSurvey Field type: F
data[, 154] <- as.numeric(data[, 154])
attributes(data)$variable.labels[154] <- "[Wärmepumpe] Welcher Anteil (in Prozent) Ihrer Empfehlungen für Heizsysteme innerhalb des letzten Jahres entfiel jeweils auf die folgenden Heiztechnologien? Eine grobe Schätzung ist ausreichend. "
names(data)[154] <- "Q10b_SQ001"
# LimeSurvey Field type: F
data[, 155] <- as.numeric(data[, 155])
attributes(data)$variable.labels[155] <- "[Pelletheizung] Welcher Anteil (in Prozent) Ihrer Empfehlungen für Heizsysteme innerhalb des letzten Jahres entfiel jeweils auf die folgenden Heiztechnologien? Eine grobe Schätzung ist ausreichend. "
names(data)[155] <- "Q10b_SQ002"
# LimeSurvey Field type: F
data[, 156] <- as.numeric(data[, 156])
attributes(data)$variable.labels[156] <- "[Fernwärme] Welcher Anteil (in Prozent) Ihrer Empfehlungen für Heizsysteme innerhalb des letzten Jahres entfiel jeweils auf die folgenden Heiztechnologien? Eine grobe Schätzung ist ausreichend. "
names(data)[156] <- "Q10b_SQ003"
# LimeSurvey Field type: F
data[, 157] <- as.numeric(data[, 157])
attributes(data)$variable.labels[157] <- "[Gasheizung] Welcher Anteil (in Prozent) Ihrer Empfehlungen für Heizsysteme innerhalb des letzten Jahres entfiel jeweils auf die folgenden Heiztechnologien? Eine grobe Schätzung ist ausreichend. "
names(data)[157] <- "Q10b_SQ004"
# LimeSurvey Field type: F
data[, 158] <- as.numeric(data[, 158])
attributes(data)$variable.labels[158] <- "[Ölheizung] Welcher Anteil (in Prozent) Ihrer Empfehlungen für Heizsysteme innerhalb des letzten Jahres entfiel jeweils auf die folgenden Heiztechnologien? Eine grobe Schätzung ist ausreichend. "
names(data)[158] <- "Q10b_SQ007"
# LimeSurvey Field type: F
data[, 159] <- as.numeric(data[, 159])
attributes(data)$variable.labels[159] <- "[Hybridheizung         ⓘ        Eine Hybridheizung verbindet ein umweltfreundliches, erneuerbares System (wie eine Wärmepumpe) mit einem konventionellen Wärmeerzeuger (wie einer Gas- oder Ölheizung).   ] Welcher Anteil (in Prozent) Ihrer Empfehlungen für Heizsysteme innerhalb des letzten Jahres entfiel jeweils auf die folgenden Heiztechnologien? Eine grobe Schätzung ist ausreichend. "
names(data)[159] <- "Q10b_SQ005"
# LimeSurvey Field type: F
data[, 160] <- as.numeric(data[, 160])
attributes(data)$variable.labels[160] <- "[Sonstiges] Welcher Anteil (in Prozent) Ihrer Empfehlungen für Heizsysteme innerhalb des letzten Jahres entfiel jeweils auf die folgenden Heiztechnologien? Eine grobe Schätzung ist ausreichend. "
names(data)[160] <- "Q10b_SQ006"
# LimeSurvey Field type: F
data[, 161] <- as.numeric(data[, 161])
attributes(data)$variable.labels[161] <- "[Ich sehe keine Hemmnisse] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 161] <- factor(data[, 161], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[161] <- "Q19a_SQ001"
# LimeSurvey Field type: F
data[, 162] <- as.numeric(data[, 162])
attributes(data)$variable.labels[162] <- "[Fehlende finanzielle Mittel beim Kunden] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 162] <- factor(data[, 162], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[162] <- "Q19a_SQ002"
# LimeSurvey Field type: F
data[, 163] <- as.numeric(data[, 163])
attributes(data)$variable.labels[163] <- "[Fachkräftemangel im eigenen Betrieb] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 163] <- factor(data[, 163], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[163] <- "Q19a_SQ003"
# LimeSurvey Field type: F
data[, 164] <- as.numeric(data[, 164])
attributes(data)$variable.labels[164] <- "[Förderprogramme zu unbekannt oder komplex] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 164] <- factor(data[, 164], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[164] <- "Q19a_SQ004"
# LimeSurvey Field type: F
data[, 165] <- as.numeric(data[, 165])
attributes(data)$variable.labels[165] <- "[Unzureichende Qualität der Produkte] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 165] <- factor(data[, 165], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[165] <- "Q19a_SQ005"
# LimeSurvey Field type: F
data[, 166] <- as.numeric(data[, 166])
attributes(data)$variable.labels[166] <- "[Skepsis beim Kunden gegenüber der Technologie] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 166] <- factor(data[, 166], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[166] <- "Q19a_SQ006"
# LimeSurvey Field type: F
data[, 167] <- as.numeric(data[, 167])
attributes(data)$variable.labels[167] <- "[Häufige Veränderungen der gesetzlichen Rahmenbedingungen] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 167] <- factor(data[, 167], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[167] <- "Q19a_SQ007"
# LimeSurvey Field type: F
data[, 168] <- as.numeric(data[, 168])
attributes(data)$variable.labels[168] <- "[Unsicherheit der Betriebskosten] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 168] <- factor(data[, 168], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[168] <- "Q19a_SQ008"
# LimeSurvey Field type: F
data[, 169] <- as.numeric(data[, 169])
attributes(data)$variable.labels[169] <- "[Hohe Investitionskosten für Anschaffung und Installation] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 169] <- factor(data[, 169], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[169] <- "Q19a_SQ009"
# LimeSurvey Field type: F
data[, 170] <- as.numeric(data[, 170])
attributes(data)$variable.labels[170] <- "[Fehlende Infrastruktur (z. B. Fernwärmenetz, Gasnetz)] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 170] <- factor(data[, 170], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[170] <- "Q19a_SQ010"
# LimeSurvey Field type: F
data[, 171] <- as.numeric(data[, 171])
attributes(data)$variable.labels[171] <- "[Sonstiges] Wenn Sie an die Entwicklung der Nachfrage für erneuerbare Heiztechnologien (einschließlich Wärmepumpen) in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 171] <- factor(data[, 171], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[171] <- "Q19a_SQ012"
# LimeSurvey Field type: F
data[, 172] <- as.numeric(data[, 172])
attributes(data)$variable.labels[172] <- "[Ich sehe keine Hemmnisse] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 172] <- factor(data[, 172], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[172] <- "Q19b_SQ001"
# LimeSurvey Field type: F
data[, 173] <- as.numeric(data[, 173])
attributes(data)$variable.labels[173] <- "[Fehlende finanzielle Mittel beim Kunden] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 173] <- factor(data[, 173], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[173] <- "Q19b_SQ002"
# LimeSurvey Field type: F
data[, 174] <- as.numeric(data[, 174])
attributes(data)$variable.labels[174] <- "[Fachkräftemangel im eigenen Betrieb] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 174] <- factor(data[, 174], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[174] <- "Q19b_SQ003"
# LimeSurvey Field type: F
data[, 175] <- as.numeric(data[, 175])
attributes(data)$variable.labels[175] <- "[Förderprogramme zu unbekannt oder komplex] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 175] <- factor(data[, 175], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[175] <- "Q19b_SQ004"
# LimeSurvey Field type: F
data[, 176] <- as.numeric(data[, 176])
attributes(data)$variable.labels[176] <- "[Unzureichende Qualität der Produkte] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 176] <- factor(data[, 176], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[176] <- "Q19b_SQ005"
# LimeSurvey Field type: F
data[, 177] <- as.numeric(data[, 177])
attributes(data)$variable.labels[177] <- "[Skepsis beim Kunden gegenüber der Technologie] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 177] <- factor(data[, 177], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[177] <- "Q19b_SQ006"
# LimeSurvey Field type: F
data[, 178] <- as.numeric(data[, 178])
attributes(data)$variable.labels[178] <- "[Häufige Veränderungen der gesetzlichen Rahmenbedingungen] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 178] <- factor(data[, 178], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[178] <- "Q19b_SQ007"
# LimeSurvey Field type: F
data[, 179] <- as.numeric(data[, 179])
attributes(data)$variable.labels[179] <- "[Unsicherheit der Betriebskosten] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 179] <- factor(data[, 179], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[179] <- "Q19b_SQ008"
# LimeSurvey Field type: F
data[, 180] <- as.numeric(data[, 180])
attributes(data)$variable.labels[180] <- "[Hohe Investitionskosten für Anschaffung und Installation] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 180] <- factor(data[, 180], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[180] <- "Q19b_SQ009"
# LimeSurvey Field type: F
data[, 181] <- as.numeric(data[, 181])
attributes(data)$variable.labels[181] <- "[Fehlende Infrastruktur (z. B. Fernwärmenetz, Gasnetz)] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 181] <- factor(data[, 181], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[181] <- "Q19b_SQ010"
# LimeSurvey Field type: F
data[, 182] <- as.numeric(data[, 182])
attributes(data)$variable.labels[182] <- "[Sonstiges] Wenn Sie an die Entwicklung der Nachfrage für Fernwärme  in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 182] <- factor(data[, 182], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[182] <- "Q19b_SQ012"
# LimeSurvey Field type: F
data[, 183] <- as.numeric(data[, 183])
attributes(data)$variable.labels[183] <- "[Ich sehe keine Hemmnisse] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 183] <- factor(data[, 183], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[183] <- "Q19c_SQ001"
# LimeSurvey Field type: F
data[, 184] <- as.numeric(data[, 184])
attributes(data)$variable.labels[184] <- "[Fehlende finanzielle Mittel beim Kunden] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 184] <- factor(data[, 184], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[184] <- "Q19c_SQ002"
# LimeSurvey Field type: F
data[, 185] <- as.numeric(data[, 185])
attributes(data)$variable.labels[185] <- "[Fachkräftemangel im eigenen Betrieb] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 185] <- factor(data[, 185], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[185] <- "Q19c_SQ003"
# LimeSurvey Field type: F
data[, 186] <- as.numeric(data[, 186])
attributes(data)$variable.labels[186] <- "[Förderprogramme zu unbekannt oder komplex] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 186] <- factor(data[, 186], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[186] <- "Q19c_SQ004"
# LimeSurvey Field type: F
data[, 187] <- as.numeric(data[, 187])
attributes(data)$variable.labels[187] <- "[Unzureichende Qualität der Produkte] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 187] <- factor(data[, 187], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[187] <- "Q19c_SQ005"
# LimeSurvey Field type: F
data[, 188] <- as.numeric(data[, 188])
attributes(data)$variable.labels[188] <- "[Skepsis beim Kunden gegenüber der Technologie] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 188] <- factor(data[, 188], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[188] <- "Q19c_SQ006"
# LimeSurvey Field type: F
data[, 189] <- as.numeric(data[, 189])
attributes(data)$variable.labels[189] <- "[Häufige Veränderungen der gesetzlichen Rahmenbedingungen] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 189] <- factor(data[, 189], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[189] <- "Q19c_SQ007"
# LimeSurvey Field type: F
data[, 190] <- as.numeric(data[, 190])
attributes(data)$variable.labels[190] <- "[Unsicherheit der Betriebskosten] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 190] <- factor(data[, 190], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[190] <- "Q19c_SQ008"
# LimeSurvey Field type: F
data[, 191] <- as.numeric(data[, 191])
attributes(data)$variable.labels[191] <- "[Hohe Investitionskosten für Anschaffung und Installation] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 191] <- factor(data[, 191], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[191] <- "Q19c_SQ009"
# LimeSurvey Field type: F
data[, 192] <- as.numeric(data[, 192])
attributes(data)$variable.labels[192] <- "[Fehlende Infrastruktur (z. B. Fernwärmenetz, Gasnetz)] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 192] <- factor(data[, 192], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[192] <- "Q19c_SQ010"
# LimeSurvey Field type: F
data[, 193] <- as.numeric(data[, 193])
attributes(data)$variable.labels[193] <- "[Sonstiges] Wenn Sie an die Entwicklung der Nachfrage für konventionelle Heiztechnologien in Bestandsgebäuden in Ihrem Kundenkreis denken: Welche Hemmnisse sehen Sie für eine positive Entwicklung?"
data[, 193] <- factor(data[, 193], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[193] <- "Q19c_SQ012"
# LimeSurvey Field type: A
data[, 194] <- as.character(data[, 194])
attributes(data)$variable.labels[194] <- "[Rank 1] Wenn es um Energiedienstleistungen geht: Welche Kundengruppe ist für Ihr Unternehmen die wichtigste / zweitwichtigste Kundengruppe?"
data[, 194] <- factor(data[, 194], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Privathaushalte", "Immobilienwirtschaft", "Öffentliche Hand", "Industrie", "Sonstiges Gewerbe (GHD)"))
names(data)[194] <- "Q7_1"
# LimeSurvey Field type: A
data[, 195] <- as.character(data[, 195])
attributes(data)$variable.labels[195] <- "[Rank 2] Wenn es um Energiedienstleistungen geht: Welche Kundengruppe ist für Ihr Unternehmen die wichtigste / zweitwichtigste Kundengruppe?"
data[, 195] <- factor(data[, 195], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Privathaushalte", "Immobilienwirtschaft", "Öffentliche Hand", "Industrie", "Sonstiges Gewerbe (GHD)"))
names(data)[195] <- "Q7_2"
# LimeSurvey Field type: A
data[, 196] <- as.character(data[, 196])
attributes(data)$variable.labels[196] <- "[Rank 3] Wenn es um Energiedienstleistungen geht: Welche Kundengruppe ist für Ihr Unternehmen die wichtigste / zweitwichtigste Kundengruppe?"
data[, 196] <- factor(data[, 196], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Privathaushalte", "Immobilienwirtschaft", "Öffentliche Hand", "Industrie", "Sonstiges Gewerbe (GHD)"))
names(data)[196] <- "Q7_3"
# LimeSurvey Field type: A
data[, 197] <- as.character(data[, 197])
attributes(data)$variable.labels[197] <- "[Rank 4] Wenn es um Energiedienstleistungen geht: Welche Kundengruppe ist für Ihr Unternehmen die wichtigste / zweitwichtigste Kundengruppe?"
data[, 197] <- factor(data[, 197], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Privathaushalte", "Immobilienwirtschaft", "Öffentliche Hand", "Industrie", "Sonstiges Gewerbe (GHD)"))
names(data)[197] <- "Q7_4"
# LimeSurvey Field type: A
data[, 198] <- as.character(data[, 198])
attributes(data)$variable.labels[198] <- "[Rank 5] Wenn es um Energiedienstleistungen geht: Welche Kundengruppe ist für Ihr Unternehmen die wichtigste / zweitwichtigste Kundengruppe?"
data[, 198] <- factor(data[, 198], levels=c("AO01","AO02","AO03","AO04","AO05"),labels=c("Privathaushalte", "Immobilienwirtschaft", "Öffentliche Hand", "Industrie", "Sonstiges Gewerbe (GHD)"))
names(data)[198] <- "Q7_5"
# LimeSurvey Field type: A
data[, 199] <- as.character(data[, 199])
attributes(data)$variable.labels[199] <- "[Rank 1] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 199] <- factor(data[, 199], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[199] <- "Q28_1"
# LimeSurvey Field type: A
data[, 200] <- as.character(data[, 200])
attributes(data)$variable.labels[200] <- "[Rank 2] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 200] <- factor(data[, 200], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[200] <- "Q28_2"
# LimeSurvey Field type: A
data[, 201] <- as.character(data[, 201])
attributes(data)$variable.labels[201] <- "[Rank 3] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 201] <- factor(data[, 201], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[201] <- "Q28_3"
# LimeSurvey Field type: A
data[, 202] <- as.character(data[, 202])
attributes(data)$variable.labels[202] <- "[Rank 4] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 202] <- factor(data[, 202], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[202] <- "Q28_4"
# LimeSurvey Field type: A
data[, 203] <- as.character(data[, 203])
attributes(data)$variable.labels[203] <- "[Rank 5] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 203] <- factor(data[, 203], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[203] <- "Q28_5"
# LimeSurvey Field type: A
data[, 204] <- as.character(data[, 204])
attributes(data)$variable.labels[204] <- "[Rank 6] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 204] <- factor(data[, 204], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[204] <- "Q28_6"
# LimeSurvey Field type: A
data[, 205] <- as.character(data[, 205])
attributes(data)$variable.labels[205] <- "[Rank 7] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 205] <- factor(data[, 205], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[205] <- "Q28_7"
# LimeSurvey Field type: A
data[, 206] <- as.character(data[, 206])
attributes(data)$variable.labels[206] <- "[Rank 8] Welche drei der folgenden Kriterien sind Ihren Kunden am wichtigsten?"
data[, 206] <- factor(data[, 206], levels=c("AO01","AO02","AO03","AO04","AO05","AO06","AO07","AO08"),labels=c("Ausreichende Heizleistung bei sehr kalten Temperaturen", "Langfristige Gesamtkosten für den Haushalt", "Höhe der Investitionskosten", "Technische Umsetzbarkeit im Gebäude", "Fördermöglichkeiten", "Zukunftssicherheit", "Langlebigkeit der Anlage", "Zeitnahe Umsetzbarkeit"))
names(data)[206] <- "Q28_8"
# LimeSurvey Field type: F
data[, 207] <- as.numeric(data[, 207])
attributes(data)$variable.labels[207] <- "In welchem Jahr wurden Sie geboren?"
names(data)[207] <- "Q21"
# LimeSurvey Field type: A
data[, 208] <- as.character(data[, 208])
attributes(data)$variable.labels[208] <- "Was ist Ihr höchster Bildungsabschluss?"
data[, 208] <- factor(data[, 208], levels=c("AO01","AO02","AO03","AO06"),labels=c("Kein Abschluss", "Abgeschlossene Lehre oder vergleichbarer Abschluss an einer Berufsschule", "Meister, Techniker oder vergleichbarer Abschluss", "Hochschulabschluss (Bachelor, Master, Diplom, Magister, Staatsexamen, Promotion)"))
names(data)[208] <- "Q22"
# LimeSurvey Field type: A
data[, 209] <- as.character(data[, 209])
attributes(data)$variable.labels[209] <- "[Sonstiges] Was ist Ihr höchster Bildungsabschluss?"
names(data)[209] <- "Q22_other"
# LimeSurvey Field type: F
data[, 210] <- as.numeric(data[, 210])
attributes(data)$variable.labels[210] <- "In welchem Jahr haben Sie Ihre Ausbildung abgeschlossen?"
names(data)[210] <- "Q23"
# LimeSurvey Field type: A
data[, 211] <- as.character(data[, 211])
attributes(data)$variable.labels[211] <- "[Wärmepumpen] Haben Sie in den letzten 3 Jahren Fortbildungen zu den folgenden Themen absolviert?"
data[, 211] <- factor(data[, 211], levels=c("AO01","AO02"),labels=c("Ja", "Nein"))
names(data)[211] <- "Q24_SQ001"
# LimeSurvey Field type: A
data[, 212] <- as.character(data[, 212])
attributes(data)$variable.labels[212] <- "[Elektrofachkraft für Wärmepumpen] Haben Sie in den letzten 3 Jahren Fortbildungen zu den folgenden Themen absolviert?"
data[, 212] <- factor(data[, 212], levels=c("AO01","AO02"),labels=c("Ja", "Nein"))
names(data)[212] <- "Q24_SQ002"
# LimeSurvey Field type: A
data[, 213] <- as.character(data[, 213])
attributes(data)$variable.labels[213] <- "[Sanierungsfahrplan] Haben Sie in den letzten 3 Jahren Fortbildungen zu den folgenden Themen absolviert?"
data[, 213] <- factor(data[, 213], levels=c("AO01","AO02"),labels=c("Ja", "Nein"))
names(data)[213] <- "Q24_SQ003"
# LimeSurvey Field type: A
data[, 214] <- as.character(data[, 214])
attributes(data)$variable.labels[214] <- "[Energieberatung] Haben Sie in den letzten 3 Jahren Fortbildungen zu den folgenden Themen absolviert?"
data[, 214] <- factor(data[, 214], levels=c("AO01","AO02"),labels=c("Ja", "Nein"))
names(data)[214] <- "Q24_SQ004"
# LimeSurvey Field type: A
data[, 215] <- as.character(data[, 215])
attributes(data)$variable.labels[215] <- "[Förderprogramme der BEG] Haben Sie in den letzten 3 Jahren Fortbildungen zu den folgenden Themen absolviert?"
data[, 215] <- factor(data[, 215], levels=c("AO01","AO02"),labels=c("Ja", "Nein"))
names(data)[215] <- "Q24_SQ005"
# LimeSurvey Field type: A
data[, 216] <- as.character(data[, 216])
attributes(data)$variable.labels[216] <- "[Sonstiges] Haben Sie in den letzten 3 Jahren Fortbildungen zu den folgenden Themen absolviert?"
data[, 216] <- factor(data[, 216], levels=c("AO01","AO02"),labels=c("Ja", "Nein"))
names(data)[216] <- "Q24_SQ006"
# LimeSurvey Field type: A
data[, 217] <- as.character(data[, 217])
attributes(data)$variable.labels[217] <- "Welche sonstigen Fortbildungen haben Sie in den letzten 3 Jahren absolviert?"
names(data)[217] <- "Q24a"
# LimeSurvey Field type: A
data[, 218] <- as.character(data[, 218])
attributes(data)$variable.labels[218] <- "Wie wird das Gebäude, in dem Sie momentan wohnen, beheizt?"
data[, 218] <- factor(data[, 218], levels=c("AO01","AO02","AO03","AO04","AO07","AO05","AO06"),labels=c("Wärmepumpe", "Pelletheizung", "Fernwärme", "Gasheizung", "Ölheizung", "Hybridheizung", "Sonstiges"))
names(data)[218] <- "Q20"
# LimeSurvey Field type: A
data[, 219] <- as.character(data[, 219])
attributes(data)$variable.labels[219] <- "Haben Sie noch Anmerkungen oder Feedback zu dieser Umfrage? Sie können uns hier gerne eine Nachricht hinterlassen."
names(data)[219] <- "G22Q118"
# LimeSurvey Field type: A
data[, 364] <- as.character(data[, 364])
attributes(data)$variable.labels[364] <- "attribute_1"
names(data)[364] <- "attribute_1"
# LimeSurvey Field type: A
data[, 365] <- as.character(data[, 365])
attributes(data)$variable.labels[365] <- "attribute_2"
names(data)[365] <- "attribute_2"
# LimeSurvey Field type: A
data[, 366] <- as.character(data[, 366])
attributes(data)$variable.labels[366] <- "attribute_3"
names(data)[366] <- "attribute_3"
# LimeSurvey Field type: A
data[, 367] <- as.character(data[, 367])
attributes(data)$variable.labels[367] <- "attribute_4"
names(data)[367] <- "attribute_4"
# LimeSurvey Field type: A
data[, 368] <- as.character(data[, 368])
attributes(data)$variable.labels[368] <- "attribute_5"
names(data)[368] <- "attribute_5"
# LimeSurvey Field type: A
data[, 369] <- as.character(data[, 369])
attributes(data)$variable.labels[369] <- "attribute_6"
names(data)[369] <- "attribute_6"
# LimeSurvey Field type: A
data[, 370] <- as.character(data[, 370])
attributes(data)$variable.labels[370] <- "attribute_7"
names(data)[370] <- "attribute_7"
# LimeSurvey Field type: A
data[, 371] <- as.character(data[, 371])
attributes(data)$variable.labels[371] <- "attribute_8"
names(data)[371] <- "attribute_8"
# LimeSurvey Field type: A
data[, 372] <- as.character(data[, 372])
attributes(data)$variable.labels[372] <- "attribute_9"
names(data)[372] <- "attribute_9"
# LimeSurvey Field type: A
data[, 373] <- as.character(data[, 373])
attributes(data)$variable.labels[373] <- "attribute_10"
names(data)[373] <- "attribute_10"
# LimeSurvey Field type: A
data[, 374] <- as.character(data[, 374])
attributes(data)$variable.labels[374] <- "attribute_11"
names(data)[374] <- "attribute_11"
# LimeSurvey Field type: A
data[, 375] <- as.character(data[, 375])
attributes(data)$variable.labels[375] <- "attribute_12"
names(data)[375] <- "attribute_12"
# LimeSurvey Field type: A
data[, 376] <- as.character(data[, 376])
attributes(data)$variable.labels[376] <- "attribute_13"
names(data)[376] <- "attribute_13"
# LimeSurvey Field type: A
data[, 377] <- as.character(data[, 377])
attributes(data)$variable.labels[377] <- "attribute_14"
names(data)[377] <- "attribute_14"
# LimeSurvey Field type: A
data[, 378] <- as.character(data[, 378])
attributes(data)$variable.labels[378] <- "attribute_15"
names(data)[378] <- "attribute_15"
# LimeSurvey Field type: A
data[, 379] <- as.character(data[, 379])
attributes(data)$variable.labels[379] <- "attribute_16"
names(data)[379] <- "attribute_16"
# LimeSurvey Field type: A
data[, 380] <- as.character(data[, 380])
attributes(data)$variable.labels[380] <- "attribute_17"
names(data)[380] <- "attribute_17"
# LimeSurvey Field type: A
data[, 381] <- as.character(data[, 381])
attributes(data)$variable.labels[381] <- "attribute_18"
names(data)[381] <- "attribute_18"
# LimeSurvey Field type: A
data[, 382] <- as.character(data[, 382])
attributes(data)$variable.labels[382] <- "attribute_19"
names(data)[382] <- "attribute_19"
# LimeSurvey Field type: A
data[, 383] <- as.character(data[, 383])
attributes(data)$variable.labels[383] <- "attribute_20"
names(data)[383] <- "attribute_20"
# LimeSurvey Field type: A
data[, 384] <- as.character(data[, 384])
attributes(data)$variable.labels[384] <- "attribute_21"
names(data)[384] <- "attribute_21"
# LimeSurvey Field type: A
data[, 385] <- as.character(data[, 385])
attributes(data)$variable.labels[385] <- "attribute_22"
names(data)[385] <- "attribute_22"
# LimeSurvey Field type: A
data[, 386] <- as.character(data[, 386])
attributes(data)$variable.labels[386] <- "attribute_23"
names(data)[386] <- "attribute_23"
# LimeSurvey Field type: A
data[, 387] <- as.character(data[, 387])
attributes(data)$variable.labels[387] <- "attribute_24"
names(data)[387] <- "attribute_24"
# LimeSurvey Field type: A
data[, 388] <- as.character(data[, 388])
attributes(data)$variable.labels[388] <- "attribute_25"
names(data)[388] <- "attribute_25"
# LimeSurvey Field type: A
data[, 389] <- as.character(data[, 389])
attributes(data)$variable.labels[389] <- "attribute_26"
names(data)[389] <- "attribute_26"
# LimeSurvey Field type: A
data[, 390] <- as.character(data[, 390])
attributes(data)$variable.labels[390] <- "attribute_27"
names(data)[390] <- "attribute_27"
# LimeSurvey Field type: A
data[, 391] <- as.character(data[, 391])
attributes(data)$variable.labels[391] <- "attribute_28"
names(data)[391] <- "attribute_28"
# LimeSurvey Field type: A
data[, 392] <- as.character(data[, 392])
attributes(data)$variable.labels[392] <- "attribute_29"
names(data)[392] <- "attribute_29"
# LimeSurvey Field type: A
data[, 393] <- as.character(data[, 393])
attributes(data)$variable.labels[393] <- "attribute_30"
names(data)[393] <- "attribute_30"
# LimeSurvey Field type: A
data[, 394] <- as.character(data[, 394])
attributes(data)$variable.labels[394] <- "attribute_31"
names(data)[394] <- "attribute_31"
# LimeSurvey Field type: A
data[, 395] <- as.character(data[, 395])
attributes(data)$variable.labels[395] <- "attribute_32"
names(data)[395] <- "attribute_32"
# LimeSurvey Field type: A
data[, 396] <- as.character(data[, 396])
attributes(data)$variable.labels[396] <- "attribute_33"
names(data)[396] <- "attribute_33"
# LimeSurvey Field type: A
data[, 397] <- as.character(data[, 397])
attributes(data)$variable.labels[397] <- "attribute_34"
names(data)[397] <- "attribute_34"
# LimeSurvey Field type: A
data[, 398] <- as.character(data[, 398])
attributes(data)$variable.labels[398] <- "attribute_35"
names(data)[398] <- "attribute_35"
# LimeSurvey Field type: A
data[, 399] <- as.character(data[, 399])
attributes(data)$variable.labels[399] <- "attribute_36"
names(data)[399] <- "attribute_36"
# LimeSurvey Field type: A
data[, 400] <- as.character(data[, 400])
attributes(data)$variable.labels[400] <- "attribute_37"
names(data)[400] <- "attribute_37"
# LimeSurvey Field type: A
data[, 401] <- as.character(data[, 401])
attributes(data)$variable.labels[401] <- "attribute_38"
names(data)[401] <- "attribute_38"
# LimeSurvey Field type: A
data[, 402] <- as.character(data[, 402])
attributes(data)$variable.labels[402] <- "attribute_39"
names(data)[402] <- "attribute_39"
# LimeSurvey Field type: A
data[, 403] <- as.character(data[, 403])
attributes(data)$variable.labels[403] <- "attribute_40"
names(data)[403] <- "attribute_40"
# LimeSurvey Field type: A
data[, 404] <- as.character(data[, 404])
attributes(data)$variable.labels[404] <- "attribute_41"
names(data)[404] <- "attribute_41"
# LimeSurvey Field type: A
data[, 405] <- as.character(data[, 405])
attributes(data)$variable.labels[405] <- "attribute_42"
names(data)[405] <- "attribute_42"
# LimeSurvey Field type: A
data[, 406] <- as.character(data[, 406])
attributes(data)$variable.labels[406] <- "attribute_43"
names(data)[406] <- "attribute_43"
# LimeSurvey Field type: A
data[, 407] <- as.character(data[, 407])
attributes(data)$variable.labels[407] <- "attribute_44"
names(data)[407] <- "attribute_44"
# LimeSurvey Field type: A
data[, 408] <- as.character(data[, 408])
attributes(data)$variable.labels[408] <- "attribute_45"
names(data)[408] <- "attribute_45"
# LimeSurvey Field type: A
data[, 409] <- as.character(data[, 409])
attributes(data)$variable.labels[409] <- "attribute_46"
names(data)[409] <- "attribute_46"
# LimeSurvey Field type: A
data[, 410] <- as.character(data[, 410])
attributes(data)$variable.labels[410] <- "attribute_47"
names(data)[410] <- "attribute_47"
# LimeSurvey Field type: A
data[, 411] <- as.character(data[, 411])
attributes(data)$variable.labels[411] <- "attribute_48"
names(data)[411] <- "attribute_48"
# LimeSurvey Field type: A
data[, 412] <- as.character(data[, 412])
attributes(data)$variable.labels[412] <- "attribute_49"
names(data)[412] <- "attribute_49"
# LimeSurvey Field type: A
data[, 413] <- as.character(data[, 413])
attributes(data)$variable.labels[413] <- "attribute_50"
names(data)[413] <- "attribute_50"
# LimeSurvey Field type: A
data[, 414] <- as.character(data[, 414])
attributes(data)$variable.labels[414] <- "attribute_51"
names(data)[414] <- "attribute_51"
# LimeSurvey Field type: A
data[, 415] <- as.character(data[, 415])
attributes(data)$variable.labels[415] <- "attribute_52"
names(data)[415] <- "attribute_52"
# LimeSurvey Field type: A
data[, 416] <- as.character(data[, 416])
attributes(data)$variable.labels[416] <- "attribute_53"
names(data)[416] <- "attribute_53"
# LimeSurvey Field type: A
data[, 417] <- as.character(data[, 417])
attributes(data)$variable.labels[417] <- "attribute_54"
names(data)[417] <- "attribute_54"

  # Hand back the fully labelled data.frame.
  return(data)
}
