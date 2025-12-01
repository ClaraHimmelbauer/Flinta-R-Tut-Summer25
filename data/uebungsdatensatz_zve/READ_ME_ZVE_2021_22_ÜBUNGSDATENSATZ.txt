****************************************************************************
*** SUBSAMPLE/ÜBUNGSDATENSATZ - Mikrodaten der Zeitverwendungserhebung 2021/022
****************************************************************************
Statistik Austria führte im Auftrag der Bundesministerin für Frauen, Familie, Integration und Medien 
von Oktober 2021 bis Dezember 2022 eine Erhebung über die Verwendung von Zeit durch. 
Dabei haben alle Personen über 9 Jahren aus 4.342 Haushalten, zwei Tage lang ein Tagebuch geführt, 
in das sie alle Tätigkeiten eigetragen haben, die 10 Minuten oder länger dauern. 
Die Mitarbeit bei dieser Erhebung war freiwillig, die Vorgaben bei der Erhebung und 
Auswertung orientierten sich dabei an europäischen Richtlinien (HETUS-Guidelines von EUROSTAT).
Bei diesem SUBSAMPLE handelt es sich um einen ÜBUNGSDATENSATZ; bestehend aus 200 zufällig gezogenen Haushalte (aus den 4.342 Respondent:innenhaushalten).

****************************************************************************
*** "Zeitverwendung 2021/22 - Mikrodaten"
****************************************************************************
*subsample
	* Die Daten beinhalten 200 zufällig gezogenen Haushalte aus der gesamten bereinigten Nettostichprobe (4.342 Haushalte). Die Daten sind daher NICHT repräsentativ. Sie eigenen sich lediglich für Übungszwecke - um die Datenstruktur kennenzulernen. Die Gewichte wurden auch NICHT neu erstellt und an das Subsample angepasst.  
	1.) subsample_ddfile_suf.csv - Zusatziformationen zu jedem Tagebuchtag - File auf Tagesebene (jede Person zweimal) 
	2.) subsample_efile_suf.csv - Tagebuchdatei - auf Timeslotebene (jede Person 144 Timeslots pro Tag - 288 Timelsots insgesamt)
	3.) subsample_indfile_suf.csv - Fragebogendatei für jeden Haushalt und jede Person die an der Erhebung teilnahm und Tagebücher ausgefüllt hat - auf Personenebene (jede Person, die 10 Jahre oder älter ist, einmal)
	4.) subsample_kinder_indfile_suf.csv - Zusatzuinformationen zu den Kindern unter 10 Jahren (haben kein Tagebuch ausgefüllt) auf Personenebene (jede Person unter 10 Jahren einmal)
	* mit der Haushalts- und Personenkennung können die Dateien verküpft werden

*codebooks
	1.) DDFILE_suf_variablen.xlsx - Codebook für Mikrodatendatei: ddfile_suf.csv
	2.) EFILE_SUF_variablen.xlsx - Codebook für Mikrodatendatei: efile_suf.csv
	3.) INDFILE_SUF_variablen.xlsx - Codebook für Mikrodatendatei: indfile_suf.csv
	4.) KINDER_INDFILE_SUF_variablen.xlsx - Codebook für Mikrodatendatei: kinder_indfile_suf.csv	

*Guidelines_Fragebogen_Tagebuch
	1.) ACL_HETUS_Wertelabels.pdf
	2.) Fragebogen_CAWI.pdf
	3.) HETUS_Guidelines.pdf
	4.) Tagebuch_hochgestellt_SIE.pdf



****************************************************************************
*** SUBSAMPLE/EXERCISE DATA SET - Microdata from the 2021/022 time use survey
*****************************************************************************
Statistics Austria conducted a survey on time use on behalf of the Federal Minister for Women, Family, Integration and Media from October 2021 to December 2022. All persons over the age of 9 from 4,342 households kept a diary for two days, in which they recorded all activities lasting 10 minutes or longer. Participation in this survey was voluntary, and the guidelines for the survey and evaluation were based on European guidelines (HETUS guidelines from EUROSTAT).This SUBSAMPLE is a PRACTICE DATA SET consisting of 200 randomly selected households (from the 4,342 respondent households).

*****************************************************************************
*** ‘Time Use 2021/22 - Microdata’
*****************************************************************************
*subsample    
	* The data includes 200 randomly selected households from the entire adjusted net sample (4,342 households). The data is therefore NOT representative. It is only suitable for practice purposes – to familiarise yourself with the data structure. The weights have also NOT been recreated and adjusted to the subsample.     
	1.) subsample_ddfile_suf.csv – Additional information for each diary day – File at daily level (each person twice) 
    	2.) subsample_efile_suf.csv – Diary file – at timeslot level (each person 144 timeslots per day – 288 timeslots in total)
	3.) subsample_indfile_suf.csv - Questionnaire file for each household and each person who participated in the survey and completed diaries - at person level (each person aged 10 or older once)
	4.) subsample_kinder_indfile_suf.csv - Additional information on children under 10 years of age (did not fill out a diary) at the person level (each person under 10 years of age once)
	* The files can be linked using the household and person IDs.

*codebooks
	1.) DDFILE_suf_variables.xlsx – codebook for microdata file: ddfile_suf.csv
	2.) EFILE_SUF_variables.xlsx – codebook for microdata file: efile_suf.csv
	3.) INDFILE_SUF_variables.xlsx - Codebook for microdata file: indfile_suf.csv
	4.) KINDER_INDFILE_SUF_variables.xlsx - Codebook for microdata file: kinder_indfile_suf.csv

*Guidelines_Questionnaire_Diary
	1.) ACL_HETUS_value_labels.pdf
    	2.) Questionnaire_CAWI.pdf
    	3.) HETUS_Guidelines.pdf
    	4.) Diary_superscripted_SIE.pdf

Translated with DeepL.com (free version)
