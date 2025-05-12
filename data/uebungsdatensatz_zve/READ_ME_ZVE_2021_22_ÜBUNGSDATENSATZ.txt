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
