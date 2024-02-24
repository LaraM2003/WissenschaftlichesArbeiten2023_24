###############################################################################
####################### Wissenschaftliches Arbeiten ###########################
################################ WS 2023/24 ###################################
###############################################################################


# Funktionen und Hilfsfunktionen laden
source("Code/Aufgabe1-Teil1.R")
source("Code/Aufgabe2-SkriptA.R")


statscategorials(titanic_aufbereitet$Survived)
# Bemerkung: 1 - ueberlebt; 0 - Gestorben

# Den Untergang der Titanic ueberlebten 342 (ca. 38.38%) der Passagiere. 549
# (ca. 61.61%) starben.
# Im Folgenden betrachten wir verschiedene moegliche Einflussfaktoren und ihre 
# Auswirkung auf die Chance eines Passagieres den Untergang zu ueberleben.

# Der erste moegliche Einflussfaktor ist hierbei die finanziellen Mittel des
# Passagieres. Wir haben zwar keine Informationen ueber die Berufe/das Vermoegen
# der Passagiere der Titanic, allerdings kennen wir ihre Ticketpreise (Merkmal
# Fare), sowie die Klasse, in welcher sie gereist sind (Merkmal PClass).
# Wir nehmen im Folgenden daher an, dass nur wohlhabende Leute sich teure
# Tickets bzw. die hoeheren Klassen leisten konnten.

# Die zweite moegliche Einflussvariable ist das Geschlecht der Passagiere.

# Die dritte ist das Alter der
# Passagiere zum Zeitpunkt des Untergangs.

# Die vierte Variable ist die Schiffsseite, auf der ihre Kabine lag.

plotCategorials(data.frame("a" = titanic_aufbereitet$Survived, 
             "b" = titanic_aufbereitet$Sex,
             "c" = titanic_aufbereitet$Pclass),
  "ueberlebt", "Geschlecht", "Klasse")

# Wie wir in dieser Grafik sehen ist die Mehrheit der Passagiere gestorben (a),
# maennlich (b) und war in der dritten Klasse eingebucht (c). Natuerlich laesst
# dies nur den Durchschnittlichen Passagier der Titanic erahnen und nicht die
# Zusammenhaenge zwischen den Variablen. Daher werden wir diese im Folgenden
# genauer betrachten.

# Wir beginnen mit dem Zusammenhang zwischen dem ueberleben und der finanziellen
# Situation der Passagiere.


statsmetric(titanic_aufbereitet$Fare)
# Minimum = 0
# Maximum = 512.3292
# Mean    = 32.20421
# Median  = 14.4542

# Die Ticketpreise reichten von 0 bis ca. 512. Im Schnitt zahlten die Passagiere
# etwa 32 (vermutlich Pound). Allerdings ist der Median deutlich niedriger,
# weswegen anznehmen ist, dass solche hohen Werte wie das Maximum den Mittel-
# wert verzerren. 


statsDichoMetric(titanic_aufbereitet$Fare, titanic_aufbereitet$Survived, "Ticketpreis")

######### Gestorben ########### ueberlebt ####### 
#########
# Mittel  22.11789              48.39541
# Varianz 985.21951             4435.16016
# sA      31.38821              66.59700

# Wie man an den Mittelwerten, aber auch deutlich in der Grafik sehen kann,
# haben die Menschen, welche ueberlebt haben im Schnitt deutlich mehr bezahlt,
# als die, die gestorben sind.
# Wie man sieht reicht die Box, sowie der obere Whisker der ueberlebenden (1)
# deutlich weiter nach rechts, was ebenfalls zeigt, dass die meisten
# ueberlebenden teurere Tickets hatten und somit wahrscheinlich wohlhabender
# waren.
# Der Boxplot der Verstorbenen (0) hingegen hat nur eine sehr kleine Box (sprich
# eine sehr kleine Differenz zwischen dem oberen und unteren Quartil) und kurze
# Whiskers. Alles hoeher als ca. 50 sind nur noch Ausreisser. Dies zeigt deutlich,
# dass diese Gruppe zum Grossteil aus Menschen der billigeren Ticketkategorien
# bestand.




# Als naechstes analysieren wir die Verbindung zwischen dem Geschlecht der 
# Passagiere und ihrem ueberleben.

statscategorials(titanic_aufbereitet$Sex)
# Ungefaehr 35% der Passagiere (314) waren weiblich, waehrend 64% (577) maennlich 
# waren.

statsTwoCategorials(titanic_aufbereitet$Sex, titanic_aufbereitet$Survived)

# Zur kurzen Erinnerung/Erklaerung:

# 0           - gestorben
# 1           - ueberlebt
# kategorial1 - Merkmal Geschlecht
# kategorial2 - Merkmal ueberleben


# Insbesondere in der zweiten Kreuztabelle ist deutlich zu erkennen, dass die 
# Mehrheit der Frauen ueberlebt hat (233:81), waehrend die meisten Maenner 
# gestorben sind (468:109). Der Chis-Quadrat-Test untermauert dies durch einen 
# P-Wert von "< 2.2e-16", was darauf hinweist, dass die Standard-Nullhypothese 
# (die Annahme, dass die Variablen unabhaengig sind) abgelehnt werden sollte. 
# Somit besteht hoechstwahrscheinlich eine Abhaengigkeit zwischen den Variablen.

# Folglich laesst sich feststellen, dass das Geschlecht einen Einfluss auf die 
# ueberlebenschancen der Passagiere der Titanic hatte. Frauen ueberlebten mit 
# einer hoeheren Wahrscheinlichkeit im Vergleich zu Maennern.

plotAgeSex()

# Zur ueberleitung ins naechste Merkmal soll hier einmal der Zusammenhang zwischen
# Alter und Geschlecht dargestellt werden. Gut sichtbar ist hier, dass es vor
# allem in der Klasse der ca. 35 Jaehringen deutlich mehr Maenner als Frauen gab.

# Jetzt betrachten wir den Zusammenhang zwischen dem Alter der
# Passagiere und ihres ueberlebens des Untergang.

statsmetric(titanic_aufbereitet$Age)
# Minimum = 0.42
# Maximum = 80
# Mean    = 29.75466
# Median  = 30


# Der Juengste Passagier der Titanic war bei ihrem Untergang 0.42 Jahre alt 
# (also etwa 5 Monate). Der aelteste Passagier war 80 Jahre alt. Die Passagiere
# waren im Durchschnitt etwa 30 Jahre alt.

statsDichoMetric(titanic_aufbereitet$Age, titanic_aufbereitet$Survived, "Alter")

######### Gestorben ########### ueberlebt ####### 
#########
# Mittel  30.68821              28.25606
# Varianz 160.47939             198.54272
# sA      12.66805              14.09052

# Wie man sowohl an den ausgegebenen Werten, wie auch den zwei Boxplots gut
# sehen kann hat das Alter scheinbar einen wesentlich kleineren Einfluss
# auf die ueberlebenswahrscheinlichkeit, als z.B. die finaziellen Mittel.
# Zwar liegt das Durschnittliche Alter der ueberlebenden (1) leicht unter
# dem der Verstorbenen (0), aber der Unterschied liegt bei nur etwa 2 Jahren.
# Auch die Quartile sind sehr aehnlich und die Whiskers haben eine aehnliche
# Laenge. 

# Das Alter scheint also scheinbar keinen grossen Einfluss darauf zu haben,
# ob ein Passagier den Untergang der Titanic ueberlebt hat oder nicht.


# Zum Abschluss stellen wir noch den Zusammenhang zwischen dem ueberleben und
# der Schiffseite dar, auf der die Kabine der Passagiere lag.
# Als Zusatzinformation sei gegeben, dass das Schiff den Eisberg mit der
# Steuerbordseite zusammenstiess.

plotRoom()

# Wie man an dem grossen "NA" Balken sehen kann, ist bei vielen Passagieren nicht
# bekannt, wo ihre Kabine gelegen war.
# Bei den bekannten Kabinen gab es ziemlich genau gleich viele ueberlebende auf
# beiden Seiten des Schiffs. Allerdings ist auch die Zahl der Tode ziemlich
# aehnlich und nur auf der Backbord (Port) Seite leicht hoeher.
# Wir koennen also an den vorliegenden Daten nicht wirklich ermitteln, ob die
# Schiffsseite der Kabine einen Einfluss auf die ueberlebenswahrscheinlichkeit
# hatte. Bei den bekannten Kabinen scheint dies aber nicht der Fall zu sein.

