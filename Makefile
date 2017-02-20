


Data/recoded.u8:
	cat Data/Original/*.dic | perl ./Scripts/recode_utf8.pl | python ./Scripts/convert.py > Data/recoded.u8


target/scala-2.11/koktai-scala_2.11-0.1.jar:
	sbt package

WikiFiles: target/scala-2.11/koktai-scala_2.11-0.1.jar
	scala  target/scala-2.11/koktai-scala_2.11-0.1.jar ./Data/koktai-ids.csv ./Data/recoded.u8 ./Wiki/

clean: 
	sbt clean ; rm Data/recoded.u8
