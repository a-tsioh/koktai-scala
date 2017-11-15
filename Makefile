


Data/recoded.u8:
	cat Data/Original/*.dic | perl ./Scripts/recode_utf8.pl > Data/recoded.u8

app/jvm/target/scala-2.12/koktai-scala_2.12-0.2.jar:
	sbt package

WikiFiles: app/jvm/target/scala-2.12/koktai-scala_2.12-0.2.jar Data/recoded.u8
	sbt "appJVM/run ./Data/recoded.u8 ./Wiki/ ./Data/koktai-ids.csv"


clean: 
	sbt clean ; rm Data/recoded.u8
