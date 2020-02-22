# koktai-scala

This repository held the original data of the 《吳守禮國台對照活用辭典》 Mandarin-Taiwanese dictionary authored by Ngô ShuLeh, 
the result of crowdsourcing process to recode some PUA Taiwanese sinograms into today's Unicode,
and the code to convert, parse and generate a TEI version of the dictionary.

# Get the data

If you don't want to touch the code, the TEI version is also made available at  [Zenodo]()
(https://zenodo.org/record/1308746), a repository for Open Science hosted at CERN.  


# Build the TEI file

To build the TEI (xml) file, you should only need [SBT](https://www.scala-sbt.org/).

The simplest way is to clone the repo and `cd` into it, and run:
```
$ sbt console
scala> koktai.ExporterTEI.writeToTmp()
```
the xml file will be written to the /tmp/ folder. 


# Licence

《吳守禮國台對照活用辭典》作者：吳守禮（Ngo ShuLeh、Wu Shouli） ，本 koktai-scala 的資料由吳守禮家族授權中華民國維基媒體協會，以創用CC 姓名標示-相同方式分享 3.0 台灣 授權條款釋出。
