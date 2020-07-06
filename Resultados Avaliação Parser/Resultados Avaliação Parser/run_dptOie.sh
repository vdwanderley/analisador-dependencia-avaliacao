java -jar ../DptOIE/DptOIE.jar -dependencyTreeIN 'lys/parse.conllux' -SC true -CC true -appositive 1 -appositive 2 > ./lys/lys_extractions.txt
cp extractedFactsByDpOIE.csv ./lys/
#
java -jar ../DptOIE/DptOIE.jar -dependencyTreeIN 'mquni/output_mquni.conllux' -SC true -CC true -appositive 1 -appositive 2 > ./mquni/mquni_extractions.txt
cp extractedFactsByDpOIE.csv ./mquni/
#
java -jar ../DptOIE/DptOIE.jar -dependencyTreeIN 'orange/result-deprojectivised.conllux' -SC true -CC true -appositive 1 -appositive 2 > ./orange/orange_extractions.txt
cp extractedFactsByDpOIE.csv ./orange/
#
java -jar ../DptOIE/DptOIE.jar -dependencyTreeIN 'stanford/output_stanford.conllux' -SC true -CC true -appositive 1 -appositive 2 > ./stanford/stanford_extractions.txt
cp extractedFactsByDpOIE.csv ./stanford/


