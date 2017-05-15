#mono ../../src/bin/Release/propane.exe --genLinkTests --stats --policy fat4_con.pro --topo fat4_con.xml
time for file in test*; do cbgp < $file >> generatedOutput; done
sort -k1 -g ExpectedOutput.txt | cut -f1 -d" " --complement > CleanedOutput.txt
diff CleanedOutput.txt generatedOutput
#rm generatedOutput CleanedOutput.txt ExpectedOutput.txt
#rm test*