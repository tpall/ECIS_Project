# Electric cell-substrate impedance assay


The measurement of cell layer resistance was performed using ECIS Zθ and 96WE1_PET plates (Applied Biophysics, Troy, NY, USA). Before use, 96-well plate was pretreated with 10 mM cysteine (200 μl/well) for 12 min at RT, washed 2 times with PBS (175 μl/well) and thereafter coated with 0.1% gelatin in 150 mM NaCl (200 μl/well) for 1 h at RT. 

HUVEC were seeded at density 5000 cells/well. After 30 h cells were switched to starvation media – M199 supplemented with 1% FBS, 25 mM Hepes and 4 mM L-glutamine for overnight (15-16 hours). After starvation, cells were treated with different concentrations of hIgG-Fc or CD44-3MUT-Fc in 155 μl of 5% FBS containing HUVEC growth media (5% FBS, M199, 4 mM L-glutamine, 12.5 μg/ml heparin, 10 mM Hepes, 7.5 μg/ml ECGS) for 1 hour at 37°C. Thereafter the cells were stimulated with VEGF, FGF2, GDF2 or HGF. For that 20 μl of 8.75 × growth factor solution in 5% FBS containing HUVEC growth media was added to cells, so that final media volume was 175 μl/well. The growth of stimulated HUVEC was further monitored in ECIS for at least 72 hours at 37°C.

All experiment data was exported from ECIS Software to xls file using export to Excel command. Excel files from rawdata folder were exported to sqlite database using 
src/Import-to-DB.R script.


Data file name examples:
/rawdata/ECIS_131123_MFT_1.xls 
/rawdata/ECIS_140419_MFT_1.xls

Metadata file name examples (each metadata layer has its own file with shape of a 96 well plate):
/rawdata/ECIS_140419_Metadata_treatment.csv

treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD
treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD
treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD
treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD
treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD
treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD
treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD
treatA,treatA,treatA,treatB,treatB,treatB,treatC,treatC,treatC,treatD,treatD,treatD

/rawdata/ECIS_140419_Metadata_doses_treatment.csv

0,0,0,0,0,0,0,0,0,0,0,0
126,126,126,126,126,126,126,126,126,126,126,126
400,400,400,400,400,400,400,400,400,400,400,400
1265,1265,1265,1265,1265,1265,1265,1265,1265,1265,1265,1265
4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,4000,4000
8000,8000,8000,8000,8000,8000,8000,8000,8000,8000,8000,8000
12640,12640,12640,12640,12640,12640,12640,12640,12640,12640,12640,12640
16000,16000,16000,16000,16000,16000,16000,16000,16000,16000,16000,16000


