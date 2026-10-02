/* Setting path and library */
libname datalib "/home/u64435675/sasuser.v94/Labs";

/* Reading in the data from the txt file, ignoring the header */
data diet;
infile "/home/u64435675/sasuser.v94/Labs/diet.txt" firstobs=2; 
input Diet PostChol PreChol;
run;

/* Checking that it loaded in correctly */
proc print data=diet;
run;

/* Running the glm model for unequal slopes ANCOVA */
ods graphics on;

proc glm data=diet plots=fitplot;
class Diet;
model PostChol=Diet PreChol Diet*PreChol;
lsmeans Diet / cl alpha=0.05;
lsmeans Diet / at PreChol=190 cl alpha=0.05 pdiff;
lsmeans Diet / at PreChol=250 cl alpha=0.05 pdiff;
run;

ods graphics off;