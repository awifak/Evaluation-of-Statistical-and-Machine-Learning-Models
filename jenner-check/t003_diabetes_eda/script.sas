/* EXPLANATORY DATA ANALYSIS */
/* Author's Empirical Analysis.sas EDA pipeline. Their external data file
   ("chemin/Diabetes.txt") is replaced with a small, self-contained
   diabetes-shaped sample inlined below as DATALINES (AGE SEX BMI BP S1-S6 Y),
   so the bundle runs with no external file. The PROC steps and their options
   are the author's, unchanged. */

ods graphics on;

data DIABETES;
    input AGE SEX BMI BP S1 S2 S3 S4 S5 S6 Y;

    label
        AGE = "Age"
        SEX = "Sexe"
        BMI = "Indice de Masse Corporelle"
        BP  = "Pression Artérielle Moyenne"
        Y   = "Progression du Diabète (Cible)";
    datalines;
39  1  27.5  65.4  234  60.5  66.9  8.45  3.8698  69  81
24  2  19.4  102.1  154  168.2  66.9  2.44  4.9262  64  160
33  1  31.4  71.5  204  70.6  31.1  4.19  5.5832  81  202
31  2  20.3  112.6  241  53.6  37.9  6.82  4.4763  98  172
48  2  26.7  79.6  143  182.0  40.8  6.07  4.7543  101  136
23  1  30.3  73.7  184  72.1  59.7  2.28  5.1618  98  130
40  2  32.3  103.2  213  55.4  29.2  3.91  5.2439  66  231
60  2  24.8  89.4  268  111.3  94.4  4.52  4.9985  121  172
22  1  36.4  71.2  160  121.5  92.6  5.52  3.7321  109  45
71  2  38.7  81.8  203  239.7  74.6  4.7  3.9155  68  105
30  1  23.6  78.6  221  208.5  36.0  4.0  3.6731  105  70
27  1  29.0  123.8  271  201.8  52.2  4.83  3.5531  109  117
22  1  19.6  76.8  138  63.7  68.3  2.73  4.8728  70  170
23  1  32.7  72.5  161  233.5  68.4  5.36  3.5867  120  81
48  2  29.6  68.1  123  192.1  79.0  5.39  5.2297  124  224
52  2  21.5  100.6  103  193.8  45.0  6.56  3.5174  91  111
52  2  39.8  87.3  154  148.5  82.0  4.34  3.8935  82  149
66  1  22.8  97.0  284  47.4  24.2  3.98  3.9965  102  67
47  2  40.9  87.9  153  62.1  58.2  4.39  4.6331  58  118
70  1  38.0  70.5  196  198.7  79.8  5.39  3.7667  100  112
24  2  29.1  114.8  118  187.1  35.1  2.9  3.6887  117  137
71  2  33.8  86.9  237  151.7  23.6  7.67  5.3275  71  162
52  1  28.4  123.9  151  47.2  38.4  5.55  5.4337  99  206
27  1  39.8  87.1  214  174.6  84.8  5.66  5.6145  122  229
27  1  30.6  63.3  209  197.5  68.9  7.5  3.6849  76  39
54  1  25.8  98.8  239  138.5  81.8  8.26  3.42  82  109
36  1  36.5  98.0  240  47.2  90.8  2.45  4.1857  122  131
36  2  30.2  119.3  226  230.7  75.8  8.21  5.9423  91  214
78  1  38.2  71.7  128  120.4  46.3  6.76  4.4784  85  111
76  1  40.5  107.7  190  70.3  90.0  8.86  3.8837  70  103
;
run;

proc contents data=DIABETES varnum;
run;

proc freq data=DIABETES;
    tables SEX / missing;
run;

proc means data=DIABETES n nmiss mean std min p25 median p75 max;
    var AGE BMI BP S1 S2 S3 S4 S5 S6 Y;
run;

proc corr data=DIABETES pearson best=5;
    var AGE SEX BMI BP S1 S2 S3 S4 S5 S6;
    with Y;
run;

proc reg data=DIABETES;
    model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 / vif collin;
run;
quit;

ods graphics off;
