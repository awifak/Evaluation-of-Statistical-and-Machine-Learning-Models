/* EXPLANATORY DATA ANALYSIS */

ods graphics on;

data DIABETES;
    infile "chemin/Diabetes.txt"
           dlm='09'x
           firstobs=3;

    input AGE SEX BMI BP S1 S2 S3 S4 S5 S6 Y_char :$20.;

    Y = input(compress(Y_char, '\'), best.);

    drop Y_char;

    label
        AGE = "Age"
        SEX = "Sexe"
        BMI = "Indice de Masse Corporelle"
        BP  = "Pression Artérielle Moyenne"
        Y   = "Progression du Diabète (Cible)";
run;

proc contents data=DIABETES varnum;
run;

proc freq data=DIABETES;
    tables SEX / missing;
run;

proc means data=DIABETES n nmiss mean std min p25 median p75 max;
    var AGE BMI BP S1 S2 S3 S4 S5 S6 Y;
run;

proc univariate data=DIABETES normal;
    var BMI BP S2 S6 Y;
    histogram BMI BP S2 S6 Y / normal kernel;
    qqplot    BMI BP S2 S6 Y / normal(mu=est sigma=est);
    inset n mean std min max / position=ne;
run;

proc sgplot data=DIABETES;
    vbox Y / category=SEX;
    refline 150 / axis=y lineattrs=(pattern=dot);
run;

proc sgplot data=DIABETES;
    vbox BMI / category=SEX;
run;

proc stdize data=DIABETES out=DIABETES_STD method=std;
    var AGE BMI BP S1 S2 S3 S4 S5 S6;
run;

data DIABETES_PLOT;
    set DIABETES_STD;
    array xvars[*] AGE BMI BP S1 S2 S3 S4 S5 S6;
    do i = 1 to dim(xvars);
        Variable = vname(xvars[i]);
        Value    = xvars[i];
        output;
    end;
    keep Variable Value;
run;

proc sgplot data=DIABETES_PLOT;
    vbox Value / category=Variable;
    refline 0 / axis=y lineattrs=(pattern=shortdash);
    xaxis display=(nolabel) fitpolicy=rotate;
    yaxis label="Valeur standardisée";
run;
title;

proc corr data=DIABETES pearson;
    var AGE SEX BMI BP S1 S2 S3 S4 S5 S6;
run;

proc corr data=DIABETES pearson best=5;
    var AGE SEX BMI BP S1 S2 S3 S4 S5 S6;
    with Y;
run;

proc sgplot data=DIABETES;
    scatter x=BMI y=Y / group=SEX markerattrs=(symbol=CircleFilled);
    reg x=BMI y=Y;
    ellipse x=BMI y=Y / alpha=0.05 type=mean;
run;

proc reg data=DIABETES;
    model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 / vif collin;
run;
quit;

ods graphics off;


/* CORRELATION MATRIX */
data DIABETES;
    infile "chemin/Diabetes.txt" dlm='09'x firstobs=3;
    input AGE SEX BMI BP S1 S2 S3 S4 S5 S6 Y;
run;


ods graphics on;

proc corr data=DIABETES;
    var AGE -- S6;
run;


proc iml;
    use DIABETES;
    read all var {AGE SEX BMI BP S1 S2 S3 S4 S5 S6} into MatX;
    close DIABETES;

    R = corr(MatX);
   
    noms = {AGE SEX BMI BP S1 S2 S3 S4 S5 S6};
    print R[colname=noms rowname=noms format=5.2];
quit;

/* BOXPLOTS */
data DIABETES;
    infile "chemin/Diabetes.txt" dlm='09'x firstobs=3;
    input AGE SEX BMI BP S1 S2 S3 S4 S5 S6 Y;
run;

proc stdize data=DIABETES out=DIABETES_STD;
    var AGE -- S6;
run;

data DIABETES_PLOT;
    set DIABETES_STD;
    array vars[*] AGE -- S6;
    do i = 1 to dim(vars);
        Variable = vname(vars[i]);
        Value = vars[i];
        output;
    end;
    keep Variable Value;
run;

title "Variable Distribution (Standardized)";
proc sgplot data=DIABETES_PLOT;
    vbox Value / category=Variable group=Variable;
    xaxis display=(nolabel);
    yaxis label="Standardized Value";
run;
title;

/*SIMULATIONS*/

data WORK.DIABETES;
    infile "chemin/Diabetes.txt"
           dlm='09'x
           missover
           firstobs=3;

    input AGE SEX BMI BP S1 S2 S3 S4 S5 S6 Y_char :$20.;
   
    Y = input(compress(Y_char, '\;'), best.);

    drop Y_char;
run;


title "1. Sélection STEPWISE avec critère AIC";
proc glmselect data=WORK.DIABETES plots=(CriterionPanel ASEPlot CoefficientPanel) seed=12345;
    partition fraction(validate=0.3);
   
    model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 /
          selection=stepwise(select=AIC choose=validate)
          stats=all;
run;

title "2. Sélection STEPWISE avec critère SBC";
proc glmselect data=WORK.DIABETES plots=(CriterionPanel ASEPlot CoefficientPanel) seed=12345;
    partition fraction(validate=0.3);
   
    model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 /
          selection=stepwise(select=SBC choose=validate)
          stats=all;
run;


title "3. Régression LASSO";
proc glmselect data=WORK.DIABETES plots=(CriterionPanel ASEPlot CoefficientPanel) seed=12345;
    partition fraction(validate=0.3);
   
    model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 /
          selection=lasso(stop=SBC choose=CV)
          cvmethod=random(10);
run;

title "4. Régression ELASTIC NET";
proc glmselect data=WORK.DIABETES plots=(CriterionPanel ASEPlot CoefficientPanel) seed=12345;
    partition fraction(validate=0.3);
   
    model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 /
          selection=elasticnet(stop=SBC choose=CV)
          cvmethod=random(10);
run;

title;
