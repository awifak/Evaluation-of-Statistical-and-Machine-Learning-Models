/* Real bundle built from the author's own Gaussian data-generating process (DGP_Gaussien) in
   "Simulations and metrics.sas": a 50-regressor design where only X1..X6 have nonzero
   coefficients (Beta[1:6] = 1, -0.35, 0.15, 0.27, 0.57, -0.14). Their PROC IML generator is
   unchanged except MC is set to 1 (one simulated dataset). A single PROC GLMSELECT then performs
   stepwise/SBC model selection on that dataset -- the core question of the project -- so the
   captured listing shows which regressors the procedure recovers. */

proc iml;

call randseed(12345);
n  = 200;
p  = 50;
MC = 1;

Beta = j(p, 1, 0);
Beta[1:6] = {1, -0.35, 0.15, 0.27, 0.57, -0.14};

Mu       = j(1, p, 0);
Varcovar = I(p);

simulated_data = j(n*MC, 2+p, .);
a = 1;

do iteration = 1 to MC;
  X   = RandNormal(n, Mu, Varcovar);
  eps = normal(j(n, 1, 0))*0.1;
  Y   = X * Beta + eps;

  simulated_data[a:a+n-1, 1]     = iteration;
  simulated_data[a:a+n-1, 2]     = Y;
  simulated_data[a:a+n-1, 3:2+p] = X;
  a = a + n;
end;

cname = {"Iteration_ID" "Y"};
do i = 1 to p;
  cname = cname || cats("X", i);
end;

create DGP_Gaussien from simulated_data[colname=cname];
append from simulated_data;
close DGP_Gaussien;

quit;

title "Stepwise (SBC) selection on one DGP-Gaussien draw: true signal is X1-X6";
ods select SelectionSummary ParameterEstimates;
proc glmselect data=DGP_Gaussien plots=none;
   model Y = X1-X50 / selection=stepwise(choose=SBC stop=SBC);
run;
title;
