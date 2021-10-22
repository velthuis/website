---
title: 2SLS with double clustered standard errors
date: 2016-01-16 00:00:00 -0500
categories: [Code, SAS]
tags: [sas, clustered s.e.]     # TAG names should always be lowercase
---

```SAS
********** SAS code for 2SLS with double-clustered standard errors **********;
/* Author:  Raisa Velthuis */
/* Created: January 22, 2016 */

*** Make adjustments below as needed. For example, in my base case scenario, the model has one regressor and one instrument, and does not include intercepts ***; 

%let data=sample; *name of dataset;

* get White cov matrix;
proc model data=&data noprint;   
where Z ne .;

  parms b_1st b_2nd;
  Y = b_2nd*X;   X = b_1st*Z;

  fit Y X / 2sls outest=V2W covout hccme=0;
  instruments Z / noint;
run; 
quit;
data v2w; set v2w; where _name_ = "b_2nd"; run;

* add ID for double-clustered observations;
proc sort data=&data; by fund time; run;
proc sort data=&data out=cluster2 nodupkey; by fund time; run;
data cluster2;
set cluster2;
if _n_=1 then cluster2 = 0;
cluster2 +1; 
keep fund time cluster2;
run;
data &data; 
merge &data cluster2; 
by fund time; 
run;

* plain first stage regression w White s.e.;
proc reg data=&data plots=none; 
model X = Z /hcc HCCMETHOD=0 acov noint;
ods output acovest=both ;
run;
quit;
data both; set both; drop Variable  Dependent  Model; run;

* estimate RD w double-clustered s.e.;
proc iml;
* import data;
use &data;
read all var {Y} into y where (Z ^= .);
 read all var {Z} into Z where (Z ^= .); 
read all var {X} into X where (Z ^= .); 
read all var {time} into time where (Z ^= .); 
read all var {fund} into time where (Z ^= .); 
read all var {cluster2} into cluster2 where (Z ^= .);
close &data; 
use both; 
read all var {Z} into V1W;
close both; 
use V2W;
read all var {b_2nd} into V2W; 
close V2W;

* counters and degrees of freedom adjustements;
N=nrow(y); 
f=unique(time); 
t=unique(time); 
d=unique(cluster2); 
M1=ncol(unique(time));
M2=ncol(unique(time));
M3=ncol(unique(cluster2));
M=min(M1,M2);
K=ncol(X); 
qc1 = (N-1)/(N-K)*M1/(M1-1); 
qc2 = (N-1)/(N-K)*M2/(M2-1); 
qc3 = (N-1)/(N-K)*M3/(M3-1); 
qc  = (N-1)/(N-K)*M/(M-1);

*** first stage estimation ***; 
b1 = inv(Z`*Z)*Z`*X; 
e = X - Z*b1;
* cluster 1 (time);
SUM=0; 
do g = f[><] to f[<>];     
h = loc(f=g);     
if nrow(h)>0 then do;        
 h = loc(time=f[h]);         
eg = e[h];         
zg = Z[h,:];
SUM=SUM+zg`*eg*eg`*zg;
end; 
end;
V11 = inv(Z`*Z)*SUM*inv(Z`*Z)*qc1;
* cluster 2 (time);
SUM=0; 
do g = t[><] to t[<>];     
h = loc(t=g);     
if nrow(h)>0 then do;         
h = loc(time=t[h]);        
eg = e[h];         
zg = Z[h,:];
SUM=SUM+zg`*eg*eg`*zg;
end; 
end;
V12 = inv(Z`*Z)*SUM*inv(Z`*Z)*qc2;
* double clustered V;
V1 = (V11+V12-V1W);**qc; 
se1 = V1##.5;
/*print V11;print V12;print V1W; print V1; print se1; */

 *** second stage estimation ***; 
b_iv = inv(Z`*X)*Z`*y; 
u = y - X*b_iv;; 
* cluster 1 (time);
SUM=0; 
do g = f[><] to f[<>];     
h = loc(f=g);    
if nrow(h)>0 then do;        
h = loc(time=f[h]);         
ug = u[h];         
zg = Z[h,:];
SUM=SUM+zg`*ug*ug`*zg;
end; 
end;
V21 = inv(Z`*X)*SUM*inv(X`*Z)*qc1;
* cluster 2 (time);
SUM=0; 
do g = t[><] to t[<>];     
h = loc(t=g);     
if nrow(h)>0 then do;         
h = loc(time=t[h]);         
ug = u[h];         
zg = Z[h,:];
SUM=SUM+zg`*ug*ug`*zg;
end; 
end;
V22 = inv(Z`*X)*SUM*inv(X`*Z)*qc2;
* double clustered V;
Vclu = (V21+V22-V2W);**qc; 
seclu = Vclu##.5; 
/*print V21;print V22;print V2W; print Vclu; print seclu; */

* store results;
create results var {b1 se1 b_iv seclu N M M1 M2 M3 K};
append; 
close results; 
quit;

* output;
data results; 
set results;
* second stage t-stat and p-value;
tvalue=b_iv/seclu; 
probt=(1-probt(abs(tvalue),M-1))*2; 
* first stage t-stat and p-value;
tvalue1=b1/se1;
probt1=(1-probt(abs(tvalue1),M-1))*2;
run;
```
