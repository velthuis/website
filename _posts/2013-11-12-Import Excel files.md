---
title: Import Excel files
date: 2013-11-12 00:00:00 -0500
categories: [Code, SAS]
tags: [sas, import, excel]  # TAG names should always be lowercase
---

This program is helpful to import a large set of similarly structured Excel files, for example to read a set of files containing Russell Index constituent data.

```SAS
/* Program to import set of Excel files obtained from Russell */
/* Each Excel file has the same format, but covers different years */
/* The file names indicate whether the data concerns the Russell 1000, 2000, or Micro index */
/* Variable names (column headers) are repeated throughout the file, and eliminated after importing *
/* Finally, CRSP permnos are matched to the index constituent data by ticker-date pairs */

/* Author:  Raisa Velthuis */
/* Created: November 22, 2013 */

* initialize;
%let path = C:\Users\Me\Documents\Russell Data\;
libname crsp 'D:\WRDS\2013'; /* location of crsp.msf table */

* import files;
%macro get_filenames(location);
/* From: http://stackoverflow.com/questions/1409543/using-sas-macro-to-pipe-a-list-of-filenames-from-
filename _dir_ "%bquote(&location.)";
data filenames(keep=memname);       
handle=dopen( '_dir_' );       
if handle > 0 then do;         
count=dnum(handle);         
do i=1 to count;           
memname=dread(handle,i);           
output filenames;
end;       
end;
rc=dclose(handle);
run;
filename _dir_ clear; 
%mend;

%get_filenames(&path);

%macro get_files(out=);
    /* Adapted from: http://www.nesug.org/Proceedings/nesug11/cc/cc17.pdf */

    /* Select index */
    data files; set filenames; where find(memname,"&out.")>0; i = _n_; run;

    /* Number of files */
    proc sql noprint; select count(memname) into :nfiles from files; quit;     %put Number of files in directory: &nfiles;

    /* Import files */
    %do i = 1 %to &nfiles;
        %* Get file name;
        proc sql noprint; select %trim(%left(memname)) into :file from files where i = &i; quit;
        %put File: &file;
        %put File: "&path.%trim(%left(&file.))";         
        filename fileloc "&path.%trim(%left(&file.))";         
        LIBNAME XLSLIB "&path.%trim(%left(&file.))";

        proc sql noprint;
        /***Get total Number of Sheets***/
            select count(distinct(MEMNAME)) into: tot
         from sashelp.vtable
         where LIBNAME ='XLSLIB' AND INDEX(MEMNAME,'General')=0;
        /**Get the sheet names in to macro variables***/          
         select distinct(MEMNAME) into: v1 - :v%trim (%left(&tot))
         from sashelp.vtable
         where LIBNAME ='XLSLIB' AND INDEX (MEMNAME,'General')=0;

        %* import data;
        PROC IMPORT OUT= WORK.Russell_&i
                    DATAFILE= fileloc
                    DBMS=EXCEL REPLACE;
         RANGE="&v1.";
         GETNAMES=YES;
         MIXED=NO;
         SCANTEXT=YES;
         USEDATE=YES;
         SCANTIME=YES;
        RUN;
    %end;

    %* combine files;     
    data &out;
        format date $10. ticker $8. desc $20. iwght best12.;
        set Russell_:;
        date2 = input(date,mmddyy10.); format date2 date9.;
        if not(missing(date2));
        drop date;         
        rename date2=date;     
    run;

    %* clean up;
    proc datasets nolist; delete files Russell_:; quit; 

%mend;

%get_files(out=R1);     /* Read in R1000 data  */
%get_files(out=R2);     /* Read in R2000 data  */
%get_files(out=RMicro); /* Read in RMicro data */

********* Add Permnos to Russell Membership data *********;
%macro add_permnos(out=);
    * add permnos based on ticker at specific date;
    proc sql; /* create single entry per ticker (sum weights of duplicate tickers) */
    create table &out._id as
    select distinct a.date, a.ticker, a.desc, sum(a.iwght) as iwght
    from &out. as a
    where a.ticker is not missing     group by a.date, a.ticker     order by a.date, a.ticker
    ;     
    quit;     
    proc sql;
    create table &out._id as
    select intnx('month',a.date,0,'e') as date format date9., a.*,
        b.permno, b.comnam, b.exchcd, b.shrcd, b.shrcls, abs(c.prc)*c.shrout as mcap
    from &out._id as a
    left join (select * from crsp.stocknames where ticker is not missing) as b     
    on a.ticker = b.ticker and a.date between b.namedt and b.nameenddt
    left join crsp.msf as c
    on b.permno = c.permno and intnx('month',a.date,0,'e') = intnx('month',c.date,0,'e')
    order by a.date, a.ticker, mcap desc
    ;     
    quit;
    data &out._id; * remove duplicate permno matches;
    set &out._id;
    where permno is not missing;     
    by date ticker descending mcap;
    if first.ticker;
    keep date permno iwght comnam;
    run; %mend;
%add_permnos(out=R1);
%add_permnos(out=R2);
%add_permnos(out=RMicro);

* crsp with russell identifiers;
proc sql;
    create table crsp_w_Russell
    as select intnx('month',a.date,0,'e') as date format date9., a.permno,
        case when b.R1000 = 1 then 1 else 0 end as R1000,
        case when missing(b.iwght) then 0 else b.iwght end as R1wght,
        case when c.R2000 = 1 then 1 else 0 end as R2000,
        case when missing(c.iwght) then 0 else c.iwght end as R2wght,
        case when d.RMicro = 1 then 1 else 0 end as RMicro,
        case when missing(d.iwght) then 0 else d.iwght end as RMwght
    from crsp.msf as a
    left join (select *, 1 as R1000 from R1_id) as b
    on a.permno=b.permno and intnx('month',a.date,0,'e') = b.date
    left join (select *, 1 as R2000 from R2_id) as c
    on a.permno=c.permno and intnx('month',a.date,0,'e') = c.date
    left join (select *, 1 as RMicro from RMicro_id) as d
    on a.permno=d.permno and intnx('month',a.date,0,'e') = d.date
    where a.date >= '01DEC1978'd     order by permno, date; quit;

**** some checks *****;
* summ stats;
proc means data=R1_id nway noprint;
class date; var iwght;
output out=nmembers1 sum=iwght_R1;
run;
proc means data=R2_id nway noprint;
class date; var iwght;
output out=nmembers2 sum=iwght_R2;
run;
proc means data=RMicro_id nway noprint;
class date; var iwght;
output out=nmembers3 sum=iwght_RM;
run; data nmembers;
merge nmembers1(drop=_type_ rename=(_freq_=nR1)) nmembers2(drop=_type_  rename=(_freq_=nR2))
     nmembers3(drop=_type_  rename=(_freq_=nRM));
by date; run;

*time series plot - number of funds; 
ods pdf file="&path.Russell N iwght.pdf" ; 
goptions reset=all; options orientation=landscape;
legend label=none value=(h=2 'Russell 1000' 'Russell 2000' 'Russell Micro') cborder=black;
axis1 label=(angle=90 'Sum of index weights') order= 0 to 1.1 by .1; 
axis2 label=(angle=90 'Number of stocks') order= 0 to 2200 by 200;
   proc gplot data=nmembers;
       symbol i=j v=none h=1 w=3;
       format date year4.;
    title "Total index weight of stocks in Russell index per month";
      plot ( iwght_R1 iwght_R2 iwght_RM) * date
        / overlay legend=legend1 vaxis=axis1
      ;
    run;
    title "Number of stocks in Russell index per month";
      plot ( nR1 nR2 nRM) * date
        / overlay legend=legend1 vaxis=axis2
      ;
    run;
    quit;
    quit;
ods pdf close;

* clean up; 
proc datasets nolist;
delete filenames Membersmcap Nmembers: R1 R1_id R2 R2_id RMicro RMicro_id;
quit;
```
