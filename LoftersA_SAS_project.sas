/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 9:48:29 AM
PROJECT: LoftersA_SAS_project_011315
PROJECT PATH: P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\alofters\Assignments' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\alofters\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA BASE "P:\QAC\qac200\students\alofters\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Adding INER   */
LIBNAME EC100004 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Adding INER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.INER_APPEND);

PROC SQL;
   CREATE TABLE MYDATA.INER_APPEND(label="INER_APPEND") AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* INER */
            (1) AS INER
      FROM EC100004.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: join_fullyr_and_er   */
%LET _CLIENTTASKLABEL='join_fullyr_and_er';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.join_fullyr_er);

PROC SQL;
   CREATE TABLE MYDATA.join_fullyr_er(label="join_fullyr_er") AS 
   SELECT t2.DUID, 
          t1.MARRY12Xr_categorical, 
          t1.sum_additionalcare, 
          t1.DSCPCP53recodedtwice, 
          t1.DSCNPC53r, 
          t1.EDRECODEr, 
          t1.DSCPHN53r, 
          t1.DSCINT53r, 
          t1.DSCGRP53r, 
          t1.'sum_SF-12V2_categorical'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.BLIND42, 
          t1.BMINDX53, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DEAF42, 
          t1.DUPERSID AS DUPERSID1, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSCGRP53, 
          t1.DSCINT53, 
          t1.DSCNPC53, 
          t1.DSCPCP53, 
          t1.DSCPHN53, 
          t1.DSDIA53, 
          t1.DSFLNV53, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCARE12, 
          t1.MIDX, 
          t1.WAGEP12X, 
          t1.UNINS12, 
          t1.STRKDX, 
          t1.SSIP12X, 
          t1.SFFLAG42, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.PROVTY42, 
          t1.ADGENH42r, 
          t1.ADDAYA42r, 
          t1.ADPALS42r, 
          t1.ADPWLM42r, 
          t1.ADMALS42r, 
          t1.ADMWLM42r, 
          t1.ADPAIN42r, 
          t1.ADCAPE42r, 
          t1.ADNRGY42r, 
          t1.ADDOWN42r, 
          t1.ADSOCA42r, 
          t1.MARRY12Xr, 
          t1.CANCERDXr, 
          t1.CHDDXr, 
          t1.FOODST12r, 
          t1.STRKDXr, 
          t1.ADHECR42r, 
          t1.ADSMOK42r, 
          t1.ADSPEC42r, 
          t1.ADNDCR42r, 
          t1.ADILCR42r, 
          t1.ADEGMC42r, 
          t1.ADINTR42r, 
          t1.ADLIST42r, 
          t1.ADILWW42r, 
          t1.ADEXPL42r, 
          t1.ADLANG42r, 
          t1.BLIND42r, 
          t1.ADCLIM42r, 
          t1.ADRTCR42r, 
          t1.MIDXr, 
          t1.DSCPCP53r, 
          t1.HIBPDXr, 
          t1.ADGENH42r_rc, 
          t1.ADPAIN42r_rc, 
          t1.ADCAPE42r_rc, 
          t1.ADNRGY42r_rc, 
          t1.'sum_SF-12V2'n, 
          t1.EDRECODEr_categorical, 
          t1.INFULLYR, 
          t2.PID, 
          t2.DUPERSID, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER
      FROM MYDATA.INFULLYR_APPEND t1
           FULL JOIN MYDATA.INER_APPEND t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: filter infullyr=1 and iner=1   */
%LET _CLIENTTASKLABEL='filter infullyr=1 and iner=1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.FILTER_FOR_JOIN_FULLYR_ER);

PROC SQL;
   CREATE TABLE WORK.FILTER_FOR_JOIN_FULLYR_ER AS 
   SELECT t1.DUID, 
          t1.MARRY12Xr_categorical, 
          t1.sum_additionalcare, 
          t1.DSCPCP53recodedtwice, 
          t1.DSCNPC53r, 
          t1.EDRECODEr, 
          t1.DSCPHN53r, 
          t1.DSCINT53r, 
          t1.DSCGRP53r, 
          t1.'sum_SF-12V2_categorical'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.BLIND42, 
          t1.BMINDX53, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DEAF42, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSCGRP53, 
          t1.DSCINT53, 
          t1.DSCNPC53, 
          t1.DSCPCP53, 
          t1.DSCPHN53, 
          t1.DSDIA53, 
          t1.DSFLNV53, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCARE12, 
          t1.MIDX, 
          t1.WAGEP12X, 
          t1.UNINS12, 
          t1.STRKDX, 
          t1.SSIP12X, 
          t1.SFFLAG42, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.PROVTY42, 
          t1.ADGENH42r, 
          t1.ADDAYA42r, 
          t1.ADPALS42r, 
          t1.ADPWLM42r, 
          t1.ADMALS42r, 
          t1.ADMWLM42r, 
          t1.ADPAIN42r, 
          t1.ADCAPE42r, 
          t1.ADNRGY42r, 
          t1.ADDOWN42r, 
          t1.ADSOCA42r, 
          t1.MARRY12Xr, 
          t1.CANCERDXr, 
          t1.CHDDXr, 
          t1.FOODST12r, 
          t1.STRKDXr, 
          t1.ADHECR42r, 
          t1.ADSMOK42r, 
          t1.ADSPEC42r, 
          t1.ADNDCR42r, 
          t1.ADILCR42r, 
          t1.ADEGMC42r, 
          t1.ADINTR42r, 
          t1.ADLIST42r, 
          t1.ADILWW42r, 
          t1.ADEXPL42r, 
          t1.ADLANG42r, 
          t1.BLIND42r, 
          t1.ADCLIM42r, 
          t1.ADRTCR42r, 
          t1.MIDXr, 
          t1.DSCPCP53r, 
          t1.HIBPDXr, 
          t1.ADGENH42r_rc, 
          t1.ADPAIN42r_rc, 
          t1.ADCAPE42r_rc, 
          t1.ADNRGY42r_rc, 
          t1.'sum_SF-12V2'n, 
          t1.EDRECODEr_categorical, 
          t1.INFULLYR, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER
      FROM MYDATA.JOIN_FULLYR_ER t1
      WHERE t1.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List of Merged Data   */
%LET _CLIENTTASKLABEL='List of Merged Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:48:12 AM
   By task: List of Merged Data

   Input Data: Local:WORK.FILTER_FOR_JOIN_FULLYR_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.FILTER_FOR_JOIN_FULLYR_ER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID1, T.DUPERSID
	FROM WORK.FILTER_FOR_JOIN_FULLYR_ER(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing for Merged Data";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID1 DUPERSID;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:48:12 AM
   By task: Data Set Attributes

   Input Data: Local:WORK.FILTER_FOR_JOIN_FULLYR_ER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForFILTER_FOR_JOIN_F);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=WORK.FILTER_FOR_JOIN_FULLYR_ER OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForFILTER_FOR_JOIN_F(LABEL="Contents Details for FILTER_FOR_JOIN_FULLYR_ER");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForFILTER_FOR_JOIN_F
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='FILTER_FOR_JOIN_FULLYR_ER';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForFILTER_FOR_JOIN_F OUT=WORK.CONTContentsForFILTER_FOR_JOIN_F;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForFILTER_FOR_JOIN_F
		WHERE memname='FILTER_FOR_JOIN_FULLYR_ER';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode MRI and XRAYS   */
%LET _CLIENTTASKLABEL='Recode MRI and XRAYS';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.RECODE_MRI_XRAYS);

PROC SQL;
   CREATE TABLE MYDATA.RECODE_MRI_XRAYS(label="RECODE_MRI_XRAYS") AS 
   SELECT t1.DUID, 
          t1.MARRY12Xr_categorical, 
          t1.sum_additionalcare, 
          t1.DSCPCP53recodedtwice, 
          t1.DSCNPC53r, 
          t1.EDRECODEr, 
          t1.DSCPHN53r, 
          t1.DSCINT53r, 
          t1.DSCGRP53r, 
          t1.'sum_SF-12V2_categorical'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.BLIND42, 
          t1.BMINDX53, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DEAF42, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSCGRP53, 
          t1.DSCINT53, 
          t1.DSCNPC53, 
          t1.DSCPCP53, 
          t1.DSCPHN53, 
          t1.DSDIA53, 
          t1.DSFLNV53, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCARE12, 
          t1.MIDX, 
          t1.WAGEP12X, 
          t1.UNINS12, 
          t1.STRKDX, 
          t1.SSIP12X, 
          t1.SFFLAG42, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.PROVTY42, 
          t1.ADGENH42r, 
          t1.ADDAYA42r, 
          t1.ADPALS42r, 
          t1.ADPWLM42r, 
          t1.ADMALS42r, 
          t1.ADMWLM42r, 
          t1.ADPAIN42r, 
          t1.ADCAPE42r, 
          t1.ADNRGY42r, 
          t1.ADDOWN42r, 
          t1.ADSOCA42r, 
          t1.MARRY12Xr, 
          t1.CANCERDXr, 
          t1.CHDDXr, 
          t1.FOODST12r, 
          t1.STRKDXr, 
          t1.ADHECR42r, 
          t1.ADSMOK42r, 
          t1.ADSPEC42r, 
          t1.ADNDCR42r, 
          t1.ADILCR42r, 
          t1.ADEGMC42r, 
          t1.ADINTR42r, 
          t1.ADLIST42r, 
          t1.ADILWW42r, 
          t1.ADEXPL42r, 
          t1.ADLANG42r, 
          t1.BLIND42r, 
          t1.ADCLIM42r, 
          t1.ADRTCR42r, 
          t1.MIDXr, 
          t1.DSCPCP53r, 
          t1.HIBPDXr, 
          t1.ADGENH42r_rc, 
          t1.ADPAIN42r_rc, 
          t1.ADCAPE42r_rc, 
          t1.ADNRGY42r_rc, 
          t1.'sum_SF-12V2'n, 
          t1.EDRECODEr_categorical, 
          t1.INFULLYR, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          /* MRIr */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="This visit did P have an MRI/CATSCAN" AS MRIr, 
          /* XRAYSr */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
               ELSE t1.XRAYS
            END) LABEL="This visit did P have x-rays" AS XRAYSr
      FROM WORK.FILTER_FOR_JOIN_FULLYR_ER t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies MRIr XRAYSr   */
%LET _CLIENTTASKLABEL='One-Way Frequencies MRIr XRAYSr';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:48:13 AM
   By task: One-Way Frequencies MRIr XRAYSr

   Input Data: Local:MYDATA.RECODE_MRI_XRAYS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.RECODE_MRI_XRAYS
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYSr, T.MRIr
	FROM MYDATA.RECODE_MRI_XRAYS(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results of Recoded MRIr and XRAYSr";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAYSr /  SCORES=TABLE;
	TABLES MRIr /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: DUPERSID count Query Builder   */
%LET _CLIENTTASKLABEL='DUPERSID count Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.DUPERSID_count);

PROC SQL;
   CREATE TABLE MYDATA.DUPERSID_count(label="DUPERSID_count") AS 
   SELECT t1.DUPERSID, 
          /* ERvisits */
            (COUNT(t1.DUPERSID)) LABEL="Number of ER visits by DUPERSID" AS ERvisits
      FROM MYDATA.RECODE_MRI_XRAYS t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Joining to rest of data   */
%LET _CLIENTTASKLABEL='Joining to rest of data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.JOIN_COUNT_TO_REST);

PROC SQL;
   CREATE TABLE MYDATA.JOIN_COUNT_TO_REST(label="JOIN_COUNT_TO_REST") AS 
   SELECT t1.DUPERSID, 
          t1.ERvisits, 
          t2.DUID, 
          t2.MARRY12Xr_categorical, 
          t2.sum_additionalcare, 
          t2.DSCPCP53recodedtwice, 
          t2.DSCNPC53r, 
          t2.EDRECODEr, 
          t2.DSCPHN53r, 
          t2.DSCINT53r, 
          t2.DSCGRP53r, 
          t2.'sum_SF-12V2_categorical'n, 
          t2.ADAPPT42, 
          t2.ADCAPE42, 
          t2.ADCLIM42, 
          t2.ADCMPD42, 
          t2.ADCMPM42, 
          t2.ADCMPY42, 
          t2.ADDAYA42, 
          t2.ADDOWN42, 
          t2.ADDPRS42, 
          t2.ADDRBP42, 
          t2.ADEFRT42, 
          t2.ADEGMC42, 
          t2.ADEXPL42, 
          t2.ADEZUN42, 
          t2.ADFFRM42, 
          t2.ADFHLP42, 
          t2.ADGENH42, 
          t2.ADHECR42, 
          t2.ADHOPE42, 
          t2.ADILCR42, 
          t2.ADILWW42, 
          t2.ADINSA42, 
          t2.ADINSB42, 
          t2.ADINST42, 
          t2.ADINTR42, 
          t2.ADLANG42, 
          t2.ADLIST42, 
          t2.ADMALS42, 
          t2.ADMWLM42, 
          t2.ADNDCR42, 
          t2.ADNERV42, 
          t2.ADNRGY42, 
          t2.ADNSMK42, 
          t2.ADOVER42, 
          t2.ADPAIN42, 
          t2.ADPALS42, 
          t2.ADPRTM42, 
          t2.ADPRX42, 
          t2.ADPWLM42, 
          t2.ADRESP42, 
          t2.ADREST42, 
          t2.ADRISK42, 
          t2.ADRTCR42, 
          t2.ADRTWW42, 
          t2.ADSAD42, 
          t2.ADSMOK42, 
          t2.ADSOCA42, 
          t2.ADSPEC42, 
          t2.ADSPRF42, 
          t2.ADTLHW42, 
          t2.ADWRTH42, 
          t2.AGE12X, 
          t2.AMAEXP12, 
          t2.AMCEXP12, 
          t2.AMCHIR12, 
          t2.AMDRC12, 
          t2.AMEEXP12, 
          t2.AMNEXP12, 
          t2.AMNURS12, 
          t2.AMOPTO12, 
          t2.AMTEXP12, 
          t2.AMTHER12, 
          t2.AMTOTC12, 
          t2.ARTHDX, 
          t2.ASTHDX, 
          t2.BLIND42, 
          t2.BMINDX53, 
          t2.BPMLDX, 
          t2.BUSNP12X, 
          t2.CANCERDX, 
          t2.CHDDX, 
          t2.CHOLDX, 
          t2.DEAF42, 
          t2.DUPERSID1, 
          t2.SEX, 
          t2.RACETHX, 
          t2.MARRY12X, 
          t2.DIABDX, 
          t2.DIVDP12X, 
          t2.DNTINS12, 
          t2.DNUNAB42, 
          t2.DSCGRP53, 
          t2.DSCINT53, 
          t2.DSCNPC53, 
          t2.DSCPCP53, 
          t2.DSCPHN53, 
          t2.DSDIA53, 
          t2.DSFLNV53, 
          t2.EDUYRDEG, 
          t2.EMPHDX, 
          t2.ERTOT12, 
          t2.FAMINC12, 
          t2.FOODST12, 
          t2.HIBPDX, 
          t2.INS12X, 
          t2.MCARE12, 
          t2.MIDX, 
          t2.WAGEP12X, 
          t2.UNINS12, 
          t2.STRKDX, 
          t2.SSIP12X, 
          t2.SFFLAG42, 
          t2.SAQELIG, 
          t2.SAQWT12F, 
          t2.PROVTY42, 
          t2.ADGENH42r, 
          t2.ADDAYA42r, 
          t2.ADPALS42r, 
          t2.ADPWLM42r, 
          t2.ADMALS42r, 
          t2.ADMWLM42r, 
          t2.ADPAIN42r, 
          t2.ADCAPE42r, 
          t2.ADNRGY42r, 
          t2.ADDOWN42r, 
          t2.ADSOCA42r, 
          t2.MARRY12Xr, 
          t2.CANCERDXr, 
          t2.CHDDXr, 
          t2.FOODST12r, 
          t2.STRKDXr, 
          t2.ADHECR42r, 
          t2.ADSMOK42r, 
          t2.ADSPEC42r, 
          t2.ADNDCR42r, 
          t2.ADILCR42r, 
          t2.ADEGMC42r, 
          t2.ADINTR42r, 
          t2.ADLIST42r, 
          t2.ADILWW42r, 
          t2.ADEXPL42r, 
          t2.ADLANG42r, 
          t2.BLIND42r, 
          t2.ADCLIM42r, 
          t2.ADRTCR42r, 
          t2.MIDXr, 
          t2.DSCPCP53r, 
          t2.HIBPDXr, 
          t2.ADGENH42r_rc, 
          t2.ADPAIN42r_rc, 
          t2.ADCAPE42r_rc, 
          t2.ADNRGY42r_rc, 
          t2.'sum_SF-12V2'n, 
          t2.EDRECODEr_categorical, 
          t2.INFULLYR, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID2, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER, 
          t2.MRIr, 
          t2.XRAYSr
      FROM MYDATA.DUPERSID_COUNT t1
           INNER JOIN MYDATA.RECODE_MRI_XRAYS t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies of ERvisits   */
%LET _CLIENTTASKLABEL='One-Way Frequencies of ERvisits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:48:13 AM
   By task: One-Way Frequencies of ERvisits

   Input Data: Local:MYDATA.JOIN_COUNT_TO_REST
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.JOIN_COUNT_TO_REST
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ERvisits
	FROM MYDATA.JOIN_COUNT_TO_REST(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies of ERvisits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ERvisits /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of ERvisits   */
%LET _CLIENTTASKLABEL='Distribution Analysis of ERvisits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:48:14 AM
   By task: Distribution Analysis of ERvisits

   Input Data: Local:MYDATA.JOIN_COUNT_TO_REST
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.JOIN_COUNT_TO_REST
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ERvisits
	FROM MYDATA.JOIN_COUNT_TO_REST(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: ERvisits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR ERvisits;
	HISTOGRAM   ERvisits / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorizing ERvisits   */
%LET _CLIENTTASKLABEL='Categorizing ERvisits';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.Categorizing_ervisits);

PROC SQL;
   CREATE TABLE MYDATA.Categorizing_ervisits(label="Categorizing_ervisits") AS 
   SELECT t1.DUPERSID, 
          t1.ERvisits, 
          t1.DUID, 
          t1.MARRY12Xr_categorical, 
          t1.sum_additionalcare, 
          t1.DSCPCP53recodedtwice, 
          t1.DSCNPC53r, 
          t1.EDRECODEr, 
          t1.DSCPHN53r, 
          t1.DSCINT53r, 
          t1.DSCGRP53r, 
          t1.'sum_SF-12V2_categorical'n, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.AGE12X, 
          t1.AMAEXP12, 
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMDRC12, 
          t1.AMEEXP12, 
          t1.AMNEXP12, 
          t1.AMNURS12, 
          t1.AMOPTO12, 
          t1.AMTEXP12, 
          t1.AMTHER12, 
          t1.AMTOTC12, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.BLIND42, 
          t1.BMINDX53, 
          t1.BPMLDX, 
          t1.BUSNP12X, 
          t1.CANCERDX, 
          t1.CHDDX, 
          t1.CHOLDX, 
          t1.DEAF42, 
          t1.DUPERSID1, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.DIABDX, 
          t1.DIVDP12X, 
          t1.DNTINS12, 
          t1.DNUNAB42, 
          t1.DSCGRP53, 
          t1.DSCINT53, 
          t1.DSCNPC53, 
          t1.DSCPCP53, 
          t1.DSCPHN53, 
          t1.DSDIA53, 
          t1.DSFLNV53, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MCARE12, 
          t1.MIDX, 
          t1.WAGEP12X, 
          t1.UNINS12, 
          t1.STRKDX, 
          t1.SSIP12X, 
          t1.SFFLAG42, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.PROVTY42, 
          t1.ADGENH42r, 
          t1.ADDAYA42r, 
          t1.ADPALS42r, 
          t1.ADPWLM42r, 
          t1.ADMALS42r, 
          t1.ADMWLM42r, 
          t1.ADPAIN42r, 
          t1.ADCAPE42r, 
          t1.ADNRGY42r, 
          t1.ADDOWN42r, 
          t1.ADSOCA42r, 
          t1.MARRY12Xr, 
          t1.CANCERDXr, 
          t1.CHDDXr, 
          t1.FOODST12r, 
          t1.STRKDXr, 
          t1.ADHECR42r, 
          t1.ADSMOK42r, 
          t1.ADSPEC42r, 
          t1.ADNDCR42r, 
          t1.ADILCR42r, 
          t1.ADEGMC42r, 
          t1.ADINTR42r, 
          t1.ADLIST42r, 
          t1.ADILWW42r, 
          t1.ADEXPL42r, 
          t1.ADLANG42r, 
          t1.BLIND42r, 
          t1.ADCLIM42r, 
          t1.ADRTCR42r, 
          t1.MIDXr, 
          t1.DSCPCP53r, 
          t1.HIBPDXr, 
          t1.ADGENH42r_rc, 
          t1.ADPAIN42r_rc, 
          t1.ADCAPE42r_rc, 
          t1.ADNRGY42r_rc, 
          t1.'sum_SF-12V2'n, 
          t1.EDRECODEr_categorical, 
          t1.INFULLYR, 
          t1.PID, 
          t1.DUPERSID2, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
          t1.MRIr, 
          t1.XRAYSr, 
          /* ERvisits_categorical */
            (CASE  
               WHEN t1.ERvisits > 2
               THEN 3
               ELSE t1.ERvisits
            END) LABEL="Visits to the ER (in categories)" AS ERvisits_categorical
      FROM MYDATA.JOIN_COUNT_TO_REST t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ERvisits vs ERvisits_categorical   */
%LET _CLIENTTASKLABEL='ERvisits vs ERvisits_categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:48:14 AM
   By task: ERvisits vs ERvisits_categorical

   Input Data: Local:MYDATA.CATEGORIZING_ERVISITS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.CATEGORIZING_ERVISITS
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ERvisits_categorical, T.ERvisits
	FROM MYDATA.CATEGORIZING_ERVISITS(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results of ERvisits vs ERvisits_categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ERvisits_categorical * ERvisits /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies ERvisits_categorical   */
%LET _CLIENTTASKLABEL='One-Way Frequencies ERvisits_categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 9:48:14 AM
   By task: One-Way Frequencies ERvisits_categorical

   Input Data: Local:MYDATA.CATEGORIZING_ERVISITS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.CATEGORIZING_ERVISITS
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ERvisits_categorical
	FROM MYDATA.CATEGORIZING_ERVISITS(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results of ERvisits_categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ERvisits_categorical /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
