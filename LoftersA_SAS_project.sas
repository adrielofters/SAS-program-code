/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 11:37:04 AM
PROJECT: LoftersA_SAS_project_011215
PROJECT PATH: P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA2 */
Libname MYDATA2 V9 'P:\QAC\qac200\students\alofters' ;


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

/*   START OF NODE: Assign Project Library (MYDATA2)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA2)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA2  "P:\QAC\qac200\students\alofters" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\alofters\Assignments";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:44 AM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\students\alofters\Assignments\meps_fullyr_2012_recodedsubset.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012_);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012_recodedsubset OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012_(LABEL="Contents Details for meps_fullyr_2012_recodedsubset");
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
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012_
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012_RECODEDSUBSET';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012_ OUT=WORK.CONTContentsFormeps_fullyr_2012_;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012_
		WHERE memname='MEPS_FULLYR_2012_RECODEDSUBSET';
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


/*   START OF NODE: Reverse coding 4 of SF-12V2   */
LIBNAME EC100011 "P:\QAC\qac200\students\alofters\Assignments";


%LET _CLIENTTASKLABEL='Reverse coding 4 of SF-12V2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012_RECOD);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012_RECOD AS 
   SELECT t1.ADAPPT42, 
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
          t1.DUPERSID, 
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
          /* ADGENH42r_rc */
            (6-t1.ADGENH42r) LABEL="SAQ: Health in general SF-12V2" AS ADGENH42r_rc, 
          /* ADPAIN42r_rc */
            (6-t1.ADPAIN42r) LABEL="SAQ 4WKS: Pain limits normal work SF-12V2" AS ADPAIN42r_rc, 
          /* ADCAPE42r_rc */
            (6-t1.ADCAPE42r) LABEL="SAW 4WKS: Felt calm/peaceful SF-12V2" AS ADCAPE42r_rc, 
          /* ADNRGY42r_rc */
            (6-t1.ADNRGY42r) LABEL="SAQ 4WKS: Had a lot of energy SF-12V2" AS ADNRGY42r_rc, 
          t1.EDRECODEr
      FROM EC100011.meps_fullyr_2012_recodedsubset t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:45 AM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_RECOD
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_RECOD
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADCAPE42r, T.ADCAPE42r_rc, T.ADGENH42r, T.ADGENH42r_rc, T.ADNRGY42r, T.ADPAIN42r, T.ADPAIN42r_rc, T.ADNRGY42r_rc
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_RECOD as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADCAPE42r /  SCORES=TABLE;
	TABLES ADCAPE42r_rc /  SCORES=TABLE;
	TABLES ADGENH42r /  SCORES=TABLE;
	TABLES ADGENH42r_rc /  SCORES=TABLE;
	TABLES ADNRGY42r /  SCORES=TABLE;
	TABLES ADPAIN42r /  SCORES=TABLE;
	TABLES ADPAIN42r_rc /  SCORES=TABLE;
	TABLES ADNRGY42r_rc /  SCORES=TABLE;
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


/*   START OF NODE: Aggregate scores SF-12V2 SAQs   */
%LET _CLIENTTASKLABEL='Aggregate scores SF-12V2 SAQs';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012__0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012__0001 AS 
   SELECT t1.ADAPPT42, 
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
          t1.DUPERSID, 
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
          /* sum_SF-12V2 */
            
            (SUM(t1.ADGENH42r_rc,t1.ADDAYA42r,t1.ADCLIM42r,t1.ADPALS42r,t1.ADPWLM42r,t1.ADMALS42r,t1.ADMWLM42r,t1.ADPAIN42r_rc,t1.ADCAPE42r_rc,t1.ADNRGY42r_rc,t1.ADDOWN42r,t1.ADSOCA42r)) 
            LABEL="Aggregate Health score of (r & rc) SF-12V2 SAQs" AS 'sum_SF-12V2'n, 
          t1.EDRECODEr
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_RECOD t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:45 AM
   By task: List Data

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T."sum_SF-12V2"n, T.ADGENH42r_rc, T.ADDAYA42r, T.ADCLIM42r, T.ADPALS42r, T.ADPWLM42r, T.ADMALS42r, T.ADMWLM42r, T.ADPAIN42r_rc, T.ADCAPE42r_rc, T.ADNRGY42r_rc, T.ADDOWN42r, T.ADSOCA42r
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__0001 as T
;
QUIT;
TITLE;
TITLE1 "Report Listing for Aggregate Health Score for SF-12V2 SAQs";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR "sum_SF-12V2"n ADGENH42r_rc ADDAYA42r ADCLIM42r ADPALS42r ADPWLM42r ADMALS42r ADMWLM42r ADPAIN42r_rc ADCAPE42r_rc ADNRGY42r_rc ADDOWN42r ADSOCA42r;
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


/*   START OF NODE: Summary Statistics for Aggregate Overall Health Score MEPS 2012 ages 18+   */
%LET _CLIENTTASKLABEL='Summary Statistics for Aggregate Overall Health Score MEPS 2012 ages 18+';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:45 AM
   By task: Summary Statistics for Aggregate Overall Health Score MEPS 2012 ages 18+

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted,
		MYDATA2.MEANSUMMARYSTATS);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T."sum_SF-12V2"n
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__0001(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for MEPS 2012 Adult 18+ AggregateOverall Health Score (SF-12V2)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR "sum_SF-12V2"n;

OUTPUT 	OUT=MYDATA2.MEANSUMMARYSTATS(LABEL="Summary Statistics for WORK.QUERY_FOR_MEPS_FULLYR_2012__0001")
	
		MEAN()= 
		STD()= 
		MIN()= 
		MAX()= 
		MODE()= 
		N()=	
		Q1()= 
		MEDIAN()= 
		Q3()=

	/ AUTONAME AUTOLABEL INHERIT
	;
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


/*   START OF NODE: Distribution Analysis for Aggregate Overall Health Scores SF-12V2 MEPS 2012 ages 18+   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate Overall Health Scores SF-12V2 MEPS 2012 ages 18+';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:46 AM
   By task: Distribution Analysis for Aggregate Overall Health Scores SF-12V2 MEPS 2012 ages 18+

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012__0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T."sum_SF-12V2"n
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__0001(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of Overall Agreggate Health Scores SF-12V2 MEPS 2012 ages 18+";
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
	VAR "sum_SF-12V2"n;
	HISTOGRAM   "sum_SF-12V2"n / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorize Aggregate Overall Health Scores SF-12V2   */
%LET _CLIENTTASKLABEL='Categorize Aggregate Overall Health Scores SF-12V2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012 AS 
   SELECT /* sum_SF-12V2_categorical */
            (CASE  
               WHEN t1.'sum_SF-12V2'n < 41 and t1.'sum_SF-12V2'n >=0
               THEN 1
            WHEN t1.'sum_SF-12V2'n >= 41 and  t1.'sum_SF-12V2'n <= 47.9
               THEN 2
            WHEN t1.'sum_SF-12V2'n >=48 and t1.'sum_SF-12V2'n <= 51.9
               THEN 3
               ELSE 4
            END) LABEL="By quartile" AS 'sum_SF-12V2_categorical'n, 
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
          t1.DUPERSID, 
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
          t1.EDRECODEr
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012__0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:46 AM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T."sum_SF-12V2_categorical"n, T."sum_SF-12V2"n
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES "sum_SF-12V2_categorical"n /  SCORES=TABLE;
	TABLES "sum_SF-12V2"n /  SCORES=TABLE;
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


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:47 AM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T."sum_SF-12V2"n, T."sum_SF-12V2_categorical"n
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES "sum_SF-12V2_categorical"n * "sum_SF-12V2"n /
		NOROW
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


/*   START OF NODE: Recoded for sources of self care info   */
%LET _CLIENTTASKLABEL='Recoded for sources of self care info';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR AS 
   SELECT /* DSCPCP53recodedtwice */
            (CASE 
               WHEN 2 = t1.DSCPCP53r THEN 0
               ELSE t1.DSCPCP53r
            END) LABEL="Learned care from primary care provider" AS DSCPCP53recodedtwice, 
          /* DSCNPC53r */
            (CASE 
               WHEN -1 = t1.DSCNPC53 THEN .
               WHEN 2 = t1.DSCNPC53 THEN 0
               WHEN -9 = t1.DSCNPC53 THEN .
               ELSE t1.DSCNPC53
            END) LABEL="Learned care from other provider" AS DSCNPC53r, 
          /* DSCPHN53r */
            (CASE 
               WHEN -1 = t1.DSCPHN53 THEN .
               WHEN 2 = t1.DSCPHN53 THEN 0
               WHEN -9 = t1.DSCPHN53 THEN .
               ELSE t1.DSCPHN53
            END) LABEL="Learned care from phone call with provider" AS DSCPHN53r, 
          /* DSCINT53r */
            (CASE 
               WHEN -1 = t1.DSCINT53 THEN .
               WHEN 2 = t1.DSCINT53 THEN 0
               WHEN -9 = t1.DSCINT53 THEN .
               ELSE t1.DSCINT53
            END) LABEL="Learned care from reading internet" AS DSCINT53r, 
          /* DSCGRP53r */
            (CASE 
               WHEN -1 = t1.DSCGRP53 THEN .
               WHEN 2 = t1.DSCGRP53 THEN 0
               WHEN -9 = t1.DSCGRP53 THEN .
               ELSE t1.DSCGRP53
            END) LABEL="Learned care by taking group class" AS DSCGRP53r, 
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
          t1.DUPERSID, 
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
          t1.EDRECODEr
      FROM WORK.QUERY_FOR_MEPS_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:47 AM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.DSCPCP53r, T.DSCPCP53recodedtwice, T.DSCGRP53, T.DSCGRP53r, T.DSCINT53, T.DSCINT53r, T.DSCNPC53, T.DSCNPC53r, T.DSCPHN53, T.DSCPHN53r
	FROM WORK.QUERY_FOR_MEPS_FULLYR as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results of Recoded Sources of Care Info Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES DSCPCP53r /  SCORES=TABLE;
	TABLES DSCPCP53recodedtwice /  SCORES=TABLE;
	TABLES DSCGRP53 /  SCORES=TABLE;
	TABLES DSCGRP53r /  SCORES=TABLE;
	TABLES DSCINT53 /  SCORES=TABLE;
	TABLES DSCINT53r /  SCORES=TABLE;
	TABLES DSCNPC53 /  SCORES=TABLE;
	TABLES DSCNPC53r /  SCORES=TABLE;
	TABLES DSCPHN53 /  SCORES=TABLE;
	TABLES DSCPHN53r /  SCORES=TABLE;
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


/*   START OF NODE: Aggregate score sources of care info   */
%LET _CLIENTTASKLABEL='Aggregate score sources of care info';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_0003);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_0003 AS 
   SELECT /* sum_additionalcare */
            (SUM(t1.DSCPCP53recodedtwice,t1.DSCNPC53r,t1.DSCPHN53r,t1.DSCINT53r,t1.DSCGRP53r)) LABEL=
            "Aggregate score of sources of additional care info" AS sum_additionalcare, 
          t1.EDRECODEr, 
          t1.DSCPCP53recodedtwice, 
          t1.DSCNPC53r, 
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
          t1.DUPERSID, 
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
          t1.'sum_SF-12V2'n
      FROM WORK.QUERY_FOR_MEPS_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data of Aggregate Scores Sources of Care Info   */
%LET _CLIENTTASKLABEL='List Data of Aggregate Scores Sources of Care Info';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:47 AM
   By task: List Data of Aggregate Scores Sources of Care Info

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.sum_additionalcare, T.DSCPCP53recodedtwice, T.DSCNPC53r, T.DSCPHN53r, T.DSCINT53r, T.DSCGRP53r
	FROM WORK.QUERY_FOR_MEPS_FULLYR_0003(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing of Aggregate Scores Sources of Care Info";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR sum_additionalcare DSCPCP53recodedtwice DSCNPC53r DSCPHN53r DSCINT53r DSCGRP53r;
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


/*   START OF NODE: Aggregate Score of Sources of Care Info    */
%LET _CLIENTTASKLABEL='Aggregate Score of Sources of Care Info ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:48 AM
   By task: Aggregate Score of Sources of Care Info 

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.sum_additionalcare
	FROM WORK.QUERY_FOR_MEPS_FULLYR_0003(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results of Aggregate Score Sources of Care Info Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR sum_additionalcare;

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


/*   START OF NODE: Aggregate Scores Sources of Care Info   */
%LET _CLIENTTASKLABEL='Aggregate Scores Sources of Care Info';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:48 AM
   By task: Aggregate Scores Sources of Care Info

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.sum_additionalcare
	FROM WORK.QUERY_FOR_MEPS_FULLYR_0003(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis ofAggregate Scores Sources of Care Info";
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
	FREQ
;
	VAR sum_additionalcare;
	HISTOGRAM   sum_additionalcare / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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


/*   START OF NODE: Categorizing sum_additionalcare   */
%LET _CLIENTTASKLABEL='Categorizing sum_additionalcare';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_0007);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_0007 AS 
   SELECT t1.sum_additionalcare, 
          t1.EDRECODEr, 
          t1.DSCPCP53recodedtwice, 
          t1.DSCNPC53r, 
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
          t1.DUPERSID, 
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
          /* sum_additionalcare_categorical */
            (CASE  
               WHEN t1.sum_additionalcare = 0 or t1.sum_additionalcare = 1
               THEN 1
            WHEN t1.sum_additionalcare = 2 
               THEN 2
            WHEN t1.sum_additionalcare = 3
               THEN 3
            WHEN t1.sum_additionalcare = 4 or t1.sum_additionalcare = 5
               THEN 4
            
            END) LABEL="Sources of additional care info" AS sum_additionalcare_categorical
      FROM WORK.QUERY_FOR_MEPS_FULLYR_0003 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis of sum_additionalcare_categorical   */
%LET _CLIENTTASKLABEL='Table Analysis of sum_additionalcare_categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:48 AM
   By task: Table Analysis of sum_additionalcare_categorical

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_0007
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_0007
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.sum_additionalcare_categorical, T.sum_additionalcare
	FROM WORK.QUERY_FOR_MEPS_FULLYR_0007(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis sum_additionalcare_categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES sum_additionalcare_categorical * sum_additionalcare /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
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


/*   START OF NODE: Summary Statistics of MARRY12Xr and EDRECODEr   */
%LET _CLIENTTASKLABEL='Summary Statistics of MARRY12Xr and EDRECODEr';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:49 AM
   By task: Summary Statistics of MARRY12Xr and EDRECODEr

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12Xr, T.EDRECODEr
	FROM WORK.QUERY_FOR_MEPS_FULLYR_0003(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results of MARRY12Xr";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR MARRY12Xr EDRECODEr;

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


/*   START OF NODE: Categorizing MARRY12Xr and EDRECODEr   */
%LET _CLIENTTASKLABEL='Categorizing MARRY12Xr and EDRECODEr';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_0006);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_0006 AS 
   SELECT /* MARRY12Xr_categorical */
            (CASE  
               WHEN t1.MARRY12Xr = 1
               THEN 1
            WHEN t1.MARRY12Xr = 2 or  t1.MARRY12Xr = 3 or  t1.MARRY12Xr = 4
               THEN 2
               ELSE 3
            END) LABEL="Marital status " AS MARRY12Xr_categorical, 
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
          t1.DUPERSID, 
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
          /* EDRECODEr_categorical */
            (CASE  
               WHEN t1.EDRECODEr >= 0 and t1.EDRECODEr <=12.9
               THEN 1
               WHEN t1.EDRECODEr = 13
               THEN 2
               ELSE 3
            END) LABEL="Education recode (edited)" AS EDRECODEr_categorical
      FROM WORK.QUERY_FOR_MEPS_FULLYR_0003 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis MARRY12Xr and EDRECODEr vs categorical   */
%LET _CLIENTTASKLABEL='Table Analysis MARRY12Xr and EDRECODEr vs categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:49 AM
   By task: Table Analysis MARRY12Xr and EDRECODEr vs categorical

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12Xr, T.MARRY12Xr_categorical, T.EDRECODEr_categorical, T.EDRECODEr
	FROM WORK.QUERY_FOR_MEPS_FULLYR_0006(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for MARRY12Xr and EDRECODEr by recoded categories";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12Xr_categorical * MARRY12Xr /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDRECODEr_categorical * EDRECODEr /
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


/*   START OF NODE: Distribution Analysis MARRY12Xr_categorical and EDRECODEr_categorical   */
%LET _CLIENTTASKLABEL='Distribution Analysis MARRY12Xr_categorical and EDRECODEr_categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011215.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011215.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 11:35:50 AM
   By task: Distribution Analysis MARRY12Xr_categorical and EDRECODEr_categorical

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12Xr_categorical, T.EDRECODEr_categorical
	FROM WORK.QUERY_FOR_MEPS_FULLYR_0006(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: MARRY12Xr_categorical and EDRECODEr_categorical";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR MARRY12Xr_categorical EDRECODEr_categorical;
	HISTOGRAM   MARRY12Xr_categorical EDRECODEr_categorical / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
