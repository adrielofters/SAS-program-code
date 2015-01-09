/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 10:10:19 AM
PROJECT: LoftersA_SAS_project_010915
PROJECT PATH: P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\alofters\Assignments' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\alofters\Assignments' ;


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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\alofters\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:09:48 AM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MYDATA.CONTCONTENTSFORMEPS_FULLYR_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA MYDATA.CONTCONTENTSFORMEPS_FULLYR_2012(LABEL="Contents Details for meps_fullyr_2012");
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
			typemem LABEL="Data Set Type" FROM MYDATA.CONTCONTENTSFORMEPS_FULLYR_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=MYDATA.CONTCONTENTSFORMEPS_FULLYR_2012 OUT=MYDATA.CONTCONTENTSFORMEPS_FULLYR_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM MYDATA.CONTCONTENTSFORMEPS_FULLYR_2012
		WHERE memname='MEPS_FULLYR_2012';
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


/*   START OF NODE: Data & observations subset   */
%LET _CLIENTTASKLABEL='Data & observations subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_FULLYR_2012_subset);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_FULLYR_2012_subset(label="MEPS_FULLYR_2012_subset") AS 
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
          t1.DUPERSID, 
          t1.EDUYRDEG, 
          t1.EMPHDX, 
          t1.ERTOT12, 
          t1.FAMINC12, 
          t1.FOODST12, 
          t1.HIBPDX, 
          t1.INS12X, 
          t1.MARRY12X, 
          t1.MCARE12, 
          t1.MIDX, 
          t1.PROVTY42, 
          t1.RACETHX, 
          t1.REGION12, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.SEX, 
          t1.SFFLAG42, 
          t1.SSIP12X, 
          t1.STRKDX, 
          t1.UNINS12, 
          t1.WAGEP12X
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS program code_subset1attributes_0108   */
%LET SYSLAST=MYDATA.MEPS_FULLYR_2012_SUBSET;
%LET _CLIENTTASKLABEL='SAS program code_subset1attributes_0108';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\alofters\SAS program code\SAS program code_subset1attributes_0108.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 9:12:41 AM
   By task: Subset Data Set Attributes

   Input Data: Local:MYDATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MYDATA.CONTCONTENTSFORMEPS_FULLYR_2012_);
TITLE "Subset Data Set Attributes";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.MEPS_FULLYR_2012_SUBSET ;

RUN;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 Adult MEPS Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 Adult MEPS Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:09:48 AM
   By task: One-Way Frequencies for 2012 Adult MEPS Subset

   Input Data: Local:MYDATA.MEPS_FULLYR_2012_SUBSET
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MEPS_FULLYR_2012_SUBSET
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42
		     , T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42
		     , T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42, T.AGE12X, T.AMAEXP12, T.AMCEXP12, T.AMCHIR12, T.AMDRC12, T.AMEEXP12
		     , T.AMNEXP12, T.AMNURS12, T.AMOPTO12, T.AMTEXP12, T.AMTHER12, T.AMTOTC12, T.ARTHDX, T.ASTHDX, T.BLIND42, T.BMINDX53, T.BPMLDX, T.BUSNP12X, T.CANCERDX, T.CHDDX, T.CHOLDX, T.DEAF42, T.SEX, T.RACETHX, T.MARRY12X, T.DIABDX
		     , T.DIVDP12X, T.DNTINS12, T.DNUNAB42, T.DSCGRP53, T.DSCINT53, T.DSCNPC53, T.DSCPCP53, T.DSCPHN53, T.DSDIA53, T.DSFLNV53, T.EDUYRDEG, T.EMPHDX, T.ERTOT12, T.FAMINC12, T.FOODST12, T.HIBPDX, T.INS12X, T.MCARE12, T.MIDX, T.WAGEP12X
		     , T.UNINS12, T.STRKDX, T.SSIP12X, T.SFFLAG42, T.SAQELIG, T.SAQWT12F, T.PROVTY42, T.REGION12
	FROM MYDATA.MEPS_FULLYR_2012_SUBSET(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 Adult MEPS Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrienne Lofters";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADAPPT42 /  SCORES=TABLE;
	TABLES ADCAPE42 /  SCORES=TABLE;
	TABLES ADCLIM42 /  SCORES=TABLE;
	TABLES ADCMPD42 /  SCORES=TABLE;
	TABLES ADCMPM42 /  SCORES=TABLE;
	TABLES ADCMPY42 /  SCORES=TABLE;
	TABLES ADDAYA42 /  SCORES=TABLE;
	TABLES ADDOWN42 /  SCORES=TABLE;
	TABLES ADDPRS42 /  SCORES=TABLE;
	TABLES ADDRBP42 /  SCORES=TABLE;
	TABLES ADEFRT42 /  SCORES=TABLE;
	TABLES ADEGMC42 /  SCORES=TABLE;
	TABLES ADEXPL42 /  SCORES=TABLE;
	TABLES ADEZUN42 /  SCORES=TABLE;
	TABLES ADFFRM42 /  SCORES=TABLE;
	TABLES ADFHLP42 /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADHECR42 /  SCORES=TABLE;
	TABLES ADHOPE42 /  SCORES=TABLE;
	TABLES ADILCR42 /  SCORES=TABLE;
	TABLES ADILWW42 /  SCORES=TABLE;
	TABLES ADINSA42 /  SCORES=TABLE;
	TABLES ADINSB42 /  SCORES=TABLE;
	TABLES ADINST42 /  SCORES=TABLE;
	TABLES ADINTR42 /  SCORES=TABLE;
	TABLES ADLANG42 /  SCORES=TABLE;
	TABLES ADLIST42 /  SCORES=TABLE;
	TABLES ADMALS42 /  SCORES=TABLE;
	TABLES ADMWLM42 /  SCORES=TABLE;
	TABLES ADNDCR42 /  SCORES=TABLE;
	TABLES ADNERV42 /  SCORES=TABLE;
	TABLES ADNRGY42 /  SCORES=TABLE;
	TABLES ADNSMK42 /  SCORES=TABLE;
	TABLES ADOVER42 /  SCORES=TABLE;
	TABLES ADPAIN42 /  SCORES=TABLE;
	TABLES ADPALS42 /  SCORES=TABLE;
	TABLES ADPRTM42 /  SCORES=TABLE;
	TABLES ADPRX42 /  SCORES=TABLE;
	TABLES ADPWLM42 /  SCORES=TABLE;
	TABLES ADRESP42 /  SCORES=TABLE;
	TABLES ADREST42 /  SCORES=TABLE;
	TABLES ADRISK42 /  SCORES=TABLE;
	TABLES ADRTCR42 /  SCORES=TABLE;
	TABLES ADRTWW42 /  SCORES=TABLE;
	TABLES ADSAD42 /  SCORES=TABLE;
	TABLES ADSMOK42 /  SCORES=TABLE;
	TABLES ADSOCA42 /  SCORES=TABLE;
	TABLES ADSPEC42 /  SCORES=TABLE;
	TABLES ADSPRF42 /  SCORES=TABLE;
	TABLES ADTLHW42 /  SCORES=TABLE;
	TABLES ADWRTH42 /  SCORES=TABLE;
	TABLES AGE12X /  SCORES=TABLE;
	TABLES AMAEXP12 /  SCORES=TABLE;
	TABLES AMCEXP12 /  SCORES=TABLE;
	TABLES AMCHIR12 /  SCORES=TABLE;
	TABLES AMDRC12 /  SCORES=TABLE;
	TABLES AMEEXP12 /  SCORES=TABLE;
	TABLES AMNEXP12 /  SCORES=TABLE;
	TABLES AMNURS12 /  SCORES=TABLE;
	TABLES AMOPTO12 /  SCORES=TABLE;
	TABLES AMTEXP12 /  SCORES=TABLE;
	TABLES AMTHER12 /  SCORES=TABLE;
	TABLES AMTOTC12 /  SCORES=TABLE;
	TABLES ARTHDX /  SCORES=TABLE;
	TABLES ASTHDX /  SCORES=TABLE;
	TABLES BLIND42 /  SCORES=TABLE;
	TABLES BMINDX53 /  SCORES=TABLE;
	TABLES BPMLDX /  SCORES=TABLE;
	TABLES BUSNP12X /  SCORES=TABLE;
	TABLES CANCERDX /  SCORES=TABLE;
	TABLES CHDDX /  SCORES=TABLE;
	TABLES CHOLDX /  SCORES=TABLE;
	TABLES DEAF42 /  SCORES=TABLE;
	TABLES SEX /  SCORES=TABLE;
	TABLES RACETHX /  SCORES=TABLE;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES DIABDX /  SCORES=TABLE;
	TABLES DIVDP12X /  SCORES=TABLE;
	TABLES DNTINS12 /  SCORES=TABLE;
	TABLES DNUNAB42 /  SCORES=TABLE;
	TABLES DSCGRP53 /  SCORES=TABLE;
	TABLES DSCINT53 /  SCORES=TABLE;
	TABLES DSCNPC53 /  SCORES=TABLE;
	TABLES DSCPCP53 /  SCORES=TABLE;
	TABLES DSCPHN53 /  SCORES=TABLE;
	TABLES DSDIA53 /  SCORES=TABLE;
	TABLES DSFLNV53 /  SCORES=TABLE;
	TABLES EDUYRDEG /  SCORES=TABLE;
	TABLES EMPHDX /  SCORES=TABLE;
	TABLES ERTOT12 /  SCORES=TABLE;
	TABLES FAMINC12 /  SCORES=TABLE;
	TABLES FOODST12 /  SCORES=TABLE;
	TABLES HIBPDX /  SCORES=TABLE;
	TABLES INS12X /  SCORES=TABLE;
	TABLES MCARE12 /  SCORES=TABLE;
	TABLES MIDX /  SCORES=TABLE;
	TABLES WAGEP12X /  SCORES=TABLE;
	TABLES UNINS12 /  SCORES=TABLE;
	TABLES STRKDX /  SCORES=TABLE;
	TABLES SSIP12X /  SCORES=TABLE;
	TABLES SFFLAG42 /  SCORES=TABLE;
	TABLES SAQELIG /  SCORES=TABLE;
	TABLES SAQWT12F /  SCORES=TABLE;
	TABLES PROVTY42 /  SCORES=TABLE;
	TABLES REGION12 /  SCORES=TABLE;
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


/*   START OF NODE: Recode variables for subset data   */
%LET _CLIENTTASKLABEL='Recode variables for subset data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE AS 
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
          /* ADGENH42r */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="SAQ: Health in general SF-12V2" AS ADGENH42r, 
          /* ADDAYA42r */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="SAQ: Health limits moderate activites SF-12V2" AS ADDAYA42r, 
          /* ADPALS42r */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="SAQ 4WKS: Accomplished less b/c of physical problems SF-12V2" AS ADPALS42r, 
          /* ADPWLM42r */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="SAQ 4WKS: Work limit b/c of physical problems SF-12V2" AS ADPWLM42r, 
          /* ADMALS42r */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="SAQ 4WKS: Accomplished less b/c of MNT probs" AS ADMALS42r, 
          /* ADMWLM42r */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="SAQ 4WKS: Work limit b/c of MNT problems SF-12V2" AS ADMWLM42r, 
          /* ADPAIN42r */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="SAQ 4WKS: Pain limits normal work SF-12V2" AS ADPAIN42r, 
          /* ADCAPE42r */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="SAQ 4WKS: Felt calm/peaceful SF-12V2" AS ADCAPE42r, 
          /* ADNRGY42r */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="SAQ 4WKS: Had a lot of energy SF-12V2" AS ADNRGY42r, 
          /* ADDOWN42r */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="SAQ 4WKS: Felt downhearted/depressed SF-12V2" AS ADDOWN42r, 
          /* ADSOCA42r */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="SAQ 4WKS: Health stopped social activites SF-12V2" AS ADSOCA42r, 
          /* MARRY12Xr */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
            END) LABEL="Marital status-12/31/12" AS MARRY12Xr, 
          /* CANCERDXr */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="Cancer diagnosis (>17)" AS CANCERDXr, 
          /* CHDDXr */
            (CASE 
               WHEN -7 = t1.CHDDX THEN .
               WHEN -8 = t1.CHDDX THEN .
               WHEN -9 = t1.CHDDX THEN .
               ELSE t1.CHDDX
            END) LABEL="Coronary heart disease diagnosis (>17)" AS CHDDXr, 
          /* FOODST12r */
            (CASE 
               WHEN -7 = t1.FOODST12 THEN .
               WHEN -8 = t1.FOODST12 THEN .
               ELSE t1.FOODST12
            END) LABEL="Did anyone pruchase food stamps" AS FOODST12r, 
          /* STRKDXr */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="Stroke diagnosis (>17)" AS STRKDXr, 
          /* ADHECR42r */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="SAQ rating of health care" AS ADHECR42r, 
          /* ADSMOK42r */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="SAQ Currently smoking" AS ADSMOK42r, 
          /* ADSPEC42r */
            (CASE 
               WHEN -1 = t1.ADSPEC42 THEN .
               WHEN -9 = t1.ADSPEC42 THEN .
               ELSE t1.ADSPEC42
            END) LABEL="SAQ 12MOS: Needed to see specialist" AS ADSPEC42r, 
          /* ADNDCR42r */
            (CASE 
               WHEN -1 = t1.ADNDCR42 THEN .
               WHEN -8 = t1.ADNDCR42 THEN .
               WHEN -9 = t1.ADNDCR42 THEN .
               ELSE t1.ADNDCR42
            END) LABEL="SAQ 12MOS: Needed any care, test, treatment" AS ADNDCR42r, 
          /* ADILCR42r */
            (CASE 
               WHEN -1 = t1.ADILCR42 THEN .
               WHEN -9 = t1.ADILCR42 THEN .
               ELSE t1.ADILCR42
            END) LABEL="SAQ 12MOS: Illness/injury needing immediate care" AS ADILCR42r, 
          /* ADEGMC42r */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="SAQ 12MOS: Easy getting medical care" AS ADEGMC42r, 
          /* ADINTR42r */
            (CASE 
               WHEN -1 = t1.ADINTR42 THEN .
               WHEN -7 = t1.ADINTR42 THEN .
               WHEN -8 = t1.ADINTR42 THEN .
               WHEN -9 = t1.ADINTR42 THEN .
               ELSE t1.ADINTR42
            END) LABEL="SAQ 12MOS: Little interest in things" AS ADINTR42r, 
          /* ADLIST42r */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="SAQ 12MOS: Doctor listened to you" AS ADLIST42r, 
          /* ADILWW42r */
            (CASE 
               WHEN -1 = t1.ADILWW42 THEN .
               WHEN -9 = t1.ADILWW42 THEN .
               ELSE t1.ADILWW42
            END) LABEL="SAQ 12MOS: Got care for illness/injury when needed" AS ADILWW42r, 
          /* ADEXPL42r */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="SAQ 12MOS: Doctor explained so understood" AS ADEXPL42r, 
          /* ADLANG42r */
            (CASE 
               WHEN -1 = t1.ADLANG42 THEN .
               ELSE t1.ADLANG42
            END) LABEL="SAQ: Language of interviewer" AS ADLANG42r, 
          /* BLIND42r */
            (CASE 
               WHEN -1 = t1.BLIND42 THEN .
               ELSE t1.BLIND42
            END) LABEL="Person is blind" AS BLIND42r, 
          /* ADCLIM42r */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="SAQ: Health limits climbing stairs SF-12V2" AS ADCLIM42r, 
          /* ADRTCR42r */
            (CASE 
               WHEN -1 = t1.ADRTCR42 THEN .
               WHEN -8 = t1.ADRTCR42 THEN .
               WHEN -9 = t1.ADRTCR42 THEN .
               ELSE t1.ADRTCR42
            END) LABEL="SAQ 12MOS: Made appointment routine medical care" AS ADRTCR42r, 
          /* MIDXr */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="Heart attack diagnosis" AS MIDXr, 
          /* DSCPCP53r */
            (CASE 
               WHEN -1 = t1.DSCPCP53 THEN .
               WHEN -9 = t1.DSCPCP53 THEN .
               ELSE t1.DSCPCP53
            END) LABEL="Learned care from primary care provider" AS DSCPCP53r, 
          /* HIBPDXr */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="High blood pressure diagnosis" AS HIBPDXr
      FROM MYDATA.MEPS_FULLYR_2012_SUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Overall table analysis recoded subset   */
%LET _CLIENTTASKLABEL='Overall table analysis recoded subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:09:49 AM
   By task: Overall table analysis recoded subset

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADCAPE42, T.ADCAPE42r, T.ADCLIM42, T.ADCLIM42r, T.ADDAYA42, T.ADDAYA42r, T.ADDOWN42, T.ADDOWN42r, T.ADEGMC42, T.ADEGMC42r, T.ADEXPL42, T.ADEXPL42r, T.ADGENH42, T.ADGENH42r, T.ADHECR42, T.ADHECR42r, T.ADILCR42, T.ADILCR42r
		     , T.ADILWW42, T.ADILWW42r, T.ADINTR42, T.ADINTR42r, T.ADLANG42, T.ADLANG42r, T.ADLIST42, T.ADLIST42r, T.ADMALS42, T.ADMALS42r, T.ADMWLM42, T.ADMWLM42r, T.ADNDCR42, T.ADNDCR42r, T.ADNRGY42, T.ADNRGY42r, T.ADPAIN42, T.ADPAIN42r
		     , T.ADPALS42, T.ADPALS42r, T.ADPWLM42, T.ADPWLM42r, T.ADRTCR42, T.ADRTCR42r, T.ADSMOK42, T.ADSMOK42r, T.ADSOCA42, T.ADSOCA42r, T.ADSPEC42, T.ADSPEC42r, T.BLIND42, T.BLIND42r, T.CANCERDX, T.CANCERDXr, T.CHDDX, T.CHDDXr, T.DSCPCP53
		     , T.DSCPCP53r, T.FOODST12, T.FOODST12r, T.HIBPDX, T.HIBPDXr, T.MARRY12X, T.MARRY12Xr, T.MIDX, T.MIDXr, T.STRKDX, T.STRKDXr
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE as T
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
	TABLES ADCAPE42r * ADCAPE42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCLIM42r * ADCLIM42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDAYA42r * ADDAYA42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDOWN42r * ADDOWN42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADEGMC42r * ADEGMC42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADEXPL42r * ADEXPL42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADGENH42r * ADGENH42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADHECR42r * ADHECR42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADILCR42r * ADILCR42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADILWW42r * ADILWW42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADINTR42r * ADINTR42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLANG42r * ADLANG42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLIST42r * ADLIST42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42r * ADMALS42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMWLM42r * ADMWLM42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNDCR42r * ADNDCR42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42r * ADNRGY42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN42r * ADPAIN42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42r * ADPALS42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPWLM42r * ADPWLM42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADRTCR42r * ADRTCR42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSMOK42r * ADSMOK42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSOCA42r * ADSOCA42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSPEC42r * ADSPEC42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES BLIND42r * BLIND42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES CANCERDXr * CANCERDX /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES CHDDXr * CHDDX /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES DSCPCP53r * DSCPCP53 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES FOODST12r * FOODST12 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HIBPDXr * HIBPDX /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARRY12Xr * MARRY12X /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MIDXr * MIDX /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES STRKDXr * STRKDX /
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


/*   START OF NODE: 5 examples recoded subset for hw   */
%LET _CLIENTTASKLABEL='5 examples recoded subset for hw';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_010915.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_010915.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 10:09:50 AM
   By task: 5 examples recoded subset for hw

   Input Data: Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CANCERDXr, T.CANCERDX, T.ADSMOK42r, T.ADSMOK42, T.ADHECR42r, T.ADHECR42, T.ADDAYA42r, T.ADDAYA42, T.ADCAPE42r, T.ADCAPE42
	FROM WORK.QUERY_FOR_MEPS_FULLYR_2012_SUBSE as T
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
	TABLES ADCAPE42r * ADCAPE42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDAYA42r * ADDAYA42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADHECR42r * ADHECR42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSMOK42r * ADSMOK42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES CANCERDXr * CANCERDX /
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
