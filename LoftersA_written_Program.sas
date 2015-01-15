/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 2:41:27 PM
PROJECT: LoftersA_SAS_project_011415
PROJECT PATH: P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011415.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\alofters\Assignments' ;

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

/*   START OF NODE: Program   */
%LET _CLIENTTASKLABEL='Program';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\alofters\Assignments\LoftersA_SAS_project_011415.egp';
%LET _CLIENTPROJECTNAME='LoftersA_SAS_project_011415.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\alofters\Assignments" ;
*Create a data subset with important variables and observations ages 18+;
*program written by Adrie Lofters;
data MYDATA.MEPS_FULLYR_PRGCODE;
set MYDATA.MEPS_FULLYR_2012_SUBSET;

Keep ADAPPT42 
          ADCAPE42 
          ADCLIM42 
          ADCMPD42 
          ADCMPM42 
          ADCMPY42 
          ADDAYA42 
          ADDOWN42 
          ADDPRS42 
          ADDRBP42 
          ADEFRT42 
          ADEGMC42 
          ADEXPL42 
          ADEZUN42 
          ADFFRM42 
          ADFHLP42 
          ADGENH42 
          ADHECR42 
          ADHOPE42 
          ADILCR42 
          ADILWW42 
          ADINSA42 
          ADINSB42 
          ADINST42 
          ADINTR42 
          ADLANG42 
          ADLIST42 
          ADMALS42 
          ADMWLM42 
          ADNDCR42 
          ADNERV42 
          ADNRGY42 
          ADNSMK42 
          ADOVER42 
          ADPAIN42 
          ADPALS42 
          ADPRTM42 
          ADPRX42 
          ADPWLM42 
          ADRESP42 
          ADREST42 
          ADRISK42 
          ADRTCR42 
          ADRTWW42 
          ADSAD42 
          ADSMOK42 
          ADSOCA42 
          ADSPEC42 
          ADSPRF42 
          ADTLHW42 
          ADWRTH42 
          AGE12X 
          AMAEXP12 
          AMCEXP12 
          AMCHIR12 
          AMDRC12 
          AMEEXP12 
          AMNEXP12 
          AMNURS12 
          AMOPTO12 
          AMTEXP12 
          AMTHER12 
          AMTOTC12 
          ARTHDX 
          ASTHDX 
          BLIND42 
          BMINDX53 
          BPMLDX 
          BUSNP12X 
          CANCERDX 
          CHDDX 
          CHOLDX 
          DEAF42 
          DIABDX 
          DIVDP12X 
          DNTINS12 
          DNUNAB42 
          DSCGRP53 
          DSCINT53 
          DSCNPC53 
          DSCPCP53 
          DSCPHN53 
          DSDIA53 
          DSFLNV53 
          DUPERSID 
          EDUYRDEG 
          EMPHDX 
          ERTOT12 
          FAMINC12 
          FOODST12 
          HIBPDX 
          INS12X 
          MARRY12X 
          MCARE12 
          MIDX 
          PROVTY42 
          RACETHX 
          REGION12 
          SAQELIG 
          SAQWT12F 
          SEX 
          SFFLAG42 
          SSIP12X 
          STRKDX 
          UNINS12 
          WAGEP12X 
          EDRECODE;

      WHERE AGE12X >= 18;


	  run;
	  proc sort;
	  by DUPERSID;
	  run;
	  data new;
	set mydata.MEPS_FULLYR_PRGCODE;
	*create temporary data set with managed variables;

	array old(*) ADGENH42
				ADDAYA42
				ADPALS42
				ADPWLM42
				ADMALS42
				ADMWLM42
				ADPAIN42
				ADCAPE42
				ADNRGY42
				ADDOWN42
				ADSOCA42
				MARRY12X
				CANCERDX
				CHDDX
				FOODST12
				STRKDX
				ADHECR42
				ADSMOK42
				ADSPEC42
				ADNDCR42
				ADILCR42
				ADEGMC42
				ADINTR42
				ADLIST42
				ADILWW42
				ADEXPL42
				ADLANG42
				BLIND42
				ADCLIM42
				ADRTCR42
				MIDX
				DSCPCP53
				HIBPDX
				EDRECODE;
	array new(*)ADGENH42r
				ADDAYA42r
				ADPALS42r
				ADPWLM42r
				ADMALS42r
				ADMWLM42r
				ADPAIN42r
				ADCAPE42r
				ADNRGY42r
				ADDOWN42r
				ADSOCA42r
				MARRY12Xr
				CANCERDXr
				CHDDXr
				FOODST12r
				STRKDXr
				ADHECR42r
				ADSMOK42r
				ADSPEC42r
				ADNDCR42r
				ADILCR42r
				ADEGMC42r
				ADINTR42r
				ADLIST42r
				ADILWW42r
				ADEXPL42r
				ADLANG42r
				BLIND42r
				ADCLIM42r
				ADRTCR42r
				MIDXr
				DSCPCP53r
				HIBPDXr
				EDRECODEr;
	do i=1 to dim(old);
	if old(i)<0 then new(i)=.;
	
	else new(i) =old(i);
	end;
	*above array to recode missing data;

LABEL ADGENH42r="SAQ: Health in general SF-12V2";
LABEL ADDAYA42r="SAQ: Health limits moderate activites SF-12V2";
LABEL ADPALS42r="SAQ 4WKS: Accomplished less b/c of physical problems SF-12V2";
LABEL ADPWLM42r="SAQ 4WKS: Work limit b/c of physical problems SF-12V2";
LABEL ADMALS42r="SAQ 4WKS: Accomplished less b/c of MNT probs";
LABEL ADMWLM42="SAQ 4WKS: Work limit b/c of MNT problems SF-12V2";
LABEL ADPAIN42="SAQ 4WKS: Pain limits normal work SF-12V2";
LABEL ADCAPE42r="SAQ 4WKS: Felt calm/peaceful SF-12V2";
LABEL ADNRGY42r="SAQ 4WKS: Had a lot of energy SF-12V2";
 LABEL ADDOWN42r="SAQ 4WKS: Felt downhearted/depressed SF-12V2";
LABEL ADSOCA42r="SAQ 4WKS: Health stopped social activites SF-12V2";
LABEL MARRY12Xr="Marital status-12/31/12";
LABEL CANCERDXr="Cancer diagnosis (>17)";
LABEL CHDDXr="Coronary heart disease diagnosis (>17)"; 
LABEL FOODST12r="Did anyone pruchase food stamps"     ;
 LABEL STRKDXr="Stroke diagnosis (>17)";
LABEL ADHECR42r="SAQ rating of health care"  ;
LABEL ADSMOK42r="SAQ Currently smoking"; 
LABEL ADSPEC42r="SAQ 12MOS: Needed to see specialist";
LABEL ADNDCR42="SAQ 12MOS: Needed any care, test, treatment";
LABEL ADILCR42r="SAQ 12MOS: Illness/injury needing immediate care";
LABEL ADEGMC42r="SAQ 12MOS: Easy getting medical care";
LABEL ADINTR42r="SAQ 12MOS: Little interest in things";
 LABEL ADLIST42r="SAQ 12MOS: Doctor listened to you";
 LABEL ADILWW42r="SAQ 12MOS: Got care for illness/injury when needed";
 LABEL ADEXPL42r="SAQ 12MOS: Doctor explained so understood";
  LABEL ADLANG42r="SAQ: Language of interviewer";
  LABEL BLIND42Rr="Person is blind";
  LABEL ADCLIM42r="SAQ: Health limits climbing stairs SF-12V2";
  LABEL ADRTCR42r="SAQ 12MOS: Made appointment routine medical care";
  LABEL MIDXr="Heart attack diagnosis";
  LABEL DSCPCP53r="Learned care from primary care provider";
  LABEL HIBPDXr="High blood pressure diagnosis"; 
  LABEL EDRECODEr="Education recode (edited)";
 
array old1(*) ADGENH42r
				ADPAIN42r ADCAPE42r ADNRGY42r;
array new1(*) ADGENH42r_rc ADPAIN42r_rc ADCAPE42r_rc ADNRGY42r_rc;
do i=1 to dim(old1);
	new1(i)=6-old1(i);
	end;
	*above array to reverse code 4 SF-12V2 variables;


  sf12v2_overall=SUM(ADGENH42r_rc,ADDAYA42r,ADCLIM42r,ADPALS42r,ADPWLM42r,ADMALS42r,ADMWLM42r,ADPAIN42r_rc,ADCAPE42r_rc,ADNRGY42r_rc,ADDOWN42r,ADSOCA42r);
  LABEL sf12v2_overall="Aggregate Health score of (r & rc) SF-12V2 SAQs";
*above 2 lines of code to create an overall health score for SF12V2 responses; 
       
if sf12v2_overall >0 and sf12v2_overall<41
then sf12v2_categorical=1;
if sf12v2_overall >=42 and sf12v2_overall<48
then sf12v2_categorical=2;
if sf12v2_overall >=48 and sf12v2_overall<52
then sf12v2_categorical=3;
else if sf12v2_overall >52 then sf12v2_categorical=4;
LABEL sf12v2_categorical="Overall health score SF-12V2";
*above code to make a categorical variable of SF-12V2 variables; 

	run;

	PROC FREQ DATA = WORK.NEW
	ORDER=INTERNAL;

	TABLES ADCLIM42r * ADCLIM42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42r_rc * ADCAPE42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDAYA42r * ADDAYA42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDOWN42r * ADDOWN42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADEGMC42r * ADEGMC42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADEXPL42r * ADEXPL42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADGENH42r_rc * ADGENH42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADHECR42r * ADHECR42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADILCR42r * ADILCR42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADILWW42r * ADILWW42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADINTR42r * ADINTR42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLANG42r * ADLANG42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLIST42r * ADLIST42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42r * ADMALS42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMWLM42r * ADMWLM42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNDCR42r * ADNDCR42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42r_rc * ADNRGY42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN42r_rc * ADPAIN42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42r * ADPALS42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPWLM42r * ADPWLM42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADRTCR42r * ADRTCR42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSMOK42r * ADSMOK42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSOCA42r * ADSOCA42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSPEC42r * ADSPEC42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES BLIND42r * BLIND42 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES CANCERDXr * CANCERDX /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES CHDDXr * CHDDX /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES DSCPCP53r * DSCPCP53 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDRECODEr * EDRECODE /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES FOODST12r * FOODST12 /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARRY12Xr * MARRY12X /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MIDXr * MIDX /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES STRKDXr * STRKDX /
		NOROW
		NOCOL
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
*Generating frequency tables to check all my my recoded and reverse coded variables against the originals;
RUN;


TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results of SF12V2 Overall Scores";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";
PROC MEANS DATA=WORK.NEW
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
	VAR sf12v2_overall;
*Generating summary statistics for the SF-12V2 overall scores to determine how to categorize them;
RUN;

TITLE;
TITLE1 "Report Listing of SF-12V2 Recoded Variables and the Sum Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Adrie Lofters";

PROC PRINT DATA=WORK.NEW
	(OBS=50)
	OBS="Row number"
	LABEL;
	VAR sf12v2_overall ADCLIM42r ADDOWN42r ADSOCA42r ADMWLM42r ADMALS42r ADPWLM42r ADPALS42r ADDAYA42r ADGENH42r_rc ADPAIN42r_rc ADCAPE42r_rc ADNRGY42r_rc;
*Generating a list to make sure my summed SF-12V2 scores are accurate;
RUN;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
