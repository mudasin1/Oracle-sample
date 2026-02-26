IDENTIFICATION DIVISION.
PROGRAM-ID.
	STP295.
AUTHOR.
	SIMON ARNOLD.
DATE-WRITTEN.
	OCTOBER 2005.
******************************************************************************
* PRINT REPORT OF AWARDS RUN BLOCKS					     *
******************************************************************************
* AMENDMENT HISTORY							     *
******************************************************************************
* DATE	   | AUTHOR   | REASON	         				     *
******************************************************************************
* 18/10/05 | S ARNOLD | ORIGINAL VERSION.                                    *
******************************************************************************
* 20/12/05 | S ARNOLD | Change the number of lines per page to accommodate   *
*          |          | /QUEUE=FIFTH.                                        *
******************************************************************************
* 09/05/08	MP	Ignore deleted students
* 21/10/13     CAG 	WI1229 - AMR blocks.       
******************************************************************************
/
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER.
	VAX-11.
OBJECT-COMPUTER.
	VAX-11.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT PRINT-FILE	ASSIGN PRINTER.
	SELECT LABELS-FILE	ASSIGN LABELS1.
/
DATA DIVISION.
FILE SECTION.
*
FD	PRINT-FILE.
01	PRINT-REC.
	03  FILLER				PIC X(132).
*
FD	LABELS-FILE.
01	LABELS-REC.
	03  L-CENTRE-NO				PIC X(06).
/
WORKING-STORAGE SECTION.
*
01  SQLFPN GLOBAL.
           02  SQLFPN-FILE-LEN PIC S9(4) COMP-5 VALUE +37.
           02  SQLFPN-FILENAME PIC X(37) VALUE "/work/SampleFiles/ProCOBOL/STP295.pco".

01  SQ0002 GLOBAL.
           02  FILLER PIC X(79) VALUE "update BNM_AWARDS_RUN_BLOCKS  set BARB_REPORT_IND='N' where BARB_REPORT_IND='Y'".

01  SQLCTX GLOBAL PIC S9(9) COMP-5 VALUE +364565190.


01  SQLEXD GLOBAL.
           02  SQL-SQLVSN   PIC S9(18) COMP-5 VALUE +13.
           02  SQL-ARRSIZ   PIC S9(9) COMP-5 VALUE +13.
           02  SQL-ITERS    PIC S9(9) COMP-5.
           02  SQL-OFFSET   PIC S9(9) COMP-5.
           02  SQL-SELERR   PIC S9(4) COMP-5.
           02  SQL-SQLETY   PIC S9(4) COMP-5.
           02  SQL-OCCURS   PIC S9(9) COMP-5.
           02  SQL-DUMMY    PIC S9(9) COMP-5.
           02  SQL-CUD      PIC S9(18) COMP-5.
           02  SQL-SQLEST   PIC S9(18) COMP-5.
           02  SQL-STMT     PIC S9(18) COMP-5.
           02  SQL-SQLADTP  PIC S9(18) COMP-5 VALUE 0.
           02  SQL-SQLTDSP  PIC S9(18) COMP-5 VALUE 0.
           02  SQL-SQPHSV   PIC S9(18) COMP-5.
           02  SQL-SQPHSL   PIC S9(18) COMP-5.
           02  SQL-SQPHSS   PIC S9(18) COMP-5.
           02  SQL-SQPIND   PIC S9(18) COMP-5.
           02  SQL-SQPINS   PIC S9(18) COMP-5.
           02  SQL-SQPARM   PIC S9(18) COMP-5.
           02  SQL-SQPARC   PIC S9(18) COMP-5.
           02  SQL-SQPADTO  PIC S9(18) COMP-5.
           02  SQL-SQPTDSO  PIC S9(18) COMP-5.
           02  SQL-SQPHR1   PIC S9(9) COMP-5.
           02  SQL-SQPHR2   PIC S9(9) COMP-5.
           02  SQL-SQPHR3   PIC S9(9) COMP-5.
           02  SQL-SQPHR4   PIC S9(9) COMP-5.
           02  SQL-SQPHR5   PIC S9(9) COMP-5.
           02  SQL-SQFOFF   PIC S9(9) COMP-5.
           02  SQL-SQCMOD   PIC S9(9) COMP-5.
           02  SQL-SQFMOD   PIC S9(9) COMP-5.
           02  SQL-SQPMEM   PIC S9(9) COMP-5.
           02  SQL-DUMMYPF  PIC S9(9) COMP-5.
           02  SQL-SQHSTV   PIC S9(18) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQHSTL   PIC S9(18) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQHSTS   PIC S9(18) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQINDV   PIC S9(18) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQINDS   PIC S9(18) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQHARM   PIC S9(18) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQHARC   PIC S9(18) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQADTO   PIC S9(4) COMP-5 OCCURS 13 TIMES.
           02  SQL-SQTDSO   PIC S9(4) COMP-5 OCCURS 13 TIMES.


01  SQ0001 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ST_CENTRE_ID  ,CN_CENTRE_NAME  ,NVL(ST_COURSE_ID,NVL(ST_NVQ_REGISTERED_ID,ST_GNVQ_REG"&
    "ISTERED_ID))  ,NVL(AC_DESCRIPTION,DECODE(GNVQ_GLEV_CODE,null ,DECODE(NL_CODE,null ,'INDIVIDUAL UNITS',('NVQ '||NL_DESCRIPTION)"&
    "),('GNVQ '||GLEV_DESCRIPTION)))  ,NVL(".

           02  FILLER PIC X(256) VALUE "AT_NAME,NVL(NVQ_TITLE_1,GNVQ_TITLE_1))  ,ST_REG_NO  ,(RPAD(((ST_FORENAMES||' ')||ST_SURNAME)"&
    ",(47-NVL(LENGTH(ST_UCAS_REF),0)))||ST_UCAS_REF)  ,BARB_NPP_ID  ,NPP_NAME  ,DECODE(BARB_REASON_CODE,'1','CENTRE DEFERRED','2','"&
    "STUDENT DEFERRED','3','CENTRE DEFERRED".

           02  FILLER PIC X(256) VALUE "','4','NO EV REPORT','5',('EV DECISION'||DECODE(max(BARB_DECISION_DATE),null ,null ,((' ('||"&
    "TO_CHAR(max(BARB_DECISION_DATE),'DD-MON-YY'))||')'))),'6','QS INTERVENTION','9','AMR BLOCK','UNKNOWN REASON')  ,DECODE(min(BAR"&
    "B_CLAIM_TYPE),'A','FULL','F','FALLBACK".

           02  FILLER PIC X(256) VALUE "','I','INTERIM','UNKNOWN')  ,TO_CHAR(TRUNC(BARB_DATE),'DD-MON-YY')  ,BARB_REASON_CODE   from"&
    " STUDENTS ,CENTRES ,NPPS ,NVQS ,NVQ_LEVELS ,GNVQS ,GNVQ_LEVELS ,APPROVAL_AWARDS ,APPROVAL_APPLICATION ,AWARD_TITLES ,AWARD_COD"&
    "ES ,BNM_AWARDS_RUN_BLOCKS where ((((((".

           02  FILLER PIC X(256) VALUE "(((((((NVL(BARB_CLEARED_IND,'N')='N' and ST_REG_NO=BARB_REG_NO) and CN_CENTRE_ID=ST_CENTRE_I"&
    "D) and NPP_ID(+)=BARB_NPP_ID) and NVQ_ID(+)=ST_NVQ_REGISTERED_ID) and NL_CODE(+)=NVQ_LEVEL_CODE) and GNVQ_ID(+)=ST_GNVQ_REGIST"&
    "ERED_ID) and GLEV_CODE(+)=GNVQ_GLEV_CO".

           02  FILLER PIC X(256) VALUE "DE) and AW_COURSE_NUMBER(+)=ST_COURSE_ID) and AA_APPLICAT_NO(+)=AW_APPLICAT_NO) and AT_NUMBE"&
    "R(+)=AA_BTEC_TITLE) and AC_CODE(+)=AW_AWARD_CODE) and (:b1='C' or BARB_REPORT_IND='Y')) and ST_DELETE is null ) group by ST_CE"&
    "NTRE_ID,CN_CENTRE_NAME,NVL(ST_COURSE_I".

           02  FILLER PIC X(256) VALUE "D,NVL(ST_NVQ_REGISTERED_ID,ST_GNVQ_REGISTERED_ID)),NVL(AC_DESCRIPTION,DECODE(GNVQ_GLEV_CODE,"&
    "null ,DECODE(NL_CODE,null ,'INDIVIDUAL UNITS',('NVQ '||NL_DESCRIPTION)),('GNVQ '||GLEV_DESCRIPTION))),NVL(AT_NAME,NVL(NVQ_TITL"&
    "E_1,GNVQ_TITLE_1)),ST_REG_NO,ST_FORENA".

           02  FILLER PIC X(256) VALUE "MES,ST_SURNAME,ST_UCAS_REF,BARB_NPP_ID,NPP_NAME,BARB_REASON_CODE,DECODE(BARB_REASON_CODE,'1'"&
    ",'CENTRE DEFERRED','2','STUDENT DEFERRED','3','CENTRE DEFERRED','4','NO EV REPORT','5','EV DECISION','6','QS INTERVENTION','9'"&
    ",'AMR BLOCK','UNKNOWN REASON'),TRUNC(B".

           02  FILLER PIC X(155) VALUE "ARB_DATE) order by DECODE(:b2,'R',BARB_REASON_CODE,'X'),ST_CENTRE_ID,NVL(ST_COURSE_ID,NVL(ST"&
    "_NVQ_REGISTERED_ID,ST_GNVQ_REGISTERED_ID)),ST_REG_NO           ".

01  SQL-RUNTIME-VARS.
           02  SQL-IAPXIT-SUCCESS  PIC S9(9) COMP-5 VALUE    +0.
           02  SQL-IAPXIT-FAILURE  PIC S9(9) COMP-5 VALUE +1403.
           02  SQL-IAPXIT-FATALERR PIC S9(9) COMP-5 VALUE  +535.

01  SQLCUD GLOBAL.
           02     FILLER PIC S9(4) COMP-5 VALUE +13.
           02     FILLER PIC S9(4) COMP-5 VALUE +4130.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +539.
           02     FILLER PIC S9(4) COMP-5 VALUE +532.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +36.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +2203.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +565.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +59.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +79.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +517.
           02     FILLER PIC S9(4) COMP-5 VALUE +736.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +74.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +766.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +89.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +541.
           02     FILLER PIC S9(4) COMP-5 VALUE +768.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +104.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +826.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +13.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +154.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +171.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +997.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +186.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +14.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +543.
           02     FILLER PIC S9(4) COMP-5 VALUE +1004.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
*       EXEC SQL
*       	BEGIN DECLARE SECTION
*       END-EXEC.
*
01	WS-USER-ID			PIC X(6)	VALUE 'abcd'.
01	WS-PASSWORD			PIC X(4)	VALUE 'abcd'.
*
01      WS-RUN-TYPE			PIC X(1).
01      WS-SORT-ORDER			PIC X(1).
01      WS-LABELS-REQD			PIC X(1).
01      WS-UPDATE-BARB			PIC X(1).
*
01	WS-CENTRE-NO                    PIC X(6).
01	WS-CENTRE-NAME                  PIC X(65).
01	WS-COURSE-NVQ-ID                PIC X(8). 
01	WS-AC-DESC                      PIC X(84).
01	WS-AT-NAME                      PIC X(90).
01	WS-REG-NO                       PIC X(7). 
01	WS-NAME-UCAS-REF                PIC X(51).
01	WS-NPP-ID                       PIC S9(9).  
01	WS-NPP-NAME                     PIC X(40).
01	WS-REASON                       PIC X(25).
01	WS-CLAIM-TYPE                   PIC X(8). 
01	WS-CLAIM-DATE                   PIC X(9). 
01	WS-REASON-CODE                  PIC X(1).
*
*       EXEC SQL
*       	END DECLARE SECTION
*       END-EXEC.
*
*       EXEC SQL
*       	INCLUDE SQLCA
*       END-EXEC.
01  SQLCA GLOBAL.
    05  SQLCAID               PIC X(8).
    05  SQLCABC               PIC S9(9) COMP-5.
    05  SQLCODE               PIC S9(9) COMP-5.
    05  SQLERRM.
        49 SQLERRML           PIC S9(4) COMP-5.
        49 SQLERRMC           PIC X(70).
    05  SQLERRP               PIC X(8).
    05  SQLERRD OCCURS 6 TIMES
                              PIC S9(9) COMP-5.
    05  SQLWARN.
        10 SQLWARN0           PIC X(1).
        10 SQLWARN1           PIC X(1).
        10 SQLWARN2           PIC X(1).
        10 SQLWARN3           PIC X(1).
        10 SQLWARN4           PIC X(1).
        10 SQLWARN5           PIC X(1).
        10 SQLWARN6           PIC X(1).
        10 SQLWARN7           PIC X(1).
    05  SQLEXT                PIC X(8).
*
01	WS-ERRORS.
  03	  WS-ERR-MESSAGE		PIC X(132) VALUE SPACES.
  03	  WS-ERR-CONNECT		PIC X(40) VALUE
			'CONNECT TO ORACLE FAILED'.
  03	  WS-ERR-FETCH			PIC X(40) VALUE
			'FETCH FAILED'.
  03	  WS-ERR-OPEN			PIC X(40) VALUE
			'OPEN CURSOR FAILED'.
  03	  WS-ERR-CLOSE			PIC X(40) VALUE
			'CLOSE CURSOR FAILED'.
  03	  WS-SQLCODE-X			PIC -Z(5)9.
*
01	WS-BITS.
	03  WS-END			PIC X.
		88 END-OF-FETCH		VALUE 'Y'.
*
	03  WS-OLD-CENTRE-NO		PIC X(6).
	03  WS-OLD-COURSE-NVQ-ID	PIC X(8).
	03  WS-OLD-REASON-CODE		PIC X(6).
*
	03  WS-PAGE-COUNT		PIC 9(4) 
*                                                COMP
                                                 COMP-5
                                                      VALUE 0.
	03  WS-LINECOUNT		PIC S999 COMP-3 VALUE 0.
	03  WS-LINE-SKIP		PIC S999 COMP-3 VALUE 0.
	03  WS-LINES-FOR-FOOTER		PIC S999 COMP-3 VALUE 8.
*
	03  WS-DATE.
		05  WS-YEAR		PIC XXXX.
		05  WS-MONTH		PIC XX.
		05  WS-DAY		PIC XX.
	03  WS-CURRENT-DATE		PIC X(21).
/
01	WS-REPORT-LINES.
*
	03  WS-LINE-1.
		05  FILLER			PIC X(11)
			VALUE 'RUN DATE : '.
		05  WS-RUN-DATE.
			07  WS-DAY		PIC XX.
			07  FILLER		PIC X VALUE '/'.
			07  WS-MONTH		PIC XX.
			07  FILLER		PIC X VALUE '/'.
			07  WS-YEAR		PIC XXXX.
		05  FILLER			PIC X(29) VALUE SPACES.
		05  WS-LINE-1-TITLE		PIC X(36)
                    VALUE "EDEXCEL AWARDS CLAIM BLOCKS LISTING".
		05  WS-LINE-1-HYPHEN		PIC X(2)  VALUE SPACES.
		05  WS-LINE-1-REASON		PIC X(31) VALUE SPACES.
		05  FILLER			PIC X(9)  VALUE ' Page No '.
		05  WS-PAGE-NUMBER		PIC Z(3)9.
*
	03  WS-LINE-1B.
		05  FILLER			PIC X(50) VALUE SPACES.
		05  WS-1B-UNDERLINE		PIC X(35) VALUE ALL "-".
*
	03  WS-LINE-2.
		05  FILLER			PIC X(15)  VALUE 'CENTRE '.
		05  WS-R-CENTRE-NO		PIC X(6).
		05  FILLER			PIC X(4)   VALUE SPACES.
		05  WS-R-CENTRE-NAME		PIC X(65).
*
	03  WS-LINE-3.
		05  WS-R-COURSE-NVQ-ID		PIC X(8).
		05  FILLER			PIC X(1)  VALUE SPACES.
		05  WS-R-COURSE-NAME		PIC X(84).
*
	03  WS-LINE-4.
		05  FILLER			PIC X(9)  VALUE SPACES.
		05  WS-R-AWARD-DESC		PIC X(90).
*
	03  WS-LINE-5.
		05  FILLER			PIC X(8)  VALUE 'REG.NO.'.
		05  FILLER			PIC X(37) VALUE 'NAME'.
		05  FILLER			PIC X(11) VALUE 'UCAS REF.'. 
		05  FILLER			PIC X(7)  VALUE 'EV NO.'.   
		05  FILLER			PIC X(26) VALUE 'EV NAME'.   
		05  FILLER			PIC X(24) VALUE 'REASON'.   
		05  FILLER			PIC X(9)  VALUE 'CLAIM'.   
		05  FILLER			PIC X(10) VALUE 'CLAIM DATE'.   
*
	03  WS-LINE-6.
		05  FILLER			PIC X(7)  VALUE ALL '-'.
                05  FILLER                      PIC X     VALUE SPACE.
		05  FILLER			PIC X(36) VALUE ALL '-'.
                05  FILLER                      PIC X     VALUE SPACE.
 		05  FILLER			PIC X(10) VALUE ALL '-'.
                05  FILLER                      PIC X     VALUE SPACE.
		05  FILLER			PIC X(6)  VALUE ALL '-'.
                05  FILLER                      PIC X     VALUE SPACE.
		05  FILLER			PIC X(25) VALUE ALL '-'.
                05  FILLER                      PIC X     VALUE SPACE.
		05  FILLER			PIC X(23) VALUE ALL '-'.
                05  FILLER                      PIC X     VALUE SPACE.
		05  FILLER			PIC X(8)  VALUE ALL '-'.
                05  FILLER                      PIC X     VALUE SPACE.
		05  FILLER			PIC X(10) VALUE ALL '-'.  
*
	03  WS-LINE-7.
		05  WS-R-REG-NO			PIC X(7).                 
                05  FILLER                      PIC X.                    
		05  WS-R-NAME-UCAS-REF		PIC X(47).                
                05  FILLER                      PIC X.                    
		05  WS-R-NPP-ID			PIC 9(6) BLANK WHEN ZERO.
                05  FILLER                      PIC X.                    
		05  WS-R-NPP-NAME		PIC X(25).                
                05  FILLER                      PIC X.                    
		05  WS-R-REASON   		PIC X(23).                
                05  FILLER                      PIC X.                    
		05  WS-R-CLAIM-TYPE		PIC X(8).                 
                05  FILLER                      PIC X.                    
		05  WS-R-CLAIM-DATE		PIC X(10).                
*
	03  WS-LINE-8A.
		05  FILLER			PIC X(16)  VALUE SPACES.
		05  FILLER          		PIC X(116) VALUE
                    "Explanation of reason:".
*
	03  WS-LINE-8B.
		05  FILLER			PIC X(16) VALUE SPACES.
		05  FILLER          		PIC X(50) VALUE
                    "EV DECISION:        The External Verifier has plac".
		05  FILLER          		PIC X(50) VALUE
                    "ed a block on certification for the qualification.".
		05  FILLER			PIC X(16) VALUE SPACES.
*
	03  WS-LINE-8C.
		05  FILLER			PIC X(16) VALUE SPACES.
		05  FILLER          		PIC X(50) VALUE
                    "NO EV REPORT:       According to Edexcel records t".
		05  FILLER          		PIC X(50) VALUE
                    "he External Verifier's report on the qualification".
		05  FILLER			PIC X(16) VALUE SPACES.
*
	03  WS-LINE-8D.
		05  FILLER			PIC X(16) VALUE SPACES.
		05  FILLER          		PIC X(50) VALUE
                    "                    or unit has not yet been recei".
		05  FILLER          		PIC X(50) VALUE
                    "ved or processed.                                 ".
		05  FILLER			PIC X(16) VALUE SPACES.
*
	03  WS-LINE-8E.
		05  FILLER			PIC X(16) VALUE SPACES.
		05  FILLER          		PIC X(50) VALUE
                    "CENTRE DEFERRED:    A block has been placed on all".
		05  FILLER          		PIC X(50) VALUE
                    " certification for the centre.                    ".
		05  FILLER			PIC X(16) VALUE SPACES.
*
	03  WS-LINE-8F.
		05  FILLER			PIC X(16) VALUE SPACES.
		05  FILLER          		PIC X(50) VALUE
                    "STUDENT DEFERRED:   A block has been placed on cer".
		05  FILLER          		PIC X(50) VALUE
                    "tification for the student.                       ".
		05  FILLER			PIC X(16) VALUE SPACES.
*
	03  WS-LINE-8G.
		05  FILLER			PIC X(16) VALUE SPACES.
		05  FILLER          		PIC X(50) VALUE
                    "Please refer to covering letter for further inform".
		05  FILLER          		PIC X(50) VALUE
                    "ation and advice.                                 ".
		05  FILLER			PIC X(16) VALUE SPACES.
/
PROCEDURE DIVISION.
*
A-CONTROL SECTION.
********************************************************************************
*									       *
*     THIS SECTION CONTROLS THE OVERALL LOGIC FLOW.			       *
*									       *
********************************************************************************
A-010.
*
* ACCEPT THE RUN TYPE....It can be either C - Cumulative (all un-cleared blocks)
*				       or N - Non-cumlative (new blocks)
*
	ACCEPT WS-RUN-TYPE.

	IF WS-RUN-TYPE NOT EQUAL "C" AND WS-RUN-TYPE NOT = "N"
        THEN
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       DISPLAY "*****                                            *****"
	       DISPLAY "*****  PROGRAM STP295:                           *****"
	       DISPLAY "*****  ERROR INVALID RUN TYPE 'A-CONTROL SECTION'*****"
	       DISPLAY "*****  MUST BE 'C'umulative or 'N'on-cumulative  *****"
	       DISPLAY "*****                                            *****"
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       STOP RUN
	END-IF.
*
	ACCEPT WS-SORT-ORDER.

	IF WS-SORT-ORDER NOT EQUAL "C" AND WS-SORT-ORDER NOT = "R"
        THEN
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       DISPLAY "*****                                            *****"
	       DISPLAY "*****  PROGRAM STP295:                           *****"
	       DISPLAY "*****  ERROR INVALID SORT ORDER A-CONTROL SECTION*****"
	       DISPLAY "*****  MUST BE 'C'entre or 'R'eason              *****"
	       DISPLAY "*****                                            *****"
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       STOP RUN
	END-IF.
*
	ACCEPT WS-LABELS-REQD.

	IF WS-LABELS-REQD NOT EQUAL "Y" AND WS-LABELS-REQD NOT = "N"
        THEN
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       DISPLAY "*****                                            *****"
	       DISPLAY "*****  PROGRAM STP295:                           *****"
	       DISPLAY "*****  ERROR INVALID LABELS REQUIRED PARAM       *****"
	       DISPLAY "*****  MUST BE 'Y'es or 'N'o                     *****"
	       DISPLAY "*****                                            *****"
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       STOP RUN
	END-IF.
*
	ACCEPT WS-UPDATE-BARB.

	IF WS-UPDATE-BARB NOT EQUAL "Y" AND WS-UPDATE-BARB NOT = "N"
        THEN
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       DISPLAY "*****                                            *****"
	       DISPLAY "*****  PROGRAM STP295:                           *****"
	       DISPLAY "*****  ERROR INVALID UPDATE BARB PARAMETER       *****"
	       DISPLAY "*****  MUST BE 'Y'es or 'N'o                     *****"
	       DISPLAY "*****                                            *****"
	       DISPLAY "******************************************************"
	       DISPLAY "******************************************************"
	       STOP RUN
	END-IF.
*
	PERFORM B-INITIALIZE.
*
	PERFORM D-PROCESS-REPORT.
*
        PERFORM E-UPDATE-BARB.
*
	PERFORM F-TERMINATE.
*
	STOP RUN.
*
A-999.
*
	EXIT.
/
B-INITIALIZE SECTION.
********************************************************************************
*									       *
*     THIS SECTION DECLARES THE REQUIRED CURSORS, OPENS THE OUTPUT FILES,      *
*	   CONNECTS TO ORACLE AND OPENS THE REQUIRED CURSORS.                  *
*									       *
********************************************************************************
B-010.
*
	PERFORM BB-OPEN-OUTPUT.
*
	PERFORM BC-CONNECT.
*
	PERFORM BD-OPEN-CURSORS.
*
	MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
	MOVE WS-CURRENT-DATE(1:8)  TO WS-DATE.
	MOVE CORRESPONDING WS-DATE TO WS-RUN-DATE.
*
        IF WS-SORT-ORDER = 'R'
        THEN
            MOVE '- ' TO WS-LINE-1-HYPHEN
            MOVE 0    TO WS-LINES-FOR-FOOTER
        ELSE
*           Sort order is C
            MOVE 8 TO WS-LINES-FOR-FOOTER
        END-IF.
*
B-999.
*
	EXIT.
/
BA-DECLARE-CURSORS SECTION.
********************************************************************************
*									       *
*     THIS SECTION DECLARES CURSOR_1.                          		       *
*									       *
********************************************************************************
BA-010.
*
*       EXEC SQL
*       	DECLARE CURSOR_1 CURSOR FOR
*       	select st_centre_id,
*       	       cn_centre_name,
*       	       nvl(st_course_id,nvl(st_nvq_registered_id
*       	                           ,st_gnvq_registered_id
*       	                           )
*       	          ),
*       	       nvl(ac_description
*                         ,decode(gnvq_glev_code
*       	                 ,null,decode(nl_code
*                                            ,null,'INDIVIDUAL UNITS'
*       	                             ,'NVQ '||nl_description
*       	                             )
*       	                 ,'GNVQ '||glev_description
*       	                 )
*       	          ),
*       	       nvl(at_name,nvl(nvq_title_1,gnvq_title_1)),
*       	       st_reg_no,
*       	       rpad(st_forenames||' '||st_surname
*                          ,47 - nvl(length(st_ucas_ref),0)
*                          ) || st_ucas_ref,
*       	       barb_npp_id,
*       	       npp_name,
*       	       decode(barb_reason_code
*       	             ,'1','CENTRE DEFERRED'
*       	             ,'2','STUDENT DEFERRED'
*       	             ,'3','CENTRE DEFERRED'
*       	             ,'4','NO EV REPORT'
*       	             ,'5','EV DECISION'
*                                ||decode(max(barb_decision_date)
*                                        ,null,null
*                                        ,' ('||to_char(max(barb_decision_date)
*                                                      ,'DD-MON-YY'
*                                                      ) || ')'
*                                        )
*       	             ,'6','QS INTERVENTION'
*       	             ,'9','AMR BLOCK'         
*       	             ,'UNKNOWN REASON'
*       	             ),
*       	       decode(min(barb_claim_type)
*                            ,'A','FULL'
*                            ,'F','FALLBACK'
*                            ,'I','INTERIM'
*                            ,'UNKNOWN'
*                            ),
*       	       to_char(trunc(barb_date),'DD-MON-YY'),
*                      barb_reason_code
*       	 from  students
*       	      ,centres
*       	      ,npps
*       	      ,nvqs
*       	      ,nvq_levels
*       	      ,gnvqs
*       	      ,gnvq_levels
*       	      ,approval_awards
*       	      ,approval_application
*       	      ,award_titles
*       	      ,award_codes
*       	      ,bnm_awards_run_blocks
*       	 where nvl(barb_cleared_ind,'N') = 'N'
*       	  and  st_reg_no                 = barb_reg_no
*       	  and  cn_centre_id              = st_centre_id
*       	  and  npp_id(+)                 = barb_npp_id
*       	  and  nvq_id(+)                 = st_nvq_registered_id
*       	  and  nl_code(+)                = nvq_level_code
*       	  and  gnvq_id(+)                = st_gnvq_registered_id
*       	  and  glev_code(+)              = gnvq_glev_code
*       	  and  aw_course_number(+)       = st_course_id
*       	  and  aa_applicat_no(+)         = aw_applicat_no
*       	  and  at_number(+)              = aa_btec_title
*       	  and  ac_code(+)                = aw_award_code
*                 and (:ws-run-type              = 'C'
*                  or  barb_report_ind           = 'Y'
*                     )
*       	  and st_delete is null
*       	group by st_centre_id
*       	        ,cn_centre_name
*       	        ,nvl(st_course_id,nvl(st_nvq_registered_id
*                                            ,st_gnvq_registered_id
*                                            )
*                           )
*       	        ,nvl(ac_description
*                           ,decode(gnvq_glev_code
*       	                   ,null,decode(nl_code
*                                              ,null,'INDIVIDUAL UNITS'
*       	                               ,'NVQ '||nl_description
*       	                               )
* 		                   ,'GNVQ '||glev_description
*       	                   )
*       	            )
*       	        ,nvl(at_name,nvl(nvq_title_1,gnvq_title_1))
*       	        ,st_reg_no
*       	        ,st_forenames
*       	        ,st_surname
*       	        ,st_ucas_ref
*       	        ,barb_npp_id
*       	        ,npp_name
*                       ,barb_reason_code
*       	        ,decode(barb_reason_code
*       	               ,'1','CENTRE DEFERRED'
*       	               ,'2','STUDENT DEFERRED'
*       	               ,'3','CENTRE DEFERRED'
* 		               ,'4','NO EV REPORT'
*   		               ,'5','EV DECISION'
*       	               ,'6','QS INTERVENTION'
*       	               ,'9','AMR BLOCK'         
*       	               ,'UNKNOWN REASON'
*       	               )
*       	        ,trunc(barb_date)
*               order by decode(:ws-sort-order,'R',barb_reason_code,'X'),
*                        st_centre_id,
*       	         nvl(st_course_id,nvl(st_nvq_registered_id
*       	                             ,st_gnvq_registered_id
*       	                             )
*       	            ),
*   		         st_reg_no
*       END-EXEC.
*
BA-999.
*
	EXIT.
/
BB-OPEN-OUTPUT SECTION.
********************************************************************************
*									       *
*     THIS SECTION OPENS THE OUTPUT FILES.				       *
*									       *
********************************************************************************
BB-010.
*
	OPEN OUTPUT PRINT-FILE.
*
        IF WS-LABELS-REQD = 'Y'
        THEN
   	  OPEN OUTPUT LABELS-FILE
        END-IF.
*
BB-999.
*
	EXIT.
/
BC-CONNECT SECTION.
********************************************************************************
*									       *
*     THIS SECTION CONNECTS TO ORACLE.					       *
*									       *
********************************************************************************
BC-010.
*
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO BC-050
*       END-EXEC.
*
*       EXEC SQL
*       	CONNECT :WS-USER-ID IDENTIFIED BY :WS-PASSWORD
*       END-EXEC.
        MOVE 10 TO SQL-ITERS
        MOVE 5 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-USER-ID
            SQL-SQHSTV(1)
        MOVE 6 TO SQL-SQHSTL(1)
        MOVE 6 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-PASSWORD
            SQL-SQHSTV(2)
        MOVE 4 TO SQL-SQHSTL(2)
        MOVE 4 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            SQL-SQHSTV(1)
            SQL-SQPHSV
        CALL "SQLADR" USING
            SQL-SQHSTL(1)
            SQL-SQPHSL
        CALL "SQLADR" USING
            SQL-SQHSTS(1)
            SQL-SQPHSS
        CALL "SQLADR" USING
            SQL-SQINDV(1)
            SQL-SQPIND
        CALL "SQLADR" USING
            SQL-SQINDS(1)
            SQL-SQPINS
        CALL "SQLADR" USING
            SQL-SQHARM(1)
            SQL-SQPARM
        CALL "SQLADR" USING
            SQL-SQHARC(1)
            SQL-SQPARC

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BC-050 END-IF.
*
	GO TO BC-999.
*
BC-050.
*
	MOVE WS-ERR-CONNECT TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
BC-999.
*
	EXIT.
/
BD-OPEN-CURSORS SECTION.
********************************************************************************
*									       *
*     THIS SECTION OPENS CURSOR_1.                                    	       *
*									       *
********************************************************************************
BD-010.
*
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO BD-050
*       END-EXEC.
*
*       EXEC SQL
*       	WHENEVER SQLWARNING
*       		CONTINUE
*       END-EXEC.
*
*       EXEC SQL OPEN CURSOR_1 END-EXEC.
        CALL "SQLADR" USING SQ0001 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 36 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        MOVE 1 TO SQL-SELERR
        MOVE 0 TO SQL-SQPMEM
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        MOVE 0 TO SQL-SQCMOD
        CALL "SQLADR" USING
            WS-RUN-TYPE
            SQL-SQHSTV(1)
        MOVE 1 TO SQL-SQHSTL(1)
        MOVE 1 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-SORT-ORDER
            SQL-SQHSTV(2)
        MOVE 1 TO SQL-SQHSTL(2)
        MOVE 1 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            SQL-SQHSTV(1)
            SQL-SQPHSV
        CALL "SQLADR" USING
            SQL-SQHSTL(1)
            SQL-SQPHSL
        CALL "SQLADR" USING
            SQL-SQHSTS(1)
            SQL-SQPHSS
        CALL "SQLADR" USING
            SQL-SQINDV(1)
            SQL-SQPIND
        CALL "SQLADR" USING
            SQL-SQINDS(1)
            SQL-SQPINS
        CALL "SQLADR" USING
            SQL-SQHARM(1)
            SQL-SQPARM
        CALL "SQLADR" USING
            SQL-SQHARC(1)
            SQL-SQPARC

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BD-050 END-IF.
*
	GO TO BD-999.
*
BD-050.
*
	MOVE WS-ERR-OPEN TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
BD-999.
*
	EXIT.
/
D-PROCESS-REPORT SECTION.
********************************************************************************
*									       *
*     THIS SECTION CONTROLS SELECTION OF AWARDS RUN BLOCKS.         	       *
*									       *
********************************************************************************
D-010.
*
	MOVE 'N' TO WS-END.
        PERFORM XA-FETCH-BLOCK.
	PERFORM DA-FETCH-BLOCKS UNTIL END-OF-FETCH.
*
        IF WS-SORT-ORDER = 'C'
        THEN
            PERFORM ZC-PAGE-FOOTER
        END-IF.
*
D-999.
*
	EXIT.
/
DA-FETCH-BLOCKS SECTION.
********************************************************************************
*									       *
*     THIS SECTION OBTAINS AWARDS RUN BLOCKS.               		       *
*									       *
********************************************************************************
DA-010.
*
        IF WS-LABELS-REQD = 'Y' AND WS-CENTRE-NO NOT EQUAL WS-OLD-CENTRE-NO
        THEN
           MOVE WS-CENTRE-NO TO L-CENTRE-NO
           WRITE LABELS-REC
        END-IF.
*
        IF WS-SORT-ORDER = 'C'
        THEN
            MOVE 0 TO WS-PAGE-COUNT
        ELSE
            IF WS-REASON-CODE NOT EQUAL WS-OLD-REASON-CODE
            THEN
              IF WS-REASON-CODE = '5'
              THEN
                MOVE WS-REASON(1:11) TO WS-LINE-1-REASON
              ELSE
                MOVE WS-REASON TO WS-LINE-1-REASON
              END-IF
              MOVE 0 TO WS-PAGE-COUNT
            END-IF
        END-IF.
*
	MOVE WS-CENTRE-NAME TO WS-R-CENTRE-NAME.
	MOVE WS-CENTRE-NO   TO WS-R-CENTRE-NO
			       WS-OLD-CENTRE-NO.
        MOVE WS-REASON-CODE TO WS-OLD-REASON-CODE.
*
        PERFORM ZA-NEW-PAGE.
*
        IF WS-SORT-ORDER = 'C'
        THEN
	    PERFORM DAA-PROCESS-CENTRE UNTIL END-OF-FETCH
			 	OR WS-CENTRE-NO   NOT EQUAL WS-OLD-CENTRE-NO
        ELSE
	    PERFORM DAA-PROCESS-CENTRE UNTIL END-OF-FETCH
                                OR WS-REASON-CODE NOT EQUAL WS-OLD-REASON-CODE
			 	OR WS-CENTRE-NO   NOT EQUAL WS-OLD-CENTRE-NO
        END-IF.
*
DA-999.
*
	EXIT.
/
DAA-PROCESS-CENTRE SECTION.
********************************************************************************
*									       *
*     THIS SECTION PRODUCES THE REPORT FOR EACH CENTRE/DEFERRAL REASON.        *
*	   A LABEL RECORD IS ALSO CREATED FOR EACH CENTRE.		       *
*									       *
********************************************************************************
DAA-010.
*
        MOVE WS-COURSE-NVQ-ID TO WS-OLD-COURSE-NVQ-ID.
*
        IF WS-LINECOUNT > 43 - WS-LINES-FOR-FOOTER
        THEN
           PERFORM ZA-NEW-PAGE
           PERFORM ZB-COURSE-HEADINGS
        ELSE
           PERFORM ZB-COURSE-HEADINGS
        END-IF.
*
	PERFORM DAAA-PROCESS-COURSE UNTIL END-OF-FETCH
			  OR WS-OLD-CENTRE-NO     NOT EQUAL WS-CENTRE-NO
			  OR WS-OLD-COURSE-NVQ-ID NOT EQUAL WS-COURSE-NVQ-ID.
*
DAA-999.
*
	EXIT.
/
DAAA-PROCESS-COURSE SECTION.
********************************************************************************
*									       *
*     THIS SECTION OBTAINS ALL BLOCKS FOR A COURSE.			       *
*									       *
********************************************************************************
DAAA-010.
*
	MOVE WS-REG-NO        TO WS-R-REG-NO.
	MOVE WS-NAME-UCAS-REF TO WS-R-NAME-UCAS-REF.
        MOVE WS-NPP-ID        TO WS-R-NPP-ID.
        MOVE WS-NPP-NAME      TO WS-R-NPP-NAME.
        MOVE WS-REASON        TO WS-R-REASON.
        MOVE WS-CLAIM-TYPE    TO WS-R-CLAIM-TYPE.
        MOVE WS-CLAIM-DATE    TO WS-R-CLAIM-DATE.
*
        IF WS-LINECOUNT > 51 - WS-LINES-FOR-FOOTER
        THEN
          PERFORM ZA-NEW-PAGE
          PERFORM ZB-COURSE-HEADINGS
        END-IF.
*
	MOVE WS-LINE-7 TO PRINT-REC.
	WRITE PRINT-REC.
	ADD 1 TO WS-LINECOUNT.
*
DAAA-100.
*
	PERFORM XA-FETCH-BLOCK.
*
DAAA-999.
*
	EXIT.
/
E-UPDATE-BARB SECTION.
********************************************************************************
*									       *
*     THIS SECTION UPDATES THE BARB TABLE.				       *
*									       *
********************************************************************************
E-010.
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO E-030
*       END-EXEC.
*
*       EXEC SQL
*       	WHENEVER SQLWARNING
*       		CONTINUE
*       END-EXEC.
*
*       EXEC SQL
*       	WHENEVER NOT FOUND
*       		CONTINUE
*       END-EXEC.
*
* ONLY UPDATE BARB IF PARAM SAYS YES.
*
        IF WS-UPDATE-BARB = 'Y'
*          EXEC SQL
*               UPDATE  BNM_AWARDS_RUN_BLOCKS
*               SET     BARB_REPORT_IND = 'N'
*               WHERE   BARB_REPORT_IND = 'Y'
*          END-EXEC
           CALL "SQLADR" USING SQ0002 SQL-STMT
           MOVE 1 TO SQL-ITERS
           MOVE 59 TO SQL-OFFSET
           MOVE 0 TO SQL-OCCURS
           CALL "SQLADR" USING
               SQLCUD
               SQL-CUD
           CALL "SQLADR" USING
               SQLCA
               SQL-SQLEST
           MOVE 4352 TO SQL-SQLETY

           CALL "SQLBEX" USING
               SQLCTX
               SQLEXD
               SQLFPN

           IF SQLCODE IN SQLCA IS LESS THAN 0
               THEN GO TO E-030 END-IF
        END-IF.
*
	GO TO E-999.
*
E-030.
	MOVE 'BNM_AWARDS_RUN_BLOCKS UPDATE FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
E-999.
*
	EXIT.
/
F-TERMINATE SECTION.
********************************************************************************
*									       *
*     THIS SECTION TERMINATES THE PROGRAM ON SUCCESFUL COMPLETION.	       *
*									       *
********************************************************************************
F-010.
*
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO F-050
*       END-EXEC.
*
*       EXEC SQL CLOSE CURSOR_1 END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 74 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO F-050 END-IF.
*
*       EXEC SQL
*       	COMMIT WORK
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 89 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO F-050 END-IF.
*
	GO TO F-100.
F-050.
*
	MOVE WS-ERR-CLOSE TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
F-100.
	CLOSE PRINT-FILE.
*
        IF WS-LABELS-REQD = 'Y'
        THEN
	  CLOSE LABELS-FILE
        END-IF.
*
F-999.
*
	EXIT.
/
XA-FETCH-BLOCK SECTION.
********************************************************************************
*									       *
*     THIS SECTION OBTAINS AN AWARDS RUN BLOCK.               		       *
*									       *
********************************************************************************
XA-010.
*
	INITIALIZE	WS-CENTRE-NO,
			WS-CENTRE-NAME,
			WS-COURSE-NVQ-ID,
			WS-AC-DESC,
			WS-AT-NAME,
			WS-REG-NO,
			WS-NAME-UCAS-REF,
                        WS-NPP-ID,
                        WS-NPP-NAME,
                        WS-REASON,
                        WS-CLAIM-TYPE,
                        WS-CLAIM-DATE.
*
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO XA-060
*       END-EXEC.
*
*       EXEC SQL
*       	WHENEVER SQLWARNING
*       		CONTINUE
*       END-EXEC.
*
*       EXEC SQL
*       	WHENEVER NOT FOUND
*       		GO TO XA-030
*       END-EXEC.
*
*       EXEC SQL
*           FETCH CURSOR_1 INTO :WS-CENTRE-NO,
*       		        :WS-CENTRE-NAME,
*       			:WS-COURSE-NVQ-ID,
*               		:WS-AC-DESC,
*       	        	:WS-AT-NAME,
*       		        :WS-REG-NO,
*       			:WS-NAME-UCAS-REF,
*                               :WS-NPP-ID,
*                               :WS-NPP-NAME,
*                               :WS-REASON,
*                               :WS-CLAIM-TYPE,
*                               :WS-CLAIM-DATE,
*                               :WS-REASON-CODE
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 104 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        MOVE 1 TO SQL-SELERR
        MOVE 0 TO SQL-SQPMEM
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        MOVE 0 TO SQL-SQFOFF
        MOVE 2 TO SQL-SQFMOD
        CALL "SQLADR" USING
            WS-CENTRE-NO
            SQL-SQHSTV(1)
        MOVE 6 TO SQL-SQHSTL(1)
        MOVE 6 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-CENTRE-NAME
            SQL-SQHSTV(2)
        MOVE 65 TO SQL-SQHSTL(2)
        MOVE 65 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-COURSE-NVQ-ID
            SQL-SQHSTV(3)
        MOVE 8 TO SQL-SQHSTL(3)
        MOVE 8 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-AC-DESC
            SQL-SQHSTV(4)
        MOVE 84 TO SQL-SQHSTL(4)
        MOVE 84 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-AT-NAME
            SQL-SQHSTV(5)
        MOVE 90 TO SQL-SQHSTL(5)
        MOVE 90 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(6)
        MOVE 7 TO SQL-SQHSTL(6)
        MOVE 7 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-NAME-UCAS-REF
            SQL-SQHSTV(7)
        MOVE 51 TO SQL-SQHSTL(7)
        MOVE 51 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-NPP-ID
            SQL-SQHSTV(8)
        MOVE 9 TO SQL-SQHSTL(8)
        MOVE 9 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            WS-NPP-NAME
            SQL-SQHSTV(9)
        MOVE 40 TO SQL-SQHSTL(9)
        MOVE 40 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            WS-REASON
            SQL-SQHSTV(10)
        MOVE 25 TO SQL-SQHSTL(10)
        MOVE 25 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            WS-CLAIM-TYPE
            SQL-SQHSTV(11)
        MOVE 8 TO SQL-SQHSTL(11)
        MOVE 8 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            WS-CLAIM-DATE
            SQL-SQHSTV(12)
        MOVE 9 TO SQL-SQHSTL(12)
        MOVE 9 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            WS-REASON-CODE
            SQL-SQHSTV(13)
        MOVE 1 TO SQL-SQHSTL(13)
        MOVE 1 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            SQL-SQHSTV(1)
            SQL-SQPHSV
        CALL "SQLADR" USING
            SQL-SQHSTL(1)
            SQL-SQPHSL
        CALL "SQLADR" USING
            SQL-SQHSTS(1)
            SQL-SQPHSS
        CALL "SQLADR" USING
            SQL-SQINDV(1)
            SQL-SQPIND
        CALL "SQLADR" USING
            SQL-SQINDS(1)
            SQL-SQPINS
        CALL "SQLADR" USING
            SQL-SQHARM(1)
            SQL-SQPARM
        CALL "SQLADR" USING
            SQL-SQHARC(1)
            SQL-SQPARC

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO XA-030 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO XA-060 END-IF.
*
	GO TO XA-999.
*
XA-030.
*
	MOVE 'Y' TO WS-END.
	GO TO XA-999.
*
XA-060.
*
	MOVE WS-ERR-FETCH TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
XA-999.
*
	EXIT.
/
ZA-NEW-PAGE SECTION.
********************************************************************************
*									       *
*     THIS SECTION PRINTS THE STUDENT REGISTRATION REPORT HEADINGS.	       *
*									       *
********************************************************************************
ZA-010.
*
        IF WS-SORT-ORDER = 'C' AND WS-LINECOUNT > 0
        THEN
            PERFORM ZC-PAGE-FOOTER
        END-IF.
*
	ADD +1 TO WS-PAGE-COUNT.
	MOVE WS-PAGE-COUNT TO WS-PAGE-NUMBER.
*
	MOVE WS-LINE-1 TO PRINT-REC.
	WRITE PRINT-REC AFTER PAGE.
*
	MOVE WS-LINE-1B TO PRINT-REC.
	WRITE PRINT-REC.
        MOVE 2 TO WS-LINECOUNT.
*
        PERFORM ZAA-CENTRE-HEADINGS.
*
ZA-999.
*
	EXIT.
/
ZAA-CENTRE-HEADINGS SECTION.
********************************************************************************
*									       *
*     THIS SECTION PRINTS CENTRE HEADING DETAILS.			       *
*									       *
********************************************************************************
ZAA-010.
*
        MOVE WS-CENTRE-NO   TO WS-R-CENTRE-NO.
        MOVE WS-CENTRE-NAME TO WS-R-CENTRE-NAME.
*
	MOVE WS-LINE-2 TO PRINT-REC.
	WRITE PRINT-REC AFTER 2.
*
        ADD 2 TO WS-LINECOUNT.
*
ZAA-999.
*
	EXIT.
/
ZB-COURSE-HEADINGS SECTION.
********************************************************************************
*									       *
*     THIS SECTION PRINTS COURSE HEADINGS DETAILS.			       *
*									       *
********************************************************************************
ZB-010.
*
        MOVE WS-COURSE-NVQ-ID TO WS-R-COURSE-NVQ-ID.
        MOVE WS-AC-DESC       TO WS-R-COURSE-NAME.
	MOVE WS-LINE-3 TO PRINT-REC.
	WRITE PRINT-REC AFTER 3.
*
        IF WS-AT-NAME NOT = SPACES
        THEN
          MOVE WS-AT-NAME TO WS-R-AWARD-DESC
	  MOVE WS-LINE-4 TO PRINT-REC
	  WRITE PRINT-REC
          ADD 1 TO WS-LINECOUNT
        END-IF.
*
        MOVE WS-LINE-5 TO PRINT-REC.
        WRITE PRINT-REC AFTER 2.
*
        MOVE WS-LINE-6 TO PRINT-REC.
        WRITE PRINT-REC.
*
        ADD 6 TO WS-LINECOUNT.
*
ZB-999.
*
	EXIT.
/
ZC-PAGE-FOOTER SECTION.
********************************************************************************
*									       *
*     THIS SECTION PRINTS INFORMATIONAL LINES AT THE BOTTOM OF THE REPORT      *
*									       *
********************************************************************************
ZC-010.
*
	MOVE WS-LINE-8A TO PRINT-REC.
        SUBTRACT WS-LINECOUNT FROM 46 GIVING WS-LINE-SKIP.
	WRITE PRINT-REC AFTER WS-LINE-SKIP.
*
	MOVE WS-LINE-8B TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE WS-LINE-8C TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE WS-LINE-8D TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE WS-LINE-8E TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE WS-LINE-8F TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE WS-LINE-8G TO PRINT-REC.
	WRITE PRINT-REC.
*
ZC-999.
*
	EXIT.
/
ZZ-ABORT SECTION.
********************************************************************************
*									       *
*     THIS SECTION ABORTS THE PROGRAM WITH AN APPROPRIATE ERROR MESSAGE.       *
*									       *
********************************************************************************
ZZ-010.
*
	MOVE SQLCODE TO WS-SQLCODE-X.
	MOVE WS-SQLCODE-X TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE SQLERRM TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE WS-ERR-MESSAGE TO PRINT-REC.
	WRITE PRINT-REC.
*
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO ZZ-050
*       END-EXEC.
*
*       EXEC SQL CLOSE CURSOR_1 END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 171 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO ZZ-050 END-IF.
*
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO ZZ-100
*       END-EXEC.
*
*       EXEC SQL
*       	ROLLBACK WORK
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 186 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO ZZ-100 END-IF.
*
	MOVE 'THE PROGRAM ABORTED' TO PRINT-REC.
	WRITE PRINT-REC.
*
	CLOSE PRINT-FILE.
        IF WS-LABELS-REQD = 'Y'
        THEN
	  CLOSE LABELS-FILE
        END-IF.
	STOP RUN.
*
ZZ-050.
*
	MOVE 'CLOSE CURSOR FAILED DURING PROGRAM ABORT' TO WS-ERR-MESSAGE.
*
	MOVE WS-ERR-MESSAGE TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE 'THE PROGRAMME ABORTED' TO PRINT-REC.
	WRITE PRINT-REC.
*
	CLOSE PRINT-FILE.
        IF WS-LABELS-REQD = 'Y'
        THEN
	  CLOSE LABELS-FILE
        END-IF.
	STOP RUN.
*
ZZ-100.
*
	MOVE 'ROLLBACK FAILED DURING PROGRAM ABORT' TO WS-ERR-MESSAGE.
*
	MOVE WS-ERR-MESSAGE TO PRINT-REC.
	WRITE PRINT-REC.
*
	MOVE 'THE PROGRAMME ABORTED' TO PRINT-REC.
	WRITE PRINT-REC.
*
	CLOSE PRINT-FILE.
        IF WS-LABELS-REQD = 'Y'
        THEN
	  CLOSE LABELS-FILE
        END-IF.
	STOP RUN.
*
ZZ-999.
*
	EXIT.
