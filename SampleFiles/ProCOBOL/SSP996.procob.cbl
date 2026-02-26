IDENTIFICATION DIVISION.
PROGRAM-ID.   SSP996.
AUTHOR.       MARZENA BREWSTER.
DATE-WRITTEN. 06-OCT-2010.
********************************************************************************
*
*	OFQUAL QUARTERLY RETURN (ISSUED AWARDS + REGNS)
*
********************************************************************************
*
*       SSP996 - Create achieve files for OFQUAL (
*		This is the 'other qual' data feed for Bath (megwen)
*               and includes open registrations between 2 dates, 996 is
*               the same except for student name is blanked out
*	
*
*	the following nqf grade coding is used......
*       	D
*      		DD
*     		DDD*
*    		DDM*
*      		DM
*   		DMM*
*      		M
*     		MM
*       	MMM*
*      		MMP*
*      		MP
*      		MPP*
*     		P
*      		PP
*       	PPP*
*        	U
*
*  * 3 character codes are decoded into the following 2 character codes (used in the file)
*  						    'DDD' = 'D1' 
*                                        'DDM' = 'D2' 
*                                        'DMM' = 'D3'   
*                                        'MMM' = 'M1' 
*                                        'MMP' = 'M2' 
*                                        'MPP' = 'M3' 
*                                        'PPP' = 'P1'    
* 03-MAY-2011   MP      GA an GB expecting grade P not our stored grade
*                       Suppress output without qan codes
* 5-AUG-2011    MP      for award codes ('FS','FO') pick up
*                       qan code from different table - only works once level
*                       achieved is known +
* 24-JUL-2014	CDS	WI947 : NG Results.
* 06-Aug-2014	CDS	WI947 : NG Results - Add DAUD.
* 21-Nov-2014	CDS	WI947 : NG Results - Add Cert Block Check.
* 10-SEP-2015   MBC     EC2164 : BLOCKING total to exclude blocked
* 12-DEC-2016	CDS	EC2439 : Stop ABS/KSQ00 reporting.
*				 Populate table OFQUAL_DETAILS to produce new format Quarterly OfQual Report.
* 05-DEC-2017   CDS     EC2608 : Remove grading date parameter.
*                       Now calculated by function FN_FIRST_ENTRY_DATE
* 02-Mar-2020   SN      EC4363 - Tech Embargo Changes on Grades 
*                      (Specific to BTEC Courses  - Y6(Award Code))
**********************************************************************
*  slightly different format 
**********************************************************************
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. VAX-11.
OBJECT-COMPUTER. VAX-11.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT  TOTALS-REP  ASSIGN SSP996TOT.
    SELECT  DATA-FILE-B ASSIGN SSP996B.
    SELECT  DATA-FILE-G ASSIGN SSP996G.
    SELECT  DATA-FILE-N ASSIGN SSP996N.
*
DATA DIVISION.
FILE SECTION.
*
FD  TOTALS-REP
    LABEL RECORDS STANDARD.
01  TOT-RECORD             	  PIC X(132).
*
FD  DATA-FILE-B
    LABEL RECORDS STANDARD.
01  DB-DETAIL-RECORD              PIC X(336).
*
FD  DATA-FILE-G
    LABEL RECORDS STANDARD.
01  DG-DETAIL-RECORD              PIC X(336).
*
FD  DATA-FILE-N
    LABEL RECORDS STANDARD.
01  DN-DETAIL-RECORD              PIC X(336).
*
WORKING-STORAGE SECTION.
*                                               
01  SQLFPN GLOBAL.
           02  SQLFPN-FILE-LEN PIC S9(4) COMP-5 VALUE +37.
           02  SQLFPN-FILENAME PIC X(37) VALUE "/work/SampleFiles/ProCOBOL/SSP996.pco".

01  SQ0001 GLOBAL.
           02  FILLER PIC X(60) VALUE "select TO_CHAR(SYSDATE,'DD-MON-YYYY')   into :b1  from DUAL ".

01  SQ0002 GLOBAL.
           02  FILLER PIC X(85) VALUE "select TO_CHAR(FN_FIRST_ENTRY_DATE(TRUNC(SYSDATE)),'DDMMYYYY')   into :b1  from DUAL ".

01  SQ0003 GLOBAL.
           02  FILLER PIC X(29) VALUE "truncate TABLE OFQUAL_DETAILS".

01  SQ0007 GLOBAL.
           02  FILLER PIC X(82) VALUE "select NVL(FN_GET_VOCATIONAL_GUEST_CENTRE(:b1,:b2,:b3),:b4)   into :b5  from DUAL ".

01  SQ0009 GLOBAL.
           02  FILLER PIC X(75) VALUE "update STUDENT_NG_STATS  set SNST_FEED_4_GRADE=:b1 where SNST_ST_REG_NO=:b2".

01  SQ0010 GLOBAL.
           02  FILLER PIC X(215) VALUE "insert into STUDENT_NG_STATS(SNST_ST_REG_NO,SNST_ORIGIN,SNST_FEED_4_GRADE,SNST_INSERT_DATE)s"&
    "elect :b1  ,4  ,:b2  ,SYSDATE   from DUAL where  not exists (select null    from STUDENT_NG_STATS where SNST_ST_REG_NO=:b1)".

01  SQ0011 GLOBAL.
           02  FILLER PIC X(47) VALUE "select DAUD_SEQ.nextval    into :b1  from DUAL ".

01  SQ0012 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into DATAFEED_AUDITS(DAUD_SEQUENCE,DAUD_MODULE,DAUD_MODULE_DESCR,DAUD_START,DAUD_END,"&
    "DAUD_AWARDED_FROM,DAUD_AWARDED_TO,DAUD_ACADEMIC_YEAR,DAUD_BTEC_NG_GRADING_DATE,DAUD_REGNS_FROM,DAUD_REGNS_TO,DAUD_OFQUAL_FROM,"&
    "DAUD_OFQUAL_TO) values (:b1,'SSD996','".

           02  FILLER PIC X(78) VALUE "OFQUAL',SYSDATE,null ,:b2,:b3,:b4,TO_DATE(:b5,'DDMMYYYY'),:b2,:b3,null ,null )".

01  SQ0014 GLOBAL.
           02  FILLER PIC X(68) VALUE "update DATAFEED_AUDITS  set DAUD_END=SYSDATE where DAUD_SEQUENCE=:b1".

01  SQ0015 GLOBAL.
           02  FILLER PIC X(93) VALUE "begin  PK_OSCA1 . PR_Q_CERT_STATUS ( :b1 , NULL , :b2:i2 , :b3:i3 , :b4:i4 , :b5:i5 ) ; END ;".

01  SQ0016 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into OFQUAL_DETAILS(ODET_ID,ODET_DAUD_ID,ODET_REG_NO,ODET_REG_TYPE,ODET_BTEC_NG,ODET_"&
    "COURSE,ODET_REG_DATE,ODET_ISSUE_DATE,ODET_ELIG,ODET_OFQUAL_GRADE) values (ODET_SEQ.nextval ,:b1,:b2,:b3,DECODE(:b4,'G','Y','N'"&
    "),:b5,TO_DATE(:b6,'DDMMYYYY'),TO_DATE(".

           02  FILLER PIC X(24) VALUE ":b7,'DDMMYYYY'),:b8,:b9)".

01  SQ0017 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into OFQUAL_DETAILS(ODET_ID,ODET_DAUD_ID,ODET_REG_NO,ODET_REG_TYPE,ODET_BTEC_NG,ODET_"&
    "COURSE,ODET_REG_DATE,ODET_ISSUE_DATE,ODET_ELIG,ODET_OFQUAL_GRADE) values (ODET_SEQ.nextval ,:b1,:b2,:b3,'N',:b4,TO_DATE(:b5,'D"&
    "DMMYYYY'),TO_DATE(:b6,'DDMMYYYY'),:b7,".

           02  FILLER PIC X(4) VALUE ":b8)".

01  SQLCTX GLOBAL PIC S9(9) COMP-5 VALUE +655389365.


01  SQLEXD GLOBAL.
           02  SQL-SQLVSN   PIC S9(18) COMP-5 VALUE +13.
           02  SQL-ARRSIZ   PIC S9(9) COMP-5 VALUE +23.
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
           02  SQL-SQHSTV   PIC S9(18) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQHSTL   PIC S9(18) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQHSTS   PIC S9(18) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQINDV   PIC S9(18) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQINDS   PIC S9(18) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQHARM   PIC S9(18) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQHARC   PIC S9(18) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQADTO   PIC S9(4) COMP-5 OCCURS 23 TIMES.
           02  SQL-SQTDSO   PIC S9(4) COMP-5 OCCURS 23 TIMES.


01  SQ0004 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ST_REG_NO  ,'                                   '  ,'                                "&
    "   '  ,ST_COURSE_ID  ,TO_CHAR(ST_REG_DATE,'DDMMYYYY')  ,((AW_AWARD_CODE||AA_BTEC_TITLE)||'XX')  ,AT_NAME  ,TO_CHAR(ST_AWARD_IS"&
    "SUE,'DDMMYYYY')  ,ST_CENTRE_ID  ,NVL(T".

           02  FILLER PIC X(256) VALUE "O_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        ')  ,TO_CHAR(ST_BIRTH_DATE,'YYYY')  ,DECODE(ST_SEX"&
    ",'F','F','M','M','U')  ,'3'  ,ST_ULN  ,DECODE(ST_AWARD_ISSUE,null ,DECODE(ST_WITHDRAWN_IND,'Y',null ,null ),DECODE(ST_FALLBACK"&
    ",'Y','U',NVL(ST_OVER_GRADE,'P  ')))  S".

           02  FILLER PIC X(256) VALUE "OVER ,AA_BTEC_TITLE  ,'  '  ,'Pearson'  ,'  '  ,' '  ,DECODE(AC_BNM_TYPE,'G','G','B')  ,'Y' "&
    " ,ST_AWARD_ELIG   from APPROVAL_APPLICATION ,APPROVAL_AWARDS ,AWARD_CODES ,AWARD_TITLES ,STUDENTS X where ((((((((((AW_AWARD_C"&
    "ODE in ('01','02','03','04','05','06',".

           02  FILLER PIC X(256) VALUE "'11') and ST_DELETE is null ) and ST_FALLBACK is null ) and (ST_COURSE_ID||'')=AW_COURSE_NUM"&
    "BER) and AA_APPLICAT_NO=(AW_APPLICAT_NO+0)) and ST_UNIVERSITY_IND is null ) and AC_CODE=AW_AWARD_CODE) and TO_NUMBER(AA_BTEC_T"&
    "ITLE)=AT_NUMBER) and (ST_AWARD_ISSUE b".

           02  FILLER PIC X(256) VALUE "etween TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR') or (ST_REG_DATE between TO_"&
    "DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR') and ST_AWARD_ISSUE is null ))) and  not exists (select null    from AWA"&
    "RD_CODES ,APPROVAL_AWARDS ,STUDENTS wh".

           02  FILLER PIC X(256) VALUE "ere (((((AC_CODE=AW_AWARD_CODE and AW_COURSE_NUMBER=ST_COURSE_ID) and ST_REG_NO=X.ST_REG_NO)"&
    " and AC_BNM_TYPE='G') and ST_AWARD_ISSUE is null ) and ST_BNM_GRADE is  not null ))) and ( not exists (select null    from STU"&
    "DENT_NG_STATS where SNST_ST_REG_NO=ST_".

           02  FILLER PIC X(256) VALUE "REG_NO) or exists (select null    from STUDENT_NG_STATS where (SNST_ST_REG_NO=ST_REG_NO and "&
    "SNST_FEED_4_GRADE is null )))) union select ST_REG_NO  ,'                                   '  ,'                             "&
    "      '  ,ST_COURSE_ID  ,TO_CHAR(ST_RE".

           02  FILLER PIC X(256) VALUE "G_DATE,'DDMMYYYY')  ,NVL(AACO_QCA_CODE,((AW_AWARD_CODE||AA_BTEC_TITLE)||'XX'))  ,AT_NAME  ,T"&
    "O_CHAR(ST_AWARD_ISSUE,'DDMMYYYY')  ,ST_CENTRE_ID  ,NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        ')  ,TO_CHAR(ST_BIRTH_DATE,'"&
    "YYYY')  ,NVL(ST_SEX,'9')  ,'3'  ,ST_UL".

           02  FILLER PIC X(256) VALUE "N  ,DECODE(TO_CHAR(ST_AWARD_ISSUE),null ,DECODE(ST_WITHDRAWN_IND,'Y',null ,null ),DECODE(ST_"&
    "FALLBACK,'Y','U',NVL(NVL(ST_OVER_GRADE,ST_BNM_GRADE),'P  ')))  SOVER ,AA_BTEC_TITLE  ,'  '  ,'Pearson'  ,'  '  ,' '  ,DECODE(A"&
    "C_BNM_TYPE,'G','G','B')  ,'Y'  ,ST_AWA".

           02  FILLER PIC X(256) VALUE "RD_ELIG   from APPROVAL_APPLICATION ,APPROVAL_AWARDS ,AWARD_CODES ,AWARD_TITLES ,AT_AWARD_CO"&
    "DES ,STUDENTS X where ((((((((((((AW_AWARD_CODE not  in ('01','02','03','04','05','06','11','27','64','35','63','93','HH','GG'"&
    ",'GA','GB','CC','EW','FS','FO') and ST".

           02  FILLER PIC X(256) VALUE "_DELETE is null ) and ST_FALLBACK is null ) and AW_AWARD_CODE=AACO_AC_CODE) and AC_CODE=AW_A"&
    "WARD_CODE) and AT_NUMBER=AACO_AT_NUMBER) and (ST_COURSE_ID||'')=AW_COURSE_NUMBER) and AA_APPLICAT_NO=(AW_APPLICAT_NO+0)) and T"&
    "O_NUMBER(AA_BTEC_TITLE)=AT_NUMBER) and".

           02  FILLER PIC X(256) VALUE " ST_UNIVERSITY_IND is null ) and (ST_AWARD_ISSUE between TO_DATE(:b1,'DD-MON-RRRR') and TO_D"&
    "ATE(:b2,'DD-MON-RRRR') or (ST_REG_DATE between TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR') and ST_AWARD_ISSUE is"&
    " null ))) and  not exists (select null".

           02  FILLER PIC X(256) VALUE "    from AWARD_CODES ,APPROVAL_AWARDS ,STUDENTS where (((((AC_CODE=AW_AWARD_CODE and AW_COUR"&
    "SE_NUMBER=ST_COURSE_ID) and ST_REG_NO=X.ST_REG_NO) and AC_BNM_TYPE='G') and ST_AWARD_ISSUE is null ) and ST_BNM_GRADE is  not "&
    "null ))) and ( not exists (select null".

           02  FILLER PIC X(256) VALUE "    from STUDENT_NG_STATS where SNST_ST_REG_NO=ST_REG_NO) or exists (select null    from STU"&
    "DENT_NG_STATS where (SNST_ST_REG_NO=ST_REG_NO and SNST_FEED_4_GRADE is null )))) union select ST_REG_NO  ,'                   "&
    "                '  ,'                 ".

           02  FILLER PIC X(256) VALUE "                  '  ,ST_COURSE_ID  ,TO_CHAR(ST_REG_DATE,'DDMMYYYY')  ,NVL(ACPA_QCA_LU_X_COD"&
    "E,((AW_AWARD_CODE||AA_BTEC_TITLE)||'XX'))  ,AT_NAME  ,TO_CHAR(ST_AWARD_ISSUE,'DDMMYYYY')  ,ST_CENTRE_ID  ,NVL(TO_CHAR(ST_BIRTH"&
    "_DATE,'DDMMYYYY'),'        ')  ,TO_CHA".

           02  FILLER PIC X(256) VALUE "R(ST_BIRTH_DATE,'YYYY')  ,NVL(ST_SEX,'9')  ,'3'  ,'  '  ,DECODE(ST_OVER_GRADE,null ,DECODE(S"&
    "T_WITHDRAWN_IND,'Y',' ',DECODE(AW_AWARD_CODE,'63',DECODE(ST_AWARD_ISSUE,null ,null ,'P'),' ')),DECODE(ST_AWARD_ISSUE,null ,' '"&
    ",DECODE(ST_FALLBACK,'Y','U',('E'||NVL(".

           02  FILLER PIC X(256) VALUE "ST_OVER_GRADE, replace(ST_ABS_LEVEL_ACHIEVED,'E',null ))))))  ,AA_BTEC_TITLE  ,'  '  ,'Pears"&
    "on'  ,DECODE(NVL(ST_OVER_GRADE,ST_ABS_LEVEL_ACHIEVED),null ,'XX',('E'||NVL(ST_OVER_GRADE, replace(ST_ABS_LEVEL_ACHIEVED,'E',nu"&
    "ll ))))  ,' '  ,'O'  ,'Y'  ,ST_AWARD_E".

           02  FILLER PIC X(256) VALUE "LIG   from STUDENTS ,APPROVAL_AWARDS ,APPROVAL_APPLICATION ,APPROVAL_COMB_PACKAGES ,AWARD_TI"&
    "TLES where (((((((AA_APPLICAT_NO=AW_APPLICAT_NO and ST_COURSE_ID=AW_COURSE_NUMBER) and AW_AWARD_CODE in ('35','63','HH')) and "&
    "AA_BTEC_TITLE=AT_NUMBER) and ST_DELETE".

           02  FILLER PIC X(256) VALUE " is null ) and ST_FALLBACK is null ) and ACPA_AT_NUMBER=AT_NUMBER) and (ST_AWARD_ISSUE betwe"&
    "en TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR') or (ST_REG_DATE between TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b"&
    "2,'DD-MON-RRRR') and ST_AWARD_ISSUE is".

           02  FILLER PIC X(256) VALUE " null ))) union select ST_REG_NO  ,'                                   '  ,'                "&
    "                   '  ,ST_COURSE_ID  ,TO_CHAR(ST_REG_DATE,'DDMMYYYY')  ,AACL_QCA_CODE  ,AT_NAME  ,TO_CHAR(ST_AWARD_ISSUE,'DDMM"&
    "YYYY')  ,ST_CENTRE_ID  ,NVL(TO_CHAR(ST".

           02  FILLER PIC X(256) VALUE "_BIRTH_DATE,'DDMMYYYY'),'        ')  ,TO_CHAR(ST_BIRTH_DATE,'YYYY')  ,NVL(ST_SEX,'9')  ,'3' "&
    " ,ST_ULN  ,DECODE(NVL(ST_BNM_GRADE,ST_OVER_GRADE),null ,DECODE(ST_WITHDRAWN_IND,'Y',' ',' '),DECODE(ST_AWARD_ISSUE,null ,' ',D"&
    "ECODE(AW_AWARD_CODE,'CC','P','EW','P',".

           02  FILLER PIC X(256) VALUE "'GA','P','GB','P','FO','P',('E'||NVL(ST_OVER_GRADE, replace(NVL(ST_ABS_LEVEL_ACHIEVED,ST_BNM"&
    "_GRADE),'E',null ))))))  ,AA_BTEC_TITLE  ,'  '  ,'Pearson'  ,DECODE(AW_AWARD_CODE,'CC',' ','EW',' ','GA',' ','GB',' ','FO',' '"&
    ",('E'||NVL(ST_OVER_GRADE, replace(NVL(".

           02  FILLER PIC X(256) VALUE "ST_ABS_LEVEL_ACHIEVED,ST_BNM_GRADE),'E',null ))))  ,' '  ,'O'  ,'Y'  ,ST_AWARD_ELIG   from S"&
    "TUDENTS ,APPROVAL_AWARDS ,APPROVAL_APPLICATION ,AT_AWARD_CODE_LEVELS ,AWARD_TITLES where (((((((((AW_AWARD_CODE=AACL_AC_CODE a"&
    "nd AT_NUMBER=AACL_AT_NUMBER) and NVL(S".

           02  FILLER PIC X(256) VALUE "T_BNM_GRADE,ST_ABS_LEVEL_ACHIEVED)=AACL_LEVEL) and AA_APPLICAT_NO=AW_APPLICAT_NO) and ST_COU"&
    "RSE_ID=AW_COURSE_NUMBER) and AW_AWARD_CODE in ('93','CC','EW','GG','GA','GB','FS','FO')) and AA_BTEC_TITLE=AT_NUMBER) and ST_D"&
    "ELETE is null ) and ST_FALLBACK is nul".

           02  FILLER PIC X(256) VALUE "l ) and ST_AWARD_ISSUE between TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR')) un"&
    "ion select ST_REG_NO  ,'                                   '  ,'                                   '  ,ST_COURSE_ID  ,TO_CHAR("&
    "ST_REG_DATE,'DDMMYYYY')  ,DECODE(AW_AW".

           02  FILLER PIC X(256) VALUE "ARD_CODE,'GA','10064187','GB','10064199',null )  ,AT_NAME  ,TO_CHAR(ST_AWARD_ISSUE,'DDMMYYYY"&
    "')  ,ST_CENTRE_ID  ,NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        ')  ,TO_CHAR(ST_BIRTH_DATE,'YYYY')  ,NVL(ST_SEX,'9')  ,'3' "&
    " ,ST_ULN  ,null   ,AA_BTEC_TITLE  ,'  ".

           02  FILLER PIC X(256) VALUE "'  ,'Pearson'  ,'XX'  ,' '  ,'O'  ,'Y'  ,ST_AWARD_ELIG   from STUDENTS ,APPROVAL_AWARDS ,APP"&
    "ROVAL_APPLICATION ,AWARD_TITLES where (((((((AA_APPLICAT_NO=AW_APPLICAT_NO and ST_COURSE_ID=AW_COURSE_NUMBER) and AW_AWARD_COD"&
    "E in ('93','CC','EW','GG','GA','GB','F".

           02  FILLER PIC X(256) VALUE "S','FO')) and AA_BTEC_TITLE=AT_NUMBER) and ST_DELETE is null ) and ST_FALLBACK is null ) and"&
    " ST_REG_DATE between TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR')) and ST_AWARD_ISSUE is null ) union select ST_R"&
    "EG_NO  ,'                             ".

           02  FILLER PIC X(256) VALUE "      '  ,'                                   '  ,ST_COURSE_ID  ,TO_CHAR(ST_REG_DATE,'DDMMYY"&
    "YY')  ,NVL(AACO_QCA_CODE,((AW_AWARD_CODE||AA_BTEC_TITLE)||'XX'))  ,AT_NAME  ,:b3  ,ST_CENTRE_ID  ,NVL(TO_CHAR(ST_BIRTH_DATE,'D"&
    "DMMYYYY'),'        ')  ,TO_CHAR(ST_BIR".

           02  FILLER PIC X(256) VALUE "TH_DATE,'YYYY')  ,NVL(ST_SEX,'9')  ,'3'  ,ST_ULN  ,DECODE(AW_AWARD_CODE,'Y6',DECODE(FN_CHECK"&
    "_TA_OVERGRADE_VISIBLE(ST_REG_NO),'Y',ST_BNM_GRADE,null ),ST_BNM_GRADE)  ,AA_BTEC_TITLE  ,'  '  ,'Pearson'  ,'  '  ,' '  ,'G'  "&
    ",'N'  ,ST_AWARD_ELIG   from STUDENTS ,".

           02  FILLER PIC X(256) VALUE "AT_AWARD_CODES ,AWARD_TITLES ,APPROVAL_APPLICATION ,AWARD_CODES ,APPROVAL_AWARDS where ((((("&
    "((((((((AACO_QCA_CODE is  not null  and AACO_AC_CODE=AW_AWARD_CODE) and AACO_AT_NUMBER=AT_NUMBER) and AT_NUMBER=TO_NUMBER(AA_B"&
    "TEC_TITLE)) and AA_APPLICAT_NO=(AW_APP".

           02  FILLER PIC X(256) VALUE "LICAT_NO+0)) and ST_DELETE is null ) and ST_FALLBACK is null ) and ST_UNIVERSITY_IND is null"&
    " ) and ST_AWARD_ISSUE is null ) and ST_BNM_GRADE is  not null ) and ST_COURSE_ID=AW_COURSE_NUMBER) and AC_BNM_TYPE='G') and AC"&
    "_CODE=AW_AWARD_CODE) and ( not exists ".

           02  FILLER PIC X(206) VALUE "(select null    from STUDENT_NG_STATS where SNST_ST_REG_NO=ST_REG_NO) or exists (select null"&
    "    from STUDENT_NG_STATS where (SNST_ST_REG_NO=ST_REG_NO and SNST_FEED_4_GRADE is null )))) order by 1           ".

01  SQ0005 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ((ST_REG_NO||'/')||SUBSTR(GUNI_ID,(LENGTH(GUNI_ID)-1)))  ST_REG_NO ,'                "&
    "                   '  ,'                                   '  ,ST_COURSE_ID  ,TO_CHAR(ST_REG_DATE,'DDMMYYYY')  ,GUNI_QCA_UNIT_"&
    "CODE  ,((GUNI_SHORT_DESCRIPTION||' LEV".

           02  FILLER PIC X(256) VALUE "EL ')||TO_CHAR(GUNI_LEVEL_CODE))  ,TO_CHAR(GSUN_KS_ACHIEVED_DATE,'DDMMYYYY')  ,ST_CENTRE_ID "&
    " ,NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        ')  ,TO_CHAR(ST_BIRTH_DATE,'YYYY')  ,NVL(ST_SEX,'9')  ,'3'  ,ST_ULN  ,DECODE("&
    "GSUN_KS_ACHIEVED_DATE,null ,null ,NVL(".

           02  FILLER PIC X(256) VALUE "ST_OVER_GRADE,'P  '))  SOVER ,' '  ,'  '  ,'Pearson'  ,'  '  ,'  '  ,' '  X  from STUDENTS ,"&
    "GNVQ_STUDENT_UNITS ,GNVQ_UNITS where (((((((((ST_COURSE_ID in ('ABS','KSQ00') and 1=2) and ST_DELETE is null ) and ST_FALLBACK"&
    " is null ) and ST_REG_NO=GSUN_ST_REG_N".

           02  FILLER PIC X(256) VALUE "O) and GSUN_GUNI_ID=GUNI_ID) and ST_UNIVERSITY_IND is null ) and GSUN_CERT_NO is  not null )"&
    " and  not exists (select 1   from KS2000_ASSESSMENT_RESULTS WW where ((((WW.KARE_PROXY_IND='Y' and WW.KARE_ST_REG_NO=GSUN_ST_R"&
    "EG_NO) and WW.KARE_GUAT_GUNI_ID=GSUN_G".

           02  FILLER PIC X(256) VALUE "UNI_ID) and TRUNC(WW.KARE_LOG_DATE)=TRUNC(GSUN_KS_ACHIEVED_DATE)) and  not exists (select 1 "&
    "  from KS2000_ASSESSMENT_RESULTS VV where (((NVL(VV.KARE_PROXY_IND,'N')<>'Y' and VV.KARE_ST_REG_NO=GSUN_ST_REG_NO) and VV.KARE"&
    "_GUAT_GUNI_ID=GSUN_GUNI_ID) and TRUNC(".

           02  FILLER PIC X(256) VALUE "VV.KARE_LOG_DATE)=TRUNC(GSUN_KS_ACHIEVED_DATE)))))) and (GSUN_KS_ACHIEVED_DATE between TO_DA"&
    "TE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR') or (ST_REG_DATE between TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MO"&
    "N-RRRR') and GSUN_KS_ACHIEVED_DATE is ".

           02  FILLER PIC X(30) VALUE "null ))) order by 1           ".

01  SQ0006 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ST_REG_NO  ,'                                   '  ,'                                "&
    "   '  ,ST_NVQ_REGISTERED_ID  ,TO_CHAR(ST_REG_DATE,'DDMMYYYY')  ,NVL(NVQ_QAN_CODE,ST_NVQ_REGISTERED_ID)  ,((NVQ_TITLE_1||' ')||"&
    "NVQ_TITLE_2)  ,TO_CHAR(ST_NVQ_CERTIFIC".

           02  FILLER PIC X(256) VALUE "ATE_PRINT_DATE,'DDMMYYYY')  ,ST_CENTRE_ID  ,NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '"&
    ")  ,TO_CHAR(ST_BIRTH_DATE,'YYYY')  ,NVL(ST_SEX,'9')  ,'3'  ,ST_ULN  ,DECODE(ST_NVQ_CERTIFICATE_PRINT_DATE,null ,DECODE(ST_WITH"&
    "DRAWN_IND,'Y',null ,null ),DECODE(ST_F".

           02  FILLER PIC X(256) VALUE "ALLBACK,'Y','U',NVL(ST_OVER_GRADE,'P  ')))  SOVER ,ST_NVQ_ELIGIBILITY_CODE  ,ST_NVQ_REGISTER"&
    "ED_ID  ,'  '  ,'Pearson'  ,'  '  ,'  '  ,' '  X  from STUDENTS ,NVQS where ((((ST_NVQ_CERTIFICATE_PRINT_DATE between TO_DATE(:"&
    "b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-".

           02  FILLER PIC X(256) VALUE "MON-RRRR') or (ST_REG_DATE between TO_DATE(:b1,'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON-RRRR')"&
    " and ST_NVQ_CERTIFICATE_PRINT_DATE is null )) and NVQ_ID=(ST_NVQ_REGISTERED_ID||'')) and ST_DELETE is null ) and ST_FALLBACK i"&
    "s null ) union select ST_REG_NO  ,'   ".

           02  FILLER PIC X(256) VALUE "                                '  ,'                                   '  ,ST_NVQ_REGISTERE"&
    "D_ID  ,TO_CHAR(ST_REG_DATE,'DDMMYYYY')  ,NCUN_QAN_CODE  ,((NCUN_TITLE_1||' ')||NCUN_TITLE_2)  ,TO_CHAR(ST_NVQ_CERTIFICATE_PRIN"&
    "T_DATE,'DDMMYYYY')  ,ST_CENTRE_ID  ,NV".

           02  FILLER PIC X(256) VALUE "L(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        ')  ,TO_CHAR(ST_BIRTH_DATE,'YYYY')  ,NVL(ST_SEX"&
    ",'9')  ,'3'  ,ST_ULN  ,DECODE(ST_NVQ_CERTIFICATE_PRINT_DATE,null ,DECODE(ST_WITHDRAWN_IND,'Y',null ,null ),DECODE(ST_FALLBACK,"&
    "'Y','U',NVL(ST_OVER_GRADE,'P  ')))  SO".

           02  FILLER PIC X(256) VALUE "VER ,ST_NVQ_ELIGIBILITY_CODE  ,NCUN_NCVQ_CODE  ,'  '  ,'Pearson'  ,'  '  ,'  '  ,' '  X  fro"&
    "m STUDENTS ,NVQ_COMPETENCE_UNITS ,NVQ_STUDENT_COMPETENCE_UNITS where (((((((ST_NVQ_CERTIFICATE_PRINT_DATE between TO_DATE(:b1,"&
    "'DD-MON-RRRR') and TO_DATE(:b2,'DD-MON".

           02  FILLER PIC X(256) VALUE "-RRRR') and NSCU_ACHIEVED_YEAR is  not null ) or (ST_REG_DATE between TO_DATE(:b1,'DD-MON-RR"&
    "RR') and TO_DATE(:b2,'DD-MON-RRRR') and ST_NVQ_CERTIFICATE_PRINT_DATE is null )) and NCUN_NCVQ_CODE=NSCU_NCUN_NCVQ_CODE) and N"&
    "SCU_ST_REG_NO=ST_REG_NO) and NCUN_SECT".

           02  FILLER PIC X(108) VALUE "OR_CODE between 890 and 896) and ST_DELETE is null ) and ST_FALLBACK is null ) order by ST_R"&
    "EG_NO           ".

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
           02     FILLER PIC S9(4) COMP-5 VALUE +304.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +60.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +309.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +55.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +85.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +317.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +74.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +29.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +513.
           02     FILLER PIC S9(4) COMP-5 VALUE +323.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +206.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +703.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +17.
           02     FILLER PIC S9(4) COMP-5 VALUE +17.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +172.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +716.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +187.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +734.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +23.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +294.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +1566.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +920.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +325.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +930.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +340.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +948.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +20.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +435.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +2156.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +1115.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +8.
           02     FILLER PIC S9(4) COMP-5 VALUE +8.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +482.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +1125.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +497.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +1143.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +21.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +596.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +82.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +1226.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +631.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +8.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +541.
           02     FILLER PIC S9(4) COMP-5 VALUE +1272.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +646.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +75.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +517.
           02     FILLER PIC S9(4) COMP-5 VALUE +1298.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +669.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +215.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +1308.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +696.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +47.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +1348.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +715.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +334.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +1361.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +758.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +13.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +541.
           02     FILLER PIC S9(4) COMP-5 VALUE +1397.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +773.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +14.
           02     FILLER PIC S9(4) COMP-5 VALUE +68.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +517.
           02     FILLER PIC S9(4) COMP-5 VALUE +1408.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +792.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +15.
           02     FILLER PIC S9(4) COMP-5 VALUE +93.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +1440.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +827.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +16.
           02     FILLER PIC S9(4) COMP-5 VALUE +280.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +1484.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +878.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +17.
           02     FILLER PIC S9(4) COMP-5 VALUE +260.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +1512.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +8.
           02     FILLER PIC S9(4) COMP-5 VALUE +8.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +925.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +18.
           02     FILLER PIC S9(4) COMP-5 VALUE +14.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +543.
           02     FILLER PIC S9(4) COMP-5 VALUE +1573.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
*       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
*
01  DS01-COURSE-DETAILS.
    03  DS01-CAND-ID              PIC  X(13) OCCURS 500.
    03  DS01-SURNAME              PIC  X(35) OCCURS 500.
    03  DS01-FORENAMES            PIC  X(35) OCCURS 500.
    03  DS01-COURSE               PIC  X(8)  OCCURS 500.
    03  DS01-REG-DATE             PIC  X(08) OCCURS 500.
    03  DS01-QUAL-CODE            PIC  X(12) OCCURS 500.
    03  DS01-TITLE                PIC X(140) OCCURS 500.
    03  DS01-AWD-DATE             PIC  X(08) OCCURS 500.
    03  DS01-CENTRE-ID            PIC  X(12) OCCURS 500.
    03  DS01-DOB                  PIC  X(08) OCCURS 500.
    03  DS01-DOB-YEAR-EST         PIC  X(04) OCCURS 500.
    03  DS01-GENDER               PIC  X(01) OCCURS 500.
    03  DS01-SPECIAL              PIC  X(01) OCCURS 500.
    03  DS01-ULN              	  PIC  X(10) OCCURS 500.
    03  DS01-RESULT               PIC  X(3)  OCCURS 500.
    03  DS01-EDEXCEL-PROG         PIC  X(12) OCCURS 500.
    03  DS01-SCHEME         	  PIC  X(12) OCCURS 500.
    03  DS01-EDEXCEL-E         	  PIC  X(10) OCCURS 500.
    03  DS01-LEVEL                PIC  X(2)  OCCURS 500.
    03  DS01-NCN                  PIC  X(10) OCCURS 500.
    03  DS01-REC-TYPE		  PIC  X(1)  OCCURS 500.
    03  DS01-CERT-IND	  	  PIC  X(1)  OCCURS 500.
    03  DS01-ELIG		  PIC  X(1)  OCCURS 500.
    03  DS01-CAND-ID-S            PIC  X(13).
    03  WS-START-DATE		  PIC  X(11).
    03  WS-END-DATE		  PIC  X(11).
    03  WS-REG-START-DATE	  PIC  X(11).
    03  WS-REG-END-DATE		  PIC  X(11).
    03  WS-CANDIDATE              PIC  X(13).
    03  WS-COURSE                 PIC  X(8).
    03  WS-ACADEMIC-YEAR          PIC  X(4).
    03  WS-VOCATIONAL-CENTRE      PIC  X(12).
    03  WS-BATH-CENTRE            PIC  X(12).
    03  WS-RUN-DATE           	  PIC  X(11).
    03  WS-NG-GRADING-DATE	  PIC  X(8).
    03  WS-FEED			  PIC  X(1).
    03  WS-RESULT		  PIC  X(03).
    03  WS-ELIG			  PIC  X(1).
    03  WS-REC-TYPE		  PIC  X(1).
        88  BTEC-NG				VALUE "G".
        88  BTEC				VALUE "B".
        88  ALL-OTHERS				VALUE "O".
    03  WS-CERTIFICATE-IND	  PIC  X(1).
        88  CERTIFICATED			VALUE "Y".
    03  WS-DAUD-SEQUENCE	  PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS-START-OR-END		  PIC  X(1).
        88  START-OF-RUN			VALUE "S".
        88  END-OF-RUN				VALUE "E".
    03 WS-REGN-TYPE		  PIC X(1).
*
01  WS-BLOCK-IND		  PIC  X(1).
    88  BLOCKED					VALUE "Y".
01  WS-STATUS			  PIC X(1).
01  WS-STATUS-I                   PIC S9(4) 
*                                           COMP
                                            COMP-5
                                                .
01  WS-STATUS2			  PIC X(10).
01  WS-STATUS2-I                  PIC S9(4) 
*                                           COMP
                                            COMP-5
                                                .
01  WS-DECISION-DATE              PIC X(11).
01  WS-DECISION-DATE-I            PIC S9(4) 
*                                           COMP
                                            COMP-5
                                                .
01  WS-MESSAGE			  PIC X(100).
01  WS-MESSAGE-I                  PIC S9(4) 
*                                           COMP
                                            COMP-5
                                                .

01  DS03-LOGIN.
    03  DS03-USERNAME             PIC  X(04) VALUE "ABCD".
    03  DS03-PASSWORD             PIC  X(04) VALUE "ABCD".
*
*    Current max for BTEC units is 73 - program will fall over if
*    there are more than 75!  DFEE wish to see them all.
*
01  DS05-UNIT-DETAILS.
    03  DS05-UNIT                 PIC  X(08) OCCURS 75.
    03  DS05-UNIT-DATE            PIC  X(08) OCCURS 75.
*
*       EXEC SQL END DECLARE SECTION END-EXEC.
*
*       EXEC SQL INCLUDE SQLCA END-EXEC.
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
01      WS01-GENERAL-STORAGE.
    03  WS01-ERR-MESSAGE          PIC  X(80).
    03  WS01-EOF-IND              PIC  X(01).
        88  WS01-EOF                         VALUE "Y".
    03  WS01-ROWS-TOTAL           PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-ROWS-THIS-FETCH      PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-BTEC-TOTAL           PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-GNVQ-TOTAL           PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-NVQ-TOTAL            PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-INDEX                PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-ROWS-2               PIC S9(04) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-INDEX-2              PIC S9(04) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-ABORT                PIC  9(09) 
*                                            COMP
                                             COMP-5
                                                  VALUE 4.
*EC2164
    03  WS01-BTEC-NO-BLOCK-TOTAL  PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .
    03  WS01-BTEC-BLOCKING-TOTAL  PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 . 
    03  WS01-BTEC-ROWS-TOTAL      PIC S9(09) 
*                                            COMP
                                             COMP-5
                                                 .     
*
01      WS03-DUMMY-RECORD.
    03  WS03-CAND-ID              PIC  X(13).
    03  WS03-SURNAME              PIC  X(35).
    03  WS03-FORENAMES            PIC  X(35).
    03  WS03-REG-DATE             PIC  X(08).
    03  WS03-QUAL-CODE            PIC  X(12).
    03  WS03-TITLE                PIC X(140).
    03  WS03-AWD-DATE             PIC  X(08).
    03  WS03-CENTRE-ID            PIC  X(12).
    03  WS03-DOB                  PIC  X(08).
    03  WS03-DOB-YEAR-EST	  PIC  X(04).
    03  WS03-GENDER               PIC  X(01).
    03  WS03-SPECIAL              PIC  X(01).
    03  WS03-ULN              	  PIC  X(10).
    03  WS03-RESULT               PIC  X(3).
    03  WS03-EDEXCEL-PROG         PIC  X(12).
    03  WS03-SCHEME         	  PIC  X(12).
    03  WS03-EDEXCEL-E         	  PIC  X(10).
    03  WS03-LEVEL         	  PIC  X(2). 
    03  WS03-NCN         	  PIC  X(10).
    03  WS03-UNIT-DETAILS.
        05  FILLER                           OCCURS 1.
            07  WS03-UNIT         PIC  X(08).
            07  WS03-UNIT-DATE    PIC  X(08).
*
01 WS-TOTALS-REPORT.
    03 WS-TR-HEAD-1.
        05 FILLER		PIC X(11) VALUE 'Run Date :'.
	05 WS-TR-H1-RUN-DATE	PIC X(11).
	05 FILLER		PIC X(104) VALUE SPACES.
	05 FILLER		PIC X(6)  VALUE 'SSP996'.
    03 WS-TR-HEAD-2.
	05 FILLER		PIC X(52) VALUE SPACES.
	05 FILLER		PIC X(29) VALUE
	  'DFEE Registration File Totals'.
	05 FILLER		PIC X(51) VALUE SPACES.
    03 WS-TR-HEAD-3.
	05 FILLER		PIC X(53) VALUE SPACES.
	05 WS-TR-H3-START-DATE	PIC X(11).
	05 FILLER		PIC X(4)  VALUE ' to '.
	05 WS-TR-H3-END-DATE	PIC X(11).
	05 FILLER		PIC X(53) VALUE SPACES.
    03 WS-TR-DETAIL.
	05 FILLER		PIC X(14) VALUE SPACES.
	05 FILLER		PIC X(7)  VALUE 'BTEC : '.
	05 WS-TR-DET-BTEC-TOTAL	PIC Z(8)9.
	05 FILLER		PIC X(28) VALUE SPACES.
	05 FILLER		PIC X(7)  VALUE 'GNVQ : '.
	05 WS-TR-DET-GNVQ-TOTAL	PIC Z(8)9.
	05 FILLER		PIC X(28) VALUE SPACES.
	05 FILLER		PIC X(7)  VALUE ' NVQ : '.
	05 WS-TR-DET-NVQ-TOTAL	PIC Z(8)9.
*
PROCEDURE DIVISION.
MAIN SECTION.
**********************************************************************
*                            
*       Main control section.
*
**********************************************************************
MAIN-START.
*
	MOVE 'S' TO WS-START-OR-END.

        PERFORM A-INITIALISE.
*
        MOVE    ZERO               TO  WS01-ROWS-TOTAL.
        MOVE   "N"                 TO  WS01-EOF-IND.
        PERFORM C-PROCESS-BTEC.  

        MOVE    ZERO               TO  WS01-ROWS-TOTAL.
        MOVE   "N"                 TO  WS01-EOF-IND.
        PERFORM E-PROCESS-GNVQ.
*
        MOVE    ZERO               TO  WS01-ROWS-TOTAL.
        MOVE   "N"                 TO  WS01-EOF-IND.
        PERFORM G-PROCESS-NVQ.
*
	MOVE 'E' TO WS-START-OR-END.

        PERFORM I-TERMINATE.
*
MAIN-EXIT.
*
        STOP RUN.
*
A-INITIALISE SECTION.
**********************************************************************
*
*       Open the file.
*       Log on to Oracle.
*
**********************************************************************
A-START.
*

    	ACCEPT WS-START-DATE.
    	ACCEPT WS-END-DATE.
    	ACCEPT WS-ACADEMIC-YEAR.
*
        DISPLAY "PARAMETERS".
        DISPLAY WS-START-DATE.
        DISPLAY WS-END-DATE.
        DISPLAY WS-ACADEMIC-YEAR.
*
        OPEN    OUTPUT  DATA-FILE-B.
        OPEN    OUTPUT  DATA-FILE-G.
        OPEN    OUTPUT  DATA-FILE-N.
        OPEN    OUTPUT  TOTALS-REP.
*
*       EXEC SQL WHENEVER SQLERROR GO TO   ZZ-ABORT END-EXEC.
*
        MOVE   "ERROR CONNECTING TO DATABASE"
                                   TO  WS01-ERR-MESSAGE.
*       EXEC SQL
*               CONNECT :DS03-USERNAME
*               IDENTIFIED BY :DS03-PASSWORD
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
            DS03-USERNAME IN
            DS03-LOGIN
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            DS03-PASSWORD IN
            DS03-LOGIN
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
            THEN GO TO ZZ-ABORT END-IF.
*
*       EXEC SQL
*           SELECT TO_CHAR(SYSDATE,'DD-MON-YYYY') 
*             INTO WS-RUN-DATE
*             FROM DUAL
*       END-EXEC.
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
        CALL "SQLADR" USING
            WS-RUN-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
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
            THEN GO TO ZZ-ABORT END-IF.

        MOVE   "ERROR CALCULATING FIRST ENTRY DATE"
                                   TO  WS01-ERR-MESSAGE.
*       EXEC SQL
*         SELECT TO_CHAR(FN_FIRST_ENTRY_DATE(TRUNC(SYSDATE)),'DDMMYYYY')
*         INTO   WS-NG-GRADING-DATE
*         FROM DUAL
*       END-EXEC.
        CALL "SQLADR" USING SQ0002 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 55 TO SQL-OFFSET
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
        CALL "SQLADR" USING
            WS-NG-GRADING-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 8 TO SQL-SQHSTL(1)
        MOVE 8 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
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
            THEN GO TO ZZ-ABORT END-IF.
*
*       EXEC SQL
*           TRUNCATE TABLE OFQUAL_DETAILS
*       END-EXEC.
        CALL "SQLADR" USING SQ0003 SQL-STMT
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
            THEN GO TO ZZ-ABORT END-IF.

	PERFORM K-MAINTAIN-DAUD.
*
A-EXIT.
*
        EXIT.
*
C-PROCESS-BTEC SECTION.
**********************************************************************
*
*       Read the required BTEC courses and registrations. 
*
**********************************************************************
C-START.
*
*       EXEC SQL
*           DECLARE GET_BTEC CURSOR FOR
*           SELECT  ST_REG_NO,
*                   '                                   ',
*                   '                                   ',
*                   ST_COURSE_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*       	    AW_AWARD_CODE||AA_BTEC_TITLE||'XX',
*                   AT_NAME,
*                   TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
*                   TO_CHAR(ST_BIRTH_DATE,'YYYY'), 
*                   DECODE(ST_SEX,'F','F','M','M','U'),
*       	   '3',
*       	   ST_ULN,
*       	  decode(ST_AWARD_ISSUE,null,
*       		    decode(st_withdrawn_ind,'Y',null,null),
*       	            decode(st_fallback,'Y', 'U', 
*       		nvl(st_over_grade,'P  '))) sover,
*       	   AA_BTEC_TITLE,
*       	   '  ',
*       	   'Pearson',
*       	    '  ',
*       	    ' ',
*       	    DECODE(AC_BNM_TYPE,'G','G','B'),
*       	    'Y',
*       	    ST_AWARD_ELIG                            
*             FROM  
*                   APPROVAL_APPLICATION,
*                   APPROVAL_AWARDS,
*       	    AWARD_CODES,
*                   AWARD_TITLES,
*       	    STUDENTS X
*            WHERE  AW_AWARD_CODE IN ('01', '02', '03',
*                                     '04', '05', '06', '11')
*              AND  st_delete is null                                               
*              AND  st_fallback is null                                               
*              AND  ST_COURSE_ID||'' = AW_COURSE_NUMBER
*              AND  AA_APPLICAT_NO = AW_APPLICAT_NO+0
*              AND  ST_UNIVERSITY_IND is null
*              AND  AC_CODE = AW_AWARD_CODE	  
*              AND  TO_NUMBER(AA_BTEC_TITLE) = AT_NUMBER
*              AND (  ST_AWARD_ISSUE 
*                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
*               or (st_reg_date 
*                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
*               and ST_AWARD_ISSUE is null)
*           	    )
*       	AND NOT EXISTS (SELECT 	NULL
*       			FROM	AWARD_CODES,
*       				APPROVAL_AWARDS,
*       				STUDENTS
*       			WHERE	AC_CODE = AW_AWARD_CODE
*       			AND	AW_COURSE_NUMBER = ST_COURSE_ID
*       			AND	ST_REG_NO =  X.ST_REG_NO
*       			AND	AC_BNM_TYPE = 'G'
*       			AND	ST_AWARD_ISSUE IS NULL
*       			AND 	ST_BNM_GRADE IS NOT NULL
*       		       )	
*       	AND   (	
*       		NOT EXISTS (SELECT	NULL
*       			    FROM	STUDENT_NG_STATS
*       			    WHERE	SNST_ST_REG_NO = ST_REG_NO
*       			   )
*       	       OR   EXISTS (SELECT	NULL
*       			    FROM	STUDENT_NG_STATS
*       			    WHERE	SNST_ST_REG_NO = ST_REG_NO
*       			    AND		SNST_FEED_4_GRADE IS NULL
*       			   )
*       	      )
*       UNION
*           SELECT  ST_REG_NO,
*                   '                                   ',
*                   '                                   ',
*                   ST_COURSE_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*		    nvl(AACO_QCA_CODE,AW_AWARD_CODE||AA_BTEC_TITLE||'XX'),      
*                   AT_NAME,
*                   TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
*       	    TO_CHAR(ST_BIRTH_DATE,'YYYY'),
*                   NVL(ST_SEX,'9'),
*       	   '3',
*                  ST_ULN,		                               
*       	decode(to_char(ST_AWARD_ISSUE),null,
*       	decode(st_withdrawn_ind,'Y',null,null),              
*       	     decode(st_fallback,'Y','U', 
*       	nvl( nvl(st_over_grade, ST_BNM_GRADE) ,'P  '))
*       	      ) sover,
*       	   AA_BTEC_TITLE,
*       	   '  ',
*       	   'Pearson',
*       	    '  ',
*       	    ' ',
*       	    DECODE(AC_BNM_TYPE,'G','G','B'),
*       	    'Y',
*       	    ST_AWARD_ELIG                            
*             FROM  
*                   APPROVAL_APPLICATION,
*                   APPROVAL_AWARDS,
*       	    AWARD_CODES,
*                   AWARD_TITLES,
*       	    AT_AWARD_CODES,
*       	    STUDENTS X
*             WHERE AW_AWARD_CODE NOT IN 
*                               ( '01','02','03','04','05','06','11',
*                                 '27','64','35','63','93','HH','GG',
*       			  'GA','GB','CC','EW','FS','FO')
*             AND   st_delete is null   
*             AND   st_fallback is null                                                      
*             AND   AW_AWARD_CODE = AACO_AC_CODE
*             AND   AC_CODE = AW_AWARD_CODE	  
*             AND   AT_NUMBER = AACO_AT_NUMBER
*             AND   ST_COURSE_ID||'' = AW_COURSE_NUMBER
*             AND   AA_APPLICAT_NO = AW_APPLICAT_NO+0
*             AND   TO_NUMBER(AA_BTEC_TITLE) = AT_NUMBER
*             AND   ST_UNIVERSITY_IND is null  
*             AND ( ST_AWARD_ISSUE                                  
*                	BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                       AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*                   OR  ( ST_REG_DATE 
*                	    BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                           AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*                           AND ST_AWARD_ISSUE IS NULL)
*                   )
*             AND NOT EXISTS   (SELECT 	NULL
*       			FROM	AWARD_CODES,
*       				APPROVAL_AWARDS,
*       				STUDENTS
*       			WHERE	AC_CODE = AW_AWARD_CODE
*       			AND	AW_COURSE_NUMBER = ST_COURSE_ID
*       			AND	ST_REG_NO =  X.ST_REG_NO
*       			AND	AC_BNM_TYPE = 'G'
*       			AND	ST_AWARD_ISSUE IS NULL
*       			AND 	ST_BNM_GRADE IS NOT NULL
*       		       )	
*             AND   (	
*       		NOT EXISTS (SELECT	NULL
*       			    FROM	STUDENT_NG_STATS
*       			    WHERE	SNST_ST_REG_NO = ST_REG_NO
*       			   )
*       	        OR  EXISTS (SELECT	NULL
*       			    FROM	STUDENT_NG_STATS
*       			    WHERE	SNST_ST_REG_NO = ST_REG_NO
*       			    AND		SNST_FEED_4_GRADE IS NULL
*       			   )
*       	    )
*       UNION
*           SELECT  ST_REG_NO,
*                   '                                   ',
*                   '                                   ',
*                   ST_COURSE_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*                   nvl(acpa_qca_lu_x_code,AW_AWARD_CODE||AA_BTEC_TITLE||'XX'),   
*                   AT_NAME,
*                   TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
*                   TO_CHAR(ST_BIRTH_DATE,'YYYY'),
*                   NVL(ST_SEX,'9'),
*                  '3',
*                  '  ',
*                  decode(st_over_grade,null,
*                  decode(st_withdrawn_ind,'Y',' ',
*        decode(aw_award_code,'63',decode(st_award_issue,null,null,'P') ,' ')),
*                  decode(st_award_issue,null,' ',
*                   decode(st_fallback,'Y', 'U', 'E'||nvl(st_over_grade, 
*                   replace(ST_ABS_LEVEL_ACHIEVED,'E',null)) ))),
*                  AA_BTEC_TITLE,
*                  '  ',
*                  'Pearson',
*                  decode(nvl(st_over_grade,ST_ABS_LEVEL_ACHIEVED),null,
*        'XX','E'||nvl(st_over_grade,replace(ST_ABS_LEVEL_ACHIEVED,'E',null))),
*       	   ' ',
*                  'O',
*       	   'Y',
*       	    ST_AWARD_ELIG                            
*       FROM    students, 
*               approval_awards,     
*               approval_application,
*               approval_comb_packages,
*               award_titles
*       WHERE   aa_applicat_no  = aw_applicat_no
*       and     st_course_id    = aw_course_number       
*       and  	aw_award_code   in ('35','63','HH')
*       and     aa_btec_title   = at_number                                     
*       and     st_delete is null                
*       AND  	st_fallback is null                                         
*       and     acpa_at_number  = at_number
*              AND ( ST_AWARD_ISSUE 
*                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*                or  ( st_reg_date 
*                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*                        and ST_AWARD_ISSUE  is null)
*                   ) 
*       UNION
*           SELECT  ST_REG_NO,
*                   '                                   ',
*                   '                                   ',
*                   ST_COURSE_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*                   AACL_QCA_CODE,   
*                   AT_NAME,
*                   TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
*                   TO_CHAR(ST_BIRTH_DATE,'YYYY'),
*                   NVL(ST_SEX,'9'),
*       	   '3',
*                  ST_ULN,
*                  decode(nvl(st_bnm_grade,st_over_grade),null,
*                  decode(st_withdrawn_ind,'Y',' ',' '),
*                  decode(st_award_issue,null,' ',
*              decode(aw_award_code,'CC','P','EW','P','GA','P','GB','P',   
*				    'FO','P',
*                  'E'||nvl(st_over_grade,     
*      	     replace(nvl(ST_ABS_LEVEL_ACHIEVED,st_bnm_grade),'E',null) )))),           
*                  AA_BTEC_TITLE,
*                  '  ',
*                  'Pearson',                                     
*       	  decode(aw_award_code,'CC',' ','EW',' ','GA',' ','GB',' ',  
*       			       'FO',' ',
*       	'E'||nvl(st_over_grade,replace(nvl(ST_ABS_LEVEL_ACHIEVED,
*       		st_bnm_grade),'E',null))),
*       	   ' ',
*                   'O',
*       	    'Y',
*       	    ST_AWARD_ELIG                            
*       FROM    students, 
*               approval_awards,     
*               approval_application,
*               AT_AWARD_CODE_levels,
*               award_titles
*       WHERE  
*            	AW_AWARD_CODE = AACL_AC_CODE
*       and  	AT_NUMBER = AACL_AT_NUMBER
*       and    	nvl(st_bnm_grade,ST_ABS_LEVEL_ACHIEVED) =  AACL_LEVEL 
*       and     aa_applicat_no  = aw_applicat_no
*       and     st_course_id    = aw_course_number       
*       and  	aw_award_code   in ('93','CC','EW','GG','GA','GB',
*       			    'FS','FO')
*       and     aa_btec_title   = at_number                                     
*       and     st_delete is null                
*       AND  	st_fallback is null                                         
*       AND 	ST_AWARD_ISSUE 
*                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*       UNION
*           SELECT  ST_REG_NO,
*                   '                                   ',
*                   '                                   ',
*                   ST_COURSE_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*                   decode(aw_award_code,'GA','10064187','GB','10064199',null),    
*                   AT_NAME,
*                   TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
*                   TO_CHAR(ST_BIRTH_DATE,'YYYY'),
*                   NVL(ST_SEX,'9'),
*                  '3',
*                  ST_ULN,                                
*       	   null,
*                  AA_BTEC_TITLE,
*                  '  ',
*                  'Pearson',
*                  'XX',
*                  ' ',
*                  'O',
*       	    'Y',
*       	    ST_AWARD_ELIG                            
*       FROM    students, 
*               approval_awards,     
*               approval_application,
*               award_titles
*       WHERE   aa_applicat_no  = aw_applicat_no
*       and     st_course_id    = aw_course_number       
*       and     aw_award_code   in ('93','CC','EW','GG','GA','GB',
*       			    'FS','FO')
*       and     aa_btec_title   = at_number                                     
*       and     st_delete is null                
*       and     st_fallback is null                                         
*       and    st_reg_date 
*                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*                        and ST_AWARD_ISSUE  is null                    
*
*       UNION
*       SELECT	ST_REG_NO,
*               '                                   ',
*               '                                   ',
*       	ST_COURSE_ID,
*       	TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*       	NVL(AACO_QCA_CODE,AW_AWARD_CODE||AA_BTEC_TITLE||'XX'),      
*               AT_NAME,
*       	:WS-NG-GRADING-DATE,
*               ST_CENTRE_ID,
*               NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
*       	TO_CHAR(ST_BIRTH_DATE,'YYYY'),
*               NVL(ST_SEX,'9'),
*       	'3',
*               ST_ULN,	
*               --Code modified for EC4363 TA Embargo 2020 Starts--	                               
*       	--ST_BNM_GRADE, 
*               DECODE(aw_award_code,'Y6',
*               DECODE(FN_CHECK_TA_OVERGRADE_VISIBLE(ST_REG_NO),
*               'Y',ST_BNM_GRADE,NULL),ST_BNM_GRADE),
*               --Code modified for EC4363 TA Embargo 2020 Ends--
*       	AA_BTEC_TITLE,
*       	'  ',
*       	'Pearson',
*       	'  ',
*       	' ',
*       	'G',
*       	'N',
*       	ST_AWARD_ELIG                            
*       FROM	  
*       	STUDENTS,
*       	AT_AWARD_CODES,
*       	AWARD_TITLES,
*       	APPROVAL_APPLICATION,
*       	AWARD_CODES,
*       	APPROVAL_AWARDS
*       WHERE	AACO_QCA_CODE 	   IS NOT NULL
*       AND     AACO_AC_CODE 		= AW_AWARD_CODE
*       AND	AACO_AT_NUMBER 		= AT_NUMBER
*       AND	AT_NUMBER 		= TO_NUMBER(AA_BTEC_TITLE)
*       AND	AA_APPLICAT_NO 		= AW_APPLICAT_NO+0
*       AND	ST_DELETE 	       IS NULL                                               
*       AND	ST_FALLBACK 	       IS NULL
*       AND	ST_UNIVERSITY_IND      IS NULL
*       AND	ST_AWARD_ISSUE	       IS NULL
*       AND	ST_BNM_GRADE 	   IS NOT NULL
*       AND	ST_COURSE_ID 		= AW_COURSE_NUMBER
*       AND	AC_BNM_TYPE		= 'G'
*       AND	AC_CODE			= AW_AWARD_CODE	
*       AND   (	
*       	NOT EXISTS (SELECT	NULL
*       		    FROM	STUDENT_NG_STATS
*       		    WHERE	SNST_ST_REG_NO = ST_REG_NO
*       		   )
*       	OR   EXISTS (SELECT	NULL
*       		     FROM	STUDENT_NG_STATS
*       		     WHERE	SNST_ST_REG_NO = ST_REG_NO
*       		     AND	SNST_FEED_4_GRADE IS NULL
*       		    )
*             )
*       ORDER BY 1
*       END-EXEC.
*
        MOVE   "C: ERROR OPENING BTEC COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        DISPLAY "BTEC START".
*        DISPLAY "  BLOCKED LEARNERS".

*       EXEC SQL
*           OPEN    GET_BTEC
*       END-EXEC.
        CALL "SQLADR" USING SQ0004 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 89 TO SQL-OFFSET
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
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(3)
        MOVE 11 TO SQL-SQHSTL(3)
        MOVE 11 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(4)
        MOVE 11 TO SQL-SQHSTL(4)
        MOVE 11 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(5)
        MOVE 11 TO SQL-SQHSTL(5)
        MOVE 11 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(6)
        MOVE 11 TO SQL-SQHSTL(6)
        MOVE 11 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(7)
        MOVE 11 TO SQL-SQHSTL(7)
        MOVE 11 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(8)
        MOVE 11 TO SQL-SQHSTL(8)
        MOVE 11 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(9)
        MOVE 11 TO SQL-SQHSTL(9)
        MOVE 11 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(10)
        MOVE 11 TO SQL-SQHSTL(10)
        MOVE 11 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(11)
        MOVE 11 TO SQL-SQHSTL(11)
        MOVE 11 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(12)
        MOVE 11 TO SQL-SQHSTL(12)
        MOVE 11 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(13)
        MOVE 11 TO SQL-SQHSTL(13)
        MOVE 11 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(14)
        MOVE 11 TO SQL-SQHSTL(14)
        MOVE 11 TO SQL-SQHSTS(14)
        MOVE 0 TO SQL-SQINDV(14)
        MOVE 0 TO SQL-SQINDS(14)
        MOVE 0 TO SQL-SQHARM(14)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(15)
        MOVE 11 TO SQL-SQHSTL(15)
        MOVE 11 TO SQL-SQHSTS(15)
        MOVE 0 TO SQL-SQINDV(15)
        MOVE 0 TO SQL-SQINDS(15)
        MOVE 0 TO SQL-SQHARM(15)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(16)
        MOVE 11 TO SQL-SQHSTL(16)
        MOVE 11 TO SQL-SQHSTS(16)
        MOVE 0 TO SQL-SQINDV(16)
        MOVE 0 TO SQL-SQINDS(16)
        MOVE 0 TO SQL-SQHARM(16)
        CALL "SQLADR" USING
            WS-NG-GRADING-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(17)
        MOVE 8 TO SQL-SQHSTL(17)
        MOVE 8 TO SQL-SQHSTS(17)
        MOVE 0 TO SQL-SQINDV(17)
        MOVE 0 TO SQL-SQINDS(17)
        MOVE 0 TO SQL-SQHARM(17)
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
            THEN GO TO ZZ-ABORT END-IF.
*                                                            
        PERFORM CA-FETCH-BTEC
          UNTIL WS01-EOF.
*
        DISPLAY "TOTAL BTEC FETCHED ", WS01-ROWS-TOTAL WITH CONVERSION.
        DISPLAY "".

        MOVE   "C: ERROR CLOSING BTEC COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
*       EXEC SQL
*           CLOSE   GET_BTEC
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 172 TO SQL-OFFSET
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
            THEN GO TO ZZ-ABORT END-IF.
*
C-EXIT.
*
        EXIT.
*
CA-FETCH-BTEC SECTION.
**********************************************************************
*
*       Get a set of BTEC awards and process them.
*
**********************************************************************
CA-START.
*
        MOVE   "CA: ERROR FETCHING BTEC AWARDS"
                                   TO  WS01-ERR-MESSAGE.
*       EXEC SQL
*           FETCH   GET_BTEC
*            INTO  :DS01-CAND-ID,     
*                  :DS01-SURNAME,                                       
*                  :DS01-FORENAMES,                                     
*                  :DS01-COURSE,   
*                  :DS01-REG-DATE,                                      
*                  :DS01-QUAL-CODE,                                     
*                  :DS01-TITLE,                                         
*                  :DS01-AWD-DATE,                                      
*                  :DS01-CENTRE-ID,                                     
*                  :DS01-DOB,       
*       	   :DS01-DOB-YEAR-EST,                                      
*                  :DS01-GENDER,        
*   		   :DS01-SPECIAL,           
*   		   :DS01-ULN,           
*   		   :DS01-RESULT,             
*   		   :DS01-EDEXCEL-PROG,       
*   		   :DS01-SCHEME,      
*   		   :DS01-EDEXCEL-E,      
*       	   :DS01-LEVEL,
*                  :DS01-NCN,
*       	   :DS01-REC-TYPE,
*       	   :DS01-CERT-IND,
*       	   :DS01-ELIG
*       END-EXEC.
        MOVE 500 TO SQL-ITERS
        MOVE 187 TO SQL-OFFSET
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
            DS01-CAND-ID IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(1)
        MOVE 13 TO SQL-SQHSTL(1)
        MOVE 13 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            DS01-SURNAME IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(2)
        MOVE 35 TO SQL-SQHSTL(2)
        MOVE 35 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            DS01-FORENAMES IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(3)
        MOVE 35 TO SQL-SQHSTL(3)
        MOVE 35 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            DS01-COURSE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(4)
        MOVE 8 TO SQL-SQHSTL(4)
        MOVE 8 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            DS01-REG-DATE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(5)
        MOVE 8 TO SQL-SQHSTL(5)
        MOVE 8 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            DS01-QUAL-CODE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(6)
        MOVE 12 TO SQL-SQHSTL(6)
        MOVE 12 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            DS01-TITLE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(7)
        MOVE 140 TO SQL-SQHSTL(7)
        MOVE 140 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            DS01-AWD-DATE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(8)
        MOVE 8 TO SQL-SQHSTL(8)
        MOVE 8 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            DS01-CENTRE-ID IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(9)
        MOVE 12 TO SQL-SQHSTL(9)
        MOVE 12 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            DS01-DOB IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(10)
        MOVE 8 TO SQL-SQHSTL(10)
        MOVE 8 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            DS01-DOB-YEAR-EST IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(11)
        MOVE 4 TO SQL-SQHSTL(11)
        MOVE 4 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            DS01-GENDER IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(12)
        MOVE 1 TO SQL-SQHSTL(12)
        MOVE 1 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            DS01-SPECIAL IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(13)
        MOVE 1 TO SQL-SQHSTL(13)
        MOVE 1 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            DS01-ULN IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(14)
        MOVE 10 TO SQL-SQHSTL(14)
        MOVE 10 TO SQL-SQHSTS(14)
        MOVE 0 TO SQL-SQINDV(14)
        MOVE 0 TO SQL-SQINDS(14)
        MOVE 0 TO SQL-SQHARM(14)
        CALL "SQLADR" USING
            DS01-RESULT IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(15)
        MOVE 3 TO SQL-SQHSTL(15)
        MOVE 3 TO SQL-SQHSTS(15)
        MOVE 0 TO SQL-SQINDV(15)
        MOVE 0 TO SQL-SQINDS(15)
        MOVE 0 TO SQL-SQHARM(15)
        CALL "SQLADR" USING
            DS01-EDEXCEL-PROG IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(16)
        MOVE 12 TO SQL-SQHSTL(16)
        MOVE 12 TO SQL-SQHSTS(16)
        MOVE 0 TO SQL-SQINDV(16)
        MOVE 0 TO SQL-SQINDS(16)
        MOVE 0 TO SQL-SQHARM(16)
        CALL "SQLADR" USING
            DS01-SCHEME IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(17)
        MOVE 12 TO SQL-SQHSTL(17)
        MOVE 12 TO SQL-SQHSTS(17)
        MOVE 0 TO SQL-SQINDV(17)
        MOVE 0 TO SQL-SQINDS(17)
        MOVE 0 TO SQL-SQHARM(17)
        CALL "SQLADR" USING
            DS01-EDEXCEL-E IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(18)
        MOVE 10 TO SQL-SQHSTL(18)
        MOVE 10 TO SQL-SQHSTS(18)
        MOVE 0 TO SQL-SQINDV(18)
        MOVE 0 TO SQL-SQINDS(18)
        MOVE 0 TO SQL-SQHARM(18)
        CALL "SQLADR" USING
            DS01-LEVEL IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(19)
        MOVE 2 TO SQL-SQHSTL(19)
        MOVE 2 TO SQL-SQHSTS(19)
        MOVE 0 TO SQL-SQINDV(19)
        MOVE 0 TO SQL-SQINDS(19)
        MOVE 0 TO SQL-SQHARM(19)
        CALL "SQLADR" USING
            DS01-NCN IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(20)
        MOVE 10 TO SQL-SQHSTL(20)
        MOVE 10 TO SQL-SQHSTS(20)
        MOVE 0 TO SQL-SQINDV(20)
        MOVE 0 TO SQL-SQINDS(20)
        MOVE 0 TO SQL-SQHARM(20)
        CALL "SQLADR" USING
            DS01-REC-TYPE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(21)
        MOVE 1 TO SQL-SQHSTL(21)
        MOVE 1 TO SQL-SQHSTS(21)
        MOVE 0 TO SQL-SQINDV(21)
        MOVE 0 TO SQL-SQINDS(21)
        MOVE 0 TO SQL-SQHARM(21)
        CALL "SQLADR" USING
            DS01-CERT-IND IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(22)
        MOVE 1 TO SQL-SQHSTL(22)
        MOVE 1 TO SQL-SQHSTS(22)
        MOVE 0 TO SQL-SQINDV(22)
        MOVE 0 TO SQL-SQINDS(22)
        MOVE 0 TO SQL-SQHARM(22)
        CALL "SQLADR" USING
            DS01-ELIG IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(23)
        MOVE 1 TO SQL-SQHSTL(23)
        MOVE 1 TO SQL-SQHSTS(23)
        MOVE 0 TO SQL-SQINDV(23)
        MOVE 0 TO SQL-SQINDS(23)
        MOVE 0 TO SQL-SQHARM(23)
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
            THEN GO TO ZZ-ABORT END-IF.
*
        IF  SQLCODE IS POSITIVE THEN
            SET     WS01-EOF       TO  TRUE
        END-IF.
        SUBTRACT WS01-ROWS-TOTAL FROM  SQLERRD(3)
                               GIVING  WS01-ROWS-THIS-FETCH.
        MOVE    SQLERRD(3)         TO  WS01-ROWS-TOTAL.
*EC2164
*	MOVE WS01-ROWS-TOTAL TO WS-TR-DET-BTEC-TOTAL.
        MOVE WS01-ROWS-TOTAL TO WS01-BTEC-ROWS-TOTAL.

        PERFORM
          VARYING WS01-INDEX
          FROM 1 BY 1
          UNTIL WS01-INDEX > WS01-ROWS-THIS-FETCH

            MOVE DS01-CAND-ID(WS01-INDEX)   TO  WS-CANDIDATE    
	    MOVE DS01-CERT-IND(WS01-INDEX)  TO  WS-CERTIFICATE-IND
	    MOVE DS01-REC-TYPE(WS01-INDEX)  TO  WS-REC-TYPE
	    MOVE DS01-ELIG(WS01-INDEX)      TO  WS-ELIG

	    MOVE 'N' TO WS-BLOCK-IND	

* 	    Only check certificate block for non-certificated BTEC NG learners...

	    IF BTEC-NG AND NOT CERTIFICATED
	    THEN
*		display 'Check Blocking'
	      PERFORM L-CHECK-BLOCKING
            END-IF

	    IF NOT BLOCKED

              MOVE    DS01-COURSE(WS01-INDEX)    TO  WS-COURSE     
              MOVE    DS01-CENTRE-ID(WS01-INDEX) TO  WS-VOCATIONAL-CENTRE   

              PERFORM H-GET-GUEST-CENTRE

	      MOVE    DS01-REC-TYPE(WS01-INDEX)     TO  WS-REC-TYPE
              MOVE    WS-BATH-CENTRE                TO  DS01-CENTRE-ID(WS01-INDEX)
    	      MOVE    DS01-RESULT(WS01-INDEX)       TO  WS-RESULT    

              MOVE    DS01-CAND-ID(WS01-INDEX)      TO  DS01-CAND-ID-S   
              MOVE    DS01-CAND-ID(WS01-INDEX)      TO  WS03-CAND-ID     
              MOVE    DS01-SURNAME(WS01-INDEX)      TO  WS03-SURNAME     
              MOVE    DS01-FORENAMES(WS01-INDEX)    TO  WS03-FORENAMES   
              MOVE    DS01-REG-DATE(WS01-INDEX)     TO  WS03-REG-DATE    
              MOVE    DS01-QUAL-CODE(WS01-INDEX)    TO  WS03-QUAL-CODE   
              MOVE    DS01-TITLE(WS01-INDEX)        TO  WS03-TITLE       
              MOVE    DS01-AWD-DATE(WS01-INDEX)     TO  WS03-AWD-DATE    
              MOVE    DS01-CENTRE-ID(WS01-INDEX)    TO  WS03-CENTRE-ID   
              MOVE    DS01-DOB(WS01-INDEX)          TO  WS03-DOB         
              MOVE    DS01-DOB-YEAR-EST(WS01-INDEX) TO  WS03-DOB-YEAR-EST         
              MOVE    DS01-GENDER(WS01-INDEX)       TO  WS03-GENDER 
              MOVE    DS01-SPECIAL(WS01-INDEX)      TO  WS03-SPECIAL
    	      MOVE    DS01-ULN(WS01-INDEX)          TO  WS03-ULN   
    	      MOVE    DS01-RESULT(WS01-INDEX)       TO  WS03-RESULT    
   	      MOVE    DS01-EDEXCEL-PROG(WS01-INDEX) TO  WS03-EDEXCEL-PROG 
   	      MOVE    DS01-SCHEME(WS01-INDEX)       TO  WS03-SCHEME
    	      MOVE    DS01-EDEXCEL-E(WS01-INDEX)    TO  WS03-EDEXCEL-E            
    	      MOVE    DS01-LEVEL(WS01-INDEX)        TO  WS03-LEVEL
              MOVE    DS01-NCN(WS01-INDEX)          TO  WS03-NCN    
*                   
	      MOVE    SPACES             TO  WS03-UNIT-DETAILS
  	      WRITE   DB-DETAIL-RECORD FROM  WS03-DUMMY-RECORD
*
	      IF BTEC-NG
	      THEN
	        PERFORM J-MAINTAIN-SNST
	      END-IF

	      MOVE 'B' TO WS-REGN-TYPE
	      PERFORM M-INSERT-ODET

            END-IF
        END-PERFORM.
*EC2164
        SUBTRACT WS01-BTEC-BLOCKING-TOTAL FROM WS01-BTEC-ROWS-TOTAL 
                               GIVING WS01-BTEC-NO-BLOCK-TOTAL.
        MOVE WS01-BTEC-NO-BLOCK-TOTAL TO WS-TR-DET-BTEC-TOTAL.
        DISPLAY "BTEC FETCHED EXCLUDING BLOCKED", 
                    WS01-BTEC-NO-BLOCK-TOTAL WITH CONVERSION.
        DISPLAY "BTEC FETCHED", WS01-ROWS-TOTAL WITH CONVERSION.
*                                                                 
CA-EXIT.                              
*                                     
        EXIT.    
*                     
E-PROCESS-GNVQ SECTION.
**********************************************************************
*
*       Cursor used for ABS and KSQ achievements 
*
**********************************************************************
E-START.
*
*       EXEC SQL
*           DECLARE GET_GNVQ CURSOR FOR
*           SELECT  ST_REG_NO||'/'||substr(guni_id,length(guni_id)-1) st_reg_no,
*                   '                                   ',
*                   '                                   ',
*                   ST_COURSE_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*                   GUNI_QCA_UNIT_CODE,                       
*                   GUNI_SHORT_DESCRIPTION||' LEVEL '||
*       		TO_CHAR(GUNI_LEVEL_CODE) ,
*                   TO_CHAR(GSUN_KS_ACHIEVED_DATE , 'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
*       	    TO_CHAR(ST_BIRTH_DATE,'YYYY'),
*                   NVL(ST_SEX,'9'),
*       	   '3',
*                  ST_ULN,
*       	  decode(GSUN_KS_ACHIEVED_DATE,null,null,
*       		nvl(st_over_grade,'P  ')) sover,
*                  ' ',
*       	   '  ',
*                  'Pearson', 
*       	   '  ',
*       	   '  ',
*                    ' ' x
*             FROM  
*       	    STUDENTS,
*       	    GNVQ_STUDENT_UNITS,
*       	    GNVQ_UNITS
*       	WHERE ST_COURSE_ID in ('ABS','KSQ00')
*       	AND 1=2
*               and  st_delete is null                                               
*               AND  st_fallback is null          
*       	AND ST_REG_NO = GSUN_ST_REG_NO
*       	AND GSUN_GUNI_ID = GUNI_ID
*               AND  ST_UNIVERSITY_IND is null
*       	AND GSUN_CERT_NO IS NOT NULL  
*               AND not  EXISTS (select 1 from ks2000_assessment_results ww
*                       where ww.KARE_PROXY_IND='Y'
*                       and ww.kare_St_REg_no=gsun_st_reg_no 
*                       and ww.KARE_GUAT_GUNI_ID = GSUN_GUNI_ID 
*                      and trunc(ww.kare_log_Date)= trunc(GSUN_KS_ACHIEVED_DATE)          
*          	and not exists (Select 1 from  ks2000_assessment_results vv 
*                                   where nvl(vv.KARE_PROXY_IND,'N') !='Y'
*                                   and vv.kare_St_REg_no=gsun_st_reg_no 
*                                   and vv.KARE_GUAT_GUNI_ID = GSUN_GUNI_ID        
*                      and trunc(vv.kare_log_Date)= trunc(GSUN_KS_ACHIEVED_DATE)
*                                                 )
*                                 )
*               AND ( GSUN_KS_ACHIEVED_DATE
*               BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                       AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
*       	or (st_reg_date 
*               BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                       AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
*                and GSUN_KS_ACHIEVED_DATE is null)  
*          	)
*       	ORDER BY 1
*       END-EXEC.
*
        MOVE   "E: ERROR OPENING GNVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        DISPLAY "GNVQ START".
*       EXEC SQL
*           OPEN    GET_GNVQ
*       END-EXEC.
        CALL "SQLADR" USING SQ0005 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 294 TO SQL-OFFSET
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
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(3)
        MOVE 11 TO SQL-SQHSTL(3)
        MOVE 11 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(4)
        MOVE 11 TO SQL-SQHSTL(4)
        MOVE 11 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
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
            THEN GO TO ZZ-ABORT END-IF.
*
        PERFORM EA-FETCH-GNVQ
          UNTIL WS01-EOF.
*
        MOVE   "E: ERROR CLOSING GNVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
*       EXEC SQL
*           CLOSE   GET_GNVQ
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 325 TO SQL-OFFSET
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
            THEN GO TO ZZ-ABORT END-IF.
*
E-EXIT.
*
        EXIT.
*
EA-FETCH-GNVQ SECTION.
**********************************************************************
*
*       Get a set of GNVQ awards and process them.
*
**********************************************************************
EA-START.
*
        MOVE   "EA: ERROR FETCHING GNVQ AWARDS"
                                   TO  WS01-ERR-MESSAGE.
*       EXEC SQL
*           FETCH   GET_GNVQ
*            INTO  :DS01-CAND-ID,     
*                  :DS01-SURNAME,                                       
*                  :DS01-FORENAMES,
*                  :DS01-COURSE,                                                 
*                  :DS01-REG-DATE,                                      
*                  :DS01-QUAL-CODE,                                     
*                  :DS01-TITLE,                                         
*                  :DS01-AWD-DATE,                                      
*                  :DS01-CENTRE-ID,                                     
*                  :DS01-DOB,                                   
*                  :DS01-DOB-YEAR-EST,                                           
*                  :DS01-GENDER,
*                  :DS01-SPECIAL,                                        
*                  :DS01-ULN,                
*                  :DS01-RESULT,              
*                  :DS01-EDEXCEL-PROG,  
*                  :DS01-SCHEME,             
*                  :DS01-EDEXCEL-E, 
*   		   :DS01-LEVEL,                     
*                  :DS01-NCN                                        
*       END-EXEC.
        MOVE 500 TO SQL-ITERS
        MOVE 340 TO SQL-OFFSET
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
            DS01-CAND-ID IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(1)
        MOVE 13 TO SQL-SQHSTL(1)
        MOVE 13 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            DS01-SURNAME IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(2)
        MOVE 35 TO SQL-SQHSTL(2)
        MOVE 35 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            DS01-FORENAMES IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(3)
        MOVE 35 TO SQL-SQHSTL(3)
        MOVE 35 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            DS01-COURSE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(4)
        MOVE 8 TO SQL-SQHSTL(4)
        MOVE 8 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            DS01-REG-DATE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(5)
        MOVE 8 TO SQL-SQHSTL(5)
        MOVE 8 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            DS01-QUAL-CODE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(6)
        MOVE 12 TO SQL-SQHSTL(6)
        MOVE 12 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            DS01-TITLE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(7)
        MOVE 140 TO SQL-SQHSTL(7)
        MOVE 140 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            DS01-AWD-DATE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(8)
        MOVE 8 TO SQL-SQHSTL(8)
        MOVE 8 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            DS01-CENTRE-ID IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(9)
        MOVE 12 TO SQL-SQHSTL(9)
        MOVE 12 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            DS01-DOB IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(10)
        MOVE 8 TO SQL-SQHSTL(10)
        MOVE 8 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            DS01-DOB-YEAR-EST IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(11)
        MOVE 4 TO SQL-SQHSTL(11)
        MOVE 4 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            DS01-GENDER IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(12)
        MOVE 1 TO SQL-SQHSTL(12)
        MOVE 1 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            DS01-SPECIAL IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(13)
        MOVE 1 TO SQL-SQHSTL(13)
        MOVE 1 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            DS01-ULN IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(14)
        MOVE 10 TO SQL-SQHSTL(14)
        MOVE 10 TO SQL-SQHSTS(14)
        MOVE 0 TO SQL-SQINDV(14)
        MOVE 0 TO SQL-SQINDS(14)
        MOVE 0 TO SQL-SQHARM(14)
        CALL "SQLADR" USING
            DS01-RESULT IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(15)
        MOVE 3 TO SQL-SQHSTL(15)
        MOVE 3 TO SQL-SQHSTS(15)
        MOVE 0 TO SQL-SQINDV(15)
        MOVE 0 TO SQL-SQINDS(15)
        MOVE 0 TO SQL-SQHARM(15)
        CALL "SQLADR" USING
            DS01-EDEXCEL-PROG IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(16)
        MOVE 12 TO SQL-SQHSTL(16)
        MOVE 12 TO SQL-SQHSTS(16)
        MOVE 0 TO SQL-SQINDV(16)
        MOVE 0 TO SQL-SQINDS(16)
        MOVE 0 TO SQL-SQHARM(16)
        CALL "SQLADR" USING
            DS01-SCHEME IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(17)
        MOVE 12 TO SQL-SQHSTL(17)
        MOVE 12 TO SQL-SQHSTS(17)
        MOVE 0 TO SQL-SQINDV(17)
        MOVE 0 TO SQL-SQINDS(17)
        MOVE 0 TO SQL-SQHARM(17)
        CALL "SQLADR" USING
            DS01-EDEXCEL-E IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(18)
        MOVE 10 TO SQL-SQHSTL(18)
        MOVE 10 TO SQL-SQHSTS(18)
        MOVE 0 TO SQL-SQINDV(18)
        MOVE 0 TO SQL-SQINDS(18)
        MOVE 0 TO SQL-SQHARM(18)
        CALL "SQLADR" USING
            DS01-LEVEL IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(19)
        MOVE 2 TO SQL-SQHSTL(19)
        MOVE 2 TO SQL-SQHSTS(19)
        MOVE 0 TO SQL-SQINDV(19)
        MOVE 0 TO SQL-SQINDS(19)
        MOVE 0 TO SQL-SQHARM(19)
        CALL "SQLADR" USING
            DS01-NCN IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(20)
        MOVE 10 TO SQL-SQHSTL(20)
        MOVE 10 TO SQL-SQHSTS(20)
        MOVE 0 TO SQL-SQINDV(20)
        MOVE 0 TO SQL-SQINDS(20)
        MOVE 0 TO SQL-SQHARM(20)
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
            THEN GO TO ZZ-ABORT END-IF.
*
        IF  SQLCODE IS POSITIVE THEN
            SET     WS01-EOF       TO  TRUE
        END-IF.
        SUBTRACT WS01-ROWS-TOTAL FROM  SQLERRD(3)
                               GIVING  WS01-ROWS-THIS-FETCH.
        MOVE    SQLERRD(3)         TO  WS01-ROWS-TOTAL.
*
	MOVE WS01-ROWS-TOTAL TO WS-TR-DET-GNVQ-TOTAL.
        DISPLAY "GNVQ FETCHED ", WS01-ROWS-TOTAL WITH CONVERSION.
*
        PERFORM
        VARYING WS01-INDEX
           FROM 1 BY 1
          UNTIL WS01-INDEX > WS01-ROWS-THIS-FETCH
            MOVE    DS01-CAND-ID(WS01-INDEX)   TO  DS01-CAND-ID-S   
            MOVE    DS01-CAND-ID(WS01-INDEX)   TO  WS03-CAND-ID     
            MOVE    DS01-SURNAME(WS01-INDEX)   TO  WS03-SURNAME     
            MOVE    DS01-FORENAMES(WS01-INDEX) TO  WS03-FORENAMES   
            MOVE    DS01-REG-DATE(WS01-INDEX)  TO  WS03-REG-DATE    
            MOVE    DS01-QUAL-CODE(WS01-INDEX) TO  WS03-QUAL-CODE   
            MOVE    DS01-TITLE(WS01-INDEX)     TO  WS03-TITLE       
            MOVE    DS01-AWD-DATE(WS01-INDEX)  TO  WS03-AWD-DATE    
            MOVE    DS01-CENTRE-ID(WS01-INDEX) TO  WS03-CENTRE-ID   
            MOVE    DS01-DOB(WS01-INDEX)       TO  WS03-DOB         
       MOVE    DS01-DOB-YEAR-EST(WS01-INDEX) TO  WS03-DOB-YEAR-EST         
            MOVE    DS01-GENDER(WS01-INDEX)    TO  WS03-GENDER
            MOVE    DS01-SPECIAL(WS01-INDEX)   TO  WS03-SPECIAL
            MOVE    DS01-ULN(WS01-INDEX)       TO  WS03-ULN   
            MOVE    DS01-RESULT(WS01-INDEX)    TO  WS03-RESULT        
            MOVE    DS01-EDEXCEL-PROG(WS01-INDEX) TO WS03-EDEXCEL-PROG
            MOVE    DS01-SCHEME(WS01-INDEX) TO WS03-SCHEME 
            MOVE    DS01-EDEXCEL-E(WS01-INDEX) TO  WS03-EDEXCEL-E      
            MOVE    DS01-LEVEL(WS01-INDEX)     TO  WS03-LEVEL  
            MOVE    DS01-NCN(WS01-INDEX)       TO  WS03-NCN   
*      
            MOVE    SPACES             TO  WS03-UNIT-DETAILS
  	    WRITE   DG-DETAIL-RECORD FROM  WS03-DUMMY-RECORD
*
        END-PERFORM.
*                                     
EA-EXIT.                              
*                                     
        EXIT.    
*                     
G-PROCESS-NVQ SECTION.
**********************************************************************
*
*       Read the required NVQ courses and registrations. 
*
**********************************************************************
G-START.
*
*       EXEC SQL
*           DECLARE GET_NVQ CURSOR FOR
*           SELECT  ST_REG_NO,
*                   '                                   ',
*                   '                                   ',
*                   ST_NVQ_REGISTERED_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*                   nvl(nvq_qan_code,ST_NVQ_REGISTERED_ID),
*                   NVQ_TITLE_1||' '||NVQ_TITLE_2,
*                   TO_CHAR(ST_NVQ_CERTIFICATE_PRINT_DATE,'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '),
*                   TO_CHAR(ST_BIRTH_DATE,'YYYY'), 
*                   NVL(ST_SEX,'9'),
*       	   '3',
*                  ST_ULN,                                               
*                 decode(ST_NVQ_CERTIFICATE_PRINT_DATE,null,
*                           decode(st_withdrawn_ind,'Y',null,null),
*                           decode(st_fallback,'Y', 'U', 
*                       nvl(st_over_grade,'P  '))) sover,
*       	   ST_NVQ_ELIGIBILITY_CODE,
*                  st_nvq_Registered_id,
*       	   '  ',
*                  'Pearson',
*       	   '  ',
*       	   '  ',
*                   ' ' x
*             FROM  STUDENTS,
*                   NVQS                                                
*             WHERE (ST_NVQ_CERTIFICATE_PRINT_DATE
*                  BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                          AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*          or (st_reg_date
*                  BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                          AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
*              and ST_NVQ_CERTIFICATE_PRINT_DATE is null   )
*       	    )
*              AND  NVQ_ID = ST_NVQ_REGISTERED_ID||''    
*              AND  st_delete is null                  
*              AND  st_fallback is null                  
*       UNION
*           SELECT  ST_REG_NO,
*                   '                                   ',                    
*       	    '                                   ',
*                   ST_NVQ_REGISTERED_ID,
*                   TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
*                   NCUN_QAN_CODE,
*                   NCUN_TITLE_1||' '||NCUN_TITLE_2,
*                   TO_CHAR(ST_NVQ_CERTIFICATE_PRINT_DATE,'DDMMYYYY'),
*                   ST_CENTRE_ID,
*                   NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '),
*                   TO_CHAR(ST_BIRTH_DATE,'YYYY'), 
*                   NVL(ST_SEX,'9'),
*       	   '3',
*                  ST_ULN,
*                 decode(ST_NVQ_CERTIFICATE_PRINT_DATE,null,
*                           decode(st_withdrawn_ind,'Y',null,null),
*                           decode(st_fallback,'Y', 'U', 
*                       nvl(st_over_grade,'P  '))) sover,
*       	    ST_NVQ_ELIGIBILITY_CODE,
*                   NCUN_NCVQ_CODE,
*       	   '  ',
*                  'Pearson',                    
*       	   '  ',
*       	   '  ',
*       	   ' ' x
*             FROM  STUDENTS,
*                   NVQ_COMPETENCE_UNITS,
*                   NVQ_STUDENT_COMPETENCE_UNITS
*          WHERE (
*             (ST_NVQ_CERTIFICATE_PRINT_DATE                              
*                     BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                     AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')         
*                AND NSCU_ACHIEVED_YEAR is not null)
*             or (st_reg_date 
*                     BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
*                     AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')         
*                 and ST_NVQ_CERTIFICATE_PRINT_DATE is null   )
*        	)
*              AND NCUN_NCVQ_CODE     =  NSCU_NCUN_NCVQ_CODE 
*              AND NSCU_ST_REG_NO     =  ST_REG_NO 
*              AND NCUN_SECTOR_CODE between 890 and 896
*              AND  st_delete is null                  
*              AND  st_fallback is null                  
*              ORDER BY ST_REG_NO
*       END-EXEC.
*
        MOVE   "G: ERROR OPENING NVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        DISPLAY "NVQ  START".
*       EXEC SQL
*           OPEN    GET_NVQ
*       END-EXEC.
        CALL "SQLADR" USING SQ0006 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 435 TO SQL-OFFSET
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
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(3)
        MOVE 11 TO SQL-SQHSTL(3)
        MOVE 11 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(4)
        MOVE 11 TO SQL-SQHSTL(4)
        MOVE 11 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(5)
        MOVE 11 TO SQL-SQHSTL(5)
        MOVE 11 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(6)
        MOVE 11 TO SQL-SQHSTL(6)
        MOVE 11 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-START-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(7)
        MOVE 11 TO SQL-SQHSTL(7)
        MOVE 11 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-END-DATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(8)
        MOVE 11 TO SQL-SQHSTL(8)
        MOVE 11 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
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
            THEN GO TO ZZ-ABORT END-IF.
*
        PERFORM GA-FETCH-NVQ
          UNTIL WS01-EOF.
*
        MOVE   "G: ERROR CLOSING NVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
*       EXEC SQL
*           CLOSE   GET_NVQ
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 482 TO SQL-OFFSET
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
            THEN GO TO ZZ-ABORT END-IF.
*
G-EXIT.
*
        EXIT.
*
GA-FETCH-NVQ SECTION.
**********************************************************************
*
*       Get a set of NVQ awards and process them.
*
**********************************************************************
GA-START.
*
        MOVE   "GA: ERROR FETCHING NVQ AWARDS"
                                   TO  WS01-ERR-MESSAGE.
*       EXEC SQL
*           FETCH   GET_NVQ
*            INTO  :DS01-CAND-ID,     
*                  :DS01-SURNAME,                                       
*                  :DS01-FORENAMES,      
*                  :DS01-COURSE,                                       
*                  :DS01-REG-DATE,                                      
*                  :DS01-QUAL-CODE,                                     
*                  :DS01-TITLE,                                         
*                  :DS01-AWD-DATE,                                      
*                  :DS01-CENTRE-ID,                                     
*                  :DS01-DOB,                
*                  :DS01-DOB-YEAR-EST,                                           
*                  :DS01-GENDER,
*                  :DS01-SPECIAL,                                        
*                  :DS01-ULN,                
*                  :DS01-RESULT,              
*       	   :DS01-ELIG,                                             
*                  :DS01-EDEXCEL-PROG,    
*                  :DS01-SCHEME,           
*                  :DS01-EDEXCEL-E, 
*                  :DS01-LEVEL,                     
*                  :DS01-NCN
*       END-EXEC.
        MOVE 500 TO SQL-ITERS
        MOVE 497 TO SQL-OFFSET
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
            DS01-CAND-ID IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(1)
        MOVE 13 TO SQL-SQHSTL(1)
        MOVE 13 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            DS01-SURNAME IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(2)
        MOVE 35 TO SQL-SQHSTL(2)
        MOVE 35 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            DS01-FORENAMES IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(3)
        MOVE 35 TO SQL-SQHSTL(3)
        MOVE 35 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            DS01-COURSE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(4)
        MOVE 8 TO SQL-SQHSTL(4)
        MOVE 8 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            DS01-REG-DATE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(5)
        MOVE 8 TO SQL-SQHSTL(5)
        MOVE 8 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            DS01-QUAL-CODE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(6)
        MOVE 12 TO SQL-SQHSTL(6)
        MOVE 12 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            DS01-TITLE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(7)
        MOVE 140 TO SQL-SQHSTL(7)
        MOVE 140 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            DS01-AWD-DATE IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(8)
        MOVE 8 TO SQL-SQHSTL(8)
        MOVE 8 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            DS01-CENTRE-ID IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(9)
        MOVE 12 TO SQL-SQHSTL(9)
        MOVE 12 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            DS01-DOB IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(10)
        MOVE 8 TO SQL-SQHSTL(10)
        MOVE 8 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            DS01-DOB-YEAR-EST IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(11)
        MOVE 4 TO SQL-SQHSTL(11)
        MOVE 4 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            DS01-GENDER IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(12)
        MOVE 1 TO SQL-SQHSTL(12)
        MOVE 1 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            DS01-SPECIAL IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(13)
        MOVE 1 TO SQL-SQHSTL(13)
        MOVE 1 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            DS01-ULN IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(14)
        MOVE 10 TO SQL-SQHSTL(14)
        MOVE 10 TO SQL-SQHSTS(14)
        MOVE 0 TO SQL-SQINDV(14)
        MOVE 0 TO SQL-SQINDS(14)
        MOVE 0 TO SQL-SQHARM(14)
        CALL "SQLADR" USING
            DS01-RESULT IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(15)
        MOVE 3 TO SQL-SQHSTL(15)
        MOVE 3 TO SQL-SQHSTS(15)
        MOVE 0 TO SQL-SQINDV(15)
        MOVE 0 TO SQL-SQINDS(15)
        MOVE 0 TO SQL-SQHARM(15)
        CALL "SQLADR" USING
            DS01-ELIG IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(16)
        MOVE 1 TO SQL-SQHSTL(16)
        MOVE 1 TO SQL-SQHSTS(16)
        MOVE 0 TO SQL-SQINDV(16)
        MOVE 0 TO SQL-SQINDS(16)
        MOVE 0 TO SQL-SQHARM(16)
        CALL "SQLADR" USING
            DS01-EDEXCEL-PROG IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(17)
        MOVE 12 TO SQL-SQHSTL(17)
        MOVE 12 TO SQL-SQHSTS(17)
        MOVE 0 TO SQL-SQINDV(17)
        MOVE 0 TO SQL-SQINDS(17)
        MOVE 0 TO SQL-SQHARM(17)
        CALL "SQLADR" USING
            DS01-SCHEME IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(18)
        MOVE 12 TO SQL-SQHSTL(18)
        MOVE 12 TO SQL-SQHSTS(18)
        MOVE 0 TO SQL-SQINDV(18)
        MOVE 0 TO SQL-SQINDS(18)
        MOVE 0 TO SQL-SQHARM(18)
        CALL "SQLADR" USING
            DS01-EDEXCEL-E IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(19)
        MOVE 10 TO SQL-SQHSTL(19)
        MOVE 10 TO SQL-SQHSTS(19)
        MOVE 0 TO SQL-SQINDV(19)
        MOVE 0 TO SQL-SQINDS(19)
        MOVE 0 TO SQL-SQHARM(19)
        CALL "SQLADR" USING
            DS01-LEVEL IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(20)
        MOVE 2 TO SQL-SQHSTL(20)
        MOVE 2 TO SQL-SQHSTS(20)
        MOVE 0 TO SQL-SQINDV(20)
        MOVE 0 TO SQL-SQINDS(20)
        MOVE 0 TO SQL-SQHARM(20)
        CALL "SQLADR" USING
            DS01-NCN IN
            DS01-COURSE-DETAILS(1)
            SQL-SQHSTV(21)
        MOVE 10 TO SQL-SQHSTL(21)
        MOVE 10 TO SQL-SQHSTS(21)
        MOVE 0 TO SQL-SQINDV(21)
        MOVE 0 TO SQL-SQINDS(21)
        MOVE 0 TO SQL-SQHARM(21)
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
            THEN GO TO ZZ-ABORT END-IF.
*
        IF  SQLCODE IS POSITIVE THEN
            SET     WS01-EOF       TO  TRUE
        END-IF.
        SUBTRACT WS01-ROWS-TOTAL FROM  SQLERRD(3)
                               GIVING  WS01-ROWS-THIS-FETCH.
        MOVE    SQLERRD(3)         TO  WS01-ROWS-TOTAL.
*
	MOVE WS01-ROWS-TOTAL TO WS-TR-DET-NVQ-TOTAL.
        DISPLAY "NVQ  FETCHED ", WS01-ROWS-TOTAL WITH CONVERSION.
*
        PERFORM
        VARYING WS01-INDEX
           FROM 1 BY 1
          UNTIL WS01-INDEX > WS01-ROWS-THIS-FETCH
            MOVE    DS01-CAND-ID(WS01-INDEX)   TO  DS01-CAND-ID-S   
            MOVE    DS01-CAND-ID(WS01-INDEX)   TO  WS03-CAND-ID     
            MOVE    DS01-SURNAME(WS01-INDEX)   TO  WS03-SURNAME     
            MOVE    DS01-FORENAMES(WS01-INDEX) TO  WS03-FORENAMES   
            MOVE    DS01-REG-DATE(WS01-INDEX)  TO  WS03-REG-DATE    
            MOVE    DS01-QUAL-CODE(WS01-INDEX) TO  WS03-QUAL-CODE   
            MOVE    DS01-TITLE(WS01-INDEX)     TO  WS03-TITLE       
            MOVE    DS01-AWD-DATE(WS01-INDEX)  TO  WS03-AWD-DATE    
            MOVE    DS01-CENTRE-ID(WS01-INDEX) TO  WS03-CENTRE-ID   
            MOVE    DS01-DOB(WS01-INDEX)       TO  WS03-DOB         
        MOVE    DS01-DOB-YEAR-EST(WS01-INDEX) TO  WS03-DOB-YEAR-EST         
            MOVE    DS01-GENDER(WS01-INDEX)    TO  WS03-GENDER
            MOVE    DS01-SPECIAL(WS01-INDEX)   TO  WS03-SPECIAL
            MOVE    DS01-ULN(WS01-INDEX)       TO  WS03-ULN   
            MOVE    DS01-RESULT(WS01-INDEX)    TO  WS03-RESULT         
            MOVE    DS01-EDEXCEL-PROG(WS01-INDEX) TO  WS03-EDEXCEL-PROG
            MOVE    DS01-SCHEME(WS01-INDEX) TO  WS03-SCHEME 
            MOVE    DS01-EDEXCEL-E(WS01-INDEX) TO  WS03-EDEXCEL-E      
            MOVE    DS01-LEVEL(WS01-INDEX)     TO  WS03-LEVEL   
            MOVE    DS01-NCN(WS01-INDEX)       TO  WS03-NCN     
	    MOVE    DS01-ELIG(WS01-INDEX)      TO  WS-ELIG
*      
            MOVE    SPACES             TO  WS03-UNIT-DETAILS
  	    WRITE   DN-DETAIL-RECORD FROM  WS03-DUMMY-RECORD
*
	    MOVE 'N' TO WS-REGN-TYPE
	    PERFORM M-INSERT-ODET

        END-PERFORM.
*                                     
GA-EXIT.                              
*                                     
        EXIT.    
*                     
H-GET-GUEST-CENTRE SECTION.
**********************************************************************
* Retrieve Guest Centre if applicable
**********************************************************************
H-START.

*       EXEC SQL WHENEVER SQLERROR      GO TO H-050 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING    CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND     CONTINUE    END-EXEC.

*       EXEC SQL
*           SELECT  NVL(FN_GET_VOCATIONAL_GUEST_CENTRE
*                           (:WS-CANDIDATE,
*                            :WS-COURSE,
*                            :WS-ACADEMIC-YEAR),
*                       :WS-VOCATIONAL-CENTRE)
*           INTO    :WS-BATH-CENTRE
*           FROM    DUAL
*       END-EXEC.
        CALL "SQLADR" USING SQ0007 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 596 TO SQL-OFFSET
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
        CALL "SQLADR" USING
            WS-CANDIDATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 13 TO SQL-SQHSTL(1)
        MOVE 13 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-COURSE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(2)
        MOVE 8 TO SQL-SQHSTL(2)
        MOVE 8 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-ACADEMIC-YEAR IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(3)
        MOVE 4 TO SQL-SQHSTL(3)
        MOVE 4 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-VOCATIONAL-CENTRE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(4)
        MOVE 12 TO SQL-SQHSTL(4)
        MOVE 12 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-BATH-CENTRE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(5)
        MOVE 12 TO SQL-SQHSTL(5)
        MOVE 12 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
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
            THEN GO TO H-050 END-IF.

        GO TO H-EXIT.
H-050.
        MOVE   "H-050 : ERROR RETRIEVING GUEST CENTRE" TO WS01-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
H-EXIT.
        EXIT.
*
I-TERMINATE SECTION.                  
**********************************************************************
*                                     
*       Finish off and say goodbye.   
*                                     
**********************************************************************
I-START.                              
*
	MOVE WS-RUN-DATE TO WS-TR-H1-RUN-DATE.
	MOVE WS-TR-HEAD-1 TO TOT-RECORD.
	WRITE TOT-RECORD AFTER PAGE.
	MOVE WS-TR-HEAD-2 TO TOT-RECORD.
	WRITE TOT-RECORD AFTER 2.
        MOVE WS-START-DATE TO WS-TR-H3-START-DATE.
        MOVE WS-END-DATE   TO WS-TR-H3-END-DATE.
	MOVE WS-TR-HEAD-3 TO TOT-RECORD.
	WRITE TOT-RECORD AFTER 3.
	MOVE WS-TR-DETAIL TO TOT-RECORD.
	WRITE TOT-RECORD AFTER 5.
*
        CLOSE   DATA-FILE-B.
        CLOSE   DATA-FILE-G.
        CLOSE   DATA-FILE-N.
	CLOSE   TOTALS-REP.

	PERFORM K-MAINTAIN-DAUD.
I-040.
*       EXEC SQL WHENEVER SQLERROR   GO TO I-050 END-EXEC.

*       EXEC SQL COMMIT WORK                     END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 631 TO SQL-OFFSET
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
            THEN GO TO I-050 END-IF.

        GO TO I-100.
I-050.
        MOVE 'COMMIT WORK FAILED' TO WS01-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
I-100.

        DISPLAY "SSP996 - SUCCESSFULLY COMPLETED".

I-EXIT.

        EXIT.
*
J-MAINTAIN-SNST SECTION.
**********************************************************************
* Insert OR update STUDENT_NG_STATS - If learner already issued a
* certificate then set grading date to ST_AWARD_ISSUE else to the
* supplied date parameter.
**********************************************************************
J-START.

*       EXEC SQL WHENEVER SQLERROR	GO TO J-050 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

*       EXEC SQL
*       	UPDATE 	STUDENT_NG_STATS
*       	SET	SNST_FEED_4_GRADE = :WS-RESULT
*       	WHERE	SNST_ST_REG_NO	  = :WS-CANDIDATE	
*       END-EXEC.
        CALL "SQLADR" USING SQ0009 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 646 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-RESULT IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 3 TO SQL-SQHSTL(1)
        MOVE 3 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-CANDIDATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(2)
        MOVE 13 TO SQL-SQHSTL(2)
        MOVE 13 TO SQL-SQHSTS(2)
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
            THEN GO TO J-050 END-IF.

*       EXEC SQL WHENEVER SQLERROR	GO TO J-060 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

*       EXEC SQL
*       	INSERT INTO STUDENT_NG_STATS
*       	(SNST_ST_REG_NO,
*       	 SNST_ORIGIN,
*       	 SNST_FEED_4_GRADE,
*       	 SNST_INSERT_DATE
*       	)
*       	SELECT	:WS-CANDIDATE,
*       		4,
*       		:WS-RESULT,
*       		SYSDATE
*       	FROM	DUAL
*       	WHERE NOT EXISTS (SELECT NULL
*       			  FROM	 STUDENT_NG_STATS
*       			  WHERE  SNST_ST_REG_NO = :WS-CANDIDATE
*       			 )
*       END-EXEC.
        CALL "SQLADR" USING SQ0010 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 669 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-CANDIDATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 13 TO SQL-SQHSTL(1)
        MOVE 13 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RESULT IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(2)
        MOVE 3 TO SQL-SQHSTL(2)
        MOVE 3 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-CANDIDATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(3)
        MOVE 13 TO SQL-SQHSTL(3)
        MOVE 13 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
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
            THEN GO TO J-060 END-IF.

	GO TO J-EXIT.
J-050.
        MOVE   "J-050 : ERROR UPDATING STUDENT_NG_STATS" TO WS01-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
J-060.
        MOVE   "J-060 : ERROR INSERTING STUDENT_NG_STATS" TO WS01-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
J-EXIT.
        EXIT.
*
K-MAINTAIN-DAUD SECTION.
**********************************************************************
* Datafeed Audits
**********************************************************************
K-START.

*       EXEC SQL WHENEVER SQLERROR	GO TO K-040 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF START-OF-RUN
	THEN
*         EXEC SQL
*           SELECT DAUD_SEQ.NEXTVAL
*           INTO   :WS-DAUD-SEQUENCE
*           FROM   DUAL			
*         END-EXEC
          CALL "SQLADR" USING SQ0011 SQL-STMT
          MOVE 1 TO SQL-ITERS
          MOVE 696 TO SQL-OFFSET
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
          CALL "SQLADR" USING
              WS-DAUD-SEQUENCE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(1)
          MOVE 4 TO SQL-SQHSTL(1)
          MOVE 4 TO SQL-SQHSTS(1)
          MOVE 0 TO SQL-SQINDV(1)
          MOVE 0 TO SQL-SQINDS(1)
          MOVE 0 TO SQL-SQHARM(1)
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
              THEN GO TO K-040 END-IF
	END-IF.

*       EXEC SQL WHENEVER SQLERROR	GO TO K-050 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF START-OF-RUN
	THEN 
*         EXEC SQL
*       	INSERT INTO DATAFEED_AUDITS
*       	(DAUD_SEQUENCE,
*       	 DAUD_MODULE,
*       	 DAUD_MODULE_DESCR,
*       	 DAUD_START,
*       	 DAUD_END,
*       	 DAUD_AWARDED_FROM,
*       	 DAUD_AWARDED_TO,
*       	 DAUD_ACADEMIC_YEAR,
*       	 DAUD_BTEC_NG_GRADING_DATE,
*       	 DAUD_REGNS_FROM,
*        	 DAUD_REGNS_TO,
*       	 DAUD_OFQUAL_FROM,
*       	 DAUD_OFQUAL_TO
*       	)
*       	VALUES
*       	(
*       	 :WS-DAUD-SEQUENCE,
*       	 'SSD996',
*       	 'OFQUAL',
*       	 SYSDATE,
*       	 NULL,
*       	 :WS-START-DATE,
*       	 :WS-END-DATE,
*       	 :WS-ACADEMIC-YEAR,
*       	 TO_DATE(:WS-NG-GRADING-DATE,'DDMMYYYY'),
*       	 :WS-START-DATE,
*       	 :WS-END-DATE,
*       	 NULL,
*       	 NULL
*       	)
*         END-EXEC
          CALL "SQLADR" USING SQ0012 SQL-STMT
          MOVE 1 TO SQL-ITERS
          MOVE 715 TO SQL-OFFSET
          MOVE 0 TO SQL-OCCURS
          CALL "SQLADR" USING
              SQLCUD
              SQL-CUD
          CALL "SQLADR" USING
              SQLCA
              SQL-SQLEST
          MOVE 4352 TO SQL-SQLETY
          CALL "SQLADR" USING
              WS-DAUD-SEQUENCE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(1)
          MOVE 4 TO SQL-SQHSTL(1)
          MOVE 4 TO SQL-SQHSTS(1)
          MOVE 0 TO SQL-SQINDV(1)
          MOVE 0 TO SQL-SQINDS(1)
          MOVE 0 TO SQL-SQHARM(1)
          CALL "SQLADR" USING
              WS-START-DATE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(2)
          MOVE 11 TO SQL-SQHSTL(2)
          MOVE 11 TO SQL-SQHSTS(2)
          MOVE 0 TO SQL-SQINDV(2)
          MOVE 0 TO SQL-SQINDS(2)
          MOVE 0 TO SQL-SQHARM(2)
          CALL "SQLADR" USING
              WS-END-DATE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(3)
          MOVE 11 TO SQL-SQHSTL(3)
          MOVE 11 TO SQL-SQHSTS(3)
          MOVE 0 TO SQL-SQINDV(3)
          MOVE 0 TO SQL-SQINDS(3)
          MOVE 0 TO SQL-SQHARM(3)
          CALL "SQLADR" USING
              WS-ACADEMIC-YEAR IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(4)
          MOVE 4 TO SQL-SQHSTL(4)
          MOVE 4 TO SQL-SQHSTS(4)
          MOVE 0 TO SQL-SQINDV(4)
          MOVE 0 TO SQL-SQINDS(4)
          MOVE 0 TO SQL-SQHARM(4)
          CALL "SQLADR" USING
              WS-NG-GRADING-DATE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(5)
          MOVE 8 TO SQL-SQHSTL(5)
          MOVE 8 TO SQL-SQHSTS(5)
          MOVE 0 TO SQL-SQINDV(5)
          MOVE 0 TO SQL-SQINDS(5)
          MOVE 0 TO SQL-SQHARM(5)
          CALL "SQLADR" USING
              WS-START-DATE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(6)
          MOVE 11 TO SQL-SQHSTL(6)
          MOVE 11 TO SQL-SQHSTS(6)
          MOVE 0 TO SQL-SQINDV(6)
          MOVE 0 TO SQL-SQINDS(6)
          MOVE 0 TO SQL-SQHARM(6)
          CALL "SQLADR" USING
              WS-END-DATE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(7)
          MOVE 11 TO SQL-SQHSTL(7)
          MOVE 11 TO SQL-SQHSTS(7)
          MOVE 0 TO SQL-SQINDV(7)
          MOVE 0 TO SQL-SQINDS(7)
          MOVE 0 TO SQL-SQHARM(7)
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
              THEN GO TO K-050 END-IF

* Commit program start and parameters...

*         EXEC SQL
*           COMMIT WORK
*         END-EXEC
          MOVE 1 TO SQL-ITERS
          MOVE 758 TO SQL-OFFSET
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
              THEN GO TO K-050 END-IF
	END-IF.

*       EXEC SQL WHENEVER SQLERROR	GO TO K-060 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF END-OF-RUN
	THEN 
*         EXEC SQL
*           UPDATE DATAFEED_AUDITS
*           SET	   DAUD_END = SYSDATE
*           WHERE  DAUD_SEQUENCE = :WS-DAUD-SEQUENCE
*         END-EXEC
          CALL "SQLADR" USING SQ0014 SQL-STMT
          MOVE 1 TO SQL-ITERS
          MOVE 773 TO SQL-OFFSET
          MOVE 0 TO SQL-OCCURS
          CALL "SQLADR" USING
              SQLCUD
              SQL-CUD
          CALL "SQLADR" USING
              SQLCA
              SQL-SQLEST
          MOVE 4352 TO SQL-SQLETY
          CALL "SQLADR" USING
              WS-DAUD-SEQUENCE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(1)
          MOVE 4 TO SQL-SQHSTL(1)
          MOVE 4 TO SQL-SQHSTS(1)
          MOVE 0 TO SQL-SQINDV(1)
          MOVE 0 TO SQL-SQINDS(1)
          MOVE 0 TO SQL-SQHARM(1)
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
              THEN GO TO K-060 END-IF
	END-IF.

	GO TO K-EXIT.
K-040.
        MOVE   "K-040 : ERROR RETRIEVING DAUD SEQUENCE" TO WS01-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
K-050.
        MOVE   "K-050 : ERROR INSERTING DATAFEED_AUDITS" TO WS01-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
K-060.
        MOVE   "K-060 : ERROR UPDATING DATAFEED_AUDITS" TO WS01-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
K-EXIT.
        EXIT.
*
L-CHECK-BLOCKING SECTION.
**********************************************************************
* For un-certificated BTEC NG do not output learner if blocked.
**********************************************************************
L-START.

*       EXEC SQL WHENEVER SQLERROR	GO TO L-040 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	MOVE +0 TO WS-STATUS-I

*       EXEC SQL EXECUTE
*         BEGIN
*           PK_OSCA1.PR_Q_CERT_STATUS
*                             (:WS-CANDIDATE,NULL,
*                              :WS-STATUS:WS-STATUS-I,
*                              :WS-STATUS2:WS-STATUS2-I,
*                              :WS-DECISION-DATE:WS-DECISION-DATE-I,
*                              :WS-MESSAGE:WS-MESSAGE-I);
*         END;
*       END-EXEC
        CALL "SQLADR" USING SQ0015 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 792 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-CANDIDATE IN
            DS01-COURSE-DETAILS
            SQL-SQHSTV(1)
        MOVE 13 TO SQL-SQHSTL(1)
        MOVE 13 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-STATUS
            SQL-SQHSTV(2)
        MOVE 1 TO SQL-SQHSTL(2)
        MOVE 1 TO SQL-SQHSTS(2)
        CALL "SQLADR" USING
            WS-STATUS-I
            SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-STATUS2
            SQL-SQHSTV(3)
        MOVE 10 TO SQL-SQHSTL(3)
        MOVE 10 TO SQL-SQHSTS(3)
        CALL "SQLADR" USING
            WS-STATUS2-I
            SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-DECISION-DATE
            SQL-SQHSTV(4)
        MOVE 11 TO SQL-SQHSTL(4)
        MOVE 11 TO SQL-SQHSTS(4)
        CALL "SQLADR" USING
            WS-DECISION-DATE-I
            SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-MESSAGE
            SQL-SQHSTV(5)
        MOVE 100 TO SQL-SQHSTL(5)
        MOVE 100 TO SQL-SQHSTS(5)
        CALL "SQLADR" USING
            WS-MESSAGE-I
            SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
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
            THEN GO TO L-040 END-IF

*	display WS-CANDIDATE  ' - STATUS' WS-STATUS

	IF WS-STATUS-I EQUAL -1
          MOVE '7' TO WS-STATUS
        END-IF

	IF WS-STATUS NOT = '0'
	THEN
 	  MOVE 'Y' TO WS-BLOCK-IND
*	  DISPLAY "    " WS-CANDIDATE
          ADD 1 TO WS01-BTEC-BLOCKING-TOTAL
	END-IF

	GO TO L-EXIT.
L-040.
        MOVE   "L-040 : ERROR CHECKING BLOCKING" TO WS01-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
L-EXIT.
        EXIT.
*
M-INSERT-ODET SECTION.
**********************************************************************
* Insert a row into OFQUAL-DETAILS of each output record ready for
* production of summary report.
**********************************************************************
M-START.

*       EXEC SQL WHENEVER SQLERROR	GO TO M-050 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF WS-REGN-TYPE = 'B'
	THEN
*         EXEC SQL
*       	INSERT INTO OFQUAL_DETAILS
*       	(ODET_ID,
*       	 ODET_DAUD_ID,
*       	 ODET_REG_NO,
*       	 ODET_REG_TYPE,
*       	 ODET_BTEC_NG,
*       	 ODET_COURSE,
*       	 ODET_REG_DATE,
*       	 ODET_ISSUE_DATE,
*       	 ODET_ELIG,
*       	 ODET_OFQUAL_GRADE
*       	)
*       	VALUES
*       	(
* 		ODET_SEQ.NEXTVAL,
*       	:WS-DAUD-SEQUENCE,
*       	:WS03-CAND-ID,
*       	:WS-REGN-TYPE,
*       	DECODE(:WS-REC-TYPE,'G','Y','N'),
*       	:WS-COURSE,
*       	TO_DATE(:WS03-REG-DATE,'DDMMYYYY'),
*       	TO_DATE(:WS03-AWD-DATE,'DDMMYYYY'),
*       	:WS-ELIG,
*       	:WS03-RESULT
* 		)
*         END-EXEC
          CALL "SQLADR" USING SQ0016 SQL-STMT
          MOVE 1 TO SQL-ITERS
          MOVE 827 TO SQL-OFFSET
          MOVE 0 TO SQL-OCCURS
          CALL "SQLADR" USING
              SQLCUD
              SQL-CUD
          CALL "SQLADR" USING
              SQLCA
              SQL-SQLEST
          MOVE 4352 TO SQL-SQLETY
          CALL "SQLADR" USING
              WS-DAUD-SEQUENCE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(1)
          MOVE 4 TO SQL-SQHSTL(1)
          MOVE 4 TO SQL-SQHSTS(1)
          MOVE 0 TO SQL-SQINDV(1)
          MOVE 0 TO SQL-SQINDS(1)
          MOVE 0 TO SQL-SQHARM(1)
          CALL "SQLADR" USING
              WS03-CAND-ID IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(2)
          MOVE 13 TO SQL-SQHSTL(2)
          MOVE 13 TO SQL-SQHSTS(2)
          MOVE 0 TO SQL-SQINDV(2)
          MOVE 0 TO SQL-SQINDS(2)
          MOVE 0 TO SQL-SQHARM(2)
          CALL "SQLADR" USING
              WS-REGN-TYPE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(3)
          MOVE 1 TO SQL-SQHSTL(3)
          MOVE 1 TO SQL-SQHSTS(3)
          MOVE 0 TO SQL-SQINDV(3)
          MOVE 0 TO SQL-SQINDS(3)
          MOVE 0 TO SQL-SQHARM(3)
          CALL "SQLADR" USING
              WS-REC-TYPE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(4)
          MOVE 1 TO SQL-SQHSTL(4)
          MOVE 1 TO SQL-SQHSTS(4)
          MOVE 0 TO SQL-SQINDV(4)
          MOVE 0 TO SQL-SQINDS(4)
          MOVE 0 TO SQL-SQHARM(4)
          CALL "SQLADR" USING
              WS-COURSE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(5)
          MOVE 8 TO SQL-SQHSTL(5)
          MOVE 8 TO SQL-SQHSTS(5)
          MOVE 0 TO SQL-SQINDV(5)
          MOVE 0 TO SQL-SQINDS(5)
          MOVE 0 TO SQL-SQHARM(5)
          CALL "SQLADR" USING
              WS03-REG-DATE IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(6)
          MOVE 8 TO SQL-SQHSTL(6)
          MOVE 8 TO SQL-SQHSTS(6)
          MOVE 0 TO SQL-SQINDV(6)
          MOVE 0 TO SQL-SQINDS(6)
          MOVE 0 TO SQL-SQHARM(6)
          CALL "SQLADR" USING
              WS03-AWD-DATE IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(7)
          MOVE 8 TO SQL-SQHSTL(7)
          MOVE 8 TO SQL-SQHSTS(7)
          MOVE 0 TO SQL-SQINDV(7)
          MOVE 0 TO SQL-SQINDS(7)
          MOVE 0 TO SQL-SQHARM(7)
          CALL "SQLADR" USING
              WS-ELIG IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(8)
          MOVE 1 TO SQL-SQHSTL(8)
          MOVE 1 TO SQL-SQHSTS(8)
          MOVE 0 TO SQL-SQINDV(8)
          MOVE 0 TO SQL-SQINDS(8)
          MOVE 0 TO SQL-SQHARM(8)
          CALL "SQLADR" USING
              WS03-RESULT IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(9)
          MOVE 3 TO SQL-SQHSTL(9)
          MOVE 3 TO SQL-SQHSTS(9)
          MOVE 0 TO SQL-SQINDV(9)
          MOVE 0 TO SQL-SQINDS(9)
          MOVE 0 TO SQL-SQHARM(9)
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
              THEN GO TO M-050 END-IF
	ELSE
*         EXEC SQL
*       	INSERT INTO OFQUAL_DETAILS
*       	(ODET_ID,
*       	 ODET_DAUD_ID,
*       	 ODET_REG_NO,
*       	 ODET_REG_TYPE,
*       	 ODET_BTEC_NG,
*       	 ODET_COURSE,
*       	 ODET_REG_DATE,
*       	 ODET_ISSUE_DATE,
*       	 ODET_ELIG,
*       	 ODET_OFQUAL_GRADE
*       	)
*       	VALUES
*       	(
* 		ODET_SEQ.NEXTVAL,
*       	:WS-DAUD-SEQUENCE,
* 		:WS03-CAND-ID,
*       	:WS-REGN-TYPE,     
*       	'N',
*       	:WS03-EDEXCEL-PROG,
*       	TO_DATE(:WS03-REG-DATE,'DDMMYYYY'),
*       	TO_DATE(:WS03-AWD-DATE,'DDMMYYYY'),
*       	:WS-ELIG,
*       	:WS03-RESULT
* 		)
*         END-EXEC
          CALL "SQLADR" USING SQ0017 SQL-STMT
          MOVE 1 TO SQL-ITERS
          MOVE 878 TO SQL-OFFSET
          MOVE 0 TO SQL-OCCURS
          CALL "SQLADR" USING
              SQLCUD
              SQL-CUD
          CALL "SQLADR" USING
              SQLCA
              SQL-SQLEST
          MOVE 4352 TO SQL-SQLETY
          CALL "SQLADR" USING
              WS-DAUD-SEQUENCE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(1)
          MOVE 4 TO SQL-SQHSTL(1)
          MOVE 4 TO SQL-SQHSTS(1)
          MOVE 0 TO SQL-SQINDV(1)
          MOVE 0 TO SQL-SQINDS(1)
          MOVE 0 TO SQL-SQHARM(1)
          CALL "SQLADR" USING
              WS03-CAND-ID IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(2)
          MOVE 13 TO SQL-SQHSTL(2)
          MOVE 13 TO SQL-SQHSTS(2)
          MOVE 0 TO SQL-SQINDV(2)
          MOVE 0 TO SQL-SQINDS(2)
          MOVE 0 TO SQL-SQHARM(2)
          CALL "SQLADR" USING
              WS-REGN-TYPE IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(3)
          MOVE 1 TO SQL-SQHSTL(3)
          MOVE 1 TO SQL-SQHSTS(3)
          MOVE 0 TO SQL-SQINDV(3)
          MOVE 0 TO SQL-SQINDS(3)
          MOVE 0 TO SQL-SQHARM(3)
          CALL "SQLADR" USING
              WS03-EDEXCEL-PROG IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(4)
          MOVE 12 TO SQL-SQHSTL(4)
          MOVE 12 TO SQL-SQHSTS(4)
          MOVE 0 TO SQL-SQINDV(4)
          MOVE 0 TO SQL-SQINDS(4)
          MOVE 0 TO SQL-SQHARM(4)
          CALL "SQLADR" USING
              WS03-REG-DATE IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(5)
          MOVE 8 TO SQL-SQHSTL(5)
          MOVE 8 TO SQL-SQHSTS(5)
          MOVE 0 TO SQL-SQINDV(5)
          MOVE 0 TO SQL-SQINDS(5)
          MOVE 0 TO SQL-SQHARM(5)
          CALL "SQLADR" USING
              WS03-AWD-DATE IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(6)
          MOVE 8 TO SQL-SQHSTL(6)
          MOVE 8 TO SQL-SQHSTS(6)
          MOVE 0 TO SQL-SQINDV(6)
          MOVE 0 TO SQL-SQINDS(6)
          MOVE 0 TO SQL-SQHARM(6)
          CALL "SQLADR" USING
              WS-ELIG IN
              DS01-COURSE-DETAILS
              SQL-SQHSTV(7)
          MOVE 1 TO SQL-SQHSTL(7)
          MOVE 1 TO SQL-SQHSTS(7)
          MOVE 0 TO SQL-SQINDV(7)
          MOVE 0 TO SQL-SQINDS(7)
          MOVE 0 TO SQL-SQHARM(7)
          CALL "SQLADR" USING
              WS03-RESULT IN
              WS03-DUMMY-RECORD
              SQL-SQHSTV(8)
          MOVE 3 TO SQL-SQHSTL(8)
          MOVE 3 TO SQL-SQHSTS(8)
          MOVE 0 TO SQL-SQINDV(8)
          MOVE 0 TO SQL-SQINDS(8)
          MOVE 0 TO SQL-SQHARM(8)
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
              THEN GO TO M-050 END-IF
	END-IF

	GO TO M-EXIT.
M-050.
        MOVE   "M-050 : ERROR INSERTING OFQUAL_DETAILS" TO WS01-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
M-EXIT.
        EXIT.
*
ZZ-ABORT SECTION.
**********************************************************************
*
*       Report error and expire.
*
**********************************************************************
ZZ-START.
*
        DISPLAY "SSP996 ERROR".
        DISPLAY WS01-ERR-MESSAGE.
	DISPLAY DS01-CAND-ID(WS01-INDEX)    
	DISPLAY DS01-SURNAME(WS01-INDEX)    
	DISPLAY DS01-FORENAMES(WS01-INDEX)  
	DISPLAY DS01-REG-DATE(WS01-INDEX)   
	DISPLAY DS01-QUAL-CODE(WS01-INDEX)  
	DISPLAY DS01-TITLE(WS01-INDEX)      
	DISPLAY DS01-AWD-DATE(WS01-INDEX)   
	DISPLAY DS01-CENTRE-ID(WS01-INDEX)  

        IF  SQLCODE IS NOT ZERO THEN
            DISPLAY SQLERRMC
        END-IF.

*       EXEC SQL WHENEVER SQLERROR   GO TO ZZ-050 END-EXEC.

*       EXEC SQL ROLLBACK WORK                    END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 925 TO SQL-OFFSET
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

        GO TO ZZ-EXIT.

ZZ-050.
        MOVE 'FAILED DURING PROGRAM ABORT' TO WS01-ERR-MESSAGE.

        CALL   "SYS$EXIT"
          USING BY VALUE WS01-ABORT.
*
ZZ-EXIT.
*
        STOP RUN.
