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
        EXEC SQL BEGIN DECLARE SECTION END-EXEC.
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
    03  WS-DAUD-SEQUENCE	  PIC S9(09) COMP.
    03  WS-START-OR-END		  PIC  X(1).
        88  START-OF-RUN			VALUE "S".
        88  END-OF-RUN				VALUE "E".
    03 WS-REGN-TYPE		  PIC X(1).
*
01  WS-BLOCK-IND		  PIC  X(1).
    88  BLOCKED					VALUE "Y".
01  WS-STATUS			  PIC X(1).
01  WS-STATUS-I                   PIC S9(4) COMP.
01  WS-STATUS2			  PIC X(10).
01  WS-STATUS2-I                  PIC S9(4) COMP.
01  WS-DECISION-DATE              PIC X(11).
01  WS-DECISION-DATE-I            PIC S9(4) COMP.
01  WS-MESSAGE			  PIC X(100).
01  WS-MESSAGE-I                  PIC S9(4) COMP.

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
        EXEC SQL END DECLARE SECTION END-EXEC.
*
        EXEC SQL INCLUDE SQLCA END-EXEC.
*
01      WS01-GENERAL-STORAGE.
    03  WS01-ERR-MESSAGE          PIC  X(80).
    03  WS01-EOF-IND              PIC  X(01).
        88  WS01-EOF                         VALUE "Y".
    03  WS01-ROWS-TOTAL           PIC S9(09) COMP.
    03  WS01-ROWS-THIS-FETCH      PIC S9(09) COMP.
    03  WS01-BTEC-TOTAL           PIC S9(09) COMP.
    03  WS01-GNVQ-TOTAL           PIC S9(09) COMP.
    03  WS01-NVQ-TOTAL            PIC S9(09) COMP.
    03  WS01-INDEX                PIC S9(09) COMP.
    03  WS01-ROWS-2               PIC S9(04) COMP.
    03  WS01-INDEX-2              PIC S9(04) COMP.
    03  WS01-ABORT                PIC  9(09) COMP VALUE 4.
*EC2164
    03  WS01-BTEC-NO-BLOCK-TOTAL  PIC S9(09) COMP.
    03  WS01-BTEC-BLOCKING-TOTAL  PIC S9(09) COMP. 
    03  WS01-BTEC-ROWS-TOTAL      PIC S9(09) COMP.     
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
        EXEC SQL WHENEVER SQLERROR GO TO   ZZ-ABORT END-EXEC.
*
        MOVE   "ERROR CONNECTING TO DATABASE"
                                   TO  WS01-ERR-MESSAGE.
        EXEC SQL
                CONNECT :DS03-USERNAME
                IDENTIFIED BY :DS03-PASSWORD
        END-EXEC.
*
	EXEC SQL
            SELECT TO_CHAR(SYSDATE,'DD-MON-YYYY') 
              INTO WS-RUN-DATE
              FROM DUAL
	END-EXEC.

        MOVE   "ERROR CALCULATING FIRST ENTRY DATE"
                                   TO  WS01-ERR-MESSAGE.
	EXEC SQL
          SELECT TO_CHAR(FN_FIRST_ENTRY_DATE(TRUNC(SYSDATE)),'DDMMYYYY')
          INTO   WS-NG-GRADING-DATE
          FROM DUAL
	END-EXEC.
*
	EXEC SQL
            TRUNCATE TABLE OFQUAL_DETAILS
	END-EXEC.

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
        EXEC SQL
            DECLARE GET_BTEC CURSOR FOR
            SELECT  ST_REG_NO,
                    '                                   ',
                    '                                   ',
                    ST_COURSE_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
		    AW_AWARD_CODE||AA_BTEC_TITLE||'XX',
                    AT_NAME,
                    TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
                    TO_CHAR(ST_BIRTH_DATE,'YYYY'), 
                    DECODE(ST_SEX,'F','F','M','M','U'),
		   '3',
		   ST_ULN,
		  decode(ST_AWARD_ISSUE,null,
			    decode(st_withdrawn_ind,'Y',null,null),
		            decode(st_fallback,'Y', 'U', 
			nvl(st_over_grade,'P  '))) sover,
		   AA_BTEC_TITLE,
		   '  ',
		   'Pearson',
		    '  ',
		    ' ',
		    DECODE(AC_BNM_TYPE,'G','G','B'),
		    'Y',
		    ST_AWARD_ELIG                            
              FROM  
                    APPROVAL_APPLICATION,
                    APPROVAL_AWARDS,
		    AWARD_CODES,
                    AWARD_TITLES,
		    STUDENTS X
             WHERE  AW_AWARD_CODE IN ('01', '02', '03',
                                      '04', '05', '06', '11')
               AND  st_delete is null                                               
               AND  st_fallback is null                                               
               AND  ST_COURSE_ID||'' = AW_COURSE_NUMBER
               AND  AA_APPLICAT_NO = AW_APPLICAT_NO+0
               AND  ST_UNIVERSITY_IND is null
	       AND  AC_CODE = AW_AWARD_CODE	  
               AND  TO_NUMBER(AA_BTEC_TITLE) = AT_NUMBER
               AND (  ST_AWARD_ISSUE 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                         AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
                or (st_reg_date 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                         AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
                and ST_AWARD_ISSUE is null)
            	    )
		AND NOT EXISTS (SELECT 	NULL
				FROM	AWARD_CODES,
					APPROVAL_AWARDS,
					STUDENTS
				WHERE	AC_CODE = AW_AWARD_CODE
				AND	AW_COURSE_NUMBER = ST_COURSE_ID
				AND	ST_REG_NO =  X.ST_REG_NO
				AND	AC_BNM_TYPE = 'G'
				AND	ST_AWARD_ISSUE IS NULL
				AND 	ST_BNM_GRADE IS NOT NULL
			       )	
		AND   (	
			NOT EXISTS (SELECT	NULL
				    FROM	STUDENT_NG_STATS
				    WHERE	SNST_ST_REG_NO = ST_REG_NO
				   )
		       OR   EXISTS (SELECT	NULL
				    FROM	STUDENT_NG_STATS
				    WHERE	SNST_ST_REG_NO = ST_REG_NO
				    AND		SNST_FEED_4_GRADE IS NULL
				   )
		      )
	UNION
            SELECT  ST_REG_NO,
                    '                                   ',
                    '                                   ',
                    ST_COURSE_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
 		    nvl(AACO_QCA_CODE,AW_AWARD_CODE||AA_BTEC_TITLE||'XX'),      
                    AT_NAME,
                    TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
		    TO_CHAR(ST_BIRTH_DATE,'YYYY'),
                    NVL(ST_SEX,'9'),
		   '3',
                   ST_ULN,		                               
		decode(to_char(ST_AWARD_ISSUE),null,
		decode(st_withdrawn_ind,'Y',null,null),              
		     decode(st_fallback,'Y','U', 
		nvl( nvl(st_over_grade, ST_BNM_GRADE) ,'P  '))
		      ) sover,
		   AA_BTEC_TITLE,
		   '  ',
		   'Pearson',
		    '  ',
		    ' ',
		    DECODE(AC_BNM_TYPE,'G','G','B'),
		    'Y',
		    ST_AWARD_ELIG                            
              FROM  
                    APPROVAL_APPLICATION,
                    APPROVAL_AWARDS,
		    AWARD_CODES,
                    AWARD_TITLES,
		    AT_AWARD_CODES,
		    STUDENTS X
              WHERE AW_AWARD_CODE NOT IN 
                                ( '01','02','03','04','05','06','11',
                                  '27','64','35','63','93','HH','GG',
				  'GA','GB','CC','EW','FS','FO')
              AND   st_delete is null   
              AND   st_fallback is null                                                      
              AND   AW_AWARD_CODE = AACO_AC_CODE
	      AND   AC_CODE = AW_AWARD_CODE	  
	      AND   AT_NUMBER = AACO_AT_NUMBER
              AND   ST_COURSE_ID||'' = AW_COURSE_NUMBER
              AND   AA_APPLICAT_NO = AW_APPLICAT_NO+0
              AND   TO_NUMBER(AA_BTEC_TITLE) = AT_NUMBER
	      AND   ST_UNIVERSITY_IND is null  
              AND ( ST_AWARD_ISSUE                                  
                 	BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
                    OR  ( ST_REG_DATE 
                 	    BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                            AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
                            AND ST_AWARD_ISSUE IS NULL)
                    )
	      AND NOT EXISTS   (SELECT 	NULL
				FROM	AWARD_CODES,
					APPROVAL_AWARDS,
					STUDENTS
				WHERE	AC_CODE = AW_AWARD_CODE
				AND	AW_COURSE_NUMBER = ST_COURSE_ID
				AND	ST_REG_NO =  X.ST_REG_NO
				AND	AC_BNM_TYPE = 'G'
				AND	ST_AWARD_ISSUE IS NULL
				AND 	ST_BNM_GRADE IS NOT NULL
			       )	
	      AND   (	
			NOT EXISTS (SELECT	NULL
				    FROM	STUDENT_NG_STATS
				    WHERE	SNST_ST_REG_NO = ST_REG_NO
				   )
		        OR  EXISTS (SELECT	NULL
				    FROM	STUDENT_NG_STATS
				    WHERE	SNST_ST_REG_NO = ST_REG_NO
				    AND		SNST_FEED_4_GRADE IS NULL
				   )
		    )
	UNION
            SELECT  ST_REG_NO,
                    '                                   ',
                    '                                   ',
                    ST_COURSE_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
                    nvl(acpa_qca_lu_x_code,AW_AWARD_CODE||AA_BTEC_TITLE||'XX'),   
                    AT_NAME,
                    TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
                    TO_CHAR(ST_BIRTH_DATE,'YYYY'),
                    NVL(ST_SEX,'9'),
                   '3',
                   '  ',
                   decode(st_over_grade,null,
                   decode(st_withdrawn_ind,'Y',' ',
         decode(aw_award_code,'63',decode(st_award_issue,null,null,'P') ,' ')),
                   decode(st_award_issue,null,' ',
                    decode(st_fallback,'Y', 'U', 'E'||nvl(st_over_grade, 
                    replace(ST_ABS_LEVEL_ACHIEVED,'E',null)) ))),
                   AA_BTEC_TITLE,
                   '  ',
                   'Pearson',
                   decode(nvl(st_over_grade,ST_ABS_LEVEL_ACHIEVED),null,
         'XX','E'||nvl(st_over_grade,replace(ST_ABS_LEVEL_ACHIEVED,'E',null))),
		   ' ',
                   'O',
		   'Y',
		    ST_AWARD_ELIG                            
        FROM    students, 
                approval_awards,     
                approval_application,
                approval_comb_packages,
                award_titles
        WHERE   aa_applicat_no  = aw_applicat_no
        and     st_course_id    = aw_course_number       
        and  	aw_award_code   in ('35','63','HH')
        and     aa_btec_title   = at_number                                     
        and     st_delete is null                
        AND  	st_fallback is null                                         
        and     acpa_at_number  = at_number
               AND ( ST_AWARD_ISSUE 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                         AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
                 or  ( st_reg_date 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                         AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
                         and ST_AWARD_ISSUE  is null)
                    ) 
	UNION
            SELECT  ST_REG_NO,
                    '                                   ',
                    '                                   ',
                    ST_COURSE_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
                    AACL_QCA_CODE,   
                    AT_NAME,
                    TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
                    TO_CHAR(ST_BIRTH_DATE,'YYYY'),
                    NVL(ST_SEX,'9'),
		   '3',
                   ST_ULN,
                   decode(nvl(st_bnm_grade,st_over_grade),null,
                   decode(st_withdrawn_ind,'Y',' ',' '),
                   decode(st_award_issue,null,' ',
               decode(aw_award_code,'CC','P','EW','P','GA','P','GB','P',   
 				    'FO','P',
                   'E'||nvl(st_over_grade,     
       	     replace(nvl(ST_ABS_LEVEL_ACHIEVED,st_bnm_grade),'E',null) )))),           
                   AA_BTEC_TITLE,
                   '  ',
                   'Pearson',                                     
		  decode(aw_award_code,'CC',' ','EW',' ','GA',' ','GB',' ',  
				       'FO',' ',
		'E'||nvl(st_over_grade,replace(nvl(ST_ABS_LEVEL_ACHIEVED,
			st_bnm_grade),'E',null))),
		   ' ',
                    'O',
		    'Y',
		    ST_AWARD_ELIG                            
        FROM    students, 
                approval_awards,     
                approval_application,
                AT_AWARD_CODE_levels,
                award_titles
        WHERE  
             	AW_AWARD_CODE = AACL_AC_CODE
        and  	AT_NUMBER = AACL_AT_NUMBER
	and    	nvl(st_bnm_grade,ST_ABS_LEVEL_ACHIEVED) =  AACL_LEVEL 
	and     aa_applicat_no  = aw_applicat_no
        and     st_course_id    = aw_course_number       
        and  	aw_award_code   in ('93','CC','EW','GG','GA','GB',
				    'FS','FO')
        and     aa_btec_title   = at_number                                     
        and     st_delete is null                
        AND  	st_fallback is null                                         
        AND 	ST_AWARD_ISSUE 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                         AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
	UNION
            SELECT  ST_REG_NO,
                    '                                   ',
                    '                                   ',
                    ST_COURSE_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
                    decode(aw_award_code,'GA','10064187','GB','10064199',null),    
                    AT_NAME,
                    TO_CHAR(ST_AWARD_ISSUE, 'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
                    TO_CHAR(ST_BIRTH_DATE,'YYYY'),
                    NVL(ST_SEX,'9'),
                   '3',
                   ST_ULN,                                
		   null,
                   AA_BTEC_TITLE,
                   '  ',
                   'Pearson',
                   'XX',
                   ' ',
                   'O',
		    'Y',
		    ST_AWARD_ELIG                            
        FROM    students, 
                approval_awards,     
                approval_application,
                award_titles
        WHERE   aa_applicat_no  = aw_applicat_no
        and     st_course_id    = aw_course_number       
        and     aw_award_code   in ('93','CC','EW','GG','GA','GB',
				    'FS','FO')
        and     aa_btec_title   = at_number                                     
        and     st_delete is null                
        and     st_fallback is null                                         
        and    st_reg_date 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                         AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
                         and ST_AWARD_ISSUE  is null                    

	UNION
        SELECT	ST_REG_NO,
                '                                   ',
                '                                   ',
		ST_COURSE_ID,
		TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
		NVL(AACO_QCA_CODE,AW_AWARD_CODE||AA_BTEC_TITLE||'XX'),      
                AT_NAME,
		:WS-NG-GRADING-DATE,
                ST_CENTRE_ID,
                NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
		TO_CHAR(ST_BIRTH_DATE,'YYYY'),
                NVL(ST_SEX,'9'),
		'3',
                ST_ULN,	
                --Code modified for EC4363 TA Embargo 2020 Starts--	                               
		--ST_BNM_GRADE, 
                DECODE(aw_award_code,'Y6',
                DECODE(FN_CHECK_TA_OVERGRADE_VISIBLE(ST_REG_NO),
                'Y',ST_BNM_GRADE,NULL),ST_BNM_GRADE),
                --Code modified for EC4363 TA Embargo 2020 Ends--
		AA_BTEC_TITLE,
		'  ',
		'Pearson',
		'  ',
		' ',
		'G',
		'N',
		ST_AWARD_ELIG                            
	FROM	  
		STUDENTS,
		AT_AWARD_CODES,
		AWARD_TITLES,
		APPROVAL_APPLICATION,
		AWARD_CODES,
		APPROVAL_AWARDS
	WHERE	AACO_QCA_CODE 	   IS NOT NULL
	AND     AACO_AC_CODE 		= AW_AWARD_CODE
	AND	AACO_AT_NUMBER 		= AT_NUMBER
	AND	AT_NUMBER 		= TO_NUMBER(AA_BTEC_TITLE)
	AND	AA_APPLICAT_NO 		= AW_APPLICAT_NO+0
	AND	ST_DELETE 	       IS NULL                                               
	AND	ST_FALLBACK 	       IS NULL
	AND	ST_UNIVERSITY_IND      IS NULL
	AND	ST_AWARD_ISSUE	       IS NULL
	AND	ST_BNM_GRADE 	   IS NOT NULL
	AND	ST_COURSE_ID 		= AW_COURSE_NUMBER
	AND	AC_BNM_TYPE		= 'G'
	AND	AC_CODE			= AW_AWARD_CODE	
	AND   (	
		NOT EXISTS (SELECT	NULL
			    FROM	STUDENT_NG_STATS
			    WHERE	SNST_ST_REG_NO = ST_REG_NO
			   )
		OR   EXISTS (SELECT	NULL
			     FROM	STUDENT_NG_STATS
			     WHERE	SNST_ST_REG_NO = ST_REG_NO
			     AND	SNST_FEED_4_GRADE IS NULL
			    )
	      )
        ORDER BY 1
        END-EXEC.
*
        MOVE   "C: ERROR OPENING BTEC COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        DISPLAY "BTEC START".
*        DISPLAY "  BLOCKED LEARNERS".

        EXEC SQL
            OPEN    GET_BTEC
        END-EXEC.
*                                                            
        PERFORM CA-FETCH-BTEC
          UNTIL WS01-EOF.
*
        DISPLAY "TOTAL BTEC FETCHED ", WS01-ROWS-TOTAL WITH CONVERSION.
        DISPLAY "".

        MOVE   "C: ERROR CLOSING BTEC COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        EXEC SQL
            CLOSE   GET_BTEC
        END-EXEC.
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
        EXEC SQL
            FETCH   GET_BTEC
             INTO  :DS01-CAND-ID,     
                   :DS01-SURNAME,                                       
                   :DS01-FORENAMES,                                     
                   :DS01-COURSE,   
                   :DS01-REG-DATE,                                      
                   :DS01-QUAL-CODE,                                     
                   :DS01-TITLE,                                         
                   :DS01-AWD-DATE,                                      
                   :DS01-CENTRE-ID,                                     
                   :DS01-DOB,       
		   :DS01-DOB-YEAR-EST,                                      
                   :DS01-GENDER,        
    		   :DS01-SPECIAL,           
    		   :DS01-ULN,           
    		   :DS01-RESULT,             
    		   :DS01-EDEXCEL-PROG,       
    		   :DS01-SCHEME,      
    		   :DS01-EDEXCEL-E,      
		   :DS01-LEVEL,
                   :DS01-NCN,
		   :DS01-REC-TYPE,
		   :DS01-CERT-IND,
		   :DS01-ELIG
        END-EXEC.            
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
        EXEC SQL
            DECLARE GET_GNVQ CURSOR FOR
            SELECT  ST_REG_NO||'/'||substr(guni_id,length(guni_id)-1) st_reg_no,
                    '                                   ',
                    '                                   ',
                    ST_COURSE_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
                    GUNI_QCA_UNIT_CODE,                       
                    GUNI_SHORT_DESCRIPTION||' LEVEL '||
			TO_CHAR(GUNI_LEVEL_CODE) ,
                    TO_CHAR(GSUN_KS_ACHIEVED_DATE , 'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '), 
		    TO_CHAR(ST_BIRTH_DATE,'YYYY'),
                    NVL(ST_SEX,'9'),
		   '3',
                   ST_ULN,
		  decode(GSUN_KS_ACHIEVED_DATE,null,null,
			nvl(st_over_grade,'P  ')) sover,
                   ' ',
		   '  ',
                   'Pearson', 
		   '  ',
		   '  ',
                     ' ' x
              FROM  
		    STUDENTS,
		    GNVQ_STUDENT_UNITS,
		    GNVQ_UNITS
		WHERE ST_COURSE_ID in ('ABS','KSQ00')
		AND 1=2
	        and  st_delete is null                                               
                AND  st_fallback is null          
		AND ST_REG_NO = GSUN_ST_REG_NO
		AND GSUN_GUNI_ID = GUNI_ID
                AND  ST_UNIVERSITY_IND is null
		AND GSUN_CERT_NO IS NOT NULL  
                AND not  EXISTS (select 1 from ks2000_assessment_results ww
                        where ww.KARE_PROXY_IND='Y'
                        and ww.kare_St_REg_no=gsun_st_reg_no 
                        and ww.KARE_GUAT_GUNI_ID = GSUN_GUNI_ID 
                       and trunc(ww.kare_log_Date)= trunc(GSUN_KS_ACHIEVED_DATE)          
	   	and not exists (Select 1 from  ks2000_assessment_results vv 
                                    where nvl(vv.KARE_PROXY_IND,'N') !='Y'
                                    and vv.kare_St_REg_no=gsun_st_reg_no 
                                    and vv.KARE_GUAT_GUNI_ID = GSUN_GUNI_ID        
                       and trunc(vv.kare_log_Date)= trunc(GSUN_KS_ACHIEVED_DATE)
                                                  )
                                  )
                AND ( GSUN_KS_ACHIEVED_DATE
                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
        	or (st_reg_date 
                BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                        AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')
                 and GSUN_KS_ACHIEVED_DATE is null)  
           	)
        	ORDER BY 1
        END-EXEC.
*
        MOVE   "E: ERROR OPENING GNVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        DISPLAY "GNVQ START".
        EXEC SQL
            OPEN    GET_GNVQ
        END-EXEC.
*
        PERFORM EA-FETCH-GNVQ
          UNTIL WS01-EOF.
*
        MOVE   "E: ERROR CLOSING GNVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        EXEC SQL
            CLOSE   GET_GNVQ
        END-EXEC.
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
        EXEC SQL
            FETCH   GET_GNVQ
             INTO  :DS01-CAND-ID,     
                   :DS01-SURNAME,                                       
                   :DS01-FORENAMES,
                   :DS01-COURSE,                                                 
                   :DS01-REG-DATE,                                      
                   :DS01-QUAL-CODE,                                     
                   :DS01-TITLE,                                         
                   :DS01-AWD-DATE,                                      
                   :DS01-CENTRE-ID,                                     
                   :DS01-DOB,                                   
                   :DS01-DOB-YEAR-EST,                                           
                   :DS01-GENDER,
                   :DS01-SPECIAL,                                        
                   :DS01-ULN,                
                   :DS01-RESULT,              
                   :DS01-EDEXCEL-PROG,  
                   :DS01-SCHEME,             
                   :DS01-EDEXCEL-E, 
    		   :DS01-LEVEL,                     
                   :DS01-NCN                                        
        END-EXEC.            
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
        EXEC SQL
            DECLARE GET_NVQ CURSOR FOR
            SELECT  ST_REG_NO,
                    '                                   ',
                    '                                   ',
                    ST_NVQ_REGISTERED_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
                    nvl(nvq_qan_code,ST_NVQ_REGISTERED_ID),
                    NVQ_TITLE_1||' '||NVQ_TITLE_2,
                    TO_CHAR(ST_NVQ_CERTIFICATE_PRINT_DATE,'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '),
                    TO_CHAR(ST_BIRTH_DATE,'YYYY'), 
                    NVL(ST_SEX,'9'),
		   '3',
                   ST_ULN,                                               
                  decode(ST_NVQ_CERTIFICATE_PRINT_DATE,null,
                            decode(st_withdrawn_ind,'Y',null,null),
                            decode(st_fallback,'Y', 'U', 
                        nvl(st_over_grade,'P  '))) sover,
		   ST_NVQ_ELIGIBILITY_CODE,
                   st_nvq_Registered_id,
		   '  ',
                   'Pearson',
		   '  ',
		   '  ',
                    ' ' x
              FROM  STUDENTS,
                    NVQS                                                
              WHERE (ST_NVQ_CERTIFICATE_PRINT_DATE
                   BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                           AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
           or (st_reg_date
                   BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                           AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR') 
               and ST_NVQ_CERTIFICATE_PRINT_DATE is null   )
		    )
               AND  NVQ_ID = ST_NVQ_REGISTERED_ID||''    
               AND  st_delete is null                  
               AND  st_fallback is null                  
	UNION
            SELECT  ST_REG_NO,
                    '                                   ',                    
		    '                                   ',
                    ST_NVQ_REGISTERED_ID,
                    TO_CHAR(ST_REG_DATE, 'DDMMYYYY'),
                    NCUN_QAN_CODE,
                    NCUN_TITLE_1||' '||NCUN_TITLE_2,
                    TO_CHAR(ST_NVQ_CERTIFICATE_PRINT_DATE,'DDMMYYYY'),
                    ST_CENTRE_ID,
                    NVL(TO_CHAR(ST_BIRTH_DATE,'DDMMYYYY'),'        '),
                    TO_CHAR(ST_BIRTH_DATE,'YYYY'), 
                    NVL(ST_SEX,'9'),
		   '3',
                   ST_ULN,
                  decode(ST_NVQ_CERTIFICATE_PRINT_DATE,null,
                            decode(st_withdrawn_ind,'Y',null,null),
                            decode(st_fallback,'Y', 'U', 
                        nvl(st_over_grade,'P  '))) sover,
		    ST_NVQ_ELIGIBILITY_CODE,
                    NCUN_NCVQ_CODE,
		   '  ',
                   'Pearson',                    
		   '  ',
		   '  ',
		   ' ' x
              FROM  STUDENTS,
                    NVQ_COMPETENCE_UNITS,
                    NVQ_STUDENT_COMPETENCE_UNITS
           WHERE (
              (ST_NVQ_CERTIFICATE_PRINT_DATE                              
                      BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                      AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')         
                 AND NSCU_ACHIEVED_YEAR is not null)
              or (st_reg_date 
                      BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-RRRR') 
                      AND     TO_DATE(:WS-END-DATE,'DD-MON-RRRR')         
                  and ST_NVQ_CERTIFICATE_PRINT_DATE is null   )
         	)
               AND NCUN_NCVQ_CODE     =  NSCU_NCUN_NCVQ_CODE 
               AND NSCU_ST_REG_NO     =  ST_REG_NO 
               AND NCUN_SECTOR_CODE between 890 and 896
               AND  st_delete is null                  
               AND  st_fallback is null                  
               ORDER BY ST_REG_NO
        END-EXEC.
*
        MOVE   "G: ERROR OPENING NVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        DISPLAY "NVQ  START".
        EXEC SQL
            OPEN    GET_NVQ
        END-EXEC.
*
        PERFORM GA-FETCH-NVQ
          UNTIL WS01-EOF.
*
        MOVE   "G: ERROR CLOSING NVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        EXEC SQL
            CLOSE   GET_NVQ
        END-EXEC.
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
        EXEC SQL
            FETCH   GET_NVQ
             INTO  :DS01-CAND-ID,     
                   :DS01-SURNAME,                                       
                   :DS01-FORENAMES,      
                   :DS01-COURSE,                                       
                   :DS01-REG-DATE,                                      
                   :DS01-QUAL-CODE,                                     
                   :DS01-TITLE,                                         
                   :DS01-AWD-DATE,                                      
                   :DS01-CENTRE-ID,                                     
                   :DS01-DOB,                
                   :DS01-DOB-YEAR-EST,                                           
                   :DS01-GENDER,
                   :DS01-SPECIAL,                                        
                   :DS01-ULN,                
                   :DS01-RESULT,              
		   :DS01-ELIG,                                             
                   :DS01-EDEXCEL-PROG,    
                   :DS01-SCHEME,           
                   :DS01-EDEXCEL-E, 
                   :DS01-LEVEL,                     
                   :DS01-NCN
        END-EXEC.            
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

        EXEC SQL WHENEVER SQLERROR      GO TO H-050 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING    CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND     CONTINUE    END-EXEC.

        EXEC SQL
            SELECT  NVL(FN_GET_VOCATIONAL_GUEST_CENTRE
                            (:WS-CANDIDATE,
                             :WS-COURSE,
                             :WS-ACADEMIC-YEAR),
                        :WS-VOCATIONAL-CENTRE)
            INTO    :WS-BATH-CENTRE
            FROM    DUAL
        END-EXEC.

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
        EXEC SQL WHENEVER SQLERROR   GO TO I-050 END-EXEC.

        EXEC SQL COMMIT WORK                     END-EXEC.

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

        EXEC SQL WHENEVER SQLERROR	GO TO J-050 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

        EXEC SQL
		UPDATE 	STUDENT_NG_STATS
		SET	SNST_FEED_4_GRADE = :WS-RESULT
		WHERE	SNST_ST_REG_NO	  = :WS-CANDIDATE	
        END-EXEC.

        EXEC SQL WHENEVER SQLERROR	GO TO J-060 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

        EXEC SQL
		INSERT INTO STUDENT_NG_STATS
		(SNST_ST_REG_NO,
		 SNST_ORIGIN,
		 SNST_FEED_4_GRADE,
		 SNST_INSERT_DATE
		)
		SELECT	:WS-CANDIDATE,
			4,
			:WS-RESULT,
			SYSDATE
		FROM	DUAL
		WHERE NOT EXISTS (SELECT NULL
				  FROM	 STUDENT_NG_STATS
				  WHERE  SNST_ST_REG_NO = :WS-CANDIDATE
				 )
        END-EXEC.

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

        EXEC SQL WHENEVER SQLERROR	GO TO K-040 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF START-OF-RUN
	THEN
          EXEC SQL
	    SELECT DAUD_SEQ.NEXTVAL
	    INTO   :WS-DAUD-SEQUENCE
	    FROM   DUAL			
          END-EXEC
	END-IF.

        EXEC SQL WHENEVER SQLERROR	GO TO K-050 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF START-OF-RUN
	THEN 
          EXEC SQL
		INSERT INTO DATAFEED_AUDITS
		(DAUD_SEQUENCE,
		 DAUD_MODULE,
		 DAUD_MODULE_DESCR,
		 DAUD_START,
		 DAUD_END,
		 DAUD_AWARDED_FROM,
		 DAUD_AWARDED_TO,
		 DAUD_ACADEMIC_YEAR,
		 DAUD_BTEC_NG_GRADING_DATE,
		 DAUD_REGNS_FROM,
	 	 DAUD_REGNS_TO,
		 DAUD_OFQUAL_FROM,
		 DAUD_OFQUAL_TO
		)
		VALUES
		(
		 :WS-DAUD-SEQUENCE,
		 'SSD996',
		 'OFQUAL',
		 SYSDATE,
		 NULL,
		 :WS-START-DATE,
		 :WS-END-DATE,
		 :WS-ACADEMIC-YEAR,
		 TO_DATE(:WS-NG-GRADING-DATE,'DDMMYYYY'),
		 :WS-START-DATE,
		 :WS-END-DATE,
		 NULL,
		 NULL
		)
          END-EXEC

* Commit program start and parameters...

	  EXEC SQL
	    COMMIT WORK
	  END-EXEC
	END-IF.

        EXEC SQL WHENEVER SQLERROR	GO TO K-060 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF END-OF-RUN
	THEN 
          EXEC SQL
	    UPDATE DATAFEED_AUDITS
	    SET	   DAUD_END = SYSDATE
	    WHERE  DAUD_SEQUENCE = :WS-DAUD-SEQUENCE
          END-EXEC
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

        EXEC SQL WHENEVER SQLERROR	GO TO L-040 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	MOVE +0 TO WS-STATUS-I

        EXEC SQL EXECUTE
          BEGIN
            PK_OSCA1.PR_Q_CERT_STATUS
                              (:WS-CANDIDATE,NULL,
                               :WS-STATUS:WS-STATUS-I,
                               :WS-STATUS2:WS-STATUS2-I,
                               :WS-DECISION-DATE:WS-DECISION-DATE-I,
                               :WS-MESSAGE:WS-MESSAGE-I);
          END;
        END-EXEC

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

        EXEC SQL WHENEVER SQLERROR	GO TO M-050 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING	CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND	CONTINUE    END-EXEC.

	IF WS-REGN-TYPE = 'B'
	THEN
          EXEC SQL
		INSERT INTO OFQUAL_DETAILS
		(ODET_ID,
		 ODET_DAUD_ID,
		 ODET_REG_NO,
		 ODET_REG_TYPE,
		 ODET_BTEC_NG,
		 ODET_COURSE,
		 ODET_REG_DATE,
		 ODET_ISSUE_DATE,
		 ODET_ELIG,
		 ODET_OFQUAL_GRADE
		)
		VALUES
		(
  		ODET_SEQ.NEXTVAL,
		:WS-DAUD-SEQUENCE,
		:WS03-CAND-ID,
		:WS-REGN-TYPE,
		DECODE(:WS-REC-TYPE,'G','Y','N'),
		:WS-COURSE,
		TO_DATE(:WS03-REG-DATE,'DDMMYYYY'),
		TO_DATE(:WS03-AWD-DATE,'DDMMYYYY'),
		:WS-ELIG,
		:WS03-RESULT
  		)
          END-EXEC
	ELSE
          EXEC SQL
		INSERT INTO OFQUAL_DETAILS
		(ODET_ID,
		 ODET_DAUD_ID,
		 ODET_REG_NO,
		 ODET_REG_TYPE,
		 ODET_BTEC_NG,
		 ODET_COURSE,
		 ODET_REG_DATE,
		 ODET_ISSUE_DATE,
		 ODET_ELIG,
		 ODET_OFQUAL_GRADE
		)
		VALUES
		(
  		ODET_SEQ.NEXTVAL,
		:WS-DAUD-SEQUENCE,
  		:WS03-CAND-ID,
		:WS-REGN-TYPE,     
		'N',
		:WS03-EDEXCEL-PROG,
		TO_DATE(:WS03-REG-DATE,'DDMMYYYY'),
		TO_DATE(:WS03-AWD-DATE,'DDMMYYYY'),
		:WS-ELIG,
		:WS03-RESULT
  		)
          END-EXEC
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

        EXEC SQL WHENEVER SQLERROR   GO TO ZZ-050 END-EXEC.

        EXEC SQL ROLLBACK WORK                    END-EXEC.

        GO TO ZZ-EXIT.

ZZ-050.
        MOVE 'FAILED DURING PROGRAM ABORT' TO WS01-ERR-MESSAGE.

        CALL   "SYS$EXIT"
          USING BY VALUE WS01-ABORT.
*
ZZ-EXIT.
*
        STOP RUN.
