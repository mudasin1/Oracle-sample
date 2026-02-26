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
	EXEC SQL
		BEGIN DECLARE SECTION
	END-EXEC.
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
	EXEC SQL
		END DECLARE SECTION
	END-EXEC.
*
	EXEC SQL
		INCLUDE SQLCA
	END-EXEC.
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
	03  WS-PAGE-COUNT		PIC 9(4) COMP VALUE 0.
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
	EXEC SQL
		DECLARE CURSOR_1 CURSOR FOR
		select st_centre_id,
		       cn_centre_name,
		       nvl(st_course_id,nvl(st_nvq_registered_id
		                           ,st_gnvq_registered_id
		                           )
		          ),
		       nvl(ac_description
                          ,decode(gnvq_glev_code
		                 ,null,decode(nl_code
                                             ,null,'INDIVIDUAL UNITS'
		                             ,'NVQ '||nl_description
		                             )
		                 ,'GNVQ '||glev_description
		                 )
		          ),
		       nvl(at_name,nvl(nvq_title_1,gnvq_title_1)),
		       st_reg_no,
		       rpad(st_forenames||' '||st_surname
                           ,47 - nvl(length(st_ucas_ref),0)
                           ) || st_ucas_ref,
		       barb_npp_id,
		       npp_name,
		       decode(barb_reason_code
		             ,'1','CENTRE DEFERRED'
		             ,'2','STUDENT DEFERRED'
		             ,'3','CENTRE DEFERRED'
		             ,'4','NO EV REPORT'
		             ,'5','EV DECISION'
                                 ||decode(max(barb_decision_date)
                                         ,null,null
                                         ,' ('||to_char(max(barb_decision_date)
                                                       ,'DD-MON-YY'
                                                       ) || ')'
                                         )
		             ,'6','QS INTERVENTION'
		             ,'9','AMR BLOCK'         
		             ,'UNKNOWN REASON'
		             ),
		       decode(min(barb_claim_type)
                             ,'A','FULL'
                             ,'F','FALLBACK'
                             ,'I','INTERIM'
                             ,'UNKNOWN'
                             ),
		       to_char(trunc(barb_date),'DD-MON-YY'),
                       barb_reason_code
		 from  students
		      ,centres
		      ,npps
		      ,nvqs
		      ,nvq_levels
		      ,gnvqs
		      ,gnvq_levels
		      ,approval_awards
		      ,approval_application
		      ,award_titles
		      ,award_codes
		      ,bnm_awards_run_blocks
		 where nvl(barb_cleared_ind,'N') = 'N'
		  and  st_reg_no                 = barb_reg_no
		  and  cn_centre_id              = st_centre_id
		  and  npp_id(+)                 = barb_npp_id
		  and  nvq_id(+)                 = st_nvq_registered_id
		  and  nl_code(+)                = nvq_level_code
		  and  gnvq_id(+)                = st_gnvq_registered_id
		  and  glev_code(+)              = gnvq_glev_code
		  and  aw_course_number(+)       = st_course_id
		  and  aa_applicat_no(+)         = aw_applicat_no
		  and  at_number(+)              = aa_btec_title
		  and  ac_code(+)                = aw_award_code
                  and (:ws-run-type              = 'C'
                   or  barb_report_ind           = 'Y'
                      )
		  and st_delete is null
		group by st_centre_id
		        ,cn_centre_name
		        ,nvl(st_course_id,nvl(st_nvq_registered_id
                                             ,st_gnvq_registered_id
                                             )
                            )
		        ,nvl(ac_description
                            ,decode(gnvq_glev_code
		                   ,null,decode(nl_code
                                               ,null,'INDIVIDUAL UNITS'
		                               ,'NVQ '||nl_description
		                               )
  		                   ,'GNVQ '||glev_description
		                   )
		            )
		        ,nvl(at_name,nvl(nvq_title_1,gnvq_title_1))
		        ,st_reg_no
		        ,st_forenames
		        ,st_surname
		        ,st_ucas_ref
		        ,barb_npp_id
		        ,npp_name
                        ,barb_reason_code
		        ,decode(barb_reason_code
		               ,'1','CENTRE DEFERRED'
		               ,'2','STUDENT DEFERRED'
		               ,'3','CENTRE DEFERRED'
  		               ,'4','NO EV REPORT'
    		               ,'5','EV DECISION'
		               ,'6','QS INTERVENTION'
		               ,'9','AMR BLOCK'         
		               ,'UNKNOWN REASON'
		               )
		        ,trunc(barb_date)
                order by decode(:ws-sort-order,'R',barb_reason_code,'X'),
                         st_centre_id,
		         nvl(st_course_id,nvl(st_nvq_registered_id
		                             ,st_gnvq_registered_id
		                             )
		            ),
    		         st_reg_no
	END-EXEC.
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
	EXEC SQL
		WHENEVER SQLERROR
			GO TO BC-050
	END-EXEC.
*
	EXEC SQL
		CONNECT :WS-USER-ID IDENTIFIED BY :WS-PASSWORD
	END-EXEC.
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
	EXEC SQL
		WHENEVER SQLERROR
			GO TO BD-050
	END-EXEC.
*
	EXEC SQL
		WHENEVER SQLWARNING
			CONTINUE
	END-EXEC.
*
	EXEC SQL OPEN CURSOR_1 END-EXEC.
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
	EXEC SQL
		WHENEVER SQLERROR
			GO TO E-030
	END-EXEC.
*
	EXEC SQL
		WHENEVER SQLWARNING
			CONTINUE
	END-EXEC.
*
	EXEC SQL
		WHENEVER NOT FOUND
			CONTINUE
	END-EXEC.
*
* ONLY UPDATE BARB IF PARAM SAYS YES.
*
        IF WS-UPDATE-BARB = 'Y'
           EXEC SQL
                UPDATE  BNM_AWARDS_RUN_BLOCKS
                SET     BARB_REPORT_IND = 'N'
                WHERE   BARB_REPORT_IND = 'Y'
           END-EXEC
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
	EXEC SQL
		WHENEVER SQLERROR
			GO TO F-050
	END-EXEC.
*
	EXEC SQL CLOSE CURSOR_1 END-EXEC.
*
	EXEC SQL
		COMMIT WORK
	END-EXEC.
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
	EXEC SQL
		WHENEVER SQLERROR
			GO TO XA-060
	END-EXEC.
*
	EXEC SQL
		WHENEVER SQLWARNING
			CONTINUE
	END-EXEC.
*
	EXEC SQL
		WHENEVER NOT FOUND
			GO TO XA-030
	END-EXEC.
*
	EXEC SQL
	    FETCH CURSOR_1 INTO :WS-CENTRE-NO,
			        :WS-CENTRE-NAME,
        			:WS-COURSE-NVQ-ID,
	        		:WS-AC-DESC,
		        	:WS-AT-NAME,
			        :WS-REG-NO,
        			:WS-NAME-UCAS-REF,
                                :WS-NPP-ID,
                                :WS-NPP-NAME,
                                :WS-REASON,
                                :WS-CLAIM-TYPE,
                                :WS-CLAIM-DATE,
                                :WS-REASON-CODE
	END-EXEC.
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
	EXEC SQL
		WHENEVER SQLERROR
			GO TO ZZ-050
	END-EXEC.
*
	EXEC SQL CLOSE CURSOR_1 END-EXEC.
*
	EXEC SQL
		WHENEVER SQLERROR
			GO TO ZZ-100
	END-EXEC.
*
	EXEC SQL
		ROLLBACK WORK
	END-EXEC.
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
