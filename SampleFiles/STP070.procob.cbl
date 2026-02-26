 IDENTIFICATION DIVISION.
PROGRAM-ID.
	STP070.
AUTHOR.
	REWRITE USING THE NEW STP060 REWRITE AS THE BASE - P E REYNOLDS.
DATE-WRITTEN.
	REWRITE SEPTEMBER 1987. 
*******************************************************************************
*     ISSUE OF CERTIFICATES
*
*	- THIS ROUTINE FROM NOVEMBER 87 ALSO ALLOWS FOR REPRINT AWARDS
*******************************************************************************
*     CRITERIA for AWARD ISSUE            
*
*     REG.    AWARD       
*     TYPE    ELIG  CERT  
*     IND     IND    NO   AWARD-CODE
*     ------  ----- ----  ------------------
*     R or S   Y    NULL  NOT 07,08,11,12,99
*
*     After ISSUE, SET 
*
*     ST-AWARD-ISSUE:  to Input Date 
*     ST-AWARD-ELIG :  NULL 
*     ST-CERT-NO    :  Allocate 
*
*******************************************************************************
*
*     Amendments History
*     ------------------
*     WHO   WHEN    WHAT
*  
*
*     MR    170993  Job 3424 - Added centre id parameter and added clause
*                              to cursor_1 select statement to allow 
*                              individual centre award runs.
*  
*    JD	    010494  Job 4776
*
*			Too many title exist for array size (650).  Due to
*			University titles (non-standard) in award_titles
*			table.
*
*			One sol is to increase array size.
*
*			Solution chosen is:
*
*			a) Maintain award title array on the 'fly'.  That is
*			   whenever title is required which is not in the array
*			   then add it to the array.
*
*			b) Perform serial (non-binary) search on the array
*
*			c) If array becomes full select from database.  In this
*			   version a warning is issued when the array size
*			   has been reached.  The program will continue but
*			   title descriptions will be selected from ORACLE.
*
*		      Notes:  Before this fix, binary searches were performed
*			      on all titles in the array
*
*			      A select from award_titles each time a new title 
*			      is required is probably failrly quick these days. 
*
*			      A look up in memory (even serially searched) is 
*			      probably quicker for 500 - 600 titles.
*
*    LR	    180595  Job 5685: Print a blank page on a programme change.
*		        5375: Print a banner page giving run details.
*		        5743: Include option to write data to a CSV file.
*		        5836: Include date of birth in the footer.
*			
*    LR	    220695  Job 5327: Exclude students if certification has been
*			      deferred for their centre and course as a
*		              result if a quality audit investigation
*			     (record exists in table DEFERRED_AWARDS).
*			
*    NHW    061195            Change format of output for new BTEC award.
*
*       NHW     110996          Do not issue certificate if centre, programme,
*				or student is deferred. Also do not issue 
*				certificate if any units are deferred.
*
*    CAG    240397  Job 6831: use AWARDS_RUN_CENTRES table.
*
* 	NHW	040997	Amend to add 2 extra fields to the output record,
*			for logo codes. Insert a record into the table
*			STUDENT_CERTIFICATE_LOGOS where logos exist.
*			Use this table to obtain logos for reprints.
*
* 	CS	300198	Amend to add 2 extra fields to the output record,
*			for logo codes. (NOCN and a spare slot)
*
*	LR	150498	Job No. 7084
*			Allow certificate reprints when deferrals exist.
*
*	MP	May 98	COA & Entry level ( Award Codes '23','24','25' & '26' ).
*
*	CAG	090698	Key Skills Pilot Qualification ( Award Code '27' ).
*
*	CAG	230698	GNVQ language awards ( Award Codes '31' & '32' ).
*
*	CAG	060798	For non-Welsh centres use 
*			ENTRY_COA_LEVELS.ECLE_ENGLISH_CODE instead of
*                       ENTRY_COA_LEVELS.ECLE_DESC.
*
*	CAG	050898	Print 'THE KEY SKILLS QUALIFICATION' for award code 27.
*
*	CAG	060898	Print 'KEY SKILLS' for award code 27 if
*			STUDENTS.ST_RECON_IND = 'Y', i.e. a reconsideration
*			student.
*
*	CAG	220998	COA Reprint bug fix - missing level.
*
*	Jeff	191098	Check pk constraint on student_certificate_logos
*	Daniels		before attempting an insert - the student may have
*			already been issued with a ceritificate.  Student
*			services have the ability to re-issue a certificate
*			(not reprint).
*
*			Problem on awards run for 16th Oct - Stud = N442133
*
*	CAG	260499	JR7781 - don't select deleted students.
*
*	NR	010699	Y2K.
*
*	MP	180699  include the final level awarded for entry level
*			awards
*
* CAG     24/06/1999    RfW 7794 - Professional Body logos.        
*
* GB      06/08/1999    RfW 7548 - Produce Key Skills Qualification for 
*				   Pilot GNVQ students
*
* GB      21/10/1999    RfW 7548 - Back out previous change             
*
* GB      27/10/1999    Bug fix  - Support re-print of KSQ certicates awarded
*				   got Pilot GNVQ students
*
* GB	  08/02/2000	RfW 7976 - Do not allow printing of 'KEY SKILLS'
*				   for re-considered students
*
* BM      21/06/2000    RfW 8043 - Add issue date to awards.
*
* BM      26/10/2000    RfW 8111 - Additional text for awards appearing in
*                                  PROFESSIONAL_BODY_BTECS as an INSTITUTE OF
*                                  MANAGEMENT award. 
*
* RWM	  16/03/2001    RfW 8140 - Automatic production of IOM Certificates of 
*                                  Eligibility.
*
* TS      14/09/2001    RfW 8094 - Key skills, do not process new key skills
* 
* RWM	  14/11/2001    RfW 8186 - Endorsement of Foreign Language 
*				   Certificates.
*
* RWM	  28/11/2001    RfW 8186 - Bug Fix Cursor_7a (Old Key Skills).
*
* jd	  19/12/2001	Speed up cursor 7a
*
* RWM	  24/01/2002	Add Labels File to solve Centres Discrepancy.
*
* Jeff	  050302	fn_centre_deferred uses reg number as a parameter
*
* RWM     16/04/2002	RfW 8186 - Endorsement of SURF Certificates.
*
* MC	  16/07/2002    Print Institue of management message on reprints
*			as well as main runs (dependant on date of award)
*
* RWM	  02/08/2002	Change Legend 'LEVEL' to 'LEVEL ATTAINED: ENTRY ' for
*			Entry Level Certificates.
*
* RWM	  07/08/2002    Endorse all Certificates which will produce a NOP
*                       with the Message:
*                       'THIS DOCUMENT CONSISTS OF MORE THAN ONE PAGE'.
*
* RWM     18/07/2002    RfW 8311 - New Certificates will no longer have Award
*                       Description pre-printed.
*
* CAG     22/10/2002    BTEC NQF Model (BNM) changes.
*
* CDS     24/10/2002    ABS ENTRY LEVEL changes.
*
* CAG     11/11/2002    BTEC NQF Model (BNM) changes continued...
*
* CDS     10/02/2003    Add TRIPLE LOGO and QCA code for award code 35..
*
* Gerald Rowe 11/03/2003 Add indicator variables as part of v9 upgrade :-
*                        fn_check_student_deferral
*                        pr_student_course_block
*                        pr_green_light
*                        fn_centre_deferred
*
* CDS	13/06/2003	Position logos 72 and 73
*
* CAG	14/07/2003	Remove dummy award codes ( 78, 79, 80, 81 ) coding.
*
* 24/07/2003	CAG	Keep a list of deferred students for reporting.
*
* 10/09/2003	CAG	Bug fix on ARC delete.                         
*
* 02/10/2003	CAG	BNM Certificate Blocking for L4/5 and Introductory.
*
* 15/10/2003	CAG	NQF level 4/5 processing.
*
* 19/11/2003	CAG	NVQ 10 week rule for award code 65 and 66.
*
* 10/03/2004	CAG	PK_BNM.FN_10_WEEK_BLOCK.                     
*
* 12/03/2004    RSH     Process all award codes in one run grouping by the
*                       document reference. This involves opening multiple
*                       output files, updating the next certificate number
*                       for multiple award codes, minor changes to each of
*                       the output file records and some a number of extra
*                       control total records in the rejects file.
* 01/02/2005    MCZ     Hard code 'REPLACEMENT' message on reprint certificate
* 02/02/2005    MCZ     Include checking for ARC_REPRINT_WORDING to display 
*                       '  REPLACEMENT' message
*
* 12/05/2005    RSH     Replace Award Code with FBFI_FCS_AWARD_REF in second
*			field of the header (01) and detail (05) records. 
*
* 28/06/2005    MP	support logo 129 for positioning in TR - top right of
*			certificate - similar to 72
*
* 28/06/2005    RSH     RfW 05/0070:
*			Do not certificate students at a centre which has
*			a finance certification block (print run only). 
*
* 08/08/2005    RSH     RfW 04/0109:
*			ESOL Entry Level and Level 1/2 processing.
*
* 17/08/2005    RSH     RfW 04/0109:
*			Incorporate MP changes dated 15/08/2005.
*			Change QCA code to QAN and put slashes in quoted code.
*
* 02/09/2005    RSH     RfW 04/0109:
*			For ESOL Entry Level, output the grade (eg. 1,2,3)
*			instead of the level (eg. E1,E2,E3).
*
* 07/09/2005	SDA	RFW 05/0082:
*			Obtain and store more info about deferrals.
*
* 17/11/2005	RSH	RFW 05/0186:
*			For a 'print' run, reject students who have
*			achieved an NVQ unit with a certification
*			end date earlier than the current date.
*
* 01/02/2006	CDS	ESOL S&L (Award Code GG)
*
* 30/03/2006	CDS	Speed up logo insertion
*
* 17/07/2006	CAG	RfC 06-0151 DISTINCTION*
*
* 18/07/2006	RSH	Change of wording in Institute of Management text.
*
* 16/11/2006	CDS	RFC 06/0163 & LQ00029421.
*			ICT Entry (HH) & ESOL S&L Levels 1/2 (GA & GB)
*
* 10/01/2007	CDS	LQ00030858 - DMS Certificate Message.
*
* 23/01/2007    CDS     LQ00030858 - BUG FIX DMS Certificate Message.
*
* 24/01/2007	CDS	LQ00030448 - Certificate Validity Message.
*
* 26/01/2007	CDS	LQ00030445 - FAD Certs - add QAN and NQF logos (TRIPLE)
*
* 14/06/2007	BM	LQ00033186 - Extended Project - Grading changes
* 12/07/2007	CDS	LQ00033186 - Extended Project - Add QAN + NQF Triple
*			logo.
*
* 19/07/2007	CDS	LQ00033907 - QCA Cert changes.
*
* 28/11/2007	RSH	LQ00035232: Reprint Certificate Fees
*			Update the associated reprint certificate fee record
*			to reflect the success/failure of the reprint.
*
* 28/03/2008	CAG	LQ00039089 - temporarily block award code 'VV'.     
* 07/04/2008	CAG	LQ00039089 - reinstate 'VV'.                        
*
* 18/06/2008	CAG	LQ00047706 - Edexcel Accreditation Service.         
*
* 02/10/2008	CAG	LQ00039913 - workskills - pk_osca1.             
*
* 20/11/2008    CDS     LQ00050844 - ESOL for WORK ('EW')
*
* 22/12/2008    CDS     LQ00033373 - Logo Positioning + SQA logo + SQA Code
*
* 09/03/2009    CDS     LQ00052420 - ELFS
*
* 27/04/2009    CDS     Bug Fix : old BTEC_REJECTED_STUDENTS records not being removed.
*
* 27/05/2009    CAG     LQ00051711 - no CYQ certification.
*
* 18/06/2009    RSH     LQ00033373 - Remove 'S' prefix from certificate number
*                       when reprinting migrated certificated SIA students.
*
* 29/07/2009	CDS	Student Archiving System
*
* 19/11/2009	CDS	Ordering of Professional Body Logos on cert
*
* 20/11/2009    RSH     LQ00058318 - Remove 'F' prefix from certificate number
*                       when reprinting migrated FPOS students.
*
* 25/11/2009    RSH     Fix bug in ordering of Professional Body Logos.
*
* 10/12/2009    CDS     LQ00060425 - Remove explicit centre name from SQA certs
*			and replace with 'AN APPROVED EDEXCEL CENTRE'
*
* 10/12/2009    RSH     LQ00057608 - 75 character award title lines.
*
* 04/01/2010    CAG     LQ49535 - new OSCA release procedure.              
*
* 09/04/2010    RSH     LQ00058318 - Remove 'H' prefix from certificate number
*                       when reprinting migrated IHCD students.
*
* 14/05/2010    RSH     EC568: Remove characters 2 and 3 from certificate
*                       number when reprinting migrated IHCD students.
*
* 18/05/2010	RSH	LQ00099999 - NEA Certificate Message.
*
* 01/06/2010	RSH	OSCA for QCF BTEC 2/3.
*
* 14/06/2010	CAG	QCF grades, e.g. ***   
*
* 16/07/2010	CAG	Increase award code arrays.       
*
* 21/07/2010    CDS     EC622 - ST_CERT_NAME
*                                                                           
* 20/08/2010    CDS     LQ00061360 - Certificate ordering/collation
*                                                                           
* 18/11/2010    CDS     LQ00062004 - ELFS again (But this time Award Code FS)
*
* 13/09/2010    MB      RFC LQ00061566 - Automatic production of 
*                       DSA Certificates of Eligibility.
*
* 25/01/2011    SDA     WI86 : Use new package PR_GET_CENTRE_NAMES
*                       to obtain correct centre cert name lines.
*
* 09/02/2011    JD      EC781 : Bugfix - new index i_st_award_elig - 
*                       Reprint cursor requires amendment
*
* 10/02/2011    MB      EC780 : Bugfix - remove use of award title 11561 for DSA
*
* 16/03/2011    CDS     WI101 : On-Screen Func Skills Levels 1/2 (Award Code FO)
*
* 11/04/2011    CAG     WI324 : new 88 record.
*
* 09/06/2011    CAG     WI324 : new 88 record reprint bug.
*
* 01/07/2011    CAG     WI324 : new 88 record reprint bug (@ARCA)
*
* 13/07/2011    JD      EC948 - Increase size of variables for run_details.
*
* 13/07/2011    RSH     WI00000069 - Remove 'R' prefix from certificate number
*                       when reprinting migrated BRC students.
*
* 21/07/2011    JD      EC948 - Increase size of variables for run_details - attempt #2
*
* 27/07/2011    MB      WI409 - Amend DOC07/DOC77 certificate files to add 
*                               flag ('IRL') where country code cn_cial_id = IRL
*
* 08/09/2011    CDS     EC1005 : Allow for 2 training organisation logos
* 14/09/2011    CDS     EC1018 : DEJAR for archived students.
* 28/09/2011    MB      EC1029 : Blank out 'IRL' indicator in the file
* 17/10/2011    RSH     WI00000069 - Remove 'A' and 'D' prefixes from certificate
*                       number when reprinting migrated Tuition students.
* 27/02/2012	CDS	EC1206 : Merge DOC77 into DOC7
* 28/02/2012    CDS     EC1209 : Add missing logo fields for new DOC34
* 08/03/2012    CDS     EC1225 : Allow for 2 centre logos
* 16/05/2012    CAG     WI790 - award code LI.                 
* 28/08/2012	JD 	EC1359 - NEA Certificate Message - SIMPLE change requested by RC.
* 15/10/2012    JD      EC1403 - Increase rejected-student limit from 100 to 1000
* 08/01/2013    CAG     WI947 - switch off NG/NQF for now.     
* 18/06/2013    CAG     WI1095  - 2 language messages.          
* 23/07/2013    JPD     WI1111 - Set globals - for non reprints
* 03/08/2013    CDS     EC - Increase Cert array (Accounted for in WI1111)
* 01/10/2013    CAG     WI947 - switch on NG/NQF.
* 23/10/2013    CDS     EC1697 - SVQ : Change AN APPROVED EDEXCEL CENTRE to
*			AN APPROVED PEARSON CENTRE
*  MB        08/12/2013   WI1099 - Branding Change EDEXCEL to PEARSON 
*  CAG       13/01/2014   WI1099 - Branding reprints.                    
* 24/04/2014	CDS	EC1826 : Student Archive (SCLO)
* MB         01/05/2014 EC1834 : suppress the word 'in' for NCCER award codes
* CAG        28/08/2014 EC1924 : COMPLETED not AWARDED for Assured (EA).       
* CAG        17/09/2014 EC1938 : NG at level 2 wording.                        
* CAG        04/11/2014 WI989 - OFQUAL NO.                                     
* CAG        06/11/2014 WI989 - Not OFQUAL NO.
* CDS	     10/11/2014 EC1977 - Reposition IHCD logo for CK597 and CK599
* CAG        17/11/2014 WI989 - continued.....
* CDS	     20/01/2015 EC2016 : Missing QN + Triple Logo for New ESOL (Q0->Q9) 
* CDS	     03/12/2015 WI1431 : A2C - Add unit code to AROF
* CAG        21/12/2015 EC2224 - triple double logo.                
* CAG        04/01/2016 EC2224 - triple double logo bug.            
* MBC        08/01/2016 EC2233 - 'W LOGO' and 'E LOGO'
* MP	     17/05/2016 EC2306 : EW changed to EWI 
* SDA        13/09/2016 award codes 1W 2W 3W 4W have message about Welsh regulation
* CAG        28/09/2016 EC2369 - GL1.                               
* SDA        19/01/2017 EC2446 Correct generation of certificate validity message
* MBC        28/03/2017 EC2480 Add fields in all_reprint_criteria_hist
* MP	     24/04/2017 EC2489 Add in handling of ac TZ like EA 
*			       add other language messages
* MBC        28/04/2017 EC2490 Language footnote, new table certificate_language_text
* CAG        17/07/2017 EC2369 - GL1 overall grade.                 
* MBC        07/09/2018 EC2769 Add new '... LOGO'
* MBC        30/10/2018 EC2796 - New message lines
* MBC        04/12/2018 EC2810  'NO LOGO' accreditation logo '0'
* MBC        02/01/2019 EC2825   Account for award code Y6 bnm type 'G' grade 1,2,3
* SN         12/03/2020 EC4363 - Tech Awards Embargo 2020 Changes on Certificate Processing
* CTS        19/08/2020 EC4623 - Increase array size of variables storing data from
*                                table FCS_BTEC_FILES and AWARD_CODES
* CTS        30/10/2020 EC4424 - Added columns for new file DOC56(Similar to DOC34)
* CTS        11/06/2021 EC4940 - Summer 2021 Results delivery Certificate Embargo for B1
***********************************************************************************
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER.
	VAX-11.
OBJECT-COMPUTER.
	VAX-11.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
	SELECT PRINT-FILE  ASSIGN "CERT_FILE".
	SELECT SORT-FILE   ASSIGN SORTFILE.
        SELECT REJECT-FILE ASSIGN REJECTFILE.
	SELECT LABELS-FILE ASSIGN LABELSFILE.
/
DATA DIVISION.
FILE SECTION.
*
* Note that WS-ANS-LENGTH which is declared in WS-BITS is defaulted to
* 80 which is the longest line for a formatted report. If a line length
* is increased, this default must also be increased.
*
FD	PRINT-FILE
	VALUE OF ID IS WS-CERT-FILE
        RECORD VARYING FROM 1 TO 595
        DEPENDING ON WS-ANS-LENGTH.
01	PRINT-REC.
        03  FILLER                              PIC X(595).
*	03  FILLER				PIC X(581).
*
FD	REJECT-FILE.
01	REJECT-REC.
	03  FILLER				PIC X(130).
*
FD      LABELS-FILE.
01      LABELS-REC.
        03  LAB-CENTRE-NO                       PIC X(6).
*
SD	SORT-FILE.
01	SORT-REC.
	03  SORT-KEY.
	  04  S-NEW-REG.
	    05  S-NEW-COMB.
	      06  S-NEW-COURSE.
		07  S-NEW-AWARD-CODE.
		  08  S-NEW-CENTRE.
		    09  S-NEW-DOC-REF.
		      10  S-DOC-REF		PIC X(5).
		    09  S-CENTRE-NO		PIC X(6).
		  08  S-AWARD-CODE		PIC X(2).
		07  S-COURSE-NO			PIC X(8).
	      06  S-COMB			PIC X.
	    05  S-REG-NO			PIC X(7).
	03  S-STUDENT-NAME			PIC X(51).
        03  S-CERT-NO                           PIC 9(9).
	03  S-AWARD-CLAIM			PIC X.
	03  S-MONTH-YEAR			PIC X(18).
        03  S-AWARD-DATE                        PIC X(11).
	03  S-ELIG				PIC X.
	03  S-REG-TYPE				PIC X.
        03  S-OVER-GRADE                        PIC X(3).
        03  S-FALLBACK                          PIC X.
        03  S-BIRTH-DATE			PIC X(8).
	03  S-INST-LOCATION			PIC X(6).
        03  S-COA-SYLLABUS-CODE                 PIC X(6).
        03  S-COA-LEVEL-ACHIEVED                PIC X(1).
        03  S-COA-LEVEL-ACHIEVED-DESCR          PIC X(30).
	03  S-RECON-IND				PIC X(1).
	03  S-KSQ				PIC X(1).
	03  S-ISSUE-DATE			PIC X(11).
	03  S-SCHEME-REG-NO			PIC X(7).
	03  S-SCHEME-REG-NO-I			PIC S9(4) COMP.
	03  S-BTEC-CODE				PIC 9(6).
	03  S-BTEC-LEVEL			PIC X(2).
	03  S-NOPS-REQD				PIC X(1).
	03  S-NOPS-REQD-I			PIC S9(4) COMP.
	03  S-NVQ-ROA-CODE			PIC X(1).
	03  S-NVQ-ROA-CODE-I			PIC S9(4) COMP.
	03  S-GNVQ-CUC-CODE			PIC X(1).
	03  S-GNVQ-CUC-CODE-I			PIC S9(4) COMP.
	03  S-CRED-TRANS-CODE			PIC X(1).
	03  S-CRED-TRANS-CODE-I			PIC S9(4) COMP.
        03  S-ABS-LEVEL-ACHIEVED                PIC X(1).
        03  S-ABS-LEVEL-ACHIEVED-I              PIC S9(4) COMP.
	03  S-CERT-FLAG				PIC X(2).
*EC2490
        03  S-CERT-MSG-SURF                     PIC X(70).
        03  S-CERT-REPRINT-WORDING              PIC X(13).
        03  S-ESOL-LEVEL-ACHIEVED               PIC X(2).
	03  S-FIRST-FEE-ID			PIC S9(9) COMP.
	03  S-LAST-FEE-ID			PIC S9(9) COMP.
        03  S-DSA-SRU-ID                        PIC X(12).
	03  S-OLD-REPRINT-CERT 			PIC X(1).
/
WORKING-STORAGE SECTION.
*
01  SQLFPN GLOBAL.
           02  SQLFPN-FILE-LEN PIC S9(4) COMP-5 VALUE +28.
           02  SQLFPN-FILENAME PIC X(28) VALUE "/work/SampleFiles/STP070.pco".

01  SQ0006 GLOBAL.
           02  FILLER PIC X(85) VALUE "insert into BTEC.RUN_DETAILS(RD_NAME,RD_DATE) values (:b1,TO_DATE(:b2,'DD-MON-YYYY'))".

01  SQ0008 GLOBAL.
           02  FILLER PIC X(140) VALUE "select TO_CHAR(SYSDATE,'DD-MON-YYYY:HH24:MI')  ,(((AC_DESCRIPTION||' (')||:b1)||')')   into "&
    ":b2,:b3  from BTEC.AWARD_CODES where AC_CODE=:b4".

01  SQ0009 GLOBAL.
           02  FILLER PIC X(151) VALUE "select FCSF_DOC_REF  ,FCSF_CERT_TEXT_1  ,FCSF_CERT_TEXT_2  ,FCSF_CERT_TEXT_3   into :b1,:b2,"&
    ":b3,:b4  from BTEC.FCS_FILES where FCSF_FILENAME=(:b5||:b6)".

01  SQ0010 GLOBAL.
           02  FILLER PIC X(63) VALUE "begin  PK_ST_GLOBAL . PR_SET_GLOBALS ( 'STP070' , :b1 ) ; END ;".

01  SQ0011 GLOBAL.
           02  FILLER PIC X(120) VALUE "select SUBSTR(CV_TYPE,9,2)  ,CV_TYPE  ,CV_NEXT_VALUE   into :b1,:b2,:b3  from BTEC.CONTROL_V"&
    "ALUES where CV_TYPE like :b4".

01  SQ0012 GLOBAL.
           02  FILLER PIC X(65) VALUE "select 'B'   into :b1  from BTEC.AWARD_TITLES where AT_NUMBER=:b2".

01  SQ0013 GLOBAL.
           02  FILLER PIC X(122) VALUE "select FBFI_DOC_REF   into :b1  from FCS_BTEC_FILES where ((FBFI_AWARD_CODE=:b2 and FBFI_TYP"&
    "E=:b3) and FBFI_PRINT_IND='Y')".

01  SQ0014 GLOBAL.
           02  FILLER PIC X(256) VALUE "select AA_BTEC_TITLE   into :b1  from AWARD_CODES ,BTEC.APPROVAL_APPLICATION ,BTEC.APPROVAL_"&
    "AWARDS ,BTEC.STUDENTS where (((((AC_CODE=(AW_AWARD_CODE||'') and AC_EXTERNAL_CERT_NO is null ) and AW_COURSE_NUMBER=ST_COURSE_"&
    "ID) and ST_COURSE_ID=:b2) and AW_APPLI".

           02  FILLER PIC X(41) VALUE "CAT_NO=AA_APPLICAT_NO) and ST_REG_NO=:b3)".

01  SQ0015 GLOBAL.
           02  FILLER PIC X(217) VALUE "select NVL(DECODE(CCTE_ID,'IRL','Y','N'),'N')   into :b1  from BTEC.CUSTOMISED_CERT_TEXT ,BT"&
    "EC.CENTRES where ((((CN_CENTRE_ID=:b2 and CCTE_ID=CN_CIAL_ID) and CCTE_ID='IRL') and CCTE_TYPE='AT') and CCTE_TYPE_VALUE=:b3)"
     .

01  SQ0016 GLOBAL.
           02  FILLER PIC X(62) VALUE "delete from BTEC_REJECTED_STUDENTS  where BRS_PROG_NO='STP070'".

01  SQ0017 GLOBAL.
           02  FILLER PIC X(256) VALUE "select FBFI_AWARD_CODE  ,(((AC_DESCRIPTION||' (')||FBFI_AWARD_CODE)||')')  ,FBFI_CERT_TEXT_1"&
    "  ,FBFI_CERT_TEXT_2  ,FBFI_CERT_TEXT_3  ,TO_CHAR(SYSDATE,'DD-MON-YYYY:HH24:MI')  ,NVL(FBFI_FCS_AWARD_REF,LPAD(FBFI_AWARD_CODE,"&
    "3,'0'))  ,FBFI_OLD_CERT_TEXT_1  ,FBFI_".

           02  FILLER PIC X(227) VALUE "OLD_CERT_TEXT_2  ,FBFI_OLD_CERT_TEXT_3   into :b1,:b2,:b3,:b4,:b5,:b6,:b7,:b8,:b9,:b10  from"&
    " AWARD_CODES ,FCS_BTEC_FILES where ((FBFI_DOC_REF=:b11 and FBFI_PRINT_IND='Y') and AC_CODE(+)=FBFI_AWARD_CODE) order by FBFI_A"&
    "WARD_CODE".

01  SQ0018 GLOBAL.
           02  FILLER PIC X(169) VALUE "insert into BTEC.RUN_DETAILS(RD_NAME,RD_DATE,RD_TIME,RD_RUN_FLAG,RD_COMMENT) values (:b1,TO_"&
    "DATE(:b2,'DD-MON-YYYY'),:b3,:b4,('NUMBER OF AWARDS ISSUED = '||TO_CHAR(:b5)))".

01  SQ0019 GLOBAL.
           02  FILLER PIC X(94) VALUE "begin  PK_PROVDEF . PR_CHECK_STUDENT_DEFERRAL ( :b1 , :b2 , :b3:i3 , :b4:i4 , :b5:i5 ) ; END "&
    ";".

01  SQ0020 GLOBAL.
           02  FILLER PIC X(92) VALUE "begin  PK_BNM . PR_AWARDS_RUN_BLOCK ( :b1 , 'STP070' , :b2 , :b3 , :b4:i4 , :b5:i5 ) ; END ;"
     .

01  SQ0021 GLOBAL.
           02  FILLER PIC X(83) VALUE "begin  PK_BNM . PR_STUDENT_COURSE_BLOCK2 ( :b1 , :b2:i2 , :b3:i3 , :b4:i4 ) ; END ;".

01  SQ0022 GLOBAL.
           02  FILLER PIC X(92) VALUE "begin  PK_BNM . PR_AWARDS_RUN_BLOCK ( :b1 , 'STP070' , :b2 , :b3 , :b4:i4 , :b5:i5 ) ; END ;"
     .

01  SQ0023 GLOBAL.
           02  FILLER PIC X(93) VALUE "begin  PK_OSCA1 . PR_Q_CERT_STATUS ( :b1 , NULL , :b2:i2 , :b3:i3 , :b4:i4 , :b5:i5 ) ; END ;".

01  SQ0024 GLOBAL.
           02  FILLER PIC X(90) VALUE "begin  PK_BNM . PR_AWARDS_RUN_BLOCK ( :b1 , 'STP070' , :b2 , :b3 , null , :b4:i4 ) ; END ;".

01  SQ0025 GLOBAL.
           02  FILLER PIC X(68) VALUE "begin  PK_GPILOT . PR_GREEN_LIGHT ( 2 , :b1 , :b2 , :b3:i3 ) ; END ;".

01  SQ0026 GLOBAL.
           02  FILLER PIC X(63) VALUE "begin  :b1:i1 := PK_GNVQLU . FN_CENTRE_DEFERRED ( :b2 ) ; END ;".

01  SQ0027 GLOBAL.
           02  FILLER PIC X(88) VALUE "begin  PK_BNM . PR_AWARDS_RUN_BLOCK ( :b1 , 'STP070' , '1' , :b2 , NULL , NULL ) ; END ;".

01  SQ0028 GLOBAL.
           02  FILLER PIC X(62) VALUE "begin  PK_BNM . PR_AWARDS_RUN_CLEAR ( :b1 , 'STP070' ) ; END ;".

01  SQ0029 GLOBAL.
           02  FILLER PIC X(89) VALUE "begin  :b1:i1 := PK_FINANCE_BLOCKING . FN_CENTRE_FINANCE_CERT_BLOCK ( :b2 , 'N' ) ; END ;".

01  SQ0030 GLOBAL.
           02  FILLER PIC X(90) VALUE "begin  PK_FINANCE_BLOCKING . PR_INSERT_CERT_BLOCK_STUDENT ( :b1 , :b2 , 'STP070' ) ; END ;".

01  SQ0031 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ((((((((((((((((((ST_SCHEME_REG_NO||'*!*')||TO_CHAR(ST_BIRTH_DATE,'DD/MM/YY'))||'*!*'"&
    ")||ST_CENTRE_ID)||'*!*')||ST_CENTRE_REF)||'*!*')||ST_COHORT)||'*!*')||NVL(ST_COURSE_ID,ST_NVQ_REGISTERED_ID))||'*!*')||SCHEME_"&
    "CODE)||'*!*')||TO_CHAR(NVL(NVL(ST_AWAR".

           02  FILLER PIC X(210) VALUE "D_ISSUE,ST_NVQ_CERTIFICATE_PRINT_DATE),TO_DATE(:b1,'DD-MON-YYYY')),'DD/MM/YY'))||'*!*')||ST_"&
    "FORENAMES)||'*!*')||ST_SURNAME)   into :b2  from SCHEMES ,STUDENTS where (SCHEME_ID(+)=ST_SCHEME_ID and ST_REG_NO=:b3)".

01  SQ0032 GLOBAL.
           02  FILLER PIC X(210) VALUE "update REPRINT_CERTIFICATE_FEES  set RCFE_STATUS='O',RCFE_REPRINT_DATE=SYSDATE,RCFE_UPDATE_D"&
    "ATE=SYSDATE,RCFE_UPDATE_USER='STP070' where ((RCFE_ID between :b1 and :b2 and RCFE_ST_REG_NO=:b3) and RCFE_STATUS='R')".

01  SQ0033 GLOBAL.
           02  FILLER PIC X(138) VALUE "begin  PK_CENTRE_NAMES . PR_GET_CENTRE_CERT_NAMES ( :b1 , TO_DATE ( :b2 , 'DD-MON-YYYY' ) , "&
    ":b3 , :b4:i4 , :b5:i5 , :b6:i6 , :b7 ) ; END ;".

01  SQ0034 GLOBAL.
           02  FILLER PIC X(164) VALUE "update BTEC.STUDENTS  set ST_AWARD_PRINTED=TO_DATE(:b1,'DD-MON-YYYY'),ST_AWARD_ISSUE=TO_DATE"&
    "(:b2,'DD-MON-YYYY'),ST_CERT_NO=:b3,ST_AWARD_CODE=:b4 where ST_REG_NO=:b5".

01  SQ0035 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into ALL_REPRINT_CRITERIA_HIST(ARCH_RUN_DATE,ARCH_INSERT_DATE,ARCH_REPRINT_TYPE,ARCH_"&
    "FIRST_REG_NO,ARCH_LAST_REG_NO,ARCH_AWARD_DATE,ARCH_AWARD_CODE,ARCH_NOPS,ARCH_UC_NAME,ARCH_CRED_TRANS,ARCH_REPRINT_WORDING,ARCH"&
    "_CUC,ARCH_REPRINT_CHARGE,ARCH_FIRST_RE".

           02  FILLER PIC X(256) VALUE "G_RCFE_ID,ARCH_LAST_REG_RCFE_ID,ARCH_DB,ARCH_RETURN_DOC,ARCH_COMMENT,ARCH_RCCO_ID)select TRU"&
    "NC(SYSDATE)  ,SYSDATE  ,ARC_REPRINT_TYPE  ,ARC_FIRST_REG_NO  ,ARC_LAST_REG_NO  ,ARC_AWARD_DATE  ,ARC_AWARD_CODE  ,ARC_NOPS  ,A"&
    "RC_UC_NAME  ,ARC_CRED_TRANS  ,ARC_REPR".

           02  FILLER PIC X(198) VALUE "INT_WORDING  ,ARC_CUC  ,ARC_REPRINT_CHARGE  ,ARC_FIRST_REG_RCFE_ID  ,ARC_LAST_REG_RCFE_ID  ,"&
    "ARC_DB  ,ARC_RETURN_DOC  ,ARC_COMMENT  ,ARC_RCCO_ID   from ALL_REPRINT_CRITERIA where ARC_REPRINT_TYPE='B'".

01  SQ0036 GLOBAL.
           02  FILLER PIC X(60) VALUE "delete from ALL_REPRINT_CRITERIA  where ARC_REPRINT_TYPE='B'".

01  SQ0037 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into STUDENT_CERTIFICATE_LOGOS(SCLO_ST_REG_NO,SCLO_CLOG_CODE,SCLO_LPOS_ID)select :b1 "&
    " ,DECODE(INSTR(:b2,'('),0,:b2,SUBSTR(:b2,1,(INSTR(:b2,'(')-1)))  ,DECODE(INSTR(:b2,'('),0,null ,SUBSTR(:b2,(INSTR(:b2,'(')+1),"&
    "(LENGTH(:b2)-(INSTR(:b2,')')-1))))   f".

           02  FILLER PIC X(136) VALUE "rom DUAL  minus select SCLO_ST_REG_NO  ,SCLO_CLOG_CODE  ,TO_CHAR(SCLO_LPOS_ID)   from STUDEN"&
    "T_CERTIFICATE_LOGOS where SCLO_ST_REG_NO=:b1".

01  SQ0038 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into STUDENT_CERTIFICATE_LOGOS(SCLO_ST_REG_NO,SCLO_CLOG_CODE,SCLO_LPOS_ID)select :b1 "&
    " ,DECODE(INSTR(:b2,'('),0,:b2,SUBSTR(:b2,1,(INSTR(:b2,'(')-1)))  ,DECODE(INSTR(:b2,'('),0,null ,SUBSTR(:b2,(INSTR(:b2,'(')+1),"&
    "(LENGTH(:b2)-(INSTR(:b2,')')-1))))   f".

           02  FILLER PIC X(136) VALUE "rom DUAL  minus select SCLO_ST_REG_NO  ,SCLO_CLOG_CODE  ,TO_CHAR(SCLO_LPOS_ID)   from STUDEN"&
    "T_CERTIFICATE_LOGOS where SCLO_ST_REG_NO=:b1".

01  SQ0039 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into STUDENT_CERTIFICATE_LOGOS(SCLO_ST_REG_NO,SCLO_CLOG_CODE,SCLO_LPOS_ID)select :b1 "&
    " ,DECODE(INSTR(:b2,'('),0,:b2,SUBSTR(:b2,1,(INSTR(:b2,'(')-1)))  ,DECODE(INSTR(:b2,'('),0,null ,SUBSTR(:b2,(INSTR(:b2,'(')+1),"&
    "(LENGTH(:b2)-(INSTR(:b2,')')-1))))   f".

           02  FILLER PIC X(136) VALUE "rom DUAL  minus select SCLO_ST_REG_NO  ,SCLO_CLOG_CODE  ,TO_CHAR(SCLO_LPOS_ID)   from STUDEN"&
    "T_CERTIFICATE_LOGOS where SCLO_ST_REG_NO=:b1".

01  SQ0040 GLOBAL.
           02  FILLER PIC X(256) VALUE "insert into STUDENT_CERTIFICATE_LOGOS(SCLO_ST_REG_NO,SCLO_CLOG_CODE,SCLO_LPOS_ID)select :b1 "&
    " ,DECODE(INSTR(:b2,'('),0,:b2,SUBSTR(:b2,1,(INSTR(:b2,'(')-1)))  ,DECODE(INSTR(:b2,'('),0,null ,SUBSTR(:b2,(INSTR(:b2,'(')+1),"&
    "(LENGTH(:b2)-(INSTR(:b2,')')-1))))   f".

           02  FILLER PIC X(136) VALUE "rom DUAL  minus select SCLO_ST_REG_NO  ,SCLO_CLOG_CODE  ,TO_CHAR(SCLO_LPOS_ID)   from STUDEN"&
    "T_CERTIFICATE_LOGOS where SCLO_ST_REG_NO=:b1".

01  SQ0041 GLOBAL.
           02  FILLER PIC X(256) VALUE "declare  CURSOR C_ARC IS SELECT ARC_FIRST_REG_RCFE_ID , ARC_LAST_REG_RCFE_ID FROM ALL_REPRIN"&
    "T_CRITERIA WHERE ARC_REPRINT_TYPE = 'B' AND ARC_FIRST_REG_RCFE_ID is not NULL AND ARC_LAST_REG_RCFE_ID is not NULL ; BEGIN FOR"&
    " C_ARC_REC IN C_ARC LOOP UPDATE REPRIN".

           02  FILLER PIC X(233) VALUE "T_CERTIFICATE_FEES SET RCFE_STATUS = 'F' , RCFE_UPDATE_DATE = SYSDATE , RCFE_UPDATE_USER = '"&
    "STP070' WHERE RCFE_ID between C_ARC_REC . ARC_FIRST_REG_RCFE_ID and C_ARC_REC . ARC_LAST_REG_RCFE_ID AND RCFE_STATUS = 'R' ; E"&
    "ND LOOP ; END ;".

01  SQ0042 GLOBAL.
           02  FILLER PIC X(67) VALUE "update BTEC.CONTROL_VALUES  set CV_NEXT_VALUE=:b1 where CV_TYPE=:b2".

01  SQ0045 GLOBAL.
           02  FILLER PIC X(171) VALUE "update BTEC.RUN_DETAILS  set RD_TIME=:b1,RD_RUN_FLAG=:b2,RD_COMMENT=('NUMBER OF AWARDS ISSUE"&
    "D = '||TO_CHAR(:b3)) where (RD_NAME=:b4 and RD_DATE=TO_DATE(:b5,'DD-MON-YYYY'))".

01  SQ0046 GLOBAL.
           02  FILLER PIC X(173) VALUE "insert into BTEC.RUN_DETAILS(RD_NAME,RD_DATE,RD_RUN_FLAG,RD_COMMENT) values (:b1,TO_DATE(:b2"&
    ",'DD-MON-YYYY'),:b3,((('NUMBER OF AWARDS ISSUED ('||:b4)||') = ')||TO_CHAR(:b5)))".

01  SQ0047 GLOBAL.
           02  FILLER PIC X(256) VALUE "select LTRIM(((AT_TITLE_PREFIX||' ')||AT_TITLE_LINE1))  ,AT_TITLE_LINE2   into :b1,:b2  from"&
    " BTEC.AWARD_TITLES where (AT_NUMBER=:b3 and  not exists (select null    from AT_AWARD_CODES where (AACO_AT_NUMBER=AT_NUMBER an"&
    "d AACO_AC_CODE in ('EA','LI','NA','NB'".

           02  FILLER PIC X(256) VALUE ",'NC','ND','NE','NF','NG','TZ')))) union select AT_TITLE_LINE1  ,AT_TITLE_LINE2   from BTEC."&
    "AWARD_TITLES where (AT_NUMBER=:b3 and exists (select null    from AT_AWARD_CODES where (AACO_AT_NUMBER=AT_NUMBER and AACO_AC_C"&
    "ODE in ('EA','LI','NA','NB','NC','ND',".

           02  FILLER PIC X(23) VALUE "'NE','NF','NG','TZ'))))".

01  SQ0048 GLOBAL.
           02  FILLER PIC X(114) VALUE "select CLOG_ID   into :b1  from CUSTOMER_LOGOS where (CLOG_CENTRE_ID in (:b2,SUBSTR(:b2,1,5)"&
    ") and CLOG_STATUS='L')".

01  SQ0049 GLOBAL.
           02  FILLER PIC X(256) VALUE "select CLOG_ID   into :b1  from CUSTOMER_LOGOS where (CLOG_INST_ID in (:b2,SUBSTR(:b2,1,5)) "&
    "and CLOG_STATUS='L') union select CLOG_ID   from CUSTOMER_LOGOS where ((CLOG_CENTRE_ID in (:b2,SUBSTR(:b2,1,5)) and CLOG_STATU"&
    "S='L') and  not exists (select null   ".

           02  FILLER PIC X(88) VALUE " from CUSTOMER_LOGOS where (CLOG_INST_ID in (:b2,SUBSTR(:b2,1,5)) and CLOG_STATUS='L')))".

01  SQ0050 GLOBAL.
           02  FILLER PIC X(256) VALUE "select distinct (CLOG_ID||DECODE(TOCC_LPOS_ID,null ,null ,(('('||TO_CHAR(TOCC_LPOS_ID))||')'"&
    ")))   into :b1  from CUSTOMER_LOGOS ,TRAIN_ORG_CENTRE_COURSES where ((((TOCC_COURSE_NUMBER=:b2 and TOCC_CENTRE_ID in (:b3,SUBS"&
    "TR(:b3,1,5))) and SYSDATE between TOCC".

           02  FILLER PIC X(97) VALUE "_START_DATE and NVL(TOCC_END_DATE,SYSDATE)) and CLOG_TORG_ID=TOCC_TORG_CODE) and CLOG_STATUS="&
    "'L')".

01  SQ0051 GLOBAL.
           02  FILLER PIC X(256) VALUE "select SCLO_ST_REG_NO  ,(SCLO_CLOG_CODE||DECODE(SCLO_LPOS_ID,null ,null ,(('('||TO_CHAR(SCLO"&
    "_LPOS_ID))||')')))  ,CLOG_TYPE_CODE  ,TO_NUMBER(SCLO_CLOG_CODE)   into :b1,:b2,:b3,:b4  from CUSTOMER_LOGOS ,STUDENT_CERTIFICA"&
    "TE_LOGOS where (SCLO_ST_REG_NO=:b5 and".

           02  FILLER PIC X(256) VALUE " SCLO_CLOG_CODE=CLOG_ID) union select SCLO_ST_REG_NO  ,(SCLO_CLOG_CODE||DECODE(SCLO_LPOS_ID,"&
    "null ,null ,(('('||TO_CHAR(SCLO_LPOS_ID))||')')))  ,CLOG_TYPE_CODE  ,TO_NUMBER(SCLO_CLOG_CODE)   from CUSTOMER_LOGOS ,STUDENT_"&
    "CERTIFICATE_LOGOS@ARCA where (SCLO_ST_".

           02  FILLER PIC X(49) VALUE "REG_NO=:b5 and SCLO_CLOG_CODE=CLOG_ID) order by 4".

01  SQ0052 GLOBAL.
           02  FILLER PIC X(218) VALUE "select distinct CLOG_ID   into :b1  from CUSTOMER_LOGOS ,LOGO_AWARDS where (((LAWA_AC_CODE=:"&
    "b2 and SYSDATE between LAWA_START_DATE and NVL(LAWA_END_DATE,SYSDATE)) and CLOG_LATY_CODE=LAWA_LATY_CODE) and CLOG_STATUS='L')".

01  SQ0053 GLOBAL.
           02  FILLER PIC X(256) VALUE "select distinct CLOG_ID  ,(CLOG_ID||DECODE(PBBT_PBOD_ID,39,DECODE(:b1,'CK597','(1)','CK599',"&
    "'(1)',DECODE(PBBT_LPOS_ID,null ,null ,(('('||TO_CHAR(PBBT_LPOS_ID))||')'))),DECODE(PBBT_LPOS_ID,null ,null ,(('('||TO_CHAR(PBB"&
    "T_LPOS_ID))||')'))))   into :b2,:b3  f".

           02  FILLER PIC X(256) VALUE "rom CUSTOMER_LOGOS ,PROFESSIONAL_BODY_BTECS where ((((CLOG_PBOD_ID=(PBBT_PBOD_ID+0) and CLOG"&
    "_STATUS='L') and PBBT_AWARD_CODE=:b4) and PBBT_AWARD_TITLE=TO_NUMBER(:b5)) and SYSDATE between PBBT_START_DATE and NVL(PBBT_EN"&
    "D_DATE,SYSDATE)) order by TO_NUMBER(CL".

           02  FILLER PIC X(6) VALUE "OG_ID)".

01  SQ0054 GLOBAL.
           02  FILLER PIC X(102) VALUE "select 'Y'   into :b1  from DUAL where TO_DATE(:b2,'DD-MON-YYYY')>TO_DATE('26-OCT-2000','DD-"&
    "MON-YYYY')".

01  SQ0055 GLOBAL.
           02  FILLER PIC X(197) VALUE "select distinct 'Y'   into :b1  from PROFESSIONAL_BODY_BTECS ,PROFESSIONAL_BODIES_NOLOGOS wh"&
    "ere (((PBBT_AWARD_TITLE=TO_NUMBER(:b2) and PBBT_PBOD_ID=PBNO_ID) and PBBT_AWARD_CODE=:b3) and PBNO_ID=10)".

01  SQ0056 GLOBAL.
           02  FILLER PIC X(197) VALUE "select distinct 'Y'   into :b1  from PROFESSIONAL_BODY_BTECS ,PROFESSIONAL_BODIES_NOLOGOS wh"&
    "ere (((PBBT_AWARD_TITLE=TO_NUMBER(:b2) and PBBT_PBOD_ID=PBNO_ID) and PBBT_AWARD_CODE=:b3) and PBNO_ID=10)".

01  SQ0057 GLOBAL.
           02  FILLER PIC X(128) VALUE "select TO_CHAR(AACO_YEARS_CERT_VALID)   into :b1  from AT_AWARD_CODES where (AACO_AT_NUMBER="&
    "TO_NUMBER(:b2) and AACO_AC_CODE=:b3)".

01  SQ0058 GLOBAL.
           02  FILLER PIC X(196) VALUE "insert into IOM_CERTIFICATES(IOMC_TYPE,IOMC_TITLE,IOMC_LEVEL,IOMC_CENTRE,IOMC_NAME,IOMC_REG_"&
    "NO,IOMC_SCHEME_REG_NO,IOMC_AWARD_DATE) values ('B',:b1,:b2,:b3,:b4,:b5,null ,TO_DATE(:b6,'DD-MON-YYYY'))".

01  SQ0059 GLOBAL.
           02  FILLER PIC X(212) VALUE "insert into DSA_CERTIFICATES(DSAC_TYPE,DSAC_TITLE,DSAC_LEVEL,DSAC_CENTRE,DSAC_NAME,DSAC_REG_"&
    "NO,DSAC_SCHEME_REG_NO,DSAC_AWARD_DATE,DSAC_SRU_ID) values ('B',:b1,:b2,:b3,:b4,:b5,null ,TO_DATE(:b6,'DD-MON-YYYY'),:b7)".

01  SQ0060 GLOBAL.
           02  FILLER PIC X(256) VALUE "select AW_AWARD_CODE  ,AW_APPLICAT_NO  ,DECODE(AC_ACTY_CODE,'KS',(SUBSTR(AC_DESCRIPTION,12,6"&
    "0)||' LEVEL'),AC_DESCRIPTION)  ,AC_ACTY_CODE  ,AC_ACCL_CODE  ,BALE_DESC  ,NVL(AC_BNM_TYPE,'*')  ,NVL(AC_ACCRED_BODY,'Q')  ,NVL"&
    "(AT_ACCRED_LOGO,'*')   into :b1,:b2,:b".

           02  FILLER PIC X(256) VALUE "3,:b4,:b5,:b6,:b7,:b8,:b9  from BTEC.BTEC_AWARD_LEVELS ,BTEC.AWARD_CODES ,AWARD_TITLES ,APPR"&
    "OVAL_APPLICATION ,BTEC.APPROVAL_AWARDS where ((((((AW_AWARD_CODE not  between '23' and '26' and AW_AWARD_CODE not  between '33"&
    "' and '37') and AW_COURSE_NUMBER=:b10)".

           02  FILLER PIC X(256) VALUE " and AA_APPLICAT_NO=(AW_APPLICAT_NO+0)) and AT_NUMBER=TO_NUMBER(AA_BTEC_TITLE)) and AC_CODE="&
    "AW_AWARD_CODE) and BALE_ID(+)=AW_BALE_CODE) union select AW_AWARD_CODE  ,AW_APPLICAT_NO  ,DECODE(AC_ACTY_CODE,'CA',AC_DESCRIPT"&
    "ION,null )  ,AC_ACTY_CODE  ,AC_ACCL_CO".

           02  FILLER PIC X(256) VALUE "DE  ,null   ,'*'  ,NVL(AC_ACCRED_BODY,'Q')  ,NVL(AT_ACCRED_LOGO,'*')   from BTEC.AWARD_CODES"&
    " ,AWARD_TITLES ,APPROVAL_APPLICATION ,BTEC.APPROVAL_AWARDS where (((((AW_AWARD_CODE between '23' and '26' or AW_AWARD_CODE bet"&
    "ween '33' and '37') and AW_COURSE_NUMB".

           02  FILLER PIC X(114) VALUE "ER=:b10) and AA_APPLICAT_NO=(AW_APPLICAT_NO+0)) and AT_NUMBER=TO_NUMBER(AA_BTEC_TITLE)) and "&
    "AC_CODE=AW_AWARD_CODE)".

01  SQ0061 GLOBAL.
           02  FILLER PIC X(108) VALUE "select AA_MAIN_BOARD  ,AA_BTEC_TITLE   into :b1,:b2  from BTEC.APPROVAL_APPLICATION where AA"&
    "_APPLICAT_NO=:b3".

01  SQ0062 GLOBAL.
           02  FILLER PIC X(52) VALUE "select FN_GET_SQA_CODE(:b1,3)   into :b2  from DUAL ".

01  SQ0063 GLOBAL.
           02  FILLER PIC X(256) VALUE "select DECODE(AACO_QCA_CODE,null ,null ,(((((' : QN '||SUBSTR(AACO_QCA_CODE,1,3))||'/')||SUB"&
    "STR(AACO_QCA_CODE,4,4))||'/')||SUBSTR(AACO_QCA_CODE,8)))   into :b1  from AT_AWARD_CODES ,APPROVAL_APPLICATION ,APPROVAL_AWARD"&
    "S where (((AACO_AT_NUMBER=TO_NUMBER(AA".

           02  FILLER PIC X(115) VALUE "_BTEC_TITLE) and AACO_AC_CODE=(AW_AWARD_CODE||'')) and AA_APPLICAT_NO=(AW_APPLICAT_NO+0)) an"&
    "d AW_COURSE_NUMBER=:b2)".

01  SQ0064 GLOBAL.
           02  FILLER PIC X(256) VALUE "select DECODE(AACL_QCA_CODE,null ,null ,(((((' : QN '||SUBSTR(AACL_QCA_CODE,1,3))||'/')||SUB"&
    "STR(AACL_QCA_CODE,4,4))||'/')||SUBSTR(AACL_QCA_CODE,8)))   into :b1  from AT_AWARD_CODE_LEVELS ,APPROVAL_APPLICATION ,APPROVAL"&
    "_AWARDS where ((((AW_COURSE_NUMBER=:b2".

           02  FILLER PIC X(142) VALUE " and AA_APPLICAT_NO=(AW_APPLICAT_NO+0)) and AACL_AT_NUMBER=TO_NUMBER(AA_BTEC_TITLE)) and AAC"&
    "L_AC_CODE=(AW_AWARD_CODE||'')) and AACL_LEVEL=:b3)".

01  SQ0065 GLOBAL.
           02  FILLER PIC X(255) VALUE "select count(1)   into :b1  from NVQ_STUDENT_COMPETENCE_UNITS ,NVQ_COMPETENCE_UNITS where (("&
    "(NSCU_ST_REG_NO=:b2 and NSCU_ACHIEVED_YEAR is  not null ) and NCUN_NCVQ_CODE=NSCU_NCUN_NCVQ_CODE) and NVL(NCUN_CERTIFICATE_END"&
    "_DATE,TRUNC(SYSDATE))<TRUNC(SYSDATE))".

01  SQ0066 GLOBAL.
           02  FILLER PIC X(245) VALUE "select count(1)   into :b1  from TRANSFER_NVQ_UNITS ,NVQ_COMPETENCE_UNITS where (((TNUN_ST_R"&
    "EG_NO=:b2 and TNUN_ACHIEVED_YEAR is  not null ) and NCUN_NCVQ_CODE=TNUN_NCUN_NCVQ_CODE) and NVL(NCUN_CERTIFICATE_END_DATE,TRUN"&
    "C(SYSDATE))<TRUNC(SYSDATE))".

01  SQ0067 GLOBAL.
           02  FILLER PIC X(129) VALUE "begin  PR_WRITE_AWARDS_FILES_TO_DB ( 'STP070' , :b1 , :b2 , :b3 , :b4 , :b5 , :b6:i6 , :b7:i"&
    "7 , :b8:i8 , :b9 , :b10:i10 ) ; END ;".

01  SQ0068 GLOBAL.
           02  FILLER PIC X(162) VALUE "insert into BTEC_REJECTED_STUDENTS(BRS_PROG_NO,BRS_RUN_DATE,BRS_REG_NO,BRS_CENTRE_NO,BRS_COU"&
    "RSE_NO)select 'STP070'  ,SYSDATE  ,:b1  ,:b2  ,:b3   from SYSTEM.DUAL ".

01  SQLCTX GLOBAL PIC S9(9) COMP-5 VALUE +487249497.


01  SQLEXD GLOBAL.
           02  SQL-SQLVSN   PIC S9(18) COMP-5 VALUE +13.
           02  SQL-ARRSIZ   PIC S9(9) COMP-5 VALUE +36.
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
           02  SQL-SQHSTV   PIC S9(18) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQHSTL   PIC S9(18) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQHSTS   PIC S9(18) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQINDV   PIC S9(18) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQINDS   PIC S9(18) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQHARM   PIC S9(18) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQHARC   PIC S9(18) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQADTO   PIC S9(4) COMP-5 OCCURS 36 TIMES.
           02  SQL-SQTDSO   PIC S9(4) COMP-5 OCCURS 36 TIMES.


01  SQ0001 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ST_REG_NO  ,NVL(ST_CERT_NAME,((ST_FORENAMES||' ')||ST_SURNAME))  ,ST_CENTRE_ID  ,ST_C"&
    "OURSE_ID  ,ST_COMB_ID  ,TO_NUMBER(ST_CERT_NO)  ,ST_AWARD_CLAIM  ,((RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,NVL(TO_DATE(:b1,'DD-MON-"&
    "YYYY'),SYSDATE)),'MONTH'),' ')||' ')||".

           02  FILLER PIC X(256) VALUE "TO_CHAR(NVL(ST_AWARD_PRINTED,NVL(TO_DATE(:b1,'DD-MON-YYYY'),SYSDATE)),'YYYY'))  ,TO_CHAR(NVL"&
    "(ST_AWARD_PRINTED,NVL(TO_DATE(:b1,'DD-MON-YYYY'),SYSDATE)),'DD-MON-YYYY')  ,ST_AWARD_ELIG  ,ST_REG_TYPE  ,NVL(ST_BNM_GRADE,ST_"&
    "OVER_GRADE)  ,ST_FALLBACK  ,TO_CHAR(ST".

           02  FILLER PIC X(256) VALUE "_BIRTH_DATE,'DD:MM:YY')  ,ST_INST_LOCATION  ,ST_COA_SYLLABUS_CODE  ,ST_COA_LEVEL_ACHIEVED  ,"&
    "DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,ECLE_WELSH_CODE,ECLE_ENGLISH_CODE)  ,NVL(ST_RECON_IND,'N')  ,'N'  ,NVL(TO_CHAR(ST_AWARD_ISS"&
    "UE,'DD-MON-YYYY'),:b2)  ,ST_SCHEME_REG".

           02  FILLER PIC X(256) VALUE "_NO  ,AA_BTEC_TITLE  ,AW_AWARD_CODE  ,ST_NOPS_REQD  ,ST_NVQ_ROA_CODE  ,ST_GNVQ_CUC_CODE  ,ST"&
    "_CRED_TRANS_CODE  ,SUBSTR(ST_ABS_LEVEL_ACHIEVED,2,1)  ,ST_ABS_LEVEL_ACHIEVED  ,AW_CERT_FLAG  ,ST_DSA_SRU_ID  ,'N'  ,CLTE_TEXT "&
    "  from BTEC.CERTIFICATE_LANGUAGE_TEXT ".

           02  FILLER PIC X(256) VALUE ",BTEC.ENTRY_COA_LEVELS ,AWARD_CODES ,BTEC.APPROVAL_APPLICATION ,BTEC.APPROVAL_AWARDS ,BTEC.S"&
    "TUDENTS where ((((((((((((((((PK_BNM.FN_10_WEEK_BLOCK(ST_REG_NO)='N' and PK_BNM.FN_HOLD_CERTIFICATION(ST_REG_NO)='N') and AC_C"&
    "ODE=(AW_AWARD_CODE||'')) and AC_EXTERN".

           02  FILLER PIC X(256) VALUE "AL_CERT_NO is null ) and AW_COURSE_NUMBER=ST_COURSE_ID) and AW_APPLICAT_NO=AA_APPLICAT_NO) a"&
    "nd ECLE_AT_CODE(+)=ST_COA_SYLLABUS_CODE) and ECLE_LEVEL(+)=ST_COA_LEVEL_ACHIEVED) and CLTE_AW_CERT_FLAG(+)=AW_CERT_FLAG) and S"&
    "T_COURSE_ID<>'KSQ00') and ST_REG_TYPE<".

           02  FILLER PIC X(256) VALUE ">'I') and ST_AWARD_ELIG='Y') and ST_CERT_NO is null ) and NVL(ST_DELETE,'N')<>'Y') and ((AC_"&
    "CODE='Y6' and FN_CHECK_TA_OVERGRADE_VISIBLE(ST_REG_NO)='Y') or AC_CODE<>'Y6')) and  not exists (select 1   from BTEC.TEMP_EC49"&
    "40_EXCLUDE_RA_AC_AT ,BTEC.TEMP_EC4940_".

           02  FILLER PIC X(256) VALUE "SUMMER_PARAM_VALUE where ((((TEMP_AC_CODE=AW_AWARD_CODE and TEMP_AT_NUMBER=AA_BTEC_TITLE) an"&
    "d TEMP_QUAL_CATEGORY='B1') and PARAM_NAME='B1 CERTIFICATION PRINT') and SYSDATE<TO_DATE(PARAM_VALUE,'DD-MON-YYYY')))) and :b3="&
    "'++++++') union select ST_REG_NO  ,NVL".

           02  FILLER PIC X(256) VALUE "(ST_CERT_NAME,((ST_FORENAMES||' ')||ST_SURNAME))  ,ST_CENTRE_ID  ,ST_COURSE_ID  ,ST_COMB_ID "&
    " ,TO_NUMBER(ST_CERT_NO)  ,ST_AWARD_CLAIM  ,((RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,NVL(TO_DATE(:b1,'DD-MON-YYYY'),SYSDATE)),'MONT"&
    "H'),' ')||' ')||TO_CHAR(NVL(ST_AWARD_P".

           02  FILLER PIC X(256) VALUE "RINTED,NVL(TO_DATE(:b1,'DD-MON-YYYY'),SYSDATE)),'YYYY'))  ,TO_CHAR(NVL(ST_AWARD_PRINTED,NVL("&
    "TO_DATE(:b1,'DD-MON-YYYY'),SYSDATE)),'DD-MON-YYYY')  ,ST_AWARD_ELIG  ,ST_REG_TYPE  ,NVL(ST_BNM_GRADE,ST_OVER_GRADE)  ,ST_FALLB"&
    "ACK  ,TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY'".

           02  FILLER PIC X(256) VALUE ")  ,ST_INST_LOCATION  ,ST_COA_SYLLABUS_CODE  ,ST_COA_LEVEL_ACHIEVED  ,DECODE(SUBSTR(ST_CENTR"&
    "E_ID,1,2),68,ECLE_WELSH_CODE,ECLE_ENGLISH_CODE)  ,NVL(ST_RECON_IND,'N')  ,'N'  ,NVL(TO_CHAR(ST_AWARD_ISSUE,'DD-MON-YYYY'),:b2)"&
    "  ,ST_SCHEME_REG_NO  ,AA_BTEC_TITLE  ,".

           02  FILLER PIC X(256) VALUE "AW_AWARD_CODE  ,ST_NOPS_REQD  ,ST_NVQ_ROA_CODE  ,ST_GNVQ_CUC_CODE  ,ST_CRED_TRANS_CODE  ,SUB"&
    "STR(ST_ABS_LEVEL_ACHIEVED,2,1)  ,ST_ABS_LEVEL_ACHIEVED  ,AW_CERT_FLAG  ,ST_DSA_SRU_ID  ,'N'  ,CLTE_TEXT   from BTEC.CERTIFICAT"&
    "E_LANGUAGE_TEXT ,BTEC.ENTRY_COA_LEVELS".

           02  FILLER PIC X(256) VALUE " ,BTEC.APPROVAL_APPLICATION ,BTEC.STUDENTS ,AWARD_CODES ,BTEC.APPROVAL_AWARDS ,AWARDS_RUN_CE"&
    "NTRES where (((((((((((((((((PK_BNM.FN_10_WEEK_BLOCK(ST_REG_NO)='N' and PK_BNM.FN_HOLD_CERTIFICATION(ST_REG_NO)='N') and AC_CO"&
    "DE=(AW_AWARD_CODE||'')) and AC_EXTERNA".

           02  FILLER PIC X(256) VALUE "L_CERT_NO is null ) and AW_COURSE_NUMBER=ST_COURSE_ID) and AW_APPLICAT_NO=AA_APPLICAT_NO) an"&
    "d ECLE_AT_CODE(+)=ST_COA_SYLLABUS_CODE) and ECLE_LEVEL(+)=ST_COA_LEVEL_ACHIEVED) and CLTE_AW_CERT_FLAG(+)=AW_CERT_FLAG) and ST"&
    "_COURSE_ID<>'KSQ00') and ST_REG_TYPE<>".

           02  FILLER PIC X(256) VALUE "'I') and ST_AWARD_ELIG='Y') and ST_CERT_NO is null ) and ST_CENTRE_ID=ARUN_CENTRE_ID) and NV"&
    "L(ST_DELETE,'N')<>'Y') and ((AC_CODE='Y6' and FN_CHECK_TA_OVERGRADE_VISIBLE(ST_REG_NO)='Y') or AC_CODE<>'Y6')) and  not exists"&
    " (select 1   from BTEC.TEMP_EC4940_EXC".

           02  FILLER PIC X(256) VALUE "LUDE_RA_AC_AT ,BTEC.TEMP_EC4940_SUMMER_PARAM_VALUE where ((((TEMP_AC_CODE=AW_AWARD_CODE and "&
    "TEMP_AT_NUMBER=AA_BTEC_TITLE) and TEMP_QUAL_CATEGORY='B1') and PARAM_NAME='B1 CERTIFICATION PRINT') and SYSDATE<TO_DATE(PARAM_"&
    "VALUE,'DD-MON-YYYY')))) and :b3='TABLE".

           02  FILLER PIC X(13) VALUE "')           ".

01  SQ0002 GLOBAL.
           02  FILLER PIC X(80) VALUE "select RE_CODE  ,RE_DESCRIPTION   from BTEC.RESULTS  order by RE_CODE           ".

01  SQ0003 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ST_REG_NO  ,NVL(ST_CERT_NAME,((ST_FORENAMES||' ')||ST_SURNAME))  ,ST_CENTRE_ID  ,ST_C"&
    "OURSE_ID  ,ST_COMB_ID  ,TO_NUMBER(DECODE(SUBSTR(ST_CERT_NO,1,1),'T',SUBSTR(ST_CERT_NO,2),'S',SUBSTR(ST_CERT_NO,2),'F',SUBSTR(S"&
    "T_CERT_NO,2),'R',SUBSTR(ST_CERT_NO,2),".

           02  FILLER PIC X(256) VALUE "'H',SUBSTR(ST_CERT_NO,4),'A',SUBSTR(ST_CERT_NO,5),'D',SUBSTR(ST_CERT_NO,5),ST_CERT_NO))  ,ST"&
    "_AWARD_CLAIM  ,((RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,NVL(ARC_AWARD_DATE,SYSDATE)),'MONTH'),' ')||' ')||TO_CHAR(NVL(ST_AWARD_PRI"&
    "NTED,NVL(ARC_AWARD_DATE,SYSDATE)),'YYY".

           02  FILLER PIC X(256) VALUE "Y'))  ,TO_CHAR(NVL(ST_AWARD_PRINTED,NVL(ARC_AWARD_DATE,SYSDATE)),'DD-MON-YYYY')  ,ST_AWARD_E"&
    "LIG  ,ST_REG_TYPE  ,NVL(ST_BNM_GRADE,ST_OVER_GRADE)  ,ST_FALLBACK  ,TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY')  ,ST_INST_LOCATION  ,ST_"&
    "COA_SYLLABUS_CODE  ,ST_COA_LEVEL_ACHIE".

           02  FILLER PIC X(256) VALUE "VED  ,DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,ECLE_WELSH_CODE,ECLE_ENGLISH_CODE)  ,NVL(ST_RECON_I"&
    "ND,'N')  ,'N'  ,NVL(TO_CHAR(ST_AWARD_ISSUE,'DD-MON-YYYY'),:b1)  ,ST_NOPS_REQD  ,ST_NVQ_ROA_CODE  ,ST_GNVQ_CUC_CODE  ,ST_CRED_T"&
    "RANS_CODE  ,SUBSTR(ST_ABS_LEVEL_ACHIEV".

           02  FILLER PIC X(256) VALUE "ED,2,1)  ,ST_ABS_LEVEL_ACHIEVED  ,AW_CERT_FLAG  ,AA_BTEC_TITLE  ,AW_AWARD_CODE  ,DECODE(ARC_"&
    "REPRINT_WORDING,'Y','  REPLACEMENT',' ')  ,NVL(ARC_FIRST_REG_RCFE_ID,0)  ,NVL(ARC_LAST_REG_RCFE_ID,0)  ,ST_DSA_SRU_ID  ,DECODE"&
    "(SIGN((TO_DATE('01/01/2014','DD/MM/YYY".

           02  FILLER PIC X(256) VALUE "Y')-NVL(ST_AWARD_ISSUE,SYSDATE))),1,'Y','N')  ,CLTE_TEXT   from BTEC.ENTRY_COA_LEVELS ,BTEC."&
    "APPROVAL_APPLICATION ,BTEC.STUDENTS ,BTEC.APPROVAL_AWARDS ,BTEC.ALL_REPRINT_CRITERIA ,BTEC.CERTIFICATE_LANGUAGE_TEXT where ((("&
    "(((((((((ARC_REPRINT_TYPE='B' and ST_R".

           02  FILLER PIC X(256) VALUE "EG_NO between ARC_FIRST_REG_NO and NVL(ARC_LAST_REG_NO,ARC_FIRST_REG_NO)) and AW_COURSE_NUMB"&
    "ER=ST_COURSE_ID) and AW_APPLICAT_NO=AA_APPLICAT_NO) and ECLE_AT_CODE(+)=ST_COA_SYLLABUS_CODE) and ECLE_LEVEL(+)=ST_COA_LEVEL_A"&
    "CHIEVED) and CLTE_AW_CERT_FLAG(+)=AW_C".

           02  FILLER PIC X(256) VALUE "ERT_FLAG) and ST_COURSE_ID<>'KSQ00') and NVL(ST_UNIVERSITY_IND,'N')<>'Y') and ST_COURSE_ID i"&
    "s  not null ) and ST_CERT_NO is  not null ) and (ST_AWARD_ELIG||'')='Y') and ARC_DB='P') union select ST_REG_NO  ,NVL(ST_CERT_"&
    "NAME,((ST_FORENAMES||' ')||ST_SURNAME)".

           02  FILLER PIC X(256) VALUE ")  ,ST_CENTRE_ID  ,ST_COURSE_ID  ,ST_COMB_ID  ,TO_NUMBER(DECODE(SUBSTR(ST_CERT_NO,1,1),'T',S"&
    "UBSTR(ST_CERT_NO,2),'S',SUBSTR(ST_CERT_NO,2),'F',SUBSTR(ST_CERT_NO,2),'R',SUBSTR(ST_CERT_NO,2),'H',SUBSTR(ST_CERT_NO,4),'A',SU"&
    "BSTR(ST_CERT_NO,5),'D',SUBSTR(ST_CERT_".

           02  FILLER PIC X(256) VALUE "NO,5),ST_CERT_NO))  ,ST_AWARD_CLAIM  ,((RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,NVL(ARC_AWARD_DAT"&
    "E,SYSDATE)),'MONTH'),' ')||' ')||TO_CHAR(NVL(ST_AWARD_PRINTED,NVL(ARC_AWARD_DATE,SYSDATE)),'YYYY'))  ,TO_CHAR(NVL(ST_AWARD_PRI"&
    "NTED,NVL(ARC_AWARD_DATE,SYSDATE)),'DD-".

           02  FILLER PIC X(256) VALUE "MON-YYYY')  ,ST_AWARD_ELIG  ,ST_REG_TYPE  ,NVL(ST_BNM_GRADE,ST_OVER_GRADE)  ,ST_FALLBACK  ,T"&
    "O_CHAR(ST_BIRTH_DATE,'DD:MM:YY')  ,ST_INST_LOCATION  ,ST_COA_SYLLABUS_CODE  ,ST_COA_LEVEL_ACHIEVED  ,DECODE(SUBSTR(ST_CENTRE_I"&
    "D,1,2),68,ECLE_WELSH_CODE,ECLE_ENGLISH".

           02  FILLER PIC X(256) VALUE "_CODE)  ,NVL(ST_RECON_IND,'N')  ,'N'  ,NVL(TO_CHAR(ST_AWARD_ISSUE,'DD-MON-YYYY'),:b1)  ,ST_N"&
    "OPS_REQD  ,ST_NVQ_ROA_CODE  ,ST_GNVQ_CUC_CODE  ,ST_CRED_TRANS_CODE  ,SUBSTR(ST_ABS_LEVEL_ACHIEVED,2,1)  ,ST_ABS_LEVEL_ACHIEVED"&
    "  ,AW_CERT_FLAG  ,AA_BTEC_TITLE  ,AW_A".

           02  FILLER PIC X(256) VALUE "WARD_CODE  ,DECODE(ARC_REPRINT_WORDING,'Y','  REPLACEMENT',' ')  ,NVL(ARC_FIRST_REG_RCFE_ID,"&
    "0)  ,NVL(ARC_LAST_REG_RCFE_ID,0)  ,ST_DSA_SRU_ID  ,DECODE(SIGN((TO_DATE('01/01/2014','DD/MM/YYYY')-NVL(ST_AWARD_ISSUE,SYSDATE)"&
    ")),1,'Y','N')  ,CLTE_TEXT   from BTEC.".

           02  FILLER PIC X(256) VALUE "ENTRY_COA_LEVELS ,BTEC.APPROVAL_APPLICATION ,BTEC.STUDENTS@ARCA ,BTEC.APPROVAL_AWARDS ,BTEC."&
    "ALL_REPRINT_CRITERIA ,BTEC.CERTIFICATE_LANGUAGE_TEXT where ((((((((((((ARC_REPRINT_TYPE='B' and ST_REG_NO between ARC_FIRST_RE"&
    "G_NO and NVL(ARC_LAST_REG_NO,ARC_FIRST".

           02  FILLER PIC X(256) VALUE "_REG_NO)) and AW_COURSE_NUMBER=ST_COURSE_ID) and AW_APPLICAT_NO=AA_APPLICAT_NO) and ECLE_AT_"&
    "CODE(+)=ST_COA_SYLLABUS_CODE) and ECLE_LEVEL(+)=ST_COA_LEVEL_ACHIEVED) and CLTE_AW_CERT_FLAG(+)=AW_CERT_FLAG) and ST_COURSE_ID"&
    "<>'KSQ00') and NVL(ST_UNIVERSITY_IND,'".

           02  FILLER PIC X(127) VALUE "N')<>'Y') and ST_COURSE_ID is  not null ) and ST_CERT_NO is  not null ) and (ST_AWARD_ELIG||"&
    "'')='Y') and ARC_DB='A')           ".

01  SQ0004 GLOBAL.
           02  FILLER PIC X(256) VALUE "select ST_REG_NO  ,NVL(ST_CERT_NAME,((ST_FORENAMES||' ')||ST_SURNAME))  ,ST_CENTRE_ID  ,ST_G"&
    "NVQ_REGISTERED_ID  ,ST_COMB_ID  ,TO_NUMBER(ST_GNVQ_CERTIFICATE_NO)  ,ST_GNVQ_CLAIM_ID  ,((RTRIM(TO_CHAR(NVL(ST_GNVQ_CERTIFICAT"&
    "E_DISP_DATE,NVL(ARC_AWARD_DATE,SYSDATE".

           02  FILLER PIC X(256) VALUE ")),'MONTH'),' ')||' ')||TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,NVL(ARC_AWARD_DATE,SYSDATE"&
    ")),'YYYY'))  ,TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,NVL(ARC_AWARD_DATE,SYSDATE)),'DD-MON-YYYY')  ,ST_GNVQ_ELIGIBILITY_CODE"&
    "  ,ST_REG_TYPE  ,null   ,ST_FALLBACK  ".

           02  FILLER PIC X(256) VALUE ",TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY')  ,ST_INST_LOCATION  ,ST_COA_SYLLABUS_CODE  ,ST_COA_LEVEL_"&
    "ACHIEVED  ,DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,ECLE_WELSH_CODE,ECLE_ENGLISH_CODE)  ,NVL(ST_RECON_IND,'N')  ,'Y'  ,NVL(TO_CHAR(S"&
    "T_GNVQ_CERTIFICATE_PRINT_DATE,'DD-MON-".

           02  FILLER PIC X(256) VALUE "YYYY'),:b1)  ,DECODE(ARC_REPRINT_WORDING,'Y','  REPLACEMENT',' ')  ,NVL(ARC_FIRST_REG_RCFE_I"&
    "D,0)  ,NVL(ARC_LAST_REG_RCFE_ID,0)  ,ST_DSA_SRU_ID  ,'N'   from BTEC.ENTRY_COA_LEVELS ,BTEC.STUDENTS ,BTEC.GNVQS ,BTEC.ALL_REP"&
    "RINT_CRITERIA where ((((((((((((((((AR".

           02  FILLER PIC X(256) VALUE "C_REPRINT_TYPE='B' and ST_CERT_NO is  not null ) and ST_REG_NO between ARC_FIRST_REG_NO and "&
    "NVL(ARC_LAST_REG_NO,ARC_FIRST_REG_NO)) and ARC_AWARD_CODE='27') and ST_GNVQ_REGISTERED_ID=GNVQ_ID) and ST_RECON_IND is null ) "&
    "and TO_DATE(TO_CHAR(ST_GNVQ_CERTIFICAT".

           02  FILLER PIC X(256) VALUE "E_DISP_DATE,'DD-MON-YYYY'),'DD-MON-YYYY') between TO_DATE('01-JUL-1999','DD-MON-YYYY') and T"&
    "O_DATE('31-AUG-1999','DD-MON-YYYY')) and GNVQ_PILOT_IND='Y') and GNVQ_SINGLEAW_IND is null ) and ECLE_AT_CODE(+)=ST_COA_SYLLAB"&
    "US_CODE) and ECLE_LEVEL(+)=ST_COA_LEVE".

           02  FILLER PIC X(256) VALUE "L_ACHIEVED) and ST_GNVQ_REGISTERED_ID is  not null ) and ST_GNVQ_CERTIFICATE_NO is  not null"&
    " ) and (ST_GNVQ_ELIGIBILITY_CODE||'')='Y') and ST_COURSE_ID<>'KSQ00') and NVL(ST_UNIVERSITY_IND,'N')<>'Y') and ARC_DB='P') uni"&
    "on select ST_REG_NO  ,NVL(ST_CERT_NAME".

           02  FILLER PIC X(256) VALUE ",((ST_FORENAMES||' ')||ST_SURNAME))  ,ST_CENTRE_ID  ,ST_GNVQ_REGISTERED_ID  ,ST_COMB_ID  ,TO"&
    "_NUMBER(ST_GNVQ_CERTIFICATE_NO)  ,ST_GNVQ_CLAIM_ID  ,((RTRIM(TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,NVL(ARC_AWARD_DATE,SYSD"&
    "ATE)),'MONTH'),' ')||' ')||TO_CHAR(NVL".

           02  FILLER PIC X(256) VALUE "(ST_GNVQ_CERTIFICATE_DISP_DATE,NVL(ARC_AWARD_DATE,SYSDATE)),'YYYY'))  ,TO_CHAR(NVL(ST_GNVQ_C"&
    "ERTIFICATE_DISP_DATE,NVL(ARC_AWARD_DATE,SYSDATE)),'DD-MON-YYYY')  ,ST_GNVQ_ELIGIBILITY_CODE  ,ST_REG_TYPE  ,null   ,ST_FALLBAC"&
    "K  ,TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY') ".

           02  FILLER PIC X(256) VALUE " ,ST_INST_LOCATION  ,ST_COA_SYLLABUS_CODE  ,ST_COA_LEVEL_ACHIEVED  ,DECODE(SUBSTR(ST_CENTRE_"&
    "ID,1,2),68,ECLE_WELSH_CODE,ECLE_ENGLISH_CODE)  ,NVL(ST_RECON_IND,'N')  ,'Y'  ,NVL(TO_CHAR(ST_GNVQ_CERTIFICATE_PRINT_DATE,'DD-M"&
    "ON-YYYY'),:b1)  ,DECODE(ARC_REPRINT_WO".

           02  FILLER PIC X(256) VALUE "RDING,'Y','  REPLACEMENT',' ')  ,NVL(ARC_FIRST_REG_RCFE_ID,0)  ,NVL(ARC_LAST_REG_RCFE_ID,0) "&
    " ,ST_DSA_SRU_ID  ,'N'   from BTEC.ENTRY_COA_LEVELS ,BTEC.STUDENTS@ARCA ,BTEC.GNVQS ,BTEC.ALL_REPRINT_CRITERIA where (((((((((("&
    "((((((ARC_REPRINT_TYPE='B' and ST_CERT".

           02  FILLER PIC X(256) VALUE "_NO is  not null ) and ST_REG_NO between ARC_FIRST_REG_NO and NVL(ARC_LAST_REG_NO,ARC_FIRST_"&
    "REG_NO)) and ARC_AWARD_CODE='27') and ST_GNVQ_REGISTERED_ID=GNVQ_ID) and ST_RECON_IND is null ) and TO_DATE(TO_CHAR(ST_GNVQ_CE"&
    "RTIFICATE_DISP_DATE,'DD-MON-YYYY'),'DD".

           02  FILLER PIC X(256) VALUE "-MON-YYYY') between TO_DATE('01-JUL-1999','DD-MON-YYYY') and TO_DATE('31-AUG-1999','DD-MON-Y"&
    "YYY')) and GNVQ_PILOT_IND='Y') and GNVQ_SINGLEAW_IND is null ) and ECLE_AT_CODE(+)=ST_COA_SYLLABUS_CODE) and ECLE_LEVEL(+)=ST_"&
    "COA_LEVEL_ACHIEVED) and ST_GNVQ_REGIST".

           02  FILLER PIC X(195) VALUE "ERED_ID is  not null ) and ST_GNVQ_CERTIFICATE_NO is  not null ) and (ST_GNVQ_ELIGIBILITY_CO"&
    "DE||'')='Y') and ST_COURSE_ID<>'KSQ00') and NVL(ST_UNIVERSITY_IND,'N')<>'Y') and ARC_DB='A')           ".

01  SQ0005 GLOBAL.
           02  FILLER PIC X(114) VALUE "select TO_CHAR(PBBT_AWARD_TITLE)  ,PBBT_AWARD_CODE   from PROFESSIONAL_BODY_BTECS where PBBT"&
    "_PBOD_ID=10           ".

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
           02     FILLER PIC S9(4) COMP-5 VALUE +1998.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +85.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +2029.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +541.
           02     FILLER PIC S9(4) COMP-5 VALUE +2046.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +4109.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +2081.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +129.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +80.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +2085.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +144.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +3967.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +2104.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +167.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +3523.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +2107.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +190.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +2132.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +213.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +2154.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +228.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +8.
           02     FILLER PIC S9(4) COMP-5 VALUE +140.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +2175.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +259.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +151.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +2188.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +298.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +114.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +521.
           02     FILLER PIC S9(4) COMP-5 VALUE +2299.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +313.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +2314.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +336.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +2333.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +351.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +63.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +2349.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +370.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +120.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +2384.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +401.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +2467.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +34.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +552.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +2657.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +36.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +711.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +525.
           02     FILLER PIC S9(4) COMP-5 VALUE +2831.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +26.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +830.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +65.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +2933.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +853.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +13.
           02     FILLER PIC S9(4) COMP-5 VALUE +122.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +2947.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +880.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +14.
           02     FILLER PIC S9(4) COMP-5 VALUE +297.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +2982.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +907.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +15.
           02     FILLER PIC S9(4) COMP-5 VALUE +217.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +3011.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +934.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +16.
           02     FILLER PIC S9(4) COMP-5 VALUE +62.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +514.
           02     FILLER PIC S9(4) COMP-5 VALUE +3050.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +949.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +17.
           02     FILLER PIC S9(4) COMP-5 VALUE +483.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +3209.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1008.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +18.
           02     FILLER PIC S9(4) COMP-5 VALUE +169.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +3613.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1043.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +19.
           02     FILLER PIC S9(4) COMP-5 VALUE +94.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +3897.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1078.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +20.
           02     FILLER PIC S9(4) COMP-5 VALUE +92.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +3914.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1113.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +21.
           02     FILLER PIC S9(4) COMP-5 VALUE +83.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +3942.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1144.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +22.
           02     FILLER PIC S9(4) COMP-5 VALUE +92.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +3957.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1179.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +23.
           02     FILLER PIC S9(4) COMP-5 VALUE +93.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +3984.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1214.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +24.
           02     FILLER PIC S9(4) COMP-5 VALUE +90.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +3999.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1245.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +25.
           02     FILLER PIC S9(4) COMP-5 VALUE +68.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +4022.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1272.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +26.
           02     FILLER PIC S9(4) COMP-5 VALUE +63.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +4048.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1295.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +27.
           02     FILLER PIC S9(4) COMP-5 VALUE +88.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +4061.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1318.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +28.
           02     FILLER PIC S9(4) COMP-5 VALUE +62.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +4078.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1337.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +29.
           02     FILLER PIC S9(4) COMP-5 VALUE +89.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +4091.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1360.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +30.
           02     FILLER PIC S9(4) COMP-5 VALUE +90.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +4113.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1383.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +31.
           02     FILLER PIC S9(4) COMP-5 VALUE +466.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +5816.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1410.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +32.
           02     FILLER PIC S9(4) COMP-5 VALUE +210.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +517.
           02     FILLER PIC S9(4) COMP-5 VALUE +5907.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1437.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +33.
           02     FILLER PIC S9(4) COMP-5 VALUE +138.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +5950.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1480.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +34.
           02     FILLER PIC S9(4) COMP-5 VALUE +164.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +517.
           02     FILLER PIC S9(4) COMP-5 VALUE +5994.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1515.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +35.
           02     FILLER PIC S9(4) COMP-5 VALUE +710.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +6037.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1530.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +36.
           02     FILLER PIC S9(4) COMP-5 VALUE +60.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +514.
           02     FILLER PIC S9(4) COMP-5 VALUE +6091.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1545.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +37.
           02     FILLER PIC S9(4) COMP-5 VALUE +392.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +6127.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1604.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +38.
           02     FILLER PIC S9(4) COMP-5 VALUE +392.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +6164.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1663.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +39.
           02     FILLER PIC S9(4) COMP-5 VALUE +392.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +6201.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1722.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +40.
           02     FILLER PIC S9(4) COMP-5 VALUE +392.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +6238.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1781.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +41.
           02     FILLER PIC S9(4) COMP-5 VALUE +489.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +6295.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1796.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +6348.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1811.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +6352.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1826.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +527.
           02     FILLER PIC S9(4) COMP-5 VALUE +6355.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1841.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +42.
           02     FILLER PIC S9(4) COMP-5 VALUE +67.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +517.
           02     FILLER PIC S9(4) COMP-5 VALUE +6366.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1864.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +43.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +541.
           02     FILLER PIC S9(4) COMP-5 VALUE +6380.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1879.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +44.
           02     FILLER PIC S9(4) COMP-5 VALUE +12.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +541.
           02     FILLER PIC S9(4) COMP-5 VALUE +6442.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1894.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +45.
           02     FILLER PIC S9(4) COMP-5 VALUE +171.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +517.
           02     FILLER PIC S9(4) COMP-5 VALUE +6478.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1929.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +46.
           02     FILLER PIC S9(4) COMP-5 VALUE +173.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +6511.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +9.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1964.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +47.
           02     FILLER PIC S9(4) COMP-5 VALUE +535.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6574.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1995.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +48.
           02     FILLER PIC S9(4) COMP-5 VALUE +114.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6647.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2022.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +49.
           02     FILLER PIC S9(4) COMP-5 VALUE +344.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6687.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2065.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +50.
           02     FILLER PIC S9(4) COMP-5 VALUE +353.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6734.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +4.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2096.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +51.
           02     FILLER PIC S9(4) COMP-5 VALUE +561.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6797.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2135.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +52.
           02     FILLER PIC S9(4) COMP-5 VALUE +218.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6886.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2158.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +53.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6922.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +5.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2193.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +54.
           02     FILLER PIC S9(4) COMP-5 VALUE +102.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6980.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2216.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +55.
           02     FILLER PIC S9(4) COMP-5 VALUE +197.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +6995.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2243.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +56.
           02     FILLER PIC S9(4) COMP-5 VALUE +197.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7015.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2270.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +57.
           02     FILLER PIC S9(4) COMP-5 VALUE +128.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7045.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2297.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +58.
           02     FILLER PIC S9(4) COMP-5 VALUE +196.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +7077.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2336.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +59.
           02     FILLER PIC S9(4) COMP-5 VALUE +212.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +7122.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +7.
           02     FILLER PIC S9(4) COMP-5 VALUE +6.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2379.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +60.
           02     FILLER PIC S9(4) COMP-5 VALUE +1138.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7173.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +11.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2438.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +61.
           02     FILLER PIC S9(4) COMP-5 VALUE +108.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7235.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2465.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +62.
           02     FILLER PIC S9(4) COMP-5 VALUE +52.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7264.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2488.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +63.
           02     FILLER PIC S9(4) COMP-5 VALUE +371.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7292.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2511.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +64.
           02     FILLER PIC S9(4) COMP-5 VALUE +398.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7332.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +2538.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +65.
           02     FILLER PIC S9(4) COMP-5 VALUE +255.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7379.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2561.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +66.
           02     FILLER PIC S9(4) COMP-5 VALUE +245.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +516.
           02     FILLER PIC S9(4) COMP-5 VALUE +7409.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2.
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2584.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +67.
           02     FILLER PIC S9(4) COMP-5 VALUE +129.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +518.
           02     FILLER PIC S9(4) COMP-5 VALUE +7460.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
           02     FILLER PIC S9(4) COMP-5 VALUE +10.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +153.
           02     FILLER PIC S9(4) COMP-5 VALUE +8.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +3.
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2639.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +68.
           02     FILLER PIC S9(4) COMP-5 VALUE +162.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +515.
           02     FILLER PIC S9(4) COMP-5 VALUE +7512.
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
           02     FILLER PIC S9(4) COMP-5 VALUE +96.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +2666.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +69.
           02     FILLER PIC S9(4) COMP-5 VALUE +14.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +543.
           02     FILLER PIC S9(4) COMP-5 VALUE +7625.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
           02     FILLER PIC S9(4) COMP-5 VALUE +1.
           02     FILLER PIC S9(4) COMP-5 VALUE +0.
*       EXEC SQL  BEGIN DECLARE SECTION  END-EXEC.
*
01	WS-USER-ID			PIC X(6)	VALUE 'ABCD'.
01	WS-PASSWORD			PIC X(4)	VALUE 'ABCD'.
*
01  WS-INPUT-CENTRE                     PIC X(6)        VALUE '%%%%%%'.
*
01  WS-GREEN-LIGHT			PIC X(1).
01  WS-GREEN-LIGHT-I                    PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CENTRE-DEFERRED			PIC X(1).
01  WS-CENTRE-DEFERRED-I                PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CERT-BLOCKED			PIC X(1).
01  WS-CERT-BLOCKED-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CENTRE-NO			PIC X(6).
01  WS-COURSE-NO			PIC X(8).
01  WS-COURSE-QCA-CODE			PIC X(20).
01  WS-COMB				PIC X(1).
01  WS-OLD-HEADERS			PIC X(1).
01  WS-STATUS				PIC X(1).
01  WS-STATUS-I                         PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-STATUS2				PIC X(10).
01  WS-STATUS2-I                        PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-MESSAGE				PIC X(100).
01  WS-MESSAGE-I                        PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-REG-NO				PIC X(7).
01  WS-88REC   				PIC X(200).
01  WS-STUDENT-NAME 			PIC X(51).
01  WS-CERT-NO                          PIC S9(9) COMP-3.
01  WS-ALLOC-CERT-NO                    PIC S9(9) COMP-3.
01  WS-AWARD-CLAIM			PIC X(1).
01  WS-MONTH-YEAR			PIC X(18).
01  WS-AWARD-DATE			PIC X(11).
01  WS-ELIG				PIC X(1).
01  WS-REG-TYPE				PIC X(1).
01  WS-OVER-GRADE                       PIC X(3).
01  WS-FALLBACK				PIC X(1).
01  WS-CERT-DATE                        PIC X(11).
01  WS-ACCRED-LOGO			PIC X(1).
01  WS-AWARD-CODE			PIC X(2).
01  WS-AWARD-CODE-SAVE			PIC X(2).
01  WS-AWARD-DESC			PIC X(60).
01  WS-CERT-NO-TYPE                     PIC X(8) VALUE 'CERT_NO%'.
01  WS-RES-CODE                         PIC X(1).
01  WS-RES-DESC                         PIC X(15).
01  WS-BTEC-AWARD-CODE			PIC X(2).
01  WS-BTEC-AWARD-TITLE			PIC S9(6) COMP-3.
01  WS-TEST-CENTRE			PIC X(6).
01  WS-MERGED-CENTRE-NO                 PIC X(6).
01  WS-CENTRE-NAME-1			PIC X(45).
01  WS-CENTRE-NAME-2			PIC X(45).
01  WS-MERGED-CENTRE-NO-I		PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CENTRE-NAME-1-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CENTRE-NAME-2-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-PR-RESULT			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CURRENT-CENTRE-NO                PIC X(6).
01  WS-CURRENT-BOARD                    PIC X(2).
01  WS-QUAL-LEVEL                       PIC X(2).
01  WS-YEARS-CERT-VALID			PIC X.
01  WS-FIRST-FEE-ID			PIC S9(9) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-LAST-FEE-ID			PIC S9(9) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-AROF-RECORD-TYPE			PIC X.
01  WS-AROF-RECORD-SEQUENCE             PIC 9(8)	 VALUE ZERO.
01  WS-AROF-CENTRE			PIC X(6).
01  WS-AROF-CENTRE-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-AROF-REG-NO                      PIC X(7).
01  WS-AROF-REG-NO-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-AROF-UNIT-NO                     PIC X(8).
01  WS-AROF-UNIT-NO-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-AROF-TEXT                        PIC X(2000).
01  WS-AROF-MESSAGE                     PIC X(132).
01  WS-AROF-MESSAGE-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-AROF-RUN-TYPE			PIC X.
01  WS-AROF-FILE-TYPE			PIC X(8).
*
01  WS-MEMBERSHIP-FLAG			PIC X(1).
01  WS-AC-DESCR				PIC X(60).
01  WS-ACTY-CODE			PIC X(2).
01  WS-AC-BNM-TYPE			PIC X(1).
01  WS-AC-ACCRED-BODY			PIC X(1).
01  WS-NO-CENTRE-NAME	        	PIC X(45) VALUE 'AN APPROVED PEARSON CENTRE'.
01  WS-ACCL-CODE			PIC X(3).
01  WS-BALE-DESC			PIC X(30).
*
01  WS-TITLE-DESC1			PIC X(79).
01  WS-TITLE-DESC2			PIC X(75).
01  WS-BTEC-TITLE                       PIC X(5).
*
01  WS-APPLICAT-NO			PIC S9(9) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-MAIN-BOARD			PIC X(2).
01  WS-DUMMY				PIC X(1).
01  WS-COURSE-TYPE			PIC X(1).
01  WS-FILE-PREFIX			PIC X(5).
01  WS-PARAM-AWARD-CODE			PIC X(2).
*
01  WS-RUN-FLAG                         PIC X(01).
01  WS-CPU-TIME                         PIC X(05).
01  WS-RUN-DATE                         PIC X(11).
01  WS-PROGRAM                          PIC X(15).
01  WS-CERT-ISSUED                      PIC S9(9) COMP-3.
01  WS-CURRENT-AWARD-DATE		PIC X(11).
01  WS-BIRTH-DATE			PIC X(8).
01  WS-CERT-ISSUE-DATE			PIC X(11).
01  WS-REPRINT-WORDING                  PIC X(13).
01  WS-DUMMY-CERT-ISSUE-DATE            PIC X(11).
01  WS-CMP-ISSUE-DATE                   PIC X(8).
01  WS-CMP-DATE                         PIC X(8).
01  WS-CMP-DATES                        PIC X(1).
01  WS-INST-LOCATION			PIC X(6).
01  WS-COA-SYLLABUS-CODE                PIC X(6).
01  WS-COA-LEVEL-ACHIEVED               PIC X(1).
01  WS-COA-LEVEL-ACHIEVED-DESCR         PIC X(30).
01  WS-RECON-IND			PIC X(1).
01  WS-KSQ				PIC X(1).
01  WS-SCHEME-REG-NO			PIC X(7).
01  WS-SCHEME-REG-NO-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-BTEC-CODE			PIC S9(6) COMP-3.
01  WS-BTEC-LEVEL			PIC X(2).
01  WS-NOPS-REQD			PIC X(1).
01  WS-NOPS-REQD-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-NVQ-ROA-CODE			PIC X(1).
01  WS-NVQ-ROA-CODE-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-GNVQ-CUC-CODE			PIC X(1).
01  WS-GNVQ-CUC-CODE-I			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CRED-TRANS-CODE			PIC X(1).
01  WS-CRED-TRANS-CODE-I		PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-ABS-LEVEL-ACHIEVED		PIC X(1).
01  WS-ABS-LEVEL-ACHIEVED-I		PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-ESOL-LEVEL-ACHIEVED		PIC X(2).
01  WS-ESOL-LEVEL-ACHIEVED-I		PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CERT-FLAG			PIC X(2).
*EC2490
01  WS-CLTE-TEXT                        PIC X(70).
01  WS-TIME-DATE			PIC X(17).
01  WS-AWARD-NAME			PIC X(65).
01  WS-AWARD-CERT-DESCRIPTION-1		PIC X(30).
01  WS-AWARD-CERT-DESCRIPTION-2		PIC X(30).
01  WS-AWARD-CERT-DESCRIPTION-3		PIC X(30).
01  WS-AWARD-DOC-REF			PIC X(20).
01  WS-ERROR-CODE			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-ERROR-CODE-I                     PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-BLOCK-STATUS			PIC X(1).
01  WS-BLOCK-STATUS-I		        PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-CHECK-UNITS 			PIC X(1)  VALUE 'N'.
01  WS-CLAIM-TYPE  			PIC X(1)  VALUE 'A'.
01  WS-NPP-ID      			PIC S9(6) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-NPP-ID-I      		        PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-DECISION-DATE                    PIC X(11).
01  WS-DECISION-DATE-I                  PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01  WS-AWARD-TYPE-LOGO			PIC X(4).
01  WS-FRAN-CENTRE-LOGO	 		PIC X(4).
01  WS-LOGO-CENTRE-ID			PIC X(6).
01  WS-LOGO-COURSE-ID                   PIC X(8).
01  WS-LOGOS				PIC X(30).
01  WS-1ST-LOGO				PIC X(11).
01  WS-2ND-LOGO				PIC X(11).
01  WS-3RD-LOGO				PIC X(11).
01  WS-4TH-LOGO				PIC X(11).
01  WS-DSA-SRU-NUMBER                   PIC X(12).
01  WS-OLD-REPRINT-CERT			PIC X(1).
*
01	WS-COE-CENTRE			PIC X(6).
01	WS-COE-NAME			PIC X(70).
01	WS-COE-REG-NO			PIC X(7).
01	WS-COE-SCHEME-REG-NO		PIC X(7).
01	WS-COE-SCHEME-REG-NO-I		PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-COE-BIRTHDATE		PIC X(8).
01 	WS-COE-DATE			PIC X(11).
01	WS-COE-BTEC-CODE		PIC S9(6) COMP-3.
01	WS-COE-BTEC-LEVEL		PIC X(2).
01      WS-COE-SRU-NUMBER               PIC X(12).
*
01	WS-PBODY-LOGOS.             
	  05 WS-PBODY-CLOG-ID          	PIC X(4)  OCCURS 5 TIMES.
	  05 WS-PBODY-LOGO          	PIC X(8)  OCCURS 5 TIMES.
*
01      WS-TORG-LOGOS.             
          05 WS-TORG-CLOG-ID            PIC X(4)  OCCURS 2 TIMES.
          05 WS-TORG-LOGO               PIC X(8)  OCCURS 2 TIMES.
*
01      WS-CENTRE-LOGOS.             
          05 WS-CENTRE-CLOG-ID		PIC X(4)  OCCURS 2 TIMES.
          05 WS-CENTRE-LOGO		PIC X(8)  OCCURS 2 TIMES.
*
01	WS-CENTRE-OR-FRAN-LOGOS.
	  05 WS-CENTRE-OR-FRAN-LOGO	PIC X(4)  OCCURS 2 TIMES.
*
01  	SCLO-DETAILS.
    	03  SCLO-ST-REG-NO		PIC X(7)	OCCURS 10 TIMES.
	03  SCLO-CLOG-CODE		PIC X(8)	OCCURS 10 TIMES.
	03  SCLO-CLOG-TYPE-CODE		PIC X		OCCURS 10 TIMES.
	03  SCLO-CLOG-CODE-NUMBER	PIC S9(6) 
*                                                 COMP
                                                  COMP-5
                                                        OCCURS 10 TIMES.
*
01	SCLO-INDEX			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	SCLO-FETCHED			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
*
01	WS-RD-CERT-FILE			PIC X(15).
01	WS-DOC-AWARD-CODE		PIC X(2).
01	WS-DOC-AWARD-TITLE		PIC S9(6) COMP-3.
01	WS-DOC-AWARD-TYPE		PIC X(1).
01	WS-DOC-REF			PIC X(5).
*
01	WS-AW-RD-CERT-FILE		PIC X(15).
01	WS-AW-FILE-PREFIX		PIC X(6).
01	WS-AW-CERT-ISSUED		PIC S9(9) COMP-3.
01	WS-AW-DOC-REF			PIC X(5).
*WI409
01      WS-IRL-CC-FLAG                  PIC X(1).
01      WS-IRL-AT-NUMBER                PIC S9(6) COMP-3.
01      WS-IRL-REG-NO                   PIC X(7).
01      WS-IRL-COURSE-NO                PIC X(6).
01      WS-IRL-CENTRE-NO                PIC X(6).
*
01	WS-CERT-NO-TAB.
	03  WS-CNT-AWARD-CODE		PIC X(2)	OCCURS 1000 TIMES.
	03  WS-CNT-CV-TYPE		PIC X(16)	OCCURS 1000 TIMES.
	03  WS-CNT-NEXT-CERT-NO		PIC S9(9) COMP-3        
							OCCURS 1000 TIMES.
*
01	WS-CNT-INDEX			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-CNT-FETCHED			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-CNT-UPDATE			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-CNT-FOUND			PIC X VALUE 'N'.
*
* EC4623 - Increased the array size of below variables from 500 to 900
01	WS-FCS-FILE-TAB.
	03  WS-FFT-AWARD-CODE		PIC X(2)	OCCURS 900 TIMES.
	03  WS-FFT-AWARD-NAME		PIC X(65)	OCCURS 900 TIMES.
	03  WS-FFT-CERT-TEXT-1		PIC X(30)	OCCURS 900 TIMES.
	03  WS-FFT-CERT-TEXT-1-I	PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      	OCCURS 900 TIMES.
	03  WS-FFT-CERT-TEXT-2		PIC X(30)	OCCURS 900 TIMES.
	03  WS-FFT-CERT-TEXT-2-I	PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      	OCCURS 900 TIMES.
	03  WS-FFT-CERT-TEXT-3		PIC X(30)	OCCURS 900 TIMES.
	03  WS-FFT-CERT-TEXT-3-I	PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      	OCCURS 900 TIMES.
	03  WS-FFT-DATE-TIME		PIC X(17)	OCCURS 900 TIMES.
	03  WS-FFT-FCS-AWARD-REF	PIC X(3)	OCCURS 900 TIMES.
	03  WS-FFT-OLD-CERT-TEXT-1	PIC X(30)	OCCURS 900 TIMES.
	03  WS-FFT-OLD-CERT-TEXT-2	PIC X(30)	OCCURS 900 TIMES.
	03  WS-FFT-OLD-CERT-TEXT-3	PIC X(30)	OCCURS 900 TIMES.
*
01	WS-FFT-INDEX			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-FFT-FETCHED			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
*
01	WS-FCS-AWARD-REF		PIC X(3).
01	WS-FCS-REF-FOUND		PIC X(1).
*
01	WS-NO-OF-EXPIRED-UNITS-1	PIC S9(6) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-NO-OF-EXPIRED-UNITS-2	PIC S9(6) 
*                                                 COMP
                                                  COMP-5
                                                      .
*
01	WS-FIRST-REG-RCFE-ID		PIC S9(9) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-LAST-REG-RCFE-ID		PIC S9(9) 
*                                                 COMP
                                                  COMP-5
                                                      .
01      WS-BATCH-NUMBER-O               PIC 99.
*
*       EXEC SQL  END DECLARE SECTION  END-EXEC.
*       EXEC SQL  INCLUDE SQLCA        END-EXEC.
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
01	WS-CERT-FILE			PIC X(30) VALUE SPACES.
*
01	WS-STDTS-COUNT-TAB.
	03  WS-SCT-DOC-REF		PIC X(5)	OCCURS 500 TIMES.
	03  WS-SCT-AWARD-CODE		PIC X(2)	OCCURS 500 TIMES.
	03  WS-SCT-PROCESSED		PIC S9(5) COMP-3
							OCCURS 500 TIMES.
	03  WS-SCT-PASSED		PIC S9(5) COMP-3
							OCCURS 500 TIMES.
	03  WS-SCT-REJECTED		PIC S9(5) COMP-3
							OCCURS 500 TIMES.
*
01	WS-SCT-INDEX			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-SCT-INDEX-MAX		PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                       VALUE 500.
01	WS-SCT-SUB			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-SCT-UPDATE			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
01	WS-SCT-FOUND			PIC X VALUE 'N'.
*
01	WS-ERRORS.
  03	  WS-ERR-MESSAGE    PIC X(132) VALUE SPACES.
  03  	  WS-ERR-NUM.
	  05  WS-ERR-CODE   PIC 9(6).
	  05  FILLER	    PIC X(74) VALUE SPACES.
  03	  WS-ERR-SEARCH	    PIC X(40) VALUE 'TABLE SEARCH FAILED'.
  03	  WS-ERR-CONNECT    PIC X(40) VALUE 'CONNECT TO ORACLE FAILED'.
  03	  WS-ERR-FETCH	    PIC X(40) VALUE 'FETCH FAILED'.
  03	  WS-ERR-OPEN	    PIC X(40) VALUE 'OPEN CURSOR FAILED'.
  03	  WS-ERR-CLOSE	    PIC X(40) VALUE 'CLOSE CURSOR FAILED'.
  03	  WS-SQLCODE-X	    PIC -Z(5)9.
  03	  WS-SQLERRD-X	    PIC -Z(5)9.
*
01	WS-BITS.
	03  WS-PBODY-SUB		PIC 9.
        03  WS-TORG-SUB                 PIC 9.
        03  WS-CENTRE-SUB		PIC 9.
        03  WS-ANS-REC			PIC X(581).
        03  WS-ANS-LENGTH		PIC 9(4) 
*                                                COMP
                                                 COMP-5
                                                      VALUE 80.
        03  WS-POINTER			PIC 9(4) 
*                                                COMP
                                                 COMP-5
                                                     .
	03  WS-PTR			PIC 999.
	03  WS-OLD-INST-LOCATION	PIC X(6).
	03  WS-FILENAME			PIC X(12).
        03  WS-FORMAT-TYPE		PIC X.
	03  WS-RUN-TYPE			PIC X(11).
	03  WS-END	                PIC X.
		88 END-OF-FETCH		VALUE 'Y'.
	03  WS-TRANSFER-END		PIC X.
		88 END-OF-TRANSFERS 	VALUE 'Y'.
	03  WS-UNITS-END		PIC X.
		88 END-OF-UNITS		VALUE 'Y'.
	03  WS-PBBTECS-END		PIC X.
		88 END-OF-PBBTECS	VALUE 'Y'.
	03  WS-EOF-IND			PIC X.
		88 WS-EOF		VALUE 'Y'.
	03  WS-LINES-LEFT		PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
	03  WS-B-CENTRE-STDTS-PASSED    PIC S9(5) COMP-3 VALUE ZERO.
	03  WS-B-COURSE-STDTS-PASSED    PIC S9(5) COMP-3 VALUE ZERO.
	03  WS-B-TOT-STDTS-PROCESSED    PIC S9(9) COMP-3 VALUE ZERO.
	03  WS-B-TOT-STDTS-PASSED       PIC S9(9) COMP-3 VALUE ZERO.
	03  WS-B-TOT-STDTS-REJECTED     PIC S9(5) COMP-3 VALUE ZERO.
*
	03  WS-B-DRF-STDTS-PROCESSED    PIC S9(9) COMP-3 VALUE ZERO.
	03  WS-B-DRF-STDTS-PASSED       PIC S9(9) COMP-3 VALUE ZERO.
	03  WS-B-DRF-STDTS-REJECTED     PIC S9(5) COMP-3 VALUE ZERO.
	03  WS-B-AWC-STDTS-PROCESSED    PIC S9(5) COMP-3 VALUE ZERO.
	03  WS-B-AWC-STDTS-PASSED       PIC S9(5) COMP-3 VALUE ZERO.
	03  WS-B-AWC-STDTS-REJECTED     PIC S9(5) COMP-3 VALUE ZERO.
*
 	03  WS-TIME-FORMAT.
  	     	05  WS-HOURS            PIC 99.
       		05  FILLER              PIC X VALUE ":".
       		05  WS-MINS             PIC 99.
*
	03  WS-FILESPEC			PIC X(32).
	03  WS-FILESPEC-DELIM		PIC X VALUE ':'.
	03  WS-DIRNAME			PIC X(20).
	03  WS-FILE-DOC-REF.
	    05  WS-FDR-CHAR-123		PIC X(3).
	    05  WS-FDR-CHAR-4		PIC X.
	    05  WS-FDR-CHAR-5		PIC X.
	03  WS-FILE-RUN-TYPE		PIC X.
        03  WS-BATCH-NUMBER		PIC XX.
*
	03  WS-NVQ-UNIT-CERT-EXPIRED	PIC X.
*           
01  WS-OP-DATE.
	03  FILLER			PIC XX	VALUE '19'.
	03  WS-OP-YY			PIC XX.
*
01	WS-SORT-KEY.
	03  WS-OLD-REG-NO.
	  04  WS-OLD-COMB.
	    05  WS-OLD-COURSE-NO.
 	      06  WS-OLD-AWARD-CODE.
		07  WS-OLD-CENTRE.
		  08  WS-OLD-DOC-REF.
		    09  WS-NEW-DOC-REF	PIC X(5).
		  08  WS-NEW-CENTRE	PIC X(6).
		07  WS-NEW-AWARD-CODE	PIC X(2).
	      06  WS-NEW-COURSE		PIC X(8).
 	    05  WS-NEW-COMB		PIC X.
	  04  WS-NEW-REG-NO		PIC X(7).
*
01      WS-REJECT-LINECOUNT    	   PIC S9(4) 
*                                            COMP
                                             COMP-5
                                                 .
01      WS-REJECT-REC-COUNT    	   PIC S9(4) 
*                                            COMP
                                             COMP-5
                                                 .
01	WS-CURRENT-DATE		   PIC X(21).	
01      WS-DATE.
        03     WS-YEAR		   PIC 9999.
        03     WS-MONTH            PIC 99.
        03     WS-DAY              PIC 99.
*
01	WS-MAX-06-LINES		PIC 9	VALUE 4.
01	WS-NUM-06-LINES		PIC 9	VALUE 0.

01	WS-CNT			PIC 99.
01	WS-START		PIC 99.
01	WS-FINISH		PIC 99.
01	WS-COMPLETE		PIC X.
	88  COMPLETE		VALUE 'Y'.
01	WS-CENTRE-ARRAY.
	03 WS-C-ARRAY OCCURS 90 TIMES.
	   05 WS-CENTRE-A		PIC X.
01      WS-REJECT-NO-TABLE.
        03  WS-REJECT-NO     OCCURS 25 TIMES INDEXED BY WS-REJECT-IND.
                05  WS-REJECT-IND-NO       PIC X.
*
01      WS-REJECT-MESSAGE-TABLE.
                05  WS-REJECT-DES-1        PIC X(60)
                    VALUE 'CENTRE NO. NOT ON CENTRE OR OLD_CENTRE TABLE'.
                05  WS-REJECT-DES-2        PIC X(60)
                    VALUE 'COURSE NO. NOT ON APPLICATION_AWARDS TABLE'.
                05  WS-REJECT-DES-3        PIC X(60)       
                    VALUE 'APPLICATION NO. NOT ON APPROVAL_APPLICATION TABLE'.
                05  WS-REJECT-DES-4        PIC X(60)
                    VALUE 'AWARD CODE NOT ON AWARD_CODES TABLE'.
                05  WS-REJECT-DES-5        PIC X(60)
                    VALUE 'COURSE TITLE NOT ON AWARD_TITLES TABLE'.
                05  WS-REJECT-DES-6        PIC X(60)
                    VALUE 'BOARD NOT ON BOARD EXAMINERS TABLE'.
                05  WS-REJECT-DES-7        PIC X(60)
                    VALUE 'COMBINATION NOT ON APPROVAL_COMBINATION TABLE'.
                05  WS-REJECT-DES-8        PIC X(60)
                    VALUE 'COMBINATION NOT ON APPROVAL_UNITS/UNITS TABLE'.
                05  WS-REJECT-DES-9        PIC X(60)
                    VALUE 'TRANSFER CENTRE NOT ON CENTRE/OLD CENTRE TABLES'.
                05  WS-REJECT-DES-10       PIC X(60)
                    VALUE 'TRANSFER UNITS NOT ON APPROVAL_UNITS/UNITS TABLE'.
                05  WS-REJECT-DES-11       PIC X(60)
                    VALUE 'STUDENT UNITS NOT ON STUDENT UNIT TABLE'.
                05  WS-REJECT-DES-12       PIC X(60)
                    VALUE 'STUDENT UNITS NOT ON APPROVAL_UNITS/UNITS TABLE'.
                05  WS-REJECT-DES-13       PIC X(60)
                    VALUE 'SUPPLEMENTARY UNIT NOT ON UNIT TABLE'.
                05  WS-REJECT-DES-14       PIC X(60)
                    VALUE 'TRANSFER STUDENT COURSE AND/OR COMBINATION MISSING'.
                05  WS-REJECT-DES-15       PIC X(60)
                    VALUE 'TRANSFER COMBINATION NOT ON APPROVAL_UNITS/UNITS'.
                05  WS-REJECT-DES-16       PIC X(60)
                    VALUE 'TRANSFER COURSE NOT ON APPLICATION_AWARDS'.
                05  WS-REJECT-DES-17       PIC X(60)
                    VALUE 'TRANSFER APPLICATION NOT ON APPROVAL_APPLICATION'.
                05  WS-REJECT-DES-18       PIC X(60).
                05  WS-REJECT-DES-19       PIC X(60)
                    VALUE 'STUDENTS UNITS HAVE NO GRADES'.
                05  WS-REJECT-DES-20       PIC X(60)
                    VALUE 'NVQ UNIT CERTIFICATION HAS EXPIRED'.
                05  WS-REJECT-DES-21       PIC X(60)
                    VALUE 'INVALID AWARD CODE FOR THIS RUN'.
                05  WS-REJECT-DES-22       PIC X(60)
                    VALUE 'CENTRE NAME NOT FOUND'.
                05  WS-REJECT-DES-23       PIC X(60)
                    VALUE 'AWARD CODE HAS NO DOCUMENT REFERENCE'.
                05  WS-REJECT-DES-24       PIC X(60)
                    VALUE 'NO NEXT CERTIFICATE NUMBER FOR AWARD CODE'.
                05  WS-REJECT-DES-25       PIC X(60) 
                    VALUE 'END OF STP070 REJECT REPORT'.
*
01      WS-REJECT-DES-TABLE REDEFINES WS-REJECT-MESSAGE-TABLE.
        03  WS-REJECT-DES    OCCURS 25 TIMES  INDEXED BY WS-REJECT-DES-IND.
                05  WS-REJECT-DES-LINE     PIC X(60).
*
01      WS-REJECT-INFO.
*
        03  WS-REJECT-TOP-HEADING.
                05  WS-REJECT-DAY          PIC 99.
                05  FILLER                 PIC X VALUE '/'.
                05  WS-REJECT-MONTH        PIC 99.
                05  FILLER                 PIC X VALUE '/'.
                05  WS-REJECT-YEAR         PIC 9999.
                05  FILLER                 PIC X(2) VALUE SPACES.
                05  FILLER                 PIC X(23) VALUE 
                    'STP070 CERTIFICATES - '.
                05  FILLER                 PIC X(17) VALUE 
                    'REJECTED STUDENTS'.
*
        03  WS-REJECT-HYPHEN.
                05  FILLER                 PIC X(130) VALUE ALL '-'.
*
        03  WS-REJECT-HEADING.
                05  FILLER		   PIC X(9)  VALUE 'REG-NO'.
                05  FILLER		   PIC X(8)  VALUE 'CENTRE'.
                05  FILLER                 PIC X(8)  VALUE 'MRG-CTR'.
                05  FILLER		   PIC X(8)  VALUE 'COURSE'.
                05  FILLER		   PIC X(5)  VALUE 'COMB'.
                05  FILLER		   PIC X(10) VALUE 'APPL-NO'.
                05  FILLER		   PIC X(7)  VALUE 'AWARD'.
                05  FILLER		   PIC X(8)  VALUE 'DOC-REF'.
                05  FILLER		   PIC X(7)  VALUE 'UNIT'.
                05  FILLER                 PIC X(60) VALUE 'MESSAGE'.
*
        03  WS-REJECT-LINE.
            04  WS-REJECT-STUDENT.
                05  WS-REJECT-REG-NO	   PIC X(9).
                05  WS-REJECT-CENTRE-NO	   PIC X(8).
                05  WS-REJECT-MERGE-CTR	   PIC X(8).
                05  WS-REJECT-COURSE-NO	   PIC X(8).
                05  WS-REJECT-COMB  	   PIC X(5).
                05  WS-REJECT-APPLICAT-NO  PIC X(8).
                05  FILLER                 PIC XX VALUE SPACES.
                05  WS-REJECT-AWARD-CODE   PIC X(7).
                05  WS-REJECT-DOC-REF      PIC X(8).
                05  WS-REJECT-UNIT-CODE    PIC X(7).
            04  WS-REJECT-MESSAGE       PIC X(60).
*
        03  WS-ABORT-INFO.
                05  FILLER                      PIC X(5) VALUE 'TYPE:'.
                05  WS-ABORT-REC-TYPE           PIC XX.
                05  FILLER                      PIC X(9) VALUE ' DOC-REF:'.
                05  WS-ABORT-DOC-REF            PIC X(5).
                05  FILLER                      PIC X(8) VALUE ' CENTRE:'.
                05  WS-ABORT-CENTRE-NO          PIC X(6).
                05  FILLER                      PIC X(7) VALUE ' AWARD:'.
                05  WS-ABORT-AWARD-CODE         PIC XX.
                05  FILLER                      PIC X(8) VALUE ' COURSE:'.
                05  WS-ABORT-COURSE-NO          PIC X(8).
                05  FILLER                      PIC X(6) VALUE ' COMB:'.
                05  WS-ABORT-COMB               PIC X(1).
                05  FILLER                      PIC X(8) VALUE ' REG-NO:'.
                05  WS-ABORT-REG-NO             PIC X(7).
                05  FILLER                      PIC X(8) VALUE ' APPLIC:'.
                05  WS-ABORT-APPLICAT-NO        PIC X(9).
*
        03  WS-CONTROL-PRINT-LINE.
                05  FILLER                      PIC X(51)
		VALUE 'CONTROL TOTALS FOR STDTS PROCESSED ON DOCUMENT REF '.
                05  WS-CNTRL-DOC-REF            PIC X(5).
                05  FILLER                      PIC X(24) VALUE SPACES.
*
        03  WS-STDTS-PROCESSED.
                05  WS-STDTS-TYPE               PIC X(41).
                05  WS-STDTS-COUNT              PIC 9(8).
                05  FILLER                      PIC X(31) VALUE SPACES.
*
	03  WS-CONTROL-REJECT-LINE1.
	    05  FILLER				PIC X(9)
		VALUE 'DOC REF: '.
	    05  WS-CRL1-DOC-REF			PIC X(5).
	    05  FILLER				PIC X(14)
		VALUE '  AWARD CODE: '.
	    05  WS-CRL1-AWARD-CODE		PIC X(2).
	    05  FILLER				PIC X(25)
		VALUE '  STUDENTS ISSUED AWARDS='.
	    05  WS-CRL1-STDTS-PASSED		PIC 9(8).
	    05  FILLER				PIC X(20)
		VALUE '  STUDENTS REJECTED='.
	    05  WS-CRL1-STDTS-REJECTED		PIC 9(8).
	    05  FILLER				PIC X(21)
		VALUE '  STUDENTS PROCESSED='.
	    05  WS-CRL1-STDTS-PROCESSED		PIC 9(8).
	    05  FILLER				PIC X(10) VALUE SPACES.
*
	03  WS-CONTROL-REJECT-LINE2.
	    05  FILLER				PIC X(9)
		VALUE 'DOC REF: '.
	    05  WS-CRL2-DOC-REF			PIC X(5).
	    05  FILLER				PIC X(16)
		VALUE '  TOTALS:       '.
	    05  FILLER				PIC X(25)
		VALUE '  STUDENTS ISSUED AWARDS='.
	    05  WS-CRL2-STDTS-PASSED		PIC 9(8).
	    05  FILLER				PIC X(20)
		VALUE '  STUDENTS REJECTED='.
	    05  WS-CRL2-STDTS-REJECTED		PIC 9(8).
	    05  FILLER				PIC X(21)
		VALUE '  STUDENTS PROCESSED='.
	    05  WS-CRL2-STDTS-PROCESSED		PIC 9(8).
	    05  FILLER				PIC X(10) VALUE SPACES.
*
01	WS-REPORT-LINES.
*
	03  WS-LINE-1.
		05  PL1-WITH			PIC X(5).
                05  PL1-RESULT-DESC             PIC X(60).
*
	03  WS-LINE-1A.
		05  PL1-BA-LEVEL		PIC X(30).
*
	03  WS-LINE-1B.
		05  PL1-KS			PIC X(30) VALUE 'KEY SKILLS'.
*
	03  WS-LINE-1C.
		05  PL1-KSQ			PIC X(30) 
			VALUE 'THE KEY SKILLS QUALIFICATION'.
*
        03  WS-LINE-1D.
                05  PL1-MULTI-LEVEL-AWARD       PIC X(60).
*
        03  WS-LINE-1E.
                05  PL1-COA	              PIC X(29)
			VALUE 'CERTIFICATE OF ACHIEVEMENT - '.
                05  PL1-COA-LEVEL             PIC X(30).
*
	03  WS-LINE-2.
		05  PL2-COURSE-NAME 	        PIC X(79).
*
	03  WS-LINE-2A.
		05  PL2-KS-LEVEL 	        PIC X(50).
*
	03  WS-LINE-3.
		05  PL3-COURSE-NAME 	        PIC X(75).
*
	03  WS-LINE-4.
                05  PL4-STUDENT-NAME            PIC X(51).
*
	03  WS-LINE-5.
		05  PL5-CENTRE-NAME	        PIC X(45).
*
	03  WS-LINE-6.
		05  PL6-CENTRE-NAME	        PIC X(45).
*
	03  WS-LINE-7.
		05  PL7-AWARD-DATE		PIC X(15).
*
	03  WS-LINE-7A.
		05  PL7A-MEMBERSHIP-MESSAGE-1   PIC X(65)
	        VALUE 
	'The above qualification is recognised by the Chartered Management'.	
*
	03  WS-LINE-7B.
		05  PL7B-MEMBERSHIP-MESSAGE-2   PIC X(65)
		VALUE
	'Institute and gives the holder accelerated access into membership'.
*                
	03  WS-LINE-7C.
		05  PL7C-MEMBERSHIP-MESSAGE-3   PIC X(68)
	        VALUE 
	'The above qualification (DMS) is at NVQ level 5 and is equivalent to'.
*
	03  WS-LINE-7D.
		05  PL7D-MEMBERSHIP-MESSAGE-4   PIC X(68)
		VALUE
	'the BTEC Advanced Professional Diploma at level 7 on the revised NQF'.
*                
	03  WS-LINE-7E.
		05  PL7E-CERT-VALIDITY-MESSAGE-1   PIC X(21)
					VALUE 'Certificate valid for'.
		05  FILLER			   PIC X
					VALUE SPACES.
		05  PL7E-YEARS-VALID		   PIC X
					VALUE SPACES.
		05  FILLER			   PIC X
					VALUE SPACES.
		05  PL7E-CERT-VALIDITY-MESSAGE-2   PIC X(24)
					VALUE 'years from date of issue'.
*                
* EC1359 - change  "NEA" to "PJEA"
*
	03  WS-LINE-7F.
		05  PL7F-NEA-MESSAGE               PIC X(44)
	        VALUE 'Pearson working in partnership with the PJEA'.
*
	03  WS-LINE-7G.
		05  PL7G-WELSH-MESSAGE-1           PIC X(58)
		VALUE
	'Qualifications Wales regulates this qualification where it'.
	03  WS-LINE-7H.
		05  PL7H-WELSH-MESSAGE-2           PIC X(57)
		VALUE
        'is awarded to learners assessed wholly or mainly in Wales'.
*
*EC2796
        03  WS-LINE-7I.
                05  PL7I-TITLE-MESSAGE   PIC X(46)
                VALUE
           'This was assessed in the context of dance arts'.
*
        03  WS-LINE-7J.
                05  PL7J-TITLE-MESSAGE   PIC X(51)
                VALUE
        'This was assessed in the context of production arts'.
*
        03  WS-LINE-7K.
                05  PL7K-TITLE-MESSAGE   PIC X(54)
                VALUE
        'This certificate does not confer a Licence to Practise'. 
*
	03  WS-LINE-8.
		05  PL8-CENTRE-NO 		PIC X(6).
		05  FILLER			PIC X VALUE ':'.
		05  PL8-COURSE-NO 		PIC X(8).
		05  FILLER			PIC X VALUE ':'.
		05  PL8-REG-NO		        PIC X(7).
                05  FILLER                      PIC X VALUE ':'.
		05  PL8-REPRINT-IND		PIC X.
		05  PL8-CERT-NO 		PIC 9(9).
		05  FILLER			PIC X VALUE ':'.
		05  PL8-BIRTH-DATE		PIC X(8).
		05  FILLER			PIC X VALUE ':'.
		05  FILLER                      PIC X(7) VALUE 'ISSUED '.
		05  PL8-ISSUE-DATE		PIC X(11).
		05  PL8-COURSE-QCA-CODE		PIC X(20).
*                05  FILLER			PIC XXX VALUE ' - '.
                05  PL8-REPRINT-WORD            PIC X(13).

*
	03  WS-BTEC-AWARD-LINE.
		05  FILLER			PIC X(28) VALUE SPACES.
		05  WS-BA-LINE			PIC X(80).
		05  FILLER			PIC X(24) VALUE SPACES.	
*
	03  WS-KEY-SKILLS-LINE.
		05  FILLER			PIC X(28) VALUE SPACES.
		05  WS-KS-LINE			PIC X(80).
		05  FILLER			PIC X(24) VALUE SPACES.	
*
        03  WS-COFA-LINE.
                05  FILLER                      PIC X(28) VALUE SPACES.
                05  WS-COA-LINE                 PIC X(80).
                05  FILLER                      PIC X(24) VALUE SPACES. 
*
	03  WS-VALIDITY-WARNING.
		05  VALIDITY-WARNING-1		PIC X(63)
		VALUE
	'***** WARNING - Not enough space to print certificate validity'.
		05  VALIDITY-WARNING-2		PIC X(29) 
		VALUE	'statement for student reg no '.
		05  VALIDITY-REG-NO		PIC X(7).
*                
*
* JOB 4776
*
01  WS-TITLE-COUNTER			PIC S9(4) 
*                                                 COMP
                                                  COMP-5
                                                      .
*
01	WS-TITLES-TAB.
	03  WS-TITLES-TABLE OCCURS 1 TO 650 TIMES DEPENDING ON WS-TITLE-COUNTER
					INDEXED BY WS-TITLE-IND.
		05  WS-TITLE-CODE		PIC X(5).
		05  WS-TITLE1			PIC X(79).
		05  WS-TITLE2			PIC X(75).
*
01	WS-RESULTS-TABLE.
	03  WS-RESULTS  OCCURS 20 ASCENDING KEY IS WS-RESULT-CODE
		                        INDEXED BY WS-RES-IND.
		05  WS-RESULT-CODE		PIC X.
		05  WS-RESULT-DESC		PIC X(15).
*
01      WS-BTEC-AWARDS-TABLE.
          05 WS-TAB-BTEC-AWARDS OCCURS 100 TIMES INDEXED BY WS-BTEC-IND.
             10 WS-TAB-BTEC-AWARD-CODE  PIC X(2).
	     10 WS-TAB-BTEC-AWARD-TITLE PIC S9(6) COMP-3.

01	WS-STORE-COMBINATION.
        03  WS-STORE-COURSE-NO                   PIC X(8).
        03  WS-STORE-APPLICAT-NO                 PIC S9(9) COMP-3.
        03  WS-STORE-AWARD-CODE                  PIC X(2).
        03  WS-STORE-COMB                        PIC X.
*
01  WS-LAST-GRADE				PIC X.
	88  WS-FAILED				VALUE 'F', 'R'.
01  WS-IP-AWARD-DATE                            PIC X(11).
01  WS-IP-RUN-TYPE				PIC X(1).
*
01  WS-CERT-MSG					PIC X(70).
01  WS-CERT-MSG-SURF				PIC X(70).
01  WS-CERT-MSG-AB				PIC X(70)
    VALUE 'THIS DOCUMENT CONSISTS OF MORE THAN ONE PAGE'.
01  WS-CERT-MSG-A				PIC X(70)
    VALUE 'ASSESSED IN A LANGUAGE OTHER THAN ENGLISH'.
01  WS-CERT-MSG-B				PIC X(70)
    VALUE 'PARTIALLY ASSESSED IN A LANGUAGE OTHER THAN ENGLISH'.
01  WS-CERT-MSG-C				PIC X(70)
    VALUE 'MEMBER OF THE STAFFORDSHIRE UNIVERSITY REGIONAL FEDERATION'.
01  WS-CERT-MSG-D				PIC X(70)
    VALUE '(Assessed in Dutch)'.
01  WS-CERT-MSG-F				PIC X(70)
    VALUE '(Assessed in French)'.
01  WS-CERT-MSG-G				PIC X(70)
    VALUE '(Assessed in Greek)'.
01  WS-CERT-MSG-I				PIC X(70)
    VALUE '(Assessed in Italian)'.
01  WS-CERT-MSG-L				PIC X(70)
    VALUE '(Assessed in Latvian & Russian)'.
01  WS-CERT-MSG-M				PIC X(70)
    VALUE '(Assessed in Mandarin Chinese)'.
01  WS-CERT-MSG-P				PIC X(70)
    VALUE '(Assessed in Portuguese)'.
01  WS-CERT-MSG-R				PIC X(70)
    VALUE '(Assessed in Arabic)'.
01  WS-CERT-MSG-S				PIC X(70)
    VALUE '(Assessed in Spanish)'.
01  WS-CERT-MSG-T				PIC X(70)
    VALUE '(Assessed in Turkish)'.
01  WS-CERT-MSG-X				PIC X(70)
    VALUE '(Assessed in Catalan)'.
*
01  WS-BANNER.
    03  WS-BAN-LINE-1.
        05  FILLER			     PIC X(15) VALUE 
            'Time/date: '.
	05  WS-BAN-TIME			     PIC X(17).
    03  WS-BAN-LINE-2.
        05  FILLER			     PIC X(15) VALUE 
            'Run type: '.
	05  WS-BAN-TYPE			     PIC X(11).
    03  WS-BAN-LINE-3.
        05  FILLER			     PIC X(15) VALUE 
            'Description: '.
	05  WS-BAN-DESCRIPTION		     PIC X(65).
*
01  WS-SUCCESS-LINE.
    03  WS-SUCCESS-LINE-1.
        05  FILLER                           PIC X(01) VALUE "*".
        05  FILLER                           PIC X(10) VALUE SPACES.
        05  WS-SUCC-NAME                     PIC X(10) VALUE 'STP070'.
        05  FILLER                           PIC X(22) VALUE
            "COMPLETED SUCCESSFULLY".
        05  FILLER                           PIC X(36) VALUE SPACES.
        05  FILLER                           PIC X(01) VALUE "*".
*
01  WS-ABORT-LINES.
*
    03  WS-ABORT-LINE-1.
        05  FILLER                           PIC X(80) VALUE ALL "*".
*
    03  WS-ABORT-LINE-2.
        05  FILLER                           PIC X(01) VALUE     "*".
        05  FILLER                           PIC X(78) VALUE SPACES.
        05  FILLER                           PIC X(01) VALUE     "*".
*
    03  WS-ABORT-LINE-3.
        05  FILLER                           PIC X(01) VALUE     "*".
        05  FILLER                           PIC X(30) VALUE SPACES.
        05  WS-ABORT-NAME                    PIC X(10) VALUE 'STP070'.
        05  FILLER                           PIC X(07) VALUE "ABORTED".
        05  FILLER                           PIC X(31) VALUE SPACES.
        05  FILLER                           PIC X(01) VALUE     "*".
*
    03  WS-ABORT-LINE-4. 
        05  FILLER                           PIC X(01) VALUE     "*".
        05  FILLER                           PIC X(26) VALUE SPACES.
        05  FILLER                           PIC X(26) VALUE 
             "PLEASE CONTACT DEVELOPMENT".
        05  FILLER                           PIC X(26) VALUE SPACES.
        05  FILLER                           PIC X(01) VALUE     "*".
*
    03  WS-ABORT-LINE-5.         
        05  FILLER                           PIC X(01) VALUE     "*".
        05  FILLER                           PIC X(05) VALUE SPACES.
        05  WS-SQL-ERR-MESS                  PIC X(75).
        05  FILLER                           PIC X(01) VALUE     "*".
*
01  WS-ABORT                                 PIC 9(9) 
*                                                     COMP
                                                      COMP-5
                                                          .
01  WS-ABORT-VAL                             PIC 9(9) 
*                                                     COMP
                                                      COMP-5
                                                           VALUE 4.
*
01  WS-DATE-DATA.
*
    03  WS-MONTH-DATA                        PIC X(36) VALUE
          'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'.
    03  WS-MONTH-NAMES REDEFINES WS-MONTH-DATA.
        05  WS-MONTH-NAME                    PIC X(03) OCCURS 12.
    03  WS-ORAC-DATE.
        05  WS-ORAC-DAY                       PIC X(02).
        05  FILLER                            PIC X VALUE "-".
        05  WS-ORAC-MONTH                     PIC X(03).
        05  FILLER                            PIC X VALUE "-".
        05  WS-ORAC-YEAR                      PIC X(04).
*
01 WS-TIME-DATA.
*
   03  WS-INTEGER                             PIC 9(09).
   03  WS-QUOTIENT                            PIC 9(09).
*
   03  WS-ITEM-CODE                           PIC S9(9) 
*                                                       COMP
                                                        COMP-5
       VALUE EXTERNAL                        JPI$_CPUTIM.
* 
   03  WS-START-TIME                         PIC 9(9) 
*                                                     COMP
                                                      COMP-5
                                                          .
   03  WS-OUT-VALUE                          PIC 9(9) 
*                                                     COMP
                                                      COMP-5
                                                          .
   03  WS-DRF-START-TIME                     PIC 9(9) 
*                                                     COMP
                                                      COMP-5
                                                          .
   03  WS-DRF-END-TIME                       PIC 9(9) 
*                                                     COMP
                                                      COMP-5
                                                          .
*
01 WS-ACCEPT-DATA.
   03  WS-ACCEPT-EXTERNAL-YN	        PIC X.
*
01 WS-BLANK-PAGE-MESSAGE 		PIC X(51) VALUE 
      "***** END OF CENTRE ********** END OF CENTRE *****".
*
01 WS-BLANK-PAGE-MESSAGE2		PIC X(56) VALUE
      "***** END OF PROGRAMME ********** END OF PROGRAMME *****".
*
/
PROCEDURE DIVISION.
*
A-CONTROL SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE OVERALL LOGIC FLOW.                             *
********************************************************************************
A-START.
	PERFORM B-INITIALISE.
	SORT SORT-FILE ON ASCENDING KEY SORT-KEY
		INPUT  PROCEDURE IS C-INPUT-PROCEDURE
		OUTPUT PROCEDURE IS D-OUTPUT-PROCEDURE.
*
*	RSH 28/11/2007 (LQ35232): Process the fee records for the
*	unsuccessful reprint requests (reprint only).
*
	IF	WS-IP-RUN-TYPE = 'S'
	THEN
		PERFORM ED-UPDATE-FEE-STATUS

		PERFORM EB-DELETE-REPRINT-AWARD-DATA
	END-IF.
*
	PERFORM F-TERMINATE.
	STOP RUN.
A-EXIT.
	EXIT.
*
/
B-INITIALISE SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE DECLARING/OPENING OF CERTAIN CURSORS,   	       *
*    OPENING THE REJECTS AND LABELS FILES, CONNECTING TO ORACLE AND            *
*    LOADING THE REFERENCE DATA TABLES.                                        *
********************************************************************************
B-START.
	PERFORM BA-DECLARE-CURSORS.
 	PERFORM BB-OPEN-OUTPUT.
	PERFORM BC-CONNECT.
        IF WS-IP-RUN-TYPE EQUAL TO 'A'
           PERFORM  BZ-SET-GLOBALS
        END-IF.
	PERFORM BCA-INSERT-RUN-DETAILS.
	PERFORM BD-OPEN-CURSORS.
*
	MOVE SPACES TO WS-RESULTS-TABLE.
	MOVE 'N' TO WS-END.
	PERFORM BF-LOAD-RESULTS VARYING WS-RES-IND FROM 1 BY 1
		UNTIL END-OF-FETCH
		OR WS-RES-IND > 20.
*
	PERFORM BJ-LOAD-PBBTECS.
*
	MOVE ZERO TO WS-TITLE-COUNTER.
*
	PERFORM BG-CLOSE-CURSOR2.
*
B-EXIT.
	EXIT.
*
/
BA-DECLARE-CURSORS SECTION.
********************************************************************************
*    THIS SECTION DECLARES THE CURSORS: CURSOR_1, CURSOR_2, CURSOR_7,          *
*    CURSOR_7A AND PBBTECS_CUR.                                                *
********************************************************************************
BA-START.

*  union to speed up select slightly
*
*       EXEC SQL  DECLARE CURSOR_1 CURSOR FOR
*       		SELECT 	ST_REG_NO,
*				NVL(ST_CERT_NAME,ST_FORENAMES||' '||ST_SURNAME),
*       			ST_CENTRE_ID,
*       			ST_COURSE_ID,
*       			ST_COMB_ID,
*                               TO_NUMBER(ST_CERT_NO),
*       			ST_AWARD_CLAIM,
*       	                RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,
*                                     NVL(TO_DATE(:WS-CERT-DATE,'DD-MON-YYYY'),
*       				SYSDATE)),'MONTH'),' ')
*                                     ||' '||
*       	                      TO_CHAR(NVL(ST_AWARD_PRINTED,
*                                     NVL(TO_DATE(:WS-CERT-DATE,'DD-MON-YYYY')
*       				,SYSDATE)),'YYYY'),
*       	                TO_CHAR(NVL(ST_AWARD_PRINTED,
*                                  NVL(TO_DATE(:WS-CERT-DATE,'DD-MON-YYYY')
*       				,SYSDATE)),'DD-MON-YYYY'),
*       			ST_AWARD_ELIG,
*       			ST_REG_TYPE,
*                               NVL(ST_BNM_GRADE,ST_OVER_GRADE),
*                               ST_FALLBACK,
*       			to_char(ST_BIRTH_DATE,
*       			'DD:MM:YY'),
*       			ST_INST_LOCATION,
*                               ST_COA_SYLLABUS_CODE,
*                               ST_COA_LEVEL_ACHIEVED,    
*                               DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,
*       			ECLE_WELSH_CODE, ECLE_ENGLISH_CODE ),
*       			NVL(ST_RECON_IND,'N'),
*       			'N',
*                               NVL(TO_CHAR(ST_AWARD_ISSUE,'DD-MON-YYYY'),
*                                          :WS-RUN-DATE),
*                               ST_SCHEME_REG_NO,
*                               AA_BTEC_TITLE,
*       			AW_AWARD_CODE,
*       			ST_NOPS_REQD,
*       			ST_NVQ_ROA_CODE,
*       			ST_GNVQ_CUC_CODE,
*       			ST_CRED_TRANS_CODE,
*       			SUBSTR(ST_ABS_LEVEL_ACHIEVED,2,1),
*       			ST_ABS_LEVEL_ACHIEVED,
*       			AW_CERT_FLAG,
*                               ST_DSA_SRU_ID,
*       			'N',
*                               CLTE_TEXT
*       	     	FROM	BTEC.certificate_language_text,
*                               BTEC.ENTRY_COA_LEVELS,
*       			AWARD_CODES,
*       			BTEC.APPROVAL_APPLICATION,
*       			BTEC.APPROVAL_AWARDS,
*       			BTEC.STUDENTS
*       		WHERE   PK_BNM.FN_10_WEEK_BLOCK(ST_REG_NO) = 'N'
*       	        AND     PK_BNM.FN_HOLD_CERTIFICATION(ST_REG_NO) = 'N'
*       		AND	AC_CODE          = AW_AWARD_CODE||''
*       		AND     AC_EXTERNAL_CERT_NO IS NULL
*       		AND	AW_COURSE_NUMBER = ST_COURSE_ID
*       		AND     AW_APPLICAT_NO   = AA_APPLICAT_NO
*                       AND     ECLE_AT_CODE(+)  = ST_COA_SYLLABUS_CODE
*                       AND     ECLE_LEVEL(+)    = ST_COA_LEVEL_ACHIEVED
*                       AND     CLTE_AW_CERT_FLAG(+) = AW_CERT_FLAG
*       		AND 	ST_COURSE_ID != 'KSQ00'
*                       AND   	ST_REG_TYPE  != 'I'
*                       AND     ST_AWARD_ELIG = 'Y'
*                       AND     ST_CERT_NO IS NULL
*       		AND	NVL(ST_DELETE,'N') <> 'Y'
*                       --Code added for EC4363 TA Embargo 2020 Starts--
*                       AND     ((AC_CODE = 'Y6'
*                       AND     FN_CHECK_TA_OVERGRADE_VISIBLE (ST_REG_NO) = 'Y')
*                       OR      AC_CODE <> 'Y6')
*                       --Code added for EC4363 TA Embargo 2020 Ends--
*       		--EC4940 Summer 2021 Results Delivery Certificate Embargo for B1 Start
*       		AND NOT EXISTS
*       		   (SELECT 1
*       			  FROM BTEC.TEMP_EC4940_EXCLUDE_RA_AC_AT,
*       				   BTEC.TEMP_EC4940_SUMMER_PARAM_VALUE
*       			 WHERE TEMP_AC_CODE       = AW_AWARD_CODE
*       			   AND TEMP_AT_NUMBER     = AA_BTEC_TITLE
*       			   AND TEMP_QUAL_CATEGORY = 'B1'
*       		   AND PARAM_NAME         = 'B1 CERTIFICATION PRINT'
*       		   AND SYSDATE < TO_DATE(PARAM_VALUE,'DD-MON-YYYY'))                            
*       		--EC4940 Summer 2021 Results Delivery Certificate Embargo for B1 Ends 
*                       AND    :WS-INPUT-CENTRE = '++++++'
*       		UNION
*       		SELECT 	ST_REG_NO,
*				NVL(ST_CERT_NAME,ST_FORENAMES||' '||ST_SURNAME),
*       			ST_CENTRE_ID,
*       			ST_COURSE_ID,
*       			ST_COMB_ID,
*                               TO_NUMBER(ST_CERT_NO),
*       			ST_AWARD_CLAIM,
*       	                RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,
*                                     NVL(TO_DATE(:WS-CERT-DATE,'DD-MON-YYYY')
*       				,SYSDATE)),'MONTH'),' ')
*                                     ||' '||
*       	                      TO_CHAR(NVL(ST_AWARD_PRINTED,
*                                     NVL(TO_DATE(:WS-CERT-DATE,'DD-MON-YYYY')
*       				,SYSDATE)),'YYYY'),
*       	                TO_CHAR(NVL(ST_AWARD_PRINTED,
*                                  NVL(TO_DATE(:WS-CERT-DATE,'DD-MON-YYYY')
*       				,SYSDATE)),'DD-MON-YYYY'),
*       			ST_AWARD_ELIG,
*       			ST_REG_TYPE,
*                               NVL(ST_BNM_GRADE,ST_OVER_GRADE),
*                               ST_FALLBACK,
*       			to_char(ST_BIRTH_DATE,
*       				'DD:MM:YY'),
*       			ST_INST_LOCATION,
*                               ST_COA_SYLLABUS_CODE,
*                               ST_COA_LEVEL_ACHIEVED,   
*                               DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,
*       			ECLE_WELSH_CODE, ECLE_ENGLISH_CODE ),
*       			NVL(ST_RECON_IND,'N'),
*       			'N',
*       		        NVL(TO_CHAR(ST_AWARD_ISSUE,'DD-MON-YYYY'),
*       				    :WS-RUN-DATE),
*                               ST_SCHEME_REG_NO,
*       			AA_BTEC_TITLE,
*       			AW_AWARD_CODE,
*       			ST_NOPS_REQD,
*       			ST_NVQ_ROA_CODE,
*       			ST_GNVQ_CUC_CODE,
*       			ST_CRED_TRANS_CODE,
*       			SUBSTR(ST_ABS_LEVEL_ACHIEVED,2,1),
*       			ST_ABS_LEVEL_ACHIEVED,
*       			AW_CERT_FLAG,
*                               ST_DSA_SRU_ID,
*       			'N',
*                               CLTE_TEXT
*       		FROM	BTEC.certificate_language_text,
*                               BTEC.ENTRY_COA_LEVELS,
*       			BTEC.APPROVAL_APPLICATION,
*       			BTEC.STUDENTS,
*       			AWARD_CODES,
*       			BTEC.APPROVAL_AWARDS,
*                               AWARDS_RUN_CENTRES
*       		WHERE   PK_BNM.FN_10_WEEK_BLOCK(ST_REG_NO) = 'N'
*       	 	AND     PK_BNM.FN_HOLD_CERTIFICATION(ST_REG_NO) = 'N'
*       		AND	AC_CODE          = AW_AWARD_CODE||''
*       		AND     AC_EXTERNAL_CERT_NO IS NULL
*       		AND	AW_COURSE_NUMBER = ST_COURSE_ID
*       		AND	AW_APPLICAT_NO   = AA_APPLICAT_NO
*                       AND     ECLE_AT_CODE(+)  = ST_COA_SYLLABUS_CODE
*                       AND     ECLE_LEVEL(+)    = ST_COA_LEVEL_ACHIEVED
*                       AND     CLTE_AW_CERT_FLAG(+) = AW_CERT_FLAG
*       		AND 	ST_COURSE_ID != 'KSQ00'
*                       AND   	ST_REG_TYPE  != 'I'
*                       AND     ST_AWARD_ELIG = 'Y'
*                       AND     ST_CERT_NO IS NULL
*                       AND     ST_CENTRE_ID    = ARUN_CENTRE_ID   
*       		AND	NVL(ST_DELETE,'N') <> 'Y'
*                       --Code added for EC4363 TA Embargo 2020 Starts--
*                       AND     ((AC_CODE = 'Y6'
*                       AND     FN_CHECK_TA_OVERGRADE_VISIBLE (ST_REG_NO) = 'Y')
*                       OR      AC_CODE <> 'Y6')
*                       --Code added for EC4363 TA Embargo 2020 Ends--
*                       --EC4940 Summer 2021 Results Delivery Certificate Embargo for B1 Start
*                       AND NOT EXISTS
*                          (SELECT 1
*                             FROM BTEC.TEMP_EC4940_EXCLUDE_RA_AC_AT,
*                                  BTEC.TEMP_EC4940_SUMMER_PARAM_VALUE
*                            WHERE TEMP_AC_CODE       = AW_AWARD_CODE
*                              AND TEMP_AT_NUMBER     = AA_BTEC_TITLE
*                              AND TEMP_QUAL_CATEGORY = 'B1'
*                          AND PARAM_NAME         = 'B1 CERTIFICATION PRINT'
*                          AND SYSDATE < TO_DATE(PARAM_VALUE,'DD-MON-YYYY'))                            
*                       --EC4940 Summer 2021 Results Delivery Certificate Embargo for B1 Ends 
*                       AND    :WS-INPUT-CENTRE = 'TABLE'
*       END-EXEC.
*
*       EXEC SQL DECLARE CURSOR_2 CURSOR FOR
*       		SELECT 	RE_CODE,
*       			RE_DESCRIPTION
*       		FROM	BTEC.RESULTS
*                       ORDER BY RE_CODE
*       END-EXEC.
*
*	RSH 28/11/2007 (LQ35232): Fetch the first and last fee record id.
*
*       EXEC SQL

*         DECLARE CURSOR_7 CURSOR FOR
*           SELECT ST_REG_NO,
*       	   NVL(ST_CERT_NAME,ST_FORENAMES||' '||ST_SURNAME),
*       	   ST_CENTRE_ID,
*       	   ST_COURSE_ID,
*       	   ST_COMB_ID,
*                  TO_NUMBER(DECODE(SUBSTR(ST_CERT_NO,1,1)
*       		     ,'T',SUBSTR(ST_CERT_NO,2)
*       		     ,'S',SUBSTR(ST_CERT_NO,2)
*       		     ,'F',SUBSTR(ST_CERT_NO,2)
*       		     ,'R',SUBSTR(ST_CERT_NO,2)
*       		     ,'H',SUBSTR(ST_CERT_NO,4)
*       		     ,'A',SUBSTR(ST_CERT_NO,5)
*       		     ,'D',SUBSTR(ST_CERT_NO,5)
*       		     ,ST_CERT_NO)),
*       	   ST_AWARD_CLAIM,
*       	   RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,
*       			     NVL(ARC_AWARD_DATE,SYSDATE)),'MONTH')
*                                    ,' ')||' '||
*       	                     TO_CHAR(NVL(ST_AWARD_PRINTED,
*       			     NVL(ARC_AWARD_DATE,SYSDATE)),'YYYY'),
*       	   TO_CHAR(NVL(ST_AWARD_PRINTED,
*       		   NVL(ARC_AWARD_DATE,SYSDATE)),
*                          'DD-MON-YYYY'),
*       	   ST_AWARD_ELIG,
*       	   ST_REG_TYPE,
*                  NVL(ST_BNM_GRADE,ST_OVER_GRADE),
*                  ST_FALLBACK,
*       	   TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY'),
*       	   ST_INST_LOCATION,
*                  ST_COA_SYLLABUS_CODE,
*                  ST_COA_LEVEL_ACHIEVED,
*                  DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,
*       			ECLE_WELSH_CODE, ECLE_ENGLISH_CODE ),
*       	   NVL(ST_RECON_IND,'N'),
*       	   'N',
*       	   NVL(TO_CHAR(ST_AWARD_ISSUE,'DD-MON-YYYY'),:WS-RUN-DATE),
*       	   ST_NOPS_REQD,
*       	   ST_NVQ_ROA_CODE,
*       	   ST_GNVQ_CUC_CODE,
*       	   ST_CRED_TRANS_CODE,
*       	   SUBSTR(ST_ABS_LEVEL_ACHIEVED,2,1),
*       	   ST_ABS_LEVEL_ACHIEVED,
*       	   AW_CERT_FLAG,
*       	   AA_BTEC_TITLE,
*       	   AW_AWARD_CODE,
*                  DECODE(ARC_REPRINT_WORDING,'Y','  REPLACEMENT',' '),
*       	   NVL(ARC_FIRST_REG_RCFE_ID,0),
*       	   NVL(ARC_LAST_REG_RCFE_ID,0),
*                  ST_DSA_SRU_ID,
*       	   DECODE(SIGN(TO_DATE('01/01/2014','DD/MM/YYYY')-
*       	    NVL(ST_AWARD_ISSUE,SYSDATE)),1,'Y','N'),
*                  CLTE_TEXT
*           FROM   BTEC.ENTRY_COA_LEVELS,
*       	   BTEC.APPROVAL_APPLICATION,
*       	   BTEC.STUDENTS,
*       	   BTEC.APPROVAL_AWARDS,
*                  BTEC.ALL_REPRINT_CRITERIA,
*                  BTEC.CERTIFICATE_LANGUAGE_TEXT
*           WHERE  ARC_REPRINT_TYPE	       = 'B'
*           AND    ST_REG_NO	      BETWEEN  ARC_FIRST_REG_NO 
*                                     AND      NVL(ARC_LAST_REG_NO,
*       					   ARC_FIRST_REG_NO)
*           AND	   AW_COURSE_NUMBER	       = ST_COURSE_ID
*           AND	   AW_APPLICAT_NO	       = AA_APPLICAT_NO
*           AND    ECLE_AT_CODE(+)	       = ST_COA_SYLLABUS_CODE
*           AND    ECLE_LEVEL(+)	       = ST_COA_LEVEL_ACHIEVED
*           AND    CLTE_AW_CERT_FLAG(+)        = AW_CERT_FLAG
*           AND    ST_COURSE_ID	       	      != 'KSQ00'
*           AND    NVL(ST_UNIVERSITY_IND,'N') != 'Y'
*           AND    ST_COURSE_ID        	  IS NOT NULL
*           AND    ST_CERT_NO	       	  IS NOT NULL
*           AND    ST_AWARD_ELIG||''	       = 'Y'
*           AND	   ARC_DB		       = 'P'
*           UNION
*           SELECT ST_REG_NO,
*       	   NVL(ST_CERT_NAME,ST_FORENAMES||' '||ST_SURNAME),
*       	   ST_CENTRE_ID,
*       	   ST_COURSE_ID,
*       	   ST_COMB_ID,
*                  TO_NUMBER(DECODE(SUBSTR(ST_CERT_NO,1,1)
*       		     ,'T',SUBSTR(ST_CERT_NO,2)
*       		     ,'S',SUBSTR(ST_CERT_NO,2)
*       		     ,'F',SUBSTR(ST_CERT_NO,2)
*       		     ,'R',SUBSTR(ST_CERT_NO,2)
*       		     ,'H',SUBSTR(ST_CERT_NO,4)
*       		     ,'A',SUBSTR(ST_CERT_NO,5)
*       		     ,'D',SUBSTR(ST_CERT_NO,5)
*       		     ,ST_CERT_NO)),
*       	   ST_AWARD_CLAIM,
*       	   RTRIM(TO_CHAR(NVL(ST_AWARD_PRINTED,
*       			     NVL(ARC_AWARD_DATE,SYSDATE)),'MONTH')
*                                    ,' ')||' '||
*       	                     TO_CHAR(NVL(ST_AWARD_PRINTED,
*       			     NVL(ARC_AWARD_DATE,SYSDATE)),'YYYY'),
*       	   TO_CHAR(NVL(ST_AWARD_PRINTED,
*       		   NVL(ARC_AWARD_DATE,SYSDATE)),
*                          'DD-MON-YYYY'),
*       	   ST_AWARD_ELIG,
*       	   ST_REG_TYPE,
*                  NVL(ST_BNM_GRADE,ST_OVER_GRADE),
*                  ST_FALLBACK,
*       	   TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY'),
*       	   ST_INST_LOCATION,
*                  ST_COA_SYLLABUS_CODE,
*                  ST_COA_LEVEL_ACHIEVED,
*                  DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,
*       			ECLE_WELSH_CODE, ECLE_ENGLISH_CODE ),
*       	   NVL(ST_RECON_IND,'N'),
*       	   'N',
*       	   NVL(TO_CHAR(ST_AWARD_ISSUE,'DD-MON-YYYY'),:WS-RUN-DATE),
*       	   ST_NOPS_REQD,
*       	   ST_NVQ_ROA_CODE,
*       	   ST_GNVQ_CUC_CODE,
*       	   ST_CRED_TRANS_CODE,
*       	   SUBSTR(ST_ABS_LEVEL_ACHIEVED,2,1),
*       	   ST_ABS_LEVEL_ACHIEVED,
*       	   AW_CERT_FLAG,
*       	   AA_BTEC_TITLE,
*       	   AW_AWARD_CODE,
*                  DECODE(ARC_REPRINT_WORDING,'Y','  REPLACEMENT',' '),
*       	   NVL(ARC_FIRST_REG_RCFE_ID,0),
*       	   NVL(ARC_LAST_REG_RCFE_ID,0),
*                  ST_DSA_SRU_ID,
*       	   DECODE(SIGN(TO_DATE('01/01/2014','DD/MM/YYYY')-
*       	    NVL(ST_AWARD_ISSUE,SYSDATE)),1,'Y','N'),
*                  CLTE_TEXT
*           FROM   BTEC.ENTRY_COA_LEVELS,
*       	   BTEC.APPROVAL_APPLICATION,
*       	   BTEC.STUDENTS@ARCA,
*       	   BTEC.APPROVAL_AWARDS,
*                  BTEC.ALL_REPRINT_CRITERIA,
*                  BTEC.CERTIFICATE_LANGUAGE_TEXT
*           WHERE  ARC_REPRINT_TYPE	       = 'B'
*           AND    ST_REG_NO	      BETWEEN  ARC_FIRST_REG_NO 
*                                     AND      NVL(ARC_LAST_REG_NO,
*       					   ARC_FIRST_REG_NO)
*           AND	   AW_COURSE_NUMBER	       = ST_COURSE_ID
*           AND	   AW_APPLICAT_NO	       = AA_APPLICAT_NO
*           AND    ECLE_AT_CODE(+)	       = ST_COA_SYLLABUS_CODE
*           AND    ECLE_LEVEL(+)	       = ST_COA_LEVEL_ACHIEVED
*           AND    CLTE_AW_CERT_FLAG(+)        = AW_CERT_FLAG
*           AND    ST_COURSE_ID	       	      != 'KSQ00'
*           AND    NVL(ST_UNIVERSITY_IND,'N') != 'Y'
*           AND    ST_COURSE_ID        	  IS NOT NULL
*           AND    ST_CERT_NO	       	  IS NOT NULL
*           AND    ST_AWARD_ELIG||''	       = 'Y'
*           AND	   ARC_DB		       = 'A'
*       END-EXEC.
*
*	RSH 28/11/2007 (LQ35232): Fetch the first and last fee record id.
*
*       EXEC SQL  DECLARE CURSOR_7A CURSOR FOR
*	    SELECT ST_REG_NO,
*		   NVL(ST_CERT_NAME,ST_FORENAMES||' '||ST_SURNAME),
*		   ST_CENTRE_ID,
*		   ST_GNVQ_REGISTERED_ID,
*		   ST_COMB_ID,
*                  TO_NUMBER(ST_GNVQ_CERTIFICATE_NO),
*		   ST_GNVQ_CLAIM_ID,
*		   RTRIM(TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,
*				 NVL(ARC_AWARD_DATE,SYSDATE)),'MONTH')
*				 ,' ')||' '||
*		         TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,
*                                NVL(ARC_AWARD_DATE,SYSDATE)),'YYYY'),
*		   TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,
*                          NVL(ARC_AWARD_DATE,SYSDATE)),
*			   'DD-MON-YYYY'),
*		   ST_GNVQ_ELIGIBILITY_CODE,
*		   ST_REG_TYPE,
*                  NULL,
*                  ST_FALLBACK,
*		   TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY'),                     
*		   ST_INST_LOCATION,
*                  ST_COA_SYLLABUS_CODE,
*                  ST_COA_LEVEL_ACHIEVED,    
*                  DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,
*       			ECLE_WELSH_CODE, ECLE_ENGLISH_CODE ),
*		   NVL(ST_RECON_IND,'N'),
*		   'Y',          
*       	   NVL(TO_CHAR(ST_GNVQ_CERTIFICATE_PRINT_DATE,
*                              'DD-MON-YYYY'),:WS-RUN-DATE),
*                  DECODE(ARC_REPRINT_WORDING,'Y','  REPLACEMENT',' '),
*       	   NVL(ARC_FIRST_REG_RCFE_ID,0),
*       	   NVL(ARC_LAST_REG_RCFE_ID,0),
*                  ST_DSA_SRU_ID,
*       	   'N'
*	    FROM   BTEC.ENTRY_COA_LEVELS,
*		   BTEC.STUDENTS,
*		   BTEC.GNVQS,
*		   BTEC.ALL_REPRINT_CRITERIA
*	    WHERE  ARC_REPRINT_TYPE		= 'B'
*	    AND	   ST_CERT_NO 		   IS NOT NULL
*	    AND	   ST_REG_NO 
*       		BETWEEN ARC_FIRST_REG_NO
*	    		AND	NVL(ARC_LAST_REG_NO,ARC_FIRST_REG_NO)
*           AND	   ARC_AWARD_CODE 		= '27'
*	    AND	   ST_GNVQ_REGISTERED_ID 	= GNVQ_ID
*	    AND	   ST_RECON_IND 	       IS NULL
*	    AND	   TO_DATE(TO_CHAR(ST_GNVQ_CERTIFICATE_DISP_DATE,
*                                  'DD-MON-YYYY'),'DD-MON-YYYY')
*               	BETWEEN TO_DATE('01-JUL-1999','DD-MON-YYYY') 
*               	AND 	TO_DATE('31-AUG-1999','DD-MON-YYYY')
*	    AND	   GNVQ_PILOT_IND 		= 'Y'
*	    AND	   GNVQ_SINGLEAW_IND 	       IS NULL
*           AND    ECLE_AT_CODE(+)  		= ST_COA_SYLLABUS_CODE
*           AND    ECLE_LEVEL(+)    		= ST_COA_LEVEL_ACHIEVED
*	    AND    ST_GNVQ_REGISTERED_ID   IS NOT NULL
*           AND    ST_GNVQ_CERTIFICATE_NO  IS NOT NULL
*           AND    ST_GNVQ_ELIGIBILITY_CODE||'' = 'Y'
*           AND    ST_COURSE_ID		       != 'KSQ00'
*	    AND	   NVL(ST_UNIVERSITY_IND,'N')  != 'Y'
*           AND	   ARC_DB			= 'P'	
*           UNION
*	    SELECT ST_REG_NO,
*		   NVL(ST_CERT_NAME,ST_FORENAMES||' '||ST_SURNAME),
*		   ST_CENTRE_ID,
*		   ST_GNVQ_REGISTERED_ID,
*		   ST_COMB_ID,
*                  TO_NUMBER(ST_GNVQ_CERTIFICATE_NO),
*		   ST_GNVQ_CLAIM_ID,
*		   RTRIM(TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,
*				 NVL(ARC_AWARD_DATE,SYSDATE)),'MONTH')
*				 ,' ')||' '||
*		         TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,
*                                NVL(ARC_AWARD_DATE,SYSDATE)),'YYYY'),
*		   TO_CHAR(NVL(ST_GNVQ_CERTIFICATE_DISP_DATE,
*                          NVL(ARC_AWARD_DATE,SYSDATE)),
*			   'DD-MON-YYYY'),
*		   ST_GNVQ_ELIGIBILITY_CODE,
*		   ST_REG_TYPE,
*                  NULL,
*                  ST_FALLBACK,
*		   TO_CHAR(ST_BIRTH_DATE,'DD:MM:YY'),                     
*		   ST_INST_LOCATION,
*                  ST_COA_SYLLABUS_CODE,
*                  ST_COA_LEVEL_ACHIEVED,    
*                  DECODE(SUBSTR(ST_CENTRE_ID,1,2),68,
*       			ECLE_WELSH_CODE, ECLE_ENGLISH_CODE ),
*		   NVL(ST_RECON_IND,'N'),
*		   'Y',          
*       	   NVL(TO_CHAR(ST_GNVQ_CERTIFICATE_PRINT_DATE,
*                              'DD-MON-YYYY'),:WS-RUN-DATE),
*                  DECODE(ARC_REPRINT_WORDING,'Y','  REPLACEMENT',' '),
*       	   NVL(ARC_FIRST_REG_RCFE_ID,0),
*       	   NVL(ARC_LAST_REG_RCFE_ID,0),
*                  ST_DSA_SRU_ID,
*       	   'N'
*	    FROM   BTEC.ENTRY_COA_LEVELS,
*		   BTEC.STUDENTS@ARCA,
*		   BTEC.GNVQS,
*		   BTEC.ALL_REPRINT_CRITERIA
*	    WHERE  ARC_REPRINT_TYPE		= 'B'
*	    AND	   ST_CERT_NO 		   IS NOT NULL
*	    AND	   ST_REG_NO 
*       		BETWEEN ARC_FIRST_REG_NO
*	    		AND	NVL(ARC_LAST_REG_NO,ARC_FIRST_REG_NO)
*           AND	   ARC_AWARD_CODE 		= '27'
*	    AND	   ST_GNVQ_REGISTERED_ID 	= GNVQ_ID
*	    AND	   ST_RECON_IND 	       IS NULL
*	    AND	   TO_DATE(TO_CHAR(ST_GNVQ_CERTIFICATE_DISP_DATE,
*                                  'DD-MON-YYYY'),'DD-MON-YYYY')
*               	BETWEEN TO_DATE('01-JUL-1999','DD-MON-YYYY') 
*               	AND 	TO_DATE('31-AUG-1999','DD-MON-YYYY')
*	    AND	   GNVQ_PILOT_IND 		= 'Y'
*	    AND	   GNVQ_SINGLEAW_IND 	       IS NULL
*           AND    ECLE_AT_CODE(+)  		= ST_COA_SYLLABUS_CODE
*           AND    ECLE_LEVEL(+)    		= ST_COA_LEVEL_ACHIEVED
*	    AND    ST_GNVQ_REGISTERED_ID   IS NOT NULL
*           AND    ST_GNVQ_CERTIFICATE_NO  IS NOT NULL
*           AND    ST_GNVQ_ELIGIBILITY_CODE||'' = 'Y'
*           AND    ST_COURSE_ID		       != 'KSQ00'
*	    AND	   NVL(ST_UNIVERSITY_IND,'N')  != 'Y'
*           AND	   ARC_DB			= 'A'
*	END-EXEC.
*
*       EXEC SQL 
*           DECLARE PBBTECS_CUR CURSOR FOR
*       	   SELECT TO_CHAR(PBBT_AWARD_TITLE),
*                         PBBT_AWARD_CODE
*                    FROM PROFESSIONAL_BODY_BTECS
*                   WHERE PBBT_PBOD_ID = 10
*       END-EXEC.
BA-EXIT.
	EXIT.
*
/
BB-OPEN-OUTPUT SECTION.
********************************************************************************
*    THIS SECTION OPENS THE REJECTS AND LABELS FILES AND ACCEPTS THE           *
*    PARAMETERS.                                                               *
********************************************************************************
BB-START.
	OPEN OUTPUT REJECT-FILE
		    LABELS-FILE.
*
	ACCEPT	WS-FILESPEC.
*
	MOVE SPACES TO WS-FILENAME.
	MOVE SPACES TO WS-DIRNAME.
*
	UNSTRING WS-FILESPEC
		DELIMITED BY WS-FILESPEC-DELIM
		INTO WS-DIRNAME WS-FILENAME.
*
	MOVE WS-FILENAME(1:5) TO WS-FILE-PREFIX.
	MOVE WS-FILENAME(1:6) TO WS-AW-FILE-PREFIX.
*
	ACCEPT	WS-FORMAT-TYPE.
*
	IF WS-FORMAT-TYPE NOT = 'F' AND 'U'
		MOVE 'INVALID FORMAT TYPE - NOT F OR U' TO WS-ERR-MESSAGE
		PERFORM ZZ-ABORT
	END-IF.
*
	ACCEPT	WS-BATCH-NUMBER.
        MOVE    WS-BATCH-NUMBER TO WS-BATCH-NUMBER-O.
*
	ACCEPT  WS-ACCEPT-EXTERNAL-YN.
*
	IF	WS-ACCEPT-EXTERNAL-YN = "y" 
		MOVE "Y" TO WS-ACCEPT-EXTERNAL-YN 
	END-IF.
*
	IF	WS-ACCEPT-EXTERNAL-YN = "n" 
		MOVE "N" TO WS-ACCEPT-EXTERNAL-YN 
	END-IF.
*
        ACCEPT 	WS-IP-RUN-TYPE. 
        IF	WS-IP-RUN-TYPE NOT EQUAL TO 'A' AND 'S'
		MOVE 'INVALID RUN TYPE - NOT A OR S' TO WS-ERR-MESSAGE
		PERFORM ZZ-ABORT.
*
	IF	WS-IP-RUN-TYPE = 'A'
	  IF	WS-ACCEPT-EXTERNAL-YN NOT = "Y" AND "N" 
 		STRING 'INVALID EXTERNAL PRINT FLAG ' DELIMITED BY SIZE
		WS-ACCEPT-EXTERNAL-YN DELIMITED BY SIZE
			INTO WS-ERR-MESSAGE
	        PERFORM ZZ-ABORT
	  END-IF
	END-IF.
*
        IF WS-IP-RUN-TYPE EQUAL TO 'A' 
	   ACCEPT WS-IP-AWARD-DATE
           MOVE   WS-IP-AWARD-DATE TO WS-CERT-DATE
	   MOVE   'P' TO WS-AROF-RUN-TYPE
	ELSE
	   MOVE   'R' TO WS-AROF-RUN-TYPE
	END-IF.

	MOVE 'BTEC' TO WS-AROF-FILE-TYPE.
*
	MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE.
	MOVE WS-CURRENT-DATE(1:8) TO WS-DATE.

        MOVE WS-DAY	TO WS-REJECT-DAY.
        MOVE WS-MONTH	TO WS-REJECT-MONTH.
        MOVE WS-YEAR	TO WS-REJECT-YEAR.

        IF      WS-IP-RUN-TYPE EQUAL TO 'A'
                ACCEPT WS-INPUT-CENTRE
                IF WS-INPUT-CENTRE = '%%%%%%'
                   MOVE '++++++' TO WS-INPUT-CENTRE
                END-IF
        END-IF.

	IF WS-IP-RUN-TYPE = 'A'
	THEN
	   MOVE 'Print run' TO WS-RUN-TYPE
	   MOVE 'P'         TO WS-FILE-RUN-TYPE
	ELSE
	   MOVE 'Reprint run' TO WS-RUN-TYPE
	   MOVE 'R'           TO WS-FILE-RUN-TYPE
	END-IF.

BB-EXIT.
	EXIT.
*
/
BC-CONNECT SECTION.
********************************************************************************
*    THIS SECTION CONNECTS TO ORACLE.                                          *
********************************************************************************
BC-START.
*       EXEC SQL WHENEVER SQLERROR GO TO BC-050 END-EXEC.
*       EXEC SQL CONNECT :WS-USER-ID IDENTIFIED BY :WS-PASSWORD END-EXEC.
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
 	GO TO BC-EXIT.
BC-050.
	MOVE WS-ERR-CONNECT TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BC-EXIT.
	EXIT.
*
/
BCA-INSERT-RUN-DETAILS SECTION.
*******************************************************************************
*    THIS SECTION GETS THE CURRENT DATE AND TIME AND INSERTS A RECORD         *
*    INTO THE RUN_DETAILS TABLE.                                              *
*******************************************************************************
BCA-START.
        MOVE WS-DAY                  TO WS-ORAC-DAY.
        MOVE WS-MONTH-NAME(WS-MONTH) TO WS-ORAC-MONTH.
        MOVE WS-YEAR                 TO WS-ORAC-YEAR.
        MOVE WS-ORAC-DATE            TO WS-RUN-DATE.
*
BCA-010.
        CALL "LIB$GETJPI" USING BY REFERENCE  WS-ITEM-CODE,OMITTED,OMITTED, 
                                BY REFERENCE  WS-OUT-VALUE.
        MOVE WS-OUT-VALUE TO WS-START-TIME.
        MOVE WS-OUT-VALUE TO WS-DRF-START-TIME.
*
BCA-020.
        MOVE WS-FILENAME TO WS-PROGRAM.
*                            
*       EXEC SQL	WHENEVER SQLERROR  GO TO BCA-100	END-EXEC.
*
*       EXEC SQL
*                   INSERT INTO BTEC.RUN_DETAILS
*                          (RD_NAME,
*       		    RD_DATE)
*                   VALUES (:WS-PROGRAM,
*                           TO_DATE(:WS-RUN-DATE,'DD-MON-YYYY'))
*       END-EXEC.
        CALL "SQLADR" USING SQ0006 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 36 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-PROGRAM
            SQL-SQHSTV(1)
        MOVE 15 TO SQL-SQHSTL(1)
        MOVE 15 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RUN-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
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
            THEN GO TO BCA-100 END-IF.
*
        GO TO BCA-200.
*
BCA-100.
	MOVE 'INSERT OF RUN-DATE FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.                   
*
BCA-200.
*
*       EXEC SQL	WHENEVER SQLERROR	GO TO BCA-300 END-EXEC.
*       EXEC SQL	COMMIT WORK	END-EXEC.
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
            THEN GO TO BCA-300 END-IF.
 	GO TO BCA-999.
*
BCA-300.
	MOVE 'COMMIT OF RUN-DATE FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.                   
*
BCA-999.
         EXIT.
* 
/
BD-OPEN-CURSORS SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE OPENING OF CURSORS CURSOR_1 AND CURSOR_2        *
*    FOR A PRINT RUN AND CURSORS CURSOR_7 AND CURSOR_7A FOR A REPRINT RUN.     *
********************************************************************************
BD-START.
	PERFORM BDA-OPEN-CURSOR-1-2.
 	IF WS-IP-RUN-TYPE EQUAL TO 'S'
   	   PERFORM BDE-OPEN-CURSOR-7
        END-IF.

BD-EXIT.
 	EXIT.
*
/
BDA-OPEN-CURSOR-1-2 SECTION.
********************************************************************************
*    THIS SECTION OPENS CURSORS CURSOR_1 AND CURSOR_2.                         *
********************************************************************************
BDA-START.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE      END-EXEC.
*       EXEC SQL WHENEVER SQLERROR   GO TO BDA-050 END-EXEC.

         IF WS-IP-RUN-TYPE NOT EQUAL TO 'S'
*          EXEC SQL OPEN CURSOR_1                     END-EXEC
           CALL "SQLADR" USING SQ0001 SQL-STMT
           MOVE 1 TO SQL-ITERS
           MOVE 74 TO SQL-OFFSET
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
               WS-CERT-DATE
               SQL-SQHSTV(1)
           MOVE 11 TO SQL-SQHSTL(1)
           MOVE 11 TO SQL-SQHSTS(1)
           MOVE 0 TO SQL-SQINDV(1)
           MOVE 0 TO SQL-SQINDS(1)
           MOVE 0 TO SQL-SQHARM(1)
           CALL "SQLADR" USING
               WS-CERT-DATE
               SQL-SQHSTV(2)
           MOVE 11 TO SQL-SQHSTL(2)
           MOVE 11 TO SQL-SQHSTS(2)
           MOVE 0 TO SQL-SQINDV(2)
           MOVE 0 TO SQL-SQINDS(2)
           MOVE 0 TO SQL-SQHARM(2)
           CALL "SQLADR" USING
               WS-CERT-DATE
               SQL-SQHSTV(3)
           MOVE 11 TO SQL-SQHSTL(3)
           MOVE 11 TO SQL-SQHSTS(3)
           MOVE 0 TO SQL-SQINDV(3)
           MOVE 0 TO SQL-SQINDS(3)
           MOVE 0 TO SQL-SQHARM(3)
           CALL "SQLADR" USING
               WS-RUN-DATE
               SQL-SQHSTV(4)
           MOVE 11 TO SQL-SQHSTL(4)
           MOVE 11 TO SQL-SQHSTS(4)
           MOVE 0 TO SQL-SQINDV(4)
           MOVE 0 TO SQL-SQINDS(4)
           MOVE 0 TO SQL-SQHARM(4)
           CALL "SQLADR" USING
               WS-INPUT-CENTRE
               SQL-SQHSTV(5)
           MOVE 6 TO SQL-SQHSTL(5)
           MOVE 6 TO SQL-SQHSTS(5)
           MOVE 0 TO SQL-SQINDV(5)
           MOVE 0 TO SQL-SQINDS(5)
           MOVE 0 TO SQL-SQHARM(5)
           CALL "SQLADR" USING
               WS-CERT-DATE
               SQL-SQHSTV(6)
           MOVE 11 TO SQL-SQHSTL(6)
           MOVE 11 TO SQL-SQHSTS(6)
           MOVE 0 TO SQL-SQINDV(6)
           MOVE 0 TO SQL-SQINDS(6)
           MOVE 0 TO SQL-SQHARM(6)
           CALL "SQLADR" USING
               WS-CERT-DATE
               SQL-SQHSTV(7)
           MOVE 11 TO SQL-SQHSTL(7)
           MOVE 11 TO SQL-SQHSTS(7)
           MOVE 0 TO SQL-SQINDV(7)
           MOVE 0 TO SQL-SQINDS(7)
           MOVE 0 TO SQL-SQHARM(7)
           CALL "SQLADR" USING
               WS-CERT-DATE
               SQL-SQHSTV(8)
           MOVE 11 TO SQL-SQHSTL(8)
           MOVE 11 TO SQL-SQHSTS(8)
           MOVE 0 TO SQL-SQINDV(8)
           MOVE 0 TO SQL-SQINDS(8)
           MOVE 0 TO SQL-SQHARM(8)
           CALL "SQLADR" USING
               WS-RUN-DATE
               SQL-SQHSTV(9)
           MOVE 11 TO SQL-SQHSTL(9)
           MOVE 11 TO SQL-SQHSTS(9)
           MOVE 0 TO SQL-SQINDV(9)
           MOVE 0 TO SQL-SQINDS(9)
           MOVE 0 TO SQL-SQHARM(9)
           CALL "SQLADR" USING
               WS-INPUT-CENTRE
               SQL-SQHSTV(10)
           MOVE 6 TO SQL-SQHSTL(10)
           MOVE 6 TO SQL-SQHSTS(10)
           MOVE 0 TO SQL-SQINDV(10)
           MOVE 0 TO SQL-SQINDS(10)
           MOVE 0 TO SQL-SQHARM(10)
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
               THEN GO TO BDA-050 END-IF
        END-IF.

*       EXEC SQL WHENEVER SQLERROR   GO TO BDA-100 END-EXEC.
*       EXEC SQL OPEN CURSOR_2                     END-EXEC.
        CALL "SQLADR" USING SQ0002 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 129 TO SQL-OFFSET
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

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BDA-100 END-IF.
	GO TO BDA-EXIT.
BDA-050.
	MOVE 'OPEN CURSOR_1 FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BDA-100.
	MOVE 'OPEN CURSOR_2 FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BDA-EXIT.
	EXIT.
*
/
BDE-OPEN-CURSOR-7 SECTION.
********************************************************************************
*    THIS SECTION OPENS CURSORS CURSOR_7 AND CURSOR_7A.                        *
********************************************************************************
BDE-START.
*       EXEC SQL WHENEVER SQLERROR   GO TO BDE-050 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE      END-EXEC.
*       EXEC SQL OPEN CURSOR_7                     END-EXEC.
        CALL "SQLADR" USING SQ0003 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 144 TO SQL-OFFSET
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
            WS-RUN-DATE
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RUN-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
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
            THEN GO TO BDE-050 END-IF.

*       EXEC SQL WHENEVER SQLERROR   GO TO BDE-100 END-EXEC.
*       EXEC SQL OPEN CURSOR_7A                    END-EXEC.
        CALL "SQLADR" USING SQ0004 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 167 TO SQL-OFFSET
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
            WS-RUN-DATE
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RUN-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
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
            THEN GO TO BDE-100 END-IF.
 
	GO TO BDE-EXIT.
BDE-050.
	MOVE 'OPEN CURSOR 7 FAILED' TO WS-ERR-MESSAGE.
 	PERFORM ZZ-ABORT.
BDE-100.
	MOVE 'OPEN CURSOR 7A FAILED' TO WS-ERR-MESSAGE.
 	PERFORM ZZ-ABORT.
BDE-EXIT.
	EXIT.
*
/
BF-LOAD-RESULTS SECTION.
********************************************************************************
*    THIS SECTION LOADS THE RESULTS TABLE, INDEXED BY WS-RES-IND.              *
********************************************************************************
BF-START.
	INITIALIZE WS-RES-CODE
		   WS-RES-DESC.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO BF-050 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE     END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO BF-100 END-EXEC.
*
*       EXEC SQL FETCH CURSOR_2 INTO 	:WS-RES-CODE,
*       				:WS-RES-DESC
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 190 TO SQL-OFFSET
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
            WS-RES-CODE
            SQL-SQHSTV(1)
        MOVE 1 TO SQL-SQHSTL(1)
        MOVE 1 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RES-DESC
            SQL-SQHSTV(2)
        MOVE 15 TO SQL-SQHSTL(2)
        MOVE 15 TO SQL-SQHSTS(2)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO BF-100 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BF-050 END-IF.
*
	MOVE WS-RES-CODE TO WS-RESULT-CODE(WS-RES-IND). 
	MOVE WS-RES-DESC TO WS-RESULT-DESC(WS-RES-IND).
	GO TO BF-EXIT.
BF-050.
	MOVE 'LOAD RESULTS CODES TABLE FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BF-100.
	MOVE 'Y' TO WS-END.
BF-EXIT.
	EXIT.
*
/
BG-CLOSE-CURSOR2 SECTION.
********************************************************************************
*    THIS SECTION CLOSES CURSOR CURSOR_2.                                      *
********************************************************************************
BG-START.
*       EXEC SQL WHENEVER SQLERROR GO TO BG-050 END-EXEC.
*       EXEC SQL CLOSE CURSOR_2                 END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 213 TO SQL-OFFSET
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
            THEN GO TO BG-050 END-IF.
*
	GO TO BG-EXIT.
BG-050.
	MOVE 'CLOSE CURSOR 2 FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BG-EXIT.
	EXIT.
*
/
BH-BANNER-DETAILS SECTION.
********************************************************************************
*    THIS SECTION OUTPUTS A BANNER PAGE GIVING THE RUN DETAILS.		       *
*    THIS SECTION IS NO LONGER PERFORMED.                       	       *
********************************************************************************
BH-START.
*
*       EXEC SQL WHENEVER SQLERROR GO TO BH-010 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND GO TO BH-010 END-EXEC.
*
*       EXEC SQL
*           SELECT TO_CHAR (SYSDATE, 'DD-MON-YYYY:HH24:MI'),
*       	   AC_DESCRIPTION || ' (' || :WS-AWARD-CODE-SAVE || ')'
*           INTO   :WS-TIME-DATE,
*       	   :WS-AWARD-NAME
*           FROM   BTEC.AWARD_CODES
*           WHERE  AC_CODE = :WS-PARAM-AWARD-CODE
*       END-EXEC.
        CALL "SQLADR" USING SQ0008 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 228 TO SQL-OFFSET
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
            WS-AWARD-CODE-SAVE
            SQL-SQHSTV(1)
        MOVE 2 TO SQL-SQHSTL(1)
        MOVE 2 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-TIME-DATE
            SQL-SQHSTV(2)
        MOVE 17 TO SQL-SQHSTL(2)
        MOVE 17 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-AWARD-NAME
            SQL-SQHSTV(3)
        MOVE 65 TO SQL-SQHSTL(3)
        MOVE 65 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-PARAM-AWARD-CODE
            SQL-SQHSTV(4)
        MOVE 2 TO SQL-SQHSTL(4)
        MOVE 2 TO SQL-SQHSTS(4)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO BH-010 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BH-010 END-IF.
*
*       EXEC SQL WHENEVER SQLERROR GO TO BH-020 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND GO TO BH-020 END-EXEC.
*
*       EXEC SQL
*           SELECT FCSF_DOC_REF,
*       	   FCSF_CERT_TEXT_1,
*       	   FCSF_CERT_TEXT_2,
*       	   FCSF_CERT_TEXT_3
*           INTO   :WS-AWARD-DOC-REF,
*       	   :WS-AWARD-CERT-DESCRIPTION-1,
*       	   :WS-AWARD-CERT-DESCRIPTION-2,
*       	   :WS-AWARD-CERT-DESCRIPTION-3
*           FROM   BTEC.FCS_FILES
*           WHERE  FCSF_FILENAME = :WS-FILE-PREFIX || :WS-AWARD-CODE-SAVE
*       END-EXEC.
        CALL "SQLADR" USING SQ0009 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 259 TO SQL-OFFSET
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
            WS-AWARD-DOC-REF
            SQL-SQHSTV(1)
        MOVE 20 TO SQL-SQHSTL(1)
        MOVE 20 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-AWARD-CERT-DESCRIPTION-1
            SQL-SQHSTV(2)
        MOVE 30 TO SQL-SQHSTL(2)
        MOVE 30 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-AWARD-CERT-DESCRIPTION-2
            SQL-SQHSTV(3)
        MOVE 30 TO SQL-SQHSTL(3)
        MOVE 30 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-AWARD-CERT-DESCRIPTION-3
            SQL-SQHSTV(4)
        MOVE 30 TO SQL-SQHSTL(4)
        MOVE 30 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-FILE-PREFIX
            SQL-SQHSTV(5)
        MOVE 5 TO SQL-SQHSTL(5)
        MOVE 5 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-AWARD-CODE-SAVE
            SQL-SQHSTV(6)
        MOVE 2 TO SQL-SQHSTL(6)
        MOVE 2 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
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
            THEN GO TO BH-020 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BH-020 END-IF.
*
	IF WS-IP-RUN-TYPE = 'A' THEN
	    MOVE 'Print run' TO WS-RUN-TYPE
        ELSE
	    MOVE 'Reprint run' TO WS-RUN-TYPE
        END-IF.
*
	IF WS-FORMAT-TYPE = 'F' THEN
	    MOVE WS-TIME-DATE  TO WS-BAN-TIME
	    MOVE WS-RUN-TYPE   TO WS-BAN-TYPE
	    MOVE WS-AWARD-NAME TO WS-BAN-DESCRIPTION
	    WRITE PRINT-REC FROM WS-BAN-LINE-1
	    WRITE PRINT-REC FROM WS-BAN-LINE-2
	    WRITE PRINT-REC FROM WS-BAN-LINE-3
	ELSE
	    MOVE SPACES TO WS-ANS-REC
	    MOVE 1 TO WS-POINTER
*
	    STRING '01*!*' DELIMITED BY SIZE 
	    	WS-TIME-DATE DELIMITED BY SIZE
            	INTO   WS-ANS-REC
	    	WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'       DELIMITED BY SIZE
                   WS-RUN-TYPE DELIMITED BY SIZE 
            INTO   WS-ANS-REC
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'         DELIMITED BY SIZE
                   WS-AWARD-NAME DELIMITED BY SIZE
            INTO   WS-ANS-REC
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'         DELIMITED BY SIZE
                   WS-AWARD-CERT-DESCRIPTION-1 DELIMITED BY SIZE
            INTO   WS-ANS-REC
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'         DELIMITED BY SIZE
                   WS-AWARD-CERT-DESCRIPTION-2 DELIMITED BY SIZE
            INTO   WS-ANS-REC
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'         DELIMITED BY SIZE
                   WS-AWARD-CERT-DESCRIPTION-3 DELIMITED BY SIZE
            INTO   WS-ANS-REC
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'         DELIMITED BY SIZE
                   WS-AWARD-DOC-REF DELIMITED BY SIZE
            INTO   WS-ANS-REC
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    COMPUTE WS-ANS-LENGTH = WS-POINTER - 1
*
	    WRITE PRINT-REC FROM WS-ANS-REC
	END-IF.
*
	GO TO BH-EXIT.
*
BH-010.
	MOVE 'FAILED TO FIND AWARD DESCRIPTION' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BH-020.
	MOVE 'FAILED TO FIND FCS FILE' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BH-EXIT.
	EXIT.
*
/
BJ-LOAD-PBBTECS SECTION.
BJ-000.
	INITIALIZE WS-BTEC-AWARDS-TABLE.
        PERFORM BJA-OPEN-PBBTECS-CURSOR.
        PERFORM BJB-FETCH-PBBTECS-CURSOR
               VARYING WS-BTEC-IND FROM 1 BY 1 
		      UNTIL WS-BTEC-IND > 100
                         OR END-OF-PBBTECS.
        PERFORM BJC-CLOSE-PBBTECS-CURSOR.
BJ-999.
        EXIT.
/
BJA-OPEN-PBBTECS-CURSOR SECTION.
BJA-000.
*       EXEC SQL WHENEVER SQLERROR GOTO BJA-090 END-EXEC.
*       EXEC SQL
*           OPEN PBBTECS_CUR
*       END-EXEC.
        CALL "SQLADR" USING SQ0005 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 298 TO SQL-OFFSET
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

        CALL "SQLBEX" USING
            SQLCTX
            SQLEXD
            SQLFPN

        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BJA-090 END-IF.
        GO TO BJA-999.
BJA-090.
	MOVE 'OPEN PBBTECS_CUR FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BJA-999.         
        EXIT.
/
BJB-FETCH-PBBTECS-CURSOR SECTION.
BJB-000.
*       EXEC SQL WHENEVER SQLERROR GOTO BJB-090 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING GOTO BJB-090 END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND GOTO BJB-100 END-EXEC.
*       EXEC SQL
*           FETCH PBBTECS_CUR
*            INTO :WS-BTEC-AWARD-TITLE,
*       	  :WS-BTEC-AWARD-CODE
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 313 TO SQL-OFFSET
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
            WS-BTEC-AWARD-TITLE
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-BTEC-AWARD-CODE
            SQL-SQHSTV(2)
        MOVE 2 TO SQL-SQHSTL(2)
        MOVE 2 TO SQL-SQHSTS(2)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO BJB-100 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO BJB-090 END-IF
        IF SQLWARN0 IS EQUAL TO "W"
            THEN GO TO BJB-090 END-IF.
        MOVE WS-BTEC-AWARD-CODE  TO WS-TAB-BTEC-AWARD-CODE(WS-BTEC-IND).
        MOVE WS-BTEC-AWARD-TITLE TO WS-TAB-BTEC-AWARD-TITLE(WS-BTEC-IND).
        GO TO BJB-999.
BJB-090.
	MOVE 'FETCH PBBTECS_CUR FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BJB-100.
        MOVE 'Y' TO WS-PBBTECS-END.
BJB-999.         
        EXIT.
/
BJC-CLOSE-PBBTECS-CURSOR SECTION.
BJC-000.
*       EXEC SQL WHENEVER SQLERROR GOTO BJC-090 END-EXEC.
*       EXEC SQL
*           CLOSE PBBTECS_CUR
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 336 TO SQL-OFFSET
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
            THEN GO TO BJC-090 END-IF.
        GO TO BJC-999.
BJC-090.
	MOVE 'CLOSE PBBTECS_CUR FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
BJC-999.         
        EXIT.
/
BZ-SET-GLOBALS SECTION.
BZ-START.
*       EXEC SQL WHENEVER SQLERROR    GO TO BZ-100 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING  CONTINUE      END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND   CONTINUE      END-EXEC.
*
*       EXEC SQL EXECUTE
*        BEGIN
*         PK_ST_GLOBAL.PR_SET_GLOBALS('STP070',:WS-BATCH-NUMBER);
*        END;
*       END-EXEC.
        CALL "SQLADR" USING SQ0010 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 351 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-BATCH-NUMBER IN
            WS-BITS
            SQL-SQHSTV(1)
        MOVE 2 TO SQL-SQHSTL(1)
        MOVE 2 TO SQL-SQHSTS(1)
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
            THEN GO TO BZ-100 END-IF.
*
        GO TO BZ-EXIT.
*
BZ-100.
        MOVE 'Call to set global variables failed' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
*
BZ-EXIT.
        EXIT.
*
/
C-INPUT-PROCEDURE SECTION.
********************************************************************************
*    THIS SECTION CONTROLS SELECTION OF THE REQUIRED STUDENT INFORMATION.      *
*    WHERE THE RUN TYPE IS 'A' ALL STUDENTS ARE PROCESSED AS NORMAL.           *
*    WHERE THE RUN TYPE IS 'S' ONLY STUDENTS FOUND ON THE AWARDS REPRINT       *
*    TABLE ARE PROCESSED.                                                      *
********************************************************************************
C-START.
	IF  WS-IP-RUN-TYPE EQUAL TO 'S'
	    GO TO C-300
	END-IF.
	
	INITIALIZE WS-CERT-NO-TAB,
		   WS-CNT-INDEX,
		   WS-CNT-FETCHED.

*       EXEC SQL WHENEVER SQLERROR  GO TO C-100  END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND CONTINUE     END-EXEC.

*       EXEC SQL 
*                SELECT SUBSTR(CV_TYPE,9,2),
*       		CV_TYPE,
*       		CV_NEXT_VALUE
*                INTO   :WS-CNT-AWARD-CODE,
*       		:WS-CNT-CV-TYPE,
*       		:WS-CNT-NEXT-CERT-NO
*                FROM   BTEC.CONTROL_VALUES
*                WHERE  CV_TYPE LIKE :WS-CERT-NO-TYPE
*       END-EXEC.
        CALL "SQLADR" USING SQ0011 SQL-STMT
        MOVE 1000 TO SQL-ITERS
        MOVE 370 TO SQL-OFFSET
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
            WS-CNT-AWARD-CODE IN
            WS-CERT-NO-TAB(1)
            SQL-SQHSTV(1)
        MOVE 2 TO SQL-SQHSTL(1)
        MOVE 2 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-CNT-CV-TYPE IN
            WS-CERT-NO-TAB(1)
            SQL-SQHSTV(2)
        MOVE 16 TO SQL-SQHSTL(2)
        MOVE 16 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-CNT-NEXT-CERT-NO IN
            WS-CERT-NO-TAB(1)
            SQL-SQHSTV(3)
        MOVE 5 TO SQL-SQHSTL(3)
        MOVE 5 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-CERT-NO-TYPE
            SQL-SQHSTV(4)
        MOVE 8 TO SQL-SQHSTL(4)
        MOVE 8 TO SQL-SQHSTS(4)
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
            THEN GO TO C-100 END-IF.

	MOVE SQLERRD(3) TO WS-CNT-FETCHED.

	GO TO C-200.
C-100.
        MOVE 'SELECT OF NEXT CERTIFICATE NUMBERS FAILED'
          TO  WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
C-200.  
	MOVE 'N' TO WS-END.
	PERFORM CA-FETCH-STUDENTS UNTIL END-OF-FETCH.
	GO TO C-EXIT.
C-300.
	MOVE 'N' TO WS-END.
	PERFORM CB-FETCH-STUDENTS UNTIL END-OF-FETCH.
 	MOVE 'N' TO WS-END.
 	PERFORM CB1-FETCH-STUDENTS UNTIL END-OF-FETCH.
C-EXIT.
	EXIT.
*
/
CA-FETCH-STUDENTS SECTION.
********************************************************************************
*    THIS SECTION OBTAINS THE REQUIRED STUDENT DETAILS.                        *
********************************************************************************
CA-START.
	INITIALIZE  	WS-REG-NO,    
			WS-STUDENT-NAME,
 			WS-CENTRE-NO,
			WS-COURSE-NO,
			WS-COMB,
                        WS-CERT-NO,
			WS-AWARD-CLAIM,
			WS-MONTH-YEAR,
			WS-AWARD-DATE,
			WS-ELIG,
			WS-REG-TYPE,
                        WS-OVER-GRADE,
                        WS-FALLBACK,
			WS-BIRTH-DATE,
			WS-INST-LOCATION,
                        WS-COA-SYLLABUS-CODE,
                        WS-COA-LEVEL-ACHIEVED,
                        WS-COA-LEVEL-ACHIEVED-DESCR,
			WS-RECON-IND,
			WS-KSQ,
			WS-CERT-ISSUE-DATE,
			WS-SCHEME-REG-NO,
			WS-SCHEME-REG-NO-I,
			WS-BTEC-CODE,
			WS-BTEC-LEVEL,
			WS-NOPS-REQD,
			WS-NOPS-REQD-I,
			WS-NVQ-ROA-CODE,
			WS-NVQ-ROA-CODE-I,
			WS-GNVQ-CUC-CODE,
			WS-GNVQ-CUC-CODE-I,
			WS-CRED-TRANS-CODE,
			WS-CRED-TRANS-CODE-I,
			WS-ABS-LEVEL-ACHIEVED,
			WS-ABS-LEVEL-ACHIEVED-I,
			WS-ESOL-LEVEL-ACHIEVED,
			WS-ESOL-LEVEL-ACHIEVED-I,
			WS-CERT-FLAG,
                        WS-CLTE-TEXT.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO CA-060   END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE       END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO CA-030   END-EXEC.
*
*	RSH 08/08/2005 (RfW 04/0109):
*	Additionally fetch the ESOL level.
*
*       EXEC SQL FETCH CURSOR_1 INTO	:WS-REG-NO,
*       				:WS-STUDENT-NAME,
*       				:WS-CENTRE-NO,
*       				:WS-COURSE-NO,
*       				:WS-COMB,
*                                       :WS-CERT-NO,
*       				:WS-AWARD-CLAIM,
*       				:WS-MONTH-YEAR,
*       				:WS-AWARD-DATE,
*       				:WS-ELIG,
*       				:WS-REG-TYPE,
*                                       :WS-OVER-GRADE,
*                                       :WS-FALLBACK,
*       				:WS-BIRTH-DATE,
*       				:WS-INST-LOCATION,
*                                       :WS-COA-SYLLABUS-CODE,
*                                       :WS-COA-LEVEL-ACHIEVED,
*                                       :WS-COA-LEVEL-ACHIEVED-DESCR,
*       				:WS-RECON-IND,
*       				:WS-KSQ,
*       				:WS-CERT-ISSUE-DATE,
*       				:WS-SCHEME-REG-NO:WS-SCHEME-REG-NO-I,
*       				:WS-BTEC-CODE,
*       				:WS-BTEC-LEVEL,
*       				:WS-NOPS-REQD:WS-NOPS-REQD-I,
*       				:WS-NVQ-ROA-CODE:WS-NVQ-ROA-CODE-I,
*       				:WS-GNVQ-CUC-CODE:WS-GNVQ-CUC-CODE-I,
*       			    :WS-CRED-TRANS-CODE:WS-CRED-TRANS-CODE-I,
*       			:WS-ABS-LEVEL-ACHIEVED:WS-ABS-LEVEL-ACHIEVED-I,
*       		      :WS-ESOL-LEVEL-ACHIEVED:WS-ESOL-LEVEL-ACHIEVED-I,
*       				:WS-CERT-FLAG,
*                                       :WS-DSA-SRU-NUMBER,
*       				:WS-OLD-REPRINT-CERT,
*                                       :WS-CLTE-TEXT
*					:WS-CERT-FREE-TEXT
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 401 TO SQL-OFFSET
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
            WS-REG-NO
            SQL-SQHSTV(1)
        MOVE 7 TO SQL-SQHSTL(1)
        MOVE 7 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-STUDENT-NAME
            SQL-SQHSTV(2)
        MOVE 51 TO SQL-SQHSTL(2)
        MOVE 51 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-CENTRE-NO
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(4)
        MOVE 8 TO SQL-SQHSTL(4)
        MOVE 8 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-COMB
            SQL-SQHSTV(5)
        MOVE 1 TO SQL-SQHSTL(5)
        MOVE 1 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-CERT-NO
            SQL-SQHSTV(6)
        MOVE 5 TO SQL-SQHSTL(6)
        MOVE 5 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-AWARD-CLAIM
            SQL-SQHSTV(7)
        MOVE 1 TO SQL-SQHSTL(7)
        MOVE 1 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-MONTH-YEAR
            SQL-SQHSTV(8)
        MOVE 18 TO SQL-SQHSTL(8)
        MOVE 18 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            WS-AWARD-DATE
            SQL-SQHSTV(9)
        MOVE 11 TO SQL-SQHSTL(9)
        MOVE 11 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            WS-ELIG
            SQL-SQHSTV(10)
        MOVE 1 TO SQL-SQHSTL(10)
        MOVE 1 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            WS-REG-TYPE
            SQL-SQHSTV(11)
        MOVE 1 TO SQL-SQHSTL(11)
        MOVE 1 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            WS-OVER-GRADE
            SQL-SQHSTV(12)
        MOVE 3 TO SQL-SQHSTL(12)
        MOVE 3 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            WS-FALLBACK
            SQL-SQHSTV(13)
        MOVE 1 TO SQL-SQHSTL(13)
        MOVE 1 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            WS-BIRTH-DATE
            SQL-SQHSTV(14)
        MOVE 8 TO SQL-SQHSTL(14)
        MOVE 8 TO SQL-SQHSTS(14)
        MOVE 0 TO SQL-SQINDV(14)
        MOVE 0 TO SQL-SQINDS(14)
        MOVE 0 TO SQL-SQHARM(14)
        CALL "SQLADR" USING
            WS-INST-LOCATION
            SQL-SQHSTV(15)
        MOVE 6 TO SQL-SQHSTL(15)
        MOVE 6 TO SQL-SQHSTS(15)
        MOVE 0 TO SQL-SQINDV(15)
        MOVE 0 TO SQL-SQINDS(15)
        MOVE 0 TO SQL-SQHARM(15)
        CALL "SQLADR" USING
            WS-COA-SYLLABUS-CODE
            SQL-SQHSTV(16)
        MOVE 6 TO SQL-SQHSTL(16)
        MOVE 6 TO SQL-SQHSTS(16)
        MOVE 0 TO SQL-SQINDV(16)
        MOVE 0 TO SQL-SQINDS(16)
        MOVE 0 TO SQL-SQHARM(16)
        CALL "SQLADR" USING
            WS-COA-LEVEL-ACHIEVED
            SQL-SQHSTV(17)
        MOVE 1 TO SQL-SQHSTL(17)
        MOVE 1 TO SQL-SQHSTS(17)
        MOVE 0 TO SQL-SQINDV(17)
        MOVE 0 TO SQL-SQINDS(17)
        MOVE 0 TO SQL-SQHARM(17)
        CALL "SQLADR" USING
            WS-COA-LEVEL-ACHIEVED-DESCR
            SQL-SQHSTV(18)
        MOVE 30 TO SQL-SQHSTL(18)
        MOVE 30 TO SQL-SQHSTS(18)
        MOVE 0 TO SQL-SQINDV(18)
        MOVE 0 TO SQL-SQINDS(18)
        MOVE 0 TO SQL-SQHARM(18)
        CALL "SQLADR" USING
            WS-RECON-IND
            SQL-SQHSTV(19)
        MOVE 1 TO SQL-SQHSTL(19)
        MOVE 1 TO SQL-SQHSTS(19)
        MOVE 0 TO SQL-SQINDV(19)
        MOVE 0 TO SQL-SQINDS(19)
        MOVE 0 TO SQL-SQHARM(19)
        CALL "SQLADR" USING
            WS-KSQ
            SQL-SQHSTV(20)
        MOVE 1 TO SQL-SQHSTL(20)
        MOVE 1 TO SQL-SQHSTS(20)
        MOVE 0 TO SQL-SQINDV(20)
        MOVE 0 TO SQL-SQINDS(20)
        MOVE 0 TO SQL-SQHARM(20)
        CALL "SQLADR" USING
            WS-CERT-ISSUE-DATE
            SQL-SQHSTV(21)
        MOVE 11 TO SQL-SQHSTL(21)
        MOVE 11 TO SQL-SQHSTS(21)
        MOVE 0 TO SQL-SQINDV(21)
        MOVE 0 TO SQL-SQINDS(21)
        MOVE 0 TO SQL-SQHARM(21)
        CALL "SQLADR" USING
            WS-SCHEME-REG-NO
            SQL-SQHSTV(22)
        MOVE 7 TO SQL-SQHSTL(22)
        MOVE 7 TO SQL-SQHSTS(22)
        CALL "SQLADR" USING
            WS-SCHEME-REG-NO-I
            SQL-SQINDV(22)
        MOVE 0 TO SQL-SQINDS(22)
        MOVE 0 TO SQL-SQHARM(22)
        CALL "SQLADR" USING
            WS-BTEC-CODE
            SQL-SQHSTV(23)
        MOVE 4 TO SQL-SQHSTL(23)
        MOVE 4 TO SQL-SQHSTS(23)
        MOVE 0 TO SQL-SQINDV(23)
        MOVE 0 TO SQL-SQINDS(23)
        MOVE 0 TO SQL-SQHARM(23)
        CALL "SQLADR" USING
            WS-BTEC-LEVEL
            SQL-SQHSTV(24)
        MOVE 2 TO SQL-SQHSTL(24)
        MOVE 2 TO SQL-SQHSTS(24)
        MOVE 0 TO SQL-SQINDV(24)
        MOVE 0 TO SQL-SQINDS(24)
        MOVE 0 TO SQL-SQHARM(24)
        CALL "SQLADR" USING
            WS-NOPS-REQD
            SQL-SQHSTV(25)
        MOVE 1 TO SQL-SQHSTL(25)
        MOVE 1 TO SQL-SQHSTS(25)
        CALL "SQLADR" USING
            WS-NOPS-REQD-I
            SQL-SQINDV(25)
        MOVE 0 TO SQL-SQINDS(25)
        MOVE 0 TO SQL-SQHARM(25)
        CALL "SQLADR" USING
            WS-NVQ-ROA-CODE
            SQL-SQHSTV(26)
        MOVE 1 TO SQL-SQHSTL(26)
        MOVE 1 TO SQL-SQHSTS(26)
        CALL "SQLADR" USING
            WS-NVQ-ROA-CODE-I
            SQL-SQINDV(26)
        MOVE 0 TO SQL-SQINDS(26)
        MOVE 0 TO SQL-SQHARM(26)
        CALL "SQLADR" USING
            WS-GNVQ-CUC-CODE
            SQL-SQHSTV(27)
        MOVE 1 TO SQL-SQHSTL(27)
        MOVE 1 TO SQL-SQHSTS(27)
        CALL "SQLADR" USING
            WS-GNVQ-CUC-CODE-I
            SQL-SQINDV(27)
        MOVE 0 TO SQL-SQINDS(27)
        MOVE 0 TO SQL-SQHARM(27)
        CALL "SQLADR" USING
            WS-CRED-TRANS-CODE
            SQL-SQHSTV(28)
        MOVE 1 TO SQL-SQHSTL(28)
        MOVE 1 TO SQL-SQHSTS(28)
        CALL "SQLADR" USING
            WS-CRED-TRANS-CODE-I
            SQL-SQINDV(28)
        MOVE 0 TO SQL-SQINDS(28)
        MOVE 0 TO SQL-SQHARM(28)
        CALL "SQLADR" USING
            WS-ABS-LEVEL-ACHIEVED
            SQL-SQHSTV(29)
        MOVE 1 TO SQL-SQHSTL(29)
        MOVE 1 TO SQL-SQHSTS(29)
        CALL "SQLADR" USING
            WS-ABS-LEVEL-ACHIEVED-I
            SQL-SQINDV(29)
        MOVE 0 TO SQL-SQINDS(29)
        MOVE 0 TO SQL-SQHARM(29)
        CALL "SQLADR" USING
            WS-ESOL-LEVEL-ACHIEVED
            SQL-SQHSTV(30)
        MOVE 2 TO SQL-SQHSTL(30)
        MOVE 2 TO SQL-SQHSTS(30)
        CALL "SQLADR" USING
            WS-ESOL-LEVEL-ACHIEVED-I
            SQL-SQINDV(30)
        MOVE 0 TO SQL-SQINDS(30)
        MOVE 0 TO SQL-SQHARM(30)
        CALL "SQLADR" USING
            WS-CERT-FLAG
            SQL-SQHSTV(31)
        MOVE 2 TO SQL-SQHSTL(31)
        MOVE 2 TO SQL-SQHSTS(31)
        MOVE 0 TO SQL-SQINDV(31)
        MOVE 0 TO SQL-SQINDS(31)
        MOVE 0 TO SQL-SQHARM(31)
        CALL "SQLADR" USING
            WS-DSA-SRU-NUMBER
            SQL-SQHSTV(32)
        MOVE 12 TO SQL-SQHSTL(32)
        MOVE 12 TO SQL-SQHSTS(32)
        MOVE 0 TO SQL-SQINDV(32)
        MOVE 0 TO SQL-SQINDS(32)
        MOVE 0 TO SQL-SQHARM(32)
        CALL "SQLADR" USING
            WS-OLD-REPRINT-CERT
            SQL-SQHSTV(33)
        MOVE 1 TO SQL-SQHSTL(33)
        MOVE 1 TO SQL-SQHSTS(33)
        MOVE 0 TO SQL-SQINDV(33)
        MOVE 0 TO SQL-SQINDS(33)
        MOVE 0 TO SQL-SQHARM(33)
        CALL "SQLADR" USING
            WS-CLTE-TEXT
            SQL-SQHSTV(34)
        MOVE 70 TO SQL-SQHSTL(34)
        MOVE 70 TO SQL-SQHSTS(34)
        MOVE 0 TO SQL-SQINDV(34)
        MOVE 0 TO SQL-SQINDS(34)
        MOVE 0 TO SQL-SQHARM(34)
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
            THEN GO TO CA-030 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO CA-060 END-IF.
*

	ADD	+1 	     TO WS-B-TOT-STDTS-PROCESSED.
	MOVE WS-REG-NO       TO S-REG-NO.
	MOVE WS-STUDENT-NAME TO S-STUDENT-NAME.
	MOVE WS-CENTRE-NO    TO S-CENTRE-NO.
	MOVE WS-COURSE-NO    TO S-COURSE-NO.
	MOVE WS-COMB         TO S-COMB.
        MOVE WS-CERT-NO      TO S-CERT-NO.
	MOVE WS-AWARD-CLAIM  TO S-AWARD-CLAIM.
	MOVE WS-MONTH-YEAR   TO S-MONTH-YEAR.
        MOVE WS-AWARD-DATE   TO S-AWARD-DATE.
	MOVE WS-ELIG	     TO S-ELIG.
	MOVE WS-REG-TYPE     TO S-REG-TYPE.
        MOVE WS-OVER-GRADE   TO S-OVER-GRADE.
        MOVE WS-FALLBACK     TO S-FALLBACK.
        MOVE WS-BIRTH-DATE   TO S-BIRTH-DATE.
	MOVE WS-INST-LOCATION TO S-INST-LOCATION.
        MOVE WS-COA-SYLLABUS-CODE        TO S-COA-SYLLABUS-CODE.
        MOVE WS-COA-LEVEL-ACHIEVED       TO S-COA-LEVEL-ACHIEVED.
        MOVE WS-COA-LEVEL-ACHIEVED-DESCR TO S-COA-LEVEL-ACHIEVED-DESCR.
	MOVE WS-RECON-IND    TO S-RECON-IND.
	MOVE WS-KSQ	     TO S-KSQ.
	MOVE WS-CERT-ISSUE-DATE TO S-ISSUE-DATE.
        MOVE WS-SCHEME-REG-NO TO S-SCHEME-REG-NO.
        MOVE WS-SCHEME-REG-NO-I TO S-SCHEME-REG-NO-I.
	MOVE WS-BTEC-CODE    TO S-BTEC-CODE.
	MOVE WS-BTEC-LEVEL   TO S-BTEC-LEVEL.
	MOVE WS-BTEC-LEVEL   TO S-AWARD-CODE.
        MOVE WS-DSA-SRU-NUMBER TO S-DSA-SRU-ID.
	MOVE WS-OLD-REPRINT-CERT TO S-OLD-REPRINT-CERT.

	IF WS-NOPS-REQD-I = -1
	  MOVE SPACES TO S-NOPS-REQD
	ELSE
	  MOVE WS-NOPS-REQD TO S-NOPS-REQD
	END-IF.
	IF WS-NVQ-ROA-CODE-I = -1
	  MOVE SPACES TO S-NVQ-ROA-CODE
	ELSE
	  MOVE WS-NVQ-ROA-CODE TO S-NVQ-ROA-CODE
	END-IF.
	IF WS-GNVQ-CUC-CODE-I = -1
	  MOVE SPACES TO S-GNVQ-CUC-CODE
	ELSE
	  MOVE WS-GNVQ-CUC-CODE TO S-GNVQ-CUC-CODE
	END-IF.
	IF WS-CRED-TRANS-CODE-I = -1
	  MOVE SPACES TO S-CRED-TRANS-CODE
	ELSE
	  MOVE WS-CRED-TRANS-CODE TO S-CRED-TRANS-CODE
	END-IF.
	IF WS-ABS-LEVEL-ACHIEVED-I = -1
	  MOVE SPACES TO S-ABS-LEVEL-ACHIEVED
	ELSE
	  MOVE WS-ABS-LEVEL-ACHIEVED TO S-ABS-LEVEL-ACHIEVED
	END-IF.
*
*	RSH 08/08/2005 (RfW 04/0109):
*	Additionally store the ESOL level.
*
	IF WS-ESOL-LEVEL-ACHIEVED-I = -1
	  MOVE SPACES TO S-ESOL-LEVEL-ACHIEVED
	ELSE
	  MOVE WS-ESOL-LEVEL-ACHIEVED TO S-ESOL-LEVEL-ACHIEVED
	END-IF.
	MOVE WS-CERT-FLAG    TO S-CERT-FLAG.
*EC2490
        MOVE WS-CLTE-TEXT    TO S-CERT-MSG-SURF.
CA-020.
	MOVE WS-BTEC-CODE TO WS-DOC-AWARD-TITLE.
	MOVE S-AWARD-CODE TO WS-DOC-AWARD-CODE.

	PERFORM CZ-GET-DOC-REF.

	MOVE WS-DOC-REF TO S-DOC-REF.

	IF S-DOC-REF NOT = SPACES
	THEN
	   RELEASE SORT-REC
	END-IF.

	MOVE SPACES TO SORT-REC.
	GO TO CA-EXIT.
CA-030.
	MOVE 'Y' TO WS-END.
	GO TO CA-EXIT.
CA-060.
	MOVE 'STUDENT SELECT FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
CA-EXIT.
	EXIT.
*
CB-FETCH-STUDENTS SECTION.
********************************************************************************
*    THIS SECTION OBTAINS THE REQUIRED STUDENT DETAILS FROM THE RANGES         *
*    SPECIFIED IN TABLE ARC_REPRINT_RANGE.                                     *
********************************************************************************
CB-START.
*
*	RSH 28/11/2007 (LQ35232): Initialise the first and last fee record id.
*
	INITIALIZE  	WS-REG-NO,
 			WS-STUDENT-NAME,
			WS-CENTRE-NO,
			WS-COURSE-NO,
			WS-COMB,
                        WS-CERT-NO,
			WS-AWARD-CLAIM,
			WS-MONTH-YEAR,
			WS-AWARD-DATE,
			WS-ELIG,
			WS-REG-TYPE,
                        WS-OVER-GRADE,
                        WS-FALLBACK,
			WS-BIRTH-DATE,
			WS-INST-LOCATION,
                        WS-COA-SYLLABUS-CODE,
                        WS-COA-LEVEL-ACHIEVED,
                        WS-COA-LEVEL-ACHIEVED-DESCR,
			WS-RECON-IND,
			WS-KSQ,
			WS-CERT-ISSUE-DATE,
			WS-NOPS-REQD,
			WS-NOPS-REQD-I,
			WS-NVQ-ROA-CODE,
			WS-NVQ-ROA-CODE-I,
			WS-GNVQ-CUC-CODE,
			WS-GNVQ-CUC-CODE-I,
			WS-CRED-TRANS-CODE,
			WS-CRED-TRANS-CODE-I,
			WS-ABS-LEVEL-ACHIEVED,
			WS-ABS-LEVEL-ACHIEVED-I,
			WS-ESOL-LEVEL-ACHIEVED,
			WS-ESOL-LEVEL-ACHIEVED-I,
			WS-CERT-FLAG,
			WS-BTEC-CODE,
			WS-BTEC-LEVEL,
                        WS-REPRINT-WORDING,
			WS-FIRST-FEE-ID,
			WS-LAST-FEE-ID,
                        WS-DSA-SRU-NUMBER,
			WS-OLD-REPRINT-CERT,
                        WS-CLTE-TEXT.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO CB-060   END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE       END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO CB-030   END-EXEC.
*
*	RSH 08/08/2005 (RfW 04/0109):
*	Additionally fetch the ESOL level.
*
*	RSH 28/11/2007 (LQ35232): Fetch the first and last fee record id.
*
*       EXEC SQL FETCH CURSOR_7 INTO	:WS-REG-NO,
*       				:WS-STUDENT-NAME,
*       				:WS-CENTRE-NO,
*       				:WS-COURSE-NO,
*       				:WS-COMB,
*                                       :WS-CERT-NO,
*       				:WS-AWARD-CLAIM,
*       		  		:WS-MONTH-YEAR,
*       				:WS-AWARD-DATE,
*       				:WS-ELIG,
*       				:WS-REG-TYPE,
*                                       :WS-OVER-GRADE,
*                                       :WS-FALLBACK,
*       				:WS-BIRTH-DATE,
*       				:WS-INST-LOCATION,
*                                       :WS-COA-SYLLABUS-CODE,
*                                       :WS-COA-LEVEL-ACHIEVED,
*                                       :WS-COA-LEVEL-ACHIEVED-DESCR,
*       				:WS-RECON-IND,
*       				:WS-KSQ,
*       				:WS-CERT-ISSUE-DATE,
*       				:WS-NOPS-REQD:WS-NOPS-REQD-I,
*       				:WS-NVQ-ROA-CODE:WS-NVQ-ROA-CODE-I,
*       				:WS-GNVQ-CUC-CODE:WS-GNVQ-CUC-CODE-I,
*       			    :WS-CRED-TRANS-CODE:WS-CRED-TRANS-CODE-I,
*       			:WS-ABS-LEVEL-ACHIEVED:WS-ABS-LEVEL-ACHIEVED-I,
*       		      :WS-ESOL-LEVEL-ACHIEVED:WS-ESOL-LEVEL-ACHIEVED-I,
*       				:WS-CERT-FLAG,
*       				:WS-BTEC-CODE,
*       				:WS-BTEC-LEVEL,
*                                       :WS-REPRINT-WORDING,
*       				:WS-FIRST-FEE-ID,
*       				:WS-LAST-FEE-ID,
*                                       :WS-DSA-SRU-NUMBER,
*       				:WS-OLD-REPRINT-CERT,
*                                       :WS-CLTE-TEXT
*       END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 552 TO SQL-OFFSET
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
            WS-REG-NO
            SQL-SQHSTV(1)
        MOVE 7 TO SQL-SQHSTL(1)
        MOVE 7 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-STUDENT-NAME
            SQL-SQHSTV(2)
        MOVE 51 TO SQL-SQHSTL(2)
        MOVE 51 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-CENTRE-NO
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(4)
        MOVE 8 TO SQL-SQHSTL(4)
        MOVE 8 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-COMB
            SQL-SQHSTV(5)
        MOVE 1 TO SQL-SQHSTL(5)
        MOVE 1 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-CERT-NO
            SQL-SQHSTV(6)
        MOVE 5 TO SQL-SQHSTL(6)
        MOVE 5 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-AWARD-CLAIM
            SQL-SQHSTV(7)
        MOVE 1 TO SQL-SQHSTL(7)
        MOVE 1 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-MONTH-YEAR
            SQL-SQHSTV(8)
        MOVE 18 TO SQL-SQHSTL(8)
        MOVE 18 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            WS-AWARD-DATE
            SQL-SQHSTV(9)
        MOVE 11 TO SQL-SQHSTL(9)
        MOVE 11 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            WS-ELIG
            SQL-SQHSTV(10)
        MOVE 1 TO SQL-SQHSTL(10)
        MOVE 1 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            WS-REG-TYPE
            SQL-SQHSTV(11)
        MOVE 1 TO SQL-SQHSTL(11)
        MOVE 1 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            WS-OVER-GRADE
            SQL-SQHSTV(12)
        MOVE 3 TO SQL-SQHSTL(12)
        MOVE 3 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            WS-FALLBACK
            SQL-SQHSTV(13)
        MOVE 1 TO SQL-SQHSTL(13)
        MOVE 1 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            WS-BIRTH-DATE
            SQL-SQHSTV(14)
        MOVE 8 TO SQL-SQHSTL(14)
        MOVE 8 TO SQL-SQHSTS(14)
        MOVE 0 TO SQL-SQINDV(14)
        MOVE 0 TO SQL-SQINDS(14)
        MOVE 0 TO SQL-SQHARM(14)
        CALL "SQLADR" USING
            WS-INST-LOCATION
            SQL-SQHSTV(15)
        MOVE 6 TO SQL-SQHSTL(15)
        MOVE 6 TO SQL-SQHSTS(15)
        MOVE 0 TO SQL-SQINDV(15)
        MOVE 0 TO SQL-SQINDS(15)
        MOVE 0 TO SQL-SQHARM(15)
        CALL "SQLADR" USING
            WS-COA-SYLLABUS-CODE
            SQL-SQHSTV(16)
        MOVE 6 TO SQL-SQHSTL(16)
        MOVE 6 TO SQL-SQHSTS(16)
        MOVE 0 TO SQL-SQINDV(16)
        MOVE 0 TO SQL-SQINDS(16)
        MOVE 0 TO SQL-SQHARM(16)
        CALL "SQLADR" USING
            WS-COA-LEVEL-ACHIEVED
            SQL-SQHSTV(17)
        MOVE 1 TO SQL-SQHSTL(17)
        MOVE 1 TO SQL-SQHSTS(17)
        MOVE 0 TO SQL-SQINDV(17)
        MOVE 0 TO SQL-SQINDS(17)
        MOVE 0 TO SQL-SQHARM(17)
        CALL "SQLADR" USING
            WS-COA-LEVEL-ACHIEVED-DESCR
            SQL-SQHSTV(18)
        MOVE 30 TO SQL-SQHSTL(18)
        MOVE 30 TO SQL-SQHSTS(18)
        MOVE 0 TO SQL-SQINDV(18)
        MOVE 0 TO SQL-SQINDS(18)
        MOVE 0 TO SQL-SQHARM(18)
        CALL "SQLADR" USING
            WS-RECON-IND
            SQL-SQHSTV(19)
        MOVE 1 TO SQL-SQHSTL(19)
        MOVE 1 TO SQL-SQHSTS(19)
        MOVE 0 TO SQL-SQINDV(19)
        MOVE 0 TO SQL-SQINDS(19)
        MOVE 0 TO SQL-SQHARM(19)
        CALL "SQLADR" USING
            WS-KSQ
            SQL-SQHSTV(20)
        MOVE 1 TO SQL-SQHSTL(20)
        MOVE 1 TO SQL-SQHSTS(20)
        MOVE 0 TO SQL-SQINDV(20)
        MOVE 0 TO SQL-SQINDS(20)
        MOVE 0 TO SQL-SQHARM(20)
        CALL "SQLADR" USING
            WS-CERT-ISSUE-DATE
            SQL-SQHSTV(21)
        MOVE 11 TO SQL-SQHSTL(21)
        MOVE 11 TO SQL-SQHSTS(21)
        MOVE 0 TO SQL-SQINDV(21)
        MOVE 0 TO SQL-SQINDS(21)
        MOVE 0 TO SQL-SQHARM(21)
        CALL "SQLADR" USING
            WS-NOPS-REQD
            SQL-SQHSTV(22)
        MOVE 1 TO SQL-SQHSTL(22)
        MOVE 1 TO SQL-SQHSTS(22)
        CALL "SQLADR" USING
            WS-NOPS-REQD-I
            SQL-SQINDV(22)
        MOVE 0 TO SQL-SQINDS(22)
        MOVE 0 TO SQL-SQHARM(22)
        CALL "SQLADR" USING
            WS-NVQ-ROA-CODE
            SQL-SQHSTV(23)
        MOVE 1 TO SQL-SQHSTL(23)
        MOVE 1 TO SQL-SQHSTS(23)
        CALL "SQLADR" USING
            WS-NVQ-ROA-CODE-I
            SQL-SQINDV(23)
        MOVE 0 TO SQL-SQINDS(23)
        MOVE 0 TO SQL-SQHARM(23)
        CALL "SQLADR" USING
            WS-GNVQ-CUC-CODE
            SQL-SQHSTV(24)
        MOVE 1 TO SQL-SQHSTL(24)
        MOVE 1 TO SQL-SQHSTS(24)
        CALL "SQLADR" USING
            WS-GNVQ-CUC-CODE-I
            SQL-SQINDV(24)
        MOVE 0 TO SQL-SQINDS(24)
        MOVE 0 TO SQL-SQHARM(24)
        CALL "SQLADR" USING
            WS-CRED-TRANS-CODE
            SQL-SQHSTV(25)
        MOVE 1 TO SQL-SQHSTL(25)
        MOVE 1 TO SQL-SQHSTS(25)
        CALL "SQLADR" USING
            WS-CRED-TRANS-CODE-I
            SQL-SQINDV(25)
        MOVE 0 TO SQL-SQINDS(25)
        MOVE 0 TO SQL-SQHARM(25)
        CALL "SQLADR" USING
            WS-ABS-LEVEL-ACHIEVED
            SQL-SQHSTV(26)
        MOVE 1 TO SQL-SQHSTL(26)
        MOVE 1 TO SQL-SQHSTS(26)
        CALL "SQLADR" USING
            WS-ABS-LEVEL-ACHIEVED-I
            SQL-SQINDV(26)
        MOVE 0 TO SQL-SQINDS(26)
        MOVE 0 TO SQL-SQHARM(26)
        CALL "SQLADR" USING
            WS-ESOL-LEVEL-ACHIEVED
            SQL-SQHSTV(27)
        MOVE 2 TO SQL-SQHSTL(27)
        MOVE 2 TO SQL-SQHSTS(27)
        CALL "SQLADR" USING
            WS-ESOL-LEVEL-ACHIEVED-I
            SQL-SQINDV(27)
        MOVE 0 TO SQL-SQINDS(27)
        MOVE 0 TO SQL-SQHARM(27)
        CALL "SQLADR" USING
            WS-CERT-FLAG
            SQL-SQHSTV(28)
        MOVE 2 TO SQL-SQHSTL(28)
        MOVE 2 TO SQL-SQHSTS(28)
        MOVE 0 TO SQL-SQINDV(28)
        MOVE 0 TO SQL-SQINDS(28)
        MOVE 0 TO SQL-SQHARM(28)
        CALL "SQLADR" USING
            WS-BTEC-CODE
            SQL-SQHSTV(29)
        MOVE 4 TO SQL-SQHSTL(29)
        MOVE 4 TO SQL-SQHSTS(29)
        MOVE 0 TO SQL-SQINDV(29)
        MOVE 0 TO SQL-SQINDS(29)
        MOVE 0 TO SQL-SQHARM(29)
        CALL "SQLADR" USING
            WS-BTEC-LEVEL
            SQL-SQHSTV(30)
        MOVE 2 TO SQL-SQHSTL(30)
        MOVE 2 TO SQL-SQHSTS(30)
        MOVE 0 TO SQL-SQINDV(30)
        MOVE 0 TO SQL-SQINDS(30)
        MOVE 0 TO SQL-SQHARM(30)
        CALL "SQLADR" USING
            WS-REPRINT-WORDING
            SQL-SQHSTV(31)
        MOVE 13 TO SQL-SQHSTL(31)
        MOVE 13 TO SQL-SQHSTS(31)
        MOVE 0 TO SQL-SQINDV(31)
        MOVE 0 TO SQL-SQINDS(31)
        MOVE 0 TO SQL-SQHARM(31)
        CALL "SQLADR" USING
            WS-FIRST-FEE-ID
            SQL-SQHSTV(32)
        MOVE 4 TO SQL-SQHSTL(32)
        MOVE 4 TO SQL-SQHSTS(32)
        MOVE 0 TO SQL-SQINDV(32)
        MOVE 0 TO SQL-SQINDS(32)
        MOVE 0 TO SQL-SQHARM(32)
        CALL "SQLADR" USING
            WS-LAST-FEE-ID
            SQL-SQHSTV(33)
        MOVE 4 TO SQL-SQHSTL(33)
        MOVE 4 TO SQL-SQHSTS(33)
        MOVE 0 TO SQL-SQINDV(33)
        MOVE 0 TO SQL-SQINDS(33)
        MOVE 0 TO SQL-SQHARM(33)
        CALL "SQLADR" USING
            WS-DSA-SRU-NUMBER
            SQL-SQHSTV(34)
        MOVE 12 TO SQL-SQHSTL(34)
        MOVE 12 TO SQL-SQHSTS(34)
        MOVE 0 TO SQL-SQINDV(34)
        MOVE 0 TO SQL-SQINDS(34)
        MOVE 0 TO SQL-SQHARM(34)
        CALL "SQLADR" USING
            WS-OLD-REPRINT-CERT
            SQL-SQHSTV(35)
        MOVE 1 TO SQL-SQHSTL(35)
        MOVE 1 TO SQL-SQHSTS(35)
        MOVE 0 TO SQL-SQINDV(35)
        MOVE 0 TO SQL-SQINDS(35)
        MOVE 0 TO SQL-SQHARM(35)
        CALL "SQLADR" USING
            WS-CLTE-TEXT
            SQL-SQHSTV(36)
        MOVE 70 TO SQL-SQHSTL(36)
        MOVE 70 TO SQL-SQHSTS(36)
        MOVE 0 TO SQL-SQINDV(36)
        MOVE 0 TO SQL-SQINDS(36)
        MOVE 0 TO SQL-SQHARM(36)
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
            THEN GO TO CB-030 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO CB-060 END-IF.
*
	ADD	+1 	     TO WS-B-TOT-STDTS-PROCESSED.
	MOVE WS-REG-NO       TO S-REG-NO.
	MOVE WS-STUDENT-NAME TO S-STUDENT-NAME.
	MOVE WS-CENTRE-NO    TO S-CENTRE-NO.
	MOVE WS-COURSE-NO    TO S-COURSE-NO.
	MOVE WS-COMB         TO S-COMB.
        MOVE WS-CERT-NO      TO S-CERT-NO.
	MOVE WS-AWARD-CLAIM  TO S-AWARD-CLAIM.
	MOVE WS-MONTH-YEAR   TO S-MONTH-YEAR.
        MOVE WS-AWARD-DATE   TO S-AWARD-DATE.
	MOVE WS-ELIG	     TO S-ELIG.
	MOVE WS-REG-TYPE     TO S-REG-TYPE.
        MOVE WS-OVER-GRADE   TO S-OVER-GRADE.
        MOVE WS-FALLBACK     TO S-FALLBACK.
        MOVE WS-BIRTH-DATE   TO S-BIRTH-DATE.
	MOVE WS-INST-LOCATION TO S-INST-LOCATION.
        MOVE WS-COA-SYLLABUS-CODE        TO S-COA-SYLLABUS-CODE.
        MOVE WS-COA-LEVEL-ACHIEVED       TO S-COA-LEVEL-ACHIEVED.
        MOVE WS-COA-LEVEL-ACHIEVED-DESCR TO S-COA-LEVEL-ACHIEVED-DESCR.
	MOVE WS-RECON-IND    TO S-RECON-IND.
	MOVE WS-KSQ	     TO S-KSQ.
	MOVE WS-CERT-ISSUE-DATE TO S-ISSUE-DATE.
        MOVE WS-DSA-SRU-NUMBER TO S-DSA-SRU-ID.
	MOVE WS-OLD-REPRINT-CERT TO S-OLD-REPRINT-CERT.

        MOVE WS-REPRINT-WORDING TO S-CERT-REPRINT-WORDING.

	IF WS-NOPS-REQD-I = -1
	  MOVE SPACES TO S-NOPS-REQD
	ELSE
	  MOVE WS-NOPS-REQD TO S-NOPS-REQD
	END-IF.
	IF WS-NVQ-ROA-CODE-I = -1
	  MOVE SPACES TO S-NVQ-ROA-CODE
	ELSE
	  MOVE WS-NVQ-ROA-CODE TO S-NVQ-ROA-CODE
	END-IF.
	IF WS-GNVQ-CUC-CODE-I = -1
	  MOVE SPACES TO S-GNVQ-CUC-CODE
	ELSE
	  MOVE WS-GNVQ-CUC-CODE TO S-GNVQ-CUC-CODE
	END-IF.
	IF WS-CRED-TRANS-CODE-I = -1
	  MOVE SPACES TO S-CRED-TRANS-CODE
	ELSE
	  MOVE WS-CRED-TRANS-CODE TO S-CRED-TRANS-CODE
	END-IF.
	IF WS-ABS-LEVEL-ACHIEVED-I = -1
	  MOVE SPACES TO S-ABS-LEVEL-ACHIEVED
	ELSE
	  MOVE WS-ABS-LEVEL-ACHIEVED TO S-ABS-LEVEL-ACHIEVED
	END-IF.
*
*	RSH 08/08/2005 (RfW 04/0109):
*	Additionally store the ESOL level.
*
	IF WS-ESOL-LEVEL-ACHIEVED-I = -1
	  MOVE SPACES TO S-ESOL-LEVEL-ACHIEVED
	ELSE
	  MOVE WS-ESOL-LEVEL-ACHIEVED TO S-ESOL-LEVEL-ACHIEVED
	END-IF.
	MOVE WS-CERT-FLAG    TO S-CERT-FLAG.
*EC2490
        MOVE WS-CLTE-TEXT    TO S-CERT-MSG-SURF.
	MOVE WS-BTEC-LEVEL TO S-AWARD-CODE.
*
*	RSH 28/11/2007 (LQ35232): Store the first and last fee record id.
*
        MOVE WS-FIRST-FEE-ID	TO S-FIRST-FEE-ID.
        MOVE WS-LAST-FEE-ID	TO S-LAST-FEE-ID.
CB-020.
	MOVE WS-BTEC-CODE TO WS-DOC-AWARD-TITLE.
	MOVE S-AWARD-CODE TO WS-DOC-AWARD-CODE.

	PERFORM CZ-GET-DOC-REF.

	MOVE WS-DOC-REF TO S-DOC-REF.

	IF S-DOC-REF NOT = SPACES
	THEN
	   RELEASE SORT-REC
	END-IF.

	MOVE SPACES TO SORT-REC.
	GO TO CB-EXIT.
CB-030.
	MOVE 'Y' TO WS-END.
	GO TO CB-EXIT.
CB-060.
	MOVE 'STUDENT SELECT FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
CB-EXIT.
	EXIT.
*
CB1-FETCH-STUDENTS SECTION.
********************************************************************************
*    THIS SECTION OBTAINS THE REQUIRED STUDENT DETAILS FROM THE RANGES         *
*    SPECIFIED IN TABLE ARC_REPRINT_RANGE FOR AWARD CODE 27 ONLY.              *
********************************************************************************
CB1-START.
*
*	RSH 28/11/2007 (LQ35232): Initialise the first and last fee record id.
*
	INITIALIZE  	WS-REG-NO,
 			WS-STUDENT-NAME,
			WS-CENTRE-NO,
			WS-COURSE-NO,
			WS-COMB,
                        WS-CERT-NO,
			WS-AWARD-CLAIM,
			WS-MONTH-YEAR,
			WS-AWARD-DATE,
			WS-ELIG,
			WS-REG-TYPE,
                        WS-OVER-GRADE,
                        WS-FALLBACK,
			WS-BIRTH-DATE,
			WS-INST-LOCATION,
                        WS-COA-SYLLABUS-CODE,
                        WS-COA-LEVEL-ACHIEVED,
                        WS-COA-LEVEL-ACHIEVED-DESCR,
			WS-RECON-IND,
			WS-KSQ,
			WS-CERT-ISSUE-DATE,
                        WS-REPRINT-WORDING,
			WS-FIRST-FEE-ID,
			WS-LAST-FEE-ID,
                        WS-DSA-SRU-NUMBER,
			WS-OLD-REPRINT-CERT.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO CB1-060   END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE       END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO CB1-030   END-EXEC.
*
*	RSH 28/11/2007 (LQ35232): Fetch the first and last fee record id.
*
*       EXEC SQL FETCH CURSOR_7a INTO	:WS-REG-NO,
*					:WS-STUDENT-NAME,
*					:WS-CENTRE-NO,
*					:WS-COURSE-NO,
*					:WS-COMB,
*                                       :WS-CERT-NO,
*					:WS-AWARD-CLAIM,
*			  		:WS-MONTH-YEAR,
*					:WS-AWARD-DATE,
*					:WS-ELIG,
*					:WS-REG-TYPE,
*                                       :WS-OVER-GRADE,
*                                       :WS-FALLBACK,
*					:WS-BIRTH-DATE,
*					:WS-INST-LOCATION,
*                                       :WS-COA-SYLLABUS-CODE,
*                                       :WS-COA-LEVEL-ACHIEVED,
*                                       :WS-COA-LEVEL-ACHIEVED-DESCR,
*					:WS-RECON-IND,
*					:WS-KSQ,
*       				:WS-CERT-ISSUE-DATE,
*                                       :WS-REPRINT-WORDING,
*       				:WS-FIRST-FEE-ID,
*       				:WS-LAST-FEE-ID,
*                                       :WS-DSA-SRU-NUMBER,
*       				:WS-OLD-REPRINT-CERT
*	END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 711 TO SQL-OFFSET
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
            WS-REG-NO
            SQL-SQHSTV(1)
        MOVE 7 TO SQL-SQHSTL(1)
        MOVE 7 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-STUDENT-NAME
            SQL-SQHSTV(2)
        MOVE 51 TO SQL-SQHSTL(2)
        MOVE 51 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-CENTRE-NO
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(4)
        MOVE 8 TO SQL-SQHSTL(4)
        MOVE 8 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-COMB
            SQL-SQHSTV(5)
        MOVE 1 TO SQL-SQHSTL(5)
        MOVE 1 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-CERT-NO
            SQL-SQHSTV(6)
        MOVE 5 TO SQL-SQHSTL(6)
        MOVE 5 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-AWARD-CLAIM
            SQL-SQHSTV(7)
        MOVE 1 TO SQL-SQHSTL(7)
        MOVE 1 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-MONTH-YEAR
            SQL-SQHSTV(8)
        MOVE 18 TO SQL-SQHSTL(8)
        MOVE 18 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            WS-AWARD-DATE
            SQL-SQHSTV(9)
        MOVE 11 TO SQL-SQHSTL(9)
        MOVE 11 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            WS-ELIG
            SQL-SQHSTV(10)
        MOVE 1 TO SQL-SQHSTL(10)
        MOVE 1 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            WS-REG-TYPE
            SQL-SQHSTV(11)
        MOVE 1 TO SQL-SQHSTL(11)
        MOVE 1 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
        CALL "SQLADR" USING
            WS-OVER-GRADE
            SQL-SQHSTV(12)
        MOVE 3 TO SQL-SQHSTL(12)
        MOVE 3 TO SQL-SQHSTS(12)
        MOVE 0 TO SQL-SQINDV(12)
        MOVE 0 TO SQL-SQINDS(12)
        MOVE 0 TO SQL-SQHARM(12)
        CALL "SQLADR" USING
            WS-FALLBACK
            SQL-SQHSTV(13)
        MOVE 1 TO SQL-SQHSTL(13)
        MOVE 1 TO SQL-SQHSTS(13)
        MOVE 0 TO SQL-SQINDV(13)
        MOVE 0 TO SQL-SQINDS(13)
        MOVE 0 TO SQL-SQHARM(13)
        CALL "SQLADR" USING
            WS-BIRTH-DATE
            SQL-SQHSTV(14)
        MOVE 8 TO SQL-SQHSTL(14)
        MOVE 8 TO SQL-SQHSTS(14)
        MOVE 0 TO SQL-SQINDV(14)
        MOVE 0 TO SQL-SQINDS(14)
        MOVE 0 TO SQL-SQHARM(14)
        CALL "SQLADR" USING
            WS-INST-LOCATION
            SQL-SQHSTV(15)
        MOVE 6 TO SQL-SQHSTL(15)
        MOVE 6 TO SQL-SQHSTS(15)
        MOVE 0 TO SQL-SQINDV(15)
        MOVE 0 TO SQL-SQINDS(15)
        MOVE 0 TO SQL-SQHARM(15)
        CALL "SQLADR" USING
            WS-COA-SYLLABUS-CODE
            SQL-SQHSTV(16)
        MOVE 6 TO SQL-SQHSTL(16)
        MOVE 6 TO SQL-SQHSTS(16)
        MOVE 0 TO SQL-SQINDV(16)
        MOVE 0 TO SQL-SQINDS(16)
        MOVE 0 TO SQL-SQHARM(16)
        CALL "SQLADR" USING
            WS-COA-LEVEL-ACHIEVED
            SQL-SQHSTV(17)
        MOVE 1 TO SQL-SQHSTL(17)
        MOVE 1 TO SQL-SQHSTS(17)
        MOVE 0 TO SQL-SQINDV(17)
        MOVE 0 TO SQL-SQINDS(17)
        MOVE 0 TO SQL-SQHARM(17)
        CALL "SQLADR" USING
            WS-COA-LEVEL-ACHIEVED-DESCR
            SQL-SQHSTV(18)
        MOVE 30 TO SQL-SQHSTL(18)
        MOVE 30 TO SQL-SQHSTS(18)
        MOVE 0 TO SQL-SQINDV(18)
        MOVE 0 TO SQL-SQINDS(18)
        MOVE 0 TO SQL-SQHARM(18)
        CALL "SQLADR" USING
            WS-RECON-IND
            SQL-SQHSTV(19)
        MOVE 1 TO SQL-SQHSTL(19)
        MOVE 1 TO SQL-SQHSTS(19)
        MOVE 0 TO SQL-SQINDV(19)
        MOVE 0 TO SQL-SQINDS(19)
        MOVE 0 TO SQL-SQHARM(19)
        CALL "SQLADR" USING
            WS-KSQ
            SQL-SQHSTV(20)
        MOVE 1 TO SQL-SQHSTL(20)
        MOVE 1 TO SQL-SQHSTS(20)
        MOVE 0 TO SQL-SQINDV(20)
        MOVE 0 TO SQL-SQINDS(20)
        MOVE 0 TO SQL-SQHARM(20)
        CALL "SQLADR" USING
            WS-CERT-ISSUE-DATE
            SQL-SQHSTV(21)
        MOVE 11 TO SQL-SQHSTL(21)
        MOVE 11 TO SQL-SQHSTS(21)
        MOVE 0 TO SQL-SQINDV(21)
        MOVE 0 TO SQL-SQINDS(21)
        MOVE 0 TO SQL-SQHARM(21)
        CALL "SQLADR" USING
            WS-REPRINT-WORDING
            SQL-SQHSTV(22)
        MOVE 13 TO SQL-SQHSTL(22)
        MOVE 13 TO SQL-SQHSTS(22)
        MOVE 0 TO SQL-SQINDV(22)
        MOVE 0 TO SQL-SQINDS(22)
        MOVE 0 TO SQL-SQHARM(22)
        CALL "SQLADR" USING
            WS-FIRST-FEE-ID
            SQL-SQHSTV(23)
        MOVE 4 TO SQL-SQHSTL(23)
        MOVE 4 TO SQL-SQHSTS(23)
        MOVE 0 TO SQL-SQINDV(23)
        MOVE 0 TO SQL-SQINDS(23)
        MOVE 0 TO SQL-SQHARM(23)
        CALL "SQLADR" USING
            WS-LAST-FEE-ID
            SQL-SQHSTV(24)
        MOVE 4 TO SQL-SQHSTL(24)
        MOVE 4 TO SQL-SQHSTS(24)
        MOVE 0 TO SQL-SQINDV(24)
        MOVE 0 TO SQL-SQINDS(24)
        MOVE 0 TO SQL-SQHARM(24)
        CALL "SQLADR" USING
            WS-DSA-SRU-NUMBER
            SQL-SQHSTV(25)
        MOVE 12 TO SQL-SQHSTL(25)
        MOVE 12 TO SQL-SQHSTS(25)
        MOVE 0 TO SQL-SQINDV(25)
        MOVE 0 TO SQL-SQINDS(25)
        MOVE 0 TO SQL-SQHARM(25)
        CALL "SQLADR" USING
            WS-OLD-REPRINT-CERT
            SQL-SQHSTV(26)
        MOVE 1 TO SQL-SQHSTL(26)
        MOVE 1 TO SQL-SQHSTS(26)
        MOVE 0 TO SQL-SQINDV(26)
        MOVE 0 TO SQL-SQINDS(26)
        MOVE 0 TO SQL-SQHARM(26)
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
            THEN GO TO CB1-030 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO CB1-060 END-IF.
*
	ADD	+1 	     TO WS-B-TOT-STDTS-PROCESSED.
	MOVE WS-REG-NO       TO S-REG-NO.
	MOVE WS-STUDENT-NAME TO S-STUDENT-NAME.
	MOVE WS-CENTRE-NO    TO S-CENTRE-NO.
	MOVE WS-COURSE-NO    TO S-COURSE-NO.
	MOVE WS-COMB         TO S-COMB.
        MOVE WS-CERT-NO      TO S-CERT-NO.
	MOVE WS-AWARD-CLAIM  TO S-AWARD-CLAIM.
	MOVE WS-MONTH-YEAR   TO S-MONTH-YEAR.
        MOVE WS-AWARD-DATE   TO S-AWARD-DATE.
	MOVE WS-ELIG	     TO S-ELIG.
	MOVE WS-REG-TYPE     TO S-REG-TYPE.
        MOVE WS-OVER-GRADE   TO S-OVER-GRADE.
        MOVE WS-FALLBACK     TO S-FALLBACK.
        MOVE WS-BIRTH-DATE   TO S-BIRTH-DATE.
	MOVE WS-INST-LOCATION TO S-INST-LOCATION.
        MOVE WS-COA-SYLLABUS-CODE        TO S-COA-SYLLABUS-CODE.
        MOVE WS-COA-LEVEL-ACHIEVED       TO S-COA-LEVEL-ACHIEVED.
        MOVE WS-COA-LEVEL-ACHIEVED-DESCR TO S-COA-LEVEL-ACHIEVED-DESCR.
	MOVE WS-RECON-IND    TO S-RECON-IND.
	MOVE WS-KSQ	     TO S-KSQ.
	MOVE WS-CERT-ISSUE-DATE TO S-ISSUE-DATE.
	MOVE WS-DSA-SRU-NUMBER TO S-DSA-SRU-ID.
	MOVE WS-OLD-REPRINT-CERT TO S-OLD-REPRINT-CERT.
        MOVE '27'               TO S-AWARD-CODE.

        MOVE WS-REPRINT-WORDING TO S-CERT-REPRINT-WORDING.
*
*	RSH 28/11/2007 (LQ35232): Store the first and last fee record id.
*
        MOVE WS-FIRST-FEE-ID	TO S-FIRST-FEE-ID.
        MOVE WS-LAST-FEE-ID	TO S-LAST-FEE-ID.

CB1-020.
	MOVE ZEROES        TO WS-DOC-AWARD-TITLE.
	MOVE S-AWARD-CODE  TO WS-DOC-AWARD-CODE.

	PERFORM CZ-GET-DOC-REF.

	MOVE WS-DOC-REF TO S-DOC-REF.

	IF S-DOC-REF NOT = SPACES
	THEN
	   RELEASE SORT-REC
	END-IF.

	MOVE SPACES TO SORT-REC.
	GO TO CB1-EXIT.
CB1-030.
	MOVE 'Y' TO WS-END.
	GO TO CB1-EXIT.
CB1-060.
	MOVE 'STUDENT SELECT FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
CB1-EXIT.
	EXIT.
*
/
CZ-GET-DOC-REF SECTION.
********************************************************************************
*    THIS SECTION GETS THE DOCUMENT REFERENCE FOR THE AWARD CODE.              *
********************************************************************************
CZ-START.
	MOVE SPACES TO WS-DOC-AWARD-TYPE.

	IF WS-DOC-AWARD-TITLE = ZEROES
	THEN
	   MOVE 'B' TO WS-DOC-AWARD-TYPE
	   GO TO CZ-020
	END-IF.

*       EXEC SQL WHENEVER SQLERROR   GO TO CZ-060   END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO CZ-060   END-EXEC.

*       EXEC SQL
*		SELECT	DECODE(NVL(AT_INTERNATIONAL_IND,'N'),'Y','I','B')
*       	SELECT	'B'
*       	INTO	:WS-DOC-AWARD-TYPE
*       	FROM	BTEC.AWARD_TITLES
*       	WHERE	AT_NUMBER = :WS-DOC-AWARD-TITLE
*       END-EXEC.
        CALL "SQLADR" USING SQ0012 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 830 TO SQL-OFFSET
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
            WS-DOC-AWARD-TYPE
            SQL-SQHSTV(1)
        MOVE 1 TO SQL-SQHSTL(1)
        MOVE 1 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-DOC-AWARD-TITLE
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO CZ-060 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO CZ-060 END-IF.
	   
CZ-020.
	MOVE SPACES TO WS-DOC-REF.

*       EXEC SQL WHENEVER SQLERROR   GO TO CZ-080   END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO CZ-040   END-EXEC.

*       EXEC SQL
*       	SELECT	FBFI_DOC_REF
*       	INTO	:WS-DOC-REF
*       	FROM	FCS_BTEC_FILES
*       	WHERE	FBFI_AWARD_CODE = :WS-DOC-AWARD-CODE
*       	AND	FBFI_TYPE = :WS-DOC-AWARD-TYPE
*       	AND	FBFI_PRINT_IND = 'Y'
*       END-EXEC.
        CALL "SQLADR" USING SQ0013 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 853 TO SQL-OFFSET
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
            WS-DOC-REF
            SQL-SQHSTV(1)
        MOVE 5 TO SQL-SQHSTL(1)
        MOVE 5 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-DOC-AWARD-CODE
            SQL-SQHSTV(2)
        MOVE 2 TO SQL-SQHSTL(2)
        MOVE 2 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-DOC-AWARD-TYPE
            SQL-SQHSTV(3)
        MOVE 1 TO SQL-SQHSTL(3)
        MOVE 1 TO SQL-SQHSTS(3)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO CZ-040 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO CZ-080 END-IF.

	GO TO CZ-EXIT.
CZ-040.
	MOVE SPACES TO WS-DOC-REF.

	GO TO CZ-EXIT.
CZ-060.
	MOVE 'SELECT OF INTERNATIONAL INDICATOR FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
CZ-080.
	MOVE 'SELECT OF DOCUMENT REFERENCE FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
CZ-EXIT.
	EXIT.
/
CRI-GET-CUST-CERT SECTION.
********************************************************************************
*    THIS SECTION CHECKS FOR FLAG 'IRL' WHERE COUNTRY CODE IS 'IRL' AT SPECIFIC*
*    AT_NUMBER (48 PROGRAMMES).                                                *
********************************************************************************
CRI-START.
        MOVE 'N' TO WS-IRL-CC-FLAG.
        MOVE ZEROES TO WS-IRL-AT-NUMBER.

*       EXEC SQL WHENEVER SQLERROR   GO TO CRI-060   END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO CRI-040   END-EXEC.

*       EXEC SQL
*               SELECT AA_BTEC_TITLE
*               INTO :WS-IRL-AT-NUMBER
*               FROM AWARD_CODES,
*                    BTEC.APPROVAL_APPLICATION,
*                    BTEC.APPROVAL_AWARDS,
*                    BTEC.STUDENTS
*               WHERE  AC_CODE = AW_AWARD_CODE||''
*               AND  AC_EXTERNAL_CERT_NO IS NULL
*               AND  AW_COURSE_NUMBER = ST_COURSE_ID
*               AND  ST_COURSE_ID = :WS-IRL-COURSE-NO
*               AND  AW_APPLICAT_NO   = AA_APPLICAT_NO
*               AND  ST_REG_NO  = :WS-IRL-REG-NO
*       END-EXEC.
        CALL "SQLADR" USING SQ0014 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 880 TO SQL-OFFSET
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
            WS-IRL-AT-NUMBER
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-IRL-COURSE-NO
            SQL-SQHSTV(2)
        MOVE 6 TO SQL-SQHSTL(2)
        MOVE 6 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-IRL-REG-NO
            SQL-SQHSTV(3)
        MOVE 7 TO SQL-SQHSTL(3)
        MOVE 7 TO SQL-SQHSTS(3)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO CRI-040 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO CRI-060 END-IF.
*    
        IF WS-IRL-AT-NUMBER NOT= ZEROES
        THEN
              GO TO CRI-020
        ELSE
              GO TO CRI-040
        END-IF.

CRI-020.
 
        MOVE 'N' TO WS-IRL-CC-FLAG.
        
*       EXEC SQL WHENEVER SQLERROR   GO TO CRI-060   END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO CRI-040   END-EXEC.
        
*       EXEC SQL
*       	SELECT	NVL(DECODE(CCTE_ID,'IRL','Y','N'),'N')
*       	INTO	:WS-IRL-CC-FLAG
*       	FROM	BTEC.CUSTOMISED_CERT_TEXT
*               ,       BTEC.CENTRES
*               WHERE   CN_CENTRE_ID = :WS-IRL-CENTRE-NO
*               AND     CCTE_ID = CN_CIAL_ID
*               AND     CCTE_ID = 'IRL'
*       	AND	CCTE_TYPE = 'AT'
*               AND     CCTE_TYPE_VALUE = :WS-IRL-AT-NUMBER
*       END-EXEC.
        CALL "SQLADR" USING SQ0015 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 907 TO SQL-OFFSET
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
            WS-IRL-CC-FLAG
            SQL-SQHSTV(1)
        MOVE 1 TO SQL-SQHSTL(1)
        MOVE 1 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-IRL-CENTRE-NO
            SQL-SQHSTV(2)
        MOVE 6 TO SQL-SQHSTL(2)
        MOVE 6 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-IRL-AT-NUMBER
            SQL-SQHSTV(3)
        MOVE 4 TO SQL-SQHSTL(3)
        MOVE 4 TO SQL-SQHSTS(3)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO CRI-040 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO CRI-060 END-IF.
	   
        IF WS-IRL-CC-FLAG = SPACES
        THEN
             MOVE 'N' TO WS-IRL-CC-FLAG
        END-IF.

	GO TO CRI-EXIT.

CRI-040.
        MOVE 'N' TO WS-IRL-CC-FLAG.
        
	GO TO CRI-EXIT.
CRI-060.
	MOVE 'SELECT OF IRL FLAG FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
CRI-EXIT.
	EXIT.
/
D-OUTPUT-PROCEDURE SECTION.
********************************************************************************
*    THIS SECTION CONTROLS PRODUCTION OF THE AWARD REPORT. IF THERE ARE        *
*    NO STUDENTS TO BE PROCESSED, AN APPROPRIATE MESSAGE IS WRITTEN TO         *
*    THE REJECTS REPORT.                                                       *
********************************************************************************
D-START.
*       EXEC SQL  WHENEVER SQLERROR  GO TO D-800  END-EXEC.
*       EXEC SQL  WHENEVER NOT FOUND CONTINUE  END-EXEC.
*
*       EXEC SQL  DELETE FROM BTEC_REJECTED_STUDENTS  
*                 WHERE BRS_PROG_NO = 'STP070'
*       END-EXEC.
        CALL "SQLADR" USING SQ0016 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 934 TO SQL-OFFSET
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
            THEN GO TO D-800 END-IF.
*
        MOVE 0  TO WS-REJECT-REC-COUNT.
        MOVE 60 TO WS-REJECT-LINECOUNT.
        MOVE SPACES TO WS-REJECT-NO-TABLE,
                       WS-REJECT-LINE,
                       REJECT-REC.

	INITIALIZE WS-STDTS-COUNT-TAB,
		   WS-SCT-INDEX.

	RETURN SORT-FILE RECORD AT END GO TO D-900.
	MOVE SORT-KEY TO WS-SORT-KEY.
*
D-500.
	PERFORM D1-FOR-EACH-DOC-REF UNTIL WS-EOF.
	GO TO D-EXIT.
D-800.
        MOVE 'DELETE FROM BTEC_REJECTED_STUDENTS FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
D-900.
	MOVE 'NO STUDENTS REQUIRE CERTS PRINTED' TO REJECT-REC.
	WRITE REJECT-REC.
D-EXIT.
	EXIT.
*
/
D1-FOR-EACH-DOC-REF SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE PROCESSING FOR EACH CENTRE WITHIN THE           *
*    CURRENT DOCUMENT REFERENCE.                                               *
********************************************************************************
D1-START.
	MOVE ZERO TO WS-B-DRF-STDTS-PROCESSED.
	MOVE ZERO TO WS-B-DRF-STDTS-PASSED.
	MOVE ZERO TO WS-B-DRF-STDTS-REJECTED.

	MOVE SPACES TO WS-REJECT-NO(23).

	IF S-DOC-REF = SPACES
	THEN
	   MOVE 'Y' TO WS-REJECT-NO(23)

	   GO TO D1-040
	END-IF.

D1-020.
	MOVE S-DOC-REF TO WS-FILE-DOC-REF.

	IF WS-FDR-CHAR-5 = SPACE
	   MOVE WS-FDR-CHAR-4 TO WS-FDR-CHAR-5
	   MOVE ZERO	      TO WS-FDR-CHAR-4
	END-IF.

	INSPECT WS-FILE-DOC-REF
		REPLACING ALL SPACE BY '_'.

	STRING	WS-DIRNAME		DELIMITED BY SPACE
		':ST'			DELIMITED BY SIZE
		WS-FILE-DOC-REF		DELIMITED BY SPACE
		WS-FILE-RUN-TYPE	DELIMITED BY SPACE
		'.'			DELIMITED BY SIZE
		WS-FORMAT-TYPE		DELIMITED BY SPACE
		WS-BATCH-NUMBER		DELIMITED BY SPACE
	INTO	WS-CERT-FILE.

	MOVE SPACES TO WS-RD-CERT-FILE.

	STRING	'ST'			DELIMITED BY SIZE
		WS-FILE-DOC-REF		DELIMITED BY SPACE
		WS-FILE-RUN-TYPE	DELIMITED BY SPACE
		'.'			DELIMITED BY SIZE
		WS-FORMAT-TYPE		DELIMITED BY SPACE
		WS-BATCH-NUMBER		DELIMITED BY SPACE
	INTO	WS-RD-CERT-FILE.
		
	OPEN OUTPUT PRINT-FILE.

	PERFORM D1A-HEADER-DETAILS.

D1-040.

	PERFORM DA-FOR-EACH-CENTRE
		UNTIL WS-EOF
		OR    S-NEW-DOC-REF NOT = WS-OLD-DOC-REF.

	IF WS-REJECT-NO(23) = 'Y'
	THEN
	   GO TO D1-060
	END-IF.

	PERFORM D1B-TRAILER-DETAILS.

	CLOSE PRINT-FILE.

	PERFORM D1C-DRF-RUN-DETAILS.

	MOVE SPACES TO WS-CERT-FILE.
D1-060.
	MOVE 'N' TO WS-SCT-FOUND.

	PERFORM VARYING WS-SCT-SUB FROM 1 BY 1
		UNTIL WS-SCT-FOUND = 'Y'
		OR    WS-SCT-SUB > WS-SCT-INDEX

		IF WS-SCT-DOC-REF(WS-SCT-SUB) = WS-NEW-DOC-REF AND
		   WS-SCT-AWARD-CODE(WS-SCT-SUB) = SPACES
		THEN
		   MOVE WS-SCT-SUB TO WS-SCT-UPDATE
		   MOVE 'Y'        TO WS-SCT-FOUND
		END-IF

	END-PERFORM.          

D1-080.

	IF WS-SCT-FOUND = 'Y'
	THEN
	   ADD WS-B-DRF-STDTS-PROCESSED TO WS-SCT-PROCESSED(WS-SCT-UPDATE)
	   ADD WS-B-DRF-STDTS-PASSED    TO WS-SCT-PASSED(WS-SCT-UPDATE)
	   ADD WS-B-DRF-STDTS-REJECTED  TO WS-SCT-REJECTED(WS-SCT-UPDATE)
	ELSE
	   IF WS-SCT-INDEX < WS-SCT-INDEX-MAX
	   THEN
	      ADD 1 TO WS-SCT-INDEX

	      MOVE WS-NEW-DOC-REF           TO WS-SCT-DOC-REF(WS-SCT-INDEX)
	      MOVE SPACES                   TO WS-SCT-AWARD-CODE(WS-SCT-INDEX)
	      MOVE WS-B-DRF-STDTS-PROCESSED TO WS-SCT-PROCESSED(WS-SCT-INDEX)
	      MOVE WS-B-DRF-STDTS-PASSED    TO WS-SCT-PASSED(WS-SCT-INDEX)
	      MOVE WS-B-DRF-STDTS-REJECTED  TO WS-SCT-REJECTED(WS-SCT-INDEX)
	   END-IF
	END-IF.

	MOVE S-DOC-REF TO WS-NEW-DOC-REF.
D1-EXIT.
	EXIT.
/
D1A-HEADER-DETAILS SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE WRITING OF THE HEADER RECORDS TO THE            *
*    CERTIFICATES FILE.                                                        *
********************************************************************************
D1A-START.

	INITIALIZE WS-FCS-FILE-TAB,
		   WS-FFT-INDEX,
		   WS-FFT-FETCHED.

	MOVE S-DOC-REF TO WS-DOC-REF.

*       EXEC SQL WHENEVER SQLERROR  GO TO D1A-100 END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND CONTINUE      END-EXEC.
*
*	RSH 12/05/2005:
*	Additionally select the three-digit FCS award reference.
*
*       EXEC SQL
*       	SELECT	FBFI_AWARD_CODE,
*       		AC_DESCRIPTION || ' (' || FBFI_AWARD_CODE ||')',
*       		FBFI_CERT_TEXT_1,
*       		FBFI_CERT_TEXT_2,
*       		FBFI_CERT_TEXT_3,
*       		TO_CHAR(SYSDATE,'DD-MON-YYYY:HH24:MI'),
*       		NVL(FBFI_FCS_AWARD_REF,
*       		    LPAD(FBFI_AWARD_CODE,3,'0')),
*       		FBFI_OLD_CERT_TEXT_1,
*       		FBFI_OLD_CERT_TEXT_2,
*       		FBFI_OLD_CERT_TEXT_3
*       	INTO	:WS-FFT-AWARD-CODE,
*       		:WS-FFT-AWARD-NAME,
*       		:WS-FFT-CERT-TEXT-1,
*       		:WS-FFT-CERT-TEXT-2,
*       		:WS-FFT-CERT-TEXT-3,
*       		:WS-FFT-DATE-TIME,
*       		:WS-FFT-FCS-AWARD-REF,
*       		:WS-FFT-OLD-CERT-TEXT-1,
*       		:WS-FFT-OLD-CERT-TEXT-2,
*       		:WS-FFT-OLD-CERT-TEXT-3
*       	FROM	AWARD_CODES,
*       		FCS_BTEC_FILES
*       	WHERE	FBFI_DOC_REF = :WS-DOC-REF
*       	AND	FBFI_PRINT_IND = 'Y'
*       	AND	AC_CODE(+) = FBFI_AWARD_CODE
*       	ORDER BY FBFI_AWARD_CODE
*       END-EXEC.
        CALL "SQLADR" USING SQ0017 SQL-STMT
        MOVE 900 TO SQL-ITERS
        MOVE 949 TO SQL-OFFSET
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
            WS-FFT-AWARD-CODE IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(1)
        MOVE 2 TO SQL-SQHSTL(1)
        MOVE 2 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-FFT-AWARD-NAME IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(2)
        MOVE 65 TO SQL-SQHSTL(2)
        MOVE 65 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-FFT-CERT-TEXT-1 IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(3)
        MOVE 30 TO SQL-SQHSTL(3)
        MOVE 30 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-FFT-CERT-TEXT-2 IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(4)
        MOVE 30 TO SQL-SQHSTL(4)
        MOVE 30 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-FFT-CERT-TEXT-3 IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(5)
        MOVE 30 TO SQL-SQHSTL(5)
        MOVE 30 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-FFT-DATE-TIME IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(6)
        MOVE 17 TO SQL-SQHSTL(6)
        MOVE 17 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-FFT-FCS-AWARD-REF IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(7)
        MOVE 3 TO SQL-SQHSTL(7)
        MOVE 3 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-FFT-OLD-CERT-TEXT-1 IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(8)
        MOVE 30 TO SQL-SQHSTL(8)
        MOVE 30 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            WS-FFT-OLD-CERT-TEXT-2 IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(9)
        MOVE 30 TO SQL-SQHSTL(9)
        MOVE 30 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            WS-FFT-OLD-CERT-TEXT-3 IN
            WS-FCS-FILE-TAB(1)
            SQL-SQHSTV(10)
        MOVE 30 TO SQL-SQHSTL(10)
        MOVE 30 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            WS-DOC-REF
            SQL-SQHSTV(11)
        MOVE 5 TO SQL-SQHSTL(11)
        MOVE 5 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
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
            THEN GO TO D1A-100 END-IF.

	MOVE SQLERRD(3) TO WS-FFT-FETCHED.
D1A-020.

	MOVE 'N' TO WS-OLD-HEADERS.
	PERFORM D1AA-WRITE-HEADERS
		VARYING WS-FFT-INDEX FROM 1 BY 1
		UNTIL	WS-FFT-INDEX > WS-FFT-FETCHED.

D1A-040.

	IF WS-SCT-INDEX < WS-SCT-INDEX-MAX
	THEN
	   ADD 1 TO WS-SCT-INDEX

	   MOVE WS-DOC-REF TO WS-SCT-DOC-REF(WS-SCT-INDEX)
	   MOVE SPACES     TO WS-SCT-AWARD-CODE(WS-SCT-INDEX)
	   MOVE ZERO       TO WS-SCT-PROCESSED(WS-SCT-INDEX)
	   MOVE ZERO       TO WS-SCT-PASSED(WS-SCT-INDEX)
	   MOVE ZERO       TO WS-SCT-REJECTED(WS-SCT-INDEX)
	END-IF.

D1A-060.

	IF	WS-IP-RUN-TYPE = 'S'
	THEN
		MOVE 'Y' TO WS-OLD-HEADERS
		PERFORM D1AA-WRITE-HEADERS
			VARYING WS-FFT-INDEX FROM 1 BY 1
			UNTIL	WS-FFT-INDEX > WS-FFT-FETCHED
	END-IF.

	GO TO D1A-EXIT.
D1A-100.
        MOVE 'SELECT OF DOC REF AWARD CODES FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
D1A-EXIT.
	EXIT.
/
D1AA-WRITE-HEADERS SECTION.
********************************************************************************
*    THIS SECTION WRITES THE HEADER RECORDS TO THE CERTIFICATES FILE.          *
********************************************************************************
D1AA-START.

	IF WS-FFT-INDEX   = 1   AND
	   WS-OLD-HEADERS = 'N'
	THEN
	   MOVE WS-FFT-DATE-TIME(WS-FFT-INDEX) TO WS-TIME-DATE

	   IF WS-FORMAT-TYPE = 'F'
	   THEN
	      MOVE WS-TIME-DATE TO WS-BAN-TIME
	      WRITE PRINT-REC FROM WS-BAN-LINE-1

	      MOVE WS-RUN-TYPE TO WS-BAN-TYPE
	      WRITE PRINT-REC FROM WS-BAN-LINE-2
	   END-IF
	END-IF.

	IF WS-FORMAT-TYPE = 'F'
	THEN
	   MOVE WS-FFT-AWARD-NAME(WS-FFT-INDEX) TO WS-BAN-DESCRIPTION
	   WRITE PRINT-REC FROM WS-BAN-LINE-3
	ELSE
	   MOVE SPACES TO WS-ANS-REC
	   MOVE 1      TO WS-POINTER
*
*	   RSH 12/05/2005:
*	   Use FCS Award Reference instead of '0'||Award Code.
*
	   IF WS-OLD-HEADERS = 'N'
	   THEN
	    STRING '01*!*'			  	DELIMITED BY SIZE 
		   WS-FFT-FCS-AWARD-REF(WS-FFT-INDEX)	DELIMITED BY SIZE 
	    INTO   WS-ANS-REC
	    WITH   POINTER WS-POINTER
	   ELSE
	    STRING '01*!*9'			  	DELIMITED BY SIZE 
		   WS-FFT-FCS-AWARD-REF(WS-FFT-INDEX)	DELIMITED BY SIZE 
	    INTO   WS-ANS-REC
	    WITH   POINTER WS-POINTER
	   END-IF

	   PERFORM T-GET-NEXT-POSITION

	   STRING '*!*'		DELIMITED BY SIZE
		  WS-TIME-DATE	DELIMITED BY SIZE
	   INTO   WS-ANS-REC
	   WITH	  POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   STRING '*!*'		DELIMITED BY SIZE
		  WS-RUN-TYPE	DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH	  POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   STRING '*!*'				  DELIMITED BY SIZE
		  WS-FFT-AWARD-NAME(WS-FFT-INDEX) DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH	  POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   IF WS-FFT-CERT-TEXT-1-I(WS-FFT-INDEX) = -1
	   THEN

	      STRING '*!*'	DELIMITED BY SIZE
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER

	   ELSE

	    IF WS-OLD-HEADERS = 'N'
	    THEN
	      STRING '*!*'			      DELIMITED BY SIZE
		     WS-FFT-CERT-TEXT-1(WS-FFT-INDEX) DELIMITED BY SIZE 
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER
	    ELSE
	      STRING '*!*'			      DELIMITED BY SIZE
		     WS-FFT-OLD-CERT-TEXT-1(WS-FFT-INDEX) DELIMITED BY SIZE 
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER
	    END-IF

	   END-IF

	   PERFORM T-GET-NEXT-POSITION

	   IF WS-FFT-CERT-TEXT-2-I(WS-FFT-INDEX) = -1
	   THEN

	      STRING '*!*'	DELIMITED BY SIZE
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER

	   ELSE

	    IF WS-OLD-HEADERS = 'N'
	    THEN
	      STRING '*!*'			      DELIMITED BY SIZE
		     WS-FFT-CERT-TEXT-2(WS-FFT-INDEX) DELIMITED BY SIZE 
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER
	    ELSE
	      STRING '*!*'			      DELIMITED BY SIZE
		     WS-FFT-OLD-CERT-TEXT-2(WS-FFT-INDEX) DELIMITED BY SIZE 
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER
	    END-IF

	   END-IF

	   PERFORM T-GET-NEXT-POSITION

	   IF WS-FFT-CERT-TEXT-3-I(WS-FFT-INDEX) = -1
	   THEN

	      STRING '*!*'	DELIMITED BY SIZE
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER

	   ELSE

	    IF WS-OLD-HEADERS = 'N'
	    THEN
	      STRING '*!*'			      DELIMITED BY SIZE
		     WS-FFT-CERT-TEXT-3(WS-FFT-INDEX) DELIMITED BY SIZE 
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER
	    ELSE
	      STRING '*!*'			      DELIMITED BY SIZE
		     WS-FFT-OLD-CERT-TEXT-3(WS-FFT-INDEX) DELIMITED BY SIZE 
	      INTO   WS-ANS-REC
	      WITH   POINTER WS-POINTER
	    END-IF

	   END-IF

	   PERFORM T-GET-NEXT-POSITION

	   STRING '*!*'		DELIMITED BY SIZE
		  WS-DOC-REF	DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH	  POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   COMPUTE WS-ANS-LENGTH = WS-POINTER - 1

	   WRITE PRINT-REC FROM WS-ANS-REC

	   MOVE 'H' 	TO WS-AROF-RECORD-TYPE
           MOVE SPACES	TO WS-AROF-CENTRE
           MOVE -1	TO WS-AROF-CENTRE-I
           MOVE SPACES  TO WS-AROF-REG-NO
           MOVE -1     	TO WS-AROF-REG-NO-I

	   PERFORM ZV-WRITE-TO-DB

	END-IF.

D1AA-020.

	IF WS-SCT-INDEX < WS-SCT-INDEX-MAX AND
	   WS-OLD-HEADERS = 'N'
	THEN
	   ADD 1 TO WS-SCT-INDEX

	   MOVE WS-DOC-REF		TO WS-SCT-DOC-REF(WS-SCT-INDEX)
	   MOVE WS-FFT-AWARD-CODE(WS-FFT-INDEX)
					TO WS-SCT-AWARD-CODE(WS-SCT-INDEX)
	   MOVE ZERO			TO WS-SCT-PROCESSED(WS-SCT-INDEX)
	   MOVE ZERO			TO WS-SCT-PASSED(WS-SCT-INDEX)
	   MOVE ZERO			TO WS-SCT-REJECTED(WS-SCT-INDEX)
	END-IF.

D1AA-EXIT.
	EXIT.
/
D1B-TRAILER-DETAILS SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE WRITING OF THE TRAILER RECORDS TO THE           *
*    CERTIFICATES FILE.                                                        *
********************************************************************************
D1B-START.
	MOVE SPACES TO PRINT-REC.
	MOVE SPACES TO WS-ANS-REC.
	MOVE 1      TO WS-POINTER.

	IF WS-FORMAT-TYPE = 'F'
	THEN
	   MOVE 'STP070 HAS SUCCESSFULLY COMPLETED' TO PRINT-REC
	   WRITE PRINT-REC AFTER PAGE
	ELSE

	   STRING '07*!*STP070 HAS SUCCESSFULLY COMPLETED' DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION
	END-IF.

	MOVE WS-NEW-DOC-REF TO WS-CNTRL-DOC-REF.

	IF WS-FORMAT-TYPE = 'F'
	THEN
	   MOVE WS-CONTROL-PRINT-LINE TO PRINT-REC
	   WRITE PRINT-REC AFTER 2
	ELSE

	   STRING '*!*CONTROL TOTALS FOR STDTS PROCESSED ON DOCUMENT REF '
					DELIMITED BY SIZE
		  WS-CNTRL-DOC-REF	DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION
	END-IF.

	MOVE 'TOTAL NUMBER OF STUDENTS ISSUED AWARDS = ' TO WS-STDTS-TYPE.
	MOVE WS-B-DRF-STDTS-PASSED                       TO WS-STDTS-COUNT.

	IF WS-FORMAT-TYPE = 'F'
	THEN
	   MOVE WS-STDTS-PROCESSED TO PRINT-REC
	   WRITE PRINT-REC AFTER 2
	ELSE

	   STRING '*!*'			DELIMITED BY SIZE
		  WS-STDTS-TYPE		DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   STRING WS-STDTS-COUNT	DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION
	END-IF.

	MOVE 'TOTAL NUMBER OF STUDENTS REJECTED      = ' TO WS-STDTS-TYPE.
	MOVE WS-B-DRF-STDTS-REJECTED                     TO WS-STDTS-COUNT.

	IF WS-FORMAT-TYPE = 'F'
	THEN
	   MOVE WS-STDTS-PROCESSED TO PRINT-REC
	   WRITE PRINT-REC
	ELSE

	   STRING '*!*'			DELIMITED BY SIZE
		  WS-STDTS-TYPE		DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   STRING WS-STDTS-COUNT	DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION
	END-IF.

	MOVE 'TOTAL NUMBER OF STUDENTS PROCESSED     = ' TO WS-STDTS-TYPE.
	MOVE WS-B-DRF-STDTS-PROCESSED                    TO WS-STDTS-COUNT.

	IF WS-FORMAT-TYPE = 'F'
	THEN
	   MOVE WS-STDTS-PROCESSED TO PRINT-REC
	   WRITE PRINT-REC
	ELSE

	   STRING '*!*'			DELIMITED BY SIZE
		  WS-STDTS-TYPE		DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   STRING WS-STDTS-COUNT	DELIMITED BY SIZE 
	   INTO   WS-ANS-REC
	   WITH   POINTER WS-POINTER

	   PERFORM T-GET-NEXT-POSITION

	   COMPUTE WS-ANS-LENGTH = WS-POINTER - 1

	   WRITE PRINT-REC FROM WS-ANS-REC

	   MOVE 80 TO WS-ANS-LENGTH

	   MOVE 'F' 	TO WS-AROF-RECORD-TYPE
           MOVE SPACES	TO WS-AROF-CENTRE
           MOVE -1	TO WS-AROF-CENTRE-I
           MOVE SPACES  TO WS-AROF-REG-NO
           MOVE -1     	TO WS-AROF-REG-NO-I

	   PERFORM ZV-WRITE-TO-DB

	END-IF.

D1B-EXIT.
	EXIT.
/
D1C-DRF-RUN-DETAILS SECTION.
********************************************************************************
*    THIS SECTION INSERTS A RUN_DETAILS RECORD FOR THE DOCUMENT REFERENCE.     *
********************************************************************************
D1C-START.

	CALL "LIB$GETJPI" USING BY REFERENCE WS-ITEM-CODE,OMITTED,OMITTED, 
				BY REFERENCE WS-OUT-VALUE.

	MOVE WS-OUT-VALUE TO WS-DRF-END-TIME.
	SUBTRACT WS-DRF-START-TIME FROM WS-OUT-VALUE GIVING WS-OUT-VALUE.
	MOVE WS-DRF-END-TIME TO WS-DRF-START-TIME.

	DIVIDE WS-OUT-VALUE BY 360000 GIVING WS-INTEGER REMAINDER WS-QUOTIENT.
	MOVE WS-INTEGER TO WS-HOURS.
	DIVIDE WS-QUOTIENT BY 6000 GIVING WS-MINS.
	MOVE WS-TIME-FORMAT TO WS-CPU-TIME.

	MOVE WS-B-DRF-STDTS-PASSED TO WS-CERT-ISSUED.
        MOVE 'Y' TO WS-RUN-FLAG.
D1C-020.

*       EXEC SQL WHENEVER SQLERROR GO TO D1C-100 END-EXEC.

*       EXEC SQL
*       	INSERT INTO BTEC.RUN_DETAILS
*       			(
*       			RD_NAME,
*       			RD_DATE,
*       			RD_TIME,
*       			RD_RUN_FLAG,
*       			RD_COMMENT
*       			)
*       	VALUES		(
*       			:WS-RD-CERT-FILE,
*       			TO_DATE(:WS-RUN-DATE,'DD-MON-YYYY'),
*       			:WS-CPU-TIME,
*       			:WS-RUN-FLAG,
*       			'NUMBER OF AWARDS ISSUED = '||
*       				TO_CHAR(:WS-CERT-ISSUED)
*       			)
*       END-EXEC.
        CALL "SQLADR" USING SQ0018 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1008 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-RD-CERT-FILE
            SQL-SQHSTV(1)
        MOVE 15 TO SQL-SQHSTL(1)
        MOVE 15 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RUN-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-CPU-TIME
            SQL-SQHSTV(3)
        MOVE 5 TO SQL-SQHSTL(3)
        MOVE 5 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-RUN-FLAG
            SQL-SQHSTV(4)
        MOVE 1 TO SQL-SQHSTL(4)
        MOVE 1 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-CERT-ISSUED
            SQL-SQHSTV(5)
        MOVE 5 TO SQL-SQHSTL(5)
        MOVE 5 TO SQL-SQHSTS(5)
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
            THEN GO TO D1C-100 END-IF.

	GO TO D1C-EXIT.

D1C-100.
	MOVE 'INSERT OF DRF RUN_DETAILS FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.                   
D1C-EXIT.
	EXIT.
/
DA-FOR-EACH-CENTRE SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE PROCESSING FOR EACH AWARD CODE WITHIN           *
*    THE CURRENT CENTRE.                                                       *
********************************************************************************
DA-START.
*
	MOVE ZERO TO  WS-B-CENTRE-STDTS-PASSED.
	MOVE HIGH-VALUE TO WS-CURRENT-AWARD-DATE.
*
	PERFORM VA-GET-CENTRE-LOGO.
*
	PERFORM DAA-FOR-EACH-AWARD-CODE UNTIL WS-EOF
                    		        OR    S-NEW-CENTRE NOT = WS-OLD-CENTRE.
*! 13-6-91 HS
*! Whilst most certificate printing is done externally all reprints
*! are done internally. For external prints, print a blank page at
*! the end of each centre
*!
	IF WS-IP-RUN-TYPE = 'A'
	  IF WS-ACCEPT-EXTERNAL-YN = "Y" 
	    IF WS-FORMAT-TYPE = 'F'
		IF   WS-B-CENTRE-STDTS-PASSED > ZERO
		     MOVE WS-BLANK-PAGE-MESSAGE TO PRINT-REC
		     WRITE PRINT-REC AFTER PAGE
		END-IF
            END-IF
	  END-IF
	END-IF.
*
	MOVE S-CENTRE-NO TO WS-NEW-CENTRE.
DA-EXIT.
	EXIT.
*
DAA-FOR-EACH-AWARD-CODE SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE PROCESSING FOR EACH COURSE WITHIN THE           *
*    CURRENT AWARD CODE.                                                       *
********************************************************************************
DAA-START.
	MOVE ZERO TO WS-B-AWC-STDTS-PROCESSED.
	MOVE ZERO TO WS-B-AWC-STDTS-PASSED.
	MOVE ZERO TO WS-B-AWC-STDTS-REJECTED.

	MOVE SPACES TO WS-REJECT-NO(24).
DAA-020.
	MOVE 'N' TO WS-CNT-FOUND.

	IF WS-IP-RUN-TYPE EQUAL TO 'S'
	THEN
	   GO TO DAA-040
	END-IF.

	PERFORM VARYING WS-CNT-INDEX FROM 1 BY 1
		UNTIL WS-CNT-FOUND = 'Y'
		OR    WS-CNT-INDEX > WS-CNT-FETCHED

		IF WS-CNT-AWARD-CODE(WS-CNT-INDEX) = S-AWARD-CODE
		THEN
		   MOVE WS-CNT-INDEX TO WS-CNT-UPDATE
		   MOVE 'Y'          TO WS-CNT-FOUND

		   MOVE WS-CNT-NEXT-CERT-NO(WS-CNT-INDEX) TO WS-ALLOC-CERT-NO
		END-IF

	END-PERFORM.          

	IF WS-CNT-FOUND = 'N'
	THEN
	   MOVE 'Y' TO WS-REJECT-NO(24)
	END-IF.

DAA-040.

	PERFORM DAB-FOR-EACH-COURSE
		UNTIL WS-EOF
		OR    S-NEW-AWARD-CODE NOT = WS-OLD-AWARD-CODE.

DAA-060.

	IF WS-CNT-FOUND = 'Y'
	THEN
	   MOVE WS-ALLOC-CERT-NO TO WS-CNT-NEXT-CERT-NO(WS-CNT-UPDATE)
	END-IF.

DAA-080.
	MOVE 'N' TO WS-SCT-FOUND.

	PERFORM VARYING WS-SCT-SUB FROM 1 BY 1
		UNTIL WS-SCT-FOUND = 'Y'
		OR    WS-SCT-SUB > WS-SCT-INDEX

		IF WS-SCT-DOC-REF(WS-SCT-SUB) = WS-NEW-DOC-REF AND
		   WS-SCT-AWARD-CODE(WS-SCT-SUB) = WS-NEW-AWARD-CODE
		THEN
		   MOVE WS-SCT-SUB TO WS-SCT-UPDATE
		   MOVE 'Y'        TO WS-SCT-FOUND
		END-IF

	END-PERFORM.          

DAA-100.

	IF WS-SCT-FOUND = 'Y'
	THEN
	   ADD WS-B-AWC-STDTS-PROCESSED TO WS-SCT-PROCESSED(WS-SCT-UPDATE)
	   ADD WS-B-AWC-STDTS-PASSED    TO WS-SCT-PASSED(WS-SCT-UPDATE)
	   ADD WS-B-AWC-STDTS-REJECTED  TO WS-SCT-REJECTED(WS-SCT-UPDATE)
	ELSE
	   IF WS-SCT-INDEX < WS-SCT-INDEX-MAX
	   THEN
	      ADD 1 TO WS-SCT-INDEX

	      MOVE WS-NEW-DOC-REF           TO WS-SCT-DOC-REF(WS-SCT-INDEX)
	      MOVE WS-NEW-AWARD-CODE        TO WS-SCT-AWARD-CODE(WS-SCT-INDEX)
	      MOVE WS-B-AWC-STDTS-PROCESSED TO WS-SCT-PROCESSED(WS-SCT-INDEX)
	      MOVE WS-B-AWC-STDTS-PASSED    TO WS-SCT-PASSED(WS-SCT-INDEX)
	      MOVE WS-B-AWC-STDTS-REJECTED  TO WS-SCT-REJECTED(WS-SCT-INDEX)
	   END-IF
	END-IF.

	MOVE S-AWARD-CODE TO WS-NEW-AWARD-CODE.
DAA-EXIT.
	EXIT.
/
DAB-FOR-EACH-COURSE SECTION.
********************************************************************************
*    THIS SECTION CONTROLS RETRIEVAL OF AWARD AND APPLICATION DETAILS.         *
*    IT ALSO, REGARDLESS OF THE COURSE NUMBER, CONTROLS THE PROCESSING         *
*    FOR EACH COMBINATION OF THE COURSE CURRENTLY BEING PROCESSED.             *
********************************************************************************
DAB-START.
*
        MOVE S-COURSE-NO TO WS-COURSE-NO.
        MOVE S-COA-SYLLABUS-CODE        TO WS-COA-SYLLABUS-CODE.

	MOVE ZERO TO  WS-B-COURSE-STDTS-PASSED.
*
        MOVE SPACES TO WS-REJECT-NO(2),
                       WS-REJECT-NO(3),
		       WS-REJECT-NO(4),
  		       WS-REJECT-NO(5),
                       WS-AWARD-CODE,
                       PL2-COURSE-NAME,
                       PL3-COURSE-NAME,
                       PL8-COURSE-NO,
		       PL8-BIRTH-DATE,
		       PL8-ISSUE-DATE.
*
	IF S-KSQ = 'Y'
		MOVE WS-COURSE-NO TO PL8-COURSE-NO
		MOVE 'THE KEY SKILLS QUALIFICATION' TO PL2-COURSE-NAME
		GO TO DAB-400
	END-IF.
	PERFORM ZP-GET-COURSE-LEVEL.
*
        IF WS-AWARD-CODE = SPACES
               MOVE 'Y' TO WS-REJECT-NO(2).
*
        MOVE WS-MAIN-BOARD  TO WS-CURRENT-BOARD.
*WI409 store course for IRL
        MOVE WS-COURSE-NO   TO WS-STORE-COURSE-NO,
                               PL8-COURSE-NO,
                               WS-IRL-COURSE-NO.

        IF WS-COA-SYLLABUS-CODE NOT EQUAL SPACES
        THEN
          MOVE WS-COA-SYLLABUS-CODE TO PL8-COURSE-NO
        ELSE
          MOVE WS-COURSE-NO TO PL8-COURSE-NO
        END-IF.

        MOVE WS-APPLICAT-NO TO WS-STORE-APPLICAT-NO.
        MOVE WS-AWARD-CODE  TO WS-STORE-AWARD-CODE.
	MOVE WS-AC-DESCR    TO PL2-KS-LEVEL.
	MOVE WS-BALE-DESC   TO PL1-BA-LEVEL.
*
* JOB 4776
*
	SET WS-TITLE-IND TO +1.

	SEARCH WS-TITLES-TABLE

	AT END 	
	PERFORM UAA-GET-TITLE
	WHEN WS-TITLE-CODE (WS-TITLE-IND) EQUAL WS-BTEC-TITLE
	MOVE WS-TITLE1 (WS-TITLE-IND) TO PL2-COURSE-NAME
	MOVE WS-TITLE2 (WS-TITLE-IND) TO PL3-COURSE-NAME.
*
DAB-400.
*
	PERFORM VC-GET-TRAIN-ORG-LOGO.

	PERFORM VE-GET-AWARD-TYPE-LOGO.
*
	PERFORM VF-GET-PBODY-LOGOS.

	PERFORM VG-GET-PBODIES.

	PERFORM VH-GET-CERT-VALIDITY.

	PERFORM DABB-FOR-EACH-COMB 
                UNTIL WS-EOF
  		OR    S-NEW-COURSE NOT = WS-OLD-COURSE-NO.
*
DAB-500.
	IF WS-IP-RUN-TYPE = 'A'
	AND WS-ACCEPT-EXTERNAL-YN = 'Y'
	AND WS-FORMAT-TYPE = 'F'
        AND WS-B-COURSE-STDTS-PASSED > 0 THEN
		WRITE PRINT-REC FROM WS-BLANK-PAGE-MESSAGE2 AFTER PAGE
	END-IF.
*
	MOVE S-COURSE-NO TO WS-NEW-COURSE.
DAB-EXIT.
	EXIT.
*
/
DABB-FOR-EACH-COMB SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE PROCESSING FOR EACH STUDENT ON THE              *
*    CURRENT COMBINATION.                                                      *
********************************************************************************
DABB-START.
	MOVE S-COMB TO WS-COMB.
 	PERFORM DABBD-FOR-EACH-STUDENT
		UNTIL WS-EOF
		OR    S-NEW-COMB NOT = WS-OLD-COMB.
	MOVE S-COMB TO WS-NEW-COMB.
        GO TO DABB-EXIT.
DABB-EXIT.
	EXIT.
*
/
DABBD-FOR-EACH-STUDENT SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE RETRIEVAL OF THE CENTRE NAME AND SETTING OF     *
*    HEADINGS ON THE CERTIFICATES, DECLARING/OPENING OF CURSOR_6, RETRIEVAL    *
*    AND PROCESS OF EACH STUDENT TRANSFER, AND CLOSING OF CURSOR_6.            *
********************************************************************************
DABBD-START.
	ADD 1 TO WS-B-DRF-STDTS-PROCESSED.
	ADD 1 TO WS-B-AWC-STDTS-PROCESSED.

	MOVE S-CENTRE-NO TO WS-CENTRE-NO.
	MOVE S-REG-NO TO WS-REG-NO.
        MOVE S-COA-LEVEL-ACHIEVED       TO WS-COA-LEVEL-ACHIEVED.
        MOVE S-COA-LEVEL-ACHIEVED-DESCR TO WS-COA-LEVEL-ACHIEVED-DESCR.
	MOVE S-RECON-IND TO WS-RECON-IND.
*
*
* For non-reprint runs check that the centre/course/student is not deferred.
*
	IF WS-IP-RUN-TYPE = 'A' THEN
*
	    MOVE +0 TO WS-BLOCK-STATUS-I
*
*           EXEC SQL EXECUTE
*            BEGIN
*             PK_PROVDEF.PR_CHECK_STUDENT_DEFERRAL
*                                         (:WS-REG-NO
*                                         ,:WS-CHECK-UNITS
*                                         ,:WS-BLOCK-STATUS:WS-BLOCK-STATUS-I
*                                         ,:WS-NPP-ID:WS-NPP-ID-I
*                                         ,:WS-DECISION-DATE:WS-DECISION-DATE-I
*                                         );
*            END;
*           END-EXEC
            CALL "SQLADR" USING SQ0019 SQL-STMT
            MOVE 1 TO SQL-ITERS
            MOVE 1043 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 4352 TO SQL-SQLETY
            CALL "SQLADR" USING
                WS-REG-NO
                SQL-SQHSTV(1)
            MOVE 7 TO SQL-SQHSTL(1)
            MOVE 7 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQINDS(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                WS-CHECK-UNITS
                SQL-SQHSTV(2)
            MOVE 1 TO SQL-SQHSTL(2)
            MOVE 1 TO SQL-SQHSTS(2)
            MOVE 0 TO SQL-SQINDV(2)
            MOVE 0 TO SQL-SQINDS(2)
            MOVE 0 TO SQL-SQHARM(2)
            CALL "SQLADR" USING
                WS-BLOCK-STATUS
                SQL-SQHSTV(3)
            MOVE 1 TO SQL-SQHSTL(3)
            MOVE 1 TO SQL-SQHSTS(3)
            CALL "SQLADR" USING
                WS-BLOCK-STATUS-I
                SQL-SQINDV(3)
            MOVE 0 TO SQL-SQINDS(3)
            MOVE 0 TO SQL-SQHARM(3)
            CALL "SQLADR" USING
                WS-NPP-ID
                SQL-SQHSTV(4)
            MOVE 4 TO SQL-SQHSTL(4)
            MOVE 4 TO SQL-SQHSTS(4)
            CALL "SQLADR" USING
                WS-NPP-ID-I
                SQL-SQINDV(4)
            MOVE 0 TO SQL-SQINDS(4)
            MOVE 0 TO SQL-SQHARM(4)
            CALL "SQLADR" USING
                WS-DECISION-DATE
                SQL-SQHSTV(5)
            MOVE 11 TO SQL-SQHSTL(5)
            MOVE 11 TO SQL-SQHSTS(5)
            CALL "SQLADR" USING
                WS-DECISION-DATE-I
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
                THEN GO TO D1C-100 END-IF
*
	    IF WS-BLOCK-STATUS-I EQUAL -1
	       MOVE '7' TO WS-BLOCK-STATUS
	    END-IF
*
	    IF 	WS-BLOCK-STATUS NOT = '0' THEN
*               EXEC SQL EXECUTE
*                BEGIN
*       	  PK_BNM.PR_AWARDS_RUN_BLOCK
*                                         (:WS-REG-NO
*                                         ,'STP070'
*                                         ,:WS-BLOCK-STATUS
*                                         ,:WS-CLAIM-TYPE
*                                         ,:WS-NPP-ID:WS-NPP-ID-I
*                                         ,:WS-DECISION-DATE:WS-DECISION-DATE-I
*                                         );
*                END;
* 	        END-EXEC
                CALL "SQLADR" USING SQ0020 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1078 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
                MOVE 0 TO SQL-SQINDV(1)
                MOVE 0 TO SQL-SQINDS(1)
                MOVE 0 TO SQL-SQHARM(1)
                CALL "SQLADR" USING
                    WS-BLOCK-STATUS
                    SQL-SQHSTV(2)
                MOVE 1 TO SQL-SQHSTL(2)
                MOVE 1 TO SQL-SQHSTS(2)
                MOVE 0 TO SQL-SQINDV(2)
                MOVE 0 TO SQL-SQINDS(2)
                MOVE 0 TO SQL-SQHARM(2)
                CALL "SQLADR" USING
                    WS-CLAIM-TYPE
                    SQL-SQHSTV(3)
                MOVE 1 TO SQL-SQHSTL(3)
                MOVE 1 TO SQL-SQHSTS(3)
                MOVE 0 TO SQL-SQINDV(3)
                MOVE 0 TO SQL-SQINDS(3)
                MOVE 0 TO SQL-SQHARM(3)
                CALL "SQLADR" USING
                    WS-NPP-ID
                    SQL-SQHSTV(4)
                MOVE 4 TO SQL-SQHSTL(4)
                MOVE 4 TO SQL-SQHSTS(4)
                CALL "SQLADR" USING
                    WS-NPP-ID-I
                    SQL-SQINDV(4)
                MOVE 0 TO SQL-SQINDS(4)
                MOVE 0 TO SQL-SQHARM(4)
                CALL "SQLADR" USING
                    WS-DECISION-DATE
                    SQL-SQHSTV(5)
                MOVE 11 TO SQL-SQHSTL(5)
                MOVE 11 TO SQL-SQHSTS(5)
                CALL "SQLADR" USING
                    WS-DECISION-DATE-I
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
                    THEN GO TO D1C-100 END-IF
		SUBTRACT 1 FROM WS-B-TOT-STDTS-PROCESSED
		SUBTRACT 1 FROM WS-B-DRF-STDTS-PROCESSED
		SUBTRACT 1 FROM WS-B-AWC-STDTS-PROCESSED
		GO TO DABBD-500
	    END-IF

	   IF	WS-AC-BNM-TYPE = 'F' OR
	     	WS-AC-BNM-TYPE = 'N' OR
	     	WS-AC-BNM-TYPE = 'H' OR
	     	WS-AC-BNM-TYPE = 'I' OR
	     	WS-AC-BNM-TYPE = '1' OR
	     	WS-AC-BNM-TYPE = 'Z' OR
	     	WS-AC-BNM-TYPE = 'S'       
	   THEN
                MOVE +0 TO WS-STATUS-I

*               EXEC SQL EXECUTE
*                BEGIN
*                 PK_BNM.PR_STUDENT_COURSE_BLOCK2
*                                         (:WS-REG-NO
*                                         ,:WS-STATUS:WS-STATUS-I
*                                         ,:WS-NPP-ID:WS-NPP-ID-I
*                                         ,:WS-DECISION-DATE:WS-DECISION-DATE-I
*                                         );
*                END;
*               END-EXEC
                CALL "SQLADR" USING SQ0021 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1113 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
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
                    WS-NPP-ID
                    SQL-SQHSTV(3)
                MOVE 4 TO SQL-SQHSTL(3)
                MOVE 4 TO SQL-SQHSTS(3)
                CALL "SQLADR" USING
                    WS-NPP-ID-I
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
                    THEN GO TO D1C-100 END-IF
                IF WS-STATUS-I EQUAL -1
                   MOVE '7' TO WS-STATUS
                END-IF
	    	IF 	WS-STATUS NOT = '0'
		THEN
*                       EXEC SQL EXECUTE
*               	 BEGIN
*   		          PK_BNM.PR_AWARDS_RUN_BLOCK
*                                         (:WS-REG-NO
*                                         ,'STP070'
*                                         ,:WS-STATUS
*                                         ,:WS-CLAIM-TYPE
*                                         ,:WS-NPP-ID:WS-NPP-ID-I
*                                         ,:WS-DECISION-DATE:WS-DECISION-DATE-I
*                                         );
*               	 END;
*             		END-EXEC
            CALL "SQLADR" USING SQ0022 SQL-STMT
            MOVE 1 TO SQL-ITERS
            MOVE 1144 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 4352 TO SQL-SQLETY
            CALL "SQLADR" USING
                WS-REG-NO
                SQL-SQHSTV(1)
            MOVE 7 TO SQL-SQHSTL(1)
            MOVE 7 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQINDS(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                WS-STATUS
                SQL-SQHSTV(2)
            MOVE 1 TO SQL-SQHSTL(2)
            MOVE 1 TO SQL-SQHSTS(2)
            MOVE 0 TO SQL-SQINDV(2)
            MOVE 0 TO SQL-SQINDS(2)
            MOVE 0 TO SQL-SQHARM(2)
            CALL "SQLADR" USING
                WS-CLAIM-TYPE
                SQL-SQHSTV(3)
            MOVE 1 TO SQL-SQHSTL(3)
            MOVE 1 TO SQL-SQHSTS(3)
            MOVE 0 TO SQL-SQINDV(3)
            MOVE 0 TO SQL-SQINDS(3)
            MOVE 0 TO SQL-SQHARM(3)
            CALL "SQLADR" USING
                WS-NPP-ID
                SQL-SQHSTV(4)
            MOVE 4 TO SQL-SQHSTL(4)
            MOVE 4 TO SQL-SQHSTS(4)
            CALL "SQLADR" USING
                WS-NPP-ID-I
                SQL-SQINDV(4)
            MOVE 0 TO SQL-SQINDS(4)
            MOVE 0 TO SQL-SQHARM(4)
            CALL "SQLADR" USING
                WS-DECISION-DATE
                SQL-SQHSTV(5)
            MOVE 11 TO SQL-SQHSTL(5)
            MOVE 11 TO SQL-SQHSTS(5)
            CALL "SQLADR" USING
                WS-DECISION-DATE-I
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
                THEN GO TO D1C-100 END-IF
			SUBTRACT 1 FROM WS-B-TOT-STDTS-PROCESSED
			SUBTRACT 1 FROM WS-B-DRF-STDTS-PROCESSED
			SUBTRACT 1 FROM WS-B-AWC-STDTS-PROCESSED
			GO TO DABBD-500
	    	END-IF
	   END-IF

           IF   WS-AC-BNM-TYPE = 'W' OR
                WS-AC-BNM-TYPE = 'T' OR
                WS-AC-BNM-TYPE = 'G' OR
                WS-AC-BNM-TYPE = 'C' 
           THEN

                MOVE +0 TO WS-STATUS-I

*               EXEC SQL EXECUTE
*                 BEGIN
*                   PK_OSCA1.PR_Q_CERT_STATUS
*                             (:WS-REG-NO,NULL,
*                              :WS-STATUS:WS-STATUS-I,
*                              :WS-STATUS2:WS-STATUS2-I,
*                              :WS-DECISION-DATE:WS-DECISION-DATE-I,
*                              :WS-MESSAGE:WS-MESSAGE-I);
*                 END;
*               END-EXEC
                CALL "SQLADR" USING SQ0023 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1179 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
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
                    THEN GO TO D1C-100 END-IF
                IF WS-STATUS-I EQUAL -1
                   MOVE '7' TO WS-STATUS
                END-IF
	    	IF 	WS-STATUS NOT = '0'
		THEN
*                       EXEC SQL EXECUTE
*               	 BEGIN
*   		          PK_BNM.PR_AWARDS_RUN_BLOCK
*                                         (:WS-REG-NO
*                                         ,'STP070'
*                                         ,:WS-STATUS
*                                         ,:WS-CLAIM-TYPE
*                                         ,null
*                                         ,:WS-DECISION-DATE:WS-DECISION-DATE-I
*                                         );
*               	 END;
*             		END-EXEC
            CALL "SQLADR" USING SQ0024 SQL-STMT
            MOVE 1 TO SQL-ITERS
            MOVE 1214 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 4352 TO SQL-SQLETY
            CALL "SQLADR" USING
                WS-REG-NO
                SQL-SQHSTV(1)
            MOVE 7 TO SQL-SQHSTL(1)
            MOVE 7 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQINDS(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                WS-STATUS
                SQL-SQHSTV(2)
            MOVE 1 TO SQL-SQHSTL(2)
            MOVE 1 TO SQL-SQHSTS(2)
            MOVE 0 TO SQL-SQINDV(2)
            MOVE 0 TO SQL-SQINDS(2)
            MOVE 0 TO SQL-SQHARM(2)
            CALL "SQLADR" USING
                WS-CLAIM-TYPE
                SQL-SQHSTV(3)
            MOVE 1 TO SQL-SQHSTL(3)
            MOVE 1 TO SQL-SQHSTS(3)
            MOVE 0 TO SQL-SQINDV(3)
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
                THEN GO TO D1C-100 END-IF
			SUBTRACT 1 FROM WS-B-TOT-STDTS-PROCESSED
			SUBTRACT 1 FROM WS-B-DRF-STDTS-PROCESSED
			SUBTRACT 1 FROM WS-B-AWC-STDTS-PROCESSED
			GO TO DABBD-500
	    	END-IF
	   END-IF

 	   	IF	WS-AWARD-CODE = '27'
		THEN
			MOVE SPACES TO WS-GREEN-LIGHT
                        MOVE +0     TO WS-GREEN-LIGHT-I
*                       EXEC SQL EXECUTE
*       		 BEGIN
*       		   PK_GPILOT.PR_GREEN_LIGHT
*       		   ( 2, :WS-CENTRE-NO, 
*                               :WS-COURSE-NO, 
*                               :WS-GREEN-LIGHT:WS-GREEN-LIGHT-I );
*       	    	 END;
*       		END-EXEC
            CALL "SQLADR" USING SQ0025 SQL-STMT
            MOVE 1 TO SQL-ITERS
            MOVE 1245 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 4352 TO SQL-SQLETY
            CALL "SQLADR" USING
                WS-CENTRE-NO
                SQL-SQHSTV(1)
            MOVE 6 TO SQL-SQHSTL(1)
            MOVE 6 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQINDS(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                WS-COURSE-NO
                SQL-SQHSTV(2)
            MOVE 8 TO SQL-SQHSTL(2)
            MOVE 8 TO SQL-SQHSTS(2)
            MOVE 0 TO SQL-SQINDV(2)
            MOVE 0 TO SQL-SQINDS(2)
            MOVE 0 TO SQL-SQHARM(2)
            CALL "SQLADR" USING
                WS-GREEN-LIGHT
                SQL-SQHSTV(3)
            MOVE 1 TO SQL-SQHSTL(3)
            MOVE 1 TO SQL-SQHSTS(3)
            CALL "SQLADR" USING
                WS-GREEN-LIGHT-I
                SQL-SQINDV(3)
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
                THEN GO TO D1C-100 END-IF
                        IF WS-GREEN-LIGHT-I EQUAL -1
                           MOVE SPACES TO WS-GREEN-LIGHT
                        END-IF
			IF 	WS-GREEN-LIGHT NOT = 'Y'
			THEN	SUBTRACT 1 FROM WS-B-TOT-STDTS-PROCESSED
		            	SUBTRACT 1 FROM WS-B-DRF-STDTS-PROCESSED
			    	SUBTRACT 1 FROM WS-B-AWC-STDTS-PROCESSED
				GO TO DABBD-500
			END-IF
		END-IF

*
*	GNVQ Language Awards deferrals processing
*
 	   	IF	WS-AWARD-CODE = '31' OR '32'
		THEN
			MOVE SPACES TO WS-CENTRE-DEFERRED
                        MOVE +0     TO WS-CENTRE-DEFERRED-I
*                       EXEC SQL EXECUTE
*       		 BEGIN
*       		   :WS-CENTRE-DEFERRED:WS-CENTRE-DEFERRED-I :=
*       			PK_GNVQLU.FN_CENTRE_DEFERRED ( :WS-REG-NO );
*       	    	 END;
*       		END-EXEC
            CALL "SQLADR" USING SQ0026 SQL-STMT
            MOVE 1 TO SQL-ITERS
            MOVE 1272 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 4352 TO SQL-SQLETY
            CALL "SQLADR" USING
                WS-CENTRE-DEFERRED
                SQL-SQHSTV(1)
            MOVE 1 TO SQL-SQHSTL(1)
            MOVE 1 TO SQL-SQHSTS(1)
            CALL "SQLADR" USING
                WS-CENTRE-DEFERRED-I
                SQL-SQINDV(1)
            MOVE 0 TO SQL-SQINDS(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                WS-REG-NO
                SQL-SQHSTV(2)
            MOVE 7 TO SQL-SQHSTL(2)
            MOVE 7 TO SQL-SQHSTS(2)
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
                THEN GO TO D1C-100 END-IF
                        IF WS-CENTRE-DEFERRED-I EQUAL -1
                           MOVE SPACES TO WS-CENTRE-DEFERRED
                        END-IF
			IF 	WS-CENTRE-DEFERRED = 'Y'
			THEN	SUBTRACT 1 FROM WS-B-TOT-STDTS-PROCESSED
			    	SUBTRACT 1 FROM WS-B-DRF-STDTS-PROCESSED
			    	SUBTRACT 1 FROM WS-B-AWC-STDTS-PROCESSED
*                               EXEC SQL EXECUTE
*               		 BEGIN
*                  		  PK_BNM.PR_AWARDS_RUN_BLOCK
*                                           (:WS-REG-NO
*                                           ,'STP070'
*                                           ,'1'
*                                           ,:WS-CLAIM-TYPE
*                                           ,NULL
*                                           ,NULL
*                                           );
*               		 END;
*             			END-EXEC
            CALL "SQLADR" USING SQ0027 SQL-STMT
            MOVE 1 TO SQL-ITERS
            MOVE 1295 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 4352 TO SQL-SQLETY
            CALL "SQLADR" USING
                WS-REG-NO
                SQL-SQHSTV(1)
            MOVE 7 TO SQL-SQHSTL(1)
            MOVE 7 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQINDS(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                WS-CLAIM-TYPE
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
                THEN GO TO D1C-100 END-IF
				GO TO DABBD-500
			END-IF
		END-IF
*         
*               Passed so far. Clear any previous BARB record.
*               EXEC SQL EXECUTE
*                BEGIN
*                 PK_BNM.PR_AWARDS_RUN_CLEAR(:WS-REG-NO,'STP070');
*                END;
*               END-EXEC
                CALL "SQLADR" USING SQ0028 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1318 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
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
                    THEN GO TO D1C-100 END-IF
*
*		RSH 28/06/2005 (RfW 05/0070):
*		If certification is blocked for the centre, ignore the
*		student and create a student certification block record.
*
		MOVE SPACES TO WS-CERT-BLOCKED
		MOVE +0     TO WS-CERT-BLOCKED-I

*               EXEC SQL EXECUTE
*       	  BEGIN
*       	    :WS-CERT-BLOCKED:WS-CERT-BLOCKED-I :=
*       		PK_FINANCE_BLOCKING.FN_CENTRE_FINANCE_CERT_BLOCK
*       					(
*       					 :WS-CENTRE-NO,
*       					 'N'
*       					);
*       	  END;
*       	END-EXEC
                CALL "SQLADR" USING SQ0029 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1337 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-CERT-BLOCKED
                    SQL-SQHSTV(1)
                MOVE 1 TO SQL-SQHSTL(1)
                MOVE 1 TO SQL-SQHSTS(1)
                CALL "SQLADR" USING
                    WS-CERT-BLOCKED-I
                    SQL-SQINDV(1)
                MOVE 0 TO SQL-SQINDS(1)
                MOVE 0 TO SQL-SQHARM(1)
                CALL "SQLADR" USING
                    WS-CENTRE-NO
                    SQL-SQHSTV(2)
                MOVE 6 TO SQL-SQHSTL(2)
                MOVE 6 TO SQL-SQHSTS(2)
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
                    THEN GO TO D1C-100 END-IF

		IF  WS-CERT-BLOCKED-I EQUAL -1
		THEN
		    MOVE SPACES TO WS-CERT-BLOCKED
 		END-IF

		IF  WS-CERT-BLOCKED = 'Y'
		THEN
		    SUBTRACT 1 FROM WS-B-TOT-STDTS-PROCESSED
		    SUBTRACT 1 FROM WS-B-DRF-STDTS-PROCESSED
		    SUBTRACT 1 FROM WS-B-AWC-STDTS-PROCESSED

*                   EXEC SQL EXECUTE
*       	      BEGIN
*       		PK_FINANCE_BLOCKING.PR_INSERT_CERT_BLOCK_STUDENT
*       					(
*       					 :WS-CENTRE-NO,
*       					 :WS-REG-NO,
*       					 'STP070'
*       					);
*       	      END;
*       	    END-EXEC
            CALL "SQLADR" USING SQ0030 SQL-STMT
            MOVE 1 TO SQL-ITERS
            MOVE 1360 TO SQL-OFFSET
            MOVE 0 TO SQL-OCCURS
            CALL "SQLADR" USING
                SQLCUD
                SQL-CUD
            CALL "SQLADR" USING
                SQLCA
                SQL-SQLEST
            MOVE 4352 TO SQL-SQLETY
            CALL "SQLADR" USING
                WS-CENTRE-NO
                SQL-SQHSTV(1)
            MOVE 6 TO SQL-SQHSTL(1)
            MOVE 6 TO SQL-SQHSTS(1)
            MOVE 0 TO SQL-SQINDV(1)
            MOVE 0 TO SQL-SQINDS(1)
            MOVE 0 TO SQL-SQHARM(1)
            CALL "SQLADR" USING
                WS-REG-NO
                SQL-SQHSTV(2)
            MOVE 7 TO SQL-SQHSTL(2)
            MOVE 7 TO SQL-SQHSTS(2)
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
                THEN GO TO D1C-100 END-IF

		    GO TO DABBD-500
		END-IF
	END-IF.
*
	MOVE S-AWARD-DATE TO WS-AWARD-DATE.
	IF WS-CURRENT-AWARD-DATE NOT = WS-AWARD-DATE
		MOVE WS-AWARD-DATE TO WS-CURRENT-AWARD-DATE.
        PERFORM DAC-CENTRE-NAME.
*
        MOVE WS-CENTRE-NO     TO PL8-CENTRE-NO,
                                 WS-IRL-CENTRE-NO.
        MOVE WS-CENTRE-NO     TO  WS-CURRENT-CENTRE-NO.
*
        IF WS-CENTRE-NAME-1 = SPACES
           AND  WS-CENTRE-NAME-2 = SPACES
           MOVE 'Y' TO WS-REJECT-NO(22)
	   GO TO DABBD-100.
*
        MOVE WS-CENTRE-NAME-1 TO PL5-CENTRE-NAME.
        MOVE WS-CENTRE-NAME-2 TO PL6-CENTRE-NAME.
*
	MOVE SPACES TO PL1-WITH.
	MOVE SPACES TO PL1-RESULT-DESC.

DABBD-050.
*
*	RSH 17/11/2005 (RfW 05/0186):
*	For 'print' runs, reject the students where any of the
*	achieved units have a certification end date earlier
*	than the current system date.
*
        IF WS-IP-RUN-TYPE = 'A'
	THEN
	    PERFORM ZT-EXPIRED-NVQ-UNITS

	    IF WS-NVQ-UNIT-CERT-EXPIRED = 'Y'
	    THEN
		MOVE 'Y' TO WS-REJECT-NO(20)

		GO TO DABBD-100
	    END-IF
	END-IF.
*
        IF  WS-STORE-AWARD-CODE = SPACES
            GO TO DABBD-100
	END-IF.
*
        IF WS-STORE-AWARD-CODE NOT = S-AWARD-CODE
	    IF WS-IP-RUN-TYPE = 'S'
		MOVE 'Y' TO WS-REJECT-NO(21)
		ADD  +1  TO  WS-B-TOT-STDTS-REJECTED     
		ADD  +1  TO  WS-B-DRF-STDTS-REJECTED     
		ADD  +1  TO  WS-B-AWC-STDTS-REJECTED     
                PERFORM ZX-REJECT
	    END-IF
            GO TO DABBD-500
	END-IF.
*
        IF WS-STORE-AWARD-CODE = '07' OR '08' OR '99'
	    IF WS-IP-RUN-TYPE = 'S'
		MOVE 'Y' TO WS-REJECT-NO(21)
		ADD  +1  TO  WS-B-TOT-STDTS-REJECTED     
		ADD  +1  TO  WS-B-DRF-STDTS-REJECTED     
		ADD  +1  TO  WS-B-AWC-STDTS-REJECTED     
                PERFORM ZX-REJECT
	    END-IF
            GO TO DABBD-500
	END-IF.
DABBD-100.
	MOVE 	S-REG-NO TO WS-REG-NO.
*
        IF 	WS-REJECT-NO-TABLE NOT = SPACES
		ADD  +1  TO  WS-B-TOT-STDTS-REJECTED     
		ADD  +1  TO  WS-B-DRF-STDTS-REJECTED     
		ADD  +1  TO  WS-B-AWC-STDTS-REJECTED     
                PERFORM ZX-REJECT
                GO TO DABBD-500.
DABBD-150.
	ADD  	+1  TO  WS-B-CENTRE-STDTS-PASSED.
	ADD  	+1  TO  WS-B-COURSE-STDTS-PASSED.
	ADD  	+1  TO  WS-B-TOT-STDTS-PASSED.
	ADD  	+1  TO  WS-B-DRF-STDTS-PASSED.
	ADD  	+1  TO  WS-B-AWC-STDTS-PASSED.
*
        SET WS-BTEC-IND TO 1.
        SEARCH WS-TAB-BTEC-AWARDS VARYING WS-BTEC-IND
	      AT END GO TO DABBD-170
              WHEN WS-TAB-BTEC-AWARD-CODE(WS-BTEC-IND)  = SPACES GO TO DABBD-170
              WHEN WS-TAB-BTEC-AWARD-CODE(WS-BTEC-IND)  = S-BTEC-LEVEL AND
                   WS-TAB-BTEC-AWARD-TITLE(WS-BTEC-IND) = S-BTEC-CODE  AND
                   WS-TAB-BTEC-AWARD-TITLE(WS-BTEC-IND) NOT = 11562
                  PERFORM ZC-INSERT-IOM-CERTS
* NEW DSA LQ00061566
              WHEN WS-TAB-BTEC-AWARD-CODE(WS-BTEC-IND)  = S-BTEC-LEVEL AND
                   WS-TAB-BTEC-AWARD-TITLE(WS-BTEC-IND) = S-BTEC-CODE  AND
                   WS-TAB-BTEC-AWARD-TITLE(WS-BTEC-IND) = 11562 
                  PERFORM ZD-INSERT-DSA-CERTS

        END-SEARCH.
*
DABBD-170.


	MOVE SPACES TO WS-COURSE-QCA-CODE.
	IF	WS-AC-BNM-TYPE = 'F' OR
	     	WS-AC-BNM-TYPE = 'N' OR
	     	WS-AC-BNM-TYPE = 'I' OR
	     	WS-AC-BNM-TYPE = '1' OR
	     	WS-AC-BNM-TYPE = 'C' OR
	     	WS-AC-BNM-TYPE = 'G' OR
	     	WS-AC-BNM-TYPE = 'W' OR
	     	WS-AC-BNM-TYPE = 'T' OR
	     	WS-AC-BNM-TYPE = 'H' OR
	     	WS-AC-BNM-TYPE = 'X' OR
	     	WS-AC-BNM-TYPE = 'S'       
	THEN
		IF WS-AWARD-CODE = 'FS' OR 'FO'
		THEN
                  GO TO DABBD-175
		END-IF
		IF	S-OVER-GRADE = '1' AND WS-AC-BNM-TYPE = 'G'
		THEN	MOVE 'GRADE PASS AT LEVEL 1' TO PL1-RESULT-DESC
		END-IF
*EC2825
                IF    WS-AWARD-CODE = 'Y6'
                THEN  
                 IF S-OVER-GRADE = '1' AND WS-AC-BNM-TYPE = 'G'
                   THEN    MOVE 'GRADE PASS AT LEVEL 1' TO PL1-RESULT-DESC
                 END-IF
                 IF S-OVER-GRADE = '2' AND WS-AC-BNM-TYPE = 'G'
                   THEN    MOVE 'GRADE MERIT AT LEVEL 1' TO PL1-RESULT-DESC
                 END-IF
                 IF S-OVER-GRADE = '3' AND WS-AC-BNM-TYPE = 'G'
                   THEN    MOVE 'GRADE DISTINCTION AT LEVEL 1' TO PL1-RESULT-DESC
                 END-IF
                 IF S-OVER-GRADE = 'U' AND WS-AC-BNM-TYPE = 'G'
                   THEN    MOVE 'GRADE UNCLASSIFIED AT LEVEL 1' TO PL1-RESULT-DESC
                 END-IF
                END-IF
* EC1938 - NG level 2 overall grade wording
* EC2825 award code 'Y6' and grade 'U'
		IF WS-AWARD-CODE = 'FA' OR 'FC' OR 'FX' OR 'FD' OR 'Y6'
		THEN
                 IF WS-AWARD-CODE = 'Y6'
                 THEN
                  IF S-OVER-GRADE = 'U' AND WS-AC-BNM-TYPE = 'G'
                  THEN    MOVE 'GRADE UNCLASSIFIED AT LEVEL 2' TO PL1-RESULT-DESC
                  END-IF
                 END-IF
		 IF	S-OVER-GRADE = 'P'
		 THEN	MOVE 'GRADE PASS AT LEVEL 2' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'M'
		 THEN	MOVE 'GRADE MERIT AT LEVEL 2' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'D'
		 THEN	MOVE 'GRADE DISTINCTION AT LEVEL 2' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'D*' OR
		   	S-OVER-GRADE = '*'
		 THEN	MOVE 'GRADE DISTINCTION* AT LEVEL 2' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'PP'
		 THEN	MOVE 'THE DOUBLE GRADE PASS PASS AT LEVEL 2' 
                        TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'MP'
		 THEN	MOVE 'THE DOUBLE GRADE MERIT PASS AT LEVEL 2' 
                        TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'MM'
		 THEN	MOVE 'THE DOUBLE GRADE MERIT MERIT AT LEVEL 2' 
                        TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'DM'
		 THEN	MOVE 'THE DOUBLE GRADE DISTINCTION MERIT AT LEVEL 2' 
		        TO   PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'DD'
		 THEN	MOVE 
 			 'THE DOUBLE GRADE DISTINCTION DISTINCTION AT LEVEL 2' 
		        TO   PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = '*D'
		 THEN	MOVE 
			 'THE DOUBLE GRADE DISTINCTION* DISTINCTION AT LEVEL 2' 
		        TO   PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = '**'
		 THEN	MOVE 
		       'THE DOUBLE GRADE DISTINCTION* DISTINCTION* AT LEVEL 2' 
		        TO   PL1-RESULT-DESC
		 END-IF
		ELSE
		 IF	S-OVER-GRADE = 'P'
		 THEN	MOVE 'GRADE PASS' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'M'
		 THEN	MOVE 'GRADE MERIT' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'D'
		 THEN	MOVE 'GRADE DISTINCTION' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'D*' OR
		   	S-OVER-GRADE = '*'
		 THEN	MOVE 'GRADE DISTINCTION*' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'PP'
		 THEN	MOVE 'THE DOUBLE GRADE PASS PASS' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'MP'
		 THEN	MOVE 'THE DOUBLE GRADE MERIT PASS' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'MM'
		 THEN	MOVE 'THE DOUBLE GRADE MERIT MERIT' TO PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'DM'
		 THEN	MOVE 'THE DOUBLE GRADE DISTINCTION MERIT' 
		        TO   PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = 'DD'
		 THEN	MOVE 'THE DOUBLE GRADE DISTINCTION DISTINCTION' 
		        TO   PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = '*D'
		 THEN	MOVE 'THE DOUBLE GRADE DISTINCTION* DISTINCTION' 
		        TO   PL1-RESULT-DESC
		 END-IF
		 IF	S-OVER-GRADE = '**'
		 THEN	MOVE 'THE DOUBLE GRADE DISTINCTION* DISTINCTION*' 
		        TO   PL1-RESULT-DESC
		 END-IF
                END-IF
		IF	S-OVER-GRADE = 'PPP'
		THEN	MOVE 'THE TRIPLE GRADE PASS PASS PASS' 
		        TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = 'MPP'
		THEN	MOVE 'THE TRIPLE GRADE MERIT PASS PASS' 
		        TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = 'MMP'
		THEN	MOVE 'THE TRIPLE GRADE MERIT MERIT PASS' 
		        TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = 'MMM'
		THEN	MOVE 'THE TRIPLE GRADE MERIT MERIT MERIT' 
		        TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = 'DMM'
		THEN	MOVE 'THE TRIPLE GRADE DISTINCTION MERIT MERIT' 
		        TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = 'DDM'
		THEN	MOVE 'THE TRIPLE GRADE DISTINCTION DISTINCTION MERIT' 
		        TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = 'DDD'
		THEN	
                 MOVE 'THE TRIPLE GRADE DISTINCTION DISTINCTION DISTINCTION' 
		 TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = '*DD'
		THEN	
                 MOVE 'THE TRIPLE GRADE DISTINCTION* DISTINCTION DISTINCTION' 
		 TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = '**D'
		THEN	
                 MOVE 'THE TRIPLE GRADE DISTINCTION* DISTINCTION* DISTINCTION' 
		 TO   PL1-RESULT-DESC
		END-IF
		IF	S-OVER-GRADE = '***'
		THEN	
                 MOVE 'THE TRIPLE GRADE DISTINCTION* DISTINCTION* DISTINCTION*' 
		 TO   PL1-RESULT-DESC
		END-IF
	        IF	PL1-RESULT-DESC NOT = SPACES
                THEN	MOVE 'WITH' TO PL1-WITH
                END-IF
		IF WS-AC-ACCRED-BODY = 'S'
		THEN
	          PERFORM ZQ-COURSE-SQA-CODE
		ELSE
	          PERFORM ZR-COURSE-QCA-CODE
		END-IF
	     	IF	WS-AC-BNM-TYPE NOT = 'H'
		THEN
                	GO TO DABBD-200
		END-IF
	END-IF.

* ABS Entry Level...
* and ICT Entry Level...

	IF WS-AWARD-CODE = '63' OR 'HH'
	THEN 
	        PERFORM ZR-COURSE-QCA-CODE

		IF WS-BTEC-TITLE = '9002' OR '9018'
		THEN

			MOVE 	'LEVEL'			TO PL1-WITH
			STRING 	' ATTAINED: ENTRY ' 	DELIMITED BY SIZE
		  		S-ABS-LEVEL-ACHIEVED	DELIMITED BY SIZE
			INTO PL1-RESULT-DESC
                	GO TO DABBD-200

		END-IF
	END-IF.

* Functional Skills...

DABBD-175.

	IF WS-AWARD-CODE = 'FE'
	THEN 
	        PERFORM ZR-COURSE-QCA-CODE

		MOVE 	'LEVEL'			TO PL1-WITH
		STRING 	' ATTAINED: ENTRY ' 	DELIMITED BY SIZE
			S-ABS-LEVEL-ACHIEVED	DELIMITED BY SIZE
		INTO PL1-RESULT-DESC
                GO TO DABBD-200
	END-IF.	

	IF WS-AWARD-CODE = 'FS'
	THEN 
	  IF S-OVER-GRADE NOT EQUAL SPACES
	  THEN
	    MOVE S-OVER-GRADE TO WS-QUAL-LEVEL
            PERFORM ZS-ESOL-QCA-CODE
	  END-IF

	  MOVE	 'LEVEL' TO PL1-WITH
	  STRING ' ATTAINED: ENTRY ' 	DELIMITED BY SIZE
		 S-OVER-GRADE(2:1)	DELIMITED BY SIZE
	  INTO PL1-RESULT-DESC

	  GO TO DABBD-200

	END-IF.	

	IF WS-AWARD-CODE = 'FO'
	THEN 
	  IF S-OVER-GRADE NOT EQUAL SPACES
	  THEN
	    MOVE S-OVER-GRADE TO WS-QUAL-LEVEL
            PERFORM ZS-ESOL-QCA-CODE
	  END-IF

	  MOVE 'LEVEL'TO PL1-WITH

	  STRING ' '		DELIMITED BY SIZE
		 S-OVER-GRADE	DELIMITED BY SIZE
	  INTO	PL1-RESULT-DESC

	  GO TO DABBD-200

	END-IF.	

* Entry Level - Award Code 35...

	IF WS-AWARD-CODE = '35' AND WS-BTEC-TITLE NOT EQUAL '2250'
	THEN 
	        PERFORM ZR-COURSE-QCA-CODE
	END-IF.	

* FAD Certs...

	IF WS-AWARD-CODE = '11' AND WS-BTEC-TITLE EQUAL '2896'
	THEN 
	        PERFORM ZR-COURSE-QCA-CODE
	END-IF.	

	IF WS-AWARD-CODE = 'VV'
	THEN 
	        PERFORM ZR-COURSE-QCA-CODE
	END-IF.	

* EC2016 : New ESOL...

	IF WS-AWARD-CODE = 'Q0' OR
	   WS-AWARD-CODE = 'Q1' OR
	   WS-AWARD-CODE = 'Q2' OR
	   WS-AWARD-CODE = 'Q3' OR
	   WS-AWARD-CODE = 'Q4' OR
	   WS-AWARD-CODE = 'Q5' OR
	   WS-AWARD-CODE = 'Q6' OR
	   WS-AWARD-CODE = 'Q7' OR
	   WS-AWARD-CODE = 'Q8' OR
	   WS-AWARD-CODE = 'Q9'
	THEN 
	        PERFORM ZR-COURSE-QCA-CODE
	END-IF.	

        IF 	S-OVER-GRADE NOT  = 'C' 
				AND 'D'
				AND 'M' 
				AND '1' 
				AND '2' 
				AND '3'
			       	AND 'A'
				AND 'B'
			       	AND 'E'	 
                GO TO DABBD-200.

DABBD-180.
*
*	RSH 08/08/2005 (RfW 04/0109):
*	ESOL Entry Level...
*
	IF (
	    WS-AWARD-CODE = '93' OR
	    WS-AWARD-CODE = 'CC' OR
	    WS-AWARD-CODE = 'GG' OR
	    WS-AWARD-CODE = 'GA' OR
	    WS-AWARD-CODE = 'GB' OR
	    WS-AWARD-CODE = 'EW' 
	   )
	THEN 
	        IF NOT (
			S-ESOL-LEVEL-ACHIEVED = SPACES
		       )
		THEN
		    MOVE S-ESOL-LEVEL-ACHIEVED TO WS-QUAL-LEVEL

		    PERFORM ZS-ESOL-QCA-CODE
		END-IF

		MOVE 'LEVEL' TO PL1-WITH
*
*      		RSH 02/09/2005 (RfW 04/0109):
*		For Entry Level, use grade instead of level.
*
		IF (
		    WS-AWARD-CODE = '93' OR WS-AWARD-CODE = 'GG'
		   )
		THEN
		    STRING	' ATTAINED: ENTRY '	DELIMITED BY SIZE
		  		S-ABS-LEVEL-ACHIEVED	DELIMITED BY SIZE
		    INTO	PL1-RESULT-DESC
		ELSE
		    STRING	' '			DELIMITED BY SIZE
		  		S-ESOL-LEVEL-ACHIEVED	DELIMITED BY SIZE
		    INTO	PL1-RESULT-DESC
		END-IF

		GO TO DABBD-200
	END-IF.	

DABBD-190.
      	MOVE 	'WITH'		TO PL1-WITH.

        IF WS-AWARD-CODE = 'EP'

          PERFORM ZR-COURSE-QCA-CODE

          IF 	S-OVER-GRADE 	= 'A'
           		MOVE 'GRADE A (a)'	TO PL1-RESULT-DESC
          ELSE
            IF 	S-OVER-GRADE 	= 'B'
                        MOVE 'GRADE B (b)'	TO PL1-RESULT-DESC
            ELSE
              IF 	S-OVER-GRADE 	= 'C'
                        MOVE 'GRADE C (c)'	TO PL1-RESULT-DESC
              ELSE
          	IF 	S-OVER-GRADE 	= 'D' 
                        MOVE 'GRADE D (d)'	TO PL1-RESULT-DESC
                ELSE
           	  IF 	S-OVER-GRADE 	= 'E'
                        MOVE 'GRADE E (e)'	TO PL1-RESULT-DESC
		  ELSE
          	    IF 	S-OVER-GRADE 	= 'U'
                        MOVE 'GRADE U (u)'	TO PL1-RESULT-DESC
                    END-IF
                  END-IF
		END-IF
	      END-IF
	    END-IF
          END-IF
        ELSE

	  IF 	S-OVER-GRADE 	= 'C'
		MOVE 'CREDIT'	TO PL1-RESULT-DESC
	  ELSE
            IF      S-OVER-GRADE    = 'M'
                  MOVE 'MERIT' TO PL1-RESULT-DESC
	    ELSE
	      IF      S-OVER-GRADE    = 'D'
		  MOVE 'DISTINCTION' TO PL1-RESULT-DESC
	      ELSE
		  MOVE    'LEVEL'          TO PL1-WITH
		  STRING ' ATTAINED: ENTRY ' DELIMITED BY SIZE
*      the space before the level is required
		  S-OVER-GRADE DELIMITED BY SIZE INTO PL1-RESULT-DESC
	      END-IF
	    END-IF
	  END-IF
        END-IF.
*
DABBD-200.

      	MOVE WS-COURSE-QCA-CODE TO PL8-COURSE-QCA-CODE.

 	IF	WS-AWARD-CODE = '31' 
	THEN	MOVE 'FOUNDATION'   TO WS-LINE-1
	END-IF.
 	IF	WS-AWARD-CODE = '32'
	THEN	MOVE 'INTERMEDIATE' TO WS-LINE-1
	END-IF.
*WI409 store reg no for IRL
	MOVE S-REG-NO       TO PL8-REG-NO,
                               WS-IRL-REG-NO.
	MOVE S-BIRTH-DATE   TO PL8-BIRTH-DATE.
	MOVE S-ISSUE-DATE   TO PL8-ISSUE-DATE.
*          
        
    	PERFORM VB-GET-FRAN-CENTRE-LOGO.
	PERFORM VD-GET-REPRINT-LOGOS.
*
	INITIALIZE WS-CENTRE-OR-FRAN-LOGOS.

	IF WS-FRAN-CENTRE-LOGO NOT EQUAL SPACES
	THEN
	  MOVE WS-FRAN-CENTRE-LOGO TO WS-CENTRE-OR-FRAN-LOGO(1)
	  MOVE SPACES		   TO WS-CENTRE-OR-FRAN-LOGO(2)
	ELSE
	  MOVE WS-CENTRE-LOGO(1)   TO WS-CENTRE-OR-FRAN-LOGO(1)
	  MOVE WS-CENTRE-LOGO(2)   TO WS-CENTRE-OR-FRAN-LOGO(2)
	END-IF.

	MOVE SPACES TO WS-LOGOS.

        IF WS-CENTRE-LOGO(2) 	EQUAL SPACES
	THEN
	  IF WS-TORG-LOGO(2)	EQUAL SPACES
          THEN
            STRING WS-AWARD-TYPE-LOGO		DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-CENTRE-OR-FRAN-LOGO(1)	DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-TORG-LOGO(1)              DELIMITED BY SPACE
            INTO   WS-LOGOS
          ELSE
            STRING WS-AWARD-TYPE-LOGO           DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-CENTRE-OR-FRAN-LOGO(1)	DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-TORG-LOGO(1)              DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-TORG-LOGO(2)              DELIMITED BY SPACE
            INTO   WS-LOGOS
	  END-IF
	ELSE
	  IF WS-TORG-LOGO(2) EQUAL SPACES
          THEN
            STRING WS-AWARD-TYPE-LOGO           DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-CENTRE-OR-FRAN-LOGO(1)	DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-CENTRE-OR-FRAN-LOGO(2)	DELIMITED BY SPACE
                   "#"                          DELIMITED BY SIZE
                   WS-TORG-LOGO(1)              DELIMITED BY SPACE
            INTO   WS-LOGOS
          ELSE
	    IF WS-AWARD-TYPE-LOGO EQUAL SPACES
	    THEN
              STRING WS-CENTRE-OR-FRAN-LOGO(1)	DELIMITED BY SPACE
                     "#"                        DELIMITED BY SIZE
                     WS-CENTRE-OR-FRAN-LOGO(2)	DELIMITED BY SPACE
                     "#"                        DELIMITED BY SIZE
                     WS-TORG-LOGO(1)            DELIMITED BY SPACE
                     "#"                        DELIMITED BY SIZE
                     WS-TORG-LOGO(2)            DELIMITED BY SPACE
              INTO   WS-LOGOS
	    ELSE	
              STRING WS-AWARD-TYPE-LOGO         DELIMITED BY SPACE
                     "#"                        DELIMITED BY SIZE
                     WS-CENTRE-OR-FRAN-LOGO(1)	DELIMITED BY SPACE
                     "#"                        DELIMITED BY SIZE
                     WS-CENTRE-OR-FRAN-LOGO(2)	DELIMITED BY SPACE
                     "#"                        DELIMITED BY SIZE
                     WS-TORG-LOGO(1)            DELIMITED BY SPACE
               INTO   WS-LOGOS
	    END-IF
	  END-IF	
        END-IF.

DABBD-300.

	MOVE S-STUDENT-NAME TO PL4-STUDENT-NAME.
*
	IF WS-IP-RUN-TYPE EQUAL TO 'S'
*               MOVE 'REPLACEMENT' TO PL8-REPRINT-WORD
                MOVE S-CERT-REPRINT-WORDING TO PL8-REPRINT-WORD
		MOVE 'R'       TO PL8-REPRINT-IND
	        MOVE S-CERT-NO TO PL8-CERT-NO
	ELSE
                MOVE SPACES           TO PL8-REPRINT-WORD
	   	MOVE SPACES 	      TO PL8-REPRINT-IND
	        MOVE WS-ALLOC-CERT-NO TO PL8-CERT-NO.
*
	MOVE S-MONTH-YEAR TO PL7-AWARD-DATE.
*          
	MOVE SPACES TO PRINT-REC.
*
	IF WS-FORMAT-TYPE = 'F' THEN
*
   	    WRITE PRINT-REC AFTER PAGE
*
	    IF WS-ACTY-CODE = 'BP' OR
	       WS-ACTY-CODE = 'BA' OR
	       WS-ACTY-CODE = 'KS'
	    THEN	
		IF WS-ACTY-CODE = 'KS'
           	   IF WS-AWARD-CODE = '16' OR
                      WS-AWARD-CODE = '17' OR
                      WS-AWARD-CODE = '18' OR
                      WS-AWARD-CODE = '19' 
		   THEN   
                   	MOVE  WS-LINE-1B TO WS-KS-LINE
	           	WRITE PRINT-REC FROM WS-KEY-SKILLS-LINE AFTER 17
		   	MOVE  WS-LINE-2A TO WS-KS-LINE
           	   	WRITE PRINT-REC FROM WS-KEY-SKILLS-LINE AFTER 2
		   ELSE
		   	MOVE  WS-LINE-1 TO WS-KS-LINE
           	   	WRITE PRINT-REC FROM WS-KEY-SKILLS-LINE AFTER 17
		   	MOVE  WS-LINE-2 TO WS-KS-LINE
           	   	WRITE PRINT-REC FROM WS-KEY-SKILLS-LINE AFTER 2
                   END-IF
		ELSE
            	   IF WS-LINE-1A NOT = SPACES
                      MOVE  WS-LINE-1A TO WS-BA-LINE
	              WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 17
		      MOVE  WS-LINE-2 TO WS-BA-LINE
           	      WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 2
	           ELSE    
		      MOVE  WS-LINE-2 TO WS-BA-LINE
           	      WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 19
	           END-IF
		END-IF
*       	
	        IF PL3-COURSE-NAME NOT = SPACES 
        	    MOVE WS-LINE-3 TO WS-BA-LINE
        	    WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 1
	            MOVE WS-LINE-4 TO WS-BA-LINE
		    WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 6
	        ELSE	
		    MOVE WS-LINE-4 TO WS-BA-LINE
  		    WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 7
                END-IF
*
                MOVE WS-LINE-5 TO WS-BA-LINE
   	        WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 4
*
                IF PL6-CENTRE-NAME NOT = SPACES 
                    MOVE WS-LINE-6 TO WS-BA-LINE
         	    WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE
                END-IF
*
                MOVE WS-LINE-7 TO WS-BA-LINE
		IF PL6-CENTRE-NAME NOT = SPACES 
	            WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 4
		ELSE
		    WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 5
		END-IF
*
                MOVE WS-LINE-8 TO WS-BA-LINE
        	WRITE PRINT-REC FROM WS-BTEC-AWARD-LINE AFTER 2
*
                MOVE SPACES TO PRINT-REC
*
	    ELSE
*
                IF WS-ACTY-CODE = 'CA'
                THEN
                   MOVE  WS-LINE-1E TO WS-COA-LINE
                   WRITE PRINT-REC FROM WS-COFA-LINE AFTER 17
                   MOVE  WS-LINE-2 TO WS-COA-LINE
                   WRITE PRINT-REC FROM WS-COFA-LINE AFTER 2
                ELSE
                   IF WS-LINE-1 NOT = SPACES
                      MOVE  WS-LINE-1 TO PRINT-REC
                      WRITE PRINT-REC AFTER 1
                      MOVE  WS-LINE-2 TO PRINT-REC
                      WRITE PRINT-REC AFTER 2
                   ELSE    
                      MOVE  WS-LINE-2 TO PRINT-REC
                      WRITE PRINT-REC AFTER 3
                   END-IF
                END-IF          
*       	
	        IF PL3-COURSE-NAME NOT = SPACES 
        	    MOVE WS-LINE-3 TO PRINT-REC
        	    WRITE PRINT-REC AFTER 1
	            MOVE WS-LINE-4 TO PRINT-REC
		    WRITE PRINT-REC AFTER 3
	        ELSE	
		    MOVE WS-LINE-4 TO PRINT-REC
  		    WRITE PRINT-REC AFTER 4
                END-IF
*
                MOVE WS-LINE-5 TO PRINT-REC
   	        WRITE PRINT-REC AFTER 3
*
                IF PL6-CENTRE-NAME NOT = SPACES 
                    MOVE WS-LINE-6 TO PRINT-REC
         	    WRITE PRINT-REC AFTER 1
                END-IF
*
                MOVE WS-LINE-7 TO PRINT-REC
	        WRITE PRINT-REC AFTER 1
*
                IF PL6-CENTRE-NAME NOT = SPACES 
                    MOVE WS-LINE-8 TO PRINT-REC
        	    WRITE PRINT-REC AFTER 33
                ELSE
		    MOVE WS-LINE-8 TO PRINT-REC
        	    WRITE PRINT-REC AFTER 34
                END-IF
*
                MOVE SPACES TO PRINT-REC
*
	    END-IF
*
* Format Type is not 'F'
	ELSE

	    MOVE SPACES TO WS-ANS-REC
*
*	    RSH 12/05/2005:
*	    Retrieve the FCS Award Reference for the Award Code
*	    from the FCS File table array.
*
	    MOVE SPACES TO WS-FCS-AWARD-REF

	    MOVE 'N' TO WS-FCS-REF-FOUND

	    PERFORM VARYING WS-FFT-INDEX FROM 1 BY 1
		    UNTIL WS-FCS-REF-FOUND = 'Y'
		    OR    WS-FFT-INDEX > WS-FFT-FETCHED

		    IF	WS-FFT-AWARD-CODE(WS-FFT-INDEX) = WS-AWARD-CODE
		    THEN
			MOVE 'Y' TO WS-FCS-REF-FOUND

			MOVE WS-FFT-FCS-AWARD-REF(WS-FFT-INDEX)
							TO WS-FCS-AWARD-REF
		    END-IF
	    END-PERFORM
*
*	    RSH 12/05/2005:
*	    If the FCS Award Reference has been found, use it instead
*	    of '0'||Award Code.
*
	    IF (
		WS-FCS-REF-FOUND = 'Y' AND
		NOT WS-FCS-AWARD-REF = SPACES
	       )
	    THEN

	      IF 	WS-IP-RUN-TYPE = 'S'
	      THEN
		IF 	S-OLD-REPRINT-CERT = 'Y'
		THEN
		 STRING	'05*!*9'		DELIMITED BY SIZE
			WS-FCS-AWARD-REF	DELIMITED BY SIZE
		 INTO	WS-ANS-REC
		ELSE
		 STRING	'05*!*'			DELIMITED BY SIZE
			WS-FCS-AWARD-REF	DELIMITED BY SIZE
		 INTO	WS-ANS-REC
		END-IF
	      ELSE
		STRING	'05*!*'			DELIMITED BY SIZE
			WS-FCS-AWARD-REF	DELIMITED BY SIZE
		INTO	WS-ANS-REC
	      END-IF

	    ELSE

		STRING	'05*!*'			DELIMITED BY SIZE
			'0'			DELIMITED BY SIZE
			WS-AWARD-CODE		DELIMITED BY SIZE
		INTO	WS-ANS-REC

	    END-IF

	    PERFORM T-GET-NEXT-POSITION

	    IF WS-ACTY-CODE = 'BP' OR
	       WS-ACTY-CODE = 'BA'

	       STRING '*!*'		DELIMITED BY SIZE
		      WS-LINE-1A	DELIMITED BY SIZE
	       INTO   WS-ANS-REC
               WITH   POINTER WS-POINTER

 	       PERFORM T-GET-NEXT-POSITION

 	       STRING '*!*'		DELIMITED BY SIZE 
                      PL2-COURSE-NAME	DELIMITED BY SIZE
	       INTO   WS-ANS-REC 
               WITH   POINTER WS-POINTER

	    ELSE

	       IF WS-ACTY-CODE = 'KS'
	       AND	 ( WS-AWARD-CODE = '16' OR
                      	   WS-AWARD-CODE = '17' OR
                           WS-AWARD-CODE = '18' OR
                           WS-AWARD-CODE = '19' OR
                           WS-AWARD-CODE = '27') 
	       THEN   
                   IF 	WS-AWARD-CODE = '27'
		   THEN

 	         	STRING '*!**!*'		DELIMITED BY SIZE
	    	     	       WS-LINE-1C	DELIMITED BY SIZE
	         	INTO   WS-ANS-REC 
                 	WITH   POINTER WS-POINTER

		   ELSE

 	         	STRING '*!*'		DELIMITED BY SIZE
	    	     	       WS-LINE-1B	DELIMITED BY SIZE
			INTO   WS-ANS-REC
                 	WITH   POINTER WS-POINTER

	         	PERFORM T-GET-NEXT-POSITION

 	         	STRING '*!*'            DELIMITED BY SIZE 
                               PL2-KS-LEVEL     DELIMITED BY SIZE
	         	INTO   WS-ANS-REC 
                 	WITH   POINTER WS-POINTER

		   END-IF                        

	       ELSE

                 IF WS-ACTY-CODE = 'CA'
                 THEN
			MOVE S-COA-LEVEL-ACHIEVED-DESCR TO PL1-COA-LEVEL

			STRING '*!*'		DELIMITED BY SIZE
			       WS-LINE-1E	DELIMITED BY SIZE
			INTO   WS-ANS-REC
                 	WITH   POINTER WS-POINTER

		 ELSE
                  IF WS-AWARD-CODE = 'EW'
                  THEN
                    MOVE SPACES TO PL1-MULTI-LEVEL-AWARD

		    IF	S-OLD-REPRINT-CERT = 'Y'
		    THEN
                     IF S-ESOL-LEVEL-ACHIEVED(1:1) = 'E'
                     THEN
                      STRING 'EDEXCEL ENTRY LEVEL '      DELIMITED BY SIZE
                             S-ESOL-LEVEL-ACHIEVED(2:1)  DELIMITED BY SIZE
                             ' CERTIFICATE'              DELIMITED BY SIZE
                      INTO   PL1-MULTI-LEVEL-AWARD
                     ELSE
                      STRING 'EDEXCEL LEVEL '            DELIMITED BY SIZE
                             S-ESOL-LEVEL-ACHIEVED(1:1)  DELIMITED BY SIZE
                             ' CERTIFICATE'              DELIMITED BY SIZE
                      INTO   PL1-MULTI-LEVEL-AWARD
		     END-IF
  		    ELSE
                     IF S-ESOL-LEVEL-ACHIEVED(1:1) = 'E'
                     THEN
                      STRING 'PEARSON ENTRY LEVEL '      DELIMITED BY SIZE
                             S-ESOL-LEVEL-ACHIEVED(2:1)  DELIMITED BY SIZE
                             ' CERTIFICATE'              DELIMITED BY SIZE
                      INTO   PL1-MULTI-LEVEL-AWARD
                     ELSE
                      STRING 'PEARSON LEVEL '            DELIMITED BY SIZE
                             S-ESOL-LEVEL-ACHIEVED(1:1)  DELIMITED BY SIZE
                             ' CERTIFICATE'              DELIMITED BY SIZE
                      INTO   PL1-MULTI-LEVEL-AWARD
		     END-IF
                    END-IF
  
                    STRING '*!*'      DELIMITED BY SIZE
                           WS-LINE-1D DELIMITED BY SIZE
                    INTO   WS-ANS-REC
                            WITH   POINTER WS-POINTER
  
                  ELSE
                    STRING '*!*'          DELIMITED BY SIZE
                           WS-LINE-1      DELIMITED BY SIZE
                    INTO   WS-ANS-REC
                            WITH   POINTER WS-POINTER
                  END-IF

                 END-IF

	         PERFORM T-GET-NEXT-POSITION

 	         STRING '*!*'           DELIMITED BY SIZE 
                        PL2-COURSE-NAME DELIMITED BY SIZE
	         INTO   WS-ANS-REC 
                 WITH   POINTER WS-POINTER

	       END-IF
	    END-IF
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'           DELIMITED BY SIZE 
                   PL3-COURSE-NAME DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'            DELIMITED BY SIZE 
                   PL4-STUDENT-NAME DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    IF WS-AC-ACCRED-BODY = 'S'
	    THEN

	      STRING '*!*'             DELIMITED BY SIZE 
                     WS-NO-CENTRE-NAME DELIMITED BY SIZE
	      INTO   WS-ANS-REC 
              WITH   POINTER WS-POINTER
*
	      PERFORM T-GET-NEXT-POSITION
*
	      STRING '*!*'           DELIMITED BY SIZE 
	      INTO   WS-ANS-REC 
              WITH   POINTER WS-POINTER
	   
            ELSE

	      STRING '*!*'           DELIMITED BY SIZE 
                     PL5-CENTRE-NAME DELIMITED BY SIZE
	      INTO   WS-ANS-REC 
              WITH   POINTER WS-POINTER
*
	      PERFORM T-GET-NEXT-POSITION
*
	      STRING '*!*'           DELIMITED BY SIZE 
                     PL6-CENTRE-NAME DELIMITED BY SIZE
	      INTO   WS-ANS-REC 
              WITH   POINTER WS-POINTER

	    END-IF
*
	    PERFORM T-GET-NEXT-POSITION
*
* ADD ENDORSEMENT TEXT 
*
	    IF S-NOPS-REQD       = 'Y' OR
	       S-NVQ-ROA-CODE    = 'Y' OR
	       S-GNVQ-CUC-CODE   = 'Y' OR
	       S-CRED-TRANS-CODE = 'Y'
              MOVE WS-CERT-MSG-AB TO WS-CERT-MSG
	    ELSE
	      MOVE SPACES TO WS-CERT-MSG
	    END-IF
*
            MOVE SPACES           TO WS-CERT-MSG-SURF
*EC2490
            IF S-CERT-FLAG NOT= SPACES
               MOVE S-CERT-MSG-SURF TO WS-CERT-MSG-SURF
            END-IF
*	    IF S-CERT-FLAG = 'A'
*              MOVE WS-CERT-MSG-A  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'B'
*              MOVE WS-CERT-MSG-B  TO WS-CERT-MSG-SURF
*	    END-IF
*            IF S-CERT-FLAG = 'C'
*              MOVE WS-CERT-MSG-C  TO WS-CERT-MSG-SURF
*            END-IF
*	    IF S-CERT-FLAG = 'D'
*              MOVE WS-CERT-MSG-D  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'F'
*              MOVE WS-CERT-MSG-F  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'G'
*              MOVE WS-CERT-MSG-G  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'I'
*              MOVE WS-CERT-MSG-I  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'L'
*              MOVE WS-CERT-MSG-L  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'M'
*              MOVE WS-CERT-MSG-M  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'P'
*              MOVE WS-CERT-MSG-P  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'R'
*              MOVE WS-CERT-MSG-R  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'S'
*              MOVE WS-CERT-MSG-S  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'T'
*              MOVE WS-CERT-MSG-T  TO WS-CERT-MSG-SURF
*	    END-IF
*	    IF S-CERT-FLAG = 'X'
*              MOVE WS-CERT-MSG-X  TO WS-CERT-MSG-SURF
*	    END-IF
*
	    STRING '*!*'     DELIMITED BY SIZE 
                   WS-CERT-MSG-SURF DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
            IF      WS-AWARD-CODE = 'EA' OR
		    WS-AWARD-CODE = 'TZ'
	    THEN
		    STRING '*!*'          DELIMITED BY SIZE
			   'COMPLETED : ' DELIMITED BY SIZE 
	                   PL7-AWARD-DATE DELIMITED BY SIZE
		    INTO   WS-ANS-REC 
	            WITH   POINTER WS-POINTER
	    ELSE
		    STRING '*!*'          DELIMITED BY SIZE
			   'AWARDED : '	  DELIMITED BY SIZE 
	                   PL7-AWARD-DATE DELIMITED BY SIZE
		    INTO   WS-ANS-REC 
	            WITH   POINTER WS-POINTER
	    END-IF
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'          DELIMITED BY SIZE 
                   WS-CERT-MSG    DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    STRING '*!*'     DELIMITED BY SIZE 
                   WS-LINE-8 DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER
*
	    PERFORM T-GET-NEXT-POSITION
*
	    MOVE SPACES TO WS-1ST-LOGO
			   WS-2ND-LOGO
			   WS-3RD-LOGO
			   WS-4TH-LOGO

	    MOVE ZERO TO WS-PTR

	    INSPECT WS-LOGOS TALLYING WS-PTR FOR LEADING '#'

	    ADD 1 TO WS-PTR

	    UNSTRING WS-LOGOS DELIMITED BY ALL '#' 
	    INTO WS-1ST-LOGO
	    	 WS-2ND-LOGO
	    	 WS-3RD-LOGO
	    	 WS-4TH-LOGO
	    WITH POINTER WS-PTR

	    IF	WS-PBODY-LOGO(5) = SPACES
	    THEN
		IF 	WS-PBODY-LOGO(1) NOT = SPACES
		THEN 
			MOVE WS-3RD-LOGO 	TO WS-4TH-LOGO
			MOVE WS-2ND-LOGO 	TO WS-3RD-LOGO
			MOVE WS-1ST-LOGO 	TO WS-2ND-LOGO
			MOVE WS-PBODY-LOGO(1)	TO WS-1ST-LOGO
		END-IF
		IF 	WS-PBODY-LOGO(2) NOT = SPACES
		THEN 
			MOVE WS-3RD-LOGO 	TO WS-4TH-LOGO
			MOVE WS-2ND-LOGO 	TO WS-3RD-LOGO
			MOVE WS-PBODY-LOGO(2)	TO WS-2ND-LOGO
		END-IF
		IF 	WS-PBODY-LOGO(3) NOT = SPACES
		THEN 
			MOVE WS-3RD-LOGO 	TO WS-4TH-LOGO
			MOVE WS-PBODY-LOGO(3)	TO WS-3RD-LOGO
		END-IF
		IF 	WS-PBODY-LOGO(4) NOT = SPACES
		THEN 
			MOVE WS-PBODY-LOGO(4)	TO WS-4TH-LOGO
		END-IF
	    END-IF

	    STRING '*!*'       DELIMITED BY SIZE 
                   WS-1ST-LOGO DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER

	    PERFORM T-GET-NEXT-POSITION

	    STRING '*!*'       DELIMITED BY SIZE 
                   WS-2ND-LOGO DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER

	    PERFORM T-GET-NEXT-POSITION

	    STRING '*!*'       DELIMITED BY SIZE 
                   WS-3RD-LOGO DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER

	    PERFORM T-GET-NEXT-POSITION

	    STRING '*!*'       DELIMITED BY SIZE 
                   WS-4TH-LOGO DELIMITED BY SIZE
	    INTO   WS-ANS-REC 
            WITH   POINTER WS-POINTER
*
*	    RSH 08/08/2005 (RfW 04/0109):
*	    Output the triple logo for ESOL awards.
*
*	    EC2016 : New ESOL - Output Triple Logo even if no QAN
 
            IF      WS-COURSE-QCA-CODE NOT = SPACES
                OR  WS-AWARD-CODE = 'Q0'
		OR  WS-AWARD-CODE = 'Q1'
		OR  WS-AWARD-CODE = 'Q2'
		OR  WS-AWARD-CODE = 'Q3'
		OR  WS-AWARD-CODE = 'Q4'
		OR  WS-AWARD-CODE = 'Q5'
		OR  WS-AWARD-CODE = 'Q6'
		OR  WS-AWARD-CODE = 'Q7'
		OR  WS-AWARD-CODE = 'Q8'
		OR  WS-AWARD-CODE = 'Q9'
                OR  WS-AWARD-CODE = '63'
		OR  WS-AWARD-CODE = '93'
		OR  WS-AWARD-CODE = 'CC'
		OR  WS-AWARD-CODE = 'GG'
		OR  WS-AWARD-CODE = 'GA'
		OR  WS-AWARD-CODE = 'GB'
		OR  WS-AWARD-CODE = 'HH'
                OR  WS-AWARD-CODE = 'EW'
		OR  WS-AWARD-CODE = '35' AND WS-BTEC-TITLE NOT EQUAL '2250'
		OR  WS-AWARD-CODE = '11' AND WS-BTEC-TITLE EQUAL '2896'
		OR  WS-AWARD-CODE = 'VV'
		OR  WS-AWARD-CODE = 'FE'
       		OR  WS-AC-ACCRED-BODY = 'S'
            THEN

	      PERFORM T-GET-NEXT-POSITION
*EC2769
	      IF WS-ACCRED-LOGO = '8'
	      THEN
		STRING '*!*S LOGO'       DELIMITED BY SIZE 
		INTO   WS-ANS-REC 
		WITH   POINTER WS-POINTER
	      ELSE
	       IF WS-ACCRED-LOGO = '3'
               THEN
		STRING '*!*EWI LOGO'      DELIMITED BY SIZE 
		INTO   WS-ANS-REC 
		WITH   POINTER WS-POINTER
               ELSE
                IF WS-ACCRED-LOGO = '1'
                THEN
                 STRING '*!*W LOGO'      DELIMITED BY SIZE 
                 INTO   WS-ANS-REC 
                 WITH   POINTER WS-POINTER
                ELSE
                 IF WS-ACCRED-LOGO = '2'
                 THEN
                  STRING '*!*E LOGO' DELIMITED BY SIZE 
                  INTO   WS-ANS-REC 
                  WITH   POINTER WS-POINTER
                 ELSE
                  IF WS-ACCRED-LOGO = '4'
                  THEN
                      STRING '*!*I LOGO' DELIMITED BY SIZE 
                      INTO   WS-ANS-REC 
                      WITH   POINTER WS-POINTER
                  ELSE
                   IF WS-ACCRED-LOGO = '5'
                   THEN
                     STRING '*!*EW LOGO' DELIMITED BY SIZE 
                     INTO   WS-ANS-REC 
                     WITH   POINTER WS-POINTER
                   ELSE
                    IF WS-ACCRED-LOGO = '6'
                    THEN
                      STRING '*!*EI LOGO' DELIMITED BY SIZE 
                      INTO   WS-ANS-REC 
                      WITH   POINTER WS-POINTER
                    ELSE
                     IF WS-ACCRED-LOGO = '7'
                     THEN
                      STRING '*!*WI LOGO' DELIMITED BY SIZE 
                      INTO   WS-ANS-REC 
                      WITH   POINTER WS-POINTER 
                     ELSE
*EC2810
                      IF WS-ACCRED-LOGO = '0'
                      THEN
                       STRING '*!*NO LOGO' DELIMITED BY SIZE 
                       INTO   WS-ANS-REC 
                       WITH   POINTER WS-POINTER 
                      ELSE
                       STRING '*!*EWI LOGO' DELIMITED BY SIZE 
		       INTO   WS-ANS-REC 
		       WITH   POINTER WS-POINTER
                      END-IF
                     END-IF
                    END-IF
                   END-IF
                  END-IF
                 END-IF
                END-IF
	       END-IF
	      END-IF

              PERFORM T-GET-NEXT-POSITION
*wi409 get IRL flag
              PERFORM CRI-GET-CUST-CERT
*EC4424 Added DOC56       
              IF WS-DOC-REF = 'DOC7'  OR
                 WS-DOC-REF = 'DOC77' OR
                 WS-DOC-REF = 'DOC34' OR
                 WS-DOC-REF = 'DOC56'     
              THEN
                 IF WS-IRL-CC-FLAG = 'Y'
                 THEN
*EC1029 blank out IRL indicator
                    STRING '*!**!*' DELIMITED BY SIZE
                    INTO WS-ANS-REC
                    WITH POINTER WS-POINTER
                 ELSE
                    STRING '*!**!*' DELIMITED BY SIZE
                    INTO WS-ANS-REC
                    WITH POINTER WS-POINTER
                 END-IF
              END-IF
            ELSE

	      PERFORM T-GET-NEXT-POSITION

	      STRING '*!*'       DELIMITED BY SIZE 
	      INTO   WS-ANS-REC 
              WITH   POINTER WS-POINTER
*wi409 get IRL flag
              PERFORM CRI-GET-CUST-CERT
*EC4424 Added DOC56
              IF WS-DOC-REF = 'DOC7'  OR
                 WS-DOC-REF = 'DOC77' OR
                 WS-DOC-REF = 'DOC34' OR
                 WS-DOC-REF = 'DOC56' 
              THEN
                 IF WS-IRL-CC-FLAG = 'Y'
                 THEN
                    STRING '*!**!*' DELIMITED BY SIZE
                    INTO WS-ANS-REC
                    WITH POINTER WS-POINTER
                 ELSE
                    STRING '*!**!*' DELIMITED BY SIZE
                    INTO WS-ANS-REC
                    WITH POINTER WS-POINTER
                 END-IF
              END-IF

	    END-IF

	    PERFORM T-GET-NEXT-POSITION

	    COMPUTE WS-ANS-LENGTH = WS-POINTER - 1
*
	    WRITE PRINT-REC FROM WS-ANS-REC
*
	    MOVE 'D'		TO WS-AROF-RECORD-TYPE
	    MOVE WS-CENTRE-NO	TO WS-AROF-CENTRE
            MOVE 0		TO WS-AROF-CENTRE-I
	    MOVE WS-REG-NO	TO WS-AROF-REG-NO
            MOVE 0     		TO WS-AROF-REG-NO-I

	    PERFORM ZV-WRITE-TO-DB

	    MOVE ZERO TO WS-NUM-06-LINES

	    IF WS-MEMBERSHIP-FLAG = 'Y' 
	    THEN

	       MOVE SPACES TO WS-ANS-REC
	       MOVE 1 TO WS-POINTER
*
               STRING '06*!*' DELIMITED BY SIZE
                       WS-LINE-7A DELIMITED BY SIZE 
               INTO     WS-ANS-REC
               WITH     POINTER WS-POINTER
*
	       ADD 1 TO WS-NUM-06-LINES
               PERFORM T-GET-NEXT-POSITION
*       
               STRING '*!*'     DELIMITED BY SIZE
                       WS-LINE-7B     DELIMITED BY SIZE
                INTO   WS-ANS-REC
                WITH   POINTER WS-POINTER

	       ADD 1 TO WS-NUM-06-LINES

* Further message only applicable to award code 22
* and award titles 164, 2179 and 5258

	       IF WS-AWARD-CODE = '22'
	       THEN

		 IF WS-BTEC-TITLE = 164 OR 2179 OR 5258
		 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7C     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7D     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES

	         END-IF
*EC2796
                 IF WS-BTEC-TITLE = 23095
                 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7I     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                 END-IF
                 
                 IF WS-BTEC-TITLE = 23889
                 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7J     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                 END-IF

                 IF WS-BTEC-TITLE = 23849 OR 23850
                 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7K     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                 END-IF
	       ELSE

* Further message only applicable to award titles 9971,9972,9973 and 9974.

		 IF WS-BTEC-TITLE = 9971 OR 9972 OR 9973 OR 9974
		 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7F     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
	         END-IF
*EC2796
                 IF WS-BTEC-TITLE = 23095
                 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7I     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                 END-IF
                 
                 IF WS-BTEC-TITLE = 23889
                 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7J     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                 END-IF

                 IF WS-BTEC-TITLE = 23849 OR 23850
                 THEN

                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7K    DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                 END-IF
               END-IF
* Not :WS-MEMBERSHIP-FLAG = 'Y'
	    ELSE

	       IF WS-AWARD-CODE = '22'
	       THEN
 
		 IF WS-BTEC-TITLE = 164 OR 2179 OR 5258
		 THEN

		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7C DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7D     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
	         END-IF 
*EC2796
                 IF WS-BTEC-TITLE = 23095
		 THEN

		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7I DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
	         END-IF 

                 IF WS-BTEC-TITLE = 23889
		 THEN

		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7J DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
                   
	         END-IF 

                 IF WS-BTEC-TITLE = 23849 OR 23850
                 THEN

                   MOVE SPACES TO WS-ANS-REC
                   MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7K DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                   
                 END-IF 
	       ELSE

* Further message only applicable to award titles 9971,9972,9973 and 9974.

 		 IF WS-BTEC-TITLE = 9971 OR 9972 OR 9973 OR 9974
		 THEN

		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7F DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
	         END-IF
*EC2796
                 IF WS-BTEC-TITLE = 23095
		 THEN
                  
		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7I DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
	         END-IF 

                 IF WS-BTEC-TITLE = 23889
		 THEN

		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7J DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
                   
	         END-IF 

                 IF WS-BTEC-TITLE = 23849 OR 23850
                 THEN

                   MOVE SPACES TO WS-ANS-REC
                   MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7K DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

                   ADD 1 TO WS-NUM-06-LINES
                   
                 END-IF 
	       END-IF
            END-IF
                      
*
            IF WS-AWARD_CODE = '1W'
	    OR WS-AWARD-CODE = '2W'
	    OR WS-AWARD-CODE = '3W'
	    OR WS-AWARD-CODE = '4W'
	    THEN
		IF WS-NUM-06-LINES = 0
		THEN
		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER
                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7G DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER
	           ADD 1 TO WS-NUM-06-LINES
                   PERFORM T-GET-NEXT-POSITION
                   STRING '*!*' DELIMITED BY SIZE
                          WS-LINE-7H DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER
	           ADD 1 TO WS-NUM-06-LINES
                ELSE
                   PERFORM T-GET-NEXT-POSITION
                   STRING '*!*' DELIMITED BY SIZE
                          WS-LINE-7G DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER
	           ADD 1 TO WS-NUM-06-LINES
                   PERFORM T-GET-NEXT-POSITION
                   STRING '*!*' DELIMITED BY SIZE
                          WS-LINE-7H DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER
	           ADD 1 TO WS-NUM-06-LINES

	        END-IF
	    END-IF
                      
*	    Check that there is still room for a certificate validity message
*	    if one is required.
*
	    IF WS-YEARS-CERT-VALID NOT EQUAL SPACES
	    THEN
	      IF WS-NUM-06-LINES < WS-MAX-06-LINES
	      THEN

                MOVE WS-YEARS-CERT-VALID TO PL7E-YEARS-VALID

		IF WS-NUM-06-LINES = 0
		THEN
		   MOVE SPACES TO WS-ANS-REC
	           MOVE 1 TO WS-POINTER

                   STRING '06*!*' DELIMITED BY SIZE
                          WS-LINE-7E DELIMITED BY SIZE 
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
		ELSE
                   PERFORM T-GET-NEXT-POSITION

                   STRING '*!*'     DELIMITED BY SIZE
                          WS-LINE-7E     DELIMITED BY SIZE
                   INTO   WS-ANS-REC
                   WITH   POINTER WS-POINTER

	           ADD 1 TO WS-NUM-06-LINES
		END-IF

                COMPUTE WS-ANS-LENGTH = WS-POINTER - 1
                WRITE PRINT-REC FROM WS-ANS-REC
                PERFORM T-GET-NEXT-POSITION

		MOVE 'D'		TO WS-AROF-RECORD-TYPE
	    	MOVE WS-CENTRE-NO	TO WS-AROF-CENTRE
		MOVE 0			TO WS-AROF-CENTRE-I
	    	MOVE WS-REG-NO		TO WS-AROF-REG-NO
		MOVE 0			TO WS-AROF-REG-NO-I

		PERFORM ZV-WRITE-TO-DB

	      ELSE
                COMPUTE WS-ANS-LENGTH = WS-POINTER - 1
                WRITE PRINT-REC FROM WS-ANS-REC
                PERFORM T-GET-NEXT-POSITION

		MOVE 'D'		TO WS-AROF-RECORD-TYPE
	    	MOVE WS-CENTRE-NO	TO WS-AROF-CENTRE
		MOVE 0			TO WS-AROF-CENTRE-I
	    	MOVE WS-REG-NO		TO WS-AROF-REG-NO
		MOVE 0			TO WS-AROF-REG-NO-I

		PERFORM ZV-WRITE-TO-DB

		DISPLAY 'NO ROOM FOR VALIDITY STATEMENT'
		MOVE WS-REG-NO TO VALIDITY-REG-NO
		WRITE REJECT-REC FROM WS-VALIDITY-WARNING
	      END-IF
            ELSE
	      IF WS-NUM-06-LINES > 0
	      THEN
                COMPUTE WS-ANS-LENGTH = WS-POINTER - 1
                WRITE PRINT-REC FROM WS-ANS-REC
                PERFORM T-GET-NEXT-POSITION

		MOVE 'D'		TO WS-AROF-RECORD-TYPE
	    	MOVE WS-CENTRE-NO	TO WS-AROF-CENTRE
		MOVE 0			TO WS-AROF-CENTRE-I
	    	MOVE WS-REG-NO		TO WS-AROF-REG-NO
		MOVE 0			TO WS-AROF-REG-NO-I

		PERFORM ZV-WRITE-TO-DB

	      END-IF
            END-IF

	END-IF.
*
* WI324 - new 88 record - start
	MOVE SPACES TO WS-ANS-REC.
	MOVE SPACES TO WS-88REC.
*       EXEC SQL WHENEVER NOT FOUND CONTINUE     END-EXEC.
*       EXEC SQL
*       	SELECT	ST_SCHEME_REG_NO			||'*!*'||
*       		TO_CHAR(ST_BIRTH_DATE,'DD/MM/YY')	||'*!*'||
*       	       	ST_CENTRE_ID    			||'*!*'||
*       	       	ST_CENTRE_REF   			||'*!*'||
*       	       	ST_COHORT       			||'*!*'||
*       		NVL(ST_COURSE_ID,ST_NVQ_REGISTERED_ID)	||'*!*'||
*       	       	SCHEME_CODE                     	||'*!*'||
*       	       	TO_CHAR(NVL(NVL(ST_AWARD_ISSUE,
*                        ST_NVQ_CERTIFICATE_PRINT_DATE),
*       		 TO_DATE(:WS-RUN-DATE,'DD-MON-YYYY')),
*       		 'DD/MM/YY') 				||'*!*'||
*       	       	ST_FORENAMES                    	||'*!*'||
*       	       	ST_SURNAME                      	
*       	INTO	:WS-88REC
*       	FROM	SCHEMES,
*       		STUDENTS
*       	WHERE	SCHEME_ID(+) = ST_SCHEME_ID
*       	AND	ST_REG_NO    = :WS-REG-NO
*       END-EXEC.
        CALL "SQLADR" USING SQ0031 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1383 TO SQL-OFFSET
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
            WS-RUN-DATE
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-88REC
            SQL-SQHSTV(2)
        MOVE 200 TO SQL-SQHSTL(2)
        MOVE 200 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(3)
        MOVE 7 TO SQL-SQHSTL(3)
        MOVE 7 TO SQL-SQHSTS(3)
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
            THEN GO TO D1C-100 END-IF.
	IF WS-88REC EQUAL SPACES
	THEN
	  MOVE '*!**!**!**!**!**!**!**!**!*' TO WS-88REC
	END-IF.
	MOVE 1 TO WS-POINTER.
        STRING '88*!*' 		DELIMITED BY SIZE
               WS-REG-NO 	DELIMITED BY SIZE
               '*!*' 		DELIMITED BY SIZE
               WS-88REC		DELIMITED BY SIZE
        INTO   WS-ANS-REC
        WITH   POINTER WS-POINTER.
	COMPUTE WS-ANS-LENGTH = WS-POINTER - 1.
	WRITE PRINT-REC FROM WS-ANS-REC.
	MOVE 'D'		TO WS-AROF-RECORD-TYPE.
	MOVE WS-CENTRE-NO	TO WS-AROF-CENTRE.
        MOVE 0			TO WS-AROF-CENTRE-I.
	MOVE WS-REG-NO		TO WS-AROF-REG-NO.
        MOVE 0     		TO WS-AROF-REG-NO-I.
	PERFORM ZV-WRITE-TO-DB.
* WI324 - new 88 record - end

*
* IF THE RUN TYPE IS 'S' AND DO  NOT UPDATE THE STUDENT RECORD
* RSH 28/11/2007 (LQ35232): Process the reprint fee record (reprint only).
*
	IF	WS-IP-RUN-TYPE = 'S'
		IF	(
				S-FIRST-FEE-ID > 0
				AND
   				S-LAST-FEE-ID  > 0
			)
		THEN
			PERFORM DABBDA-UPDATE-FEE-STATUS
		END-IF

		GO TO DABBD-500
	END-IF.

        PERFORM E-UPDATE-STUDENT.
        PERFORM EA-WRITE-LABEL.
	PERFORM EC-INSERT-REPRINT-LOGOS.
*
DABBD-500.
        MOVE SPACES TO WS-REJECT-NO(21).
	MOVE SPACES TO WS-REJECT-NO(22).
*
*	RSH 17/11/2005 (RfW 05/0186):
*	Reset the expired NVQ units error flag.
*	
	MOVE SPACES TO WS-REJECT-NO(20).
*
        RETURN SORT-FILE RECORD
	       AT END MOVE 'Y' TO WS-EOF-IND.
DABBD-EXIT.
	EXIT.
*
/
DABBDA-UPDATE-FEE-STATUS SECTION.
********************************************************************************
*    RSH 28/11/2007 (LQ35232):                                                 *
*    THIS SECTION UPDATES THE REPRINT FEE STATUS.                              *
********************************************************************************
DABBDA-START.

*       EXEC SQL WHENEVER SQLERROR   GO TO DABBDA-100	END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE     	END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  CONTINUE     	END-EXEC.

	MOVE S-FIRST-FEE-ID	TO WS-FIRST-REG-RCFE-ID.
	MOVE S-LAST-FEE-ID	TO WS-LAST-REG-RCFE-ID.

*       EXEC SQL
*       	UPDATE	reprint_certificate_fees
*       	SET	rcfe_status		=	'O'
*       	,	rcfe_reprint_date	=	SYSDATE
*       	,	rcfe_update_date	=	SYSDATE
*       	,	rcfe_update_user	=	'STP070'
*       	WHERE	rcfe_id			between	:WS-FIRST-REG-RCFE-ID
*       					and	:WS-LAST-REG-RCFE-ID
*       	AND	rcfe_st_reg_no		=	:WS-REG-NO
*       	AND	rcfe_status		=	'R'
*       END-EXEC.
        CALL "SQLADR" USING SQ0032 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1410 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-FIRST-REG-RCFE-ID
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-LAST-REG-RCFE-ID
            SQL-SQHSTV(2)
        MOVE 4 TO SQL-SQHSTL(2)
        MOVE 4 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(3)
        MOVE 7 TO SQL-SQHSTL(3)
        MOVE 7 TO SQL-SQHSTS(3)
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
            THEN GO TO DABBDA-100 END-IF.

	GO TO DABBDA-EXIT.
                   
DABBDA-100.

	MOVE 'REPRINT FEE UPDATE 1 FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.

DABBDA-EXIT.
	EXIT.
*
/
DAC-CENTRE-NAME SECTION.
********************************************************************************
*    THIS SECTION OBTAINS THE CENTRE NAME. IF IT IS NOT FOUND, AN APPROPRIATE  *
*	  MESSAGE IS PRODUCED ON THE REPORT.                                   *
********************************************************************************
DAC-START.
 	INITIALIZE WS-MERGED-CENTRE-NO,
                   WS-CENTRE-NAME-1,
		   WS-CENTRE-NAME-2.
*
        MOVE SPACES TO WS-REJECT-NO(22).
        MOVE SPACES TO WS-REJECT-NO(1).        
*
*       EXEC SQL WHENEVER SQLERROR    GO TO DAC-100 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING  CONTINUE      END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND   GO TO DAC-200 END-EXEC.
*
*	DISPLAY "CENtre is ",ws-centre-no.
*	DISPLAY "award date is ",ws-award-date.	
*	DISPLAY "reg# is ",ws-reg-no.
*       EXEC SQL EXECUTE
*        BEGIN
*         PK_CENTRE_NAMES.PR_GET_CENTRE_CERT_NAMES
*                                 (:WS-CENTRE-NO
*                                 ,TO_DATE(:WS-AWARD-DATE,'DD-MON-YYYY')
*                                 ,:WS-REG-NO
*                                 ,:WS-MERGED-CENTRE-NO:WS-MERGED-CENTRE-NO-I
*   		                  ,:WS-CENTRE-NAME-1   :WS-CENTRE-NAME-1-I   
*       		          ,:WS-CENTRE-NAME-2   :WS-CENTRE-NAME-2-I   
*                                 ,:WS-PR-RESULT
*                                 );
*        END;
*       END-EXEC.
        CALL "SQLADR" USING SQ0033 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1437 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-CENTRE-NO
            SQL-SQHSTV(1)
        MOVE 6 TO SQL-SQHSTL(1)
        MOVE 6 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-AWARD-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(3)
        MOVE 7 TO SQL-SQHSTL(3)
        MOVE 7 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-MERGED-CENTRE-NO
            SQL-SQHSTV(4)
        MOVE 6 TO SQL-SQHSTL(4)
        MOVE 6 TO SQL-SQHSTS(4)
        CALL "SQLADR" USING
            WS-MERGED-CENTRE-NO-I
            SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-CENTRE-NAME-1
            SQL-SQHSTV(5)
        MOVE 45 TO SQL-SQHSTL(5)
        MOVE 45 TO SQL-SQHSTS(5)
        CALL "SQLADR" USING
            WS-CENTRE-NAME-1-I
            SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-CENTRE-NAME-2
            SQL-SQHSTV(6)
        MOVE 45 TO SQL-SQHSTL(6)
        MOVE 45 TO SQL-SQHSTS(6)
        CALL "SQLADR" USING
            WS-CENTRE-NAME-2-I
            SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-PR-RESULT
            SQL-SQHSTV(7)
        MOVE 2 TO SQL-SQHSTL(7)
        MOVE 2 TO SQL-SQHSTS(7)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO DAC-200 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO DAC-100 END-IF.
*
*	display "result is ",ws-pr-result.
        IF WS-PR-RESULT NOT EQUAL ZERO
        THEN
         GO TO DAC-200
        END-IF.
                     
	GO TO DAC-EXIT.
*
DAC-100.
	MOVE 'CENTRE NAME SELECT FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
DAC-200.
	MOVE 'Y' TO WS-REJECT-NO(1).
        GO TO DAC-EXIT.
DAC-EXIT.
	EXIT.
*
E-UPDATE-STUDENT SECTION.
********************************************************************************
*    THIS SECTION UPDATES THE STUDENTS TABLE.                                  *
********************************************************************************
E-START.
	MOVE S-REG-NO TO WS-REG-NO.
*
E-010.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO E-030  END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE     END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO E-030  END-EXEC.
*                                    
*       EXEC SQL 
*            UPDATE BTEC.STUDENTS
*            SET ST_AWARD_PRINTED = 
*       		TO_DATE(:WS-CURRENT-AWARD-DATE,'DD-MON-YYYY'), 
*                ST_AWARD_ISSUE   = 
*       		TO_DATE(:WS-RUN-DATE,'DD-MON-YYYY'),
*                ST_CERT_NO       = :WS-ALLOC-CERT-NO,
*                ST_AWARD_CODE    = :WS-AWARD-CODE
*            WHERE ST_REG_NO      = :WS-REG-NO
*       END-EXEC.
        CALL "SQLADR" USING SQ0034 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1480 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-CURRENT-AWARD-DATE
            SQL-SQHSTV(1)
        MOVE 11 TO SQL-SQHSTL(1)
        MOVE 11 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RUN-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-ALLOC-CERT-NO
            SQL-SQHSTV(3)
        MOVE 5 TO SQL-SQHSTL(3)
        MOVE 5 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-AWARD-CODE
            SQL-SQHSTV(4)
        MOVE 2 TO SQL-SQHSTL(4)
        MOVE 2 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(5)
        MOVE 7 TO SQL-SQHSTL(5)
        MOVE 7 TO SQL-SQHSTS(5)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO E-030 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO E-030 END-IF.
*
        ADD 1 TO WS-ALLOC-CERT-NO.
	GO TO E-EXIT.
E-030.
	MOVE 'STUDENT UPDATE FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
E-EXIT.
	EXIT.
*
/
EA-WRITE-LABEL SECTION.
********************************************************************************
* Add centre to labels file
********************************************************************************
EA-START.
	MOVE S-CENTRE-NO TO LAB-CENTRE-NO.
	WRITE LABELS-REC.
EA-EXIT.
	EXIT.
/
EB-DELETE-REPRINT-AWARD-DATA SECTION.
*
*  AT END OF JOB FOR REPRINT AWARDS - DELETE THE RANGE DATA FROM THE TABLE
*
EB-010.
*
*   RSH 28/11/2007 (LQ35232): Store a copy of each reprint request
*   before they are deleted.
*
*       EXEC SQL WHENEVER SQLERROR	GO TO EB-900	END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE	END-EXEC.
*       EXEC SQL WHENEVER NOTFOUND	CONTINUE	END-EXEC.

*       EXEC SQL
*       	INSERT INTO
*       		ALL_REPRINT_CRITERIA_HIST
*       		(
*       			ARCH_RUN_DATE
*       		,	ARCH_INSERT_DATE
*       		,	ARCH_REPRINT_TYPE
*       		,	ARCH_FIRST_REG_NO
*       		,	ARCH_LAST_REG_NO
*       		,	ARCH_AWARD_DATE
*       		,	ARCH_AWARD_CODE
*       		,	ARCH_NOPS
*       		,	ARCH_UC_NAME
*       		,	ARCH_CRED_TRANS
*       		,	ARCH_REPRINT_WORDING
*       		,	ARCH_CUC
*       		,	ARCH_REPRINT_CHARGE
*       		,	ARCH_FIRST_REG_RCFE_ID
*       		,	ARCH_LAST_REG_RCFE_ID
*       		,	ARCH_DB
*                       ,       ARCH_RETURN_DOC
*                       ,       ARCH_COMMENT    
*                       ,       ARCH_RCCO_ID    
*       		)
*       	SELECT
*       			trunc(SYSDATE)
*       		,	SYSDATE
*       		,	ARC_REPRINT_TYPE
*       		,	ARC_FIRST_REG_NO
*       		,	ARC_LAST_REG_NO
*       		,	ARC_AWARD_DATE
*       		,	ARC_AWARD_CODE
*       		,	ARC_NOPS
*       		,	ARC_UC_NAME
*       		,	ARC_CRED_TRANS
*       		,	ARC_REPRINT_WORDING
*       		,	ARC_CUC
*       		,	ARC_REPRINT_CHARGE
*       		,	ARC_FIRST_REG_RCFE_ID
*       		,	ARC_LAST_REG_RCFE_ID
*       		,	ARC_DB
*                       ,       ARC_RETURN_DOC
*                       ,       ARC_COMMENT    
*                       ,       ARC_RCCO_ID    
*       	FROM
*       		ALL_REPRINT_CRITERIA
*       	WHERE
*       		ARC_REPRINT_TYPE	=	'B'
*   	END-EXEC.
        CALL "SQLADR" USING SQ0035 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1515 TO SQL-OFFSET
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
            THEN GO TO EB-900 END-IF.

EB-020.

*       EXEC SQL WHENEVER SQLERROR	GO TO EB-910	END-EXEC.

*       EXEC SQL
*       	DELETE FROM
*       		ALL_REPRINT_CRITERIA
*       	WHERE
*       		ARC_REPRINT_TYPE = 'B'
*       END-EXEC.
        CALL "SQLADR" USING SQ0036 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1530 TO SQL-OFFSET
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
            THEN GO TO EB-910 END-IF.

	GO TO EB-999.

EB-900.
	MOVE 'COPY TO ALL_REPRINT_CRITERIA_HIST FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.

EB-910.
	MOVE 'DELETE FROM ALL_REPRINT_CRITERIA FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.

EB-999.
	EXIT.
/
EC-INSERT-REPRINT-LOGOS SECTION.
********************************************************************************
*	If logos are used then store the info for future reprints.             *
*
*
*	Patch - 191098 - Jeff Daniels - check row not already in table
*			 See amendment log in title
*
********************************************************************************
EC-START.
*
*       EXEC SQL WHENEVER SQLERROR GO TO EC-010  END-EXEC.
*
	IF	WS-1ST-LOGO NOT = SPACES
	THEN
*
*               EXEC SQL 
*       	INSERT INTO STUDENT_CERTIFICATE_LOGOS (
*       	        SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		SCLO_LPOS_ID)
*       	SELECT
*       		:WS-REG-NO,
*       		DECODE(INSTR(:WS-1ST-LOGO,'('),
*              		       0,
*              		       :WS-1ST-LOGO,
*              		       SUBSTR(:WS-1ST-LOGO,1,INSTR(:WS-1ST-LOGO,'(')-1)
*             		      ),
*       		DECODE(INSTR(:WS-1ST-LOGO,'('),
*              		       0,
*              		       NULL,
*              		       SUBSTR(:WS-1ST-LOGO,
*       			      INSTR(:WS-1ST-LOGO,'(')+1,
*       			      LENGTH(:WS-1ST-LOGO)
*       				-(INSTR(:WS-1ST-LOGO,')')-1))
*             		      )
*       	FROM
*       		DUAL
*       	MINUS
*       		SELECT
*       		SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		TO_CHAR(SCLO_LPOS_ID)
*       	FROM
*       		STUDENT_CERTIFICATE_LOGOS
*       	WHERE	SCLO_ST_REG_NO = :WS-REG-NO 
*       	END-EXEC
                CALL "SQLADR" USING SQ0037 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1545 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
                MOVE 0 TO SQL-SQINDV(1)
                MOVE 0 TO SQL-SQINDS(1)
                MOVE 0 TO SQL-SQHARM(1)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(2)
                MOVE 11 TO SQL-SQHSTL(2)
                MOVE 11 TO SQL-SQHSTS(2)
                MOVE 0 TO SQL-SQINDV(2)
                MOVE 0 TO SQL-SQINDS(2)
                MOVE 0 TO SQL-SQHARM(2)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(3)
                MOVE 11 TO SQL-SQHSTL(3)
                MOVE 11 TO SQL-SQHSTS(3)
                MOVE 0 TO SQL-SQINDV(3)
                MOVE 0 TO SQL-SQINDS(3)
                MOVE 0 TO SQL-SQHARM(3)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(4)
                MOVE 11 TO SQL-SQHSTL(4)
                MOVE 11 TO SQL-SQHSTS(4)
                MOVE 0 TO SQL-SQINDV(4)
                MOVE 0 TO SQL-SQINDS(4)
                MOVE 0 TO SQL-SQHARM(4)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(5)
                MOVE 11 TO SQL-SQHSTL(5)
                MOVE 11 TO SQL-SQHSTS(5)
                MOVE 0 TO SQL-SQINDV(5)
                MOVE 0 TO SQL-SQINDS(5)
                MOVE 0 TO SQL-SQHARM(5)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(6)
                MOVE 11 TO SQL-SQHSTL(6)
                MOVE 11 TO SQL-SQHSTS(6)
                MOVE 0 TO SQL-SQINDV(6)
                MOVE 0 TO SQL-SQINDS(6)
                MOVE 0 TO SQL-SQHARM(6)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(7)
                MOVE 11 TO SQL-SQHSTL(7)
                MOVE 11 TO SQL-SQHSTS(7)
                MOVE 0 TO SQL-SQINDV(7)
                MOVE 0 TO SQL-SQINDS(7)
                MOVE 0 TO SQL-SQHARM(7)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(8)
                MOVE 11 TO SQL-SQHSTL(8)
                MOVE 11 TO SQL-SQHSTS(8)
                MOVE 0 TO SQL-SQINDV(8)
                MOVE 0 TO SQL-SQINDS(8)
                MOVE 0 TO SQL-SQHARM(8)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(9)
                MOVE 11 TO SQL-SQHSTL(9)
                MOVE 11 TO SQL-SQHSTS(9)
                MOVE 0 TO SQL-SQINDV(9)
                MOVE 0 TO SQL-SQINDS(9)
                MOVE 0 TO SQL-SQHARM(9)
                CALL "SQLADR" USING
                    WS-1ST-LOGO
                    SQL-SQHSTV(10)
                MOVE 11 TO SQL-SQHSTL(10)
                MOVE 11 TO SQL-SQHSTS(10)
                MOVE 0 TO SQL-SQINDV(10)
                MOVE 0 TO SQL-SQINDS(10)
                MOVE 0 TO SQL-SQHARM(10)
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(11)
                MOVE 7 TO SQL-SQHSTL(11)
                MOVE 7 TO SQL-SQHSTS(11)
                MOVE 0 TO SQL-SQINDV(11)
                MOVE 0 TO SQL-SQINDS(11)
                MOVE 0 TO SQL-SQHARM(11)
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
                    THEN GO TO EC-010 END-IF
*
	END-IF.
*
	IF	WS-2ND-LOGO NOT = SPACES
	THEN
*
*               EXEC SQL 
*       	INSERT INTO STUDENT_CERTIFICATE_LOGOS (
*       	        SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		SCLO_LPOS_ID)
*       	SELECT
*       		:WS-REG-NO,
*       		DECODE(INSTR(:WS-2ND-LOGO,'('),
*              		       0,
*              		       :WS-2ND-LOGO,
*              		       SUBSTR(:WS-2ND-LOGO,1,INSTR(:WS-2ND-LOGO,'(')-1)
*             		      ),
*       		DECODE(INSTR(:WS-2ND-LOGO,'('),
*              		       0,
*              		       NULL,
*              		       SUBSTR(:WS-2ND-LOGO,
*       			      INSTR(:WS-2ND-LOGO,'(')+1,
*       			      LENGTH(:WS-2ND-LOGO)
*       				-(INSTR(:WS-2ND-LOGO,')')-1))
*             		      )
*       	FROM
*       		DUAL
*       	MINUS
*       		SELECT
*       		SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		TO_CHAR(SCLO_LPOS_ID)
*       	FROM
*       		STUDENT_CERTIFICATE_LOGOS
*       	WHERE	SCLO_ST_REG_NO = :WS-REG-NO 
*       	END-EXEC
                CALL "SQLADR" USING SQ0038 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1604 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
                MOVE 0 TO SQL-SQINDV(1)
                MOVE 0 TO SQL-SQINDS(1)
                MOVE 0 TO SQL-SQHARM(1)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(2)
                MOVE 11 TO SQL-SQHSTL(2)
                MOVE 11 TO SQL-SQHSTS(2)
                MOVE 0 TO SQL-SQINDV(2)
                MOVE 0 TO SQL-SQINDS(2)
                MOVE 0 TO SQL-SQHARM(2)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(3)
                MOVE 11 TO SQL-SQHSTL(3)
                MOVE 11 TO SQL-SQHSTS(3)
                MOVE 0 TO SQL-SQINDV(3)
                MOVE 0 TO SQL-SQINDS(3)
                MOVE 0 TO SQL-SQHARM(3)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(4)
                MOVE 11 TO SQL-SQHSTL(4)
                MOVE 11 TO SQL-SQHSTS(4)
                MOVE 0 TO SQL-SQINDV(4)
                MOVE 0 TO SQL-SQINDS(4)
                MOVE 0 TO SQL-SQHARM(4)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(5)
                MOVE 11 TO SQL-SQHSTL(5)
                MOVE 11 TO SQL-SQHSTS(5)
                MOVE 0 TO SQL-SQINDV(5)
                MOVE 0 TO SQL-SQINDS(5)
                MOVE 0 TO SQL-SQHARM(5)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(6)
                MOVE 11 TO SQL-SQHSTL(6)
                MOVE 11 TO SQL-SQHSTS(6)
                MOVE 0 TO SQL-SQINDV(6)
                MOVE 0 TO SQL-SQINDS(6)
                MOVE 0 TO SQL-SQHARM(6)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(7)
                MOVE 11 TO SQL-SQHSTL(7)
                MOVE 11 TO SQL-SQHSTS(7)
                MOVE 0 TO SQL-SQINDV(7)
                MOVE 0 TO SQL-SQINDS(7)
                MOVE 0 TO SQL-SQHARM(7)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(8)
                MOVE 11 TO SQL-SQHSTL(8)
                MOVE 11 TO SQL-SQHSTS(8)
                MOVE 0 TO SQL-SQINDV(8)
                MOVE 0 TO SQL-SQINDS(8)
                MOVE 0 TO SQL-SQHARM(8)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(9)
                MOVE 11 TO SQL-SQHSTL(9)
                MOVE 11 TO SQL-SQHSTS(9)
                MOVE 0 TO SQL-SQINDV(9)
                MOVE 0 TO SQL-SQINDS(9)
                MOVE 0 TO SQL-SQHARM(9)
                CALL "SQLADR" USING
                    WS-2ND-LOGO
                    SQL-SQHSTV(10)
                MOVE 11 TO SQL-SQHSTL(10)
                MOVE 11 TO SQL-SQHSTS(10)
                MOVE 0 TO SQL-SQINDV(10)
                MOVE 0 TO SQL-SQINDS(10)
                MOVE 0 TO SQL-SQHARM(10)
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(11)
                MOVE 7 TO SQL-SQHSTL(11)
                MOVE 7 TO SQL-SQHSTS(11)
                MOVE 0 TO SQL-SQINDV(11)
                MOVE 0 TO SQL-SQINDS(11)
                MOVE 0 TO SQL-SQHARM(11)
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
                    THEN GO TO EC-010 END-IF
*
	END-IF.
*
	IF	WS-3RD-LOGO NOT = SPACES
	THEN
*
*               EXEC SQL 
*       	INSERT INTO STUDENT_CERTIFICATE_LOGOS (
*       	        SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		SCLO_LPOS_ID)
*       	SELECT
*       		:WS-REG-NO,
*       		DECODE(INSTR(:WS-3RD-LOGO,'('),
*              		       0,
*              		       :WS-3RD-LOGO,
*              		       SUBSTR(:WS-3RD-LOGO,1,INSTR(:WS-3RD-LOGO,'(')-1)
*             		      ),
*       		DECODE(INSTR(:WS-3RD-LOGO,'('),
*              		       0,
*              		       NULL,
*              		       SUBSTR(:WS-3RD-LOGO,
*       			      INSTR(:WS-3RD-LOGO,'(')+1,
*       			      LENGTH(:WS-3RD-LOGO)
*       				-(INSTR(:WS-3RD-LOGO,')')-1))
*             		      )
*       	FROM
*       		DUAL
*       	MINUS
*       		SELECT
*       		SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		TO_CHAR(SCLO_LPOS_ID)
*       	FROM
*       		STUDENT_CERTIFICATE_LOGOS
*       	WHERE	SCLO_ST_REG_NO = :WS-REG-NO 
*       	END-EXEC
                CALL "SQLADR" USING SQ0039 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1663 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
                MOVE 0 TO SQL-SQINDV(1)
                MOVE 0 TO SQL-SQINDS(1)
                MOVE 0 TO SQL-SQHARM(1)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(2)
                MOVE 11 TO SQL-SQHSTL(2)
                MOVE 11 TO SQL-SQHSTS(2)
                MOVE 0 TO SQL-SQINDV(2)
                MOVE 0 TO SQL-SQINDS(2)
                MOVE 0 TO SQL-SQHARM(2)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(3)
                MOVE 11 TO SQL-SQHSTL(3)
                MOVE 11 TO SQL-SQHSTS(3)
                MOVE 0 TO SQL-SQINDV(3)
                MOVE 0 TO SQL-SQINDS(3)
                MOVE 0 TO SQL-SQHARM(3)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(4)
                MOVE 11 TO SQL-SQHSTL(4)
                MOVE 11 TO SQL-SQHSTS(4)
                MOVE 0 TO SQL-SQINDV(4)
                MOVE 0 TO SQL-SQINDS(4)
                MOVE 0 TO SQL-SQHARM(4)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(5)
                MOVE 11 TO SQL-SQHSTL(5)
                MOVE 11 TO SQL-SQHSTS(5)
                MOVE 0 TO SQL-SQINDV(5)
                MOVE 0 TO SQL-SQINDS(5)
                MOVE 0 TO SQL-SQHARM(5)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(6)
                MOVE 11 TO SQL-SQHSTL(6)
                MOVE 11 TO SQL-SQHSTS(6)
                MOVE 0 TO SQL-SQINDV(6)
                MOVE 0 TO SQL-SQINDS(6)
                MOVE 0 TO SQL-SQHARM(6)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(7)
                MOVE 11 TO SQL-SQHSTL(7)
                MOVE 11 TO SQL-SQHSTS(7)
                MOVE 0 TO SQL-SQINDV(7)
                MOVE 0 TO SQL-SQINDS(7)
                MOVE 0 TO SQL-SQHARM(7)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(8)
                MOVE 11 TO SQL-SQHSTL(8)
                MOVE 11 TO SQL-SQHSTS(8)
                MOVE 0 TO SQL-SQINDV(8)
                MOVE 0 TO SQL-SQINDS(8)
                MOVE 0 TO SQL-SQHARM(8)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(9)
                MOVE 11 TO SQL-SQHSTL(9)
                MOVE 11 TO SQL-SQHSTS(9)
                MOVE 0 TO SQL-SQINDV(9)
                MOVE 0 TO SQL-SQINDS(9)
                MOVE 0 TO SQL-SQHARM(9)
                CALL "SQLADR" USING
                    WS-3RD-LOGO
                    SQL-SQHSTV(10)
                MOVE 11 TO SQL-SQHSTL(10)
                MOVE 11 TO SQL-SQHSTS(10)
                MOVE 0 TO SQL-SQINDV(10)
                MOVE 0 TO SQL-SQINDS(10)
                MOVE 0 TO SQL-SQHARM(10)
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(11)
                MOVE 7 TO SQL-SQHSTL(11)
                MOVE 7 TO SQL-SQHSTS(11)
                MOVE 0 TO SQL-SQINDV(11)
                MOVE 0 TO SQL-SQINDS(11)
                MOVE 0 TO SQL-SQHARM(11)
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
                    THEN GO TO EC-010 END-IF
*
	END-IF.
*
	IF	WS-4TH-LOGO NOT = SPACES
	THEN
*
*               EXEC SQL 
*       	INSERT INTO STUDENT_CERTIFICATE_LOGOS (
*       	        SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		SCLO_LPOS_ID)
*       	SELECT
*       		:WS-REG-NO,
*       		DECODE(INSTR(:WS-4TH-LOGO,'('),
*              		       0,
*              		       :WS-4TH-LOGO,
*              		       SUBSTR(:WS-4TH-LOGO,1,INSTR(:WS-4TH-LOGO,'(')-1)
*             		      ),
*       		DECODE(INSTR(:WS-4TH-LOGO,'('),
*              		       0,
*              		       NULL,
*              		       SUBSTR(:WS-4TH-LOGO,
*       			      INSTR(:WS-4TH-LOGO,'(')+1,
*       			      LENGTH(:WS-4TH-LOGO)
*       				-(INSTR(:WS-4TH-LOGO,')')-1))
*             		      )
*       	FROM
*       		DUAL
*       	MINUS
*       		SELECT
*       		SCLO_ST_REG_NO,
*       		SCLO_CLOG_CODE,
*       		TO_CHAR(SCLO_LPOS_ID)
*       	FROM
*       		STUDENT_CERTIFICATE_LOGOS
*       	WHERE	SCLO_ST_REG_NO = :WS-REG-NO 
*       	END-EXEC
                CALL "SQLADR" USING SQ0040 SQL-STMT
                MOVE 1 TO SQL-ITERS
                MOVE 1722 TO SQL-OFFSET
                MOVE 0 TO SQL-OCCURS
                CALL "SQLADR" USING
                    SQLCUD
                    SQL-CUD
                CALL "SQLADR" USING
                    SQLCA
                    SQL-SQLEST
                MOVE 4352 TO SQL-SQLETY
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(1)
                MOVE 7 TO SQL-SQHSTL(1)
                MOVE 7 TO SQL-SQHSTS(1)
                MOVE 0 TO SQL-SQINDV(1)
                MOVE 0 TO SQL-SQINDS(1)
                MOVE 0 TO SQL-SQHARM(1)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(2)
                MOVE 11 TO SQL-SQHSTL(2)
                MOVE 11 TO SQL-SQHSTS(2)
                MOVE 0 TO SQL-SQINDV(2)
                MOVE 0 TO SQL-SQINDS(2)
                MOVE 0 TO SQL-SQHARM(2)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(3)
                MOVE 11 TO SQL-SQHSTL(3)
                MOVE 11 TO SQL-SQHSTS(3)
                MOVE 0 TO SQL-SQINDV(3)
                MOVE 0 TO SQL-SQINDS(3)
                MOVE 0 TO SQL-SQHARM(3)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(4)
                MOVE 11 TO SQL-SQHSTL(4)
                MOVE 11 TO SQL-SQHSTS(4)
                MOVE 0 TO SQL-SQINDV(4)
                MOVE 0 TO SQL-SQINDS(4)
                MOVE 0 TO SQL-SQHARM(4)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(5)
                MOVE 11 TO SQL-SQHSTL(5)
                MOVE 11 TO SQL-SQHSTS(5)
                MOVE 0 TO SQL-SQINDV(5)
                MOVE 0 TO SQL-SQINDS(5)
                MOVE 0 TO SQL-SQHARM(5)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(6)
                MOVE 11 TO SQL-SQHSTL(6)
                MOVE 11 TO SQL-SQHSTS(6)
                MOVE 0 TO SQL-SQINDV(6)
                MOVE 0 TO SQL-SQINDS(6)
                MOVE 0 TO SQL-SQHARM(6)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(7)
                MOVE 11 TO SQL-SQHSTL(7)
                MOVE 11 TO SQL-SQHSTS(7)
                MOVE 0 TO SQL-SQINDV(7)
                MOVE 0 TO SQL-SQINDS(7)
                MOVE 0 TO SQL-SQHARM(7)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(8)
                MOVE 11 TO SQL-SQHSTL(8)
                MOVE 11 TO SQL-SQHSTS(8)
                MOVE 0 TO SQL-SQINDV(8)
                MOVE 0 TO SQL-SQINDS(8)
                MOVE 0 TO SQL-SQHARM(8)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(9)
                MOVE 11 TO SQL-SQHSTL(9)
                MOVE 11 TO SQL-SQHSTS(9)
                MOVE 0 TO SQL-SQINDV(9)
                MOVE 0 TO SQL-SQINDS(9)
                MOVE 0 TO SQL-SQHARM(9)
                CALL "SQLADR" USING
                    WS-4TH-LOGO
                    SQL-SQHSTV(10)
                MOVE 11 TO SQL-SQHSTL(10)
                MOVE 11 TO SQL-SQHSTS(10)
                MOVE 0 TO SQL-SQINDV(10)
                MOVE 0 TO SQL-SQINDS(10)
                MOVE 0 TO SQL-SQHARM(10)
                CALL "SQLADR" USING
                    WS-REG-NO
                    SQL-SQHSTV(11)
                MOVE 7 TO SQL-SQHSTL(11)
                MOVE 7 TO SQL-SQHSTS(11)
                MOVE 0 TO SQL-SQINDV(11)
                MOVE 0 TO SQL-SQINDS(11)
                MOVE 0 TO SQL-SQHARM(11)
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
                    THEN GO TO EC-010 END-IF
*
	END-IF.
*
	GO TO EC-EXIT.
*
EC-010.
        MOVE 'EC-010 : ERROR INSERTING REPRINT LOGOS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
EC-EXIT.
	EXIT.
*
/
*
*   RSH 28/11/2007 (LQ35232): Update reprint fee status to failed for
*   all unsuccessful reprints.
*
ED-UPDATE-FEE-STATUS SECTION.
ED-START.
*   EXEC SQL
*       WHENEVER SQLERROR GO TO ED-100
*   END-EXEC.

*   EXEC SQL
*       WHENEVER NOT FOUND CONTINUE
*   END-EXEC.

*   EXEC SQL EXECUTE
*       DECLARE
*         CURSOR c_arc
*         IS
*       	SELECT
*       		arc_first_reg_rcfe_id
*       	,	arc_last_reg_rcfe_id
*       	FROM
*       		all_reprint_criteria
*       	WHERE
*       		arc_reprint_type	=	'B'
*       	AND	arc_first_reg_rcfe_id	is not	NULL
*       	AND	arc_last_reg_rcfe_id	is not	NULL;
*       BEGIN
*       	FOR	c_arc_rec
*       	IN	c_arc
*       	LOOP
*       		UPDATE	reprint_certificate_fees
*       		SET	rcfe_status		=	'F'
*       		,	rcfe_update_date	=	SYSDATE
*       		,	rcfe_update_user	=	'STP070'
*       		WHERE	rcfe_id			between
*       				c_arc_rec.arc_first_reg_rcfe_id
*       						and
*       				c_arc_rec.arc_last_reg_rcfe_id
*       		AND	rcfe_status		=	'R';
*       	END LOOP;
*       END;
*   END-EXEC.
    CALL "SQLADR" USING SQ0041 SQL-STMT
    MOVE 1 TO SQL-ITERS
    MOVE 1781 TO SQL-OFFSET
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
        THEN GO TO ED-100 END-IF.

    GO TO ED-EXIT.

ED-100.
    MOVE "ED-100: REPRINT FEE UPDATE 2 FAILED" TO WS-ERR-MESSAGE.
    PERFORM ZZ-ABORT.

ED-EXIT.
    EXIT.
/
F-TERMINATE SECTION.
********************************************************************************
*    THIS SECTION CLOSES CURSORS CURSOR_1, CURSOR_7 AND CURSOR_7A,             *
*    UPDATES THE RUN DETAILS AND WRITES CONTROL INFORMATION TO THE             *
*    REJECTS FILE.                                                             *
********************************************************************************
F-START.
*
*       EXEC SQL
*		WHENEVER SQLERROR
*       		GO TO F-030
*       END-EXEC.
*
        IF WS-IP-RUN-TYPE NOT EQUAL TO 'S'
*          EXEC SQL
*       	CLOSE CURSOR_1
*          END-EXEC
           MOVE 1 TO SQL-ITERS
           MOVE 1796 TO SQL-OFFSET
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
               THEN GO TO F-030 END-IF
	ELSE
*          EXEC SQL
*       	CLOSE CURSOR_7
*          END-EXEC
           MOVE 1 TO SQL-ITERS
           MOVE 1811 TO SQL-OFFSET
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
               THEN GO TO F-030 END-IF
*          EXEC SQL
*       	CLOSE CURSOR_7A
*          END-EXEC
           MOVE 1 TO SQL-ITERS
           MOVE 1826 TO SQL-OFFSET
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
               THEN GO TO F-030 END-IF
        END-IF.
*
	PERFORM FA-UPDATE-RUN-DETAILS.
*
F-010.
	IF WS-IP-RUN-TYPE EQUAL TO 'S'
		GO TO  F-040.
*       EXEC SQL WHENEVER SQLERROR   GO TO F-020 END-EXEC.
*       EXEC SQL FOR :WS-CNT-FETCHED
*       	UPDATE	BTEC.CONTROL_VALUES
*       	SET	CV_NEXT_VALUE = :WS-CNT-NEXT-CERT-NO
*       	WHERE	CV_TYPE = :WS-CNT-CV-TYPE
*       END-EXEC.
        CALL "SQLADR" USING SQ0042 SQL-STMT
        MOVE WS-CNT-FETCHED TO SQL-ITERS
        MOVE 1841 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-CNT-NEXT-CERT-NO IN
            WS-CERT-NO-TAB(1)
            SQL-SQHSTV(1)
        MOVE 5 TO SQL-SQHSTL(1)
        MOVE 5 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-CNT-CV-TYPE IN
            WS-CERT-NO-TAB(1)
            SQL-SQHSTV(2)
        MOVE 16 TO SQL-SQHSTL(2)
        MOVE 16 TO SQL-SQHSTS(2)
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
            THEN GO TO F-020 END-IF.
        GO TO F-040.
F-020.
	MOVE 'UPDATE FAILED ON CONTROL_VALUES TABLE' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
F-030.
	MOVE 'CLOSE CURSORS 1/7 FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
F-040.
*       EXEC SQL WHENEVER SQLERROR   GO TO F-050 END-EXEC.
*       EXEC SQL COMMIT WORK                     END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 1864 TO SQL-OFFSET
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
	GO TO F-100.
F-050.
	MOVE 'COMMIT WORK FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
F-100.
 	MOVE ALL SPACES TO REJECT-REC.

	MOVE 'STP070 HAS SUCCESSFULLY COMPLETED' TO REJECT-REC.
	WRITE REJECT-REC AFTER PAGE.

	PERFORM VARYING WS-SCT-SUB FROM 1 BY 1
		UNTIL WS-SCT-SUB > WS-SCT-INDEX

	  IF WS-SCT-PROCESSED(WS-SCT-SUB) GREATER THAN ZERO
	  THEN
	    IF WS-SCT-AWARD-CODE(WS-SCT-SUB) NOT = SPACES
	    THEN
	      MOVE SPACES TO WS-AW-RD-CERT-FILE

	      STRING	WS-AW-FILE-PREFIX	DELIMITED BY SPACE
			WS-SCT-AWARD-CODE(WS-SCT-SUB)
						DELIMITED BY SPACE
			WS-FILE-RUN-TYPE	DELIMITED BY SPACE
			'.'			DELIMITED BY SIZE
			WS-FORMAT-TYPE		DELIMITED BY SPACE
			WS-BATCH-NUMBER		DELIMITED BY SPACE
	      INTO	WS-AW-RD-CERT-FILE

	      MOVE WS-SCT-DOC-REF(WS-SCT-SUB) TO WS-AW-DOC-REF
	      MOVE WS-SCT-PASSED(WS-SCT-SUB)  TO WS-AW-CERT-ISSUED

	      PERFORM FB-INSERT-RUN-DETAILS

	      MOVE WS-SCT-DOC-REF(WS-SCT-SUB)    TO WS-CRL1-DOC-REF
	      MOVE WS-SCT-AWARD-CODE(WS-SCT-SUB) TO WS-CRL1-AWARD-CODE
	      MOVE WS-SCT-PASSED(WS-SCT-SUB)     TO WS-CRL1-STDTS-PASSED
	      MOVE WS-SCT-REJECTED(WS-SCT-SUB)   TO WS-CRL1-STDTS-REJECTED
	      MOVE WS-SCT-PROCESSED(WS-SCT-SUB)  TO WS-CRL1-STDTS-PROCESSED

	      MOVE WS-CONTROL-REJECT-LINE1 TO REJECT-REC
	    ELSE
	      MOVE WS-SCT-DOC-REF(WS-SCT-SUB)   TO WS-CRL2-DOC-REF
	      MOVE WS-SCT-PASSED(WS-SCT-SUB)    TO WS-CRL2-STDTS-PASSED
	      MOVE WS-SCT-REJECTED(WS-SCT-SUB)  TO WS-CRL2-STDTS-REJECTED
	      MOVE WS-SCT-PROCESSED(WS-SCT-SUB) TO WS-CRL2-STDTS-PROCESSED

	      MOVE WS-CONTROL-REJECT-LINE2 TO REJECT-REC
	    END-IF

	    IF WS-SCT-SUB = 1
	    THEN
	      WRITE REJECT-REC AFTER 2
	    ELSE
	      WRITE REJECT-REC
	    END-IF
	  END-IF

	END-PERFORM.

F-110.
*       EXEC SQL WHENEVER SQLERROR   GO TO F-130 END-EXEC.
*       EXEC SQL COMMIT WORK                     END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 1879 TO SQL-OFFSET
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
            THEN GO TO F-130 END-IF.
	GO TO F-150.
F-130.
	MOVE 'RUN_DETAILS COMMIT FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
F-150.
	CLOSE REJECT-FILE
	      LABELS-FILE.

        DISPLAY WS-SUCCESS-LINE.
F-EXIT.
	EXIT.
/
FA-UPDATE-RUN-DETAILS SECTION.
*****************************************************************************
*	This section update RUN_DETAILS table which stores the CPU time	    *
*									    *
*****************************************************************************
FA-START.
        CALL "LIB$GETJPI" USING BY REFERENCE  WS-ITEM-CODE,OMITTED,OMITTED, 
                                BY REFERENCE  WS-OUT-VALUE.
*
        SUBTRACT WS-START-TIME FROM WS-OUT-VALUE GIVING WS-OUT-VALUE.
        DIVIDE WS-OUT-VALUE BY 360000 GIVING WS-INTEGER REMAINDER WS-QUOTIENT.
        MOVE   WS-INTEGER            TO WS-HOURS.
        DIVIDE WS-QUOTIENT BY 6000 GIVING WS-MINS.
        MOVE   WS-TIME-FORMAT        TO WS-CPU-TIME.
*
	MOVE   WS-B-TOT-STDTS-PASSED TO WS-CERT-ISSUED.
*
FA-100.
        MOVE 'Y' TO WS-RUN-FLAG.
*
*       EXEC SQL	WHENEVER SQLERROR	GO TO FA-200	END-EXEC.
*       EXEC SQL	WHENEVER NOT FOUND 	GO TO FA-200	END-EXEC.
*
*       EXEC SQL
*       	UPDATE  BTEC.RUN_DETAILS
*        	SET 	RD_TIME     = :WS-CPU-TIME,
*                       RD_RUN_FLAG = :WS-RUN-FLAG,
*       		RD_COMMENT  = 'NUMBER OF AWARDS ISSUED = '||
*                                      TO_CHAR(:WS-CERT-ISSUED)
*               WHERE 	RD_NAME = :WS-PROGRAM
*               AND   	RD_DATE = TO_DATE(:WS-RUN-DATE,'DD-MON-YYYY')
*       END-EXEC.
        CALL "SQLADR" USING SQ0045 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1894 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-CPU-TIME
            SQL-SQHSTV(1)
        MOVE 5 TO SQL-SQHSTL(1)
        MOVE 5 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RUN-FLAG
            SQL-SQHSTV(2)
        MOVE 1 TO SQL-SQHSTL(2)
        MOVE 1 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-CERT-ISSUED
            SQL-SQHSTV(3)
        MOVE 5 TO SQL-SQHSTL(3)
        MOVE 5 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-PROGRAM
            SQL-SQHSTV(4)
        MOVE 15 TO SQL-SQHSTL(4)
        MOVE 15 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-RUN-DATE
            SQL-SQHSTV(5)
        MOVE 11 TO SQL-SQHSTL(5)
        MOVE 11 TO SQL-SQHSTS(5)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO FA-200 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO FA-200 END-IF.
*
        GO TO FA-999.
*
FA-200.
	MOVE 'RUN_DETAILS UPDATE FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
*
FA-999.
         EXIT.
*     
/
FB-INSERT-RUN-DETAILS SECTION.
*****************************************************************************
*	This section inserts a RUN_DETAILS for each award code              *
*	for which there are certificates to be printed.                     *
*****************************************************************************
FB-START.
        MOVE 'Y' TO WS-RUN-FLAG.

FB-100.

*       EXEC SQL WHENEVER SQLERROR	GO TO FB-200	END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	GO TO FB-200	END-EXEC.

*       EXEC SQL
*       	INSERT INTO BTEC.RUN_DETAILS
*       			(
*       			RD_NAME,
*       			RD_DATE,
*       			RD_RUN_FLAG,
*       			RD_COMMENT
*       			)
*       	VALUES		(
*       			:WS-AW-RD-CERT-FILE,
*       			TO_DATE(:WS-RUN-DATE,'DD-MON-YYYY'),
*       			:WS-RUN-FLAG,
*       			'NUMBER OF AWARDS ISSUED (' ||
*       				:WS-AW-DOC-REF || ') = '||
*       				TO_CHAR(:WS-AW-CERT-ISSUED)
*       			)
*       END-EXEC.
        CALL "SQLADR" USING SQ0046 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1929 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-AW-RD-CERT-FILE
            SQL-SQHSTV(1)
        MOVE 15 TO SQL-SQHSTL(1)
        MOVE 15 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-RUN-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-RUN-FLAG
            SQL-SQHSTV(3)
        MOVE 1 TO SQL-SQHSTL(3)
        MOVE 1 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-AW-DOC-REF
            SQL-SQHSTV(4)
        MOVE 5 TO SQL-SQHSTL(4)
        MOVE 5 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-AW-CERT-ISSUED
            SQL-SQHSTV(5)
        MOVE 5 TO SQL-SQHSTL(5)
        MOVE 5 TO SQL-SQHSTS(5)
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
            THEN GO TO FB-200 END-IF.

        GO TO FB-999.

FB-200.
	MOVE 'RUN_DETAILS INSERT FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.

FB-999.
	EXIT.
/
T-GET-NEXT-POSITION SECTION.
********************************************************************************
* FIND THE NEXT FREE CHARACTER POSITION IN WS-ANS-REC			       *
********************************************************************************
T-START.
*
	CALL "STR$TRIM" USING BY DESCRIPTOR WS-ANS-REC, WS-ANS-REC,
                              BY REFERENCE WS-POINTER.
	COMPUTE WS-POINTER = WS-POINTER + 1.
*
T-EXIT.
	EXIT.
/
UAA-GET-TITLE SECTION.
********************************************************************************
*                                                                              *
*	SEARCH TABLE AWARD_TITLES FOR TITLE DESCRIPTION FOR TITLE KEY
*                                                                              *
********************************************************************************
UAA-010.
*
*       EXEC SQL
*       	WHENEVER SQLERROR
*       		GO TO UAA-060
*       END-EXEC.
*
*       EXEC SQL
*       	WHENEVER SQLWARNING
*       		CONTINUE
*       END-EXEC.
*
*       EXEC SQL
*       	WHENEVER NOT FOUND
*       		GO TO UAA-030
*       END-EXEC.
*
*       EXEC SQL
*       	SELECT	LTRIM(AT_TITLE_PREFIX||' '||AT_TITLE_LINE1),
*       		AT_TITLE_LINE2
*       	INTO	:WS-TITLE-DESC1,
*       		:WS-TITLE-DESC2
*       	FROM	BTEC.AWARD_TITLES
*       	WHERE 	AT_NUMBER	= :WS-BTEC-TITLE
*       	AND NOT EXISTS (SELECT NULL
*                               FROM   AT_AWARD_CODES
*                               WHERE  AACO_AT_NUMBER = AT_NUMBER
*                               AND    AACO_AC_CODE 
*                               IN ('EA','LI','NA','NB','NC',
*                                   'ND','NE','NF','NG','TZ'))
*       	UNION		
*       	SELECT	AT_TITLE_LINE1,                                   
*       		AT_TITLE_LINE2
*       	FROM	BTEC.AWARD_TITLES
*       	WHERE 	AT_NUMBER	= :WS-BTEC-TITLE
*       	AND     EXISTS (SELECT NULL
*                               FROM   AT_AWARD_CODES
*                               WHERE  AACO_AT_NUMBER = AT_NUMBER
*                               AND    AACO_AC_CODE   IN
*                                ('EA','LI','NA','NB','NC',
*                                   'ND','NE','NF','NG','TZ'))
*       END-EXEC.
        CALL "SQLADR" USING SQ0047 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 1964 TO SQL-OFFSET
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
            WS-TITLE-DESC1
            SQL-SQHSTV(1)
        MOVE 79 TO SQL-SQHSTL(1)
        MOVE 79 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-TITLE-DESC2
            SQL-SQHSTV(2)
        MOVE 75 TO SQL-SQHSTL(2)
        MOVE 75 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-BTEC-TITLE
            SQL-SQHSTV(3)
        MOVE 5 TO SQL-SQHSTL(3)
        MOVE 5 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-BTEC-TITLE
            SQL-SQHSTV(4)
        MOVE 5 TO SQL-SQHSTL(4)
        MOVE 5 TO SQL-SQHSTS(4)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO UAA-030 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO UAA-060 END-IF.
*
	IF	WS-TITLE-COUNTER	EQUAL 652
	DISPLAY 'WARNING **** ARRAY LIMIT EXCEEDED'
	END-IF.

	IF 	WS-TITLE-COUNTER	NOT EQUAL 652

	ADD	+1 		TO WS-TITLE-COUNTER
	MOVE 	WS-TITLE-DESC1 	TO WS-TITLE1 (WS-TITLE-COUNTER)
	MOVE 	WS-TITLE-DESC2 	TO WS-TITLE2 (WS-TITLE-COUNTER)
	MOVE	WS-BTEC-TITLE 	TO WS-TITLE-CODE (WS-TITLE-COUNTER)

	END-IF.

	MOVE 	WS-TITLE-DESC1	TO	PL2-COURSE-NAME.
	MOVE 	WS-TITLE-DESC2	TO	PL3-COURSE-NAME.

	GO TO UAA-999.
*
UAA-030.
*
        MOVE 'Y' TO WS-REJECT-NO(5)

	GO TO UAA-999.
*
UAA-060.
*
	MOVE 'AWARD TITLES SELECT FAILED IN UAA' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
UAA-999.
*
	EXIT.
/
VA-GET-CENTRE-LOGO SECTION.
VA-START.
*
        INITIALIZE WS-CENTRE-LOGOS.
	IF	WS-IP-RUN-TYPE = 'S'
	THEN
		GO TO VA-999
	END-IF.
	MOVE S-CENTRE-NO TO WS-LOGO-CENTRE-ID.
*
*       EXEC SQL WHENEVER SQLERROR GO TO VA-010 END-EXEC.
*
*       EXEC SQL WHENEVER NOT FOUND GO TO VA-999 END-EXEC.
*
*       EXEC SQL
*       SELECT	CLOG_ID
*       INTO    :WS-CENTRE-LOGO
*       FROM	CUSTOMER_LOGOS
*       WHERE	CLOG_CENTRE_ID IN (:WS-LOGO-CENTRE-ID,
*       				SUBSTR(:WS-LOGO-CENTRE-ID,1,5))
*       AND     CLOG_STATUS = 'L'
*       END-EXEC.
        CALL "SQLADR" USING SQ0048 SQL-STMT
        MOVE 2 TO SQL-ITERS
        MOVE 1995 TO SQL-OFFSET
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
            WS-CENTRE-LOGO IN
            WS-CENTRE-LOGOS(1)
            SQL-SQHSTV(1)
        MOVE 8 TO SQL-SQHSTL(1)
        MOVE 8 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(2)
        MOVE 6 TO SQL-SQHSTL(2)
        MOVE 6 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VA-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VA-010 END-IF.
*
	GO TO VA-999.
*
VA-010.
*
	MOVE 'VA-010: ERROR SELECTING CENTRE LOGOS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
VA-999.
*
	EXIT.
/
VB-GET-FRAN-CENTRE-LOGO SECTION.
VB-START.
*
	IF	S-INST-LOCATION = WS-OLD-INST-LOCATION
	THEN
		GO TO VB-999
	END-IF.
*
	INITIALIZE WS-FRAN-CENTRE-LOGO,WS-OLD-INST-LOCATION.
	IF	S-INST-LOCATION = SPACES
	OR	WS-IP-RUN-TYPE = 'S'
	THEN
		GO TO VB-999
	END-IF.
	MOVE S-INST-LOCATION TO WS-LOGO-CENTRE-ID,WS-OLD-INST-LOCATION.
*
*       EXEC SQL WHENEVER SQLERROR GO TO VB-010 END-EXEC.
*
*       EXEC SQL WHENEVER NOT FOUND GO TO VB-999 END-EXEC.
*
*       EXEC SQL
*       SELECT	CLOG_ID
*       INTO    :WS-FRAN-CENTRE-LOGO
*       FROM	CUSTOMER_LOGOS
*       WHERE	CLOG_INST_ID IN (:WS-LOGO-CENTRE-ID,
*       				SUBSTR(:WS-LOGO-CENTRE-ID,1,5))
*       AND	CLOG_STATUS = 'L'
*       UNION
*       SELECT	CLOG_ID
*       FROM	CUSTOMER_LOGOS
*       WHERE	CLOG_CENTRE_ID IN (:WS-LOGO-CENTRE-ID,
*       				SUBSTR(:WS-LOGO-CENTRE-ID,1,5))
*       AND	CLOG_STATUS = 'L'
*       AND NOT EXISTS (
*       	SELECT 	NULL
*       	FROM	CUSTOMER_LOGOS
*       	WHERE	CLOG_INST_ID IN (:WS-LOGO-CENTRE-ID,
*       				SUBSTR(:WS-LOGO-CENTRE-ID,1,5))
*       	AND	CLOG_STATUS = 'L')
*       END-EXEC.
        CALL "SQLADR" USING SQ0049 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2022 TO SQL-OFFSET
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
            WS-FRAN-CENTRE-LOGO
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(2)
        MOVE 6 TO SQL-SQHSTL(2)
        MOVE 6 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(4)
        MOVE 6 TO SQL-SQHSTL(4)
        MOVE 6 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(5)
        MOVE 6 TO SQL-SQHSTL(5)
        MOVE 6 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(6)
        MOVE 6 TO SQL-SQHSTL(6)
        MOVE 6 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(7)
        MOVE 6 TO SQL-SQHSTL(7)
        MOVE 6 TO SQL-SQHSTS(7)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VB-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VB-010 END-IF.
*
	GO TO VB-999.
*
VB-010.
*
	MOVE 'VB-010: ERROR SELECTING FRANCHISE CENTRE LOGOS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
VB-999.
*
	EXIT.
/
VC-GET-TRAIN-ORG-LOGO SECTION.
VC-START.
*
        INITIALIZE WS-TORG-LOGOS.
	IF	WS-IP-RUN-TYPE = 'S'
	THEN
		GO TO VC-999
	END-IF.
	MOVE S-CENTRE-NO TO WS-LOGO-CENTRE-ID.
	MOVE S-COURSE-NO TO WS-LOGO-COURSE-ID.
*
*       EXEC SQL WHENEVER SQLERROR GO TO VC-010 END-EXEC.
*
*       EXEC SQL WHENEVER NOT FOUND GO TO VC-999 END-EXEC.
*
*       EXEC SQL
*       SELECT	DISTINCT CLOG_ID||
*       	  DECODE(TOCC_LPOS_ID,
*       		 NULL,
*       		 NULL,
*       		 '('||TO_CHAR(TOCC_LPOS_ID)||')'
*       		)
*       INTO    :WS-TORG-LOGO
*       FROM	CUSTOMER_LOGOS,
*       	TRAIN_ORG_CENTRE_COURSES
*       WHERE	TOCC_COURSE_NUMBER = :WS-LOGO-COURSE-ID
*       AND	TOCC_CENTRE_ID IN (:WS-LOGO-CENTRE-ID,
*       				SUBSTR(:WS-LOGO-CENTRE-ID,1,5))
*       AND	SYSDATE BETWEEN TOCC_START_DATE AND NVL(TOCC_END_DATE,SYSDATE)
*       AND	CLOG_TORG_ID = TOCC_TORG_CODE
*       AND	CLOG_STATUS = 'L'
*       END-EXEC.
        CALL "SQLADR" USING SQ0050 SQL-STMT
        MOVE 2 TO SQL-ITERS
        MOVE 2065 TO SQL-OFFSET
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
            WS-TORG-LOGO IN
            WS-TORG-LOGOS(1)
            SQL-SQHSTV(1)
        MOVE 8 TO SQL-SQHSTL(1)
        MOVE 8 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-LOGO-COURSE-ID
            SQL-SQHSTV(2)
        MOVE 8 TO SQL-SQHSTL(2)
        MOVE 8 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-LOGO-CENTRE-ID
            SQL-SQHSTV(4)
        MOVE 6 TO SQL-SQHSTL(4)
        MOVE 6 TO SQL-SQHSTS(4)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VC-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VC-010 END-IF.

VC-005.
*	IF WS-TRAIN-ORG-LOGO = '72'
*	THEN
*	  MOVE '72(TR)' TO WS-TRAIN-ORG-LOGO 
*	END-IF.

*	IF WS-TRAIN-ORG-LOGO = '129'
*	THEN
*	  MOVE '129(TR)' TO WS-TRAIN-ORG-LOGO 
*	END-IF.

*
	GO TO VC-999.
*
VC-010.
*
	MOVE 'VC-010: ERROR SELECTING TRAINING ORG LOGOS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
VC-999.
*
	EXIT.
/
VD-GET-REPRINT-LOGOS SECTION.
VD-START.
*
	IF	WS-IP-RUN-TYPE NOT = 'S'
	THEN
		GO TO VD-999
	END-IF.
*
	INITIALIZE SCLO-DETAILS,
		   SCLO-INDEX,
		   SCLO-FETCHED,
		   WS-AWARD-TYPE-LOGO,
		   WS-FRAN-CENTRE-LOGO.
*
	INITIALIZE WS-PBODY-LOGOS.
        INITIALIZE WS-TORG-LOGOS.
        INITIALIZE WS-CENTRE-LOGOS.

*       EXEC SQL WHENEVER SQLERROR GO TO VD-010 END-EXEC.
*
*       EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC.
*
*       EXEC SQL
*       SELECT	SCLO_ST_REG_NO,
*       	SCLO_CLOG_CODE||
*       	  DECODE(SCLO_LPOS_ID,
*       		 NULL,
*       		 NULL,
*       		 '('||TO_CHAR(SCLO_LPOS_ID)||')'
*       		),
*       	CLOG_TYPE_CODE,
*       	TO_NUMBER(SCLO_CLOG_CODE)
*       INTO	:SCLO-ST-REG-NO,
*       	:SCLO-CLOG-CODE,
*       	:SCLO-CLOG-TYPE-CODE,
*       	:SCLO-CLOG-CODE-NUMBER
*       FROM	CUSTOMER_LOGOS,
*       	STUDENT_CERTIFICATE_LOGOS
*       WHERE	SCLO_ST_REG_NO = :WS-REG-NO
*       AND   	SCLO_CLOG_CODE = CLOG_ID
*       UNION
*       SELECT	SCLO_ST_REG_NO,
*       	SCLO_CLOG_CODE||
*       	  DECODE(SCLO_LPOS_ID,
*       		 NULL,
*       		 NULL,
*       		 '('||TO_CHAR(SCLO_LPOS_ID)||')'
*       		),
*       	CLOG_TYPE_CODE,
*       	TO_NUMBER(SCLO_CLOG_CODE)
*       FROM	CUSTOMER_LOGOS,
*       	STUDENT_CERTIFICATE_LOGOS@ARCA
*       WHERE	SCLO_ST_REG_NO = :WS-REG-NO
*       AND   	SCLO_CLOG_CODE = CLOG_ID
*       ORDER BY 4		
*       END-EXEC.
        CALL "SQLADR" USING SQ0051 SQL-STMT
        MOVE 10 TO SQL-ITERS
        MOVE 2096 TO SQL-OFFSET
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
            SCLO-ST-REG-NO IN
            SCLO-DETAILS(1)
            SQL-SQHSTV(1)
        MOVE 7 TO SQL-SQHSTL(1)
        MOVE 7 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            SCLO-CLOG-CODE IN
            SCLO-DETAILS(1)
            SQL-SQHSTV(2)
        MOVE 8 TO SQL-SQHSTL(2)
        MOVE 8 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            SCLO-CLOG-TYPE-CODE IN
            SCLO-DETAILS(1)
            SQL-SQHSTV(3)
        MOVE 1 TO SQL-SQHSTL(3)
        MOVE 1 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            SCLO-CLOG-CODE-NUMBER IN
            SCLO-DETAILS(1)
            SQL-SQHSTV(4)
        MOVE 4 TO SQL-SQHSTL(4)
        MOVE 4 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(5)
        MOVE 7 TO SQL-SQHSTL(5)
        MOVE 7 TO SQL-SQHSTS(5)
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
            THEN GO TO VD-010 END-IF.
*
	MOVE SQLERRD(3) TO SCLO-FETCHED.
	MOVE 0	        TO WS-PBODY-SUB.
        MOVE 0          TO WS-TORG-SUB.
        MOVE 0          TO WS-CENTRE-SUB.
*
	PERFORM VARYING SCLO-INDEX
		FROM 1 BY 1
		UNTIL SCLO-INDEX > SCLO-FETCHED
*
		EVALUATE SCLO-CLOG-TYPE-CODE(SCLO-INDEX)
*
                WHEN "C"
                  ADD 1 TO WS-CENTRE-SUB
                  MOVE SCLO-CLOG-CODE(SCLO-INDEX) 
		  TO   WS-CENTRE-LOGO(WS-CENTRE-SUB)
		WHEN "F" MOVE SCLO-CLOG-CODE(SCLO-INDEX) TO WS-FRAN-CENTRE-LOGO
                WHEN "T"
                  ADD 1 TO WS-TORG-SUB
                  MOVE SCLO-CLOG-CODE(SCLO-INDEX) TO WS-TORG-LOGO(WS-TORG-SUB)
		WHEN "A" MOVE SCLO-CLOG-CODE(SCLO-INDEX) TO WS-AWARD-TYPE-LOGO
		WHEN "P" 
		  ADD 1 TO WS-PBODY-SUB
		  MOVE SCLO-CLOG-CODE(SCLO-INDEX) TO WS-PBODY-LOGO(WS-PBODY-SUB)
*
		END-EVALUATE
*
	END-PERFORM.
*
	GO TO VD-999.
*
VD-010.
*
	MOVE 'VD-010: ERROR SELECTING REPRINT LOGOS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
VD-999.
*
	EXIT.
/
VE-GET-AWARD-TYPE-LOGO SECTION.
VE-START.

	INITIALIZE WS-AWARD-TYPE-LOGO.

	IF	WS-IP-RUN-TYPE = 'S'
	THEN
		GO TO VE-999
	END-IF.

	MOVE S-COURSE-NO TO WS-LOGO-COURSE-ID.

*       EXEC SQL WHENEVER SQLERROR 	GO TO VE-010 END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND 	GO TO VE-999 END-EXEC.

*       EXEC SQL
*         SELECT DISTINCT CLOG_ID
*         INTO	 :WS-AWARD-TYPE-LOGO
*         FROM	 CUSTOMER_LOGOS,
*       	 LOGO_AWARDS
*         WHERE	 LAWA_AC_CODE = :WS-AWARD-CODE
*         AND	 SYSDATE BETWEEN LAWA_START_DATE AND NVL(LAWA_END_DATE,SYSDATE)
*         AND	 CLOG_LATY_CODE = LAWA_LATY_CODE
*         AND	 CLOG_STATUS = 'L'
*       END-EXEC.
        CALL "SQLADR" USING SQ0052 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2135 TO SQL-OFFSET
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
            WS-AWARD-TYPE-LOGO
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-AWARD-CODE
            SQL-SQHSTV(2)
        MOVE 2 TO SQL-SQHSTL(2)
        MOVE 2 TO SQL-SQHSTS(2)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VE-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VE-010 END-IF.
*
	GO TO VE-999.
*
VE-010.
*
	MOVE 'VE-010: ERROR SELECTING AWARD TYPE LOGOS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
VE-999.
*
	EXIT.
*
VF-GET-PBODY-LOGOS SECTION.
VF-START.
*
	INITIALIZE WS-PBODY-LOGOS.

	IF	WS-IP-RUN-TYPE = 'S'
	THEN
		GO TO VF-999
	END-IF.

*       EXEC SQL WHENEVER SQLERROR  GO TO VF-010 END-EXEC.
*
*       EXEC SQL WHENEVER NOT FOUND GO TO VF-999 END-EXEC.
*
*       EXEC SQL
*        SELECT	DISTINCT CLOG_ID,
*       	  CLOG_ID||
*       	  DECODE(PBBT_PBOD_ID,
*       		 39,	
*       	  	 DECODE(:WS-COURSE-NO,
*       			'CK597',
*       			'(1)',
*       			'CK599',
*       			'(1)',
*       	    		DECODE(PBBT_LPOS_ID,
*       		   	       NULL,
*       		   	       NULL,
*       		   	       '('||TO_CHAR(PBBT_LPOS_ID)||')'
*       			      )
*       		       ),
*       	    	 DECODE(PBBT_LPOS_ID,
*       		   	NULL,
*       		   	NULL,
*       		   	'('||TO_CHAR(PBBT_LPOS_ID)||')'
*       		       )
*       		)
*        INTO   :WS-PBODY-CLOG-ID,
*       	:WS-PBODY-LOGO
*        FROM	CUSTOMER_LOGOS,
*       	PROFESSIONAL_BODY_BTECS
*        WHERE	CLOG_PBOD_ID     = PBBT_PBOD_ID+0
*        AND    CLOG_STATUS 	 = 'L'
*        AND	PBBT_AWARD_CODE	 = :WS-AWARD-CODE
*        AND	PBBT_AWARD_TITLE = TO_NUMBER(:WS-BTEC-TITLE)
*        AND    SYSDATE   BETWEEN PBBT_START_DATE
*                             AND NVL(PBBT_END_DATE,SYSDATE)
*        ORDER BY TO_NUMBER(CLOG_ID) 
*       END-EXEC.
        CALL "SQLADR" USING SQ0053 SQL-STMT
        MOVE 5 TO SQL-ITERS
        MOVE 2158 TO SQL-OFFSET
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
            WS-COURSE-NO
            SQL-SQHSTV(1)
        MOVE 8 TO SQL-SQHSTL(1)
        MOVE 8 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-PBODY-CLOG-ID IN
            WS-PBODY-LOGOS(1)
            SQL-SQHSTV(2)
        MOVE 4 TO SQL-SQHSTL(2)
        MOVE 4 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-PBODY-LOGO IN
            WS-PBODY-LOGOS(1)
            SQL-SQHSTV(3)
        MOVE 8 TO SQL-SQHSTL(3)
        MOVE 8 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-AWARD-CODE
            SQL-SQHSTV(4)
        MOVE 2 TO SQL-SQHSTL(4)
        MOVE 2 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-BTEC-TITLE
            SQL-SQHSTV(5)
        MOVE 5 TO SQL-SQHSTL(5)
        MOVE 5 TO SQL-SQHSTS(5)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VF-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VF-010 END-IF.
*
	GO TO VF-999.
*
VF-010.
*
	MOVE 'VF-010: ERROR SELECTING P.BODY LOGOS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
*
VF-999.
*
	EXIT.
/
*
VG-GET-PBODIES SECTION.
VG-START.
*
        INITIALIZE WS-MEMBERSHIP-FLAG.
        INITIALIZE WS-CMP-DATES.
        MOVE S-ISSUE-DATE TO  WS-DUMMY-CERT-ISSUE-DATE.

*       EXEC SQL WHENEVER SQLERROR  GO TO VG-010 END-EXEC.
*
*       EXEC SQL WHENEVER NOT FOUND GO TO VG-999 END-EXEC.
        
*       EXEC SQL
*        SELECT 'Y' 
*        INTO   :WS-CMP-DATES
*        FROM   DUAL
*        WHERE  TO_DATE(:WS-DUMMY-CERT-ISSUE-DATE,'DD-MON-YYYY') 
*             > TO_DATE('26-OCT-2000','DD-MON-YYYY')
*       END-EXEC.
        CALL "SQLADR" USING SQ0054 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2193 TO SQL-OFFSET
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
            WS-CMP-DATES
            SQL-SQHSTV(1)
        MOVE 1 TO SQL-SQHSTL(1)
        MOVE 1 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-DUMMY-CERT-ISSUE-DATE
            SQL-SQHSTV(2)
        MOVE 11 TO SQL-SQHSTL(2)
        MOVE 11 TO SQL-SQHSTS(2)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VG-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VG-010 END-IF.
    
        IF WS-IP-RUN-TYPE = 'S'
          IF WS-CMP-DATES = 'Y' 
           
*          EXEC SQL WHENEVER SQLERROR  GO TO VG-010 END-EXEC.
*
*          EXEC SQL WHENEVER NOT FOUND GO TO VG-999 END-EXEC.
*
*          EXEC SQL
*            SELECT DISTINCT 'Y'
*            INTO   :WS-MEMBERSHIP-FLAG
*            FROM   PROFESSIONAL_BODY_BTECS,
*                    PROFESSIONAL_BODIES_NOLOGOS
*            WHERE  PBBT_AWARD_TITLE =  TO_NUMBER(:WS-BTEC-TITLE)
*            AND    PBBT_PBOD_ID     =  PBNO_ID
*            AND    PBBT_AWARD_CODE  =  :WS-AWARD-CODE
*            AND    PBNO_ID = 10
*          END-EXEC
           CALL "SQLADR" USING SQ0055 SQL-STMT
           MOVE 1 TO SQL-ITERS
           MOVE 2216 TO SQL-OFFSET
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
               WS-MEMBERSHIP-FLAG
               SQL-SQHSTV(1)
           MOVE 1 TO SQL-SQHSTL(1)
           MOVE 1 TO SQL-SQHSTS(1)
           MOVE 0 TO SQL-SQINDV(1)
           MOVE 0 TO SQL-SQINDS(1)
           MOVE 0 TO SQL-SQHARM(1)
           CALL "SQLADR" USING
               WS-BTEC-TITLE
               SQL-SQHSTV(2)
           MOVE 5 TO SQL-SQHSTL(2)
           MOVE 5 TO SQL-SQHSTS(2)
           MOVE 0 TO SQL-SQINDV(2)
           MOVE 0 TO SQL-SQINDS(2)
           MOVE 0 TO SQL-SQHARM(2)
           CALL "SQLADR" USING
               WS-AWARD-CODE
               SQL-SQHSTV(3)
           MOVE 2 TO SQL-SQHSTL(3)
           MOVE 2 TO SQL-SQHSTS(3)
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
           IF SQLCODE IN SQLCA IS EQUAL TO 1403
               THEN GO TO VG-999 END-IF
           IF SQLCODE IN SQLCA IS LESS THAN 0
               THEN GO TO VG-010 END-IF

          END-IF

          GO TO VG-999
        END-IF.

*       EXEC SQL WHENEVER SQLERROR  GO TO VG-010 END-EXEC.
*
*       EXEC SQL WHENEVER NOT FOUND GO TO VG-999 END-EXEC.
*
*       EXEC SQL
*        SELECT DISTINCT 'Y'
*        INTO   :WS-MEMBERSHIP-FLAG
*        FROM   PROFESSIONAL_BODY_BTECS,
*       	PROFESSIONAL_BODIES_NOLOGOS
*        WHERE  PBBT_AWARD_TITLE =  TO_NUMBER(:WS-BTEC-TITLE)
*        AND    PBBT_PBOD_ID     =  PBNO_ID
*        AND    PBBT_AWARD_CODE  =  :WS-AWARD-CODE
*        AND    PBNO_ID = 10
*       END-EXEC.
        CALL "SQLADR" USING SQ0056 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2243 TO SQL-OFFSET
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
            WS-MEMBERSHIP-FLAG
            SQL-SQHSTV(1)
        MOVE 1 TO SQL-SQHSTL(1)
        MOVE 1 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-BTEC-TITLE
            SQL-SQHSTV(2)
        MOVE 5 TO SQL-SQHSTL(2)
        MOVE 5 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-AWARD-CODE
            SQL-SQHSTV(3)
        MOVE 2 TO SQL-SQHSTL(3)
        MOVE 2 TO SQL-SQHSTS(3)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VG-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VG-010 END-IF.
*
        GO TO VG-999.
*
VG-010.
*
        MOVE 'VG-010: ERROR SELECTING P.BODY ' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
*
VG-999.
*
        EXIT.
/
VH-GET-CERT-VALIDITY SECTION.
VH-START.
*
        MOVE SPACES TO WS-YEARS-CERT-VALID.

*       EXEC SQL WHENEVER SQLERROR  GO TO VH-010 END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND GO TO VH-999 END-EXEC.
        
*       EXEC SQL
*            SELECT TO_CHAR(AACO_YEARS_CERT_VALID)
*            INTO   WS-YEARS-CERT-VALID
*            FROM   AT_AWARD_CODES
*            WHERE  AACO_AT_NUMBER = TO_NUMBER(:WS-BTEC-TITLE)
*            AND    AACO_AC_CODE   = :WS-AWARD-CODE
*       END-EXEC.
        CALL "SQLADR" USING SQ0057 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2270 TO SQL-OFFSET
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
            WS-YEARS-CERT-VALID
            SQL-SQHSTV(1)
        MOVE 1 TO SQL-SQHSTL(1)
        MOVE 1 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-BTEC-TITLE
            SQL-SQHSTV(2)
        MOVE 5 TO SQL-SQHSTL(2)
        MOVE 5 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-AWARD-CODE
            SQL-SQHSTV(3)
        MOVE 2 TO SQL-SQHSTL(3)
        MOVE 2 TO SQL-SQHSTS(3)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO VH-999 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO VH-010 END-IF.

        GO TO VH-999.
VH-010.
        MOVE 'VH-010: ERROR SELECTING CERTIFICATE VALIDITY' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
VH-999.
        EXIT.
/
*
ZC-INSERT-IOM-CERTS SECTION.
*******************************************************************************
ZC-000.
*
	MOVE S-SCHEME-REG-NO TO WS-COE-SCHEME-REG-NO.
	MOVE S-SCHEME-REG-NO-I TO WS-COE-SCHEME-REG-NO-I.
        IF WS-COE-SCHEME-REG-NO-I NOT = -1 THEN GO TO ZC-999.
	MOVE S-REG-NO	    TO WS-COE-REG-NO.
	MOVE S-STUDENT-NAME TO WS-COE-NAME.
	MOVE S-CENTRE-NO    TO WS-COE-CENTRE.
	MOVE S-ISSUE-DATE   TO WS-COE-DATE.
	MOVE S-BTEC-CODE    TO WS-COE-BTEC-CODE.
	MOVE S-BTEC-LEVEL   TO WS-COE-BTEC-LEVEL.
*
*       EXEC SQL WHENEVER SQLERROR GO TO ZC-090 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING GO TO ZC-090 END-EXEC.
*       EXEC SQL
*           INSERT INTO IOM_CERTIFICATES (
*             IOMC_TYPE,
*             IOMC_TITLE,
*             IOMC_LEVEL,
*             IOMC_CENTRE,
*             IOMC_NAME,
*             IOMC_REG_NO,
*             IOMC_SCHEME_REG_NO,
*             IOMC_AWARD_DATE)
*           VALUES (
*             'B',
*             :WS-COE-BTEC-CODE,
*             :WS-COE-BTEC-LEVEL,
*             :WS-COE-CENTRE,
*             :WS-COE-NAME,
*             :WS-COE-REG-NO,
*             NULL,
*             TO_DATE(:WS-COE-DATE,'DD-MON-YYYY'))
*       END-EXEC.
        CALL "SQLADR" USING SQ0058 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2297 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-COE-BTEC-CODE
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-COE-BTEC-LEVEL
            SQL-SQHSTV(2)
        MOVE 2 TO SQL-SQHSTL(2)
        MOVE 2 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-COE-CENTRE
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-COE-NAME
            SQL-SQHSTV(4)
        MOVE 70 TO SQL-SQHSTL(4)
        MOVE 70 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-COE-REG-NO
            SQL-SQHSTV(5)
        MOVE 7 TO SQL-SQHSTL(5)
        MOVE 7 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-COE-DATE
            SQL-SQHSTV(6)
        MOVE 11 TO SQL-SQHSTL(6)
        MOVE 11 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
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
            THEN GO TO ZC-090 END-IF
        IF SQLWARN0 IS EQUAL TO "W"
            THEN GO TO ZC-090 END-IF.
        GO TO ZC-999.
ZC-090.
	MOVE 'INSERT INTO IOM_CERTS FAILED' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
ZC-999.
        EXIT.

*
ZD-INSERT-DSA-CERTS SECTION.
*******************************************************************************
ZD-000.
*
        MOVE S-SCHEME-REG-NO TO WS-COE-SCHEME-REG-NO.
        MOVE S-SCHEME-REG-NO-I TO WS-COE-SCHEME-REG-NO-I.
        IF WS-COE-SCHEME-REG-NO-I NOT = -1 THEN GO TO ZD-999.
        MOVE S-REG-NO       TO WS-COE-REG-NO.
        MOVE S-STUDENT-NAME TO WS-COE-NAME.
        MOVE S-CENTRE-NO    TO WS-COE-CENTRE.
        MOVE S-ISSUE-DATE   TO WS-COE-DATE.
        MOVE S-BTEC-CODE    TO WS-COE-BTEC-CODE.
        MOVE S-BTEC-LEVEL   TO WS-COE-BTEC-LEVEL.
        MOVE S-DSA-SRU-ID   TO WS-COE-SRU-NUMBER.
*
*       EXEC SQL WHENEVER SQLERROR GO TO ZD-090 END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING GO TO ZD-090 END-EXEC.
*       EXEC SQL
*           INSERT INTO DSA_CERTIFICATES (
*             DSAC_TYPE,
*             DSAC_TITLE,
*             DSAC_LEVEL,
*             DSAC_CENTRE,
*             DSAC_NAME,
*             DSAC_REG_NO,
*             DSAC_SCHEME_REG_NO,
*             DSAC_AWARD_DATE,
*             DSAC_SRU_ID
*             )
*           VALUES (
*             'B',
*             :WS-COE-BTEC-CODE,
*             :WS-COE-BTEC-LEVEL,
*             :WS-COE-CENTRE,
*             :WS-COE-NAME,
*             :WS-COE-REG-NO,
*             NULL,
*             TO_DATE(:WS-COE-DATE,'DD-MON-YYYY'),
*             :WS-COE-SRU-NUMBER)
*       END-EXEC.
        CALL "SQLADR" USING SQ0059 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2336 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-COE-BTEC-CODE
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-COE-BTEC-LEVEL
            SQL-SQHSTV(2)
        MOVE 2 TO SQL-SQHSTL(2)
        MOVE 2 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-COE-CENTRE
            SQL-SQHSTV(3)
        MOVE 6 TO SQL-SQHSTL(3)
        MOVE 6 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-COE-NAME
            SQL-SQHSTV(4)
        MOVE 70 TO SQL-SQHSTL(4)
        MOVE 70 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-COE-REG-NO
            SQL-SQHSTV(5)
        MOVE 7 TO SQL-SQHSTL(5)
        MOVE 7 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-COE-DATE
            SQL-SQHSTV(6)
        MOVE 11 TO SQL-SQHSTL(6)
        MOVE 11 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-COE-SRU-NUMBER
            SQL-SQHSTV(7)
        MOVE 12 TO SQL-SQHSTL(7)
        MOVE 12 TO SQL-SQHSTS(7)
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
            THEN GO TO ZD-090 END-IF
        IF SQLWARN0 IS EQUAL TO "W"
            THEN GO TO ZD-090 END-IF.
        GO TO ZD-999.
ZD-090.
        MOVE 'INSERT INTO DSA_CERTS FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
ZD-999.
        EXIT.
*
ZP-GET-COURSE-LEVEL SECTION.
********************************************************************************
*    THIS SECTION OBTAINS AWARD AND APPLICATION DETAILS OF THE COURSE          *
*	  CURRENTLY BEING PROCESSED.					       *
********************************************************************************
ZP-START.
        INITIALIZE WS-AWARD-CODE,
                   WS-APPLICAT-NO,
                   WS-MAIN-BOARD,
                   WS-BTEC-TITLE,
		   WS-AC-DESCR,
		   WS-ACTY-CODE,
		   WS-ACCL-CODE,
		   WS-BALE-DESC,
		   WS-ACCRED-LOGO,
		   WS-AC-ACCRED-BODY.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO ZP-030  END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE        END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO ZP-EXIT END-EXEC.
*
*       EXEC SQL SELECT AW_AWARD_CODE,  
*                       AW_APPLICAT_NO,
*       		DECODE(AC_ACTY_CODE,
*       		       'KS',
*       		    	SUBSTR(AC_DESCRIPTION,12,60)||' LEVEL',
*       			AC_DESCRIPTION),
*       		AC_ACTY_CODE,
*       		AC_ACCL_CODE,
*       		BALE_DESC,
*       		NVL(AC_BNM_TYPE,'*'),
*       		NVL(AC_ACCRED_BODY,'Q'),
*       		NVL(AT_ACCRED_LOGO,'*')
*       	 INTO 	:WS-AWARD-CODE, 
*                       :WS-APPLICAT-NO,
*       		:WS-AC-DESCR,
*       		:WS-ACTY-CODE,
*       		:WS-ACCL-CODE,
*       		:WS-BALE-DESC,
*       		:WS-AC-BNM-TYPE,
*       		:WS-AC-ACCRED-BODY,
*       		:WS-ACCRED-LOGO
*       	 FROM   BTEC.BTEC_AWARD_LEVELS,
*       	        BTEC.AWARD_CODES,
*       		AWARD_TITLES,
*       		APPROVAL_APPLICATION,
*       	        BTEC.APPROVAL_AWARDS
*                WHERE  AW_AWARD_CODE NOT BETWEEN '23' AND '26'
*       	 AND    AW_AWARD_CODE NOT BETWEEN '33' AND '37'
*                AND    AW_COURSE_NUMBER = :WS-COURSE-NO
*       	 AND    AA_APPLICAT_NO   = AW_APPLICAT_NO+0
*       	 AND    AT_NUMBER        = TO_NUMBER(AA_BTEC_TITLE)
*       	 AND	AC_CODE		 = AW_AWARD_CODE
*       	 AND	BALE_ID(+)	 = AW_BALE_CODE
*                UNION
*                SELECT AW_AWARD_CODE,  
*                       AW_APPLICAT_NO,
*       	  DECODE(AC_ACTY_CODE,
*                               'CA',
*       			AC_DESCRIPTION,
*                               NULL),                       
*                       AC_ACTY_CODE,
*                       AC_ACCL_CODE,
*                       NULL,
*       		'*',
*       		NVL(AC_ACCRED_BODY,'Q'),
*       		NVL(AT_ACCRED_LOGO,'*')
*                FROM   BTEC.AWARD_CODES,
*       		AWARD_TITLES,
*       		APPROVAL_APPLICATION,
*                       BTEC.APPROVAL_AWARDS
*                WHERE  (AW_AWARD_CODE BETWEEN '23' AND '26'
*       	      OR AW_AWARD_CODE  BETWEEN '33' AND '37')
*                AND    AW_COURSE_NUMBER = :WS-COURSE-NO
*       	 AND    AA_APPLICAT_NO   = AW_APPLICAT_NO+0
*       	 AND    AT_NUMBER        = TO_NUMBER(AA_BTEC_TITLE)
*                AND    AC_CODE          = AW_AWARD_CODE
*       END-EXEC.
        CALL "SQLADR" USING SQ0060 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2379 TO SQL-OFFSET
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
            WS-AWARD-CODE
            SQL-SQHSTV(1)
        MOVE 2 TO SQL-SQHSTL(1)
        MOVE 2 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-APPLICAT-NO
            SQL-SQHSTV(2)
        MOVE 4 TO SQL-SQHSTL(2)
        MOVE 4 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-AC-DESCR
            SQL-SQHSTV(3)
        MOVE 60 TO SQL-SQHSTL(3)
        MOVE 60 TO SQL-SQHSTS(3)
        MOVE 0 TO SQL-SQINDV(3)
        MOVE 0 TO SQL-SQINDS(3)
        MOVE 0 TO SQL-SQHARM(3)
        CALL "SQLADR" USING
            WS-ACTY-CODE
            SQL-SQHSTV(4)
        MOVE 2 TO SQL-SQHSTL(4)
        MOVE 2 TO SQL-SQHSTS(4)
        MOVE 0 TO SQL-SQINDV(4)
        MOVE 0 TO SQL-SQINDS(4)
        MOVE 0 TO SQL-SQHARM(4)
        CALL "SQLADR" USING
            WS-ACCL-CODE
            SQL-SQHSTV(5)
        MOVE 3 TO SQL-SQHSTL(5)
        MOVE 3 TO SQL-SQHSTS(5)
        MOVE 0 TO SQL-SQINDV(5)
        MOVE 0 TO SQL-SQINDS(5)
        MOVE 0 TO SQL-SQHARM(5)
        CALL "SQLADR" USING
            WS-BALE-DESC
            SQL-SQHSTV(6)
        MOVE 30 TO SQL-SQHSTL(6)
        MOVE 30 TO SQL-SQHSTS(6)
        MOVE 0 TO SQL-SQINDV(6)
        MOVE 0 TO SQL-SQINDS(6)
        MOVE 0 TO SQL-SQHARM(6)
        CALL "SQLADR" USING
            WS-AC-BNM-TYPE
            SQL-SQHSTV(7)
        MOVE 1 TO SQL-SQHSTL(7)
        MOVE 1 TO SQL-SQHSTS(7)
        MOVE 0 TO SQL-SQINDV(7)
        MOVE 0 TO SQL-SQINDS(7)
        MOVE 0 TO SQL-SQHARM(7)
        CALL "SQLADR" USING
            WS-AC-ACCRED-BODY
            SQL-SQHSTV(8)
        MOVE 1 TO SQL-SQHSTL(8)
        MOVE 1 TO SQL-SQHSTS(8)
        MOVE 0 TO SQL-SQINDV(8)
        MOVE 0 TO SQL-SQINDS(8)
        MOVE 0 TO SQL-SQHARM(8)
        CALL "SQLADR" USING
            WS-ACCRED-LOGO
            SQL-SQHSTV(9)
        MOVE 1 TO SQL-SQHSTL(9)
        MOVE 1 TO SQL-SQHSTS(9)
        MOVE 0 TO SQL-SQINDV(9)
        MOVE 0 TO SQL-SQINDS(9)
        MOVE 0 TO SQL-SQHARM(9)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(10)
        MOVE 8 TO SQL-SQHSTL(10)
        MOVE 8 TO SQL-SQHSTS(10)
        MOVE 0 TO SQL-SQINDV(10)
        MOVE 0 TO SQL-SQINDS(10)
        MOVE 0 TO SQL-SQHARM(10)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(11)
        MOVE 8 TO SQL-SQHSTL(11)
        MOVE 8 TO SQL-SQHSTS(11)
        MOVE 0 TO SQL-SQINDV(11)
        MOVE 0 TO SQL-SQINDS(11)
        MOVE 0 TO SQL-SQHARM(11)
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
            THEN GO TO ZP-EXIT END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO ZP-030 END-IF.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO ZP-045  END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE        END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  GO TO ZP-EXIT END-EXEC.
*
*       EXEC SQL SELECT AA_MAIN_BOARD,
*                       AA_BTEC_TITLE
*       	 INTO   :WS-MAIN-BOARD,
*                       :WS-BTEC-TITLE
*       	 FROM   BTEC.APPROVAL_APPLICATION
*       	 WHERE  AA_APPLICAT_NO = :WS-APPLICAT-NO 
*       END-EXEC.
        CALL "SQLADR" USING SQ0061 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2438 TO SQL-OFFSET
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
            WS-MAIN-BOARD
            SQL-SQHSTV(1)
        MOVE 2 TO SQL-SQHSTL(1)
        MOVE 2 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-BTEC-TITLE
            SQL-SQHSTV(2)
        MOVE 5 TO SQL-SQHSTL(2)
        MOVE 5 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-APPLICAT-NO
            SQL-SQHSTV(3)
        MOVE 4 TO SQL-SQHSTL(3)
        MOVE 4 TO SQL-SQHSTS(3)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO ZP-EXIT END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO ZP-045 END-IF.
        GO TO ZP-EXIT.   
ZP-030.
	MOVE 'COURSE SELECT FAILED ON APPROVAL_AWARDS' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
ZP-045.
	MOVE 'BOARD SELECT FAILED ON APPROVAL_APPLICATION' TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.
ZP-EXIT.
	EXIT.
*
ZQ-COURSE-SQA-CODE SECTION.
********************************************************************************
*    THIS SECTION OBTAINS THE SQA CODE IF THERE IS ONE.
********************************************************************************
ZQ-START.

        MOVE SPACES TO WS-COURSE-QCA-CODE.

*       EXEC SQL WHENEVER SQLERROR   GO TO ZQ-050  END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE      END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
*
*       EXEC SQL 
*       	SELECT 	FN_GET_SQA_CODE(:WS-COURSE-NO,3)
*       	INTO 	:WS-COURSE-QCA-CODE
*       	FROM	DUAL
*       END-EXEC.
        CALL "SQLADR" USING SQ0062 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2465 TO SQL-OFFSET
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
            WS-COURSE-NO
            SQL-SQHSTV(1)
        MOVE 8 TO SQL-SQHSTL(1)
        MOVE 8 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-COURSE-QCA-CODE
            SQL-SQHSTV(2)
        MOVE 20 TO SQL-SQHSTL(2)
        MOVE 20 TO SQL-SQHSTS(2)
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
            THEN GO TO ZQ-050 END-IF.
*
	GO TO ZQ-EXIT.

ZQ-050.

        MOVE    'FUNCTION FN_GET_SQA_CODE FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.

ZQ-EXIT.
	EXIT.
*
ZR-COURSE-QCA-CODE SECTION.
********************************************************************************
*    THIS SECTION OBTAINS AN NQF COURSE QCA CODE IF THERE IS ONE.              *
********************************************************************************
ZR-START.

        MOVE SPACES TO WS-COURSE-QCA-CODE.

*       EXEC SQL WHENEVER SQLERROR   GO TO ZR-050  END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING CONTINUE      END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND  CONTINUE      END-EXEC.
*
*       EXEC SQL 
*       	SELECT 	DECODE(AACO_QCA_CODE,NULL,NULL,
*       			' : QN '||
*       			SUBSTR(AACO_QCA_CODE,1,3)||
*       			'/'||
*       			SUBSTR(AACO_QCA_CODE,4,4)||
*       			'/'||
*       			SUBSTR(AACO_QCA_CODE,8))
*       	INTO 	:WS-COURSE-QCA-CODE
*       	FROM    AT_AWARD_CODES,
*       		APPROVAL_APPLICATION,
*       		APPROVAL_AWARDS
*       	WHERE	AACO_AT_NUMBER		= TO_NUMBER(AA_BTEC_TITLE)
*       	AND	AACO_AC_CODE		= AW_AWARD_CODE||''
*       	AND	AA_APPLICAT_NO		= AW_APPLICAT_NO+0
*       	AND	AW_COURSE_NUMBER	= :WS-COURSE-NO
*       END-EXEC.
        CALL "SQLADR" USING SQ0063 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2488 TO SQL-OFFSET
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
            WS-COURSE-QCA-CODE
            SQL-SQHSTV(1)
        MOVE 20 TO SQL-SQHSTL(1)
        MOVE 20 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(2)
        MOVE 8 TO SQL-SQHSTL(2)
        MOVE 8 TO SQL-SQHSTS(2)
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
            THEN GO TO ZR-050 END-IF.
*
	GO TO ZR-EXIT.

ZR-050.

        MOVE    'SELECT FROM AT_AWARD_CODES FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.

ZR-EXIT.
	EXIT.
*
ZS-ESOL-QCA-CODE SECTION.
********************************************************************************
*    RSH 08/08/2005 (RfW 04/0109):                                             *
*    THIS SECTION OBTAINS AN ESOL COURSE QCA CODE IF THERE IS ONE.             *
********************************************************************************
ZS-START.
*       EXEC SQL WHENEVER SQLERROR	GO TO ZS-050	END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE	END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	CONTINUE	END-EXEC.

	MOVE SPACES TO WS-COURSE-QCA-CODE.

*       EXEC SQL 
*       	SELECT
*       		DECODE(AACL_QCA_CODE,
*       			NULL,NULL,
*       			' : QN '||
*       			SUBSTR(AACL_QCA_CODE,1,3)||
*       			'/'||
*       			SUBSTR(AACL_QCA_CODE,4,4)||
*       			'/'||
*       			SUBSTR(AACL_QCA_CODE,8))
*       	INTO
*       		:WS-COURSE-QCA-CODE
*       	FROM
*       		AT_AWARD_CODE_LEVELS,
*       		APPROVAL_APPLICATION,
*       		APPROVAL_AWARDS
*       	WHERE
*       		AW_COURSE_NUMBER	= :WS-COURSE-NO
*       	AND	AA_APPLICAT_NO		= AW_APPLICAT_NO+0
*       	AND	AACL_AT_NUMBER		= TO_NUMBER(AA_BTEC_TITLE)
*       	AND	AACL_AC_CODE		= AW_AWARD_CODE||''
*       	AND	AACL_LEVEL		= :WS-QUAL-LEVEL
*       END-EXEC.
        CALL "SQLADR" USING SQ0064 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2511 TO SQL-OFFSET
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
            WS-COURSE-QCA-CODE
            SQL-SQHSTV(1)
        MOVE 20 TO SQL-SQHSTL(1)
        MOVE 20 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(2)
        MOVE 8 TO SQL-SQHSTL(2)
        MOVE 8 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-QUAL-LEVEL
            SQL-SQHSTV(3)
        MOVE 2 TO SQL-SQHSTL(3)
        MOVE 2 TO SQL-SQHSTS(3)
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
            THEN GO TO ZS-050 END-IF.

	GO TO ZS-EXIT.

ZS-050.
	MOVE 'SELECT FROM AT_AWARD_CODE_LEVELS FAILED' TO WS-ERR-MESSAGE.

	PERFORM ZZ-ABORT.

ZS-EXIT.
	EXIT.
*
ZT-EXPIRED-NVQ-UNITS SECTION.
********************************************************************************
* RSH 17/11/2005 (RfW05/0186): 						       *
* CHECK FOR ACHIEVED NVQ UNITS WHERE CERTIFICATION HAS EXPIRED.	    	       *
********************************************************************************
ZT-010.
	MOVE 'N' TO WS-NVQ-UNIT-CERT-EXPIRED.

ZT-020.
*       EXEC SQL WHENEVER SQLERROR	GO TO ZT-900	END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE	END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	GO TO ZT-900	END-EXEC.

*       EXEC SQL
*       	SELECT
*       		COUNT(1)
*       	INTO
*       		:WS-NO-OF-EXPIRED-UNITS-1
*       	FROM
*       		NVQ_STUDENT_COMPETENCE_UNITS,
*       		NVQ_COMPETENCE_UNITS
*       	WHERE
*       		NSCU_ST_REG_NO	= :WS-REG-NO
*       	AND	NSCU_ACHIEVED_YEAR IS NOT NULL
*       	AND	NCUN_NCVQ_CODE	= NSCU_NCUN_NCVQ_CODE
*       	AND	NVL(NCUN_CERTIFICATE_END_DATE,TRUNC(SYSDATE))
*       				< TRUNC(SYSDATE)
*       END-EXEC.
        CALL "SQLADR" USING SQ0065 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2538 TO SQL-OFFSET
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
            WS-NO-OF-EXPIRED-UNITS-1
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(2)
        MOVE 7 TO SQL-SQHSTL(2)
        MOVE 7 TO SQL-SQHSTS(2)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO ZT-900 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO ZT-900 END-IF.

ZT-030.
	IF WS-NO-OF-EXPIRED-UNITS-1 > ZERO
	THEN
	    MOVE 'Y' TO WS-NVQ-UNIT-CERT-EXPIRED

	    GO TO ZT-999
	END-IF.

ZT-040.

*       EXEC SQL WHENEVER SQLERROR	GO TO ZT-910	END-EXEC.
*       EXEC SQL WHENEVER SQLWARNING	CONTINUE	END-EXEC.
*       EXEC SQL WHENEVER NOT FOUND	GO TO ZT-910	END-EXEC.

*       EXEC SQL
*       	SELECT
*       		COUNT(1)
*       	INTO
*       		:WS-NO-OF-EXPIRED-UNITS-2
*       	FROM
*       		TRANSFER_NVQ_UNITS,
*       		NVQ_COMPETENCE_UNITS
*       	WHERE
*       		TNUN_ST_REG_NO	= :WS-REG-NO
*       	AND	TNUN_ACHIEVED_YEAR IS NOT NULL
*       	AND	NCUN_NCVQ_CODE	= TNUN_NCUN_NCVQ_CODE
*       	AND	NVL(NCUN_CERTIFICATE_END_DATE,TRUNC(SYSDATE))
*       				< TRUNC(SYSDATE)
*       END-EXEC.
        CALL "SQLADR" USING SQ0066 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2561 TO SQL-OFFSET
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
            WS-NO-OF-EXPIRED-UNITS-2
            SQL-SQHSTV(1)
        MOVE 4 TO SQL-SQHSTL(1)
        MOVE 4 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(2)
        MOVE 7 TO SQL-SQHSTL(2)
        MOVE 7 TO SQL-SQHSTS(2)
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
        IF SQLCODE IN SQLCA IS EQUAL TO 1403
            THEN GO TO ZT-910 END-IF
        IF SQLCODE IN SQLCA IS LESS THAN 0
            THEN GO TO ZT-910 END-IF.

	IF WS-NO-OF-EXPIRED-UNITS-2 > ZERO
	THEN
	    MOVE 'Y' TO WS-NVQ-UNIT-CERT-EXPIRED
	END-IF.

	GO TO ZT-999.

ZT-900.
	MOVE "SELECTION OF ACHIEVED NVQ UNITS FAILED" TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.

ZT-910.
	MOVE "SELECTION OF TRANSFERRED NVQ UNITS FAILED" TO WS-ERR-MESSAGE.
	PERFORM ZZ-ABORT.

ZT-999.                                                                        
	EXIT.
*
ZV-WRITE-TO-DB SECTION.
********************************************************************************
*    THIS SECTION WRITES OUTPUT TO TABLE AWARDS_RUN_OUTPUT_FILES               *
********************************************************************************
ZV-START.

*         EXEC SQL WHENEVER SQLERROR	GO TO ZV-900	END-EXEC.
*         EXEC SQL WHENEVER NOT FOUND	GO TO ZV-900	END-EXEC.

	  ADD 1 TO WS-AROF-RECORD-SEQUENCE.

	  MOVE WS-ANS-REC	TO 	WS-AROF-TEXT.
	  MOVE SPACES		TO	WS-AROF-MESSAGE.

          MOVE SPACES		TO 	WS-AROF-UNIT-NO.
          MOVE -1     		TO 	WS-AROF-UNIT-NO-I.

*         EXEC SQL EXECUTE
*           BEGIN
*             PR_WRITE_AWARDS_FILES_TO_DB
*                      ('STP070',
*       		:WS-CERT-FILE,
*       		:WS-AROF-RUN-TYPE,
*                       :WS-AROF-RECORD-TYPE,
*                       :WS-AROF-RECORD-SEQUENCE,
*       		:WS-AROF-FILE-TYPE,
*                       :WS-AROF-CENTRE:WS-AROF-CENTRE-I,
*                       :WS-AROF-REG-NO:WS-AROF-REG-NO-I,
*                       :WS-AROF-UNIT-NO:WS-AROF-UNIT-NO-I,
*                       :WS-AROF-TEXT,
*       		:WS-AROF-MESSAGE:WS-AROF-MESSAGE-I
*       	       );
*           END;
*         END-EXEC.
          CALL "SQLADR" USING SQ0067 SQL-STMT
          MOVE 1 TO SQL-ITERS
          MOVE 2584 TO SQL-OFFSET
          MOVE 0 TO SQL-OCCURS
          CALL "SQLADR" USING
              SQLCUD
              SQL-CUD
          CALL "SQLADR" USING
              SQLCA
              SQL-SQLEST
          MOVE 4352 TO SQL-SQLETY
          CALL "SQLADR" USING
              WS-CERT-FILE
              SQL-SQHSTV(1)
          MOVE 30 TO SQL-SQHSTL(1)
          MOVE 30 TO SQL-SQHSTS(1)
          MOVE 0 TO SQL-SQINDV(1)
          MOVE 0 TO SQL-SQINDS(1)
          MOVE 0 TO SQL-SQHARM(1)
          CALL "SQLADR" USING
              WS-AROF-RUN-TYPE
              SQL-SQHSTV(2)
          MOVE 1 TO SQL-SQHSTL(2)
          MOVE 1 TO SQL-SQHSTS(2)
          MOVE 0 TO SQL-SQINDV(2)
          MOVE 0 TO SQL-SQINDS(2)
          MOVE 0 TO SQL-SQHARM(2)
          CALL "SQLADR" USING
              WS-AROF-RECORD-TYPE
              SQL-SQHSTV(3)
          MOVE 1 TO SQL-SQHSTL(3)
          MOVE 1 TO SQL-SQHSTS(3)
          MOVE 0 TO SQL-SQINDV(3)
          MOVE 0 TO SQL-SQINDS(3)
          MOVE 0 TO SQL-SQHARM(3)
          CALL "SQLADR" USING
              WS-AROF-RECORD-SEQUENCE
              SQL-SQHSTV(4)
          MOVE 8 TO SQL-SQHSTL(4)
          MOVE 8 TO SQL-SQHSTS(4)
          MOVE 0 TO SQL-SQINDV(4)
          MOVE 0 TO SQL-SQINDS(4)
          MOVE 0 TO SQL-SQHARM(4)
          CALL "SQLADR" USING
              WS-AROF-FILE-TYPE
              SQL-SQHSTV(5)
          MOVE 8 TO SQL-SQHSTL(5)
          MOVE 8 TO SQL-SQHSTS(5)
          MOVE 0 TO SQL-SQINDV(5)
          MOVE 0 TO SQL-SQINDS(5)
          MOVE 0 TO SQL-SQHARM(5)
          CALL "SQLADR" USING
              WS-AROF-CENTRE
              SQL-SQHSTV(6)
          MOVE 6 TO SQL-SQHSTL(6)
          MOVE 6 TO SQL-SQHSTS(6)
          CALL "SQLADR" USING
              WS-AROF-CENTRE-I
              SQL-SQINDV(6)
          MOVE 0 TO SQL-SQINDS(6)
          MOVE 0 TO SQL-SQHARM(6)
          CALL "SQLADR" USING
              WS-AROF-REG-NO
              SQL-SQHSTV(7)
          MOVE 7 TO SQL-SQHSTL(7)
          MOVE 7 TO SQL-SQHSTS(7)
          CALL "SQLADR" USING
              WS-AROF-REG-NO-I
              SQL-SQINDV(7)
          MOVE 0 TO SQL-SQINDS(7)
          MOVE 0 TO SQL-SQHARM(7)
          CALL "SQLADR" USING
              WS-AROF-UNIT-NO
              SQL-SQHSTV(8)
          MOVE 8 TO SQL-SQHSTL(8)
          MOVE 8 TO SQL-SQHSTS(8)
          CALL "SQLADR" USING
              WS-AROF-UNIT-NO-I
              SQL-SQINDV(8)
          MOVE 0 TO SQL-SQINDS(8)
          MOVE 0 TO SQL-SQHARM(8)
          CALL "SQLADR" USING
              WS-AROF-TEXT
              SQL-SQHSTV(9)
          MOVE 2000 TO SQL-SQHSTL(9)
          MOVE 2000 TO SQL-SQHSTS(9)
          MOVE 0 TO SQL-SQINDV(9)
          MOVE 0 TO SQL-SQINDS(9)
          MOVE 0 TO SQL-SQHARM(9)
          CALL "SQLADR" USING
              WS-AROF-MESSAGE
              SQL-SQHSTV(10)
          MOVE 132 TO SQL-SQHSTL(10)
          MOVE 132 TO SQL-SQHSTS(10)
          CALL "SQLADR" USING
              WS-AROF-MESSAGE-I
              SQL-SQINDV(10)
          MOVE 0 TO SQL-SQINDS(10)
          MOVE 0 TO SQL-SQHARM(10)
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
              THEN GO TO ZV-900 END-IF
          IF SQLCODE IN SQLCA IS LESS THAN 0
              THEN GO TO ZV-900 END-IF.

	  IF WS-AROF-MESSAGE-I EQUAL -1
	    MOVE SPACES TO WS-AROF-MESSAGE
	  END-IF.

	  IF WS-AROF-MESSAGE EQUAL SPACES
	  THEN
	    GO TO ZV-999
	  END-IF.

ZV-900.
        MOVE 'INSERT TO AROF TABLE FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
ZV-999.                                                                        
	EXIT.
*
ZX-REJECT SECTION.
********************************************************************************
*    THIS SECTION CONTROLS THE OUTPUT OF REJECTED RECORD LINES                 *
********************************************************************************
ZX-START.
        MOVE S-CENTRE-NO          TO WS-REJECT-CENTRE-NO.
        MOVE S-COURSE-NO          TO WS-REJECT-COURSE-NO.
        MOVE S-COMB               TO WS-REJECT-COMB.
        MOVE S-REG-NO             TO WS-REJECT-REG-NO.
	MOVE S-DOC-REF            TO WS-REJECT-DOC-REF.
        IF WS-STORE-APPLICAT-NO = ZEROS 
               MOVE SPACES TO WS-REJECT-APPLICAT-NO
          ELSE MOVE WS-STORE-APPLICAT-NO TO WS-REJECT-APPLICAT-NO.
        MOVE WS-STORE-AWARD-CODE  TO WS-REJECT-AWARD-CODE.
        PERFORM ZY-PRINT-REJECT VARYING WS-REJECT-IND FROM 1 BY 1 
                UNTIL WS-REJECT-IND > 25.
ZX-100.
*       EXEC SQL WHENEVER SQLERROR  GO TO ZX-200  END-EXEC.
*
*       EXEC SQL INSERT INTO BTEC_REJECTED_STUDENTS 
*                       ( BRS_PROG_NO
*                       , BRS_RUN_DATE
*                       , BRS_REG_NO
*                       , BRS_CENTRE_NO
*                       , BRS_COURSE_NO )
*                SELECT 'STP070',
*                       SYSDATE,
*                       :WS-REG-NO,
*                       :WS-CURRENT-CENTRE-NO,
*                       :WS-COURSE-NO
*                FROM   SYSTEM.DUAL
*       END-EXEC.
        CALL "SQLADR" USING SQ0068 SQL-STMT
        MOVE 1 TO SQL-ITERS
        MOVE 2639 TO SQL-OFFSET
        MOVE 0 TO SQL-OCCURS
        CALL "SQLADR" USING
            SQLCUD
            SQL-CUD
        CALL "SQLADR" USING
            SQLCA
            SQL-SQLEST
        MOVE 4352 TO SQL-SQLETY
        CALL "SQLADR" USING
            WS-REG-NO
            SQL-SQHSTV(1)
        MOVE 7 TO SQL-SQHSTL(1)
        MOVE 7 TO SQL-SQHSTS(1)
        MOVE 0 TO SQL-SQINDV(1)
        MOVE 0 TO SQL-SQINDS(1)
        MOVE 0 TO SQL-SQHARM(1)
        CALL "SQLADR" USING
            WS-CURRENT-CENTRE-NO
            SQL-SQHSTV(2)
        MOVE 6 TO SQL-SQHSTL(2)
        MOVE 6 TO SQL-SQHSTS(2)
        MOVE 0 TO SQL-SQINDV(2)
        MOVE 0 TO SQL-SQINDS(2)
        MOVE 0 TO SQL-SQHARM(2)
        CALL "SQLADR" USING
            WS-COURSE-NO
            SQL-SQHSTV(3)
        MOVE 8 TO SQL-SQHSTL(3)
        MOVE 8 TO SQL-SQHSTS(3)
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
            THEN GO TO ZX-200 END-IF.
*
        ADD 1 TO WS-REJECT-REC-COUNT.
        IF WS-REJECT-REC-COUNT > 1000
               MOVE '1000 STUDENTS REJECTED' TO WS-ERR-MESSAGE
               PERFORM ZZ-ABORT.
        MOVE SPACES TO WS-REJECT-LINE.
        GO TO ZX-EXIT.
ZX-200.
        MOVE 'INSERT TO BTEC_REJECTED_STUDENTS TABLE FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
ZX-EXIT.
        EXIT.
*
/
ZY-PRINT-REJECT SECTION.              
********************************************************************************
*    THIS SECTION WRITES REJECTED RECORD LINES                                 *
********************************************************************************
ZY-START.
*
        IF WS-REJECT-NO(WS-REJECT-IND) = SPACES
                GO TO ZY-EXIT.
        SET WS-REJECT-DES-IND TO WS-REJECT-IND.
        MOVE WS-REJECT-DES(WS-REJECT-DES-IND) TO WS-REJECT-MESSAGE.
        IF (WS-REJECT-DES-IND = 1 OR 22) 
        AND WS-MERGED-CENTRE-NO NOT = WS-CENTRE-NO
        	MOVE WS-MERGED-CENTRE-NO TO WS-REJECT-MERGE-CTR
        END-IF.
        IF WS-REJECT-LINECOUNT < 55
                GO TO ZY-100.
*
        MOVE WS-REJECT-TOP-HEADING TO REJECT-REC.
        WRITE REJECT-REC AFTER PAGE.
        MOVE WS-REJECT-HEADING TO REJECT-REC.
        WRITE REJECT-REC AFTER 2.        
        MOVE WS-REJECT-HYPHEN TO REJECT-REC.
        WRITE REJECT-REC AFTER 1.        
        MOVE SPACES TO REJECT-REC.
        WRITE REJECT-REC AFTER 2.
        MOVE 3 TO WS-REJECT-LINECOUNT.
ZY-100.
        MOVE WS-REJECT-LINE TO REJECT-REC.
        WRITE REJECT-REC AFTER 1.        
        ADD 1 TO WS-REJECT-LINECOUNT.
        MOVE SPACES TO WS-REJECT-STUDENT.
        GO TO ZY-EXIT.
ZY-EXIT.
        EXIT.
*
/
ZZ-ABORT SECTION.
********************************************************************************
*    THIS SECTION ABORTS THE PROGRAM WITH AN APPROPRIATE ERROR MESSAGE.        *
********************************************************************************
ZZ-START.
	MOVE WS-ERR-MESSAGE TO REJECT-REC.
	WRITE REJECT-REC AFTER 4.
*
        MOVE 'S '          TO WS-ABORT-REC-TYPE.
        MOVE S-DOC-REF     TO WS-ABORT-DOC-REF.
        MOVE S-CENTRE-NO   TO WS-ABORT-CENTRE-NO.
        MOVE S-AWARD-CODE  TO WS-ABORT-AWARD-CODE.
        MOVE S-COURSE-NO   TO WS-ABORT-COURSE-NO.
        MOVE S-COMB        TO WS-ABORT-COMB.
        MOVE S-REG-NO      TO WS-ABORT-REG-NO.
        MOVE WS-ABORT-INFO TO REJECT-REC.
	WRITE REJECT-REC AFTER 2.
*
        MOVE 'NW '             TO WS-ABORT-REC-TYPE.
        MOVE WS-NEW-DOC-REF    TO WS-ABORT-DOC-REF.
        MOVE WS-NEW-CENTRE     TO WS-ABORT-CENTRE-NO.
        MOVE WS-NEW-AWARD-CODE TO WS-ABORT-AWARD-CODE.
        MOVE WS-NEW-COURSE     TO WS-ABORT-COURSE-NO.
        MOVE WS-NEW-COMB       TO WS-ABORT-COMB.
        MOVE WS-NEW-REG-NO     TO WS-ABORT-REG-NO.
        MOVE WS-ABORT-INFO TO REJECT-REC.
	WRITE REJECT-REC AFTER 2.
*
        MOVE 'WS'           TO WS-ABORT-REC-TYPE.
        MOVE WS-DOC-REF     TO WS-ABORT-DOC-REF.
        MOVE WS-CENTRE-NO   TO WS-ABORT-CENTRE-NO.
        MOVE WS-AWARD-CODE  TO WS-ABORT-AWARD-CODE.
        MOVE WS-COURSE-NO   TO WS-ABORT-COURSE-NO.
        MOVE WS-COMB        TO WS-ABORT-COMB.
        MOVE WS-REG-NO      TO WS-ABORT-REG-NO.
        IF WS-APPLICAT-NO = ZEROS
                MOVE SPACES TO WS-ABORT-APPLICAT-NO
           ELSE MOVE WS-APPLICAT-NO TO WS-ABORT-APPLICAT-NO.
        MOVE WS-AWARD-CODE  TO WS-ABORT-AWARD-CODE.
        MOVE WS-ABORT-INFO TO REJECT-REC.
	WRITE REJECT-REC AFTER 2.
*
	MOVE SQLERRMC TO REJECT-REC.
	WRITE REJECT-REC AFTER 2.
	MOVE SQLCODE TO WS-ERR-CODE.
	MOVE WS-ERR-NUM TO REJECT-REC.
	WRITE REJECT-REC AFTER 1.
*
*       EXEC SQL WHENEVER SQLERROR   GO TO ZZ-050 END-EXEC.
*       EXEC SQL WHENEVER SQLERROR   GO TO ZZ-100 END-EXEC.
*       EXEC SQL ROLLBACK WORK                    END-EXEC.
        MOVE 1 TO SQL-ITERS
        MOVE 2666 TO SQL-OFFSET
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
        GO TO ZZ-200.
ZZ-050.
	MOVE 'CLOSE CURSOR FAILED DURING PROGRAM ABORT' TO WS-ERR-MESSAGE.
	MOVE WS-ERR-MESSAGE TO REJECT-REC.
        WRITE REJECT-REC.

	IF WS-CERT-FILE NOT = SPACES
	THEN
	   MOVE WS-ERR-MESSAGE TO PRINT-REC
	   WRITE PRINT-REC
	END-IF.

        GO TO ZZ-200.
ZZ-100.
	MOVE 'ROLLBACK FAILED DURING PROGRAM ABORT' TO WS-ERR-MESSAGE.
	MOVE WS-ERR-MESSAGE TO REJECT-REC.
	WRITE REJECT-REC.

	IF WS-CERT-FILE NOT = SPACES
	THEN
	   MOVE WS-ERR-MESSAGE TO PRINT-REC
	   WRITE PRINT-REC
	END-IF.

ZZ-200.
	MOVE 'THE PROGRAMME ABORTED' TO REJECT-REC.
        WRITE REJECT-REC.
	CLOSE REJECT-FILE
	      LABELS-FILE.

	IF WS-CERT-FILE NOT = SPACES
	THEN
	   MOVE 'THE PROGRAMME ABORTED' TO PRINT-REC
	   WRITE PRINT-REC
	   CLOSE PRINT-FILE
	END-IF.

        PERFORM ZZ-500.
*
ZZ-500.
*
        MOVE SQLERRM      TO WS-SQL-ERR-MESS.
*
        DISPLAY WS-ABORT-LINE-1.
        DISPLAY WS-ABORT-LINE-2.
        DISPLAY WS-ABORT-LINE-3.
        DISPLAY WS-ABORT-LINE-2.
        DISPLAY WS-ABORT-LINE-4.
        DISPLAY WS-ABORT-LINE-2.
        DISPLAY WS-ABORT-LINE-5.
        DISPLAY WS-ABORT-LINE-2.
        DISPLAY WS-ABORT-LINE-1.
*
        MOVE WS-ABORT-VAL TO WS-ABORT.
        CALL "SYS$EXIT" USING BY VALUE WS-ABORT.
*
        STOP RUN.
ZZ-EXIT.
	EXIT.
/	
