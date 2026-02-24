IDENTIFICATION DIVISION.
PROGRAM-ID.   SSPDFE.
AUTHOR.       CTS.
DATE-WRITTEN. 06-SEP-2023.
********************************************************************************
*
*       BATH DATAFEED (ISSUED AWARDS + REGNS)
*
********************************************************************************
* Amendment Log :                                                              *
*                                                                              *
* When        Who      What                                                    *
* ----------  -------  ------------------------------------------------------- *
*
* 06-Sep-2023 CTS      EC5718 - DFE Report Generation Process To Insert/Update 
*                      learners' details into DFE_REPORT_DETAILS table and to 
*                      capture any of it's changes of the mandatory columns
*                      in the DFE_REPORT_DETAILS_AUDIT_TRAIL table.  Generate
*                      BTEC and NVQ Learners details to the business thru FTP.
* 23-Apr-2024 CTS      EC5877 - Update in the Order By clause to have at_number
*                      as one of the columns in the BTEC Cursor.
********************************************************************************
ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. VAX-11.
OBJECT-COMPUTER. VAX-11.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT  CSV-FILE-B    ASSIGN  SSPDFEB.
    SELECT  CSV-FILE-N    ASSIGN  SSPDFEN.
    SELECT  IGNORE-RPT    ASSIGN  SSPDFEI.
    SELECT  TOTALS-RPT    ASSIGN  SSPDFETOT.
*
DATA DIVISION.
FILE SECTION.
*
FD  CSV-FILE-B
    RECORD VARYING FROM 1 TO 950
    DEPENDING ON WS-REC-LENGTH.
01  DB-DETAIL-RECORD          PIC X(950).
*
FD  CSV-FILE-N
    RECORD VARYING FROM 1 TO 950
    DEPENDING ON WS-REC-LENGTH.
01  DN-DETAIL-RECORD          PIC X(950).
*
FD  IGNORE-RPT
    RECORD VARYING FROM 1 TO 80
    DEPENDING ON WS-REC-LENGTH.
01  DI-DETAIL-RECORD          PIC X(80).
*
FD  TOTALS-RPT
    LABEL RECORDS STANDARD.
01  TOT-RECORD                PIC X(132).
*
WORKING-STORAGE SECTION.
*                                               
        EXEC SQL BEGIN DECLARE SECTION END-EXEC.
*
01  DS01-COURSE-DETAILS.
    03  DS01-REC-TYPE               PIC X(9) OCCURS 500.
    03  DS01-UNIQUE-CANDIDATE-IDR   PIC X(13) OCCURS 500.
    03  DS01-UNIQUE-LEARNER-NUM     PIC X(10) OCCURS 500.
    03  DS01-UNIQUE-PUPIL-NUM       PIC X(2) OCCURS 500.
    03  DS01-CANDIDATE-IDR-OTHER    PIC X(7) OCCURS 500.
    03  DS01-FIRSTNAME-DB           PIC X(35) OCCURS 500.
    03  DS01-FIRSTNAME-RPT          PIC X(35) OCCURS 500.
    03  DS01-SURNAME-DB             PIC X(35) OCCURS 500.
    03  DS01-SURNAME-RPT            PIC X(35) OCCURS 500.
    03  DS01-GENDER                 PIC X(2) OCCURS 500.
    03  DS01-DOB                    PIC X(10) OCCURS 500.
    03  DS01-NCN                    PIC X(6) OCCURS 500.
    03  DS01-CENTREURN              PIC S9(6) COMP OCCURS 500.
    03  DS01-LEA                    PIC X(3) OCCURS 500.
    03  DS01-CENTRE-IDR-OTHER       PIC X(6) OCCURS 500.
    03  DS01-UKPRN                  PIC S9(8) COMP OCCURS 500.
    03  DS01-CENTRENAME             PIC X(65) OCCURS 500.
    03  DS01-CENTREADDRESS1         PIC X(150) OCCURS 500.
    03  DS01-CENTREADDRESS2         PIC X(36) OCCURS 500.
    03  DS01-CENTREADDRESS3         PIC X(30) OCCURS 500.
    03  DS01-CENTREADDRESS4         PIC X(25) OCCURS 500.
    03  DS01-CENTREPOSTCODE         PIC X(10) OCCURS 500.
    03  DS01-CENTRETYPE             PIC X(2) OCCURS 500.
    03  DS01-QUAL-NUMBER-TYPE       PIC X(5) OCCURS 500.
    03  DS01-QUAL-NUMBER            PIC X(10) OCCURS 500.
    03  DS01-SPEC-CODE              PIC X(8) OCCURS 500.
    03  DS01-SPEC-TITLE             PIC X(150) OCCURS 500.
    03  DS01-QUAL-TYPE              PIC X(100) OCCURS 500.
    03  DS01-EXAMSERIES             PIC X(2) OCCURS 500.
    03  DS01-REG-DATE               PIC X(10) OCCURS 500.
    03  DS01-FIRST-ENTRY-DATE       PIC X(10) OCCURS 500.
    03  DS01-AWARD-DATE             PIC X(10) OCCURS 500.
    03  DS01-QUAL-GRADE             PIC X(3) OCCURS 500.
    03  DS01-PRIVATE-CANDIDATE      PIC S9(4) COMP  OCCURS 500.
    03  DS01-PARTIAL-ABSENCE        PIC S9(4) COMP  OCCURS 500.
    03  DS01-CERT-IND               PIC X(1) OCCURS 500.
    03  DS01-COURSE                 PIC X(8) OCCURS 500.
    03  DS01-AT-NUMBER              PIC 9(6) OCCURS 500.
    03  DS01-BNM-TYPE               PIC X(1) OCCURS 500. 

01  DS02-COURSE-DETAILS.
    03  DS02-REC-TYPE               PIC X(9) OCCURS 500.
    03  DS02-CANDIDATE-IDR-OTHER    PIC X(7) OCCURS 500.
    03  DS02-FIRSTNAME-DB           PIC X(35) OCCURS 500.
    03  DS02-SURNAME-DB             PIC X(35) OCCURS 500.
    03  DS02-GENDER                 PIC X(2) OCCURS 500.
    03  DS02-DOB                    PIC X(10) OCCURS 500.
    03  DS02-NCN                    PIC X(6) OCCURS 500.
    03  DS02-CENTREURN              PIC S9(6) COMP OCCURS 500.
    03  DS02-CENTRE-IDR-OTHER       PIC X(6) OCCURS 500.
    03  DS02-UKPRN                  PIC S9(8) COMP OCCURS 500.
    03  DS02-CENTRENAME             PIC X(65) OCCURS 500.
    03  DS02-CENTREADDRESS1         PIC X(150) OCCURS 500.
    03  DS02-CENTREADDRESS2         PIC X(36) OCCURS 500.
    03  DS02-CENTREADDRESS3         PIC X(30) OCCURS 500.
    03  DS02-CENTREADDRESS4         PIC X(25) OCCURS 500.
    03  DS02-CENTREPOSTCODE         PIC X(10) OCCURS 500.
    03  DS02-CENTRETYPE             PIC X(2) OCCURS 500.
    03  DS02-QUAL-NUMBER-TYPE       PIC X(5) OCCURS 500.
    03  DS02-QUAL-NUMBER            PIC X(10) OCCURS 500.
    03  DS02-SPEC-CODE              PIC X(8) OCCURS 500.
    03  DS02-SPEC-TITLE             PIC X(150) OCCURS 500.
    03  DS02-QUAL-TYPE              PIC X(100) OCCURS 500.
    03  DS02-REG-DATE               PIC X(10) OCCURS 500.
    03  DS02-FIRST-ENTRY-DATE       PIC X(10) OCCURS 500.
    03  DS02-AWARD-DATE             PIC X(10) OCCURS 500.
    03  DS02-QUAL-GRADE             PIC X(3) OCCURS 500.

    03  WS-START-DATE               PIC  X(11).
    03  WS-END-DATE                 PIC  X(11).
    03  WS-AMEND-CUTOFF-DATE        PIC  X(11).
    03  WS-ACAD-YEAR-RERUN          PIC  X(3).
    03  WS-REG-START-DATE           PIC  X(11).
    03  WS-REG-END-DATE             PIC  X(11).
    03  WS-ACAD-START-DATE          PIC  X(11).
    03  WS-NG-GRADING-DATE          PIC  X(10).
    03  WS-CANDIDATE                PIC  X(7).
    03  WS-RESTRICT-SPL-CHARS       PIC  X(80).

    03  WS-RUN-DATE                 PIC  X(11).
    03  WS-REC-TYPE                 PIC  X(09).
    03  WS-PROCESS                  PIC  X(1).
        88 BTEC-PROCESS             VALUE 'B'.
        88 NVQ-PROCESS              VALUE 'N'.
    03  WS-BNM-TYPE                 PIC  X(1).
        88  BTEC-NG                 VALUE "G".
        88  BTEC                    VALUE "B".
        88  ALL-OTHERS              VALUE "O".
    03  WS-CERTIFICATE-IND          PIC  X(1).
        88  CERTIFICATED            VALUE "Y".
    03  WS-AWARD-DATE-FUTURE        PIC  X(1).
        88  AWARD-DATE-FUTURE       VALUE 'Y'.
    03  WS-DAUD-SEQUENCE            PIC S9(09) COMP.
    03  WS-START-OR-END             PIC  X(1).
        88  START-OF-RUN            VALUE "S".
        88  END-OF-RUN              VALUE "E".
    03  WS-DFE-INS-UPDAT          PIC  X(1).
        88  DFE-INS               VALUE "I".
        88  DFE-UPDAT             VALUE "U".
    03  WS-COL-UPDATE             PIC  X(1).
        88  COL-UPDATE            VALUE 'Y'.
    03  WS-RES-UPDATE             PIC  X(1).
        88  RES-UPDATE            VALUE 'Y'.
    03  WS-FIRST-TIME             PIC  X(1).
        88  FIRST-TIME            VALUE 'Y'.
    03  WS-IGN-FIRST-TIME         PIC  X(1).
        88  IGN-FIRST-TIME        VALUE 'Y'.
    03  WS-SKIP                   PIC X(1).
        88  SKIP                  VALUE 'Y'.
*
01  WS-ERR-MESSAGE                PIC X(132)
                                  VALUE SPACES.

01  WS-BLOCK-IND                  PIC  X(1).
    88  BLOCKED                   VALUE "Y".
01  WS-STATUS                     PIC X(1).
01  WS-STATUS-I                   PIC S9(4) COMP.
01  WS-STATUS2                    PIC X(10).
01  WS-STATUS2-I                  PIC S9(4) COMP.
01  WS-BLOCK-STATUS               PIC X(1).
01  WS-BLOCK-STATUS-I             PIC S9(4) COMP.
01  WS-CHECK-UNITS                PIC X(1)  VALUE 'N'.
01  WS-NPP-ID                     PIC S9(6) COMP.
01  WS-NPP-ID-I                   PIC S9(4) COMP.
01  WS-DECISION-DATE              PIC X(11).
01  WS-DECISION-DATE-I            PIC S9(4) COMP.
01  WS-MESSAGE                    PIC X(100).
01  WS-MESSAGE-I                  PIC S9(4) COMP.
01  WS-TIME-DATE                  PIC X(17).
01  WS-USER-NAME                  PIC X(15) VALUE 'DFE_JOB'.
*
01  DS03-LOGIN.
    03  DS03-USERNAME             PIC  X(04) VALUE "ABCD".
    03  DS03-PASSWORD             PIC  X(04) VALUE "ABCD".
*
        EXEC SQL END DECLARE SECTION END-EXEC.
*
        EXEC SQL INCLUDE SQLCA END-EXEC.
*
01      WS-REC-LENGTH             PIC 9(4) COMP.
*
01  WS01-GENERAL-STORAGE.
    03  WS01-ERR-MESSAGE          PIC  X(80).
    03  WS01-EOF-IND              PIC  X(01).
        88  WS01-EOF              VALUE "Y".
    03  WS01-ROWS-TOTAL           PIC S9(09) COMP.
    03  WS-BTEC-ROWS-TOTAL        PIC 9(9).
    03  WS-NVQ-ROWS-TOTAL         PIC 9(9).
    03  WS01-ROWS-THIS-FETCH      PIC S9(09) COMP.
    03  WS01-INDEX                PIC S9(09) COMP.
    03  WS01-ABORT                PIC  9(09) COMP VALUE 4.
*
01  WS-DRD-CURR-RECORD.
    03  WS-C-UNIQUE-CANDIDATE-IDR PIC X(13).
    03  WS-C-UNIQUE-LEARNER-NUM   PIC X(10).
    03  WS-C-UNIQUE-PUPIL-NUM     PIC X(2).
    03  WS-C-CANDIDATE-IDR-OTHER  PIC X(7).
    03  WS-C-FIRSTNAME            PIC X(35).
    03  WS-C-SURNAME              PIC X(35).
    03  WS-C-GENDER               PIC X(2).
    03  WS-C-DOB                  PIC X(10).
    03  WS-C-NCN                  PIC X(6).
    03  WS-C-CENTREURN            PIC S9(6) COMP.
    03  WS-C-LEA                  PIC X(3).
    03  WS-C-CENTRE-IDR-OTHER     PIC X(6).
    03  WS-C-UKPRN                PIC S9(8) COMP.
    03  WS-C-CENTRENAME           PIC X(65).
    03  WS-C-CENTREADDRESS1       PIC X(150).
    03  WS-C-CENTREADDRESS2       PIC X(36).
    03  WS-C-CENTREADDRESS3       PIC X(30).
    03  WS-C-CENTREADDRESS4       PIC X(25).
    03  WS-C-CENTREPOSTCODE       PIC X(10).
    03  WS-C-CENTRETYPE           PIC X(2).
    03  WS-C-QUAL-NUMBER-TYPE     PIC X(5).
    03  WS-C-QUAL-NUMBER          PIC X(10).
    03  WS-C-SPEC-CODE            PIC X(8).
    03  WS-C-SPEC-TITLE           PIC X(150).
    03  WS-C-QUAL-TYPE            PIC X(100).
    03  WS-C-EXAMSERIES           PIC X(2).
    03  WS-C-REG-DATE             PIC X(10).
    03  WS-C-FIRST-ENTRY-DATE     PIC X(10).
    03  WS-C-AWARD-DATE           PIC X(10).
    03  WS-C-QUAL-GRADE           PIC X(3).
    03  WS-C-PRIVATE-CANDIDATE    PIC S9(4) COMP.
    03  WS-C-PARTIAL-ABSENCE      PIC S9(4) COMP.
    03  WS-C-PROG-TYPE            PIC X(1).
*
01  WS-DRD-PREV-RECORD.
    03  WS-P-REC-TYPE             PIC X(9).
    03  WS-P-UNIQUE-CANDIDATE-IDR PIC X(13).
    03  WS-P-UNIQUE-LEARNER-NUM   PIC X(10).
    03  WS-P-CANDIDATE-IDR-OTHER  PIC X(7).
    03  WS-P-FIRSTNAME            PIC X(35).
    03  WS-P-SURNAME              PIC X(35).
    03  WS-P-GENDER               PIC X(2).
    03  WS-P-DOB                  PIC X(10).
    03  WS-P-NCN                  PIC X(6).
    03  WS-P-CENTREURN            PIC S9(6) COMP.
    03  WS-P-CENTRE-IDR-OTHER     PIC X(6).
    03  WS-P-UKPRN                PIC S9(8) COMP.
    03  WS-P-CENTRENAME           PIC X(65).
    03  WS-P-CENTREADDRESS1       PIC X(150).
    03  WS-P-CENTREADDRESS2       PIC X(36).
    03  WS-P-CENTREADDRESS3       PIC X(30).
    03  WS-P-CENTREADDRESS4       PIC X(25).
    03  WS-P-CENTREPOSTCODE       PIC X(10).
    03  WS-P-CENTRETYPE           PIC X(2).
    03  WS-P-QUAL-NUMBER-TYPE     PIC X(5).
    03  WS-P-QUAL-NUMBER          PIC X(10).
    03  WS-P-SPEC-CODE            PIC X(8).
    03  WS-P-SPEC-TITLE           PIC X(150).
    03  WS-P-QUAL-TYPE            PIC X(100).
    03  WS-P-REG-DATE             PIC X(10).
    03  WS-P-FIRST-ENTRY-DATE     PIC X(10).
    03  WS-P-AWARD-DATE           PIC X(10).
    03  WS-P-QUAL-GRADE           PIC X(3).
*
01  WS-COUNT-VAR.
    03  WS-BTEC-UPD-RESULT-CNT    PIC 9(9).
    03  WS-BTEC-UPD-AMEND-CNT     PIC 9(9).
    03  WS-BTEC-TOTAL-UPD-CNT     PIC 9(9).
    03  WS-BLOCKED-LEARNER-CNT    PIC 9(9).
    03  WS-BTEC-INSERT-CNT        PIC 9(9).
    03  WS-BTEC-CNT               PIC 9(9).
    03  WS-NVQ-CNT                PIC 9(9).
    03  WS-BTEC-SKIPPED-CNT       PIC 9(9).
    03  WS-BTEC-IGN-CNT           PIC 9(9).
    03  WS-NVQ-UPD-RESULT-CNT     PIC 9(9).
    03  WS-NVQ-UPD-AMEND-CNT      PIC 9(9).
    03  WS-NVQ-TOTAL-UPD-CNT      PIC 9(9).
    03  WS-NVQ-INSERT-CNT         PIC 9(9).
    03  WS-NVQ-SKIPPED-CNT        PIC 9(9).
    03  WS-NVQ-IGN-CNT            PIC 9(9).
    03  WS-CHECK-BLOCKING-CNT     PIC 9(9).
    03  WS-DFE-IGN-CNT            PIC 9(9).
*
01  WS-DFE-REPORT-HDR.
    03  FILLER                  PIC X(60) VALUE
       'RecordType, UniqueCandidateIdentifier, UniqueLearnerNumber, '. 
    03  FILLER                  PIC X(56) VALUE
       'UniquePupilNumber, CandidateIdentifierOther, FirstName, '.
    03  FILLER                  PIC X(57) VALUE
       'MiddleNames, Surname, Sex, DOB, NCN, CentreURN, LAESTAB, '.
    03  FILLER                  PIC X(58) VALUE
       'CentreIdentifierOther, UKPRN, CentreName, CentreAddress1, '.
    03  FILLER                  PIC X(64) VALUE
       'CentreAddress2, CentreAddress3, CentreAddress4, CentrePostCode, '.
    03  FILLER                  PIC X(58) VALUE
       'CentreType, QualificationNumberType, QualificationNumber, '.
    03  FILLER                  PIC X(58) VALUE
       'SpecificationCode, SpecificationTitle, QualificationType, '.
    03  FILLER                  PIC X(57) VALUE
       'ExamSeries, RegistrationDate, FirstEntryDate, AwardDate, '.
    03  FILLER                PIC X(52) VALUE
       'QualificationGrade, PrivateCandidate, PartialAbsence'.
*
01  WS-RPT-COLS.
    03  WS-RPT-FIRSTNAME            PIC X(35).
    03  WS-RPT-SURNAME              PIC X(35).
    03  WS-RPT-CENTREURN            PIC S9(6)
           SIGN LEADING SEPARATE VALUE -2.
    03  WS-RPT-UKPRN                PIC S9(8)
           SIGN LEADING SEPARATE VALUE -2.
    03  WS-RPT-CENTREPOSTCODE       PIC X(10).
    03  WS-RPT-QUAL-NUMBER          PIC X(10).
    03  WS-RPT-QUAL-TYPE            PIC X(100).
*
01  WS-DFE-REPORT-DETAIL            PIC X(850) VALUE SPACES.
01  WS-DFE-IGNORE-RPT               PIC X(80)  VALUE SPACES.
*
01  WS-DFE-IGNORE-LNR-HDR.
    03  FILLER                      PIC X(16)  VALUE
        'Ignored Learners'.
*
01  WS-DFE-IGNORE-LNR-DTL.
    03  WS-DFE-IGN-REG-NO           PIC X(7).
    03  FILLER                      PIC X(73).
*
01  WS-TOTALS-REPORT.
    03 WS-TR-HEAD-1.
       05 FILLER                  PIC X(11) VALUE 'Run Date :'.
       05 WS-TR-H1-RUN-DATE       PIC X(11).
       05 FILLER                  PIC X(104) VALUE SPACES.
       05 FILLER                  PIC X(6)  VALUE 'SSPDFE'.
    03 WS-TR-HEAD-2.
       05 FILLER                  PIC X(52) VALUE SPACES.
       05 FILLER                  PIC X(29) VALUE
         'DFEE Registration File Totals'.
       05 FILLER                  PIC X(51) VALUE SPACES.
    03 WS-TR-HEAD-3.
       05 FILLER                  PIC X(53) VALUE SPACES.
       05 WS-TR-H3-START-DATE PIC X(11).
       05 FILLER                  PIC X(4)  VALUE ' to '.
       05 WS-TR-H3-END-DATE PIC X(11).
       05 FILLER                  PIC X(53) VALUE SPACES.
    03 WS-TR-DETAIL.
       05 FILLER                  PIC X(14) VALUE SPACES.
       05 FILLER                  PIC X(7)  VALUE 'BTEC : '.
       05 WS-TR-DET-BTEC-TOTAL PIC Z(8)9.
       05 FILLER                  PIC X(28) VALUE SPACES.
       05 FILLER                  PIC X(7)  VALUE ' NVQ : '.
       05 WS-TR-DET-NVQ-TOTAL     PIC Z(8)9.
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

        MOVE   'Y'                 TO  WS-IGN-FIRST-TIME.

*BTEC Process 
        MOVE    ZERO               TO  WS01-ROWS-TOTAL.
        MOVE   "N"                 TO  WS01-EOF-IND.
        MOVE    'B'                TO  WS-PROCESS.
        MOVE   'Y'                 TO  WS-FIRST-TIME.
        PERFORM C-PROCESS-BTEC.

*NVQ Process 
        MOVE    ZERO               TO  WS01-ROWS-TOTAL.
        MOVE   "N"                 TO  WS01-EOF-IND.
        MOVE    'N'                TO  WS-PROCESS.
        MOVE   'Y'                 TO  WS-FIRST-TIME.
        PERFORM D-PROCESS-NVQ.
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
        ACCEPT WS-AMEND-CUTOFF-DATE.
        ACCEPT WS-ACAD-YEAR-RERUN.
        ACCEPT WS-REG-START-DATE.
        ACCEPT WS-REG-END-DATE.

        
        OPEN    OUTPUT  CSV-FILE-B.
        OPEN    OUTPUT  CSV-FILE-N.
        OPEN    OUTPUT  IGNORE-RPT.
        OPEN    OUTPUT  TOTALS-RPT.
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
             ALTER SESSION SET OPTIMIZER_MODE=all_rows
        END-EXEC.
*
        EXEC SQL
            SELECT TO_CHAR (SYSDATE, 'DD-MON-YYYY:HH24:MI')
            INTO   :WS-TIME-DATE
            FROM   DUAL
        END-EXEC.

        EXEC SQL
             SELECT RV_DESCRIPTION
             INTO   :WS-RESTRICT-SPL-CHARS
             FROM   REF_VALUES 
             WHERE  RV_DOMAIN = 'SSDDFE'
             AND    RV_CODE   = 'DFE_RSC'
        END-EXEC.

        DISPLAY ' SSPDFE STARTED AT : ' WS-TIME-DATE

        MOVE   "ERROR CALCULATING FIRST ENTRY DATE"
                                   TO  WS01-ERR-MESSAGE.
*
        EXEC SQL
          SELECT NVL(TO_CHAR(FN_FIRST_ENTRY_DATE(TRUNC(SYSDATE))
            ,'YYYY-MM-DD'),'2999-12-31')
          INTO   :WS-NG-GRADING-DATE
          FROM DUAL
        END-EXEC.
*
        EXEC SQL
          SELECT FN_GET_ACAD_YEAR(:WS-START-DATE)
          INTO   :WS-ACAD-START-DATE
          FROM DUAL
        END-EXEC.
*
        EXEC SQL
            SELECT TO_CHAR(SYSDATE,'DD-MON-YYYY') 
              INTO WS-RUN-DATE
              FROM DUAL
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
        SELECT 
        '-2' AS UniqueCandidateIdentifier,
        '-2' AS UniqueLearnerNumber,
        '-2' AS UniquePupilNumber,
        st_reg_no AS CandidateIdentifierOther,
        st_forenames AS FirstNameDB,
        NVL(TRIM(TRANSLATE(st_forenames,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS FirstNameRPT,
        st_surname AS SurnameDB,
        NVL(TRIM(TRANSLATE(st_surname,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS SurnameRPT,
        DECODE(st_sex,'M','M','F','F','-2') AS Sex,
        CASE 
          WHEN ST_BIRTH_DATE IS NULL 
            THEN '2999-12-31'
          WHEN ST_BIRTH_DATE <= TO_DATE('1900-01-01','YYYY-MM-DD')
            THEN '2999-12-31'
          ELSE
            TO_CHAR(ST_BIRTH_DATE,'YYYY-MM-DD')
        END AS DOB,
        fn_get_ncn(st_centre_id) AS NCN,
        NVL(ecen_urn,-2) AS CentreURN, 
        '-2' AS LAESTAB,
        st_centre_id AS CentreIdentifierOther,
        decode(LENGTH(ecen_ukprn),8,ecen_ukprn,-2) AS UKPRN,
        cn_centre_name AS CentreName,
        NVL(cn_address_1||DECODE(cn_address_1,NULL,NULL,' ')
        ||cn_address_2,' ') AS CentreAddress1,
        NVL(cn_address_3,' ') AS CentreAddress2,
        NVL(cn_town,' ') AS CentreAddress3,
        NVL(cn_county,' ') AS CentreAddress4,
        NVL(cn_postcode,'-2') AS CentrePostcode,
        DECODE(cn_bure_buni_id,0,'12',
            DECODE(cn_status,0,'9',
                             2,'5',
                             3,'1',
                             4,'11',
                             5,'2',
                             6,'3',
                             7,'1',
                             8,'10',
                             9,'12',
                             10,'6',
                             11,'2',
                             12,'13',
                             13,'5',
                             14,'13','13')) AS CentreType, 
        aaco_qca_code
         AS QualificationNumber,
        AA_BTEC_TITLE AS SpecificationCode,
        at_name AS SpecificationTitle,
        FN_GET_DFE_QUAL_TYPE(aaco_qca_code) AS QualificationType,
        '-2' AS ExamSeries,
        NVL(TO_CHAR(st_reg_date,'YYYY-MM-DD'),'2999-12-31')
         AS RegistrationDate,
        CASE
          WHEN AC_BNM_TYPE <> 'G' OR ST_AWARD_ISSUE IS NOT NULL
            THEN  NVL(TO_CHAR(st_award_issue,'YYYY-MM-DD'),'2999-12-31')
          WHEN ST_AWARD_ISSUE IS NULL AND ST_BNM_GRADE IS NULL
            THEN '2999-12-31'
          WHEN ST_AWARD_ISSUE IS NULL AND ST_BNM_GRADE IS NOT NULL 
               AND (TO_DATE(:WS-NG-GRADING-DATE,'YYYY-MM-DD') > SYSDATE )
            THEN 
              FN_GET_LATEST_RESULT_DATE(st_reg_no,:ws-acad-start-date)
          ELSE :WS-NG-GRADING-DATE
        END AS FirstEntryDate,
        NULL AS AwardDate,
        DECODE(ac_bnm_type,
               'G',
                DECODE(st_award_issue,NULL,
           DECODE(st_bnm_grade,NULL,'-2',
                DECODE(aw_award_code,'Y6',
                        DECODE(FN_CHECK_TA_GRADE_VISIBLE_EOL(ST_REG_NO),
                               'Y',
                               st_bnm_grade,
                               '-2'
                              ),
             st_bnm_grade  
             )
            ), 
                      NVL(NVL(st_over_grade,st_bnm_grade),'P')
                      ),
                DECODE(st_award_issue,
                       NULL,
                       '-2',
                       NVL(NVL(st_over_grade,st_bnm_grade),'P')
             )
          ) AS QualificationGrade,
        -2 AS PrivateCandidate,
        -2 AS PartialAbsence,
        DECODE(ac_bnm_type,'G','G','B') AS btec_ng_type,
        DECODE(st_award_issue,NULL,'N','Y') AS Cert_Ind,
        st_course_id AS Course_id,
        at_number,
        NULL AS drd_rec_type,
        NULL AS drd_st_reg_no,
        NULL AS drd_forenames,
        NULL AS drd_surname,
        NULL AS drd_sex,
        NULL AS drd_birth_date, 
        NULL AS drd_ncn,
        NULL AS drd_urn,
        NULL AS drd_centre_id,
        NULL AS drd_ukprn,
        NULL AS drd_centre_name,
        NULL AS drd_address_1,
        NULL AS drd_address_2,
        NULL AS drd_address_3,
        NULL AS drd_address_4,
        NULL AS drd_postcode,
        NULL AS drd_centre_type,
        NULL AS drd_qual_num_type,
        NULL AS drd_qual_number,
        NULL AS drd_spec_code,
        NULL AS drd_spec_title,
        NULL AS drd_qual_type,
        NULL AS drd_reg_date,
        NULL AS drd_first_entry_date,
        NULL AS drd_award_date,
        NULL AS drd_qual_grade
        FROM students x,
             centres,
             legacy_centres,
             edexcel_centres,
             approval_awards,
             approval_application,
             at_award_codes,
             award_titles,
             award_codes
        WHERE st_centre_id             = cn_centre_id
          AND cn_centre_id             = lcen_centre_ref
          AND lcen_ecen_id             = ecen_id  
          AND st_course_id             = aw_course_number
          AND aw_applicat_no           = aa_applicat_no
          AND TO_NUMBER(aa_btec_title) = at_number
          AND aaco_at_number           = at_number
          AND aaco_ac_code             = aw_award_code
          AND ac_code                  = aw_award_code
          AND aaco_qca_code            IS NOT NULL
          AND st_delete                IS NULL
          AND st_fallback              IS NULL
          AND st_university_ind        IS NULL
          AND lcen_ltyp_id             = 1
          AND aw_award_code NOT IN 
             ('01','02','03','04','05','06','11',
              '27','64','35','63','93','HH','GG',
              'GA','GB','CC','EW','FS','FO')
          AND (
                (st_award_issue 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-YYYY') 
                     AND TO_DATE(:WS-END-DATE,'DD-MON-YYYY')
                )
          OR
                (st_reg_date 
                  BETWEEN TO_DATE(:WS-REG-START-DATE,'DD-MON-YYYY') 
                      AND TO_DATE(:WS-REG-END-DATE,'DD-MON-YYYY')
            AND st_award_issue IS NULL
                )
              )
           AND NOT EXISTS 
               (SELECT 1
                  FROM dfe_report_details
                 WHERE drd_st_reg_no = st_reg_no
               )
        UNION ALL
        SELECT 
        '-2' AS UniqueCandidateIdentifier,
        '-2' AS UniqueLearnerNumber,
        '-2' AS UniquePupilNumber,
        st_reg_no AS CandidateIdentifierOther,
        st_forenames AS FirstNameDB,
        NVL(TRIM(TRANSLATE(st_forenames,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS FirstNameRPT,
        st_surname AS SurnameDB,
        NVL(TRIM(TRANSLATE(st_surname,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS SurnameRPT,
        DECODE(st_sex,'M','M','F','F','-2') AS Sex,
        CASE 
          WHEN ST_BIRTH_DATE IS NULL 
            THEN '2999-12-31'
          WHEN ST_BIRTH_DATE <= TO_DATE('1900-01-01','YYYY-MM-DD')
            THEN '2999-12-31'
          ELSE
            TO_CHAR(ST_BIRTH_DATE,'YYYY-MM-DD')
        END AS DOB,
        fn_get_ncn(st_centre_id) AS NCN,
        NVL(ecen_urn,-2) AS CentreURN, 
        '-2' AS LAESTAB,
        st_centre_id AS CentreIdentifierOther,
        decode(LENGTH(ecen_ukprn),8,ecen_ukprn,-2) AS UKPRN,
        cn_centre_name AS CentreName,
        NVL(cn_address_1||
        decode(cn_address_1,null,null,' ')
        ||cn_address_2,' ') AS CentreAddress1,
        NVL(cn_address_3,' ') AS CentreAddress2,
        NVL(cn_town,' ') AS CentreAddress3,
        NVL(cn_county,' ') AS CentreAddress4,
        NVL(cn_postcode,'-2') AS CentrePostcode,
        DECODE(cn_bure_buni_id,0,'12',
            DECODE(cn_status,0,'9',
                             2,'5',
                             3,'1',
                             4,'11',
                             5,'2',
                             6,'3',
                             7,'1',
                             8,'10',
                             9,'12',
                             10,'6',
                             11,'2',
                             12,'13',
                             13,'5',
                             14,'13','13')) AS CentreType, 
        aaco_qca_code
          AS QualificationNumber,
        AA_BTEC_TITLE AS SpecificationCode,
        at_name AS SpecificationTitle,
        FN_GET_DFE_QUAL_TYPE(aaco_qca_code) AS QualificationType,
        '-2' AS ExamSeries,
        NVL(TO_CHAR(st_reg_date,'YYYY-MM-DD'),'2999-12-31')
         AS RegistrationDate,
        CASE
          WHEN AC_BNM_TYPE <> 'G' OR ST_AWARD_ISSUE IS NOT NULL
            THEN  NVL(TO_CHAR(st_award_issue,'YYYY-MM-DD'),'2999-12-31')
          WHEN ST_AWARD_ISSUE IS NULL AND ST_BNM_GRADE IS NULL
            THEN '2999-12-31'
          WHEN ST_AWARD_ISSUE IS NULL AND ST_BNM_GRADE IS NOT NULL 
               AND (TO_DATE(:WS-NG-GRADING-DATE,'YYYY-MM-DD') > SYSDATE )
            THEN 
              FN_GET_LATEST_RESULT_DATE(st_reg_no,:ws-acad-start-date)
          ELSE :WS-NG-GRADING-DATE
        END AS FirstEntryDate,
        NULL AS AwardDate,
        DECODE(ac_bnm_type,
               'G',
                DECODE(st_award_issue,NULL,
           DECODE(st_bnm_grade,NULL,'-2',
                DECODE(aw_award_code,'Y6',
                   DECODE(FN_CHECK_TA_GRADE_VISIBLE_EOL(ST_REG_NO),
                               'Y',
                               st_bnm_grade,
                               '-2'
                              ),
             st_bnm_grade  
             )
            ), 
                      NVL(NVL(st_over_grade,st_bnm_grade),'P')
                      ),
                DECODE(st_award_issue,
                       NULL,
                       '-2',
                       NVL(NVL(st_over_grade,st_bnm_grade),'P')
             )
          ) AS QualificationGrade,
        -2 AS PrivateCandidate,
        -2 AS PartialAbsence,
        DECODE(ac_bnm_type,'G','G','B') AS btec_ng_type,
        DECODE(st_award_issue,NULL,'N','Y') AS Cert_Ind,
        st_course_id AS Course_id,
        at_number,
        drd_rec_type,
        drd_st_reg_no,
        drd_forenames,
        drd_surname,
        drd_sex,
        drd_birth_date,
        drd_ncn,
        drd_urn,
        drd_centre_id,
        drd_ukprn,
        drd_centre_name,
        NVL(drd_address_1,' '),
        NVL(drd_address_2,' '),
        NVL(drd_address_3,' '),
        NVL(drd_address_4,' '),
        drd_postcode,
        drd_centre_type,
        drd_qual_num_type,
        drd_qual_number,
        drd_spec_code,
        drd_spec_title,
        drd_qual_type,
        drd_reg_date,
        drd_first_entry_date,
        drd_award_date,
        drd_qual_grade
        FROM students,
          dfe_report_details,
             centres,
             legacy_centres,
             edexcel_centres,
             approval_awards,
             approval_application,
             at_award_codes,
             award_titles,
             award_codes
        WHERE st_reg_no                = drd_st_reg_no
          AND st_centre_id             = cn_centre_id
          AND cn_centre_id             = lcen_centre_ref
          AND lcen_ecen_id             = ecen_id
          AND st_course_id             = aw_course_number
          AND aw_applicat_no           = aa_applicat_no
          AND TO_NUMBER(aa_btec_title) = at_number
          AND aw_award_code            = aaco_ac_code
          AND at_number                = aaco_at_number
          AND aw_award_code            = ac_code
          AND aaco_qca_code              IS NOT NULL
          AND st_delete                  IS NULL
          AND st_fallback                IS NULL
          AND st_university_ind          IS NULL
          AND lcen_ltyp_id             = 1
          AND aw_award_code NOT IN 
             ('01','02','03','04','05','06','11',
              '27','64','35','63','93','HH','GG',
              'GA','GB','CC','EW','FS','FO')
          AND ((drd_rec_type IN ('Result','Amendment') AND
                (
           (ac_bnm_type = 'G' AND st_bnm_grade IS NOT NULL AND 
              ( st_bnm_grade <> drd_qual_grade OR
                 (TRUNC(SYSDATE) <= drd_amend_cutoff_date)
               )
           ) OR 
                 (ac_bnm_type <> 'G' AND st_award_issue IS NOT NULL AND
                    (TRUNC(SYSDATE) <= drd_amend_cutoff_date OR 
                      ( NVL(NVL(st_over_grade,st_bnm_grade),'P')
                        <> drd_qual_grade
                      )
                    )
                  )
                 )
              )
              OR       
            (drd_rec_type = 'Entry' AND 
            (UPPER(:WS-ACAD-YEAR-RERUN) = 'NO' OR
               (UPPER(:WS-ACAD-YEAR-RERUN) = 'YES' AND
                NVL(st_award_issue,
                    TO_DATE(:WS-NG-GRADING-DATE,'YYYY-MM-DD'))
                    <= TO_DATE(:WS-END-DATE,'DD-MON-YYYY'))
             ) AND
            (DECODE
          (ac_bnm_type,
               'G',
                DECODE(st_award_issue,NULL,
           DECODE(st_bnm_grade,NULL,'-2',
                DECODE(aw_award_code,'Y6',
                   DECODE(FN_CHECK_TA_GRADE_VISIBLE_EOL(ST_REG_NO),
                               'Y',
                               st_bnm_grade,
                               '-2'
                              ),
             st_bnm_grade  
             )
            ), 
                      NVL(NVL(st_over_grade,st_bnm_grade),'P')
                      ),
                DECODE(st_award_issue,
                       NULL,
                       '-2',
                       NVL(NVL(st_over_grade,st_bnm_grade),'P')
             )
          ) <> drd_qual_grade)
          ) 
              )
        UNION ALL
        SELECT
        '-2' AS UniqueCandidateIdentifier,
        '-2' AS UniqueLearnerNumber,
        '-2' AS UniquePupilNumber,
        st_reg_no AS CandidateIdentifierOther,
        st_forenames AS FirstNameDB,
        NVL(TRIM(TRANSLATE(st_forenames,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS FirstNameRPT,
        st_surname AS SurnameDB,
        NVL(TRIM(TRANSLATE(st_surname,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS SurnameRPT,
        DECODE(st_sex,'M','M','F','F','-2') AS Sex,
        CASE 
          WHEN ST_BIRTH_DATE IS NULL 
            THEN '2999-12-31'
          WHEN ST_BIRTH_DATE <= TO_DATE('1900-01-01','YYYY-MM-DD')
            THEN '2999-12-31'
          ELSE
            TO_CHAR(ST_BIRTH_DATE,'YYYY-MM-DD')
        END AS DOB,
        fn_get_ncn(st_centre_id) AS NCN,
        NVL(ecen_urn,-2) AS CentreURN, 
        '-2' AS LAESTAB,
        st_centre_id AS CentreIdentifierOther,
        decode(LENGTH(ecen_ukprn),8,ecen_ukprn,-2) AS UKPRN,
        cn_centre_name AS CentreName,
        NVL(cn_address_1||DECODE(cn_address_1,NULL,NULL,' ')
        ||cn_address_2,' ') AS CentreAddress1,
        NVL(cn_address_3,' ') AS CentreAddress2,
        NVL(cn_town,' ') AS CentreAddress3,
        NVL(cn_county,' ') AS CentreAddress4,
        NVL(cn_postcode,'-2') AS CentrePostcode,
        DECODE(cn_bure_buni_id,0,'12',
            DECODE(cn_status,0,'9',
                             2,'5',
                             3,'1',
                             4,'11',
                             5,'2',
                             6,'3',
                             7,'1',
                             8,'10',
                             9,'12',
                             10,'6',
                             11,'2',
                             12,'13',
                             13,'5',
                             14,'13','13')) AS CentreType,
        DECODE(st_award_issue,NULL,
               DECODE(aw_award_code,
                      'FO','50098275',
                      'FS','50085001'
                     ),
               fn_get_qca_code(aw_award_code,at_number,
             NVL(st_bnm_grade,st_abs_level_achieved))
           ) AS QualificationNumber,
        AA_BTEC_TITLE AS SpecificationCode,
        at_name AS SpecificationTitle,
        FN_GET_DFE_QUAL_TYPE(DECODE(st_award_issue,NULL,
              DECODE(aw_award_code,
                     'FO','50098275',
                     'FS','50085001'
                    ),
              fn_get_qca_code(aw_award_code,at_number,
               NVL(st_bnm_grade,st_abs_level_achieved))
         )) AS QualificationType,
        '-2' AS ExamSeries,
        NVL(TO_CHAR(st_reg_date,'YYYY-MM-DD'),'2999-12-31')
         AS RegistrationDate,
        NVL(TO_CHAR(st_award_issue,'YYYY-MM-DD'),'2999-12-31')
         AS FirstEntryDate,
        NULL AS AwardDate,
        DECODE(st_award_issue,NULL,'-2',
           DECODE(NVL(st_bnm_grade,st_over_grade),
               NULL,
               '-2',
                    DECODE(aw_award_code,
                           'FO','P',
                           'E'||NVL(st_over_grade,     
                              REPLACE(NVL(st_abs_level_achieved,
                                          st_bnm_grade),'E',NULL))
                           ) 
                             
                 )
             ) AS QualificationGrade,
        -2 AS PrivateCandidate,
        -2 AS PartialAbsence,
        'O' AS btec_ng_type,
        DECODE(st_award_issue,NULL,'N','Y') AS Cert_Ind,
        st_course_id AS Course_id,
        at_number,
        NULL AS drd_rec_type,
        NULL AS drd_st_reg_no,
        NULL AS drd_forenames,
        NULL AS drd_surname,
        NULL AS drd_sex,
        NULL AS drd_birth_date,
        NULL AS drd_ncn,
        NULL AS drd_urn,
        NULL AS drd_centre_id,
        NULL AS drd_ukprn,
        NULL AS drd_centre_name,
        NULL AS drd_address_1,
        NULL AS drd_address_2,
        NULL AS drd_address_3,
        NULL AS drd_address_4,
        NULL AS drd_postcode,
        NULL AS drd_centre_type,
        NULL AS drd_qual_num_type,
        NULL AS drd_qual_number,
        NULL AS drd_spec_code,
        NULL AS drd_spec_title,
        NULL AS drd_qual_type,
        NULL AS drd_reg_date,
        NULL AS drd_first_entry_date,
        NULL AS drd_award_date,
        NULL AS drd_qual_grade
        FROM students,
             centres,
             legacy_centres,
             edexcel_centres,
             approval_awards,
             approval_application,
             award_titles
        WHERE st_centre_id             = cn_centre_id
          AND cn_centre_id             = lcen_centre_ref
          AND lcen_ecen_id             = ecen_id
          AND st_course_id             = aw_course_number
          AND aw_applicat_no       = aa_applicat_no
          AND TO_NUMBER(aa_btec_title) = at_number
          AND lcen_ltyp_id             = 1
          AND aw_award_code IN ('FS','FO')
          AND st_delete                IS NULL
          AND st_fallback              IS NULL
          AND (
                (st_award_issue 
                 BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-YYYY') 
                     AND TO_DATE(:WS-END-DATE,'DD-MON-YYYY')
                )
                OR
                (st_reg_date 
                  BETWEEN TO_DATE(:WS-REG-START-DATE,'DD-MON-YYYY') 
                      AND TO_DATE(:WS-REG-END-DATE,'DD-MON-YYYY')
                  AND st_award_issue IS NULL
                )
              )
           AND NOT EXISTS 
                (SELECT 1
                   FROM dfe_report_details
                  WHERE drd_st_reg_no = st_reg_no
                )
        UNION ALL
        SELECT
        '-2' AS UniqueCandidateIdentifier,
        '-2' AS UniqueLearnerNumber,
        '-2' AS UniquePupilNumber,
        st_reg_no AS CandidateIdentifierOther,
        st_forenames AS FirstNameDB,
        NVL(TRIM(TRANSLATE(st_forenames,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS FirstNameRPT,
        st_surname AS SurnameDB,
        NVL(TRIM(TRANSLATE(st_surname,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS SurnameRPT,
        DECODE(st_sex,'M','M','F','F','-2') AS Sex,
        CASE 
          WHEN ST_BIRTH_DATE IS NULL 
            THEN '2999-12-31'
          WHEN ST_BIRTH_DATE <= TO_DATE('1900-01-01','YYYY-MM-DD')
            THEN '2999-12-31'
          ELSE
            TO_CHAR(ST_BIRTH_DATE,'YYYY-MM-DD')
        END AS DOB,
        fn_get_ncn(st_centre_id) AS NCN,
        NVL(ecen_urn,-2) AS CentreURN, 
        '-2' AS LAESTAB,
        st_centre_id AS CentreIdentifierOther,
        decode(LENGTH(ecen_ukprn),8,ecen_ukprn,-2) AS UKPRN,
        cn_centre_name AS CentreName,
        NVL(cn_address_1||
        decode(cn_address_1,null,null,' ')
        ||cn_address_2,' ') AS CentreAddress1,
        NVL(cn_address_3,' ') AS CentreAddress2,
        NVL(cn_town,' ') AS CentreAddress3,
        NVL(cn_county,' ') AS CentreAddress4,
        NVL(cn_postcode,'-2') AS CentrePostcode,
        DECODE(cn_bure_buni_id,0,'12',
            DECODE(cn_status,0,'9',
                             2,'5',
                             3,'1',
                             4,'11',
                             5,'2',
                             6,'3',
                             7,'1',
                             8,'10',
                             9,'12',
                             10,'6',
                             11,'2',
                             12,'13',
                             13,'5',
                             14,'13','13')) AS CentreType,
        DECODE(st_award_issue,NULL,
               DECODE(aw_award_code,
                      'FO','50098275',
                      'FS','50085001'
                     ),
               fn_get_qca_code(aw_award_code,at_number,
                NVL(st_bnm_grade,st_abs_level_achieved))
              ) AS QualificationNumber,
        AA_BTEC_TITLE AS SpecificationCode,
        at_name AS SpecificationTitle,
        FN_GET_DFE_QUAL_TYPE(DECODE(st_award_issue,NULL,
               DECODE(aw_award_code,
                      'FO','50098275',
                      'FS','50085001'
                     ),
               fn_get_qca_code(aw_award_code,at_number,
                NVL(st_bnm_grade,st_abs_level_achieved))
         )) AS QualificationType,
        '-2' AS ExamSeries,
        NVL(TO_CHAR(st_reg_date,'YYYY-MM-DD'),'2999-12-31')
         AS RegistrationDate,
        NVL(TO_CHAR(st_award_issue,'YYYY-MM-DD'),'2999-12-31')
         AS FirstEntryDate,
        NULL AS AwardDate,
        DECODE(st_award_issue,NULL,'-2',
           DECODE(NVL(st_bnm_grade,st_over_grade),
               NULL,
               '-2',
                    DECODE(aw_award_code,
                           'FO','P',
                           'E'||NVL(st_over_grade,     
                                     REPLACE(NVL(st_abs_level_achieved,
                                                 st_bnm_grade),'E',NULL))
                           ) 
                             
                 )
             ) AS QualificationGrade,
        -2 AS PrivateCandidate,
        -2 AS PartialAbsence,
        'O' AS btec_ng_type,
        DECODE(st_award_issue,NULL,'N','Y') AS Cert_Ind,
        st_course_id AS Course_id,
        at_number,
        drd_rec_type,
        drd_st_reg_no,
        drd_forenames,
        drd_surname,
        drd_sex,
        drd_birth_date,
        drd_ncn,
        drd_urn,
        drd_centre_id,
        drd_ukprn,
        drd_centre_name,
        NVL(drd_address_1,' '),
        NVL(drd_address_2,' '),
        NVL(drd_address_3,' '),
        NVL(drd_address_4,' '),
        drd_postcode,
        drd_centre_type,
        drd_qual_num_type,
        drd_qual_number,
        drd_spec_code,
        drd_spec_title,
        drd_qual_type,
        drd_reg_date,
        drd_first_entry_date,
        drd_award_date,
        drd_qual_grade
        FROM students,
          dfe_report_details,
             centres,
             legacy_centres,
             edexcel_centres,
             approval_awards,
             approval_application,
             award_titles
        WHERE st_reg_no       = drd_st_reg_no
          AND st_centre_id             = cn_centre_id
          AND cn_centre_id             = lcen_centre_ref
          AND lcen_ecen_id             = ecen_id
          AND st_course_id             = aw_course_number
          AND aw_applicat_no       = aa_applicat_no
          AND TO_NUMBER(aa_btec_title) = at_number
          AND lcen_ltyp_id             = 1
          AND aw_award_code IN ('FS','FO')   
          AND st_delete                IS NULL                                               
          AND st_fallback              IS NULL
          AND ((drd_rec_type IN ('Result','Amendment') AND 
                st_award_issue IS NOT NULL AND
                 (TRUNC(SYSDATE) <= drd_amend_cutoff_date OR 
                   (DECODE(NVL(st_bnm_grade,st_over_grade),
                         NULL,'-2',
                           DECODE(aw_award_code,
                                  'FO','P',
                                  'E'||NVL(st_over_grade,     
                                     REPLACE(NVL(st_abs_level_achieved,
                                             st_bnm_grade),'E',NULL))
                                 )                     
                              )
                         <> drd_qual_grade 
                   )
                 )
               )
              OR       
            (drd_rec_type = 'Entry' AND 
            (UPPER(:WS-ACAD-YEAR-RERUN) = 'NO' OR
               (UPPER(:WS-ACAD-YEAR-RERUN) = 'YES' AND
                NVL(st_award_issue,TO_DATE('2999-12-31','YYYY-MM-DD'))
                    <= TO_DATE(:WS-END-DATE,'DD-MON-YYYY'))
            ) AND
            (DECODE(st_award_issue,NULL,'-2',
              DECODE(NVL(st_bnm_grade,st_over_grade),
                  NULL,'-2',
                    DECODE(aw_award_code,
                           'FO','P',
                           'E'||NVL(st_over_grade,     
                              REPLACE(NVL(st_abs_level_achieved,
                                      st_bnm_grade),'E',NULL))
                          )                     
                       )
                      ) <> drd_qual_grade
             )
            )   
        )
*ORDER BY at_number, Student reg no
*        ORDER BY 35, 4
*EC5877 - Update to the Order By Column (at_number) position
         ORDER BY 37, 4
        END-EXEC.

        MOVE   "C: ERROR OPENING BTEC COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        DISPLAY "BTEC Process Started...".
        EXEC SQL
            OPEN GET_BTEC
        END-EXEC.
*
        PERFORM CA-FETCH-BTEC
          UNTIL WS01-EOF.
*
        DISPLAY "TOTAL BTEC FETCHED ", WS01-ROWS-TOTAL WITH CONVERSION.

        MOVE   "C: ERROR CLOSING BTEC COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        EXEC SQL
            CLOSE GET_BTEC
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
             FETCH GET_BTEC
              INTO :DS01-UNIQUE-CANDIDATE-IDR,
                   :DS01-UNIQUE-LEARNER-NUM,
                   :DS01-UNIQUE-PUPIL-NUM,
                   :DS01-CANDIDATE-IDR-OTHER,
                   :DS01-FIRSTNAME-DB,
                   :DS01-FIRSTNAME-RPT,
                   :DS01-SURNAME-DB,
                   :DS01-SURNAME-RPT,
                   :DS01-GENDER,
                   :DS01-DOB,
                   :DS01-NCN,
                   :DS01-CENTREURN,
                   :DS01-LEA,
                   :DS01-CENTRE-IDR-OTHER,
                   :DS01-UKPRN,
                   :DS01-CENTRENAME,
                   :DS01-CENTREADDRESS1,
                   :DS01-CENTREADDRESS2,
                   :DS01-CENTREADDRESS3,
                   :DS01-CENTREADDRESS4,
                   :DS01-CENTREPOSTCODE,
                   :DS01-CENTRETYPE,
                   :DS01-QUAL-NUMBER,
                   :DS01-SPEC-CODE,
                   :DS01-SPEC-TITLE,
                   :DS01-QUAL-TYPE,
                   :DS01-EXAMSERIES,
                   :DS01-REG-DATE,
                   :DS01-FIRST-ENTRY-DATE,
                   :DS01-AWARD-DATE,
                   :DS01-QUAL-GRADE,
                   :DS01-PRIVATE-CANDIDATE,
                   :DS01-PARTIAL-ABSENCE,
                   :DS01-BNM-TYPE,
                   :DS01-CERT-IND,
                   :DS01-COURSE,
                   :DS01-AT-NUMBER,
                   :DS02-REC-TYPE,
                   :DS02-CANDIDATE-IDR-OTHER,
                   :DS02-FIRSTNAME-DB,
                   :DS02-SURNAME-DB,
                   :DS02-GENDER,
                   :DS02-DOB,
                   :DS02-NCN,
                   :DS02-CENTREURN,
                   :DS02-CENTRE-IDR-OTHER,
                   :DS02-UKPRN,
                   :DS02-CENTRENAME,
                   :DS02-CENTREADDRESS1,
                   :DS02-CENTREADDRESS2,
                   :DS02-CENTREADDRESS3,
                   :DS02-CENTREADDRESS4,
                   :DS02-CENTREPOSTCODE,
                   :DS02-CENTRETYPE,
                   :DS02-QUAL-NUMBER-TYPE,
                   :DS02-QUAL-NUMBER,
                   :DS02-SPEC-CODE,
                   :DS02-SPEC-TITLE,
                   :DS02-QUAL-TYPE,
                   :DS02-REG-DATE,
                   :DS02-FIRST-ENTRY-DATE,
                   :DS02-AWARD-DATE,
                   :DS02-QUAL-GRADE
        END-EXEC.
*
CA-010.
        IF  SQLCODE IS POSITIVE THEN
            SET     WS01-EOF       TO  TRUE
        END-IF.

        SUBTRACT WS01-ROWS-TOTAL FROM  SQLERRD(3)
                               GIVING  WS01-ROWS-THIS-FETCH.
        MOVE    SQLERRD(3)         TO  WS01-ROWS-TOTAL.

        MOVE    WS01-ROWS-TOTAL    TO  WS-BTEC-ROWS-TOTAL
        DISPLAY 'BTEC TOTAL ROWS FETCHED : ' WS-BTEC-ROWS-TOTAL.

        PERFORM  VARYING WS01-INDEX
          FROM 1 BY 1
          UNTIL WS01-INDEX > WS01-ROWS-THIS-FETCH

            MOVE 'N'                       TO WS-SKIP
*ST_REG_NO
            MOVE DS01-CANDIDATE-IDR-OTHER(WS01-INDEX) 
              TO WS-C-CANDIDATE-IDR-OTHER WS-CANDIDATE 

            MOVE DS01-CERT-IND(WS01-INDEX)  TO  WS-CERTIFICATE-IND
            MOVE DS01-BNM-TYPE(WS01-INDEX)  TO  WS-BNM-TYPE

            MOVE 'N' TO WS-BLOCK-IND

*    Only check certificate block for non-certificated BTEC NG learners...

            IF  BTEC-NG AND NOT CERTIFICATED AND 
                DS01-QUAL-GRADE(WS01-INDEX) NOT = '-2' 
            THEN
                ADD 1 TO WS-CHECK-BLOCKING-CNT
                PERFORM L-CHECK-BLOCKING
            END-IF

            IF  NOT  BLOCKED
       
                PERFORM  E-MOVE-VALIDATE-DB-COLS

              IF  SKIP
                  ADD  +1   TO  WS-BTEC-IGN-CNT
              ELSE
*LEARNER'S PREVIOUS DRD RECORD DETAILS THRU E-CONTINUE.

                IF  WS-P-CANDIDATE-IDR-OTHER = WS-C-CANDIDATE-IDR-OTHER
                    IF  WS-C-QUAL-GRADE = '-2'
                        ADD  +1  TO  WS-BTEC-SKIPPED-CNT
                    ELSE
                      IF  BTEC-NG  AND NOT CERTIFICATED AND 
                          WS-C-AWARD-DATE = '2998-12-31'
                          PERFORM Q-WRITE-IGNORED-LNR
                      ELSE
                        PERFORM M-CANDIDATE-REC-COMPARE
                      END-IF
                    END-IF
                ELSE
*INSERT LEARNERS INTO DFE_REPORT_DETAILS
                  IF  WS-C-QUAL-GRADE = '-2'
                      MOVE 'Entry'      TO WS-REC-TYPE
                      MOVE '2999-12-31' TO WS-C-FIRST-ENTRY-DATE 
                                           WS-C-AWARD-DATE
                      MOVE 'I'          TO WS-DFE-INS-UPDAT
                      MOVE 'B'          TO WS-C-PROG-TYPE
                      PERFORM N-DRD-INSERT-UPDAT
                  ELSE
                    IF  BTEC-NG  AND NOT CERTIFICATED
                        IF  WS-C-AWARD-DATE = '2998-12-31'
                            PERFORM Q-WRITE-IGNORED-LNR
                        ELSE
                          IF  WS-ACAD-YEAR-RERUN = 'YES' 
                              EXEC SQL
                                SELECT 
                                  CASE
                                    WHEN TO_DATE(:WS-C-AWARD-DATE,'YYYY-MM-DD')
                                       > TO_DATE(:WS-END-DATE,'DD-MON-YYYY')
                                      THEN 'Y'
                                    ELSE 'N'
                                  END
                                INTO   :WS-AWARD-DATE-FUTURE
                                FROM DUAL
                              END-EXEC
                              IF  AWARD-DATE-FUTURE
                                  MOVE 'Entry'      TO WS-REC-TYPE
                                  MOVE '-2'         TO WS-C-QUAL-GRADE
                                  MOVE '2999-12-31' TO WS-C-FIRST-ENTRY-DATE 
                                                       WS-C-AWARD-DATE
                              ELSE 
                                  MOVE 'Result'     TO WS-REC-TYPE
                              END-IF
                          ELSE
                              MOVE 'Result'         TO WS-REC-TYPE
                          END-IF 
                          MOVE  'I'                 TO WS-DFE-INS-UPDAT
                          MOVE  'B'                 TO WS-C-PROG-TYPE
                          PERFORM N-DRD-INSERT-UPDAT
                        END-IF
                    ELSE
                      MOVE  'Result'                TO WS-REC-TYPE
                      MOVE  'I'                     TO WS-DFE-INS-UPDAT
                      MOVE  'B'                     TO WS-C-PROG-TYPE
                      PERFORM N-DRD-INSERT-UPDAT
                    END-IF
                  END-IF
                END-IF 
*IF SKIP --> END-IF
              END-IF 
*IF NOT BLOCKED --> END-IF
            END-IF
        END-PERFORM.

        MOVE  WS-BTEC-CNT  TO  WS-TR-DET-BTEC-TOTAL.
*
CA-EXIT.                              
    EXIT.    
*                     
D-PROCESS-NVQ SECTION.
**********************************************************************
*
*       Read the required NVQ courses and registrations. 
*
**********************************************************************
D-START.
*
        EXEC SQL
            DECLARE GET_NVQ CURSOR FOR
        SELECT 
        '-2' AS UniqueCandidateIdentifier,
        '-2' AS UniqueLearnerNumber,
        '-2' AS UniquePupilNumber,
        st.st_reg_no AS CandidateIdentifierOther,
        st.st_forenames AS FirstNameDB,
        NVL(TRIM(TRANSLATE(st.st_forenames,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS FirstNameRPT,
        st.st_surname AS SurnameDB,
        NVL(TRIM(TRANSLATE(st.st_surname,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS SurnameRPT,
        DECODE(st.st_sex,'M','M','F','F','-2') AS Sex,
        CASE 
          WHEN ST_BIRTH_DATE IS NULL 
            THEN '2999-12-31'
          WHEN ST_BIRTH_DATE <= TO_DATE('1900-01-01','YYYY-MM-DD')
            THEN '2999-12-31'
          ELSE
            TO_CHAR(ST_BIRTH_DATE,'YYYY-MM-DD')
        END AS DOB,
        fn_get_ncn(st.st_centre_id) AS NCN,
        NVL(ecen_urn,-2) AS CentreURN, 
        '-2' AS LAESTAB,
        st.st_centre_id AS CentreIdentifierOther, 
        decode(LENGTH(ecen_ukprn),8,ecen_ukprn,-2) AS UKPRN,
        cen.cn_centre_name AS CentreName,
        NVL(cen.cn_address_1||
        decode(cen.cn_address_1,null,null,' ')
        ||cen.cn_address_2,' ') AS CentreAddress1,
        NVL(cen.cn_address_3,' ') AS CentreAddress2,
        NVL(cen.cn_town,' ') AS CentreAddress3,
        NVL(cen.cn_county,' ') AS CentreAddress4,
        NVL(cen.cn_postcode,'-2') AS CentrePostcode,
        DECODE(CN_BURE_BUNI_ID,0,'12',
            DECODE(cn_status,0,'9',
                             2,'5',
                             3,'1',
                             4,'11',
                             5,'2',
                             6,'3',
                             7,'1',
                             8,'10',
                             9,'12',
                             10,'6',
                             11,'2',
                             12,'13',
                             13,'5',
                             14,'13','13')) AS CentreType, 
        NVL(nvq.nvq_qan_code,st.st_nvq_registered_id) 
          AS QualificationNumber,
        st.st_nvq_registered_id AS SpecificationCode,  
        nvq.nvq_title_1||' '||nvq.nvq_title_2 
          AS SpecificationTitle,
        FN_GET_DFE_QUAL_TYPE(NVL(nvq.nvq_qan_code
          ,st.st_nvq_registered_id)) AS QualificationType,
        '-2' AS ExamSeries,
        NVL(TO_CHAR(st_reg_date,'YYYY-MM-DD')
        ,'2999-12-31') AS RegistrationDate,
        NVL(TO_CHAR(st_nvq_certificate_print_date
        ,'YYYY-MM-DD'),'2999-12-31') AS FirstEntryDate,
        NULL AS AwardDate,
        DECODE(st_nvq_certificate_print_date,
          NULL,'-2',NVL(st_over_grade,'P')) AS QualificationGrade,
        -2 AS PrivateCandidate, 
        -2 AS PartialAbsence,
        NULL AS drd_rec_type,
        NULL AS drd_st_reg_no,
        NULL AS drd_forenames,
        NULL AS drd_surname,
        NULL AS drd_sex,
        NULL AS drd_birth_date,
        NULL AS drd_ncn,
        NULL AS drd_urn,
        NULL AS drd_centre_id,
        NULL AS drd_ukprn,
        NULL AS drd_centre_name,
        NULL AS drd_address_1,
        NULL AS drd_address_2,
        NULL AS drd_address_3,
        NULL AS drd_address_4,
        NULL AS drd_postcode,
        NULL AS drd_centre_type,
        NULL AS drd_qual_num_type,
        NULL AS drd_qual_number,
        NULL AS drd_spec_code,
        NULL AS drd_spec_title,
        NULL AS drd_qual_type,
        NULL AS drd_reg_date,
        NULL AS drd_first_entry_date,
        NULL AS drd_award_date,
        NULL AS drd_qual_grade
        FROM students st,
             nvqs nvq,
             centres cen,
             legacy_centres,
             edexcel_centres
        WHERE  st_nvq_registered_id    = nvq_id
          AND  st_centre_id            = cn_centre_id
          AND  cn_centre_id            = lcen_centre_ref
          AND  lcen_ltyp_id            = 1
          AND  lcen_ecen_id            = ecen_id
          AND  st_delete               IS NULL
          AND  st_fallback             IS NULL
          AND (
              (st_nvq_certificate_print_date
              BETWEEN TO_DATE(:WS-START-DATE,'DD-MON-YYYY') 
                  AND TO_DATE(:WS-END-DATE,'DD-MON-YYYY')
              )
           OR 
              (st_reg_date
              BETWEEN TO_DATE(:WS-REG-START-DATE,'DD-MON-YYYY') 
                  AND TO_DATE(:WS-REG-END-DATE,'DD-MON-YYYY') 
                  AND st_nvq_certificate_print_date IS NULL 
              )
              )
          AND NOT EXISTS 
               (SELECT 1
                 FROM dfe_report_details
                WHERE drd_st_reg_no = st_reg_no
              )
        UNION ALL
        SELECT  
        '-2' AS UniqueCandidateIdentifier,
        '-2' AS UniqueLearnerNumber,
        '-2' AS UniquePupilNumber,
        st.st_reg_no AS CandidateIdentifierOther,
        st.st_forenames AS FirstNameDB,
        NVL(TRIM(TRANSLATE(st.st_forenames,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS FirstNameRPT,
        st.st_surname AS SurnameDB,
        NVL(TRIM(TRANSLATE(st.st_surname,:WS-RESTRICT-SPL-CHARS,' ')
                 ),' ') AS SurnameRPT,
        DECODE(st.st_sex,'M','M','F','F','-2') AS Sex,
        CASE 
          WHEN ST_BIRTH_DATE IS NULL 
            THEN '2999-12-31'
          WHEN ST_BIRTH_DATE <= TO_DATE('1900-01-01','YYYY-MM-DD')
            THEN '2999-12-31'
          ELSE
            TO_CHAR(ST_BIRTH_DATE,'YYYY-MM-DD')
        END AS DOB,
        fn_get_ncn(st.st_centre_id) AS NCN,
        NVL(ecen_urn,-2) AS CentreURN, 
        '-2' AS LAESTAB,
        st.st_centre_id AS CentreIdentifierOther,
        decode(LENGTH(ecen_ukprn),8,ecen_ukprn,-2) AS UKPRN,
        cen.cn_centre_name AS CentreName,
        NVL(cen.cn_address_1||
        decode(cen.cn_address_1,null,null,' ')
        ||cen.cn_address_2,' ') AS CentreAddress1,
        NVL(cen.cn_address_3,' ') AS CentreAddress2,
        NVL(cen.cn_town,' ') AS CentreAddress3,
        NVL(cen.cn_county,' ') AS CentreAddress4,
        NVL(cen.cn_postcode,'-2') AS CentrePostcode,
        DECODE(CN_BURE_BUNI_ID,0,'12',
            DECODE(cn_status,0,'9',
                             2,'5',
                             3,'1',
                             4,'11',
                             5,'2',
                             6,'3',
                             7,'1',
                             8,'10',
                             9,'12',
                             10,'6',
                             11,'2',
                             12,'13',
                             13,'5',
                             14,'13','13')) AS CentreType, 
        NVL(nvq.nvq_qan_code,st.st_nvq_registered_id) 
          AS QualificationNumber,
        st.st_nvq_registered_id AS SpecificationCode,  
        nvq.nvq_title_1||' '||nvq.nvq_title_2 
          AS SpecificationTitle,
        FN_GET_DFE_QUAL_TYPE(NVL(nvq.nvq_qan_code
          ,st.st_nvq_registered_id)) AS QualificationType,
        '-2' AS ExamSeries,
        NVL(TO_CHAR(st_reg_date,'YYYY-MM-DD'),'2999-12-31')
         AS RegistrationDate,
        NVL(TO_CHAR(st_nvq_certificate_print_date,'YYYY-MM-DD')
          ,'2999-12-31') AS FirstEntryDate,
        NULL AS AwardDate,
        DECODE(st_nvq_certificate_print_date,
          NULL,'-2',NVL(st_over_grade,'P')) AS QualificationGrade,
        -2 AS PrivateCandidate, 
        -2 AS PartialAbsence,
        drd_rec_type,
        drd_st_reg_no,
        drd_forenames,
        drd_surname,
        drd_sex,
        drd_birth_date,
        drd_ncn,
        drd_urn,
        drd_centre_id,
        drd_ukprn,
        drd_centre_name,
        NVL(drd_address_1,' '),
        NVL(drd_address_2,' '),
        NVL(drd_address_3,' '),
        NVL(drd_address_4,' '),
        drd_postcode,
        drd_centre_type,
        drd_qual_num_type,
        drd_qual_number,
        drd_spec_code,
        drd_spec_title,
        drd_qual_type,
        drd_reg_date,
        drd_first_entry_date,
        drd_award_date,
        drd_qual_grade
        FROM students st,
             nvqs nvq,
             centres cen,
             legacy_centres,
             edexcel_centres,
             dfe_report_details
        WHERE  st_nvq_registered_id    = nvq_id
          AND  st_reg_no             = drd_st_reg_no
          AND  st_centre_id            = cn_centre_id
          AND  cn_centre_id            = lcen_centre_ref
          AND  lcen_ltyp_id            = 1
          AND  lcen_ecen_id            = ecen_id
          AND  st_delete               IS NULL
          AND  st_fallback             IS NULL
          AND ((drd_rec_type IN ('Result','Amendment') AND
               st_nvq_certificate_print_date IS NOT NULL AND
                ( TRUNC(SYSDATE) <= drd_amend_cutoff_date OR 
                  NVL(st_over_grade,'P') <> drd_qual_grade 
                 )
               )
              OR       
              (drd_rec_type = 'Entry' AND 
              (UPPER(:WS-ACAD-YEAR-RERUN) = 'NO' OR
                (UPPER(:WS-ACAD-YEAR-RERUN) = 'YES' AND
                 NVL(st_nvq_certificate_print_date,
                    TO_DATE('2999-12-31','YYYY-MM-DD'))
                    <= TO_DATE(:WS-END-DATE,'DD-MON-YYYY'))
               ) AND
               (DECODE(st_nvq_certificate_print_date,
                NULL,'-2',NVL(st_over_grade,'P')) <> drd_qual_grade)
              )
          )
*ORDER BY ST_REG_NO
        ORDER BY 4
        END-EXEC.
*
        MOVE   "D: ERROR OPENING NVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        EXEC SQL
             SELECT TO_CHAR (SYSDATE, 'DD-MON-YYYY:HH24:MI')
               INTO   :WS-TIME-DATE
               FROM   DUAL
        END-EXEC.

        DISPLAY "NVQ Process Started at " WS-TIME-DATE
        EXEC SQL
            OPEN GET_NVQ
        END-EXEC.
*
        PERFORM DA-FETCH-NVQ
          UNTIL WS01-EOF.
*
        DISPLAY "TOTAL NVQ FETCHED ", WS01-ROWS-TOTAL WITH CONVERSION.

        MOVE   "D: ERROR CLOSING NVQ COURSES CURSOR"
                                   TO  WS01-ERR-MESSAGE. 
*    
        EXEC SQL
            CLOSE   GET_NVQ
        END-EXEC.
*
D-EXIT.
*
        EXIT.
*
DA-FETCH-NVQ SECTION.
**********************************************************************
*
*       Get a set of NVQ awards and process them.
*
**********************************************************************
DA-START.
*
       MOVE   "DA: ERROR FETCHING NVQ AWARDS"
                                    TO  WS01-ERR-MESSAGE.
*
       EXEC SQL
             FETCH   GET_NVQ
             INTO   :DS01-UNIQUE-CANDIDATE-IDR,
                    :DS01-UNIQUE-LEARNER-NUM, 
                    :DS01-UNIQUE-PUPIL-NUM,
                    :DS01-CANDIDATE-IDR-OTHER,
                    :DS01-FIRSTNAME-DB,
                    :DS01-FIRSTNAME-RPT,
                    :DS01-SURNAME-DB,
                    :DS01-SURNAME-RPT,
                    :DS01-GENDER,
                    :DS01-DOB,
                    :DS01-NCN,
                    :DS01-CENTREURN,
                    :DS01-LEA,
                    :DS01-CENTRE-IDR-OTHER,
                    :DS01-UKPRN,
                    :DS01-CENTRENAME,
                    :DS01-CENTREADDRESS1,
                    :DS01-CENTREADDRESS2,
                    :DS01-CENTREADDRESS3,
                    :DS01-CENTREADDRESS4,
                    :DS01-CENTREPOSTCODE,
                    :DS01-CENTRETYPE,
                    :DS01-QUAL-NUMBER,
                    :DS01-SPEC-CODE,
                    :DS01-SPEC-TITLE,
                    :DS01-QUAL-TYPE,
                    :DS01-EXAMSERIES,
                    :DS01-REG-DATE,
                    :DS01-FIRST-ENTRY-DATE,
                    :DS01-AWARD-DATE,
                    :DS01-QUAL-GRADE,
                    :DS01-PRIVATE-CANDIDATE,
                    :DS01-PARTIAL-ABSENCE,
                    :DS02-REC-TYPE,
                    :DS02-CANDIDATE-IDR-OTHER,
                    :DS02-FIRSTNAME-DB,
                    :DS02-SURNAME-DB,
                    :DS02-GENDER,
                    :DS02-DOB,
                    :DS02-NCN,
                    :DS02-CENTREURN,
                    :DS02-CENTRE-IDR-OTHER,
                    :DS02-UKPRN,
                    :DS02-CENTRENAME,
                    :DS02-CENTREADDRESS1,
                    :DS02-CENTREADDRESS2,
                    :DS02-CENTREADDRESS3,
                    :DS02-CENTREADDRESS4,
                    :DS02-CENTREPOSTCODE,
                    :DS02-CENTRETYPE,
                    :DS02-QUAL-NUMBER-TYPE,
                    :DS02-QUAL-NUMBER,
                    :DS02-SPEC-CODE,
                    :DS02-SPEC-TITLE,
                    :DS02-QUAL-TYPE,
                    :DS02-REG-DATE,
                    :DS02-FIRST-ENTRY-DATE,
                    :DS02-AWARD-DATE,
                    :DS02-QUAL-GRADE
       END-EXEC. 
*
DA-010.
        IF  SQLCODE IS POSITIVE THEN
            SET     WS01-EOF       TO  TRUE
        END-IF.

        SUBTRACT WS01-ROWS-TOTAL FROM  SQLERRD(3)
                               GIVING  WS01-ROWS-THIS-FETCH.
        MOVE    SQLERRD(3)         TO  WS01-ROWS-TOTAL.
    
        MOVE    WS01-ROWS-TOTAL    TO  WS-NVQ-ROWS-TOTAL
        DISPLAY 'NVQ TOTAL ROWS FETCHED : ' WS-NVQ-ROWS-TOTAL
    
        PERFORM VARYING WS01-INDEX
          FROM 1 BY 1
          UNTIL WS01-INDEX > WS01-ROWS-THIS-FETCH

          MOVE 'N'                 TO  WS-SKIP 

          MOVE DS01-CANDIDATE-IDR-OTHER(WS01-INDEX)
            TO WS-C-CANDIDATE-IDR-OTHER  WS-CANDIDATE

          PERFORM  E-MOVE-VALIDATE-DB-COLS

          IF  SKIP
              ADD  +1   TO  WS-NVQ-IGN-CNT
          ELSE
*LEARNER'S PREVIOUS DRD RECORD DETAILS THRU E-CONTINUE.

            IF  WS-P-CANDIDATE-IDR-OTHER = WS-C-CANDIDATE-IDR-OTHER
                IF  WS-C-QUAL-GRADE = '-2'
                    ADD  +1  TO  WS-NVQ-SKIPPED-CNT
                ELSE 
                    PERFORM M-CANDIDATE-REC-COMPARE
                END-IF
            ELSE
*INSERT LEARNERS INTO DFE_REPORT_DETAILS
              IF  WS-C-QUAL-GRADE = '-2'
                  MOVE 'Entry'      TO WS-REC-TYPE
              ELSE 
                  MOVE 'Result'     TO WS-REC-TYPE
              END-IF
              MOVE 'I'              TO WS-DFE-INS-UPDAT
              MOVE 'N'              TO WS-C-PROG-TYPE
              PERFORM N-DRD-INSERT-UPDAT
            END-IF
*IF SKIP -> END-IF
          END-IF
        END-PERFORM.

        MOVE  WS-NVQ-CNT  TO  WS-TR-DET-NVQ-TOTAL.
*                                   
DA-EXIT.                              
    EXIT.    
*
E-MOVE-VALIDATE-DB-COLS SECTION.
**********************************************************************
* Move & Validate the DB columns section common for both BTEC & NVQ
**********************************************************************
E-START.
*
        MOVE DS01-UNIQUE-CANDIDATE-IDR(WS01-INDEX)
          TO WS-C-UNIQUE-CANDIDATE-IDR

        MOVE DS01-UNIQUE-LEARNER-NUM(WS01-INDEX)
          TO WS-C-UNIQUE-LEARNER-NUM

        MOVE DS01-UNIQUE-PUPIL-NUM(WS01-INDEX)
          TO WS-C-UNIQUE-PUPIL-NUM

        MOVE DS01-FIRSTNAME-DB(WS01-INDEX)   
          TO WS-C-FIRSTNAME 
        MOVE DS01-FIRSTNAME-RPT(WS01-INDEX) 
          TO WS-RPT-FIRSTNAME

        MOVE DS01-SURNAME-DB(WS01-INDEX)
          TO WS-C-SURNAME
        MOVE DS01-SURNAME-RPT(WS01-INDEX)
          TO WS-RPT-SURNAME

        IF WS-RPT-FIRSTNAME = SPACES AND WS-RPT-SURNAME = SPACES
           MOVE 'x'              TO WS-RPT-FIRSTNAME WS-RPT-SURNAME
        ELSE 
           IF WS-RPT-FIRSTNAME NOT = SPACES AND WS-RPT-SURNAME = SPACES
              MOVE WS-RPT-FIRSTNAME TO WS-RPT-SURNAME
              MOVE 'x'              TO WS-RPT-FIRSTNAME
           ELSE 
              IF WS-RPT-FIRSTNAME = SPACES AND WS-RPT-SURNAME NOT = SPACES
                 MOVE 'x'           TO WS-RPT-FIRSTNAME
              END-IF
          END-IF 
        END-IF
              
        MOVE DS01-GENDER(WS01-INDEX)  TO  WS-C-GENDER      

        MOVE DS01-DOB(WS01-INDEX)     TO  WS-C-DOB        
  
        IF DS01-NCN(WS01-INDEX) = '00000'
           MOVE '-2'                 TO WS-C-NCN
        ELSE
           MOVE DS01-NCN(WS01-INDEX) TO WS-C-NCN
        END-IF

        MOVE DS01-CENTREURN(WS01-INDEX)
          TO WS-C-CENTREURN WS-RPT-CENTREURN

        MOVE DS01-LEA(WS01-INDEX)  TO  WS-C-LEA
 
        MOVE DS01-CENTRE-IDR-OTHER(WS01-INDEX)
          TO WS-C-CENTRE-IDR-OTHER

        MOVE DS01-UKPRN(WS01-INDEX)
          TO WS-C-UKPRN WS-RPT-UKPRN
*
*Centre Details
        MOVE DS01-CENTRENAME(WS01-INDEX)
          TO WS-C-CENTRENAME

        MOVE DS01-CENTREADDRESS1(WS01-INDEX) 
          TO WS-C-CENTREADDRESS1

        MOVE DS01-CENTREADDRESS2(WS01-INDEX) 
          TO WS-C-CENTREADDRESS2

        MOVE DS01-CENTREADDRESS3(WS01-INDEX)
          TO WS-C-CENTREADDRESS3

        MOVE DS01-CENTREADDRESS4(WS01-INDEX)
          TO WS-C-CENTREADDRESS4
        
        MOVE DS01-CENTRETYPE(WS01-INDEX)
          TO WS-C-CENTRETYPE

        MOVE DS01-CENTREPOSTCODE(WS01-INDEX)
          TO WS-C-CENTREPOSTCODE

*If centre type is foreign post code is -2          
        IF WS-C-CENTRETYPE = '12'
           MOVE  '-2'  TO  WS-RPT-CENTREPOSTCODE
        ELSE 
           MOVE WS-C-CENTREPOSTCODE
             TO WS-RPT-CENTREPOSTCODE
        END-IF

        MOVE DS01-QUAL-NUMBER(WS01-INDEX)
          TO WS-C-QUAL-NUMBER WS-RPT-QUAL-NUMBER

        IF WS-C-QUAL-NUMBER(1:3) = '100' OR '500' OR '501' 
                         OR '600' OR '601' OR '603' OR '610'
           MOVE 'QAN'  TO  WS-C-QUAL-NUMBER-TYPE
           STRING WS-C-QUAL-NUMBER(1:3) DELIMITED BY SIZE
             '/' DELIMITED BY SIZE
             WS-C-QUAL-NUMBER(4:4) DELIMITED BY SIZE   
             '/' DELIMITED BY SIZE
             WS-C-QUAL-NUMBER(8:1) DELIMITED BY SIZE
             INTO WS-RPT-QUAL-NUMBER
        ELSE 
          IF WS-C-QUAL-NUMBER(1:1) = 'R' OR 'G' OR 'F'
             MOVE 'SQN'  TO  WS-C-QUAL-NUMBER-TYPE
          ELSE 
            IF WS-C-QUAL-NUMBER(1:1) = 'C'
               MOVE 'QIW'  TO  WS-C-QUAL-NUMBER-TYPE
               STRING WS-C-QUAL-NUMBER(1:3) DELIMITED BY SIZE
                 '/' DELIMITED BY SIZE
                 WS-C-QUAL-NUMBER(4:4) DELIMITED BY SIZE   
                 '/' DELIMITED BY SIZE
                 WS-C-QUAL-NUMBER(8:1) DELIMITED BY SIZE
                 INTO WS-RPT-QUAL-NUMBER
            ELSE
*BUSINESS TEST QUAL NUMBER - SKIP
               IF  WS-C-QUAL-NUMBER(1:3) = 'EPA' OR 'ITU'
                   MOVE 'Y'     TO  WS-SKIP
                   GO TO E-EXIT
               ELSE 
                   MOVE 'Other' TO  WS-C-QUAL-NUMBER-TYPE
               END-IF
            END-IF
          END-IF
        END-IF

        MOVE DS01-SPEC-CODE(WS01-INDEX)  TO  WS-C-SPEC-CODE

        MOVE DS01-SPEC-TITLE(WS01-INDEX) TO  WS-C-SPEC-TITLE

        MOVE DS01-QUAL-TYPE(WS01-INDEX)
          TO WS-C-QUAL-TYPE WS-RPT-QUAL-TYPE

*Essential Skills Wales - ESW
        IF  WS-C-QUAL-TYPE = 'Essential Skills Wales'
            MOVE 'Y'     TO  WS-SKIP
            GO TO E-EXIT
        END-IF

*OGQ Check only for BTEC
        IF  BTEC-PROCESS
            IF  WS-C-QUAL-TYPE = 'Other General Qualification'
                MOVE 'Other Vocational Qualification'          
                  TO WS-RPT-QUAL-TYPE
            END-IF
        END-IF

        MOVE DS01-EXAMSERIES(WS01-INDEX)  TO  WS-C-EXAMSERIES

        MOVE DS01-REG-DATE(WS01-INDEX)    TO  WS-C-REG-DATE

        MOVE DS01-FIRST-ENTRY-DATE(WS01-INDEX) 
          TO WS-C-FIRST-ENTRY-DATE WS-C-AWARD-DATE 

        MOVE DS01-QUAL-GRADE(WS01-INDEX)  TO  WS-C-QUAL-GRADE

        MOVE DS01-PRIVATE-CANDIDATE(WS01-INDEX)
          TO WS-C-PRIVATE-CANDIDATE

        MOVE DS01-PARTIAL-ABSENCE(WS01-INDEX) 
          TO WS-C-PARTIAL-ABSENCE.
*
E-CONTINUE.
*LEARNER'S PREVIOUS DRD RECORD DETAILS
        MOVE DS02-REC-TYPE(WS01-INDEX)          
          TO WS-P-REC-TYPE
        MOVE DS02-CANDIDATE-IDR-OTHER(WS01-INDEX) 
          TO WS-P-CANDIDATE-IDR-OTHER
        MOVE DS02-FIRSTNAME-DB(WS01-INDEX)      TO WS-P-FIRSTNAME
        MOVE DS02-SURNAME-DB(WS01-INDEX)        TO WS-P-SURNAME
        MOVE DS02-GENDER(WS01-INDEX)            TO WS-P-GENDER
        MOVE DS02-DOB (WS01-INDEX)              TO WS-P-DOB
        MOVE DS02-NCN (WS01-INDEX)              TO WS-P-NCN
        MOVE DS02-CENTREURN(WS01-INDEX)         TO WS-P-CENTREURN
        MOVE DS02-CENTRE-IDR-OTHER(WS01-INDEX)
          TO WS-P-CENTRE-IDR-OTHER
        MOVE DS02-UKPRN(WS01-INDEX)             TO WS-P-UKPRN
        MOVE DS02-CENTRENAME(WS01-INDEX)        TO WS-P-CENTRENAME
        MOVE DS02-CENTREADDRESS1(WS01-INDEX)    
          TO WS-P-CENTREADDRESS1
        MOVE DS02-CENTREADDRESS2(WS01-INDEX)    
          TO WS-P-CENTREADDRESS2
        MOVE DS02-CENTREADDRESS3(WS01-INDEX)    
          TO WS-P-CENTREADDRESS3
        MOVE DS02-CENTREADDRESS4(WS01-INDEX)    
          TO WS-P-CENTREADDRESS4
        MOVE DS02-CENTREPOSTCODE(WS01-INDEX)    
          TO WS-P-CENTREPOSTCODE
        MOVE DS02-CENTRETYPE(WS01-INDEX)        
          TO WS-P-CENTRETYPE
        MOVE DS02-QUAL-NUMBER-TYPE(WS01-INDEX)
          TO WS-P-QUAL-NUMBER-TYPE
        MOVE DS02-QUAL-NUMBER(WS01-INDEX)       TO WS-P-QUAL-NUMBER
        MOVE DS02-SPEC-CODE(WS01-INDEX)         TO WS-P-SPEC-CODE
        MOVE DS02-SPEC-TITLE(WS01-INDEX)        TO WS-P-SPEC-TITLE
        MOVE DS02-QUAL-TYPE(WS01-INDEX)         TO WS-P-QUAL-TYPE
        MOVE DS02-REG-DATE(WS01-INDEX)          TO WS-P-REG-DATE
        MOVE DS02-FIRST-ENTRY-DATE(WS01-INDEX)  
          TO WS-P-FIRST-ENTRY-DATE
        MOVE DS02-AWARD-DATE(WS01-INDEX)        TO WS-P-AWARD-DATE
        MOVE DS02-QUAL-GRADE(WS01-INDEX)        TO WS-P-QUAL-GRADE.
*
E-EXIT.
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
    MOVE WS-RUN-DATE              TO WS-TR-H1-RUN-DATE.
    MOVE WS-TR-HEAD-1             TO TOT-RECORD.
    WRITE TOT-RECORD AFTER PAGE.
    MOVE WS-TR-HEAD-2             TO TOT-RECORD.
    WRITE TOT-RECORD AFTER 1.
    MOVE WS-START-DATE            TO WS-TR-H3-START-DATE.
    MOVE WS-END-DATE              TO WS-TR-H3-END-DATE.
    MOVE WS-TR-HEAD-3             TO TOT-RECORD.
    WRITE TOT-RECORD AFTER 2.
    MOVE WS-TR-DETAIL             TO TOT-RECORD.
    WRITE TOT-RECORD AFTER 3.
*
    IF  WS-DFE-IGN-CNT = 0
        MOVE "No Learners Ignored."   TO  WS-DFE-IGNORE-RPT
        MOVE  20                      TO  WS-REC-LENGTH
        WRITE  DI-DETAIL-RECORD     FROM  WS-DFE-IGNORE-RPT
    END-IF
*
    CLOSE   CSV-FILE-B.
    CLOSE   CSV-FILE-N.
    CLOSE   IGNORE-RPT.
    CLOSE   TOTALS-RPT.

    PERFORM K-MAINTAIN-DAUD.

    EXEC SQL
         SELECT TO_CHAR (SYSDATE, 'DD-MON-YYYY:HH24:MI')
         INTO   :WS-TIME-DATE
         FROM   DUAL
    END-EXEC.

    DISPLAY '                                                        '
    DISPLAY '                                                        '
    DISPLAY '********************************************************'
    DISPLAY '           BTEC Records Processed Details               '
    DISPLAY '********************************************************'
    DISPLAY 'Total Fetch Count                # ' WS-BTEC-ROWS-TOTAL
    DISPLAY 'Report Generation Count          # ' WS-BTEC-CNT
    DISPLAY 'Insert Count                     # ' WS-BTEC-INSERT-CNT
    DISPLAY 'Result Update Count (R)          # ' WS-BTEC-UPD-RESULT-CNT
    DISPLAY 'Amendment Update Count (A)       # ' WS-BTEC-UPD-AMEND-CNT
    DISPLAY 'Total (R) + (A) Update Count     # ' WS-BTEC-TOTAL-UPD-CNT
    DISPLAY "Skipped Learner's Count          # " WS-BTEC-SKIPPED-CNT
    DISPLAY "EPA/ITU/ESW Learner's Skip Count # " WS-BTEC-IGN-CNT
    DISPLAY "Blocked Learner's Count          # " WS-BLOCKED-LEARNER-CNT
    DISPLAY "BTEC Ignored Learner's Count     # " WS-DFE-IGN-CNT
    DISPLAY '********************************************************'
    DISPLAY "Blocked Check Call               # " WS-CHECK-BLOCKING-CNT
    DISPLAY '********************************************************'
    DISPLAY '                                                        '
    DISPLAY '                                                        '
    DISPLAY '********************************************************'
    DISPLAY '        NVQ Records Processed Details                   '
    DISPLAY '********************************************************'
    DISPLAY 'Total Fetch Count                # ' WS-NVQ-ROWS-TOTAL
    DISPLAY 'Report Generation Count          # ' WS-NVQ-CNT
    DISPLAY 'Insert Count                     # ' WS-NVQ-INSERT-CNT
    DISPLAY 'Result Update Count (R)          # ' WS-NVQ-UPD-RESULT-CNT
    DISPLAY 'Amendment Update Count (A)       # ' WS-NVQ-UPD-AMEND-CNT
    DISPLAY 'Total (R) + (A) Update Count     # ' WS-NVQ-TOTAL-UPD-CNT
    DISPLAY "Skipped Learner's Count          # " WS-NVQ-SKIPPED-CNT
    DISPLAY "EPA/ITU/ESW Learner's Skip Count # " WS-NVQ-IGN-CNT
    DISPLAY '********************************************************'
    DISPLAY '                                                        '

    DISPLAY ' SSPDFE ENDED AT : ' WS-TIME-DATE.

I-040.
        EXEC SQL WHENEVER SQLERROR   GO TO I-050 END-EXEC.

        EXEC SQL COMMIT WORK                     END-EXEC.

        GO TO I-100.
I-050.
        MOVE 'COMMIT WORK FAILED' TO WS-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
I-100.

        DISPLAY "SSPDFE - SUCCESSFULLY COMPLETED".
*
I-EXIT.
*
        EXIT.
*
K-MAINTAIN-DAUD SECTION.
**********************************************************************
* Datafeed Audits
**********************************************************************
K-START.

        EXEC SQL WHENEVER SQLERROR GO TO K-040 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING CONTINUE  END-EXEC.
        EXEC SQL WHENEVER NOT FOUND CONTINUE   END-EXEC.

        IF START-OF-RUN
        THEN
           EXEC SQL
             SELECT DAUD_SEQ.NEXTVAL
             INTO   :WS-DAUD-SEQUENCE
             FROM   DUAL
           END-EXEC
        END-IF.

        EXEC SQL WHENEVER SQLERROR GO TO K-050 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING CONTINUE  END-EXEC.
        EXEC SQL WHENEVER NOT FOUND CONTINUE   END-EXEC.

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
         'SSDDFE',
         'BATH',
         SYSDATE,
         NULL,
         :WS-START-DATE,
         :WS-END-DATE,
         NULL,
         TO_DATE(:WS-NG-GRADING-DATE,'YYYY-MM-DD'),
         :WS-REG-START-DATE,
         :WS-REG-END-DATE,
         NULL,
         NULL
        )
      END-EXEC

* Commit program start and parameters...

        EXEC SQL
          COMMIT WORK
        END-EXEC
        END-IF.

        EXEC SQL WHENEVER SQLERROR GO TO K-060 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING CONTINUE    END-EXEC.
        EXEC SQL WHENEVER NOT FOUND CONTINUE    END-EXEC.

        IF END-OF-RUN
        THEN 
              EXEC SQL
                UPDATE DATAFEED_AUDITS
                SET    DAUD_END = SYSDATE
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

        EXEC SQL WHENEVER SQLERROR GO TO L-040 END-EXEC.
        EXEC SQL WHENEVER SQLWARNING CONTINUE  END-EXEC.
        EXEC SQL WHENEVER NOT FOUND CONTINUE    END-EXEC.
     
        MOVE +0 TO WS-BLOCK-STATUS-I
*
        EXEC SQL EXECUTE
         BEGIN
          PK_PROVDEF.PR_CHECK_STUDENT_DEFERRAL
                       (:WS-CANDIDATE
                       ,:WS-CHECK-UNITS
                       ,:WS-BLOCK-STATUS:WS-BLOCK-STATUS-I
                       ,:WS-NPP-ID:WS-NPP-ID-I
                       ,:WS-DECISION-DATE:WS-DECISION-DATE-I
                       );
         END;
        END-EXEC
*
        IF  WS-BLOCK-STATUS-I EQUAL -1
            MOVE  '7'          TO  WS-BLOCK-STATUS
        END-IF
        
        IF  WS-BLOCK-STATUS NOT = '0' THEN
            MOVE  'Y'          TO  WS-BLOCK-IND
            DISPLAY "Student Deferral Block " WS-CANDIDATE
            ADD 1              TO  WS-BLOCKED-LEARNER-CNT
        ELSE 

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
    
            IF  WS-STATUS-I EQUAL -1
                MOVE '7'         TO  WS-STATUS
            END-IF
    
            IF  WS-STATUS NOT = '0' THEN
                MOVE 'Y'          TO  WS-BLOCK-IND
                DISPLAY "Certificate Block " WS-CANDIDATE
                ADD 1             TO  WS-BLOCKED-LEARNER-CNT
            END-IF

        END-IF.

        GO TO L-EXIT.
L-040.
    MOVE   "L-040 : ERROR CHECKING BLOCKING" TO WS01-ERR-MESSAGE.
    PERFORM ZZ-ABORT.
L-EXIT.
       EXIT.
*
M-CANDIDATE-REC-COMPARE SECTION.
**********************************************************************
* If a candidate already exists in DFE_REPORT_DETAILS table, compare if 
* any of the columns changed and update the record with the record type  
* either Result or Amendment and other modified columns.
**********************************************************************
M-START.
*
        MOVE 'N' TO WS-COL-UPDATE WS-RES-UPDATE

        IF  WS-P-REC-TYPE = 'Entry'
            IF  WS-P-QUAL-GRADE NOT = WS-C-QUAL-GRADE
                MOVE 'Result'     TO WS-REC-TYPE
                MOVE 'Y'          TO WS-RES-UPDATE
                MOVE 'U'  TO  WS-DFE-INS-UPDAT
                PERFORM N-DRD-INSERT-UPDAT
            ELSE
              IF  BTEC-PROCESS
                  ADD  1  TO  WS-BTEC-SKIPPED-CNT
              ELSE
                  ADD  1  TO  WS-NVQ-SKIPPED-CNT
              END-IF
            END-IF
            GO TO M-EXIT
        END-IF

        IF  WS-P-REC-TYPE = 'Result' OR 'Amendment'
            IF  WS-P-QUAL-GRADE       NOT = WS-C-QUAL-GRADE       
                MOVE 'Y'              TO WS-RES-UPDATE
            ELSE 
              IF  WS-P-FIRSTNAME        NOT = WS-C-FIRSTNAME        OR
                  WS-P-SURNAME          NOT = WS-C-SURNAME          OR
                  WS-P-GENDER           NOT = WS-C-GENDER           OR
                  WS-P-DOB              NOT = WS-C-DOB              OR
                  WS-P-NCN              NOT = WS-C-NCN              OR
                  WS-P-CENTREURN        NOT = WS-C-CENTREURN        OR
                  WS-P-CENTRE-IDR-OTHER NOT = WS-C-CENTRE-IDR-OTHER OR
                  WS-P-UKPRN            NOT = WS-C-UKPRN            OR
                  WS-P-CENTRENAME       NOT = WS-C-CENTRENAME       OR
                  WS-P-CENTREADDRESS1   NOT = WS-C-CENTREADDRESS1   OR
                  WS-P-CENTREADDRESS2   NOT = WS-C-CENTREADDRESS2   OR
                  WS-P-CENTREADDRESS3   NOT = WS-C-CENTREADDRESS3   OR
                  WS-P-CENTREADDRESS4   NOT = WS-C-CENTREADDRESS4   OR
                  WS-P-CENTREPOSTCODE   NOT = WS-C-CENTREPOSTCODE   OR
                  WS-P-CENTRETYPE       NOT = WS-C-CENTRETYPE       OR
                  WS-P-QUAL-NUMBER-TYPE NOT = WS-C-QUAL-NUMBER-TYPE OR
                  WS-P-QUAL-NUMBER      NOT = WS-C-QUAL-NUMBER      OR
                  WS-P-QUAL-TYPE        NOT = WS-C-QUAL-TYPE        OR
                  WS-P-SPEC-CODE        NOT = WS-C-SPEC-CODE        OR
                  WS-P-SPEC-TITLE       NOT = WS-C-SPEC-TITLE       OR
                  WS-P-REG-DATE         NOT = WS-C-REG-DATE         
                    MOVE  'Y'           TO WS-COL-UPDATE
              END-IF 
            END-IF 
        END-IF

        IF  RES-UPDATE
            EXEC SQL
                 SELECT FN_GET_DFE_RECORD_TYPE(:WS-P-FIRST-ENTRY-DATE,
                        :WS-C-FIRST-ENTRY-DATE)
                 INTO   :WS-REC-TYPE
                 FROM DUAL
            END-EXEC
            MOVE 'U'           TO  WS-DFE-INS-UPDAT
            PERFORM N-DRD-INSERT-UPDAT
        ELSE  
          IF  COL-UPDATE
              MOVE 'Amendment'       TO WS-REC-TYPE
              MOVE 'U'  TO  WS-DFE-INS-UPDAT
              PERFORM N-DRD-INSERT-UPDAT
          ELSE
            IF  BTEC-PROCESS
                ADD  1  TO  WS-BTEC-SKIPPED-CNT
            ELSE
                ADD  1  TO  WS-NVQ-SKIPPED-CNT
            END-IF
          END-IF
        END-IF.
*
M-EXIT.
        EXIT.
*
N-DRD-INSERT-UPDAT SECTION.
**********************************************************************
* Insert DFE_REPORT_DETAILS for the first with the Record Type
* 'Entry' else Update 'Amendment' if there is any changes to any of the
* columns.
**********************************************************************
N-START.

        IF  DFE-UPDAT

            EXEC SQL WHENEVER SQLERROR GO TO N-050 END-EXEC.
            EXEC SQL WHENEVER SQLWARNING CONTINUE  END-EXEC.
            EXEC SQL WHENEVER NOT FOUND CONTINUE   END-EXEC.
        
            EXEC  SQL
                  UPDATE  DFE_REPORT_DETAILS
                  SET  DRD_REC_TYPE = :WS-REC-TYPE
                      ,DRD_UCI_REF  = :WS-C-UNIQUE-CANDIDATE-IDR
                      ,DRD_ULN = :WS-C-UNIQUE-LEARNER-NUM
                      ,DRD_FORENAMES = DECODE(:WS-C-FIRSTNAME,
                         ' ',NULL,:WS-C-FIRSTNAME)
                      ,DRD_SURNAME = DECODE(:WS-C-SURNAME,
                         ' ',NULL,:WS-C-SURNAME)
                      ,DRD_SEX = :WS-C-GENDER
                      ,DRD_BIRTH_DATE = :WS-C-DOB
                      ,DRD_NCN = :WS-C-NCN      
                      ,DRD_URN = :WS-C-CENTREURN
                      ,DRD_CENTRE_ID = :WS-C-CENTRE-IDR-OTHER
                      ,DRD_UKPRN = :WS-C-UKPRN
                      ,DRD_CENTRE_NAME = :WS-C-CENTRENAME
                      ,DRD_ADDRESS_1 = DECODE(:WS-C-CENTREADDRESS1,
                         ' ',NULL,:WS-C-CENTREADDRESS1)
                      ,DRD_ADDRESS_2 = DECODE(:WS-C-CENTREADDRESS2,
                         ' ',NULL,:WS-C-CENTREADDRESS2)
                      ,DRD_ADDRESS_3 = DECODE(:WS-C-CENTREADDRESS3,
                         ' ',NULL,:WS-C-CENTREADDRESS3)
                      ,DRD_ADDRESS_4 = DECODE(:WS-C-CENTREADDRESS4,
                         ' ',NULL,:WS-C-CENTREADDRESS4)
                      ,DRD_POSTCODE = :WS-C-CENTREPOSTCODE
                      ,DRD_CENTRE_TYPE = :WS-C-CENTRETYPE
                      ,DRD_QUAL_NUM_TYPE = :WS-C-QUAL-NUMBER-TYPE
                      ,DRD_QUAL_NUMBER = :WS-C-QUAL-NUMBER
                      ,DRD_SPEC_CODE = :WS-C-SPEC-CODE
                      ,DRD_SPEC_TITLE = :WS-C-SPEC-TITLE
                      ,DRD_QUAL_TYPE = :WS-C-QUAL-TYPE
                      ,DRD_REG_DATE = :WS-C-REG-DATE
                      ,DRD_FIRST_ENTRY_DATE = 
                       DECODE(:WS-RES-UPDATE,'Y',:WS-C-FIRST-ENTRY-DATE,
                              DRD_FIRST_ENTRY_DATE)  
                      ,DRD_AWARD_DATE = 
                       DECODE(:WS-RES-UPDATE,'Y',:WS-C-AWARD-DATE,
                              DRD_AWARD_DATE) 
                      ,DRD_QUAL_GRADE = :WS-C-QUAL-GRADE
                      ,DRD_AMEND_CUTOFF_DATE = NVL(DRD_AMEND_CUTOFF_DATE,
                       TO_DATE(:WS-AMEND-CUTOFF-DATE,'DD-MON-YYYY'))
                      ,DRD_USER_NAME  = :WS-USER-NAME
                      ,DRD_UPDATE_DATE = SYSDATE
                  WHERE DRD_ST_REG_NO   = :WS-CANDIDATE
            END-EXEC

            IF  BTEC-PROCESS
                ADD  +1  TO  WS-BTEC-TOTAL-UPD-CNT
                IF  WS-REC-TYPE = 'Result'
                    ADD  +1  TO  WS-BTEC-UPD-RESULT-CNT
                ELSE 
                    ADD  +1  TO  WS-BTEC-UPD-AMEND-CNT
                END-IF
            ELSE
                ADD  +1  TO  WS-NVQ-TOTAL-UPD-CNT
                IF  WS-REC-TYPE = 'Result'
                    ADD  +1  TO  WS-NVQ-UPD-RESULT-CNT
                ELSE 
                    ADD  +1  TO  WS-NVQ-UPD-AMEND-CNT
                END-IF
            END-IF

        ELSE 
        
            EXEC SQL WHENEVER SQLERROR GO TO N-060 END-EXEC.
            EXEC SQL WHENEVER SQLWARNING CONTINUE  END-EXEC.
            EXEC SQL WHENEVER NOT FOUND CONTINUE   END-EXEC.
            
            IF  WS-REC-TYPE = 'Entry' OR 'Result'
                EXEC SQL
                  INSERT INTO DFE_REPORT_DETAILS VALUES
                    (:WS-REC-TYPE
                    ,:WS-C-UNIQUE-CANDIDATE-IDR
                    ,:WS-C-UNIQUE-LEARNER-NUM
                    ,:WS-C-UNIQUE-PUPIL-NUM
                    ,:WS-C-CANDIDATE-IDR-OTHER
                    ,DECODE(:WS-C-FIRSTNAME,' ',NULL,:WS-C-FIRSTNAME)
                    ,NULL
                    ,DECODE(:WS-C-SURNAME,' ',NULL,:WS-C-SURNAME)
                    ,:WS-C-GENDER
                    ,:WS-C-DOB
                    ,:WS-C-NCN
                    ,:WS-C-CENTREURN
                    ,:WS-C-LEA
                    ,:WS-C-CENTRE-IDR-OTHER
                    ,:WS-C-UKPRN
                    ,:WS-C-CENTRENAME
                    ,DECODE(:WS-C-CENTREADDRESS1,' ',NULL,
                     :WS-C-CENTREADDRESS1)
                    ,DECODE(:WS-C-CENTREADDRESS2,' ',NULL,
                     :WS-C-CENTREADDRESS2)
                    ,DECODE(:WS-C-CENTREADDRESS3,' ',NULL,
                     :WS-C-CENTREADDRESS3)
                    ,DECODE(:WS-C-CENTREADDRESS4,' ',NULL,
                     :WS-C-CENTREADDRESS4)
                    ,:WS-C-CENTREPOSTCODE
                    ,:WS-C-CENTRETYPE
                    ,:WS-C-QUAL-NUMBER-TYPE
                    ,:WS-C-QUAL-NUMBER
                    ,:WS-C-SPEC-CODE
                    ,:WS-C-SPEC-TITLE
                    ,:WS-C-QUAL-TYPE
                    ,:WS-C-EXAMSERIES
                    ,:WS-C-REG-DATE
                    ,:WS-C-FIRST-ENTRY-DATE
                    ,:WS-C-AWARD-DATE
                    ,:WS-C-QUAL-GRADE
                    ,:WS-C-PRIVATE-CANDIDATE
                    ,:WS-C-PARTIAL-ABSENCE
                    ,DECODE(:WS-REC-TYPE,'Result',
                     TO_DATE(:WS-AMEND-CUTOFF-DATE,'DD-MON-YYYY'),NULL)
                    ,:WS-C-PROG-TYPE
                    ,:WS-USER-NAME
                    ,SYSDATE
                    ,NULL
                    )
                END-EXEC
            END-IF

            IF  BTEC-PROCESS
                ADD  +1  TO  WS-BTEC-INSERT-CNT
            ELSE
                ADD  +1  TO  WS-NVQ-INSERT-CNT
            END-IF

        END-IF.

        IF  BTEC-PROCESS                    
            IF  FIRST-TIME
                MOVE 520                 TO WS-REC-LENGTH
                WRITE  DB-DETAIL-RECORD  FROM  WS-DFE-REPORT-HDR
                MOVE  'N'                TO WS-FIRST-TIME
            END-IF
            PERFORM P-STRING-COLS
            WRITE  DB-DETAIL-RECORD    FROM  WS-DFE-REPORT-DETAIL
            ADD    +1                  TO    WS-BTEC-CNT
        ELSE 
            IF FIRST-TIME
               MOVE 520                 TO WS-REC-LENGTH
               WRITE  DN-DETAIL-RECORD  FROM  WS-DFE-REPORT-HDR
               MOVE  'N'                TO WS-FIRST-TIME
            END-IF
            PERFORM P-STRING-COLS
            WRITE  DN-DETAIL-RECORD  FROM  WS-DFE-REPORT-DETAIL
            ADD  +1                  TO    WS-NVQ-CNT
        END-IF
        
        GO TO N-EXIT.
N-050.
        MOVE   "N-050 : ERROR UPDATING DFE_REPORT_DETAILS" TO WS01-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
N-060.
        DISPLAY 'ERROR Inserting the candidate : ' WS-C-CANDIDATE-IDR-OTHER
        MOVE   "N-060 : ERROR INSERTING DFE_REPORT_DETAILS" TO WS01-ERR-MESSAGE.
        PERFORM ZZ-ABORT.
N-EXIT.
        EXIT.
*
**********************************************************************
* To concatenate the column values without leading and trailing spaces
* for the CSV report.
**********************************************************************
P-STRING-COLS SECTION.
P-START.

        INITIALIZE WS-REC-LENGTH WS-DFE-REPORT-DETAIL.

        EXEC SQL
            SELECT :WS-REC-TYPE||','||
                :WS-C-UNIQUE-CANDIDATE-IDR||','||
                :WS-C-UNIQUE-LEARNER-NUM||','||
                '-2'||','||
                :WS-C-CANDIDATE-IDR-OTHER||','||
                :WS-RPT-FIRSTNAME||','||
                ''||','||
                :WS-RPT-SURNAME||','||
                :WS-C-GENDER||','||
                :WS-C-DOB||','||
                :WS-C-NCN||','||
                :WS-RPT-CENTREURN||','||
                '-2'||','||
                :WS-C-CENTRE-IDR-OTHER||','||
                :WS-RPT-UKPRN||','||
                TRANSLATE(:WS-C-CENTRENAME,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS1,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS2,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS3,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS4,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                REPLACE(:WS-RPT-CENTREPOSTCODE,',',NULL)||','||
                :WS-C-CENTRETYPE||','||
                :WS-C-QUAL-NUMBER-TYPE||','||
                REPLACE(:WS-RPT-QUAL-NUMBER,'  ',' ')||','||
                :WS-C-SPEC-CODE||','||
                TRANSLATE(:WS-C-SPEC-TITLE,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                :WS-RPT-QUAL-TYPE||','||
                '-2'||','||
                :WS-C-REG-DATE||','||
                :WS-C-FIRST-ENTRY-DATE||','||
                :WS-C-AWARD-DATE||','||
                :WS-C-QUAL-GRADE||','||
                '-2'||','||
                '-2',
                LENGTH(:WS-REC-TYPE||','||
                :WS-C-UNIQUE-CANDIDATE-IDR||','||
                :WS-C-UNIQUE-LEARNER-NUM||','||
                '-2'||','||
                :WS-C-CANDIDATE-IDR-OTHER||','||
                :WS-RPT-FIRSTNAME||','||
                ''||','||
                :WS-RPT-SURNAME||','||
                :WS-C-GENDER||','||
                :WS-C-DOB||','||
                :WS-C-NCN||','||
                :WS-RPT-CENTREURN||','||
                '-2'||','||
                :WS-C-CENTRE-IDR-OTHER||','||
                :WS-RPT-UKPRN||','||
                TRANSLATE(:WS-C-CENTRENAME,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS1,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS2,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS3,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                TRANSLATE(:WS-C-CENTREADDRESS4,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                REPLACE(:WS-RPT-CENTREPOSTCODE,',',NULL)||','||
                :WS-C-CENTRETYPE||','||
                :WS-C-QUAL-NUMBER-TYPE||','||
                REPLACE(:WS-RPT-QUAL-NUMBER,'  ',' ')||','||
                :WS-C-SPEC-CODE||','||
                TRANSLATE(:WS-C-SPEC-TITLE,
                          :WS-RESTRICT-SPL-CHARS,' ')||','||
                :WS-RPT-QUAL-TYPE||','||
                '-2'||','||
                :WS-C-REG-DATE||','||
                :WS-C-FIRST-ENTRY-DATE||','||
                :WS-C-AWARD-DATE||','||
                :WS-C-QUAL-GRADE||','||
                '-2'||','||
                '-2')
            INTO  :WS-DFE-REPORT-DETAIL,
                  :WS-REC-LENGTH
            FROM  DUAL
        END-EXEC.
*
P-EXIT.
        EXIT.
*
* WRITE IGNORED LEARNERS TO THE FILE  
*
Q-WRITE-IGNORED-LNR SECTION.
*
Q-START.
*
        INITIALIZE WS-DFE-IGNORE-LNR-DTL.

        IF  IGN-FIRST-TIME
            MOVE  16               TO  WS-REC-LENGTH
            WRITE DI-DETAIL-RECORD FROM  WS-DFE-IGNORE-LNR-HDR
            MOVE 'N'               TO  WS-IGN-FIRST-TIME
        END-IF 

        MOVE  7                    TO  WS-REC-LENGTH 
        MOVE  WS-CANDIDATE         TO  WS-DFE-IGN-REG-NO
        ADD   1                    TO  WS-DFE-IGN-CNT
 
        WRITE  DI-DETAIL-RECORD    FROM  WS-DFE-IGNORE-LNR-DTL.
*
Q-EXIT.
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
        DISPLAY "SSPDFE ERROR".
        DISPLAY WS01-ERR-MESSAGE.

        IF  SQLCODE IS NOT ZERO THEN
            DISPLAY SQLERRMC
        END-IF.
*
        EXEC SQL WHENEVER SQLERROR   GO TO ZZ-050 END-EXEC.

        EXEC SQL ROLLBACK WORK                    END-EXEC.
*
        GO TO ZZ-EXIT.

ZZ-050.
        MOVE 'FAILED DURING PROGRAM ABORT' TO WS-ERR-MESSAGE.

        CALL   "SYS$EXIT"
          USING BY VALUE WS01-ABORT.
*
ZZ-EXIT.
*
        STOP RUN.
