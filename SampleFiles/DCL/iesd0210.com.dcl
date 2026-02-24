$! IESD0210.COM - JCQ BTEC Overall Results 
$!
$! This file is called from EDXF0010 - SPECIAL MODULE RUN
$!
$! Author:	CDS
$! Date:	19th Sep 2016
$!
$! Parameters:	P1 - LIVE or TEST run
$!		P2 - Academic Year
$!		P3 - Exclude BTEC NG Level 2 Results
$!		P4 - Exclude BTEC NG Level 3 Results
$!              P5 - Email address to send report to
$!              P6 - Results Pre Visible Days for TEST run
$!
$! Amendment History:
$! -----------------
$! Who  Change Date  Description
$! ---  -----------  -----------
$! CDS  19-SEP-2016  EC2346 : Initial version.
$! CTS  29-JAN-2024  EC5824 : TEST results backup dates change in
$!                   EDIFACT and JCQresults generation
$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$!
$ @COMMON_TOP:CIP140
$ SHOW SYMBOL DAEMONUSERNAMEOUT
$ SHOW SYMBOL DORACLELOGONOUT
$ SHOW SYMBOL DEMAILADDRESSOUT
$!
$ ENVIRONMENT = F$TRNLNM("EVP_ENVIRONMENT_TYPE")
$ SHOW SYMBOL ENVIRONMENT
$!
$ LIVE_OR_TEST == P1
$!
$ IF P1 .EQS. "LIVE"
$ THEN
$   source_directory == "ies_live_jcq_overall_resack_outbox"
$   destination == "MERCURY"
$   destination_directory == "/edidata/jcq_overall_results"
$!  EC5824
$   PATH == "edi"
$!  EC5824
$   IF ENVIRONMENT .NES. "P"
$   THEN
$     LIVE_OR_TEST == "TEST"
$     source_directory == "ies_test_jcq_overall_resack_outbox"
$     destination == "NT"
$     destination_directory == "edi/smr_jcq_overall_results"
$   ENDIF
$ ELSE
$   source_directory == "ies_test_jcq_overall_resack_outbox"
$   destination == "NT"
$!  EC5824 -- START
$   IF ENVIRONMENT .EQS. "D"
$   THEN
$     PATH == "dvasa/edi"
$     destination_directory == "dvasa/edi/smr_jcq_overall_results"
$   ELSE
$     IF ENVIRONMENT .EQS. "U"
$     THEN
$       PATH == "uat/edi"
$       destination_directory == "uat/edi/smr_jcq_overall_results"
$     ELSE
$       PATH == "edi"
$       destination_directory == "edi/smr_jcq_overall_results"
$     ENDIF
$   ENDIF
$!  EC5824 -- END
$ ENDIF
$! 
$ academic_year == ''P2'
$!
$ IF P3 .EQS. "YES"
$ THEN
$   EXCLUDE_L2 == "Y"
$ ELSE
$   EXCLUDE_L2 == "N"
$ ENDIF
$!!
$ IF P4 .EQS. "YES"
$ THEN
$   EXCLUDE_L3 == "Y"
$ ELSE
$   EXCLUDE_L3 == "N"
$ ENDIF
$!!
$ IF P5 .NES. ""
$ THEN
$   DEMAILADDRESSOUT = ''P5'
$ ENDIF
$!
$!  EC5824
$ RESULTS_PREVISIBLE_TESTDAYS = ''P6'
$!  EC5824
$!
$ write sys$output "Parameters Supplied"
$ show symbol P1
$ show symbol P2
$ show symbol P3
$ show symbol P4
$ show symbol P5
$!  EC5824
$ show symbol P6
$!  EC5824
$ write sys$output "Parameters Used"
$ show symbol live_or_test
$ show symbol exclude_l2
$ show symbol exclude_l3
$ show symbol P4
$ show symbol P5
$!  EC5824
$ show symbol RESULTS_PREVISIBLE_TESTDAYS
$!  EC5824
$!
$  TIM = F$CVTIME()
$  datestring == F$EXTRACT(0,4,TIM)+F$EXTRACT(5,2,TIM)+F$EXTRACT(8,2,TIM)
$  timestring == F$EXTRACT(11,2,TIM)+F$EXTRACT(14,2,TIM)+F$EXTRACT(17,2,TIM)
$!
$ L_OUTFILENAME		== "iess0210_"+datestring+"_"+timestring+".TXT"
$ L_OUTFILENAME_RENAMED == "iess0210_"+datestring+"_"+timestring+".SENT"
$!
$ OPEN/WRITE TEMPCOM IES_TOP:TEMP_IESD0210.COM
$!
$ WRITE TEMPCOM "$ !! JCQ BTEC OVERALL RESULTS PRODUCTION (IESD0210)"
$ WRITE TEMPCOM "$ !! =============================================="
$ WRITE TEMPCOM "$ !!"
$ WRITE TEMPCOM "$ SET VER"
$!  EC5824-Start - A new parameter 'Results Previsible Test Days' is passed
$ write tempcom "$ SQLPLUS abcd/abcd@ORA  @IES_PROGS:IESS0210 ''LIVE_OR_TEST' ''ACADEMIC_YEAR' ''EXCLUDE_L2' ''EXCLUDE_L3' ''DEMAILADDRESSOUT' ''L_OUTFILENAME' ''RESULTS_PREVISIBLE_TESTDAYS'"
$!  EC5824-END
$ WRITE TEMPCOM "$ @ies_top:iesd0230 ''destination' ''source_directory' ''destination_directory' *.*;"
$ WRITE TEMPCOM "$ on error then continue"
$ WRITE TEMPCOM "$ rename/nolog ''source_directory':*.*; ies_archive"
$ WRITE TEMPCOM "$ OPEN/WRITE TEMPMSG IES_TOP:TEMP_IESD0210.MSG"
$ WRITE TEMPCOM "$ WRITE TEMPMSG ""Mime-version: 1.0"""
$ WRITE TEMPCOM "$ WRITE TEMPMSG ""Content-Type: text/plain; charset=ISO-8859-1"""
$ WRITE TEMPCOM "$ WRITE TEMPMSG ""Content-Transfer-Encoding: 7bit"""
$ WRITE TEMPCOM "$ WRITE TEMPMSG """""
$ WRITE TEMPCOM "$ WRITE TEMPMSG ""Please find attached the JCQ BTEC Overall Results Summary Report."""
$ WRITE TEMPCOM "$ WRITE TEMPMSG "" """
$ WRITE TEMPCOM "$ CLOSE TEMPMSG"
$ WRITE TEMPCOM "$ mime"
$ WRITE TEMPCOM "open/draft IES_TOP:TEMP_IESD0210.MSG"
$ WRITE TEMPCOM "add IES_TOP:''l_outfilename'"
$ WRITE TEMPCOM "save"
$ WRITE TEMPCOM "exit"
$ WRITE TEMPCOM "$ MAIL/SUBJECT=""JCQ BTEC OVERALL RESULTS SUMMARY REPORT"" -"
$ WRITE TEMPCOM "      IES_TOP:TEMP_IESD0210.MSG SMTP%""''DEMAILADDRESSOUT'"""
$!  EC5824-START - Included generic variable 'PATH' to route file to specific network folder of DVASA/UAT/PROD
$! WRITE TEMPCOM "$ @common_top:ftp_file nt ies_top edi ''l_outfilename' ''l_outfilename'"
$ WRITE TEMPCOM "$ @common_top:ftp_file nt ies_top ''PATH' ''l_outfilename' ''l_outfilename'"
$!  EC5824-END
$ WRITE TEMPCOM "$ rename/log ies_top:''l_outfilename' ''l_outfilename_renamed'"
$ WRITE TEMPCOM "$ EXIT"
$ CLOSE TEMPCOM
$!
$ SUBMIT/QUE=OVERNIGHT/NOPRINT/KEEP/LOG=IES_LOGS IES_TOP:TEMP_IESD0210
$!
$ EXIT
