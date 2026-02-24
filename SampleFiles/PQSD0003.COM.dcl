$! PQSD0003.COM
$! Description: Batch BTEC NG results.
$! September 2012
$! WHO	WHEN		WHAT
$! RSH	20/09/2012	Initial version.
$! RSH	23/09/2014	WI1387: C2 business skills results.
$!
$ ON ERROR THEN GOTO fail_error
$ ON WARNING THEN GOTO fail_warning
$ SET VER
$!
$ l_process_stage = "Run pqss0004"
$!
$ sqlplus abcd/abcd @PQS_PROGS:PQSS0004
$!
$ l_process_stage = "Run pqss0005"
$!
$ sqlplus abcd/abcd @PQS_PROGS:PQSS0005
$!
$ l_process_stage = "Search for submit file"
$!
$ file_and_dir = f$search("PQS_FILES:TEMP_SUBMIT_RESULT_BATCHES.COM;0",1)
$!
$ IF file_and_dir .EQS. ""
$ THEN
$   GOTO fail_warning
$ ENDIF
$!
$ l_process_stage = "Run submit file"
$!
$ IF f$file(file_and_dir,"EOF") .NE. 0
$ THEN
$   @'file_and_dir'
$ ELSE
$   delete/noconf 'file_and_dir'
$ ENDIF
$!
$ l_process_stage = "Run pqss0070"
$!
$ sqlplus abcd/abcd @PQS_PROGS:PQSS0070
$!
$ l_process_stage = "Run pqss0071"
$!
$ curr_date = f$extract(0,4,f$cvtime())+f$extract(5,2,f$cvtime())+f$extract(8,2,f$cvtime())+f$extract(11,2,f$cvtime())+f$extract(14,2,f$cvtime())
$!
$ sqlplus abcd/abcd @PQS_PROGS:PQSS0071
$!
$ l_process_stage = "Search for output file"
$!
$ file_and_dir2 = f$search("PQS_FILES:PQSS0071_RESULT_ERRORS.CSV;0",2)
$!
$ IF file_and_dir2 .EQS. ""
$ THEN
$   GOTO end_reports
$ ENDIF
$!
$ l_process_stage = "Email output file"
$!
$ IF f$file(file_and_dir2,"EOF") .NE. 0
$ THEN
$   open/write tempmsg PQS_FILES:TEMP_RESULT_ERRORS.MSG
$   write tempmsg "Mime-version: 1.0"
$   write tempmsg "Content-Type: text/plain; charset=ISO-8859-1"
$   write tempmsg "Content-Transfer-Encoding: 7bit"
$   write tempmsg ""
$   write tempmsg "Please find attached the external results error report"
$   close tempmsg
$!
$   mime
    open/draft PQS_FILES:TEMP_RESULT_ERRORS.MSG
    add PQS_FILES:PQSS0071_RESULT_ERRORS.CSV;0
    save
    exit
$!
$   IF f$trnlnm("EVP_ENVIRONMENT_TYPE") .EQS. "P"
$   THEN
$     MAIL/SUBJECT="PQS external result errors" -
        PQS_FILES:TEMP_RESULT_ERRORS.MSG;0 SMTP%"IQSalerts@pearson.com"
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_REPORTS PQSS0071_RESULT_ERRORS.CSV PQSS0071_RESULT_ERRORS_'curr_date.CSV
$   ELSE
$     MAIL/SUBJECT="PQS external result errors" -
        PQS_FILES:TEMP_RESULT_ERRORS.MSG;0 SMTP%"IQSalerts@pearson.com"
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_TEST/REPORTS PQSS0071_RESULT_ERRORS.CSV PQSS0071_RESULT_ERRORS_'curr_date.CSV
$   ENDIF
$ ELSE
$   delete/noconf 'file_and_dir2'
$!
$   WRITE SYS$OUTPUT "NO EXTERNAL RESULT ERRORS TO REPORT"
$ ENDIF
$!
$END_REPORTS:
$!
$ l_process_stage = "Run pqss0077"
$!
$ curr_date = f$extract(0,4,f$cvtime())+f$extract(5,2,f$cvtime())+f$extract(8,2,f$cvtime())+f$extract(11,2,f$cvtime())+f$extract(14,2,f$cvtime())+f$extract(17,2,f$cvtime())
$!
$ sqlplus abcd/abcd @PQS_PROGS:PQSS0077
$!
$ l_process_stage = "Search for output file"
$!
$ file_and_dir3 = f$search("PQS_FILES:PQSS0077_C2BS_RESULTS.CSV;0",3)
$!
$ IF file_and_dir3 .EQS. ""
$ THEN
$   GOTO end_extracts
$ ENDIF
$!
$ l_process_stage = "Email output file"
$!
$ IF f$file(file_and_dir3,"EOF") .NE. 0
$ THEN
$   open/write tempmsg PQS_FILES:TEMP_C2BS_RESULTS.MSG
$   write tempmsg "Mime-version: 1.0"
$   write tempmsg "Content-Type: text/plain; charset=ISO-8859-1"
$   write tempmsg "Content-Transfer-Encoding: 7bit"
$   write tempmsg ""
$   write tempmsg "Please find attached the C2 Business Skills results extract"
$   close tempmsg
$!
$   mime
    open/draft PQS_FILES:TEMP_C2BS_RESULTS.MSG
    add PQS_FILES:PQSS0077_C2BS_RESULTS.CSV;0
    save
    exit
$!
$   IF f$trnlnm("EVP_ENVIRONMENT_TYPE") .EQS. "P"
$   THEN
$     MAIL/SUBJECT="PQS C2 Business Skills results" -
        PQS_FILES:TEMP_C2BS_RESULTS.MSG;0 SMTP%"jeff.dan@pearson.com"
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_CONTINGENCY PQSS0077_C2BS_RESULTS.CSV BS_RESULT_DATA_'curr_date.CSV
$   ELSE
$     MAIL/SUBJECT="PQS C2 Business Skills results" -
        PQS_FILES:TEMP_C2BS_RESULTS.MSG;0 SMTP%"jeff.dan@pearson.com"
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_TEST/CONTINGENCY PQSS0077_C2BS_RESULTS.CSV BS_RESULT_DATA_'curr_date.CSV
$   ENDIF
$ ELSE
$   delete/noconf 'file_and_dir3'
$!
$   WRITE SYS$OUTPUT "NO C2 BUSINESS SKILLS RESULTS TO EXTRACT"
$ ENDIF
$!
$END_EXTRACTS:
$!
$ EXIT
$!
$! ----------------------------------------
$! Error handling
$! ----------------------------------------
$ ON ERROR GOTO error_handler_error
$ ON WARNING GOTO error_handler_error
$ SET NOVER
$!
$FAIL_ERROR:
$ l_error_type = "ERROR"
$ GOTO exit_with_error
$!
$FAIL_WARNING:
$ l_error_type = "WARNING"
$ GOTO exit_with_error
$!
$EXIT_WITH_ERROR:
$!
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*************************************************"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "An error has occured at stage:  ''l_process_stage'"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "Error type ''l_error_type'"
$ WRITE SYS$OUTPUT ""
$ WRITE SYS$OUTPUT "*************************************************"
$!
$ EXIT 4
$!
$ERROR_HANDLER_ERROR:
$ EXIT 4
