$! PQSD00011.COM
$! Description: Report BTEC NG unconfirmed bookings.
$! October 2012
$! WHO	WHEN		WHAT
$! RSH	15/10/2012	Initial version.
$! RSH	25/10/2012	WI870: Do not send email if no unconfirmed bookings to report.
$! RSH	20/11/2012	WI870: Send email to Jon Soanes (live environment only).
$! RSH	17/01/2013	EC1476: Send email to eprocessing (live environment only).
$! RSH	21/03/2013	EC1551: Send email to MP and JD (live environment only).
$! RSH	22/04/2013	EC1574: Send email to SP and OT (live environment only).
$! RSH	17/12/2013	EC1737: Additional report.
$! RSH  09/01/2014	EC1737: Include MP changes.
$! RSH  15/01/2014	EC1737: Correct all unconfirmed email.
$! RSH  17/01/2014	EC1737: New daily report.
$! RSH  20/01/2014	EC1737: Do not resubmit.
$! RSH	22/03/2016	EC2278: Additional report.
$! RSH	27/04/2016	EC2298: Send delayed confirmations report to KD. 
$! COG  21/06/2019      EC2955: INC5018424  - Add EOL AMS related email address to all emails sent in the file
$! CTS  13/11/2019      EC4043: SRQ0098194 - Remove Kevin's Email ID from the PQS Report 
$! CTS  20/07/2020      EC4550: INC5557262/PRB0066138 - Add James Bradshaw to the Unconfirmed bookigs email list
$! CTS  17/11/2020      EC4773: INC5806769/PRB0067027 - Add users to PQS all unconfirmed bookings email list
$! CTS  19/01/2021      EC4863: SRQ0207890/PRB0067377 - Add Recipient 'AMSUKQuals@pearson.com' for all email listing
$!
$ ON ERROR THEN GOTO fail_error
$ ON WARNING THEN GOTO fail_warning
$ SET VER
$!
$ l_process_stage = "Run pqss0012"
$!
$ sqlplus abcd/abcd @PQS_PROGS:PQSS0012
$!
$ l_process_stage = "Search for output file"
$!
$ file_and_dir = f$search("PQS_FILES:PQSS0012_UNCONFIRMED_BOOKINGS.CSV;0")
$!
$ IF file_and_dir .EQS. ""
$ THEN
$   GOTO fail_warning
$ ENDIF
$!
$ l_process_stage = "Email output file"
$!
$ IF f$file(file_and_dir,"EOF") .NE. 0
$ THEN
$   open/write tempmsg PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG
$   write tempmsg "Mime-version: 1.0"
$   write tempmsg "Content-Type: text/plain; charset=ISO-8859-1"
$   write tempmsg "Content-Transfer-Encoding: 7bit"                                          
$   write tempmsg ""
$   write tempmsg "You have received this email because there are BTEC NG/FS ICT BOOKINGS" 
$   write tempmsg "within the next 4 days which have been placed in IQS but are UNCONFIRMED."
$   write tempmsg "contact IQS@pearson.com to be removed from the email list"
$   write tempmsg ""    
$   write tempmsg "PROMPT ACTION IS REQUIRED"
$   write tempmsg ""                                                                         
$   write tempmsg ""
$   write tempmsg "Link to documents no longer available - Prog ref PQSD0011 "   
$   write tempmsg ""                                                                         
$!   write tempmsg "The contents of this page may change from time to time." 
$!   write tempmsg "You MUST follow the above link each time you receive one of these emails."
$   close tempmsg
$!
$   mime
    open/draft PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG
    add PQS_FILES:PQSS0012_UNCONFIRMED_BOOKINGS.CSV;0
    save
    exit
$!
$   IF f$trnlnm("EVP_ENVIRONMENT_TYPE") .EQS. "P"
$   THEN                                                                                        
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"pearsononscreenplatform@pearson.com"	
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"pamela.hughes@pearson.com"                              
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"gary.marsland@pearson.com"              
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"cheryl.bell@pearson.com"              
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"pamela.clarke@pearson.com"               
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"matthew.ralph@pearson.com"               
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"geraint.davies@pearson.com"               
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"James.Bradshaw@pearson.com"               
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"garcia.cardoso@pearson.com"                 
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"automatedalerts@pearson.com"                 
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"jonathan.taylor@pearson.com"            
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"arunmechery.ousephachan@pearson.com "
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"priyanka.selvam@pearson.com "
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"baskar.r@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" - 
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"nithya.k@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"alan.sturge@pearson.com"  
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"QDM@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"katieisabelle.greenaway@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"fsondemand@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"Alexa.oldham-knott@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"emma.norbury@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"jill.bacon@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"IQSalerts@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"ria.lakhani@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"farihaa.azam@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"adam.jugoo@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"gary.ellis@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"AMSGroup@pearson.com"
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"AMSUKQuals@pearson.com"
$      MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"IQSDEV@pearson.com"
$!
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_BOOK/PQS_BOOK_UNCONF PQSS0012_UNCONFIRMED_BOOKINGS.CSV PQSS0012_UNCONFIRMED_BOOKINGS.CSV
$   ELSE
$!     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"IQSalerts@pearson.com"
$     MAIL/SUBJECT="BTEC/FS URGENT - booking incidents occurring in next 0-4 days" -
        PQS_FILES:TEMP_UNCONFIRMED_BOOKINGS.MSG;0 SMTP%"IQSDEV@pearson.com"
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_TEST/BOOK PQSS0012_UNCONFIRMED_BOOKINGS.CSV PQSS0012_UNCONFIRMED_BOOKINGS.CSV
$   ENDIF
$ ELSE
$   delete/noconf 'file_and_dir'
$!
$   WRITE SYS$OUTPUT "NO URGENT UNCONFIRMED BOOKINGS TO REPORT"
$ ENDIF
$!
$ l_process_stage = "Search for second output file"
$!
$ file_and_dir2 = f$search("PQS_FILES:PQSS0012_ALL_UNCONFIRMED.CSV;0")
$!
$ IF file_and_dir2 .EQS. ""
$ THEN
$   GOTO fail_warning
$ ENDIF
$!
$ l_process_stage = "Email second output file"
$!
$ IF f$file(file_and_dir2,"EOF") .NE. 0
$ THEN
$   open/write tempmsg PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG
$   write tempmsg "Mime-version: 1.0"
$   write tempmsg "Content-Type: text/plain; charset=ISO-8859-1"
$   write tempmsg "Content-Transfer-Encoding: 7bit"
$   write tempmsg ""
$   write tempmsg "Please find attached all BTEC NG/FSICT unconfirmed bookings"
$   close tempmsg
$!
$   mime
    open/draft PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG
    add PQS_FILES:PQSS0012_ALL_UNCONFIRMED.CSV;0
    save
    exit
$
$   IF f$trnlnm("EVP_ENVIRONMENT_TYPE") .EQS. "P"
$   THEN                                                  
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"pearsononscreenplatform@pearson.com"            
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"jonathan.taylor@pearson.com"            
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"pamela.hughes@pearson.com"            
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"kevin.downes@pearson.com" 
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"baskar.r@pearson.com" 
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"qdm@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"IQSalerts@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"arunmechery.ousephachan@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"priyanka.selvam@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"AMSGroup@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"James.Bradshaw@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"katieisabelle.greenaway@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"carl.mason@pearson.com"
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"AMSUKQuals@pearson.com"
$     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"IQSDEV@pearson.com"
$!
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_BOOK/PQS_BOOK_UNCONF PQSS0012_ALL_UNCONFIRMED.CSV PQSS0012_ALL_UNCONFIRMED.CSV
$   ELSE
$!     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"IQSalerts@pearson.com"
$     MAIL/SUBJECT="PQS all unconfirmed bookings" -
        PQS_FILES:TEMP_ALL_UNCONFIRMED.MSG;0 SMTP%"IQSDEV@pearson.com"
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_TEST/BOOK PQSS0012_ALL_UNCONFIRMED.CSV PQSS0012_ALL_UNCONFIRMED.CSV
$   ENDIF
$ ELSE
$   delete/noconf 'file_and_dir2'
$!
$   WRITE SYS$OUTPUT "NO UNCONFIRMED BOOKINGS TO REPORT"
$ ENDIF
$!
$ l_process_stage = "Search for third output file"
$!
$ file_and_dir3 = f$search("PQS_FILES:PQSS0012_DAILY_UNCONFIRMED.CSV;0")
$!
$ IF file_and_dir3 .EQS. ""
$ THEN
$   GOTO fail_warning
$ ENDIF
$!
$ l_process_stage = "Email third output file"
$!
$ IF f$file(file_and_dir3,"EOF") .NE. 0
$ THEN
$   open/write tempmsg PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG
$   write tempmsg "Mime-version: 1.0"
$   write tempmsg "Content-Type: text/plain; charset=ISO-8859-1"
$   write tempmsg "Content-Transfer-Encoding: 7bit"
$   write tempmsg ""
$   write tempmsg "Please find attached BTEC NG/FSICT unconfirmed bookings within the next month"
$   write tempmsg ""
$   write tempmsg "You have received this email because there are BTEC NG or FS ICT BOOKINGS" 
$   write tempmsg "within the next 30  days which have been placed in IQS but are UNCONFIRMED."
$   write tempmsg ""
$   write tempmsg "Link to documents no longer available - Prog ref PQSD0011 "
$   write tempmsg ""
$!   write tempmsg "The contents of this page may change from time to time." 
$!   write tempmsg "You MUST follow the above link each time you receive one of these emails." 
$   close tempmsg
$!
$   mime
    open/draft PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG
    add PQS_FILES:PQSS0012_DAILY_UNCONFIRMED.CSV;0
    save
    exit
$!
$   IF f$trnlnm("EVP_ENVIRONMENT_TYPE") .EQS. "P"
$   THEN                                                                     
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"baskar.r@pearson.com" 
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"QDM@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"IQSalerts@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"arunmechery.ousephachan@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"priyanka.selvam@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"AMSGroup@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"AMSUKQUALS@pearson.com"
$     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"IQSDEV@pearson.com" 
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_BOOK/PQS_BOOK_UNCONF PQSS0012_DAILY_UNCONFIRMED.CSV PQSS0012_DAILY_UNCONFIRMED.CSV
$   ELSE
$!     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"IQSalerts@pearson.com"
$     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"IQSDEV@pearson.com" 
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_TEST/BOOK PQSS0012_DAILY_UNCONFIRMED.CSV PQSS0012_DAILY_UNCONFIRMED.CSV
$   ENDIF
$ ELSE
$   delete/noconf 'file_and_dir3'
$!
$   WRITE SYS$OUTPUT "NO UNCONFIRMED BOOKINGS WITHIN THE NEXT MONTH TO REPORT"
$ ENDIF
$!
$ l_process_stage = "Search for fourth output file"
$!
$ file_and_dir4 = f$search("PQS_FILES:PQSS0012_CONFIRMATION_DELAYS.CSV;0")
$!
$ IF file_and_dir4 .EQS. ""
$ THEN
$   GOTO fail_warning
$ ENDIF
$!
$ l_process_stage = "Email fourth output file"
$!
$ IF f$file(file_and_dir4,"EOF") .NE. 0
$ THEN
$   open/write tempmsg PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG
$   write tempmsg "Mime-version: 1.0"
$   write tempmsg "Content-Type: text/plain; charset=ISO-8859-1"
$   write tempmsg "Content-Transfer-Encoding: 7bit"
$   write tempmsg ""
$   write tempmsg "Please find attached BTEC NG/FSICT delayed booking confirmations"
$   close tempmsg
$!
$   mime
    open/draft PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG
    add PQS_FILES:PQSS0012_CONFIRMATION_DELAYS.CSV;0
    save
    exit
$!
$   IF f$trnlnm("EVP_ENVIRONMENT_TYPE") .EQS. "P"
$   THEN                                                                     
$!     MAIL/SUBJECT="BTEC NG/FS ICT - delayed booking confirmations" -
        PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG;0 SMTP%"AMSGROUP@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - delayed booking confirmations" -
        PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG;0 SMTP%"baskar.r@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - delayed booking confirmations" -
        PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG;0 SMTP%"james.bradshaw@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - delayed booking confirmations" -
        PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG;0 SMTP%"simon.bulmer@pearson.com"
$!     MAIL/SUBJECT="BTEC NG/FS ICT - delayed booking confirmations" -
        PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG;0 SMTP%"arunmechery.ousephachan@pearson.com"
$     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"IQSDEV@pearson.com" 
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_BOOK/PQS_BOOK_UNCONF PQSS0012_CONFIRMATION_DELAYS.CSV PQSS0012_CONFIRMATION_DELAYS.CSV
$   ELSE
$!     MAIL/SUBJECT="BTEC NG/FS ICT - delayed booking confirmations" -
        PQS_FILES:TEMP_CONFIRMATION_DELAYS.MSG;0 SMTP%"IQSalerts@pearson.com"
$     MAIL/SUBJECT="BTEC NG/FS ICT - booking incidents occurring in the next month" -
        PQS_FILES:TEMP_DAILY_UNCONFIRMED.MSG;0 SMTP%"IQSDEV@pearson.com" 
$!
$     @COMMON_TOP:FTP_FILE NT PQS_FILES PQS_TEST/BOOK PQSS0012_CONFIRMATION_DELAYS.CSV PQSS0012_CONFIRMATION_DELAYS.CSV
$   ENDIF
$ ELSE
$   delete/noconf 'file_and_dir4'
$!
$   WRITE SYS$OUTPUT "NO DELAYED BOOKING CONFIRMATIONS TO REPORT"
$ ENDIF
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
