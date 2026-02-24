create or replace PACKAGE     pk_edx_eie_udi4 AS

   invalid_call_source   EXCEPTION;
   v_database VARCHAR2(9) := null;
   TYPE st_centre_names_tbl   IS TABLE OF CENTRES.CN_SHORT_NAME%TYPE
	INDEX BY BINARY_INTEGER;
   TYPE st_centre_id_tbl IS TABLE OF STUDENTS.ST_CENTRE_ID%TYPE
	INDEX BY BINARY_INTEGER;
   TYPE st_reg_no_tbl IS TABLE OF students.st_reg_no%TYPE
        INDEX BY BINARY_INTEGER;
   TYPE st_surname_tbl   IS TABLE OF STUDENTS.ST_SURNAME%TYPE
        INDEX BY BINARY_INTEGER;
   TYPE st_forenames_tbl IS TABLE OF STUDENTS.ST_FORENAMES%TYPE
        INDEX BY BINARY_INTEGER;

PROCEDURE pr_iu4_register_candidate
               (p_user_id               IN   VARCHAR2,
                p_ip_address            IN   VARCHAR2,
                p_call_source           IN   NUMBER,
                p_in_centre_id          IN  VARCHAR2,
                p_in_call_type          IN  VARCHAR2,
                p_in_order_no           IN  VARCHAR2,
                p_in_enrol_date         IN  DATE,
                p_in_lsc_code           IN  VARCHAR2,
                p_in_award_code         IN  VARCHAR2,
                p_in_unit_codes         IN  VARCHAR2,
                p_in_centre_ref         IN  VARCHAR2,
                p_in_forenames          IN  VARCHAR2,
                p_in_last_name          IN  VARCHAR2,
                p_in_sex                IN  VARCHAR2,
                p_in_dob                IN  DATE,
                p_in_completion_date    IN  DATE,
                p_in_study_mode         IN  VARCHAR2,
                p_in_combination        IN  VARCHAR2,
                p_in_franchise_no       IN  VARCHAR2,
                p_in_srf_ind            IN  VARCHAR2,
		p_in_licensed		IN  VARCHAR2,
		p_in_no_of_licensed_units IN NUMBER,
		p_in_uln		IN  VARCHAR2,
                p_in_cohort             IN  VARCHAR2,
                p_out_error_message     OUT VARCHAR2,
                p_out_reg_no            OUT students.st_reg_no%TYPE,
		p_in_received_date	IN  DATE DEFAULT NULL,
		p_in_internal_user	IN  VARCHAR2 DEFAULT NULL);
-------------------------------
-- EC2752 same as STV270
-------------------------------
PROCEDURE delete_registration
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_prog_code               IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2);
----------------------------------------
-- LQ00033373 Validate candidate details
----------------------------------------
PROCEDURE pr_iu4_validate_ons_candidate
               (p_user_id               IN   VARCHAR2,
                p_ip_address            IN   VARCHAR2,
                p_call_source           IN   NUMBER,
                p_in_centre             IN  VARCHAR2,
                p_in_qual_code          IN  VARCHAR2,
                p_in_programme_code     IN  VARCHAR2,
                p_in_reg_no             IN  VARCHAR2,
                p_in_forenames          IN  VARCHAR2,
                p_in_lastname           IN  VARCHAR2,
                p_in_dob                IN  DATE,
                p_in_gender             IN  VARCHAR2,
                p_out_error_message     OUT VARCHAR2,
                p_out_reg_no            OUT students.st_reg_no%TYPE,
                p_out_registration_required OUT VARCHAR2);

------------------------------------------
-- UC132 - Claim Candidate Unit Exemptions
------------------------------------------
PROCEDURE pr_udi4_st_unit_exempt_claim
          (p_user_id               IN   VARCHAR2,
           p_ip_address            IN   VARCHAR2,
           p_call_source           IN   NUMBER,
           p_in_programme_code     IN  VARCHAR2, -- SIA and SQA Course Ids
           p_in_reg_no             IN  VARCHAR2,
           p_in_unit_code          IN  VARCHAR2,
           p_in_exempt_ind         IN  VARCHAR2, -- NULL or 'YES'
           p_in_un_exempt_ind      IN  VARCHAR2, -- NULL or 'YES'
           p_in_declaration_ind    IN  VARCHAR2, -- NULL or 'Y'
           p_out_error_message     OUT VARCHAR2);


-------------------------------------------------------------------------------
-- Reinstate/Withdraw KS/ALAN onscreen and paper candidate(s).
-------------------------------------------------------------------------------

PROCEDURE pr_reinstate_withdraw_ks_cand
  (p_user_id                    IN   VARCHAR2,
   p_ip_address                 IN   VARCHAR2,
   p_call_source                IN   NUMBER,
   p_in_reg_no                  IN   VARCHAR2,
   p_in_qual_type               IN   VARCHAR2, --  (KSQ00/ABS/KSQ00OP/ABSOP)
   p_in_withdraw_ind            IN   VARCHAR2, -- NULL or 'YES'
   p_in_reinstate_ind           IN   VARCHAR2, -- NULL or 'YES'
   p_out_error_message          OUT  VARCHAR2
  );

-------------------------------------------------------------------------------
-- 1.18.
-------------------------------------------------------------------------------

PROCEDURE pr_u4_upd_candidate_details
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no			IN  VARCHAR2,
   p_in_centre_ref		IN  VARCHAR2,
   p_in_sex			IN  VARCHAR2,
   p_in_compl_date		IN  DATE,
   p_in_study_mode		IN  VARCHAR2,
   p_in_evidence_route_code     IN  VARCHAR2,
   p_in_combination		IN  VARCHAR2,
   p_in_franchise_no		IN  VARCHAR2,
   p_in_lsc_code		IN  VARCHAR2,
   p_in_surname 		IN VARCHAR2,
   p_in_forenames 		IN VARCHAR2,
   p_in_dob 			IN DATE,
   p_in_uln			IN VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  );


-------------------------------------------------------------------------------
-- 1.21 Insert unit code and result for a candidate for a unit.
-------------------------------------------------------------------------------

PROCEDURE pr_iu4_add_candidate_unit
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_unit_code               IN  VARCHAR2,
   p_in_unit_result             IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2,
   -- EC4940 Summer 2021 Results Delivery Starts
   p_in_unit_ra_ind     IN  VARCHAR2 DEFAULT NULL
   -- EC4940 Summer 2021 Results Delivery Ends
  );

-------------------------------------------------------------------------------
-- 1.22
-------------------------------------------------------------------------------

PROCEDURE pr_u4_claim_candidate_prog
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_award_date		IN  VARCHAR2,
   p_in_overall_result		IN  VARCHAR2,
   p_in_claim_type		IN  VARCHAR2,
   p_in_withdraw		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  );

-------------------------------------------------------------------------------
-- 1.23.
-------------------------------------------------------------------------------

PROCEDURE pr_u4_upd_lcnsd_stud_details
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_full_qual_issued	IN  VARCHAR2,				-- VALUES 'Y' or NULL only
   p_in_unit_cert_issued        IN  VARCHAR2,				-- VALUES 'Y' or NULL only
   p_in_withdrawn               IN  VARCHAR2,				-- VALUES 'Y' or NULL only
   p_in_btec_award_date         IN  VARCHAR2,				-- Format MM/YY
   p_in_accred_service		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  );

-------------------------------------------------------------------------------
-- 1.99
-------------------------------------------------------------------------------

PROCEDURE pr_iu4_add_units_n_claim_prog
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_prog_code               IN  VARCHAR2,
   p_in_unit_codes_and_results  IN  VARCHAR2,
   p_in_award_date		IN  VARCHAR2,
   p_in_overall_result		IN  VARCHAR2,
   p_in_claim_type		IN  VARCHAR2,
   p_in_withdraw		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  );

PROCEDURE pr_valid_btec_reg(p_btec_id       IN VARCHAR2,
                            p_centre_id     IN VARCHAR2,
                            p_commence_date IN DATE,
                            p_out_error     OUT VARCHAR2,
		            p_in_internal_user	IN  VARCHAR2 DEFAULT NULL);

PROCEDURE pr_udi4_student_images
               (p_user_id                     IN   VARCHAR2,
                p_ip_address                  IN   VARCHAR2,
                p_call_source                 IN   NUMBER,
                p_in_reg_no                   IN   VARCHAR2,
                p_in_view_photo_image         IN   VARCHAR2,  -- Y OR NULL
                p_in_view_signature_image     IN   VARCHAR2, -- Y OR NULL
                p_in_upload_photo_image       IN   VARCHAR2,  -- Y OR B OR NULL
                p_in_upload_signature_image   IN   VARCHAR2, -- Y OR B OR NULL
                p_in_delete_photo_image       IN   VARCHAR2,  -- Y OR NULL
                p_in_delete_signature_image   IN   VARCHAR2, -- Y OR NULL
                p_in_photo_image              IN   BLOB,
                p_in_signature_image          IN   BLOB,
                p_in_photo_file_name          IN   VARCHAR2,
                p_in_signature_file_name      IN  VARCHAR2,
                p_out_photo_image               OUT  BLOB,
                p_out_signature_image           OUT  BLOB,
                p_out_error_message             OUT  VARCHAR2);

PROCEDURE pr_udi4_sia_bulk_images
               (p_user_id                     IN   VARCHAR2,
                p_ip_address                  IN   VARCHAR2,
                p_call_source                 IN   NUMBER,
                p_in_call_type                IN   VARCHAR2, -- V OR I
                p_in_reg_no                   IN   VARCHAR2,
                p_in_photo_image              IN   BLOB,
                p_in_signature_image          IN   BLOB,
                p_in_internal_user            IN   VARCHAR2,
                p_out_student_forenames       OUT  VARCHAR2,
                p_out_student_surname         OUT  VARCHAR2,
                p_out_centre_id               OUT  VARCHAR2,
                p_out_sia_ind                 OUT  VARCHAR2,
                p_out_photo_ind               OUT  VARCHAR2,
                p_out_signature_ind           OUT  VARCHAR2,
                p_out_deleted_ind             OUT  VARCHAR2,
                p_out_certificated_ind        OUT  VARCHAR2,
                p_out_invalid_reg_no_ind      OUT  VARCHAR2,
                p_out_error_message           OUT  VARCHAR2);

PROCEDURE pr_udi4_sia_upd_upload_dets
               (p_user_id                     IN   VARCHAR2,
                p_ip_address                  IN   VARCHAR2,
                p_call_source                 IN   NUMBER,
                p_in_batch_number             IN   NUMBER,
                p_in_upload_date              IN   DATE,
                p_in_reg_no                   IN   st_reg_no_tbl,
                p_out_error_reg_no            OUT  st_reg_no_tbl,
                p_out_error_message           OUT  VARCHAR2);

PROCEDURE pr_udi4_sia_update_status
               (p_user_id                     IN   VARCHAR2,
                p_ip_address                  IN   VARCHAR2,
                p_call_source                 IN   NUMBER,
                p_in_reg_no                   IN   st_reg_no_tbl,
                p_out_error_reg_no            OUT  st_reg_no_tbl,
                p_out_error_message           OUT  VARCHAR2);

PROCEDURE test_validate_ons_candidate
               (p_user_id               IN   VARCHAR2,
                p_ip_address            IN   VARCHAR2,
                p_call_source           IN   NUMBER,
                p_in_centre             IN  VARCHAR2,
                p_in_qual_code          IN  VARCHAR2,
                p_in_programme_code     IN  VARCHAR2,
                p_in_reg_no             IN  VARCHAR2,
                p_in_forenames          IN  VARCHAR2,
                p_in_lastname           IN  VARCHAR2,
                p_in_dob                IN  DATE,
                p_in_gender             IN  VARCHAR2);

--EC2752
PROCEDURE test_delete_registration ( p_in_reg_no                  IN  VARCHAR2,
                                     p_in_prog_code               IN  VARCHAR2);

PROCEDURE test_st_unit_exempt_claim
          (p_user_id               IN   VARCHAR2,
           p_ip_address            IN   VARCHAR2,
           p_call_source           IN   NUMBER,
           p_in_programme_code     IN  VARCHAR2,
           p_in_reg_no             IN  VARCHAR2,
           p_in_unit_code          IN  VARCHAR2,
           p_in_exempt_ind         IN  VARCHAR2,
           p_in_un_exempt_ind      IN  VARCHAR2,
           p_in_declaration_ind    IN  VARCHAR2 );

PROCEDURE test_sia_bulk_images;

PROCEDURE test_sia_upd_upload_dets;

PROCEDURE test_sia_update_status(p_in_reg_no st_reg_no_tbl);
   -- EC4940 Summer 2021 Results Delivery Starts
PROCEDURE pr_iu4_add_units_n_claim_ra
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_prog_code               IN  VARCHAR2,
   p_in_unit_codes_and_results  IN  VARCHAR2,
   p_in_award_date		IN  VARCHAR2,
   p_in_overall_result		IN  VARCHAR2,
   p_in_claim_type		IN  VARCHAR2,
   p_in_withdraw		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  );
   -- EC4940 Summer 2021 Results Delivery Ends
END pk_edx_eie_udi4;
/

create or replace PACKAGE BODY     pk_edx_eie_udi4 AS

-------------------------------------------------------------------------------
-- Global Variables
-------------------------------------------------------------------------------
 gv_record_count         NUMBER(8)               := 0;
 l_record_count    	NUMBER(8)             	:= 0;
 dumvar            	VARCHAR2(1)           	:= NULL;
 l_franch_ind	        VARCHAR2(1)           	:= NULL;
 l_module_id    	modules.modu_id%TYPE  	:= 0;
 l_sequence_id 		NUMBER    		:= 0;
 l_call_status		NUMBER(1) 		:= 1;
 l_award_id		VARCHAR2(8)     	:= NULL;
 l_reg_type      	VARCHAR2(1)		:= NULL;
 l_call_type		VARCHAR2(1)     	:= NULL;
 l_user_id      module_call_hist.mchi_in_source_username%TYPE 	  := NULL;
 l_ip_address   module_call_hist.mchi_in_source_ip_address%TYPE   := NULL;
 l_call_source  module_call_source.mcso_id%TYPE 	  	  := NULL;
 l_dml_type     module_call_hist.mchi_in_source_trans_id%TYPE 	  := NULL;
 l_in_request_params module_call_hist.mchi_in_request_params%TYPE := NULL;
 l_out_trans_values  module_call_hist.mchi_out_trans_values%TYPE  := NULL;
 l_error_message     module_call_hist.mchi_out_error_message%TYPE := NULL;
 l_centre_id         students.st_centre_id%TYPE 		  := NULL;
 l_qual_code         varchar2(1)                                  := NULL;
 l_programme_code    students.st_course_id%TYPE                   := NULL;
 l_centre_ref        students.st_centre_ref%TYPE                  := NULL;
 l_surname           students.st_surname%TYPE                     := NULL;
 l_forenames         students.st_forenames%TYPE                   := NULL;
 l_sex               students.st_sex%TYPE                         := NULL;
 l_uln		     students.st_uln%TYPE                         := NULL;
 l_cohort            students.st_cohort%TYPE                      := NULL;
 l_commence_date     students.st_commence%TYPE                    := NULL;
 l_est_completion    students.st_est_completion%TYPE              := NULL;
 l_prog_no           students.st_course_id%TYPE			  := NULL;
 l_reg_no            students.st_reg_no%TYPE                      := NULL;
 l_gender            students.st_sex%TYPE                         := NULL;
 l_units             VARCHAR2(200)				  := NULL;
 l_study_mode	     students.st_study_mode%TYPE		  := NULL;
 l_combination       students.st_comb_id%TYPE			  := NULL;
 l_franchise_no	     students.st_inst_location%TYPE		  := NULL;
 l_lsc_code	     students.st_inst_tec_code%TYPE		  := NULL;
 l_licensed	     students.st_licensed_ind%TYPE		  := NULL;
 l_count_valid_reg   number := 0;
 gv_package_name	VARCHAR2(20) := 'PK_EDX_EIE_UDI4';
 gv_module_name         VARCHAR2(30);
 gv_dml_type            module_call_hist.mchi_in_source_trans_id%TYPE;
 gv_module_id           module_call_hist.mchi_modu_id%TYPE;
 gv_ip_address           module_call_hist.mchi_in_source_ip_address%TYPE;
 gv_out_trans_values    module_call_hist.mchi_out_trans_values%TYPE;
 gv_in_request_params   module_call_hist.mchi_in_request_params%TYPE;
 gv_error_message       module_call_hist.mchi_out_error_message%TYPE := NULL;
 gv_lsc_error		VARCHAR2(30) := NULL;
 gv_row_count		NUMBER;
 gv_status		NUMBER;
 gv_reg_no              students.st_reg_no%type                    := NULL;
 gv_db_reg_no            students.st_reg_no%TYPE;
 gv_user_id              module_call_hist.mchi_in_source_username%TYPE;
 gv_registration_required  VARCHAR2(5) := 'No';
 i			NUMBER;

PROCEDURE pr_log_no_commit
       (p_user_id		VARCHAR2,
        p_dml_type		VARCHAR2,
        p_ip_address		VARCHAR2,
        p_call_source		module_call_source.mcso_id%TYPE,
	p_call_status		NUMBER, -- status: 0 = OK, 1 = BAD
        p_module_id		modules.modu_id%TYPE,
        p_in_request_params	module_call_hist.mchi_in_request_params%TYPE,
    	p_out_trans_values 	module_call_hist.mchi_out_trans_values%TYPE,
        p_rowcount		NUMBER,
        p_error_message 	VARCHAR2
       ) IS

  v_seq_no	NUMBER;

  CURSOR c1 IS
    SELECT module_call_hist_seq_no.NEXTVAL
      FROM DUAL;

  BEGIN

  OPEN c1;
  FETCH c1 INTO v_seq_no;
  CLOSE c1;

  INSERT INTO module_call_hist
    (mchi_id,
     mchi_in_source_username,
     mchi_in_source_ip_address,
     mchi_in_source_trans_id,
     mchi_mcso_id,
     mchi_modu_id,
     mchi_mcst_id,              -- status: 0 = OK, 1 = BAD
     mchi_in_request_params,
     mchi_out_trans_values,
     mchi_query_rowcount,
     mchi_called_date,
     mchi_out_error_message
    )
  VALUES
    (v_seq_no,
     p_user_id,
     p_ip_address,
     p_dml_type,
     p_call_source,
     p_module_id,
     p_call_status,
     p_in_request_params,
     p_out_trans_values,
     p_rowcount,
     SYSDATE,
     p_error_message
    );

  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      RAISE_APPLICATION_ERROR(-20001,
                              'Error - Inserting to mchi table - '||
                              sqlerrm
                             );
  END pr_log_no_commit;

PROCEDURE pr_log_autonomous
       (p_user_id		VARCHAR2,
        p_dml_type		VARCHAR2,
        p_ip_address		VARCHAR2,
        p_call_source		module_call_source.mcso_id%TYPE,
	p_call_status		NUMBER, -- status: 0 = OK, 1 = BAD
        p_module_id		modules.modu_id%TYPE,
        p_in_request_params	module_call_hist.mchi_in_request_params%TYPE,
    	p_out_trans_values 	module_call_hist.mchi_out_trans_values%TYPE,
        p_rowcount		NUMBER,
        p_error_message 	VARCHAR2
       ) IS

  PRAGMA AUTONOMOUS_TRANSACTION;

  v_seq_no	NUMBER;

  CURSOR c1 IS
    SELECT module_call_hist_seq_no.NEXTVAL
      FROM DUAL;

  BEGIN

  OPEN c1;
  FETCH c1 INTO v_seq_no;
  CLOSE c1;

  INSERT INTO module_call_hist
    (mchi_id,
     mchi_in_source_username,
     mchi_in_source_ip_address,
     mchi_in_source_trans_id,
     mchi_mcso_id,
     mchi_modu_id,
     mchi_mcst_id,              -- status: 0 = OK, 1 = BAD
     mchi_in_request_params,
     mchi_out_trans_values,
     mchi_query_rowcount,
     mchi_called_date,
     mchi_out_error_message
    )
  VALUES
    (v_seq_no,
     p_user_id,
     p_ip_address,
     p_dml_type,
     p_call_source,
     p_module_id,
     p_call_status,
     p_in_request_params,
     p_out_trans_values,
     p_rowcount,
     SYSDATE,
     p_error_message
    );

  COMMIT;

  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      RAISE_APPLICATION_ERROR(-20001,
                              'Error - Inserting to mchi table - '||
                              sqlerrm
                             );
  END pr_log_autonomous;

PROCEDURE pr_check_promissor
    (p_centre_id        IN  VARCHAR2,
     p_course_id        IN  VARCHAR2,
     p_in_claim_type	IN  VARCHAR2,
     p_in_withdraw	IN  VARCHAR2
    )
IS

CURSOR check_promissor IS
        SELECT ATRU_MIN_NO_TESTS_REQD
        FROM   AWARD_TITLE_RULES        ATRU,
               APPROVAL_APPLICATION     AA,
               APPROVAL_AWARDS          AW,
               PROMISSOR_TEST_CENTRES   PTC
        WHERE  PTC_CN_CENTRE_ID = P_CENTRE_ID
        AND    PTC_COURSE_ID    = P_COURSE_ID
        AND    AW_COURSE_NUMBER = PTC_COURSE_ID
        AND    AA_APPLICAT_NO   = AW_APPLICAT_NO
        AND    ATRU_AT_NUMBER   = AA_BTEC_TITLE
        AND    EXISTS (SELECT   NULL
                       FROM     ASSESSMENTS
                       WHERE    ASSE_ATRU_ID = ATRU.ATRU_ID
                       AND      ASSE_ADMO_ID in (1,7,11)			-- On-demand TEST
                   )
        AND    NOT EXISTS (SELECT   NULL
                           FROM     ASSESSMENTS
                           WHERE    ASSE_ATRU_ID  = ATRU.ATRU_ID
                           AND      ASSE_ADMO_ID not in (1,7,11)		-- NON On-demand TEST
                          );

v_dum_num  NUMBER(2);

BEGIN

        OPEN check_promissor;
        FETCH check_promissor INTO v_dum_num;
        IF check_promissor%FOUND
	THEN
	  if p_in_claim_type = 'F' and v_dum_num > 1
	  THEN
	    -- If fallback claimed and min no of on-screen tests required is more than one then allow the fallback claim...
            null;
	  ELSE
	    if p_in_withdraw is not null
	    THEN
	      -- Allow withdrawals...
              null;
	    ELSE
              gv_error_message := 'Unit reporting and claims not allowed. '||
	        		  'Course is achieved entirely by on-screen tests. ';
	    END IF;
	  END IF;
          CLOSE check_promissor;
        END IF;

END pr_check_promissor;

PROCEDURE pr_check_fully_tested
    (p_in_award_title   IN  NUMBER,
     p_in_claim_type	IN  VARCHAR2,
     p_in_withdraw	IN  VARCHAR2
    )
IS

CURSOR check_fully_tested IS
        SELECT ATRU_FULLY_TESTED_IND,
	       ATRU_MIN_NO_TESTS_REQD
        FROM   AWARD_TITLE_RULES        ATRU
        WHERE  ATRU_AT_NUMBER   = P_IN_AWARD_TITLE;

v_fully_tested	VARCHAR2(1)	:= null;
v_dum_num  	NUMBER(2)	:= 0;

BEGIN

	v_fully_tested 	:= null;
	v_dum_num	:= 0;

	for r1 in check_fully_tested loop
	  v_fully_tested := r1.atru_fully_tested_ind;
	  v_dum_num	 := r1.atru_min_no_tests_reqd;
	end loop;

	if nvl(v_fully_tested,'N') = 'Y'
	then
	  IF p_in_claim_type = 'F' and v_dum_num > 1
	  THEN
	    -- If fallback claimed and min no of tests required is more than one then allow the fallback claim...
            null;
	  ELSE
	    if p_in_withdraw is not null
	    THEN
	      -- Allow withdrawals...
              null;
	    ELSE
              gv_error_message := 'Unit reporting and claims not allowed. '||
				  'Course is achieved entirely by tests. ';
	    END IF;
	  END IF;
        END IF;

END pr_check_fully_tested;

PROCEDURE pr_auto_abs_approve
    (p_qual_type        IN  VARCHAR2,  -- KSQ00/ABS
     p_centre_id        IN  VARCHAR2
    )

  IS

    v_next_seq          NUMBER := 0;
    v_applicat_no       NUMBER := 0;

    CURSOR c1 IS
    SELECT MAX(aj.aj_seq_no) + 1,
           aw.aw_applicat_no
      FROM approval_joint  aj,
           approval_awards aw
     WHERE aw.aw_applicat_no = aj.aj_applicat_no
       AND aw.aw_course_number = p_qual_type
    GROUP BY aw.aw_applicat_no;

 BEGIN

    -- Logic taken from STP139.sql

    BEGIN

      OPEN c1;
      FETCH c1 INTO v_next_seq, v_applicat_no;
      CLOSE c1;

    EXCEPTION

      WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR
          (-20000,
           'p_insert_approval_joint SELECT:'||SQLERRM);

    END;

    BEGIN
   INSERT INTO approval_joint
      (aj_applicat_no,
       aj_centre_no,
       aj_seq_no
      )
    SELECT v_applicat_no,
           SUBSTR(p_centre_id, 1, 5), -- no approvals at sub-site level
           V_next_seq
      FROM DUAL
     WHERE NOT EXISTS
             (SELECT NULL
                FROM approval_joint aj
               WHERE aj.aj_applicat_no = v_applicat_no
                 AND aj_centre_no||'' = RTRIM(SUBSTR(p_centre_id, 1, 5))
             )
           AND EXISTS
            (SELECT NULL
                FROM centres cn
              WHERE cn.cn_centre_id = RTRIM(SUBSTR(p_centre_id, 1, 5))
             );

    EXCEPTION

      WHEN NO_DATA_FOUND THEN NULL;
      WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR
          (-20000,
           'p_insert_approval_joint INSERT:'||SQLERRM);

    END;
end pr_auto_abs_approve;

-------------------------------------------------------------------------------
-- Validate REG NO
-------------------------------------------------------------------------------


PROCEDURE pr_validate_reg_no
  (p_reg_no 		IN  VARCHAR2
  )
IS
BEGIN
  IF gv_error_message IS NULL THEN
    SELECT
      DECODE(pk_edx_shared.fn_validate_reg_no(p_reg_no),
             'Y', NULL,
             'W', 'Student withdrawn',
             'D', 'Student deleted',
             'N', 'Invalid Student Registration Number',
             'ERROR in PR_VALIDATE_REG_NO'
            )
      INTO gv_error_message
      FROM DUAL;
  END IF;
END pr_validate_reg_no;

-------------------------------------------------------------------------------
-- Validate QUALIFICATION TYPE
-------------------------------------------------------------------------------

PROCEDURE pr_validate_qual_type(p_qual_type IN VARCHAR2)
IS
BEGIN
  IF gv_error_message IS NULL THEN
    IF pk_edx_shared.fn_validate_qual_type(p_qual_type) = 'N' THEN
       gv_error_message := 'Invalid Qualification Type '||p_qual_type;
    END IF;
  END IF;
END pr_validate_qual_type;

-------------------------------------------------------------------------------
-- Validate SEX
-------------------------------------------------------------------------------

PROCEDURE pr_validate_sex
  (p_sex 		IN  VARCHAR2
  )
IS
BEGIN
  IF gv_error_message IS NULL THEN
    IF p_sex = 'M' OR p_sex = 'F' THEN
      NULL;
    ELSE
      gv_error_message := 'Invalid sex: '||p_sex;
    END IF;
  END IF;
END pr_validate_sex;


-------------------------------------------------------------------------------
-- Validate STUDY MODE
-------------------------------------------------------------------------------

PROCEDURE pr_validate_study_mode
  (p_study_mode	IN  VARCHAR2
  )
IS
v_valid_study_mode	VARCHAR2(1);
CURSOR c1 IS
  SELECT 'Y'
    FROM ref_values
   WHERE rv_domain = 'STUDY_MODE'
     AND rv_code = p_study_mode
  ;
BEGIN
  IF gv_error_message IS NULL THEN
    v_valid_study_mode := 'N';
    OPEN c1;
    FETCH c1 INTO v_valid_study_mode;
    CLOSE c1;
    IF v_valid_study_mode <> 'Y' THEN
      gv_error_message := 'Invalid study mode: '||p_study_mode;
    END IF;
  END IF;
END pr_validate_study_mode;


-------------------------------------------------------------------------------
-- Validate FRANCHISE NO
-------------------------------------------------------------------------------

PROCEDURE pr_validate_franchise_no
  (p_franchise_no	IN  VARCHAR2
  )
IS
v_valid_franchise_no	VARCHAR2(1);
CURSOR c1 IS
  SELECT 'Y'
    FROM institutions
   WHERE inst_id = p_franchise_no
  ;
BEGIN
  IF gv_error_message IS NULL THEN
    v_valid_franchise_no := 'N';
    OPEN c1;
    FETCH c1 INTO v_valid_franchise_no;
    CLOSE c1;
    IF v_valid_franchise_no <> 'Y' THEN
      gv_error_message := 'Invalid Collaborative Partner No. '||p_franchise_no;
    END IF;
  END IF;
END pr_validate_franchise_no;


-------------------------------------------------------------------------------
-- Validate LSC CODE
-------------------------------------------------------------------------------

PROCEDURE pr_validate_lsc_code
  (p_lsc_code   IN  VARCHAR2
  )
IS
  v_valid_franchise_no    	VARCHAR2(1);
  CURSOR c1 IS
  SELECT 'Y'
    FROM centre_tecs  ct
   WHERE ct.ct_tec_code  = p_lsc_code
  ;

BEGIN
  IF gv_error_message IS NULL THEN
    IF p_lsc_code IS NOT NULL THEN
      v_valid_franchise_no := 'N';
      OPEN c1;
      FETCH c1 INTO v_valid_franchise_no;
      CLOSE c1;
      IF v_valid_franchise_no <> 'Y' THEN
        gv_error_message := 'Invalid LSC Code: '||p_lsc_code;
      END IF;
    END IF;
  END IF;
END pr_validate_lsc_code;


-------------------------------------------------------------------------------
-- Validate COMBINATION ID
-------------------------------------------------------------------------------

PROCEDURE pr_validate_combination
  (p_comb_id    IN  VARCHAR2,
   p_reg_no	IN  VARCHAR2
  )
IS
  v_ac_award_code	approval_combination.ac_award_code%TYPE;
  v_applicat_no		approval_awards.aw_applicat_no%TYPE;
  v_award_code		approval_awards.aw_award_code%TYPE;
  v_course_id		students.st_course_id%TYPE;

-- Taken from stv060.inp, post-change trigger
  CURSOR c1 IS
  SELECT ac.ac_award_code,
         aw.aw_applicat_no,
         aw.aw_award_code,
         st.st_course_id
    FROM approval_combination ac,
         approval_awards aw,
         students st
   WHERE ac.ac_applicat_no(+) = aw.aw_applicat_no
     AND ac.ac_award_code(+) = aw.aw_award_code
     AND ac.ac_combination(+) = p_comb_id
     AND aw.aw_course_number = st.st_course_id
     AND st.st_reg_no = p_reg_no
  ;
BEGIN
  IF gv_error_message IS NULL THEN
    OPEN c1;
    FETCH c1 INTO v_ac_award_code,
                  v_applicat_no,
                  v_award_code,
                  v_course_id;
    CLOSE c1;
    IF v_ac_award_code IS NULL THEN
      gv_error_message := 'Invalid Comb Id for course '||
                          v_course_id||
                          ' (App no/Award Cd = '||
                          v_applicat_no||
                          '/'||
                          v_award_code||
                          ').';
    END IF;
  END IF;
END pr_validate_combination;

PROCEDURE pr_log
       (p_mchi_id       module_call_hist.mchi_id%TYPE,
        p_user_id       VARCHAR2,
        p_dml_type      VARCHAR2,
        p_ip_address    VARCHAR2,
        p_call_source   module_call_source.mcso_id%TYPE,
	p_call_status   NUMBER,
        p_module_id     modules.modu_id%TYPE,
        p_in_request_params
                module_call_hist.mchi_in_request_params%TYPE,
    	p_out_trans_values
               module_call_hist.mchi_out_trans_values%TYPE,
        p_rowcount 	NUMBER,
        p_date     	DATE,
        p_error_message VARCHAR2)
IS
BEGIN
         INSERT INTO module_call_hist
	       (mchi_id,
		mchi_in_source_username,
		mchi_in_source_ip_address,
		mchi_in_source_trans_id,
		mchi_mcso_id,
		mchi_modu_id,
		mchi_mcst_id,
	   	mchi_in_request_params,
     	 	mchi_out_trans_values,
	  	mchi_query_rowcount,
		mchi_called_date,
		mchi_out_error_message)
      	   VALUES
	       (p_mchi_id,
	        p_user_id,
	      	p_ip_address,
	  	p_dml_type,
		p_call_source,
		NVL(p_module_id,'0'),
		p_call_status,
	   	p_in_request_params,
       		p_out_trans_values,
 	 	p_rowcount,
		SYSDATE,
		p_error_message);

EXCEPTION
	WHEN OTHERS THEN
	RAISE_APPLICATION_ERROR(-20000,'Error inserting to mchi table -
'||sqlerrm);

END pr_log;

PROCEDURE pr_modname(p_modname IN varchar2) IS
BEGIN
       INSERT INTO MODULES
               (MODU_NAME,
                MODU_ASUB_ID,
                MODU_MTYP_ID,
                MODU_TITLE,
                MODU_DESC,
                MODU_LOGGED_DATE)
        VALUES
               (p_modname,
                1,
                'O',
                p_modname,
                p_modname,
                SYSDATE);
END pr_modname;

/* PR_VALIDATE_REGNO
   Get the regno prefix based on the year and the next registration number
   from control_values. If it's a live registration(rather than prevalidation)
   then update control values increasing the next number by 1
*/

PROCEDURE pr_validate_regno(p_start_date DATE,
			    p_call_type  VARCHAR2,
		            p_regno OUT VARCHAR2)
IS
   l_regno_year		  VARCHAR2(4)	:= null;
   l_reg_desc		  VARCHAR2(1)	:= null;
   l_rowid		  VARCHAR2(30)	:= null;
   l_regno		  VARCHAR2(7) 	:= null;
   l_cv_next_value	  NUMBER(6)	:= 0;
   l_alloc_reg_no_message VARCHAR2(132) := null;

BEGIN
       IF    TO_CHAR(p_start_date,'MON')  = 'SEP' THEN
             l_regno_year := TO_CHAR(p_start_date, 'YYYY');
       ELSIF TO_CHAR(p_start_date, 'MON') = 'OCT' THEN
             l_regno_year := TO_CHAR(p_start_date, 'YYYY');
       ELSIF TO_CHAR(p_start_date, 'MON') = 'NOV' THEN
             l_regno_year := TO_CHAR(p_start_date, 'YYYY');
       ELSIF TO_CHAR(p_start_date, 'MON') = 'DEC' THEN
             l_regno_year := TO_CHAR(p_start_date, 'YYYY');
       ELSE
             l_regno_year := to_char(to_number(TO_CHAR(p_start_date,
               'YYYY')) - 1);
       END IF;

 SELECT   rv_description
   INTO   l_reg_desc
   FROM   REF_VALUES
   WHERE  RV_DOMAIN = 'YEAR_PREFIX'
   AND 	  RV_CODE = l_regno_year;

  PR_ALLOCATE_REG_NO
     (l_reg_desc,
      l_regno,
      l_rowid,
      l_cv_next_value,
      l_alloc_reg_no_message
     );

  if l_alloc_reg_no_message is not null
  then
    RAISE_APPLICATION_ERROR(-20000,'Error - '||l_alloc_reg_no_message);
  end if;

  IF p_call_type = 'I' AND l_error_message IS NULL
  THEN

    l_alloc_reg_no_message := null;

    PR_UPDATE_REG_NO_CV
      (l_rowid,
       l_cv_next_value,
       l_alloc_reg_no_message
      );

    if l_alloc_reg_no_message is not null
    then
      RAISE_APPLICATION_ERROR(-20000,'Error - '||l_alloc_reg_no_message);
    end if;

  END IF;

  p_regno :=  l_regno;

EXCEPTION
    WHEN OTHERS THEN
	RAISE_APPLICATION_ERROR(-20000,'Error getting regno '||sqlerrm);

END pr_validate_regno;

/* PR_GET_MODULE_ID - check if the module name exists in MODULES table
   If it does get the module id, if not then insert the new name.
*/

PROCEDURE pr_get_module_id(p_modname IN VARCHAR2,
			   p_mod_id  OUT VARCHAR2)
IS
CURSOR get_mod IS
      	SELECT 	modu_id
        FROM 	modules
       	WHERE 	modu_name = p_modname;
BEGIN
	OPEN  get_mod;
        FETCH get_mod INTO p_mod_id;
        IF get_mod%NOTFOUND THEN

      	  INSERT INTO MODULES
               (MODU_NAME,
                MODU_ASUB_ID,
                MODU_MTYP_ID,
                MODU_TITLE,
                MODU_DESC,
                MODU_LOGGED_DATE)
          VALUES
               (p_modname,
                1,
                'O',
                p_modname,
                p_modname,
                SYSDATE);
        END IF;
     	CLOSE get_mod;

EXCEPTION
	WHEN OTHERS THEN
	RAISE_APPLICATION_ERROR(-20000,'Error inserting to MODULES
table '||sqlerrm);

END pr_get_module_id;


FUNCTION fn_extract_proctype(p_modname IN VARCHAR2) RETURN VARCHAR2 IS
        dml_type module_call_hist.mchi_in_source_trans_id%TYPE;
BEGIN
        SELECT SUBSTR  (p_modname,
        (INSTR(p_modname,'_',1) + 1),
           (INSTR(p_modname,'_',1,2)  -
              (INSTR(p_modname,'_',1)+1)
        ))
        INTO dml_type
        FROM dual;
        RETURN dml_type;

END fn_extract_proctype;

FUNCTION check_franchise_units(p_franchise VARCHAR2, p_unit VARCHAR2)
RETURN VARCHAR2 IS
     	v_char VARCHAR2(1);

-- This cursor taken from STP140 DABCBAA-905.
-- NB. There is another cursor which validates franchise no for NVQ
--     registrations at stp140 DABCBAA-920, however this is not applicable
--     here.
CURSOR fran_units_cur1 IS
        SELECT  'X'
        FROM    APPROVAL_UNITS,
                APPROVAL_AWARDS,
                FRANCHISE_ARRANGEMENT
        WHERE   FA_INST_ID      = p_franchise
        AND     FA_COURSE_NUMBER= AW_COURSE_NUMBER
        AND     AW_APPLICAT_NO  = AU_APPLICAT_NO
        AND     AW_AWARD_CODE   = AU_AWARD_CODE
        AND     AU_UNIT_NO      = p_unit
        UNION
        SELECT  'X'
        FROM    IU_APPROVAL_UNITS,
                FRANCHISE_ARRANGEMENT
        WHERE   FA_INST_ID      =  p_franchise
        AND     FA_IAA_APPLICAT_NO = IAU_APPLICAT_NO
        AND     IAU_UNIT_NO     = p_unit
  ;

BEGIN
        v_char := 'N';
	OPEN fran_units_cur1;
	FETCH fran_units_cur1 INTO v_char;
        CLOSE fran_units_cur1;
        RETURN v_char;
END;

FUNCTION check_franchise(p_franchise VARCHAR2, p_award VARCHAR2)
RETURN VARCHAR2 IS
	v_char VARCHAR2(1);
CURSOR fran_cur IS
     SELECT 'Y'
     FROM   FRANCHISE_ARRANGEMENT
     WHERE  FA_INST_ID 		= p_franchise
     AND    FA_COURSE_NUMBER 	= p_award;
BEGIN
	OPEN fran_cur;
	FETCH fran_cur INTO v_char;
	IF fran_cur%NOTFOUND THEN
	    return 'N';
	ELSE
	    return 'Y';
	END IF;
END;

PROCEDURE check_source(v_call_source NUMBER) IS

CURSOR check_source_cur IS
      	SELECT 'X'
        FROM MODULE_CALL_SOURCE
       	WHERE MCSO_ID = v_call_source;

BEGIN
	OPEN check_source_cur;
	FETCH check_source_cur INTO dumvar;

	IF check_source_cur%NOTFOUND THEN

	  SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          INTO l_sequence_id
          FROM dual;

            pk_edx_eie_udi4.pr_log
                ( l_sequence_id,
                  l_user_id,
                  l_dml_type,
                  l_ip_address,
                  l_call_source,
                  l_call_status,
                  l_module_id,
                  l_in_request_params,
                  '',
                  0,
                  SYSDATE,
                  'Invalid Call Source id');

          COMMIT;

 	  RAISE_APPLICATION_ERROR
          (-20000,'Invalid Call source id ');

          l_error_message 	:= null;
          l_in_request_params 	:= null;

	  CLOSE check_source_cur;
        END IF;

       	CLOSE check_source_cur;

EXCEPTION
	WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20000,'Error with call source
id '||sqlerrm);

END check_source;

PROCEDURE pr_eie_exclusions(p_user VARCHAR2, p_ip VARCHAR2) IS
CURSOR exclusion_cur IS
   SELECT 'X'
   FROM   eie_exclusions
   WHERE  eexc_user_id = upper(rtrim(ltrim(p_user)))
   OR    eexc_ip_address = p_ip;
BEGIN
   OPEN exclusion_cur;
   FETCH exclusion_cur INTO dumvar;
   IF exclusion_cur%FOUND THEN

        SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          INTO l_sequence_id
          FROM dual;

   pk_edx_eie_udi4.pr_log
                ( l_sequence_id,
                  l_user_id,
                  l_dml_type,
                  l_ip_address,
                  l_call_source,
                  l_call_status,
                  l_module_id,
                  l_in_request_params,
                  '',
                  0,
                  SYSDATE,
                  'Invalid USER and-or IP ADDRESS');

        COMMIT;
        RAISE_APPLICATION_ERROR(-20000,'Invalid log in');

     ELSE
        NULL;
     END IF;
   CLOSE exclusion_cur;
END pr_eie_exclusions;

/*  this procedure not needed at present as reg type is not input ..
PROCEDURE pr_validate_reg_type
                (p_reg_type             IN  VARCHAR2)
IS
BEGIN
   IF p_reg_type IN ('I', 'S', 'R') then
      null;
   ELSE
      	l_reg_type := 'R';

	IF l_call_type <> 'I' THEN

	l_call_status   := 2;

        SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          INTO l_sequence_id
          FROM dual;

            pk_edx_eie_udi4.pr_log
                ( l_sequence_id,
                  l_user_id,
                  l_dml_type,
                  l_ip_address,
                  l_call_source,
                  l_call_status,
                  l_module_id,
                  l_in_request_params,
                  '',
                  0,
                  SYSDATE,
                  l_error_message);

        COMMIT;

	l_error_message := null;

	END IF;

   END IF ;
EXCEPTION
	WHEN OTHERS THEN
	RAISE_APPLICATION_ERROR(-20000,'Error validating regtype '||sqlerrm);
END pr_validate_reg_type;
*/

PROCEDURE pr_eie_students_insert(p_regno eie_students.estu_st_reg_no%TYPE,
				 p_eie_type VARCHAR2)
IS
CURSOR previous_result_cur IS
	SELECT 'X'
	FROM EIE_STUDENTS
	WHERE estu_st_reg_no = p_regno
	AND estu_esdt_id = p_eie_type;
BEGIN
	OPEN previous_result_cur;
	FETCH previous_result_cur INTO dumvar;
	IF previous_result_cur%NOTFOUND THEN
 	  INSERT INTO EIE_STUDENTS(estu_st_reg_no, estu_esdt_id)
	  VALUES(p_regno, p_eie_type);
        ELSE
	  NULL;
	END IF;
	CLOSE previous_result_cur;
EXCEPTION

        WHEN OTHERS THEN

	SELECT MODULE_CALL_HIST_SEQ_NO.nextval
        INTO l_sequence_id
        FROM dual;

 	l_error_message :=  'Could not insert to EIE_STUDENTS';

            pk_edx_eie_udi4.pr_log
                ( l_sequence_id,
                  l_user_id,
                  l_dml_type,
                  l_ip_address,
                  l_call_source,
                  l_call_status,
                  l_module_id,
                  l_in_request_params,
                  '',
                  0,
                  SYSDATE,
                  l_error_message);

          COMMIT;

RAISE_APPLICATION_ERROR(-20000,'Failed to insert to EIE_STUDENTS '||sqlerrm);

END pr_eie_students_insert;

/***********************************************************************/

PROCEDURE pr_validate_student(p_st_regno      IN VARCHAR2,
			      p_in_claim_type IN VARCHAR2,
			      p_in_withdraw   IN VARCHAR2)
IS
      v_award_id	VARCHAR2(13);
      v_cert_end	date                        := null;
      v_cert_end2	date                        := null;
      v_nqf_ind		AWARD_CODES.AC_NQF_IND%TYPE := null;
      v_award_code	AWARD_CODES.AC_CODE%TYPE    := null;
      v_award_title	AWARD_TITLES.AT_NUMBER%TYPE := 0;

CURSOR stud_details IS
	SELECT 	st_reg_no,
		st_reg_type,
		st_course_id,
                st_cert_no,
		st_nvq_registered_id,
		st_nvq_certificate_no,
		st_delete,
	       	st_centre_id centre_id,
		st_award_claim,
                st_nvq_claim_id,
		st_reg_date,
		st_commence,
		st_bnm_grade,
		st_expired_ind
	  FROM students
	 WHERE st_reg_no = p_st_regno;

	stud_rec stud_details%ROWTYPE;

CURSOR student_units_cur IS
	SELECT  nscu_st_reg_no,
      		nscu_ncun_ncvq_code,
		nscu_achieved_year
	FROM    nvq_student_competence_units
	WHERE   nscu_st_reg_no = p_st_regno
	UNION
	SELECT  su_reg_no,
		su_unit_id,
		su_date
	FROM    student_units
	WHERE   su_reg_no = p_st_regno;

        st_units_rec student_units_cur%ROWTYPE;

CURSOR pending_deletion_cur IS
	SELECT  null
	FROM    invoice_audit_trails
	WHERE   iatr_st_reg_no = p_st_regno
	AND     iatr_adjustment_type = 'P';

        pending_deletion_rec pending_deletion_cur%ROWTYPE;

cursor c1 is
 SELECT max(NACC_CERTIFICATE_END_DATE)
  FROM  BTEC.NVQ_ACCREDITATIONS
       ,BTEC.NVQS
  WHERE NVQ_ID        = NACC_NVQ_ID
   AND  NVQ_NCVQ_CODE = stud_rec.st_nvq_registered_id;

cursor c2 is
 SELECT MAX(AACC_CERTIFICATE_END_DATE)
  FROM  ATAC_ACCREDITATIONS,
        APPROVAL_AWARDS,
        APPROVAL_APPLICATION
  WHERE AACC_AT_NUMBER   = AA_BTEC_TITLE
   AND  AACC_AC_CODE     = AW_AWARD_CODE
   AND  AA_APPLICAT_NO   = AW_APPLICAT_NO
   AND  AW_COURSE_NUMBER = stud_rec.st_course_id;

cursor c3 is
 SELECT add_months(nvl(aw_app_to_date
                      ,decode(aw_app_to
                             ,null,null
                             ,to_date('31-AUG-'||to_char(greatest(1
                                                                 ,least(aw_app_to + 1
                                                                       ,4000
                                                                       )
                                                                 )
                                                        ,'FM9999'
                                                        )
                                     ,'DD-MON-YYYY'
                                     )
                             )
                      )
                  ,12*(nvl(ac_standard_course_length,1) + 1)
                  )
       ,ac_nqf_ind
       ,ac_code
  FROM  APPROVAL_AWARDS
       ,AWARD_CODES
  WHERE AW_COURSE_NUMBER = stud_rec.st_course_id
   AND  AC_CODE          = AW_AWARD_CODE;

cursor c4 is
 SELECT to_number(aa_btec_title)	award_title
  FROM  APPROVAL_APPLICATION
       ,APPROVAL_AWARDS
       ,AWARD_CODES
  WHERE AA_APPLICAT_NO   = AW_APPLICAT_NO
   AND	AW_COURSE_NUMBER = stud_rec.st_course_id
   AND  AC_CODE          = AW_AWARD_CODE;

BEGIN

  OPEN stud_details;
  FETCH stud_details INTO stud_rec;

	IF stud_details%NOTFOUND THEN
	  l_record_count := 0;

	  SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          INTO l_sequence_id
          FROM dual;

	  pk_edx_eie_udi4.pr_log
                 (l_sequence_id,
                  l_user_id,
                  'U',
                  l_ip_address,
                  l_call_source,
		  1,
                  '3556',  -- module id for udi4 from modules table.
                  p_st_regno,  -- input parameter
                  '',
                  l_record_count,
                  SYSDATE,
                  'Invalid student regno');
          COMMIT;

	  gv_error_message := 'Cannot find Student with reg_no '||p_st_regno;

        END IF;

	IF stud_rec.st_expired_ind IS NOT NULL
        THEN
          gv_error_message := 'Student Registration Expired ';
        END IF;

        pr_check_promissor(stud_rec.centre_id, stud_rec.st_course_id, p_in_claim_type, p_in_withdraw);

    	IF gv_error_message IS NULL
    	THEN
	  v_award_title := 0;
	  for r4 in c4 loop
	    v_award_title := r4.award_title;
	  end loop;

          pr_check_fully_tested(v_award_title, p_in_claim_type, p_in_withdraw);

        END IF;

    	IF gv_error_message IS NULL
    	THEN

	  v_award_id := stud_rec.st_course_id||stud_rec.st_nvq_registered_id;

	  IF stud_rec.st_delete is null THEN

	    OPEN pending_deletion_cur;
	    FETCH pending_deletion_cur INTO pending_deletion_rec;

	    IF pending_deletion_cur%FOUND THEN

	      l_record_count := 0;

	      SELECT MODULE_CALL_HIST_SEQ_NO.nextval
              INTO l_sequence_id
              FROM dual;

	      pk_edx_eie_udi.pr_log
                 (l_sequence_id,
                  l_user_id,
                  'U',
                  l_ip_address,
                  l_call_source,
		  1,
                  '3556',  -- module id for udi4 from modules table.
                  p_st_regno,  -- input parameter
                  '',
                  l_record_count,
                  SYSDATE,
                  'Deleted Student');

              COMMIT;

	      gv_error_message := 'Student reg '||p_st_regno||' is pending deletion';

              CLOSE pending_deletion_cur;

            END IF;

          ELSE

	    l_record_count := 0;

	    SELECT MODULE_CALL_HIST_SEQ_NO.nextval
            INTO l_sequence_id
            FROM dual;

	    pk_edx_eie_udi4.pr_log
                 (l_sequence_id,
                  l_user_id,
                  'U',
                  l_ip_address,
                  l_call_source,
		  1,
                  '3556',  -- module id for udi4 from modules table.
                  p_st_regno,  -- input parameter
                  '',
                  l_record_count,
                  SYSDATE,
                  'Deleted Student');

            COMMIT;

	    gv_error_message := 'Student reg '||p_st_regno||' has been deleted';

	  END IF;

 	  if stud_rec.st_reg_type != 'I'
	  then
--         Check cert end date.
	   if stud_rec.st_course_id is null
	   then
	    if stud_rec.st_nvq_registered_id is not null
	    then
	     open c1;
	     fetch c1 into v_cert_end;
	     close c1;
	     if trunc(sysdate) > nvl(v_cert_end,sysdate-1)
	     then
              gv_error_message := 'NVQ not currently valid for certification';
             end if;
	    end if;
           else
            open c3;
	    fetch c3 into v_cert_end
                         ,v_nqf_ind
                         ,v_award_code;
	    close c3;

	    if v_award_code not in('27','63','64','93','CC','GA','GB','GG','HH')
	    then
	     if nvl(v_nqf_ind,'N') = 'Y'
             then
	      open c2;
	      fetch c2 into v_cert_end;
	      close c2;
	     else
	      open c2;
              fetch c2 into v_cert_end2;
              close c2;
              if v_cert_end2 is not null then v_cert_end := v_cert_end2; end if;
	     end if;

	     if trunc(sysdate) > nvl(v_cert_end,sysdate-1)
	     then
              gv_error_message:='Programme not currently valid for certification';
	     end if;
 	    end if;
	   end if;
	  end if;

          IF stud_rec.st_reg_type <> 'I' THEN

-- If a btec full award registration check if student has registered...
-- Seems they are using this procedure also for NVQ students so add nvq reg

	    IF stud_rec.st_course_id IS NOT NULL
	    OR stud_rec.st_nvq_registered_id IS NOT NULL THEN

-- Has BTEC or NVQ student previously claimed?
	      IF stud_rec.st_cert_no IS NOT NULL
              OR stud_rec.st_nvq_certificate_no IS NOT NULL THEN

           	  l_record_count := 0;

        	  SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          	  INTO l_sequence_id
          	  FROM dual;

          	  pk_edx_eie_udi4.pr_log
                 (l_sequence_id,
                  l_user_id,
                  'U',
                  l_ip_address,
                  l_call_source,
                  1,
                  '3556',  -- module id for udi4 from modules table.
                  p_st_regno,  -- input parameter
                  '',
                  l_record_count,
                  SYSDATE,
                  'Previous Claim');

        	  COMMIT;

        	  gv_error_message := 'Error - Student '||p_st_regno||
' has previously been issued a certificate for '||v_award_id;

              END IF;

              IF stud_rec.st_bnm_grade = 'U' THEN

           	  l_record_count := 0;

        	  SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          	  INTO l_sequence_id
          	  FROM dual;

          	  pk_edx_eie_udi4.pr_log
                 (l_sequence_id,
                  l_user_id,
                  'U',
                  l_ip_address,
                  l_call_source,
                  1,
                  '3556',  -- module id for udi4 from modules table.
                  p_st_regno,  -- input parameter
                  '',
                  l_record_count,
                  SYSDATE,
                  'Overall Grade');

        	  COMMIT;

        	  gv_error_message := 'Error - Overall Grade. Must be declined';

              END IF;

	    END IF;
	  ELSE
-- Itype student reporting results..
            OPEN student_units_cur;
	    FETCH student_units_cur INTO st_units_rec;
	    IF student_units_cur%NOTFOUND THEN

-- Itype student is not registered on any units
              gv_error_message := 'Error - ITYPE Student '||p_st_regno||' is not registered on any units';

            END IF;
	    CLOSE student_units_cur;
          END IF;

      END IF;

IF stud_details%ISOPEN
THEN
	CLOSE stud_details;
END IF;

EXCEPTION

	WHEN OTHERS THEN

	l_error_message := sqlerrm;
        l_record_count  := 0;

	SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          INTO l_sequence_id
          FROM dual;

	  pk_edx_eie_udi4.pr_log
                 (l_sequence_id,
                  l_user_id,
                  'U',
                  l_ip_address,
                  l_call_source,
		  1,
                  '3556',  -- module id for udi4 from modules table.
                  p_st_regno,  -- input parameter
                  '',
                  l_record_count,
                  SYSDATE,
                  gv_error_message);

        COMMIT;

	RAISE_APPLICATION_ERROR(-20000,'Student validation error '||sqlerrm);

END pr_validate_student;

-------------------------------------------------------------------------------
-- Validate CENTRE
-------------------------------------------------------------------------------

PROCEDURE pr_validate_centre(p_centre IN VARCHAR2)
IS
BEGIN
        select  decode(pk_edx_shared.fn_validate_centre(p_centre),
                         'Y',null,
                         'W','|ERROR : Withdrawn Centre '||p_centre||'|',
                         'N','|ERROR : Invalid Centre '||p_centre||'|',
                         '|ERROR in PR_VALIDATE_CENTRE|'
                        )
        into    gv_error_message
        from    dual;
END pr_validate_centre;

PROCEDURE pr_validate_comb(p_course_number IN VARCHAR2, p_combination IN VARCHAR2)
IS

	CURSOR C1 IS
        SELECT 	NULL
          FROM 	APPROVAL_COMBINATION,
		APPROVAL_AWARDS
         WHERE 	AC_APPLICAT_NO	 = AW_APPLICAT_NO+0
	 AND	AC_COMBINATION   = P_COMBINATION
	 AND    AW_COURSE_NUMBER = P_COURSE_NUMBER;

	P_DUMMY VARCHAR2(1);

BEGIN

            OPEN C1;
	    FETCH C1 INTO P_DUMMY;
	    IF 	C1%NOTFOUND
	    THEN
              gv_error_message := 'Error - Invalid Combination '''||P_COMBINATION||'''';
            END IF;
	    CLOSE C1;

END pr_validate_comb;

PROCEDURE pr_validate_srf_ind(p_srf_ind IN VARCHAR2)
IS
BEGIN

  IF p_srf_ind NOT IN ('Y', 'N') THEN
    gv_error_message := 'Invalid SRF indicator - must be "Y" or "N"';
  END IF;

END pr_validate_srf_ind;

PROCEDURE pr_validate_centre_unit
		       (p_centre_id centres.cn_centre_id%TYPE,
 			p_centre_unit VARCHAR2,
			p_commence DATE)
IS
/* checks whether unit is approved at the centre on enrolment date */
CURSOR check_centre_unit_approval IS

        SELECT 1
          FROM ALL_UNIT_APPROVALS
         WHERE AUAP_CENTRE_ID = substr(p_centre_id,1,5)
           AND AUAP_UNIT_CODE = p_centre_unit
           AND (p_commence BETWEEN AUAP_APP_FROM_DATE
                           AND    AUAP_APP_TO_DATE)
        UNION
	SELECT  1
	FROM 	nvq_centre_unit_approvals
	WHERE 	ncua_centre_id 	= substr(p_centre_id,1,5)
	AND 	ncua_ncun_ncvq_code = p_centre_unit
        AND     (p_commence BETWEEN
	        ncua_app_from_date AND ncua_app_to_date);

	v_rec  check_centre_unit_approval%ROWTYPE;

BEGIN
	OPEN check_centre_unit_approval;
	FETCH check_centre_unit_approval INTO v_rec;
	IF check_centre_unit_approval%NOTFOUND THEN
          gv_error_message := 'Invalid Centre-Unit '||p_centre_id||'-'||
                              p_centre_unit;
	END IF;
        CLOSE check_centre_unit_approval;
EXCEPTION
   WHEN OTHERS THEN
	RAISE_APPLICATION_ERROR(-20000,'Error in validate centre-unit
'||sqlerrm);

END pr_validate_centre_unit;

PROCEDURE  pr_check_subcentres(p_cen_id IN centres.cn_centre_id%TYPE)
IS
CURSOR check_subs IS
        SELECT 'X'
        FROM   sys.dual
        WHERE  length(p_cen_id) = 5
        AND    EXISTS (SELECT 'X'
                FROM   CENTRES
                WHERE  CN_CENTRE_ID LIKE p_cen_id||'%'
                AND    LENGTH(CN_CENTRE_ID) = 6);

	subcentres_exist  EXCEPTION;

BEGIN
  OPEN check_subs;
  FETCH check_subs INTO dumvar;
  IF check_subs%FOUND THEN
    gv_error_message := gv_error_message||' * Subcentres exist for '||p_cen_id;
    CLOSE check_subs;
  ELSE
    CLOSE check_subs;
  END IF;

EXCEPTION
	WHEN NO_DATA_FOUND THEN NULL;
	WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR(-20000,'Error in subcentre check '||sqlerrm);

END pr_check_subcentres;

PROCEDURE pr_cohort_block(p_cen_id 	IN VARCHAR2,
			  p_award_id    IN VARCHAR2,
			  p_completion_date IN DATE)
IS
  CURSOR get_bnm_type IS
	SELECT 'X'
	  FROM approval_awards,
	       award_codes
	 WHERE aw_course_number = p_award_id
           AND aw_award_code 	= ac_code
	   AND NVL(AC_BNM_TYPE,'X') IN ('F','N','S','H');

  v_dumchar VARCHAR2(1);

BEGIN

  OPEN get_bnm_type;
  FETCH get_bnm_type
   INTO dumvar;
  IF get_bnm_type%FOUND THEN

  	pk_bnm.pr_new_cohort_block(p_cen_id,
			     	   p_award_id,
			     	   TO_CHAR(p_completion_date,'DD/MM/YYYY'),
			   	   'EOL');
  END IF;
  IF get_bnm_type%ISOPEN THEN
    CLOSE get_bnm_type;
  END IF;

END pr_cohort_block;

PROCEDURE pr_valid_btec_reg(p_btec_id       IN VARCHAR2,
		            p_centre_id     IN VARCHAR2,
			    p_commence_date IN DATE,
			    p_out_error     OUT VARCHAR2,
		            p_in_internal_user	IN  VARCHAR2 DEFAULT NULL)

IS

CURSOR check_btec_valid IS
	SELECT 'Y'
	 FROM approval_awards,
	      approval_joint
	WHERE aj_applicat_no 	= aw_applicat_no
	  AND aj_centre_no 	= SUBSTR(p_centre_id,1,5)
	  AND aw_course_number 	= p_btec_id
	  AND p_commence_date
		<= NVL(AW_APP_TO_DATE,ADD_MONTHS(TO_DATE('31-DEC'||NVL(aw_app_to,9999)),8))
	  AND NVL(AW_APP_FROM_DATE,TO_DATE('01-SEP'||NVL(AW_APP_FROM,2100)))
		<= p_commence_date
	  ;
l_next_seq              number(4,0);
l_applicat_no           number(8,0);
l_blocked               varchar2(1);

BEGIN

  OPEN check_btec_valid;

  FETCH check_btec_valid INTO dumvar;

  IF check_btec_valid%NOTFOUND THEN

    IF p_btec_id = 'ABS' THEN
-- Auto approve btec registrations

    pk_edx_eie_udi4.pr_auto_abs_approve('ABS',p_centre_id);

/*
     select  max(o.aj_seq_no) + 1,
                aw_applicat_no
        INTO
                l_next_seq,
                l_applicat_no
        from
                approval_joint o,
                approval_awards
        where
                aw_applicat_no  = o.aj_applicat_no
        and
                aw_course_number        = 'ABS'
        group by
                aw_applicat_no;


 	INSERT INTO approval_joint
        (aj_applicat_no,aj_centre_no,aj_seq_no)
	VALUES
	('185416',SUBSTR(p_centre_id,1,5),l_next_seq)
	;
*/
    ELSE

    	gv_error_message := 'BTEC '||p_btec_id||' at centre '||p_centre_id||'
is not approved for this date - '||TO_CHAR(p_commence_date);
        p_out_error := gv_error_message;  -- for external calls

    END IF;

  ELSE

   IF	P_IN_INTERNAL_USER = 'Y'
   THEN
	NULL;
   ELSE
   	L_BLOCKED := FN_EXT_REG_BLOCK(P_CENTRE_ID,P_BTEC_ID);
    	IF	L_BLOCKED = 'Y'
	THEN
    	 gv_error_message := 'BTEC '||p_btec_id||' at centre '||p_centre_id||
           ' blocked for external registration.';
         p_out_error := gv_error_message;
    	END IF;
   END IF;

  END IF;

  CLOSE check_btec_valid;

END pr_valid_btec_reg;

PROCEDURE pr_udi4_student_images
               (p_user_id                     IN   VARCHAR2,
                p_ip_address                  IN   VARCHAR2,
                p_call_source                 IN   NUMBER,
                p_in_reg_no                   IN   VARCHAR2,
                p_in_view_photo_image         IN   VARCHAR2,  -- Y OR NULL
                p_in_view_signature_image     IN   VARCHAR2, -- Y OR NULL
                p_in_upload_photo_image       IN   VARCHAR2,  -- Y OR B OR NULL
                p_in_upload_signature_image   IN   VARCHAR2, -- Y OR B OR NULL
                p_in_delete_photo_image       IN   VARCHAR2,  -- Y OR NULL
                p_in_delete_signature_image   IN   VARCHAR2, -- Y OR NULL
                p_in_photo_image              IN   BLOB,
                p_in_signature_image          IN   BLOB,
                p_in_photo_file_name          IN   VARCHAR2,
                p_in_signature_file_name      IN  VARCHAR2,
                p_out_photo_image               OUT  BLOB,
                p_out_signature_image           OUT  BLOB,
                p_out_error_message             OUT  VARCHAR2)
IS

 lblob BLOB;

 cursor c_check_photo
 is
   select DBMS_LOB.GETLENGTH(spho_face)		photo_length
   from   student_photos
   where  spho_st_reg_no = p_in_reg_no;

 cursor c_check_signature
 is
   select DBMS_LOB.GETLENGTH(ssig_signature)    signature_length
   from   student_signatures
   where  ssig_st_reg_no = p_in_reg_no;

 cursor c_spho_seq
 is
    select spho_seq.nextval
    from   dual;

 cursor c_ssig_seq
 is
    select ssig_seq.nextval
    from   dual;

  n_spho_seq student_photos.spho_id%type;
  n_ssig_seq student_signatures.ssig_id%type;

  n_face_length		number := 0;
  n_signature_length	number := 0;

  n_in_photo_length	number := 0;
  n_in_signature_length number := 0;

  photo_exists		boolean;
  signature_exists	boolean;

  procedure pr_insert_photo(p_spho_seq		 number,
			    p_reg_no		 varchar2,
       			    p_photo_image	 blob,
       			    p_photo_file_name varchar2)
  is
  begin
     insert into student_photos
     (spho_id,
      spho_st_reg_no,
      spho_face,
      spho_face_file_name
     )
     values
     (p_spho_seq,
      p_reg_no,
      p_photo_image,
      p_photo_file_name
     );
  end pr_insert_photo;

  procedure pr_insert_signature(p_ssig_seq		number,
			    	p_reg_no		varchar2,
       			    	p_signature_image	blob,
       			    	p_signature_file_name 	varchar2)
  is
  begin
     insert into student_signatures
     (ssig_id,
      ssig_st_reg_no,
      ssig_signature,
      ssig_signature_file_name
     )
     values
     (p_ssig_seq,
      p_reg_no,
      p_signature_image,
      p_signature_file_name
     );
  end pr_insert_signature;

BEGIN

  -- Do initial stuff

  gv_module_name := 'PR_UDI4_STUDENT_IMAGES';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  gv_in_request_params :=
		'P1 = '||p_in_reg_no||' | '||
		'P2 = '||p_in_view_photo_image||' | '||
		'P3 = '||p_in_view_signature_image||' | '||
		'P4 = '||p_in_upload_photo_image||' | '||
		'P5 = '||p_in_upload_signature_image||' | '||
		'P6 = '||p_in_delete_photo_image||' | '||
		'P7 = '||p_in_delete_signature_image||' | '||
--		'P8 = '||p_in_photo_image||' | '||
--		'P9 = '||p_in_signature_image||' | '||
		'P10 = '||p_in_photo_file_name||' | '||
		'P11 = '||p_in_signature_file_name;

  photo_exists	   := FALSE;
  signature_exists := FALSE;

  for r1 in c_check_photo loop
    photo_exists  := TRUE;
    n_face_length := r1.photo_length;
  end loop;

  for r2 in c_check_signature loop
    signature_exists  := TRUE;
    n_signature_length := r2.signature_length;
  end loop;

  if NVL(p_in_upload_photo_image,'N') = 'Y'
  then
    select DBMS_LOB.GETLENGTH(p_in_photo_image )
    into   n_in_photo_length
    from   dual;

    if n_in_photo_length > 0
    then
      if photo_exists
      then
        delete student_photos
        where  spho_st_reg_no = p_in_reg_no;
      end if;

      open  c_spho_seq;
      fetch c_spho_seq into n_spho_seq;
      close c_spho_seq;

      pr_insert_photo(n_spho_seq,p_in_reg_no,p_in_photo_image,p_in_photo_file_name);
    end if;
  end if;

  if NVL(p_in_upload_signature_image,'N') = 'Y'
  then
    select DBMS_LOB.GETLENGTH(p_in_signature_image )
    into   n_in_signature_length
    from   dual;

    if n_in_signature_length > 0
    then
      if signature_exists
      then
        delete student_signatures
        where  ssig_st_reg_no = p_in_reg_no;
      end if;

      open  c_ssig_seq;
      fetch c_ssig_seq into n_ssig_seq;
      close c_ssig_seq;

      pr_insert_signature(n_ssig_seq,p_in_reg_no,p_in_signature_image,p_in_signature_file_name);
    end if;
  end if;

  -- RSH 21/05/2009 (LQ00033373)
  if NVL(p_in_upload_photo_image,'N') = 'B'
  then
    select DBMS_LOB.GETLENGTH(p_in_photo_image )
    into   n_in_photo_length
    from   dual;

    if n_in_photo_length > 0
    then
      if photo_exists
      then
        p_out_error_message := 'Cannot bulk upload existing photo';
      else
        open  c_spho_seq;
        fetch c_spho_seq into n_spho_seq;
        close c_spho_seq;

        pr_insert_photo(n_spho_seq,p_in_reg_no,p_in_photo_image,p_in_photo_file_name);
      end if;
    end if;
  end if;

  if NVL(p_in_upload_signature_image,'N') = 'B'
  then
    select DBMS_LOB.GETLENGTH(p_in_signature_image )
    into   n_in_signature_length
    from   dual;

    if n_in_signature_length > 0
    then
      if signature_exists
      then
	if p_out_error_message is null
        then
          p_out_error_message := 'Cannot bulk upload existing signature';
        else
          p_out_error_message := 'Cannot bulk upload existing photo and signature';
        end if;
      else
        open  c_ssig_seq;
        fetch c_ssig_seq into n_ssig_seq;
        close c_ssig_seq;

        pr_insert_signature(n_ssig_seq,p_in_reg_no,p_in_signature_image,p_in_signature_file_name);
      end if;
    end if;
  end if;

  if NVL(p_in_delete_photo_image,'N') = 'Y'
  then
    if photo_exists
    then
      delete student_photos
      where spho_st_reg_no = p_in_reg_no;
    end if;
  end if;

  if NVL(p_in_delete_signature_image,'N') = 'Y'
  then
    if signature_exists
    then
      delete student_signatures
      where ssig_st_reg_no = p_in_reg_no;
    end if;
  end if;

  if NVL(p_in_view_photo_image,'N') = 'Y'
  then
    select spho_face
    into   p_out_photo_image
    from   student_photos
    where  spho_st_reg_no = p_in_reg_no;
  end if;

  if NVL(p_in_view_signature_image,'N') = 'Y'
  then
    select ssig_signature
    into   p_out_signature_image
    from   student_signatures
    where  ssig_st_reg_no = p_in_reg_no;
  end if;

  IF p_out_error_message IS NULL THEN
     gv_error_message	 := NULL;
     gv_status 	 	 := 0;
     gv_row_count 	 := 1;
     gv_out_trans_values := NULL;
  ELSE
     gv_error_message	 := p_out_error_message;
     gv_status	 	 := 1;
     gv_row_count 	 := 0;
     gv_out_trans_values := NULL;
  END IF;

  -------------------
  -- PROCESSING END
  -------------------

  pk_edx_shared.pr_log
    (p_user_id,
     gv_dml_type,
     p_ip_address,
     p_call_source,
     gv_status,                              -- status: 0 = OK, 1 = BAD
     gv_module_id,
     gv_in_request_params,
     gv_out_trans_values,
     gv_row_count,
     gv_error_message
    );

END pr_udi4_student_images;

PROCEDURE pr_udi4_sia_bulk_images
	(
		p_user_id			IN	VARCHAR2
	,	p_ip_address			IN	VARCHAR2
	,	p_call_source			IN	NUMBER
	,	p_in_call_type			IN	VARCHAR2
	,	p_in_reg_no			IN	VARCHAR2
	,	p_in_photo_image		IN	BLOB
	,	p_in_signature_image		IN	BLOB
	,	p_in_internal_user		IN	VARCHAR2
	,	p_out_student_forenames		OUT	VARCHAR2
	,	p_out_student_surname		OUT	VARCHAR2
	,	p_out_centre_id			OUT	VARCHAR2
	,	p_out_sia_ind			OUT	VARCHAR2
	,	p_out_photo_ind			OUT	VARCHAR2
	,	p_out_signature_ind		OUT	VARCHAR2
	,	p_out_deleted_ind		OUT	VARCHAR2
	,	p_out_certificated_ind		OUT	VARCHAR2
	,	p_out_invalid_reg_no_ind	OUT	VARCHAR2
 	,	p_out_error_message		OUT	VARCHAR2
	)
IS
	student_exist		BOOLEAN;
	valid_student		BOOLEAN;

	v_certificated_ind	VARCHAR2(1)				:=	NULL;
	v_deleted_ind		VARCHAR2(1)				:=	NULL;
	v_existing_photo	BLOB;
	v_existing_signature	BLOB;
	v_photo_in_length	NUMBER					:=	NULL;
	v_photo_missing_ind	VARCHAR2(10)				:=	NULL;
	v_signature_in_length	NUMBER					:=	NULL;
	v_signature_missing_ind	VARCHAR2(10)				:=	NULL;
	v_st_centre_id		STUDENTS.ST_CENTRE_ID%TYPE		:=	NULL;
	v_st_cert_no		STUDENTS.ST_CERT_NO%TYPE		:=	NULL;
	v_st_course_id		STUDENTS.ST_COURSE_ID%TYPE		:=	NULL;
	v_st_delete		STUDENTS.ST_DELETE%TYPE			:=	NULL;
	v_st_forenames		STUDENTS.ST_FORENAMES%TYPE		:=	NULL;
	v_st_surname		STUDENTS.ST_SURNAME%TYPE		:=	NULL;
	v_upload_type		VARCHAR2(1)				:=	NULL;

	CURSOR	c_st
		(
			c_st_reg_no	STUDENTS.ST_REG_NO%TYPE
		)
	IS
		select
			ST_FORENAMES
		,	ST_SURNAME
		,	ST_CENTRE_ID
		from
			STUDENTS
		where
			ST_REG_NO		=	c_st_reg_no
		;

	CURSOR	c_ssca
		(
			c_st_reg_no	STUDENTS.ST_REG_NO%TYPE
		)
	IS
		select
			'Y'
		,	nvl(ST_DELETE,'N')
		,	ST_CERT_NO
		,	dbms_lob.getlength(p_in_photo_image)
		,	dbms_lob.getlength(p_in_signature_image)
		from
			SIA_STUDENT_COLLECTION_AUDIT
		,	STUDENTS
		where
			ST_REG_NO		=	c_st_reg_no
		and	ST_SCHEME_REG_NO	is	NULL
		and	SSCA_ST_REG_NO		=	ST_REG_NO
		union
		select
			'Y'
		,	nvl(ST_DELETE,'N')
		,	ST_CERT_NO
		,	dbms_lob.getlength(p_in_photo_image)
		,	dbms_lob.getlength(p_in_signature_image)
		from
			SIA_STUDENT_COLLECTION_AUDIT
		,	STUDENTS
		where
			ST_SCHEME_REG_NO	=	c_st_reg_no
		and	SSCA_ST_REG_NO		=	ST_REG_NO
		;

BEGIN

	-- Do initial stuff

	gv_module_name	:=	'PR_UDI4_SIA_BULK_IMAGES';

	-- Validate user

	pk_edx_shared.pr_validate_user(p_user_id,p_ip_address);

	-- Validate module, and get dml type (I/Q) and module number (id)

	pk_edx_shared.pr_validate_module(gv_module_name,gv_dml_type,gv_module_id);

	-- Validate call source

	pk_edx_shared.pr_validate_source(p_call_source);

	-- Store input parameters

	gv_in_request_params :=
		'P1 = '||p_in_call_type||' | '||
		'P2 = '||p_in_reg_no||' | '||
		'P5 = '||p_in_internal_user;

	-------------------
	-- PROCESSING START
	-------------------

	----------------------
	-- VALIDATE PARAMETERS
	----------------------

	IF	p_in_call_type	is	NULL
	THEN
		p_out_error_message	:=	'Call type not specified';
	ELSIF	p_in_call_type	not in	(
						'V'
					,	'I'
					)
	THEN
		p_out_error_message	:=	'Invalid call type - must be V or I';
	ELSIF	p_in_reg_no	is	NULL
	THEN
		p_out_error_message	:=	'Registration number not specified';
	ELSIF	p_in_internal_user	is	NULL
	THEN
		p_out_error_message	:=	'Internal user not specified';
	ELSIF	p_in_internal_user	not in	(
						'Y'
					,	'N'
					)
	THEN
		p_out_error_message	:=	'Invalid internal user - must be Y or N';
	END IF;

	---------------------------
	-- RETRIEVE STUDENT DETAILS
	---------------------------

	IF	p_out_error_message	is	NULL
	THEN
		OPEN	c_st
			(
				p_in_reg_no
			);
		FETCH	c_st
		INTO
			v_st_forenames
		,	v_st_surname
		,	v_st_centre_id;
		IF	c_st%NOTFOUND
		THEN
			student_exist	:=	FALSE;
		ELSE
			student_exist	:=	TRUE;
		END IF;
		CLOSE	c_st;

		IF	student_exist
		THEN
			p_out_student_forenames		:=	v_st_forenames;
			p_out_student_surname		:=	v_st_surname;
			p_out_centre_id			:=	v_st_centre_id;
			p_out_invalid_reg_no_ind	:=	'N';

			-----------------------------
			-- CHECK STUDENT ON SIA AWARD
			-----------------------------

			OPEN	c_ssca
				(
					p_in_reg_no
				);
			FETCH	c_ssca
			INTO
				p_out_sia_ind
			,	v_st_delete
			,	v_st_cert_no
			,	v_photo_in_length
			,	v_signature_in_length;
			IF	c_ssca%NOTFOUND
			THEN
				p_out_sia_ind		:=	'N';
				v_st_delete		:=	'N';
				v_st_cert_no		:=	NULL;
				v_photo_in_length	:=	0;
				v_signature_in_length	:=	0;
			END IF;
			CLOSE	c_ssca;

			----------------------------
			-- CHECK STUDENT NOT DELETED
			----------------------------

			IF	v_st_delete	=	'Y'
			THEN
				p_out_deleted_ind	:=	'Y';
			ELSE
				p_out_deleted_ind	:=	'N';
			END IF;

			---------------------------------
			-- CHECK STUDENT NOT CERTIFICATED
			---------------------------------

			IF	v_st_cert_no	is not	NULL
			THEN
				p_out_certificated_ind	:=	'Y';
			ELSE
				p_out_certificated_ind	:=	'N';
			END IF;

			-------------------
			-- CHECK FOR IMAGES
			-------------------

			IF	p_out_sia_ind	=	'Y'
			THEN
				select
					decode	(
							pk_edx_eie_q.fn_get_photo_image_ind
								(
									v_st_course_id
								,	p_in_reg_no
								)
						,	'NO','Y'
						,	'N'
						)
				,	decode	(
							pk_edx_eie_q.fn_get_signature_image_ind
								(
									v_st_course_id
								,	p_in_reg_no
								)
						,	'NO','Y'
						,	'N'
						)
				into
					p_out_photo_ind
				,	p_out_signature_ind
				from
					DUAL;
			ELSE
				p_out_photo_ind		:=	'N';
				p_out_signature_ind	:=	'N';
			END IF;
		ELSE
			p_out_student_forenames		:=	NULL;
			p_out_student_surname		:=	NULL;
			p_out_centre_id			:=	NULL;
			p_out_sia_ind			:=	NULL;
			p_out_photo_ind			:=	NULL;
			p_out_signature_ind		:=	NULL;
			p_out_deleted_ind		:=	NULL;
			p_out_certificated_ind		:=	NULL;
			p_out_invalid_reg_no_ind	:=	'Y';
		END IF;
	END IF;

	IF	p_in_call_type	=	'I'
	THEN
		IF	p_out_error_message	is	NULL
		THEN
			IF	p_out_invalid_reg_no_ind	=	'Y'
			THEN
				p_out_error_message	:=	'Cannot upload images - invalid registration number';
			ELSIF	p_out_sia_ind		=	'N'
			THEN
				p_out_error_message	:=	'Cannot upload images - not an SIA candidate';
			ELSIF	p_out_deleted_ind	=	'Y'
			THEN
				p_out_error_message	:=	'Cannot upload images - candidate has been deleted';
			ELSIF	p_in_internal_user	=	'N'
			THEN
				IF	p_out_certificated_ind	=	'Y'
				THEN
					p_out_error_message	:=	'Centres cannot upload images for certificated candidates';
				ELSIF	(
						(
							v_photo_in_length	>	0
							AND
							p_out_photo_ind		=	'Y'
						)
						OR
						(
							v_signature_in_length	>	0
							AND
							p_out_signature_ind	=	'Y'
						)
					)
				THEN
					p_out_error_message	:=	'Centres cannot use bulk upload for replacing images';
				END IF;
			END IF;
		END IF;

		IF	p_out_error_message	is	NULL
		THEN
			IF	p_in_internal_user	=	'Y'
			THEN
				v_upload_type	:=	'Y';
			ELSE
				v_upload_type	:=	'B';
			END IF;

 	       		--------------
			-- LOAD IMAGES
			--------------
			pr_udi4_student_images
				(
					p_user_id
				,	p_ip_address
				,	p_call_source
				,	p_in_reg_no
				,	'N'			-- p_in_view_photo_image
				,	'N'			-- p_in_view_signature_image
				,	v_upload_type		-- p_in_upload_photo_image
				,	v_upload_type		-- p_in_upload_signature_image
				,	'N'			-- p_in_delete_photo_image
				,	'N'			-- p_in_delete_signature_image
	 			,	p_in_photo_image
				,	p_in_signature_image
				,	NULL			-- p_in_photo_file_name
				,	NULL			-- p_in_signature_file_name
				,	v_existing_photo	-- p_out_photo_image
				,	v_existing_signature	-- p_out_signature_image
				,	p_out_error_message
				);
		END IF;
	END IF;

	-------------------
	-- PROCESSING END
	-------------------

	IF	p_out_error_message	is	NULL
	THEN
		gv_error_message	:=	NULL;
		gv_status		:=	0;
		gv_row_count		:=	1;
		gv_out_trans_values	:=	NULL;
	ELSE
		gv_error_message	:=	p_out_error_message;
		gv_status		:=	1;
		gv_row_count		:=	0;
		gv_out_trans_values	:=	NULL;
	END IF;

	pk_edx_shared.pr_log
		(
			p_user_id
		,	gv_dml_type
		,	p_ip_address
		,	p_call_source
		,	gv_status		-- status: 0 = OK, 1 = BAD
		,	gv_module_id
		,	gv_in_request_params
		,	gv_out_trans_values
		,	gv_row_count
		,	gv_error_message
		);

END pr_udi4_sia_bulk_images;

PROCEDURE pr_udi4_sia_upd_upload_dets
	(
		p_user_id		IN	VARCHAR2
	,	p_ip_address		IN	VARCHAR2
	,	p_call_source		IN	NUMBER
	,	p_in_batch_number	IN	NUMBER
	,	p_in_upload_date	IN	DATE
	,	p_in_reg_no		IN	st_reg_no_tbl
	,	p_out_error_reg_no	OUT	st_reg_no_tbl
	,	p_out_error_message	OUT	VARCHAR2
	)
IS
	i		INTEGER		:=	0;
	j		INTEGER		:=	0;
	v_updated_ind	VARCHAR2(1)	:=	NULL;

	PROCEDURE pr_update_ssca
		(
			p_st_reg_no	IN	VARCHAR2
		,	p_batch_number	IN	NUMBER
		,	p_upload_date	IN	DATE
		,	p_success_ind	OUT	VARCHAR2
		)
	IS
	BEGIN
		update
			SIA_STUDENT_COLLECTION_AUDIT
		set
			SSCA_C_BATCH_NO		=	p_batch_number
		,	SSCA_C_UPLOAD_DATE	=	p_upload_date
		where
			SSCA_ST_REG_NO		=	p_st_reg_no
		;

		IF	SQL%ROWCOUNT	>	0
		THEN
			p_success_ind	:=	'Y';
		ELSE
			p_success_ind	:=	'N';
		END IF;
	END pr_update_ssca;

BEGIN

	-- Do initial stuff

	gv_module_name	:=	'PR_UDI4_SIA_UPD_UPLOAD_DETS';

	-- Validate user

	pk_edx_shared.pr_validate_user(p_user_id,p_ip_address);

	-- Validate module, and get dml type (I/Q) and module number (id)

	pk_edx_shared.pr_validate_module(gv_module_name,gv_dml_type,gv_module_id);

	-- Validate call source

	pk_edx_shared.pr_validate_source(p_call_source);

	-- Store input parameters

	gv_in_request_params :=
		'P1 = '||to_char(p_in_batch_number)||' | '||
		'P2 = '||to_char(p_in_upload_date,'dd/mm/yyyy hh24:mi:ss');

	-------------------
	-- PROCESSING START
	-------------------

	IF	p_in_batch_number	is	NULL
	THEN
		p_out_error_message	:=	'Batch number not specified';
	ELSIF	p_in_upload_date	is	NULL
	THEN
		p_out_error_message	:=	'Upload date not specified';
	END IF;

	i	:=	1;
	j	:=	0;

	BEGIN
		WHILE	(
				p_out_error_message	is	NULL
				AND
				p_in_reg_no(i)		is not	NULL
			)
		LOOP
			pr_update_ssca
				(
					p_in_reg_no(i)
				,	p_in_batch_number
				,	p_in_upload_date
				,	v_updated_ind
				);

			IF	v_updated_ind	=	'N'
			THEN
				j	:=	j + 1;

				p_out_error_reg_no(j)	:=	p_in_reg_no(i);
			END IF;

			i	:=	i + 1;
		END LOOP;

	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			IF	i	=	1
			THEN
				p_out_error_message	:=	'No registration numbers';
			ELSE
				NULL;
			END IF;
	END;

	IF	j	>	0
	THEN
		p_out_error_message	:=	'Not all registration numbers updated';
	END IF;

	-------------------
	-- PROCESSING END
	-------------------

	IF	p_out_error_message	is	NULL
	THEN
		gv_error_message	:=	NULL;
		gv_status		:=	0;
		gv_row_count		:=	(i - 1);
		gv_out_trans_values	:=	NULL;
	ELSE
		gv_error_message	:=	p_out_error_message;
		gv_status		:=	1;
		gv_row_count		:=	j;
		gv_out_trans_values	:=	NULL;
	END IF;

	pk_edx_shared.pr_log
		(
			p_user_id
		,	gv_dml_type
		,	p_ip_address
		,	p_call_source
		,	gv_status		-- status: 0 = OK, 1 = BAD
		,	gv_module_id
		,	gv_in_request_params
		,	gv_out_trans_values
		,	gv_row_count
		,	gv_error_message
		);

END pr_udi4_sia_upd_upload_dets;

PROCEDURE pr_udi4_sia_update_status
	(
		p_user_id		IN	VARCHAR2
	,	p_ip_address		IN	VARCHAR2
	,	p_call_source		IN	NUMBER
	,	p_in_reg_no		IN	st_reg_no_tbl
	,	p_out_error_reg_no	OUT	st_reg_no_tbl
	,	p_out_error_message	OUT	VARCHAR2
	)
IS
	v_in_sub	INTEGER		:= 0;
	v_out_sub	INTEGER		:= 0;

	v_st_reg_no	students.st_reg_no%TYPE;
	v_out_values	module_call_hist.mchi_out_trans_values%type;

BEGIN

	-- Do initial stuff

	gv_module_name := 'PR_UDI4_SIA_UPDATE_STATUS';

	-- Validate user

	pk_edx_shared.pr_validate_user(p_user_id,p_ip_address);

	-- Validate module, and get dml type (I/Q) and module number (id)

	pk_edx_shared.pr_validate_module(gv_module_name,gv_dml_type,gv_module_id);

	-- Validate call source

	pk_edx_shared.pr_validate_source(p_call_source);

	-- Initialise input parameters

	gv_in_request_params := NULL;

	-- Initialise output transaction values

	v_out_values := NULL;

	-------------------
	-- PROCESSING START
	-------------------

	v_in_sub  := 1;
	v_out_sub := 0;

	BEGIN
		WHILE	p_in_reg_no(v_in_sub) is not NULL
		LOOP
			v_st_reg_no := p_in_reg_no(v_in_sub);

			IF	gv_in_request_params is NULL
			THEN
				gv_in_request_params := 'P1 = '||v_st_reg_no;
			ELSE
				IF	length(gv_in_request_params) < 1993
				THEN
					gv_in_request_params := gv_in_request_params||';'||v_st_reg_no;
				END IF;
			END IF;

			update	sia_student_collection_audit
			set	ssca_status = 'C'
			where	ssca_st_reg_no = v_st_reg_no
			and	ssca_status = 'N';

			IF	SQL%ROWCOUNT = 0
			THEN
				v_out_sub := v_out_sub + 1;

				p_out_error_reg_no(v_out_sub) := v_st_reg_no;

				IF	v_out_values is NULL
				THEN
					v_out_values := 'ERRORS = '||v_st_reg_no;
				ELSE
					IF	length(v_out_values) < 1990
					THEN
						v_out_values := v_out_values||';'||v_st_reg_no;
					END IF;
				END IF;
			END IF;

			v_in_sub := v_in_sub + 1;
		END LOOP;

	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			IF	v_in_sub = 1
			THEN
				p_out_error_message := 'No registration numbers';
			ELSE
				NULL;
			END IF;
	END;

	IF	v_out_sub > 0
	THEN
		p_out_error_message := 'Not all registration numbers updated';
	END IF;

	-------------------
	-- PROCESSING END
	-------------------

	IF	p_out_error_message	is NULL
	THEN
		gv_error_message	:= NULL;
		gv_status	 	:= 0;
		gv_row_count		:= (v_in_sub - 1);
	ELSE
		gv_error_message	:= p_out_error_message;
		gv_status		:= 1;
		gv_row_count		:= v_out_sub;
	END IF;

	IF	v_out_values is NULL
	THEN
		v_out_values := 'NO ERRORS';
	END IF;

	gv_out_trans_values := 'A: '||v_out_values;

	pk_edx_eie_udi4.pr_log_autonomous
		(
			p_user_id
		,	gv_dml_type
		,	p_ip_address
		,	p_call_source
		,	gv_status		-- status: 0 = OK, 1 = BAD
		,	gv_module_id
		,	gv_in_request_params
		,	gv_out_trans_values
		,	gv_row_count
		,	gv_error_message
		);

	gv_out_trans_values := 'B: '||v_out_values;

	pk_edx_eie_udi4.pr_log_no_commit
		(
			p_user_id
		,	gv_dml_type
		,	p_ip_address
		,	p_call_source
		,	gv_status		-- status: 0 = OK, 1 = BAD
		,	gv_module_id
		,	gv_in_request_params
		,	gv_out_trans_values
		,	gv_row_count
		,	gv_error_message
		);

	gv_out_trans_values := NULL;

END pr_udi4_sia_update_status;

/**************************************************************/

PROCEDURE pr_get_award_code(l_award_id   IN  VARCHAR2,
                            l_award_code OUT VARCHAR2,
			    l_diag_only  OUT VARCHAR2,
			    l_diag_type  OUT NUMBER)
IS

    ac_code	VARCHAR2(2);
    diag_only   VARCHAR2(1);
    diag_type   NUMBER;

CURSOR get_ac IS
  SELECT aw_award_code,
	 nvl(at_diag_only_ind,'N') diag_only,
	 at_edto_id		   diag_type
    FROM award_titles,
	 approval_application,
	 approval_awards
   WHERE aw_course_number = l_award_id
   AND	 aa_applicat_no   = aw_applicat_no
   AND	 at_number	  = to_number(aa_btec_title);
BEGIN
  FOR v_rec IN get_ac LOOP
        ac_code   := v_rec.aw_award_code;
	diag_only := v_rec.diag_only;
	diag_type := v_rec.diag_type;
  END LOOP;

  l_award_code := ac_code;
  l_diag_only  := diag_only;
  l_diag_type  := diag_type;

EXCEPTION
  WHEN OTHERS THEN
   gv_error_message := 'Error - could not find award code for course'||sqlerrm;
END pr_get_award_code;

PROCEDURE pr_get_est_completion (l_award_id	  IN  VARCHAR2,
			         l_est_compn	  OUT DATE)
IS

    est_compn	DATE;

CURSOR get_ac IS
  SELECT '31-JUL-'||lpad(to_number(to_char(add_months(
	   sysdate,-8),'YY'))+ac_standard_course_length,2,'0')	est_completion
    FROM award_codes,
	 approval_awards
   WHERE ac_code = aw_award_code
   AND   aw_course_number = l_award_id;
BEGIN
  FOR v_rec IN get_ac LOOP
    est_compn := v_rec.est_completion;
  END LOOP;

  l_est_compn := est_compn;

EXCEPTION
  WHEN OTHERS THEN
   gv_error_message := 'Error - calculating estimated completion date'||sqlerrm;
END pr_get_est_completion;

PROCEDURE pr_validate_prog_code(p_programme_code IN VARCHAR2)
IS
BEGIN
  IF gv_error_message IS NULL THEN
    IF pk_edx_shared.fn_validate_qual_type(p_programme_code) = 'N' THEN
       gv_error_message := 'Invalid Qualification Type '||p_programme_code;
    END IF;
  END IF;
END pr_validate_prog_code;

PROCEDURE pr_evidence_route(p_award_id IN nvqs.nvq_id%TYPE,
			    p_applicat_id  IN NUMBER)
IS
	l_erou_code          varchar2(8);

CURSOR check_eroute IS
             SELECT 'X'
             FROM   nvq_evidence_routes
             WHERE  nero_nvq_id = p_award_id;

CURSOR get_eroute IS
             select aero_erou_code
             from   approved_evidence_routes aer
             where  aero_napp_applicat_id = p_applicat_id
             and    aero_seq_no =
                     (select min (aero_seq_no)
                      from   approved_evidence_routes
                      where  aero_napp_applicat_id =
aer.aero_napp_applicat_id);

BEGIN
    	OPEN check_eroute;
	FETCH check_eroute INTO dumvar;
	IF check_eroute%FOUND THEN
	  OPEN  get_eroute;
       	  FETCH get_eroute
	  INTO  l_erou_code;
	  IF get_eroute%NOTFOUND THEN
       	    RAISE_APPLICATION_ERROR(-20000,'Failed to get default
evidence route for NVQ '||p_award_id);
       	  END IF;
	CLOSE check_eroute;
        END IF;

END pr_evidence_route;

PROCEDURE pr_load_units(p_st_reg_no VARCHAR2, p_unit_id VARCHAR2)
IS
BEGIN

  IF SUBSTR(p_unit_id,1,1) = 'U' THEN
    pk_edx_shared3.pr_insert_nvq_stud_comp_unit
      (p_st_reg_no,
       p_unit_id,
       NULL,
       l_error_message
      );
  ELSE
    pk_edx_shared3.pr_insert_student_unit
      (p_st_reg_no,
       p_unit_id,
       NULL,
       NULL,
       l_error_message
      );
  END IF;

END pr_load_units;


PROCEDURE pr_unstring_units
  (p_unit_string IN VARCHAR2,
   p_st_reg_no   IN VARCHAR2,
   p_centre_id   IN VARCHAR2,
   p_forenames   IN VARCHAR2,
   p_surname     IN VARCHAR2,
   p_sex         IN VARCHAR2,
   p_dob         IN DATE,
   p_reg_type    IN VARCHAR2, -- I(insert)/V(alidate)
   l_module_id   IN NUMBER,
   p_franchise	 IN VARCHAR2,
   p_user_id     IN VARCHAR2,
   p_ip_address  IN VARCHAR2,
   p_call_source IN NUMBER,
   p_call_type   IN VARCHAR2,
   p_start_date  IN DATE,
   p_lsc_code	 IN VARCHAR2
  )

  -- NB. p_unit_string = string of units (each 8 chars max) separated by ":"s

IS

  v_colon_posn 		NUMBER;
  v_start_posn 		NUMBER;
  v_length		NUMBER;
  v_length_sub_string	NUMBER;
  v_unit		VARCHAR2(8);
  v_dup_reg_no    	VARCHAR2(7);
  v_dup_cohort    	VARCHAR2(8);
  v_dup_centre_ref 	VARCHAR2(10);
  v_error_message	VARCHAR2(30);

BEGIN
  v_start_posn := 1;
  v_length := LENGTH(p_unit_string);
  v_colon_posn := 0;

  WHILE v_colon_posn < v_length LOOP

    v_colon_posn := INSTR(p_unit_string, ':', v_start_posn);
    IF v_colon_posn = 0 THEN
      v_colon_posn := v_length + 1;
    END IF;

    v_length_sub_string := v_colon_posn - v_start_posn;

    IF v_length_sub_string > 0 THEN
      v_unit :=
        UPPER(SUBSTR(p_unit_string, v_start_posn, v_length_sub_string));
      ---------------------------------
      -- process unstringed unit here
      ---------------------------------
      -- check if there are any nvq units. As this procedure is only called for
      -- btec registrations then this means comb code should be 'C'.

	IF v_length > 8       -- could be at least one nvq unit
	AND v_unit like 'U%'  -- yes there is
	THEN
	    UPDATE students
	      SET  st_reg_comb_code = 'C'
	     WHERE st_reg_no = p_st_reg_no;
        END IF;

      -- Unit validation
      pk_edx_eie_udi4.pr_validate_centre_unit
        (p_centre_id,
         v_unit,
         p_start_date
        );

      -- Franchise validation
	IF p_franchise IS NOT NULL AND
           p_lsc_code IS NOT NULL THEN
	  l_franch_ind := check_franchise_units(p_franchise, v_unit);
	  IF l_franch_ind 	= 'N' THEN
            gv_error_message := 'Invalid Collaborative Partner Unit Id. '||v_unit;
	  END IF;
        END IF;

      -- Check if duplicate registration

        IF gv_error_message IS NULL
  	THEN

        pk_edx_shared.pr_check_duplicate_student
          ('I',                -- I Types
           p_forenames,
           p_surname,
           p_sex,
           p_dob,
           'I',
           NULL,               -- N/A - course_id for BTECs
           p_centre_id,
           v_unit,
           NULL,               -- N/A - NVQ Version
           v_dup_reg_no,
           v_dup_cohort,
           v_dup_centre_ref,
           gv_error_message
          );

        END IF;

      -- Insert units if action is I(nsert)
      IF p_call_type = 'I'
      AND gv_error_message IS NULL THEN
        pk_edx_eie_udi4.pr_load_units
          (p_st_reg_no,
           v_unit
          );
      END IF;
    END IF;
    v_start_posn := v_colon_posn + 1;
  END LOOP;

--  IF p_call_type = 'I' AND
--     gv_error_message IS NULL THEN
--    pk_edx_shared3.pr_update_audit_trail
--      (p_st_reg_no,
--       gv_error_message
--      );
--  END IF;

END pr_unstring_units;

-------------------------------------------------------------------------------
-- RSH 07/02/2006 (RFC 05/0355):
-- Check Phase 1 Approval
-------------------------------------------------------------------------------

PROCEDURE pr_check_phase1_approval (
				    p_centre_id		IN  VARCHAR2,
				    p_award_id		IN  VARCHAR2,
				    p_completion_date	IN  DATE,
				    p_registration_type	IN  VARCHAR2
				   )
IS
    lv_approval_date	DATE
			:= trunc(SYSDATE);
    lv_cohort_date	VARCHAR2(11);
    lv_cohort_return	VARCHAR2(12);
    lv_cohort_valid	VARCHAR2(1);
    lv_group_desc	VARCHAR2(50);
    lv_phase1_return	VARCHAR2(1);
    lv_sector_code	SECTOR_CODES.SECTOR_CODE%TYPE;
    lv_sector_group	VARCHAR2(1);
    lv_sector_return	VARCHAR2(52);
    lv_sector_valid	VARCHAR2(1);

    PROCEDURE pr_get_phase1_sector (
				    p_course_no  IN  VARCHAR2
				   )
    IS
	CURSOR c1 (
		   c_course_no	APPROVAL_AWARDS.AW_COURSE_NUMBER%TYPE
		  )
	IS
	    select
		    nvl(AT_SECTOR_CODE,0)
	    from
		    AWARD_TITLES,
		    APPROVAL_APPLICATION,
		    APPROVAL_AWARDS
	    where
		    AW_COURSE_NUMBER = c_course_no
	    and	    AA_APPLICAT_NO = AW_APPLICAT_NO
	    and	    AT_NUMBER = AA_BTEC_TITLE;

    BEGIN
	OPEN c1 (
		 p_course_no
		);
	FETCH c1
	INTO  lv_sector_code;
	IF (
	    c1%NOTFOUND
	   )
	THEN
	    lv_sector_code := 0;
	END IF;
	CLOSE c1;

    END pr_get_phase1_sector;

BEGIN
    lv_phase1_return := pk_validation.fn_centre_phase1_approval
                                (
                                        p_centre_id,
                                        lv_approval_date
                                );

    IF (
        lv_phase1_return = 'Y'
       )
    THEN
	IF (
	    p_registration_type = 'R'
	   )
	THEN
	    pr_get_phase1_sector (
				  p_award_id
				 );

	    lv_sector_return := pk_validation.fn_centre_phase1_sector
					(
						p_centre_id,
						lv_sector_code
					);

	    lv_sector_valid := substr(lv_sector_return,1,1);
	    lv_sector_group := substr(lv_sector_return,2,1);
	    lv_group_desc   := rtrim(substr(lv_sector_return,3,50));

	    IF (
		lv_sector_valid = 'N'
	       )
	    THEN
		gv_error_message := 'Centre Phase 1 Approval: '	||
				    'Course '			||
				    p_award_id			||
				    ' in unapproved sector - '	||
				    lv_group_desc		||
				    ' ('			||
				    lv_sector_group		||
				    ')';
	    ELSE
		lv_cohort_return := pk_validation.fn_centre_phase1_cohort
					(
						p_centre_id,
						lv_sector_group,
						p_award_id,
						p_completion_date
					);

		lv_cohort_valid := substr(lv_cohort_return,1,1);
		lv_cohort_date  := substr(lv_cohort_return,5,8);

		IF (
		    lv_cohort_valid = 'N'
		   )
		THEN
		    gv_error_message := 'Centre Phase 1 Approval: '	||
					'Completion month '		||
					to_char(p_completion_date,
						'MON-YYYY')		||
					' different to existing '	||
					'cohort month '			||
					lv_cohort_date;
		END IF;
	    END IF;
	ELSE
	    gv_error_message := 'Centre Phase 1 Approval: '	||
				'Invalid type of registration '	||
				p_registration_type;
	END IF;
    END IF;

END pr_check_phase1_approval;


/** 1.16. STUDENT REGISTRATION ***********************************/

PROCEDURE pr_iu4_register_candidate
                (p_user_id              IN  VARCHAR2,
                 p_ip_address           IN  VARCHAR2,
                 p_call_source          IN  NUMBER,
                 p_in_centre_id      	IN  VARCHAR2,
                 p_in_call_type         IN  VARCHAR2,
		 p_in_order_no		IN  VARCHAR2,
		 p_in_enrol_date	IN  DATE,
		 p_in_lsc_code		IN  VARCHAR2,
	    	 p_in_award_code	IN  VARCHAR2,
		 p_in_unit_codes	IN  VARCHAR2,
                 p_in_centre_ref        IN  VARCHAR2,
                 p_in_forenames         IN  VARCHAR2,
                 p_in_last_name         IN  VARCHAR2,
                 p_in_sex               IN  VARCHAR2,
                 p_in_dob               IN  DATE,
                 p_in_completion_date  	IN  DATE,
		 p_in_study_mode	IN  VARCHAR2,
		 p_in_combination	IN  VARCHAR2,
		 p_in_franchise_no	IN  VARCHAR2,
                 p_in_srf_ind		IN  VARCHAR2,
		 p_in_licensed		IN  VARCHAR2,
		 p_in_no_of_licensed_units IN NUMBER,
		 p_in_uln		IN  VARCHAR2,
                 p_in_cohort            IN  VARCHAR2,
		 p_out_error_message	OUT VARCHAR2,
                 p_out_reg_no           OUT students.st_reg_no%TYPE,
		 p_in_received_date	IN  DATE DEFAULT NULL,
		 p_in_internal_user	IN  VARCHAR2 DEFAULT NULL)
IS
         v_st_regno	  VARCHAR2(7)  	:= NULL;
         v_dup_reg_no	  VARCHAR2(7)	:= NULL;
         v_dup_cohort	  VARCHAR2(8)	:= NULL;
         v_dup_centre_ref VARCHAR2(10)	:= NULL;
         v_error_message  VARCHAR2(30) 	:= NULL;
         v_reg_type	  VARCHAR2(1)	:= NULL;
	 r_award_code	  VARCHAR2(2)   := NULL;
	 r_diag_only	  VARCHAR2(1)   := NULL;
	 r_diag_type	  NUMBER	:= NULL;
	 r_est_completion DATE		:= NULL;
         p_previous_regno VARCHAR2(7)   := NULL;

	 diagnostic_only  BOOLEAN	:= FALSE;

PROCEDURE pr_process_r_type
IS
BEGIN

  -- FULL BTEC AWARD REGISTRATION...

  -- Validate data

  pk_edx_eie_udi4.pr_validate_srf_ind(p_in_srf_ind);

  pk_edx_eie_udi4.pr_check_subcentres(l_centre_id);

  pk_edx_eie_udi4.pr_validate_centre(l_centre_id);

  pk_edx_eie_udi4.pr_validate_lsc_code(l_lsc_code);

  pk_edx_eie_udi4.pr_get_award_code(l_award_id, r_award_code, r_diag_only, r_diag_type);

  diagnostic_only := FALSE;

  if r_diag_only       = 'Y'	AND
     r_diag_type  is not null
  then
    diagnostic_only := TRUE;
  end if;

  pk_edx_eie_udi4.pr_get_est_completion(l_award_id, r_est_completion);

  if	l_combination is not null
  then	pk_edx_eie_udi4.pr_validate_comb(l_award_id, l_combination);
  end if;

  pk_edx_eie_udi4.pr_cohort_block(l_centre_id,
				  l_award_id,
				  nvl(p_in_completion_date,r_est_completion));
  -- Allow a NULL study mode if JCQ BTEC Auto-registration...
  -- LQ00060727: Allow blank study mode for all registrations.

  -- IF l_study_mode IS NULL THEN
  --   if p_call_source <> 7 then
  --     gv_error_message := 'R type students must have a study mode';
  --   end if;
  -- END IF;

  IF l_combination IS NULL and nvl(l_licensed,'N') not in ('A','Y')
  THEN
    gv_error_message := 'R type students must have a combination Id';
  END IF;

  pk_edx_eie_udi4.pr_valid_btec_reg
       (l_award_id      ,
        l_centre_id     ,
        p_in_enrol_date ,
        l_error_message ,
	p_in_internal_user
       );

  -- RSH 07/02/2006 (RFC 05/0355):
  -- Ensure that the registration is valid for a phase 1 centre.

  IF (
      gv_error_message IS NULL
     )
  THEN
      pk_edx_eie_udi4.pr_check_phase1_approval
				(
					l_centre_id,
					l_award_id,
					nvl(p_in_completion_date,r_est_completion),
					'R'
				);
  END IF;

  IF p_in_franchise_no IS NOT NULL THEN
    l_franch_ind    := check_franchise(l_franchise_no, l_award_id);
    IF l_franch_ind         = 'N' then
      gv_error_message := 'Invalid Collaborative Partner No.';
    END IF;
  END IF;

  IF gv_error_message IS NULL
  THEN

    IF p_in_call_type = 'I'
    THEN
      IF NOT diagnostic_only
      THEN
        -- get new registration number........
        pk_edx_eie_udi4.pr_validate_regno
           (p_in_enrol_date,l_call_type,v_st_regno);
      ELSE
        v_st_regno := 'VALID';
      END IF;
    END IF;

    IF p_in_award_code <> 'ABS' THEN

  	pk_edx_shared.pr_check_duplicate_student
       ('B',            -- R Type
        l_forenames,
        l_surname,
        l_sex,
        p_in_dob,
        'R',
        l_award_id,
        l_centre_id,
        NULL,   -- N/A NVQ Id
        NULL,   -- N/A NVQ Version
        v_dup_reg_no,
        v_dup_cohort,
        v_dup_centre_ref,
        gv_error_message
       );

    END IF;

  END IF;

  IF p_in_call_type = 'V' AND
     gv_error_message IS NULL THEN
    p_out_reg_no := 'VALID';
    gv_out_trans_values :=
       'Validation Only:BTEC Student Registration '||
       l_surname||' ,'||l_forenames||
       ' ,Award - '||l_award_id;
  END IF;

  IF p_in_call_type = 'I' and NOT diagnostic_only
  THEN
    IF gv_error_message IS NULL
    THEN
      -- Insert to database

      INSERT INTO students
      (st_reg_no,
       st_reg_form_ref,
       st_course_id,
       st_surname,
       st_forenames,
       st_sex,
       st_birth_date,
       st_centre_id,
       st_centre_ref,
       st_commence,
       st_est_completion,
       st_reg_type,
       st_reg_comb_code,
       st_year,
       st_reg_date,
       st_inst_tec_code,
       st_study_mode,
       st_comb_id,
       st_inst_location,
       st_reg_received_date,
       st_srf_reqd,
       ST_EDI_CODE,
       ST_REG_ORIGIN,
       ST_INSERT_USER,
       st_university_ind,
       st_licensed_ind,
       st_uln,
       st_cohort
      )
      VALUES
      (v_st_regno,
       UPPER(rtrim(ltrim(p_in_order_no))),
       l_award_id,
       l_surname,
       l_forenames,
       l_sex,
       p_in_dob,
       l_centre_id,
       l_centre_ref,
       p_in_enrol_date,
       nvl(p_in_completion_date,r_est_completion),
       'R',
       'B',
       decode(l_licensed,'Y',0,'A',0,1),
       TRUNC(SYSDATE),
       l_lsc_code,
       l_study_mode,
       l_combination,
       l_franchise_no,
       nvl(p_in_received_date,SYSDATE),
       decode(l_licensed,'Y',null,'A',null,p_in_srf_ind),
       decode(p_call_source,7,'E',null),
       decode(p_call_source,7,'J',15,'A',17,'A',19,'A','W'),
       p_user_id,
       decode(l_licensed,'Y','Y',null),
       decode(l_licensed,'Y','Y',null),
       l_uln,
       l_cohort
      );

      p_out_reg_no := v_st_regno;

      pr_eie_students_insert(v_st_regno, 'REG');

      gv_out_trans_values :=
         'Insert:BTEC Student Registration '||
         l_surname||' ,'||l_forenames||
         ' ,Award - '||l_award_id||', Regno - '||v_st_regno;

    ELSE

      p_out_reg_no := v_dup_reg_no;

      gv_out_trans_values :=
         'Insert:BTEC Student Registration '||
         l_surname||' ,'||l_forenames||
         ' ,Award - '||l_award_id||' ,Regno - Duplicate';

    END IF;

  END IF;

  IF p_in_call_type = 'I' and diagnostic_only
  THEN
    IF gv_error_message IS NULL
    THEN
      -- Insert to database

        insert into ES_DIAGNOSTIC_TOOL_PURCHASES
        (
	EDTP_ID,
	EDTP_CENTRE_ID,
	EDTP_SURNAME,
	EDTP_FORENAMES,
	EDTP_EDTO_ID,
	EDTP_GENDER,
	EDTP_DOB,
	EDTP_ORIGIN,
	EDTP_INSERT_BY,
	EDTP_INSERT_DATE,
	EDTP_LAST_UPDATE_BY,
	EDTP_LAST_UPDATE_DATE
        )
        values
       (
        edtp_seq.nextval,
	l_centre_id,
        l_surname,
        l_forenames,
        r_diag_type,
        l_sex,
        p_in_dob,
	'P',
	p_user_id,
	sysdate,
	p_user_id,
	sysdate
       );

       gv_out_trans_values :=
         'Diagnostic Tool Purchase '||
         l_surname||' ,'||l_forenames;

    ELSE

       gv_out_trans_values :=
         'Diagnostic Tool Purchase '||
         l_surname||' ,'||l_forenames||' ERROR - '||gv_error_message;

    END IF;

  END IF;

END pr_process_r_type;


PROCEDURE pr_process_i_type
IS
BEGIN

  pk_edx_eie_udi4.pr_validate_srf_ind(p_in_srf_ind);

  pk_edx_eie_udi4.pr_check_subcentres(l_centre_id);

  pk_edx_eie_udi4.pr_validate_centre(l_centre_id);

  pk_edx_eie_udi4.pr_validate_lsc_code(l_lsc_code);

  -- RSH 07/02/2006 (RFC 05/0355):
  -- Ensure that the registration is valid for a phase 1 centre.

  IF (
      gv_error_message is NULL
     )
  THEN
      pk_edx_eie_udi4.pr_check_phase1_approval
				(
					l_centre_id,
					l_award_id,
					p_in_completion_date,
					'I'
				);
  END IF;

  IF nvl(l_licensed,'N') = 'Y' and
     gv_error_message is NULL
  THEN
    if p_in_unit_codes is not null
    then
      gv_error_message := 'Specific unit codes should not be supplied for licensed registrations';
    end if;
    if p_in_no_of_licensed_units not between 1 and 99
    then
      gv_error_message := 'Number of licensed units must be between 1 and 99';
    end if;
  END IF;

  IF p_in_call_type = 'V' AND
     gv_error_message IS NULL THEN
    IF p_in_unit_codes IS NOT NULL THEN
      pk_edx_eie_udi4.pr_unstring_units
               (l_units,
                v_st_regno,
                l_centre_id,
                l_forenames,
                l_surname,
                l_sex,
                p_in_dob,
                'V',
                l_module_id ,
                l_franchise_no,
                p_user_id   ,
                p_ip_address,
                p_call_source,
                l_call_type,
                p_in_enrol_date,
                p_in_lsc_code
               );
    else -- EC2604
     --
     gv_error_message := 'Potential I type registration - but no units however / possible missing course number';
     --
    END IF;
    IF gv_error_message IS NULL THEN
      p_out_reg_no := 'VALID';
      gv_out_trans_values :=
        'Validation Only:Itype Student '||l_surname||' ,'||
        l_forenames||' ,Regno = '||v_st_regno;
    END IF;
  END IF;

  IF p_in_call_type = 'I'	AND
     gv_error_message IS NULL
  THEN
    -- Get new reg no
    pk_edx_eie_udi4.pr_validate_regno(p_in_enrol_date,l_call_type,v_st_regno);
    INSERT INTO students
               (st_reg_no,
                st_reg_form_ref,
                st_surname,
                st_forenames,
                st_sex,
                st_birth_date,
                st_centre_id,
                st_centre_ref,
                st_commence,
                st_est_completion,
                st_reg_type,
                st_reg_comb_code,
                st_year,
                st_reg_date,
                st_inst_tec_code,
                st_study_mode,
                st_comb_id,
                st_inst_location,
                st_reg_received_date,
                st_srf_reqd,
	       	ST_REG_ORIGIN,
       		ST_INSERT_USER,
	        st_university_ind,
       	 	st_licensed_ind,
		st_uln,
                st_cohort)
        VALUES
               (v_st_regno,
                UPPER(rtrim(ltrim(p_in_order_no))),
                l_surname,
                l_forenames,
                l_sex,
                p_in_dob,
                l_centre_id,
                l_centre_ref,
                p_in_enrol_date,
                p_in_completion_date,
                DECODE(UPPER(p_in_award_code),'','I','R'),
                'B',
	        decode(l_licensed,'Y',0,1),
                TRUNC(SYSDATE),
                l_lsc_code,
                l_study_mode,
                l_combination,
                l_franchise_no,
                sysdate,
	        decode(l_licensed,'Y',null,p_in_srf_ind),
	        'W',
       		p_user_id,
	        decode(l_licensed,'Y','Y',null),
	        decode(l_licensed,'Y','Y',null),
		l_uln,
                l_cohort
		);

    p_out_reg_no := v_st_regno;

    IF nvl(l_licensed,'N') <> 'Y'
    THEN
      IF p_in_unit_codes IS NOT NULL
      THEN
        pk_edx_eie_udi4.pr_unstring_units
               (l_units,
                v_st_regno,
                l_centre_id,
                l_forenames,
                l_surname,
                l_sex,
                p_in_dob,
                'I',
                l_module_id ,
                l_franchise_no,
                p_user_id   ,
                p_ip_address,
                p_call_source,
                l_call_type,
                p_in_enrol_date,
                p_in_lsc_code
               );
      END IF;
    ELSE
      -- Licensed Itype Registration...
      for i in 1..p_in_no_of_licensed_units
      loop
	insert into student_units
	      (su_reg_no,
	       su_unit_id
	      )
	select v_st_regno,
	       'Z'||to_char(99999-i+1)
	from   dual;
      end loop;
    END IF;

    IF gv_error_message IS NULL
    THEN
      pr_eie_students_insert(v_st_regno, 'REG');
      gv_out_trans_values :=
        'INSERT itype Student '||l_surname||' ,'||
        l_forenames||' ,Regno = '||v_st_regno;
    END IF;
  END IF;

END pr_process_i_type;

BEGIN

  -- Do initial stuff

  gv_module_name := 'PR_IU4_REGISTER_CANDIDATE';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  gv_in_request_params :=
                'P1 = '||p_in_centre_id||' | '||
                'P2 = '||p_in_call_type||' | '||
                'P3 = '||p_in_order_no||' | '||
                'P4 = '||p_in_enrol_date||' | '||
                'P5 = '||p_in_lsc_code||' | '||
                'P6 = '||p_in_award_code||' | '||
                'P7 = '||p_in_unit_codes||' | '||
                'P8 = '||p_in_centre_ref||' | '||
                'P9 = '||p_in_forenames||' | '||
                'P10 = '||p_in_last_name||' | '||
                'P11 = '||p_in_sex||' | '||
                'P12 = '||p_in_dob||' | '||
                'P13 = '||p_in_completion_date||' | '||
                'P14 = '||p_in_study_mode||' | '||
                'P15 = '||p_in_combination||' | '||
		'P16 = '||p_in_franchise_no||' | '||
		'P17 = '||p_in_licensed||' | '||
		'P18 = '||to_char(p_in_no_of_licensed_units)||' | '||
		'P19 = '||p_in_uln||'|'||
                'P20 = '||p_in_cohort;

  -- Standardise parameters

        v_database := null;

        l_award_id      := UPPER(RTRIM(LTRIM(p_in_award_code)));
        l_user_id       := UPPER(RTRIM(LTRIM(p_user_id)));
        l_centre_id     := UPPER(RTRIM(LTRIM(p_in_centre_id)));
        l_call_type     := UPPER(RTRIM(LTRIM(p_in_call_type)));
        l_centre_ref    := UPPER(RTRIM(LTRIM(p_in_centre_ref)));
        l_surname       := UPPER(RTRIM(LTRIM(p_in_last_name)));
        l_forenames     := UPPER(RTRIM(LTRIM(p_in_forenames)));
        l_sex           := UPPER(RTRIM(LTRIM(p_in_sex)));
	l_study_mode	:= UPPER(RTRIM(LTRIM(p_in_study_mode)));
	l_combination	:= UPPER(RTRIM(LTRIM(p_in_combination)));
	l_franchise_no	:= UPPER(RTRIM(LTRIM(p_in_franchise_no)));
	l_lsc_code      := UPPER(RTRIM(LTRIM(p_in_lsc_code)));
	l_prog_no	:= UPPER(RTRIM(LTRIM(p_in_award_code)));
	l_units		:= UPPER(RTRIM(LTRIM(p_in_unit_codes)));
	l_licensed	:= UPPER(RTRIM(LTRIM(p_in_licensed)));
	l_uln		:= UPPER(RTRIM(LTRIM(p_in_uln)));
        l_cohort        := UPPER(RTRIM(LTRIM(p_in_cohort)));

  -- PROCESSING START

	gv_error_message := NULL;

	if nvl(l_licensed,'N') in ('A','Y')
	then
	  l_study_mode  := 'A';
	  l_combination := NULL;
	end if;

    	l_record_count  	:= 0;
 	dumvar          	:= NULL;
 	l_franch_ind    	:= NULL;
 	l_module_id     	:= 0;
 	l_sequence_id   	:= 0;
 	l_call_status   	:= 1;
 	l_reg_type      	:= NULL;
        l_dml_type      	:= NULL;
 	l_in_request_params	:= NULL;
 	l_out_trans_values      := NULL;
 	l_commence_date         := NULL;
 	l_est_completion        := NULL;
        gv_lsc_error    	:= null;

  IF p_in_award_code IS NOT NULL THEN
    pr_process_r_type;
  ELSE
    pr_process_i_type;
  END IF;

  IF gv_error_message IS NULL THEN
    gv_status := 0;
    i := 1;
  ELSE
    IF p_in_call_type = 'I' THEN
      ROLLBACK;
    END IF;
    gv_status := 1;
    i := 0;
  END IF;

  p_out_error_message := gv_error_message;

  -- EC2589
  IF p_in_call_type = 'I' AND gv_error_message IS NULL THEN commit; end if;

  pk_edx_shared.pr_log
    (p_user_id,
     gv_dml_type,
     p_ip_address,
     p_call_source,
     gv_status,                     -- status: 0 = OK, 1 = BAD
     gv_module_id,
     gv_in_request_params,
     gv_out_trans_values,
     i,                              -- row count
     gv_error_message                -- error message
    );

  COMMIT;

  IF p_in_call_type = 'I' THEN
    IF gv_error_message IS NULL THEN
-- WI842
      IF r_award_code = 'CC' AND NOT fn_ks_abs_process_expired('ESOL','R',trunc(sysdate)) THEN

        p_previous_regno := v_st_regno;

        -- Automatic ABS registration so get another regno.

        pk_edx_eie_udi4.pr_validate_regno
        (p_in_enrol_date,l_call_type,v_st_regno);

        pk_edx_eie_udi4.pr_auto_abs_approve('ABS',l_centre_id);

        INSERT INTO students
        (st_reg_no,
         st_reg_form_ref,
         st_course_id,
         st_surname,
         st_forenames,
         st_sex,
         st_birth_date,
         st_centre_id,
         st_centre_ref,
         st_commence,
         st_est_completion,
         st_reg_type,
         st_reg_comb_code,
         st_year,
         st_reg_date,
         st_inst_tec_code,
         st_study_mode,
         st_student_worth,
         st_invo_id,
         st_comb_id,
         st_inst_location,
         st_reg_received_date,
         st_srf_reqd,
         ST_REG_ORIGIN,
         ST_INSERT_USER,
         st_esol_abs_reg,
         st_uln,
         st_cohort
        )
        VALUES
        (v_st_regno,
         UPPER(rtrim(ltrim(p_in_order_no))),
         'ABS',
         l_surname,
         l_forenames,
         l_sex,
         p_in_dob,
         l_centre_id,
         l_centre_ref,
         p_in_enrol_date,
         p_in_completion_date,
         'R',
         'B',
         1,
         TRUNC(SYSDATE),
         l_lsc_code,
         l_study_mode,
         0,
         'ZKSQ',
         'A',
         l_franchise_no,
         SYSDATE,
         p_in_srf_ind,
         decode(p_call_source,7,'J','W'),
         p_user_id,
         p_previous_regno,
         l_uln,
         l_cohort
        );

        pr_eie_students_insert(v_st_regno, 'REG');

        gv_in_request_params :=
                'P1 = '||p_in_centre_id||' | '||
                'P2 = '||p_in_call_type||' | '||
                'P3 = '||p_in_order_no||' | '||
                'P4 = '||p_in_enrol_date||' | '||
                'P5 = '||p_in_lsc_code||' | '||
                'P6 = '||'ABS -Auto Reg'||' | '||
                'P7 = '||p_in_unit_codes||' | '||
                'P8 = '||p_in_centre_ref||' | '||
                'P9 = '||p_in_forenames||' | '||
                'P10 = '||p_in_last_name||' | '||
                'P11 = '||p_in_sex||' | '||
                'P12 = '||p_in_dob||' | '||
                'P13 = '||p_in_completion_date||' | '||
                'P14 = '||p_in_study_mode||' | '||
                'P15 = '||p_in_combination||' | '||
		'P16 = '||p_in_franchise_no||' | '||
                'P20 = '||p_in_cohort;

        gv_out_trans_values :=
         'Insert:BTEC Auto ABS Registration '||
         l_surname||' ,'||l_forenames||
         ' ,Award - '||l_award_id||' ,Regno - '||v_st_regno;

	-- EC2589
	   commit;

        pk_edx_shared.pr_log
        (p_user_id,
         gv_dml_type,
         p_ip_address,
         p_call_source,
         gv_status,                     -- status: 0 = OK, 1 = BAD
         gv_module_id,
         gv_in_request_params,
         gv_out_trans_values,
         i,                              -- row count
         gv_error_message                -- error message
         );

        COMMIT;

      END IF;

    ELSE

      IF p_call_source = 1 THEN

        p_out_error_message := NULL;

      END IF;

    END IF;

  END IF;

END pr_iu4_register_candidate;
-------------------------------
-- EC2752 same as STV270
-------------------------------
PROCEDURE delete_registration
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_prog_code               IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2)
IS
--VARIABLES
v_reg_no   students.st_reg_no%type := null;
v_course_id varchar2(8)  := null;
v_output_string     VARCHAR2(400) := null;

-- WHEN-VALIDATE-ITEM
-- IATR_ST_REG_NO (p_in_reg_no)
 n_scheme_id       schemes.scheme_id%type;
 v_scheme_reg_no   students.st_scheme_reg_no%type;
 v_st_reg_no       students.st_reg_no%type;
 v_st_prog_no      VARCHAR2 (8);
 d_reg_rcvd	   date;
 v_scheme_ind      VARCHAR2(1);
 v_non_contig_ind  VARCHAR2(1);
 n_buni_id        business_units.buni_id%type;
 v_type           VARCHAR2(1);

 cursor  c_work_cursor (cp_reg_no students.st_reg_no%type)
 is
 SELECT  st_scheme_reg_no,
         st_scheme_id,
         decode(st_reg_type,'I','I',
          decode(st_course_id,NULL,decode(st_nvq_registered_id,null,'G','N'),'B')),
         nvl(st_course_id,nvl(st_nvq_registered_id,nvl(st_gnvq_registered_id,'I-Type'))),
         st_reg_received_date,
         cn_bure_buni_id
 FROM    BTEC.STUDENTS
 ,       BTEC.CENTRES
 WHERE   CN_CENTRE_ID = ST_CENTRE_ID
and to_char(sysdate,'MON')  in ('SEP','OCT','NOV','DEC','JAN')
and to_char(st_reg_date,'MON')  in ('SEP','OCT','NOV','DEC','JAN')
and trunc(add_months(st_reg_date,-1),'YEAR')=trunc(add_months(sysdate,-1),'YEAR')
AND  ST_REG_NO = cp_reg_no;

 cursor c_scheme (cp_reg_no students.st_reg_no%type)  is
   select 'x'
   from   students
   where  st_reg_no = cp_reg_no
   and    st_scheme_reg_no is not null;

 v_scheme  VARCHAR2(1);

 cursor c_top_up (cp_reg_no students.st_reg_no%type) is
 select	'Y'
 from	bnm_topup_students
 where	btst_st_reg_no = cp_reg_no;

   v_topup_ind varchar2(1);

 cursor c_topup_uninvoiced (cp_reg_no students.st_reg_no%type) is
 select 'x'
 from dual
 where   EXISTS (select  NULL
                 from	bnm_topup_students
  		 where	btst_st_reg_no = cp_reg_no
  		 and    btst_invo_id   is null);

   v_topup_uninvoiced varchar2(1);

  cursor c_university (cp_reg_no students.st_reg_no%type) is
  select 'Y'
   from   btec.STUDENTS
   where  ST_SCHEME_REG_NO = cp_reg_no
   and    ST_UNIVERSITY_IND = 'Y'
   and    nvl(ST_LICENSED_IND,'N') = 'N';

   v_university varchar2(1);

   cursor c_already_deleted (cp_reg_no students.st_reg_no%type) is
    select   'x'
    from     BTEC.STUDENTS
    where    st_reg_no = cp_reg_no
    and      ST_DELETE = 'Y'
    union
    select   st_reg_no
    from     BTEC.STUDENTS
    where    ST_SCHEME_REG_NO = cp_reg_no
    and      ST_DELETE = 'Y';

    v_already_deleted varchar2(1);

   cursor  c_certified (cp_reg_no students.st_reg_no%type)
   is
   select  'x'
   from    students
   where   st_reg_no = cp_reg_no
   and  (st_cert_no             is not null or
         st_gnvq_certificate_no is not null or
         st_nvq_certificate_no  is not null)
 union
 select  st_reg_no
 from    students
 where   st_scheme_reg_no = cp_reg_no
 and  (st_cert_no             is not null or
       st_gnvq_certificate_no is not null or
       st_nvq_certificate_no  is not null);

   v_certified varchar2(1);

 cursor  c_sat_gnvq_test (cp_reg_no students.st_reg_no%type) is
 select  distinct 'x'
 from    btec.gnvq_student_test_passes_view
 where   gstp_st_reg_no = cp_reg_no;

 v_sat_gnvq_test varchar2(1);

   CURSOR c_got_test (cp_reg_no students.st_reg_no%type) IS
   SELECT 'Y'
   FROM   BTEC.ASS_EXAMINATIONS
   WHERE  EXA_ST_REG_NO = cp_reg_no;

   v_got_test varchar2(1) := null;

    v_valid_bracs_user varchar2(1) := null;

   -- External Assessment
   cursor c_external  (cp_reg_no students.st_reg_no%type)
   is
   SELECT 'x'
   FROM DUAL
   WHERE EXISTS (
      select 'x'
      from prom_btec_ass_bookings,
            ASSESSMENTS,
            ASSESSMENT_UNITS,
            UNITS
      where PBAA_CATALOGUE_NAME = ASSE_CATALOGUE_NAME
      AND ASSE_ID = AUNI_ASSE_ID
      AND AUNI_UN_UNIT_CODE = UN_UNIT_CODE
      AND PBAA_ST_reg_no = cp_reg_no
      and un_type = '3');

   v_external varchar2(1) := null;

   cursor c_ac_code (cp_prog_no VARCHAR2)
   is
   select	ac_code
   from		award_titles,
 		award_codes,
 		approval_application,
 		approval_awards
   where        at_number = to_number(aa_btec_title)
   and		ac_code	  = aw_award_code||''
   and		aa_applicat_no	= aw_applicat_no+0
   and		aw_course_number = cp_prog_no;

   v_award_code award_codes.ac_code%type;

   -- Pending Results (SARE,SU)
   cursor c_units_results (cp_reg_no students.st_reg_no%type) is
   select 'x'
   from student_assessment_results
   where sare_st_reg_no = cp_reg_no
   and   sare_aout_id not in ('A','V')
   union
   SELECT 'x'
   FROM   STUDENT_UNITS
   WHERE  SU_REG_NO = CP_REG_NO
   AND   NVL(SUBSTR(SU_GRADE_5||SU_GRADE_4||SU_GRADE_3||SU_GRADE_2||SU_GRADE_1,1,1),'*')
          IN ('P','M','D','N','U')
   AND NOT EXISTS ( SELECT NULL
	            FROM   NVQ_STUDENT_COMPETENCE_UNITS
                    WHERE  NSCU_ST_REG_NO = CP_REG_NO
                    AND    NSCU_ACHIEVED_YEAR IS NOT NULL
                                                  );
    v_units_results varchar2(1) := null;

 --ON-INSERT trigger
 CURSOR  c_student_delete (cp_reg_no students.st_reg_no%type,
                           cp_non_contig varchar2) IS
 SELECT  st_reg_no,
         st_scheme_reg_no,
         st_scheme_id,
         decode(st_course_id, 'KSQ00', 0, st_student_worth)
         st_student_worth,
         st_invo_id,
         decode(decode(st_srco_code,  '',  st_invo_id,  ''),  '',  'P',  'G')   invoice_state,
         st_course_id
 FROM    students
 WHERE   st_reg_no = cp_reg_no
   AND   NVL(st_delete,  'N') != 'Y'
   AND   st_cert_no IS NULL
   AND   st_ks_unit_cert_no IS NULL
   AND   nvl(st_university_ind,'N')||nvl(st_licensed_ind,'N') in ('NN','YY')
   AND   nvl(cp_non_contig,'N') <> 'Y'
   UNION
   SELECT  st_reg_no,
         st_scheme_reg_no,
         st_scheme_id,
         decode(st_course_id, 'KSQ00', 0, st_student_worth)
         st_student_worth,
         st_invo_id,
         decode(decode(st_srco_code,  '',  st_invo_id,  ''),  '',  'P',  'G')   invoice_state,
         st_course_id
 FROM    students
 WHERE   st_reg_no = cp_reg_no
   AND   NVL(st_delete,  'N') != 'Y'
   AND   st_cert_no IS NULL
   AND   st_ks_unit_cert_no IS NULL
   AND   nvl(st_university_ind,'N')||nvl(st_licensed_ind,'N') in ('NN','YY')
   AND   nvl(cp_non_contig,'N') = 'Y';

 CURSOR  c_pending (cp_reg_no VARCHAR2 )
 IS
 SELECT  DECODE(COUNT(iatr_st_reg_no),  0,  'N',  'Y')
 FROM    invoice_audit_trails
 WHERE   iatr_st_reg_no = cp_reg_no
 AND   iatr_adjustment_type = 'P';

 CURSOR  c_topup (cp_reg_no VARCHAR2 ) IS
 SELECT  btst_fee, btst_invo_id
 FROM    bnm_topup_students
 WHERE   btst_st_reg_no = cp_reg_no
 ORDER BY btst_btop_id desc;


 v_topup_fee       NUMBER(8,4);
 v_invoice	   VARCHAR2(6);

 cv_non_contig_ind varchar2(1) := null;

-- FUNCTIONS
--==========
FUNCTION	FN_NON_CONTIG_SCHEME(p_scheme_reg_no VARCHAR2)
RETURN		VARCHAR2
IS
	v_non_contig_ind	VARCHAR2(1)	:=	NULL;
	v_scheme_reg_no		VARCHAR2(7)	:=	NULL;
	v_min_st_reg_no		VARCHAR2(7)	:=	NULL;
	v_max_st_reg_no		VARCHAR2(7)	:=	NULL;

	CURSOR	c_scheme
	IS
	  select
		ST_SCHEME_REG_NO
	,	min(ST_REG_NO)		MIN_ST_REG_NO
	,	max(ST_REG_NO)		MAX_ST_REG_NO
	from    STUDENTS
	where   ST_SCHEME_REG_NO	=	p_scheme_reg_no
	group by
		ST_SCHEME_REG_NO;

	CURSOR	c_students(c_st_low VARCHAR2,c_st_high VARCHAR2)
	IS
	  select
		nvl(ST_SCHEME_REG_NO,ST_REG_NO)	SCHEME_ST_REG_NO
	  from STUDENTS
	  where ST_REG_NO between c_st_low and c_st_high
	  order by ST_REG_NO;

	r_students	c_students%ROWTYPE;

BEGIN
	v_non_contig_ind	:=	'N';

	OPEN	c_scheme;
	FETCH	c_scheme
	INTO	v_scheme_reg_no
			,	v_min_st_reg_no
			,	v_max_st_reg_no;
	IF	c_scheme%NOTFOUND
	THEN
		v_scheme_reg_no	:= NULL;
	END IF;

	CLOSE	c_scheme;

	IF v_scheme_reg_no is not NULL
	THEN
	   OPEN  c_students(v_min_st_reg_no,
		            v_max_st_reg_no);
	   LOOP
		FETCH c_students INTO r_students;
		EXIT WHEN c_students%NOTFOUND OR v_non_contig_ind = 'Y';

                IF r_students.SCHEME_ST_REG_NO	<> v_scheme_reg_no
		THEN
		   v_non_contig_ind := 'Y';
		END IF;
	   END LOOP;
	   CLOSE c_students;
	END IF;

	RETURN	v_non_contig_ind;
END   FN_NON_CONTIG_SCHEME;

--MAIN
BEGIN

  -- Do initial stuff

  gv_module_name := 'DELETE_REGISTRATION';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

   gv_in_request_params :=
                'P1 = '||p_in_reg_no||' | '||
                'P2 = '||p_in_prog_code;

   -- Standardise parameters

        l_user_id       := UPPER(RTRIM(LTRIM(p_user_id)));
        v_course_id     := UPPER(RTRIM(LTRIM(p_in_prog_code)));
        v_reg_no        := UPPER(RTRIM(LTRIM(p_in_reg_no)));

   -- PROCESSING START

        gv_error_message := NULL;
        gv_reg_no        := NULL;
        gv_record_count  := 0;


    -- 1.Validate Candidate, date, etc...
    if gv_error_message is null then

         OPEN  c_work_cursor (v_reg_no);
         FETCH   c_work_cursor INTO
                v_scheme_reg_no,
                n_scheme_id,
                v_type,
                v_st_prog_no,
                d_reg_rcvd,
                n_buni_id;

         if  c_work_cursor%NOTFOUND then
           gv_error_message := 'Student reg No. '||v_reg_no||' is not valid';
         end if;
         close c_work_cursor;
      end if;

      -- 2. Scheme Student
      if gv_error_message is null then

          open  c_scheme (v_reg_no);
          fetch c_scheme into  v_scheme;
          close c_scheme;

          if v_scheme is not null
          then
            gv_reg_no        :=  v_reg_no;
            gv_error_message := 'Scheme registrations can only be deleted via your Pearson contact';
          end if;
      end if;

      --3. Checking for external assessment
      if gv_error_message is null then
          open c_external(v_reg_no);
          fetch c_external into v_external;
          close c_external;

          if v_external is not null then
            gv_error_message := 'Student reg No. '||v_reg_no||' booked external assessment';
          end if;
      end if;

      -- 3. Candidate has results or units...
      if gv_error_message is null then

                open c_units_results (v_reg_no);
                fetch c_units_results into v_units_results;
                close c_units_results;

                if v_units_results is not null
                then
                    gv_error_message := 'Student cannot be deleted...result(s)/unit(s) exist';
                end if;
       end if;

       -- Get award code
       if gv_error_message is null then
              if v_type = 'B' then
                 open c_ac_code (v_st_prog_no);
                 fetch c_ac_code into v_award_code;
                 close c_ac_code;
              end if;
       end if;

       -- Check TopUp Uninvoiced
       if gv_error_message is null
       then
           if v_type = 'B' or v_type = 'N'
           then
             open c_top_up (v_reg_no);
             fetch c_top_up into v_topup_ind;
             close c_top_up;

             if v_topup_ind = 'Y'
	     then
              open c_topup_uninvoiced (v_reg_no);
              fetch c_topup_uninvoiced  into v_topup_uninvoiced;
              close c_topup_uninvoiced;

              if v_topup_uninvoiced is not null then
                  gv_error_message := 'Cannot Delete A Un-Invoiced Top-Up Student';
              end if;
             end if;
           end if;
       end if;

       -- University Student
       if gv_error_message is null then
          open c_university (v_reg_no);
          fetch c_university into v_university;
          close c_university;

           if v_university is not null then
               gv_error_message := 'This is a University Student Enrolment, deletion not possible here';
           end if;
       end if;

       if gv_error_message is null then
           open c_already_deleted(v_reg_no);
           fetch c_already_deleted into v_already_deleted;
           close c_already_deleted;

           if v_already_deleted is not null then
                gv_error_message := 'Student Registration '||
                                     v_reg_no||
                                   ' has ALREADY been DELETED';
           end if;
       end if;

       -- Certificated
       if gv_error_message is null then
          open c_certified (v_reg_no);
          fetch c_certified into v_certified;
          close c_certified;

          if v_certified is not null
          then
             gv_error_message := 'Student '||v_reg_no||' has received a certificate';
          end if;
       end if;

       -- 90 days rule
       if gv_error_message is null then
            if sysdate >= (d_reg_rcvd + 90)
            then
              if n_buni_id = 0 then
                 gv_error_message := 'Registration date '||d_reg_rcvd||' exceeds 90 day deletion rule';
    	      end if;
            end if;
       end if;

       -- PRE-INSERT trigger
       if gv_error_message is null then
           OPEN  c_got_test (v_reg_no);
           FETCH c_got_test INTO v_got_test;
           CLOSE c_got_test;

           if v_got_test is not null then
               gv_error_message := 'Student has taken GNVQ tests and should not be deleted';
           end if;
       end if;

    -- DELETE
    if gv_error_message is null then
        v_non_contig_ind	:=	null;
	v_scheme_reg_no		:=	null;
       	cv_non_contig_ind := fn_non_contig_scheme(v_reg_no);

      FOR delete_rec IN c_student_delete (v_reg_no,cv_non_contig_ind) LOOP

          OPEN    c_pending(delete_rec.st_reg_no);
          FETCH   c_pending  INTO    v_already_deleted;
          CLOSE   c_pending;

          IF  v_already_deleted = 'N'
          THEN
            IF  v_non_contig_ind IS NULL
            THEN
              IF  delete_rec.st_scheme_reg_no IS NOT NULL
              THEN
                  v_non_contig_ind := fn_non_contig_scheme(delete_rec.st_scheme_reg_no);
              ELSE
             	  v_non_contig_ind := 'N';
              END IF;

              v_scheme_reg_no	:=	delete_rec.st_scheme_reg_no;
            END IF;

            IF	(v_non_contig_ind = 'N'
		 OR
		 (v_non_contig_ind = 'Y'
		  AND
		  delete_rec.st_scheme_reg_no = v_scheme_reg_no
		 )
	        )
	    THEN
		IF  delete_rec.st_scheme_reg_no IS NULL
         	THEN
                    delete_rec.st_scheme_reg_no  :=  delete_rec.st_reg_no;
         	END IF;
		    v_topup_fee := -1;

		    if	v_topup_ind = 'Y' then
	     		OPEN    c_topup(delete_rec.st_reg_no);
  	   		FETCH   c_topup INTO    v_topup_fee, v_invoice;
                        IF c_topup%NOTFOUND
    	 		THEN
    	 	            v_topup_fee := -1;
    	 		END IF;
                        CLOSE   c_topup;
		    end if;

        	    INSERT INTO invoice_audit_trails
            		        (IATR_ID,
                                 IATR_ST_REG_NO,
                		 IATR_ADJUSTMENT_TYPE,
                    		 IATR_ADJUSTMENT_DATE,
                                 IATR_INVOICE_STATE,
                     		 IATR_NEW_STUDENT_WORTH,
                                 IATR_OLD_STUDENT_WORTH,
                     		 IATR_CREDITED_INVO_ID,
				 IATR_DML_USER)
         		VALUES    (invoice_audit_trail_seq_no.nextval,
                                   delete_rec.st_reg_no,
                                   'P',
                                   trunc(SYSDATE),
                                   decode(delete_rec.st_scheme_reg_no,delete_rec.st_reg_no,delete_rec.invoice_state,'P'),
                                   0,
                                   decode(v_topup_fee,-1,delete_rec.st_student_worth,v_topup_fee),
                                   decode(v_topup_fee,-1,delete_rec.st_invo_id,v_invoice),
			           substr(p_user_id,1,30)
				  );
	    END IF;
          ELSE
             gv_error_message := 'Student '||delete_rec.st_reg_no||' is already PENDING deletion';
          END IF;
     END LOOP;
   end if;

   -- THE END
   if gv_error_message is not null
   then
      p_out_error_message := gv_error_message;
   end if;

END delete_registration;
-------------------------------
-- Validate Onscreen Candidates
-------------------------------
PROCEDURE pr_iu4_validate_ons_candidate
               (p_user_id               IN   VARCHAR2,
                p_ip_address            IN   VARCHAR2,
                p_call_source           IN   NUMBER,
                p_in_centre             IN  VARCHAR2,
                p_in_qual_code          IN  VARCHAR2,
                p_in_programme_code     IN  VARCHAR2,
                p_in_reg_no             IN  VARCHAR2,
                p_in_forenames          IN  VARCHAR2,
                p_in_lastname           IN  VARCHAR2,
                p_in_dob                IN  DATE,
                p_in_gender             IN  VARCHAR2,
                p_out_error_message     OUT VARCHAR2,
                p_out_reg_no            OUT students.st_reg_no%TYPE,
                p_out_registration_required OUT VARCHAR2)
IS

 -- Check if reg no in the file matches
 -- existing student record
 -- ===================================
    CURSOR c_cand_exists (cp_reg_no students.st_reg_no%type,
                          cp_course_id students.st_course_id%type)
    IS
      select nvl(st_scheme_reg_no,st_reg_no)
      ,      st_surname
      ,      st_forenames
      ,      st_birth_date
      ,      st_centre_id
      ,      st_course_id
      ,      st_sex
      from   students
      where  st_reg_no = cp_reg_no
      and    st_delete      IS NULL
      and    st_expired_ind IS NULL
      and    st_fallback    IS NULL
      UNION
       select nvl(st_scheme_reg_no,st_reg_no)
      ,      st_surname
      ,      st_forenames
      ,      st_birth_date
      ,      st_centre_id
      ,      st_course_id
      ,      st_sex
      from   students
      where  st_scheme_reg_no = cp_reg_no
      and    st_course_id = cp_course_id
      and    st_delete      IS NULL
      and    st_expired_ind IS NULL
      and    st_fallback    IS NULL;

CURSOR c_validstudent ( cp_surname   students.st_surname%type
                        ,cp_forenames students.st_forenames%type
                        ,cp_dob       students.st_birth_date%type
                        ,cp_course_id students.st_course_id%type
                        ,cp_centreid  students.st_centre_id%type
                        ,cp_gender    students.st_sex%type)
    IS
      select nvl(st_scheme_reg_no,st_reg_no) reg_no
      ,      st_forenames
      ,      st_surname
      ,      st_birth_date
      from   students
      where  st_centre_id = cp_centreid
      and    st_course_id = cp_course_id
      and    st_surname   = cp_surname
      and    substr(st_forenames,1,2) = substr(cp_forenames,1,2)
      and    st_sex = cp_gender
      and    st_delete      IS NULL
      and    st_expired_ind IS NULL
      and    st_fallback    IS NULL
      and    NVL(st_birth_date,cp_dob) = cp_dob;

-- Variables
v_reg_no   students.st_reg_no%type := null;
v_surname  students.st_surname%type := null;
v_forenames students.st_forenames%type := null;
d_dob       students.st_birth_date%type := null;
v_centre_id students.st_centre_id%type  := null;
v_course_id varchar2(8)  := null;
v_reg_nos varchar2(500) := null;
v_str_forenames varchar2(500) := null;
v_str_surnames  varchar2(500) := null;
v_str_dobs      varchar2(500) := null;
v_gender   students.st_sex%type := null;

 v_evaluation_string VARCHAR2(400) := null;
 n_sub_pos           NUMBER := 0;
 v_output_string     VARCHAR2(400) := null;
 n_exact_match       NUMBER := 0;
 n_exact_match2      NUMBER := 0;
 n_close_match       NUMBER := 0;
 n_close_match2      NUMBER := 0;
 n_forenames_length   NUMBER := 0;

BEGIN

  -- Do initial stuff

  gv_module_name := 'PR_IU4_VALIDATE_ONS_CANDIDATE';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  gv_in_request_params :=
                'P1 = '||p_in_centre||' | '||
                'P2 = '||p_in_qual_code||' | '||
                'P3 = '||p_in_programme_code||' | '||
                'P4 = '||p_in_reg_no||' | '||
                'P5 = '||p_in_programme_code||' | '||
                'P6 = '||p_in_forenames||' | '||
                'P7 = '||p_in_lastname||' | '||
                'P8 = '||p_in_dob||' | '||
                'P9 = '||p_in_gender;

   -- Standardise parameters


        v_database := null;

        l_user_id       := UPPER(RTRIM(LTRIM(p_user_id)));
        l_centre_id     := UPPER(RTRIM(LTRIM(p_in_centre)));
        l_qual_code     := UPPER(RTRIM(LTRIM(p_in_qual_code)));
        l_surname       := UPPER(RTRIM(LTRIM(p_in_lastname)));
        l_forenames     := UPPER(RTRIM(LTRIM(p_in_forenames)));
        v_course_id     := UPPER(RTRIM(LTRIM(p_in_programme_code)));
        l_reg_no        := UPPER(RTRIM(LTRIM(p_in_reg_no)));
        l_gender        := UPPER(RTRIM(LTRIM(p_in_gender)));

  -- PROCESSING START

	gv_error_message := NULL;
        gv_reg_no        := NULL;
        gv_record_count  := 0;

        if v_course_id in ('KSQ00OP','ABSOP') then
           if PK_EDX_SHARED.FN_ONSPROM_QUAL_TYPE_CHECK(v_course_id) = 'PROMONSC' -- simply because it has the suffix "OP"
           then
              l_prog_no := rtrim(v_course_id,'OP');
           end if;
        else
             l_prog_no := v_course_id;
        end if;

      if l_prog_no is not null
      then
          pk_edx_eie_udi4.pr_validate_prog_code(l_prog_no);

          if gv_error_message is not null then
            gv_reg_no := l_reg_no;
          end if;
      end if;

      if l_reg_no is not null and
         gv_error_message is null
      then
        gv_reg_no := l_reg_no;

        open c_cand_exists (l_reg_no,
                            l_prog_no);
        fetch c_cand_exists into v_reg_no
                                ,v_surname
                                ,v_forenames
                                ,d_dob
                                ,v_centre_id
                                ,v_course_id
                                ,v_gender;
        close c_cand_exists;

        if v_reg_no is not null then
           if nvl(v_course_id,'X') <> nvl(l_prog_no,'X')
           then
              if v_course_id is not null then

                gv_reg_no        := v_reg_no;
                gv_error_message := 'Learner '||v_reg_no||' registered on course '||v_course_id||' not '||
                                     l_prog_no;

                v_reg_no := null;
                gv_registration_required := 'No';
              else
                gv_reg_no        := v_reg_no;
                gv_error_message := 'Learner '||v_reg_no||' not registered on course '||
                                     l_prog_no;

                v_reg_no := null;
                gv_registration_required := 'No';

              end if;
           else
                gv_reg_no := v_reg_no;
                gv_registration_required := 'No';
           end if;
        end if;
      end if;

      if v_reg_no is not null and
         gv_error_message is null then

         if substr(v_centre_id,1,5) <> substr(l_centre_id,1,5) then
              gv_reg_no        := v_reg_no;
              gv_error_message := 'Learner '||l_reg_no||' not registered at centre '||l_centre_id||
                                         ' (registered at '||v_centre_id||' as '||v_forenames||' '||v_surname||' )';

              gv_registration_required := 'No';
         else
           if ( UPPER(nvl(v_surname,'x')) <> UPPER(nvl(l_surname,'x')) ) then
              gv_reg_no        := v_reg_no;
              gv_error_message := 'Learner '||l_reg_no||
                                  ' is registered as '||v_forenames||' '||v_surname||' and not as named in the file';

              gv_registration_required := 'No';
           else
                if  UPPER(nvl(v_surname,'x'))    = UPPER(nvl(l_surname,'x')) and
                    to_date(d_dob,'dd-mm-yyyy') <> to_date(p_in_dob,'dd-mm-yyyy')
                then
                   gv_reg_no        := v_reg_no;
                   gv_error_message := 'Learner '||l_reg_no||
                         ' date of birth is different to the one supplied in the file';

                   gv_registration_required := 'No';

             --   elsif ( UPPER(nvl(v_forenames,'x')) not like UPPER(nvl(l_forenames,'x')||'%') and
             --           UPPER(nvl(v_surname,'x'))   =  UPPER(nvl(l_surname,'x')) and
             --           to_date(d_dob,'dd-mm-yyyy')  <>  to_date(p_in_dob,'dd-mm-yyyy')
             --        )
             --   then
             --      gv_reg_no        := v_reg_no;
             --      gv_error_message := 'Learner '||l_reg_no||
             --            ' is registered as '||v_forenames||' '||v_surname||' and not as named in the file';

             --      gv_registration_required := 'No';

                elsif (   UPPER(nvl(v_surname,'x'))    = UPPER(nvl(l_surname,'x'))  and
                          to_date(d_dob,'dd-mm-yyyy')  = to_date(p_in_dob,'dd-mm-yyyy') and
                          nvl(v_gender,'U') <> nvl(l_gender,'U') and
                          v_gender is not null and
                          l_gender is not null
                       )
                then
                     gv_reg_no        := v_reg_no;
                     gv_error_message := 'Learner '||l_reg_no||
                         ' gender is different to the one supplied in the file';

                     gv_registration_required := 'No';
                end if;
           end if;
         end if;
      end if;

      if ((v_reg_no is null and gv_error_message is null)
         or
         (l_reg_no is null and gv_error_message is null))
      then

            if l_surname   is not null and
               l_centre_id is not null and
               l_forenames is not null and
               p_in_dob is not null and
               l_gender is not null then

               l_count_valid_reg  := 0;
               for r_validstudent in c_validstudent ( l_surname
                                                       ,l_forenames
                                                       ,p_in_dob
                                                       ,l_prog_no
                                                       ,l_centre_id
                                                       ,l_gender)
               loop
                     v_reg_no     := r_validstudent.reg_no;
                     v_forenames  := r_validstudent.st_forenames;
                     v_surname    := r_validstudent.st_surname;
                     d_dob        := r_validstudent.st_birth_date;

                     l_count_valid_reg := l_count_valid_reg + 1;

                     if l_count_valid_reg > 1 then
                       v_reg_nos := v_reg_nos||','||v_reg_no;
                       v_str_forenames := v_str_forenames||';'||v_forenames;
                     else
                       v_reg_nos := v_reg_no;
                       v_str_forenames := v_forenames;
                     end if;
               end loop;

              if l_reg_no is null then

                if v_reg_no is null then
                  gv_reg_no        := l_reg_no;
                  gv_error_message := 'No registration number found for learner '||l_forenames||' '||l_surname;

                  gv_registration_required := 'Yes';
                else
                  if l_count_valid_reg > 1 then

                    v_evaluation_string := v_str_forenames||';';

                    select instr(v_evaluation_string,l_forenames)
                    into n_exact_match
                    from dual;

                    if n_exact_match <> 0 then

                        select instr(v_evaluation_string,l_forenames,n_exact_match + 1)
                        into n_exact_match2
                        from dual;

                        -- More than one exact match found
                        if n_exact_match2 <> 0 then

                            gv_reg_no      := l_reg_no;
                            gv_error_message := 'More than one registration number '||v_reg_nos||
                                        ' found for learner '||l_forenames||' '||l_surname;

                            gv_registration_required := 'No';
                        else

                            select substr(v_evaluation_string,n_exact_match,instr(substr(v_evaluation_string,n_exact_match),';')-1)
                            into v_output_string
                            from dual;

                            for r_validstudent in c_validstudent ( l_surname
                                                       ,v_output_string
                                                       ,p_in_dob
                                                       ,l_prog_no
                                                       ,l_centre_id
                                                       ,l_gender)
                            loop
                                   v_reg_no     := r_validstudent.reg_no;
                                   v_forenames  := r_validstudent.st_forenames;
                                   v_surname    := r_validstudent.st_surname;
                                   d_dob        := r_validstudent.st_birth_date;

                                   gv_reg_no        := v_reg_no;

                                   gv_registration_required := 'No';
                            end loop;
                        end if;

                    -- No exact match found for the forenames
                    else

                      select length(l_forenames)
                      into n_forenames_length
                      from dual;

                      n_forenames_length := n_forenames_length - 1;

                      for i in 1..n_forenames_length loop

                        select instr(v_evaluation_string,substr(l_forenames,1,n_forenames_length))
                        into  n_close_match
                        from dual;

                        if n_close_match <> 0 then

                           select instr(v_str_forenames,substr(l_forenames,1,n_forenames_length), n_close_match + 1)
                           into  n_close_match2
                           from dual;

                           -- More than one close match
                           if n_close_match2 <> 0 then

                              gv_reg_no      := l_reg_no;
                              gv_error_message := 'More than one registration number '||v_reg_nos||
                                                  ' found for learner '||l_forenames||' '||l_surname;

                              gv_registration_required := 'No';
                           else

                              select substr(v_evaluation_string,n_close_match,
                                     instr(substr(v_evaluation_string,n_close_match),';')-1)
                              into v_output_string
                              from dual;

                              for r_validstudent in c_validstudent ( l_surname
                                                       ,v_output_string
                                                       ,p_in_dob
                                                       ,l_prog_no
                                                       ,l_centre_id
                                                       ,l_gender)
                              loop
                                   v_reg_no     := r_validstudent.reg_no;
                                   v_forenames  := r_validstudent.st_forenames;
                                   v_surname    := r_validstudent.st_surname;
                                   d_dob        := r_validstudent.st_birth_date;

                                   gv_reg_no        := v_reg_no;

                                   gv_registration_required := 'No';
                              end loop;
                           end if;
                           exit;
                        else
                           n_forenames_length := n_forenames_length - 1;

                        end if;
                      end loop;

                    end if;

                  else   -- only one match found
                    gv_reg_no        := v_reg_no;

                    gv_registration_required := 'No';
                  end if;
                end if;
              else
                if v_reg_no is null then
                  gv_reg_no        := l_reg_no;
                  gv_error_message := 'Invalid registration number supplied in the file';

                  gv_registration_required := 'No';
                else
                  if v_reg_no <> l_reg_no
                  then
                   gv_reg_no        := v_reg_no;
                   gv_error_message := 'Learner registration number is '||v_reg_no||' not as supplied in the file '||
                                       l_reg_no;

                   gv_registration_required := 'No';

                  end if;
                end if;
              end if;
            end if;

      end if;

      p_out_error_message := gv_error_message;
      p_out_reg_no        := gv_reg_no;
      p_out_registration_required  := gv_registration_required;
      gv_record_count := gv_record_count + 1;

END pr_iu4_validate_ons_candidate;

-------------------------------------------------------------------------------
--  Reinstate/Withdraw KS/ALAN onscreen and paper candidate(s).
-------------------------------------------------------------------------------
PROCEDURE pr_reinstate_withdraw_ks_cand
  (p_user_id                    IN   VARCHAR2,
   p_ip_address                 IN   VARCHAR2,
   p_call_source                IN   NUMBER,
   p_in_reg_no                  IN   VARCHAR2,
   p_in_qual_type               IN   VARCHAR2, --  (KSQ00/ABS/KSQ00OP/ABSOP)
   p_in_withdraw_ind            IN   VARCHAR2, -- NULL or 'YES'
   p_in_reinstate_ind           IN   VARCHAR2, -- NULL or 'YES'
   p_out_error_message          OUT  VARCHAR2
  )
IS
   v_withdrawn_ind       VARCHAR2(1);
   v_centre_id           students.st_centre_id%TYPE;
   v_course_id           students.st_course_id%TYPE;
   v_outstanding         VARCHAR2(1);

  CURSOR c1 IS
  SELECT st_centre_id
    FROM students
   WHERE st_reg_no = p_in_reg_no
     ;

-- ec853 - second statement removed from this cursor
   CURSOR c_outstanding_book (cp_reg_no students.st_reg_no%type) IS
   SELECT 'x'
   FROM  prom_btec_ass_bookings
   WHERE pbaa_pbat_id is null
   AND   pbaa_st_reg_no = cp_reg_no;

BEGIN

  ---------------------------
  -- STANDARD INITIAL STUFF
  ---------------------------

  gv_module_name := 'PR_REINSTATE_WITHDRAW_KS_CAND';

  gv_in_request_params :=
    'P1 = '||p_in_reg_no||' | '||
    'P3 = '||p_in_qual_type||' | '||
    'P4 = '||p_in_withdraw_ind||' | '||
    'P5 = '||p_in_reinstate_ind;

  -------------------------------------
  -- Validate user/module/call source
  -------------------------------------

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  gv_user_id := p_user_id;
  gv_ip_address := p_ip_address;

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  ---------------------
  -- PROCESSING START
  ---------------------
  gv_out_trans_values   := NULL;

  --------------------------
  -- Validate parameters
  --------------------------

  gv_error_message      := NULL;

  if p_in_qual_type in ('KSQ00OP','ABSOP') then
     if PK_EDX_SHARED.FN_ONSPROM_QUAL_TYPE_CHECK(p_in_qual_type) = 'PROMONSC' -- simply because it has the suffix "OP"
     then
        v_course_id := rtrim(p_in_qual_type,'OP');
      end if;
  else
       v_course_id := p_in_qual_type;
  end if;

  pr_validate_qual_type(v_course_id);

  pr_validate_reg_no(p_in_reg_no);

  IF gv_error_message = 'Student withdrawn' THEN
   IF p_in_reinstate_ind = 'YES' THEN
      gv_error_message := NULL;
   END IF;
  END IF;

  -- Validate Reg No and course combination - Return true reg no...

  IF gv_error_message IS NULL THEN

    gv_db_reg_no    := null;
    v_withdrawn_ind := NULL;

    gv_db_reg_no := pk_edx_shared.fn_scheme_reg_to_reg_no(p_in_reg_no,v_course_id);

    IF gv_db_reg_no IS NULL THEN
      gv_error_message :=
         'Invalid Course / Student Regn No Combination';
    ELSE

      SELECT st_withdrawn_ind
        INTO v_withdrawn_ind
        FROM students
       WHERE st_reg_no = gv_db_reg_no
      ;

      IF v_withdrawn_ind IS NULL AND
        p_in_reinstate_ind = 'YES'
      THEN
        gv_error_message := 'Student not withdrawn';
      ELSE
        gv_error_message := NULL;
      END IF;
    END IF;
  END IF;

  ------------------------------------
  -- Log error if invalid data found
  ------------------------------------

  IF gv_error_message IS NOT NULL THEN

    pk_edx_shared.pr_log
      (gv_user_id,
       gv_dml_type,
       gv_ip_address,
       p_call_source,                 -- should be 4 for key skills
       1,                              -- status: 0 = OK, 1 = BAD
       gv_module_id,
       gv_in_request_params,
       gv_out_trans_values,
       i,                              -- row count
       gv_error_message                            -- error message
      );

   p_out_error_message := gv_error_message;

  END IF;


  ----------------------------------
  -- Update Database if data valid
  ----------------------------------

  IF gv_error_message IS NULL THEN

   p_out_error_message := ' '; -- Abdul can't handle NULL;
  END IF;

  IF gv_error_message IS NULL THEN

   IF p_in_reinstate_ind = 'YES'  THEN

       UPDATE students
       SET st_withdrawn_ind = NULL,
           st_withdrawn_date = NULL,
           st_result_user = p_user_id,
           st_result_origin = 'W'
       WHERE st_reg_no = gv_db_reg_no
       AND st_course_id = v_course_id
       ;
    ELSIF p_in_withdraw_ind = 'YES' THEN

       OPEN c_outstanding_book (gv_db_reg_no);
       FETCH c_outstanding_book INTO v_outstanding;
       CLOSE c_outstanding_book;

       IF v_outstanding IS NOT NULL THEN

            gv_error_message := 'Candidates with future test bookings cannot be withdrawn';
            p_out_error_message := gv_error_message;
       ELSE
          UPDATE students
          SET st_withdrawn_ind   = 'Y',
              st_withdrawn_date  = SYSDATE,
              st_result_user = p_user_id,
              st_result_origin = 'W'
          WHERE st_reg_no  = gv_db_reg_no
          AND   st_course_id = v_course_id
          ;
       END IF;
    END IF;

    IF gv_error_message  IS NULL THEN

      OPEN c1;
      FETCH c1 INTO v_centre_id;
      CLOSE c1;

      -- EC2589
      commit;

      pk_edx_shared.pr_log
        (gv_user_id,
         gv_dml_type,
         gv_ip_address,
         p_call_source,                 -- should be 4 for key skills
         0,                              -- status: 0 = OK, 1 = BAD
         gv_module_id,
         gv_in_request_params,
         gv_out_trans_values,
         1,                              -- row count
         NULL                            -- error message
        );

       p_out_error_message := ' '; -- Abdul can't handle NULL;

    END IF;
  END IF;


  -------------------
  -- PROCESSING END
  -------------------


EXCEPTION
    WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR
          (-20000,
           gv_package_name||'.'||
           gv_module_name||', '||
           SQLERRM||', '||
           p_user_id||', '||
           p_ip_address||', '||
      p_call_source
          );

END pr_reinstate_withdraw_ks_cand;

------------------------------------------
-- UC132 - Claim Candidate Unit Exemptions
------------------------------------------
PROCEDURE pr_udi4_st_unit_exempt_claim
          (p_user_id               IN   VARCHAR2,
           p_ip_address            IN   VARCHAR2,
           p_call_source           IN   NUMBER,
           p_in_programme_code     IN  VARCHAR2, -- SIA and SQA Course Ids
           p_in_reg_no             IN  VARCHAR2,
           p_in_unit_code          IN  VARCHAR2,
           p_in_exempt_ind         IN  VARCHAR2, -- NULL or 'YES'
           p_in_un_exempt_ind      IN  VARCHAR2, -- NULL or 'YES'
           p_in_declaration_ind    IN  VARCHAR2, -- NULL or 'Y'
           p_out_error_message     OUT VARCHAR2)
IS

CURSOR c_validate_reg_no (cp_reg_no students.st_reg_no%type,
                          cp_programme_code students.st_course_id%type)
IS
   SELECT 'Y',
       st_delete,
       st_withdrawn_ind,
       st_withdrawn_date,
       st_cert_no
  FROM students
 WHERE st_reg_no = cp_reg_no
 AND   st_course_id = cp_programme_code;

CURSOR c_validate_qual
IS
  select
            'X'
     from    award_codes
     ,       approval_awards
     ,       approval_application
     ,       award_titles
     ,       award_title_rules
     ,       assessment_units
     ,       assessments
     where   aw_applicat_no      = aa_applicat_no
     and     aa_btec_title       = to_char(atru_at_number)
     and     atru_at_number      = at_number
     and     NVL(at_sia_qual,'N') = 'Y'
--     and     to_char(atru_at_number) in ('9732','9733','9734','5932','6223','6893','5939','6932')
     and     atru_id             = asse_atru_id
     and     aw_award_code       = ac_code
     and     aw_course_number is not null
     and     aw_course_number not in ('KSQ00','ABS')
     and     asse_id = auni_asse_id
     and     auni_un_unit_code = p_in_unit_code
     and     asse_admo_id     = 5
     and     aw_course_number = p_in_programme_code;

CURSOR c_already_passed (cp_reg_no varchar2,
                         cp_unit_code varchar2)
IS
   SELECT 'x'
   FROM   student_units
   WHERE  su_unit_id = cp_unit_code
   AND    su_reg_no = UPPER(cp_reg_no)
   AND    NVL(substr(su_grade_5||
                                       su_grade_4||
                                       su_grade_3||
                                       su_grade_2||
                                       su_grade_1,1,1),'NONE')  = 'P';

CURSOR c_already_exempt (cp_reg_no varchar2,
                         cp_unit_code varchar2)
IS
    SELECT 'x'
        FROM   units,
               assessment_units,
               assessments,
               student_units,
               student_assessment_results
        WHERE  sare_asse_id = asse_id
        AND    sare_st_reg_no = UPPER(cp_reg_no)
        AND    sare_tpba_id = 'PROXY'
        AND    un_unit_code = auni_un_unit_code
        AND    auni_asse_id = asse_id
        AND    asse_admo_id = 5
        AND    su_unit_id = cp_unit_code
        AND    su_unit_id = auni_un_unit_code
        AND    su_reg_no = UPPER(cp_reg_no)
        AND    NVL(substr(su_grade_5||
                                       su_grade_4||
                                       su_grade_3||
                                       su_grade_2||
                                       su_grade_1,1,1),'NONE')  = 'P';

  cursor c_student_units (cp_reg_no  student_units.su_reg_no%type,
                          cp_unit_id student_units.su_unit_id%type)
                     is
                       select su_grade_1,
                              su_grade_2,
                              su_grade_3,
                              su_grade_4,
                              su_grade_5,
                              su_date
                       from student_units
                       where su_reg_no  = cp_reg_no
                       and   su_unit_id = cp_unit_id;

 v_su_grade_2 student_units.su_grade_2%type := null;
 v_su_grade_3 student_units.su_grade_3%type := null;
 v_su_grade_4 student_units.su_grade_4%type := null;
 v_su_grade_5 student_units.su_grade_5%type := null;
 v_check varchar2(1) := null;
 v_su_grade_1 student_units.su_grade_1%type := null;
 v_su_date    student_units.su_date%type    := null;

v_status                VARCHAR2(1);
v_withdrawn_ind         VARCHAR2(1);
v_found                 VARCHAR2(1);
v_delete                VARCHAR2(1);
v_withdrawn_date        DATE;
v_certified             students.st_cert_no%type;
v_btec_qual_exempt_found       VARCHAR2(1);

v_exempt_found varchar2(1);
v_course_id students.st_course_id%type := NULL;
v_already_passed varchar2(1);

BEGIN

v_status := NULL;
v_withdrawn_ind := NULL;
v_found := NULL;
v_delete := NULL;
v_withdrawn_date := NULL;
v_certified := NULL;
v_btec_qual_exempt_found := NULL;
v_exempt_found  := NULL;
v_already_passed := NULL;

  ---------------------------
  -- STANDARD INITIAL STUFF
  ---------------------------

  gv_module_name := 'PR_UDI4_ST_UNIT_EXEMPT_CLAIM';

  gv_in_request_params :=
    'P1 = '||p_in_programme_code||' | '||
    'P2 = '||p_in_reg_no||' | '||
    'P3 = '||p_in_unit_code||' | '||
    'P4 = '||p_in_exempt_ind||' | '||
    'P5 = '||p_in_un_exempt_ind||' | '||
    'P6 = '||p_in_declaration_ind;

  -------------------------------------
  -- Validate user/module/call source
  -------------------------------------

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  gv_user_id := p_user_id;
  gv_ip_address := p_ip_address;

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  ---------------------
  -- PROCESSING START
  ---------------------
  gv_out_trans_values   := NULL;

  --------------------------
  -- Validate parameters
  --------------------------

  gv_error_message      := NULL;

  v_course_id := p_in_programme_code;

  -- Validate Qualification
  IF gv_error_message IS NULL THEN

     OPEN c_validate_qual;
     FETCH c_validate_qual INTO v_btec_qual_exempt_found;
     CLOSE c_validate_qual;

     IF v_btec_qual_exempt_found IS NULL THEN
        gv_error_message := 'Qualification Type / Unit combination not Eligible for Exemption';
     END IF;
  END IF;

  -- Validate Reg No and course combination - Return true reg no...
  IF gv_error_message IS NULL THEN

    gv_db_reg_no    := null;
    v_withdrawn_ind := NULL;

    SELECT pk_edx_shared.fn_scheme_reg_to_reg_no(p_in_reg_no,v_course_id)
      INTO gv_db_reg_no
      FROM students
     WHERE st_reg_no = p_in_reg_no
    ;

    IF gv_db_reg_no IS NULL THEN
      gv_error_message :=
         'Invalid Course / Student Regn No Combination';
    END IF;

  END IF;

  -- Validate Student
  IF gv_error_message IS NULL THEN

     OPEN c_validate_reg_no (gv_db_reg_no,
                             v_course_id);
     FETCH c_validate_reg_no INTO v_found,
              v_delete,
              v_withdrawn_ind,
              v_withdrawn_date,
              v_certified;

     CLOSE c_validate_reg_no;

     IF v_found = 'Y' THEN
        IF v_delete = 'Y' THEN
           v_status := 'D';
           gv_error_message := 'Student deleted';
        ELSE
          IF v_withdrawn_ind = 'Y' OR
             v_withdrawn_date IS NOT NULL THEN
             v_status := 'W';
             gv_error_message := 'Student withdrawn';
          ELSE
            IF v_certified IS NOT NULL AND
               NVL(p_in_un_exempt_ind,'NO') = 'YES'
            THEN
             v_status := 'C';
             gv_error_message := 'Candidate who is already certified cannot have unit achievement deleted';
            ELSE
             v_status := 'Y';
             gv_error_message := null;
            END IF;
          END IF;
        END IF;
     ELSE
       v_status := 'N';
       gv_error_message := 'Invalid Student Registration Number';
     END IF;
  END IF;

  IF gv_error_message IS NULL THEN

    IF NVL(p_in_exempt_ind,'NO') = 'YES' THEN

      OPEN c_already_passed(gv_db_reg_no,
                            p_in_unit_code);
      FETCH c_already_passed INTO v_already_passed;
      CLOSE c_already_passed;

      IF v_already_passed IS NOT NULL THEN
        gv_error_message := 'Candidate who already achieved unit cannot be exempt';
      END IF;
    END IF;
  END IF;

  IF gv_error_message IS NULL THEN

    IF NVL(p_in_exempt_ind,'NO') = 'YES' THEN

      OPEN c_already_exempt(gv_db_reg_no,
                            p_in_unit_code);
      FETCH c_already_exempt INTO v_exempt_found;
      CLOSE c_already_exempt;

      IF v_exempt_found IS NOT NULL THEN

       gv_error_message := 'Candidate who already achieved unit cannot be exempt';
      END IF;
    END IF;
  END IF;
  ------------------------------------
  -- Log error if invalid data found
  ------------------------------------

  IF gv_error_message IS NOT NULL THEN

    pk_edx_shared.pr_log
      (gv_user_id,
       gv_dml_type,
       gv_ip_address,
       p_call_source,                 -- should be 4 for key skills
       1,                              -- status: 0 = OK, 1 = BAD
       gv_module_id,
       gv_in_request_params,
       gv_out_trans_values,
       i,                              -- row count
       gv_error_message                            -- error message
      );

   p_out_error_message := gv_error_message;

  END IF;

  ----------------------------------
  -- Update Database if data valid
  ----------------------------------
  IF gv_error_message IS NULL THEN

   p_out_error_message := ' '; -- Abdul can't handle NULL;
  END IF;

  IF gv_error_message IS NULL THEN

   IF p_in_exempt_ind = 'YES'  THEN

      -- Create SARE record
      insert into student_assessment_results
      (
        SARE_ID
       ,SARE_ST_REG_NO
       ,SARE_ASSE_ID
       ,SARE_AOUT_ID
       ,SARE_SCORED_ITEMS_CORRECT
       ,SARE_SCORED_ITEMS_INCORRECT
       ,SARE_SCORED_ITEMS_SKIPPED
       ,SARE_UNSCORED_ITEMS_CORRECT
       ,SARE_UNSCORED_ITEMS_INCORRECT
       ,SARE_UNSCORED_ITEMS_SKIPPED
       ,SARE_ARFI_ID
       ,SARE_DATETIME
       ,SARE_TPBA_ID
       ,SARE_EXEMPT_USER
      )
      select
        SARE_SEQ.NEXTVAL
       ,upper(gv_db_reg_no)
       ,ASSE_ID
      ,'P'
       ,0
       ,0
       ,0
       ,0
       ,0
       ,0
       ,0
       ,SYSDATE
       ,'PROXY'
       , gv_user_id
      from assessments s,
           assessment_units t
      where asse_id= auni_asse_id
      and   auni_un_unit_code = upper(p_in_unit_code)
      and   asse_admo_id = 5
      and not exists
      (select 1
       from
       student_assessment_results
       where  sare_st_reg_no = gv_db_reg_no
       and    sare_asse_id   = s.asse_id
       and    sare_aout_id   = 'P'
      );

      -- Create STUDENT_UNITS entry
      OPEN c_student_units(gv_db_reg_no,
                           UPPER(p_in_unit_code));
      FETCH c_student_units into v_su_grade_1,
                                 v_su_grade_2,
                                 v_su_grade_3,
                                 v_su_grade_4,
                                 v_su_grade_5,
                                 v_su_date;
       IF c_student_units%FOUND then

            IF v_su_grade_2 is null and
               v_su_grade_1 <> 'P' and
               v_su_date    <> to_char(sysdate,'YY') THEN

                             UPDATE STUDENT_UNITS
                             SET    SU_GRADE_2='P'
                             WHERE  SU_REG_NO =  gv_db_reg_no
                             AND    SU_UNIT_ID = p_in_unit_code;
            ELSIF v_su_grade_2 is not null and
                  v_su_grade_3 is null and
                  v_su_grade_2 <> 'P' and
                  v_su_date    <> to_char(sysdate,'YY') THEN

                             UPDATE STUDENT_UNITS
                             SET    SU_GRADE_3='P'
                             WHERE  SU_REG_NO =  gv_db_reg_no
                             AND    SU_UNIT_ID = p_in_unit_code;
            ELSIF v_su_grade_3 is not null and
                  v_su_grade_4 is null and
                  v_su_grade_3 <> 'P' and
                  v_su_date    <> to_char(sysdate,'YY') THEN

                             UPDATE STUDENT_UNITS
                             SET    SU_GRADE_4='P'
                             WHERE  SU_REG_NO =  gv_db_reg_no
                             AND    SU_UNIT_ID = p_in_unit_code;
            ELSIF v_su_grade_4 is not null and
                  v_su_grade_5 is null and
                  v_su_grade_4 <> 'P' and
                  v_su_date    <> to_char(sysdate,'YY') THEN

                             UPDATE STUDENT_UNITS
                             SET    SU_GRADE_5='P'
                             WHERE  SU_REG_NO =  gv_db_reg_no
                             AND    SU_UNIT_ID =  p_in_unit_code;
            END IF;

       ELSE -- New STUDENT_UNITS entry

                            -- Create entry in STUDENT_UNITS table
                            -- ========================================
                            INSERT INTO STUDENT_UNITS
                            (SU_REG_NO,
                             SU_UNIT_ID,
                             SU_GRADE_1,
                             SU_DATE)
                            VALUES
                            (
                             gv_db_reg_no,
                             p_in_unit_code,
                            'P',
                            to_char(sysdate,'YY'));
       END IF;

       CLOSE c_student_units;

       -- Create audit_entry
       BEGIN
         -- Action can be 'C' - exemption claimed
         --               'U' - exemption un-claimed
         INSERT INTO sia_proxy_witnesses
          (spwi_witness,
           spwi_course_id,
           spwi_un_unit_code,
           spwi_entry_date,
           spwi_st_reg_no,
           spwi_audit_action)
         VALUES
          (gv_user_id,
           v_course_id,
           p_in_unit_code,
           sysdate,
           gv_db_reg_no,
           'C');
       EXCEPTION
          WHEN OTHERS
          THEN
            Raise_application_error(-20002,sqlerrm);
       END;


   END IF;

   IF NVL(p_in_un_exempt_ind,'NO') = 'YES' THEN

      -- Delete SARE entry
       delete student_assessment_results
       where sare_asse_id = (select asse_id
                             from assessments s,
                                  assessment_units t
                             where sare_asse_id = s.asse_id
                             and   s.asse_id = t.auni_asse_id
                             and   s.asse_admo_id = 5
                             and   t.auni_un_unit_code = upper(p_in_unit_code))
       and sare_st_reg_no = gv_db_reg_no
       and  sare_aout_id   = 'P'
       and  sare_tpba_id = 'PROXY';

      -- Delete STUDENT_UNITS entry
      delete student_units
      where  su_reg_no = gv_db_reg_no
      and    su_unit_id = upper(p_in_unit_code)
      and     NVL(substr(su_grade_5||
                                       su_grade_4||
                                       su_grade_3||
                                       su_grade_2||
                                       su_grade_1,1,1),'NONE')  = 'P';

      -- Create audit_entry
      BEGIN
         -- Action can be 'C' - exemption claimed
         --               'U' - exemption un-claimed
         INSERT INTO sia_proxy_witnesses
          (spwi_witness,
           spwi_course_id,
           spwi_un_unit_code,
           spwi_entry_date,
           spwi_st_reg_no,
           spwi_audit_action)
         VALUES
          (p_user_id,
           v_course_id,
           p_in_unit_code,
           sysdate,
           gv_db_reg_no,
           'U'
           );
      EXCEPTION
          WHEN OTHERS
          THEN
            Raise_application_error(-20002,sqlerrm);
      END;

   END IF;
  END IF;

  -------------------
  -- PROCESSING END
  -------------------


EXCEPTION
    WHEN OTHERS THEN
        RAISE_APPLICATION_ERROR
          (-20000,
           gv_package_name||'.'||
           gv_module_name||', '||
           SQLERRM||', '||
           p_user_id||', '||
           p_ip_address||', '||
      p_call_source
          );

END pr_udi4_st_unit_exempt_claim;

/************************************************************************/

PROCEDURE pr_insert_student_units(p_st_reg_no VARCHAR2, p_unit_id VARCHAR2)
IS
	CURSOR  reported_nvq_units IS
	SELECT 	nscu_st_reg_no,
		nscu_ncun_ncvq_code
	FROM 	nvq_student_competence_units
	WHERE   nscu_st_reg_no 		= p_st_reg_no
	AND 	nscu_ncun_ncvq_code 	= p_unit_id;

	rec_units reported_nvq_units%ROWTYPE;
BEGIN

	l_error_message := null;

	OPEN reported_nvq_units;
	FETCH reported_nvq_units INTO rec_units;
	IF reported_nvq_units%NOTFOUND THEN

	  INSERT INTO nvq_student_competence_units
	       	(nscu_st_reg_no,
                 nscu_ncun_ncvq_code,
                 nscu_achieved_year)
	  VALUES  (p_st_reg_no, p_unit_id, TO_CHAR(SYSDATE,'YY'));

	ELSE
	   UPDATE nvq_student_competence_units
	   SET nscu_achieved_year = TO_CHAR(SYSDATE,'YY')
	   WHERE nscu_st_reg_no   = p_st_reg_no
	     AND nscu_ncun_ncvq_code = p_unit_id;
	END IF;
        CLOSE reported_nvq_units;
EXCEPTION
	WHEN OTHERS THEN

	l_error_message := 'Error inserting Student Units - '||sqlerrm;

       	SELECT MODULE_CALL_HIST_SEQ_NO.nextval
          INTO l_sequence_id
          FROM dual;

	pk_edx_eie_udi4.pr_log
                 (l_sequence_id,
	          l_user_id,
		  l_dml_type,
	    	  l_ip_address,
		  l_call_source,
		  l_call_status,
		  l_module_id,
	   	  l_in_request_params,
                  l_out_trans_values,
 	 	  l_record_count,
		  SYSDATE,
		  l_error_message);
        COMMIT;

	l_error_message    := null;
	l_out_trans_values := null;

	RAISE_APPLICATION_ERROR(-20000,'Error inserting student units -
'||sqlerrm);
        CLOSE reported_nvq_units;
END pr_insert_student_units;

-------------------------------------------------------------------------------
-- 1.18.  Amend candidate registration details
-------------------------------------------------------------------------------

PROCEDURE pr_u4_upd_candidate_details
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no			IN  VARCHAR2,
   p_in_centre_ref		IN  VARCHAR2,
   p_in_sex			IN  VARCHAR2,
   p_in_compl_date		IN  DATE,
   p_in_study_mode		IN  VARCHAR2,
   p_in_evidence_route_code     IN  VARCHAR2,
   p_in_combination		IN  VARCHAR2,
   p_in_franchise_no		IN  VARCHAR2,
   p_in_lsc_code		IN  VARCHAR2,
   p_in_surname 		IN  VARCHAR2,
   p_in_forenames 		IN  VARCHAR2,
   p_in_dob 			IN  DATE,
   p_in_uln			IN VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  )

IS

CURSOR  check_previous IS
   SELECT stat_st_reg_no
     FROM students_audit_trail
    WHERE stat_st_reg_no = p_in_reg_no
      AND stat_st_column IN ('ST_FORENAMES','ST_SURNAME','ST_BIRTH_DATE');

CURSOR st_details_cur IS
   SELECT st_reg_no,
          st_surname,
	  st_forenames,
          NVL(st_nvq_certificate_no,st_gnvq_certificate_no) nvq_cert,
          st_cert_no,
          st_ks_unit_cert_no,
	  'N' schemelearner,
	  nvl(st_uln,'*') uln
     FROM students
    WHERE st_reg_no = p_in_reg_no
      AND st_scheme_reg_no IS NULL
    UNION
   SELECT st_reg_no,
          st_surname,
	  st_forenames,
          NVL(st_nvq_certificate_no,st_gnvq_certificate_no),
          st_cert_no,
          st_ks_unit_cert_no,
          'Y',
	  nvl(st_uln,'*')
     FROM students
    WHERE st_scheme_reg_no = p_in_reg_no
    ORDER BY 1;

CURSOR mas_students_cur IS
   SELECT st_reg_no, st_scheme_reg_no
     FROM students
    WHERE st_scheme_reg_no = p_in_reg_no;

    v_mas_reg_no VARCHAR2(7)    := NULL;
    v_scheme_reg VARCHAR2(7)    := NULL;

BEGIN

  -- Do initial stuff

        l_user_id       := upper(rtrim(ltrim(p_user_id)));
        l_centre_ref    := upper(rtrim(ltrim(p_in_centre_ref)));
        l_surname       := upper(rtrim(ltrim(p_in_surname)));
        l_forenames     := upper(rtrim(ltrim(p_in_forenames)));
        l_sex           := upper(p_in_sex);
        l_uln           := upper(rtrim(ltrim(p_in_uln)));

  gv_module_name := 'PR_U4_UPD_CANDIDATE_DETAILS';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  gv_in_request_params :=
    'P1 = '||p_in_reg_no||' | '||
    'P2 = '||p_in_centre_ref||' | '||
    'P3 = '||p_in_sex||' | '||
    'P4 = '||p_in_compl_date||' | '||
    'P5 = '||p_in_study_mode||' | '||
    'P6 = '||p_in_evidence_route_code||' | '||
    'P7 = '||p_in_combination||' | '||
    'P8 = '||p_in_franchise_no||' | '||
    'P9 = '||p_in_lsc_code||' | '||
    'P10 = '||p_in_surname||' | '||
    'P11 = '||p_in_forenames||' | '||
    'P12 = '||p_in_dob||' | '||
    'P13 = '||p_in_uln
;

  ---------------------
  -- PROCESSING START
  ---------------------

  ------------------------
  -- Validate parameters
  ------------------------

  gv_error_message := NULL;

  pr_validate_reg_no(p_in_reg_no);

  IF p_in_sex IS NOT NULL THEN
    pr_validate_sex(p_in_sex);
  END IF;

  IF p_in_study_mode IS NOT NULL THEN
    pr_validate_study_mode(p_in_study_mode);
  END IF;

  IF p_in_franchise_no IS NOT NULL THEN
    pr_validate_franchise_no(p_in_franchise_no);
  END IF;

  IF p_in_lsc_code IS NOT NULL THEN
    pr_validate_lsc_code(p_in_lsc_code);
  END IF;

  IF p_in_combination IS NOT NULL THEN
    pr_validate_combination(p_in_combination, p_in_reg_no);
  END IF;

-- If trying to update surname, forenames or date of birth and any previous
-- change has been made to names or dob then thats all folks..

  IF (
      p_in_surname IS NOT NULL OR
      p_in_forenames IS NOT NULL OR
      p_in_dob IS NOT NULL
     )
  THEN
    FOR v_prev_rec IN check_previous
    LOOP
      IF p_call_source = 7
      THEN
        gv_error_message := 'CANDIDATE DETAILS PREVIOUSLY AMENDED';
      ELSE
        gv_error_message := 'The candidate detail cannot be amended as an amendment has previously been made.  '||
                            'Please submit your request in writing to Edexcel.';
      END IF;
    END LOOP;
  END IF;

-- Cannot change more than one field...

  IF (
      p_in_surname IS NOT NULL AND
      p_in_forenames IS NOT NULL AND
      p_in_dob IS NOT NULL
     ) OR
     (
      p_in_surname IS NOT NULL AND
      p_in_dob IS NOT NULL
     ) OR
     (
      p_in_forenames IS NOT NULL AND
      p_in_dob IS NOT NULL
     )
  THEN
    IF p_call_source = 7
    THEN
      gv_error_message := 'MULTIPLE CHANGES TO CANDIDATE DETAILS REQUESTED';
    ELSE
      gv_error_message :=
	    'To prevent substitution of one candidate for another, only first name,surname or the date of birth can be amended.
	     Please submit any other amendment request in writing to Edexcel.';
    END IF;
  END IF;

-- Unless reversing forenames and surnames

  FOR v_cur_rec IN st_details_cur
  LOOP
    IF (
        p_in_reg_no = v_cur_rec.st_reg_no AND
        p_in_surname IS NOT NULL AND
        p_in_forenames IS NOT NULL
       )
    THEN
      IF (
          p_in_surname =  v_cur_rec.st_forenames AND
          p_in_forenames = v_cur_rec.st_surname
         )
      THEN
        NULL;
      ELSE
        IF p_call_source = 7
        THEN
          gv_error_message := 'MULTIPLE CHANGES TO CANDIDATE DETAILS REQUESTED';
        ELSE
          gv_error_message :=
	       'To prevent substitution of one candidate for another only first name,surname or the date of birth can be amended.
	        Please submit any other amendment request in writing to Edexcel.';
        END IF;
      END IF;
    END IF;

    IF (
        v_cur_rec.nvq_cert           IS NOT NULL OR
        v_cur_rec.st_cert_no         IS NOT NULL OR
        v_cur_rec.st_ks_unit_cert_no IS NOT NULL
       ) and (v_cur_rec.schemelearner  = 'N'
          or (v_cur_rec.schemelearner  = 'Y'
         and (p_in_surname IS NOT NULL
          or p_in_forenames IS NOT NULL
          or p_in_sex is not null
          or p_in_dob is not null
          or nvl(p_in_uln,'*') <> v_cur_rec.uln)))
    THEN
      IF p_call_source = 7
      THEN
        gv_error_message := 'STUDENT HAS BEEN CERTIFICATED : CHANGES TO CANDIDATE DETAILS NOT ALLOWED';
      ELSE
        gv_error_message := 'Student has been issued a certificate.
		No updates allowed.';
      END IF;
    END IF;
  END LOOP;

  -- NB. Combination ID processing:
  -- A new set of units appears on STV070 when combination ID is changed,
  -- however this is handled by screen itself, no units need to be added
  -- or deleted as a result of a combination ID change here.

  IF gv_error_message IS NULL THEN
    UPDATE students
       SET st_centre_ref	= NVL(p_in_centre_ref, st_centre_ref),
           st_sex 		= NVL(p_in_sex, st_sex),
           st_est_completion 	= NVL(p_in_compl_date, st_est_completion),
           st_study_mode 	= NVL(p_in_study_mode, st_study_mode),
           st_comb_id 		= NVL(p_in_combination, st_comb_id),
           st_inst_location 	= NVL(p_in_franchise_no, st_inst_location),
           st_inst_tec_code 	= NVL(p_in_lsc_code, st_inst_tec_code),
	   st_erou_code 	= NVL(p_in_evidence_route_code,st_erou_code),
           st_surname           = NVL(p_in_surname, st_surname),
           st_forenames         = NVL(p_in_forenames, st_forenames),
           st_birth_date        = NVL(p_in_dob, st_birth_date),
	   st_uln		= decode(p_call_source,1,l_uln,
					 NVL(l_uln, st_uln))
     WHERE st_reg_no 			= p_in_reg_no
     and   st_nvq_certificate_no	is null
     and   st_gnvq_certificate_no	is null
     and   st_cert_no			is null
     and   st_ks_unit_cert_no		is null
    ;

-- If its a MAS student then update the associated registrations.
    FOR v_mas_rec IN mas_students_cur LOOP
      v_mas_reg_no       := v_mas_rec.st_reg_no;
      v_scheme_reg       := v_mas_rec.st_scheme_reg_no;
      IF v_scheme_reg <> v_mas_reg_no THEN
        UPDATE students
           SET st_centre_ref    = NVL(p_in_centre_ref, st_centre_ref),
               st_sex           = NVL(p_in_sex, st_sex),
               st_est_completion = NVL(p_in_compl_date, st_est_completion),
               st_study_mode    = NVL(p_in_study_mode, st_study_mode),
               st_comb_id       = NVL(p_in_combination, st_comb_id),
               st_inst_location = NVL(p_in_franchise_no, st_inst_location),
               st_inst_tec_code = NVL(p_in_lsc_code, st_inst_tec_code),
               st_erou_code     = NVL(p_in_evidence_route_code,st_erou_code),
               st_surname       = NVL(p_in_surname, st_surname),
               st_forenames     = NVL(p_in_forenames, st_forenames),
               st_birth_date    = NVL(p_in_dob, st_birth_date),
	       st_uln		= decode(p_call_source,1,l_uln,
					 NVL(l_uln, st_uln))
         WHERE st_reg_no 		= v_mas_reg_no
     	 and   st_nvq_certificate_no	is null
     	 and   st_gnvq_certificate_no	is null
     	 and   st_cert_no		is null
     	 and   st_ks_unit_cert_no	is null;
      END IF;
    END LOOP;
  END IF;

<<chk_error>>

  IF gv_error_message IS NULL THEN
    gv_status := 0;
    gv_row_count := 1;
    gv_out_trans_values := 'Student updated';
  ELSE
    p_out_error_message := gv_error_message;
    gv_status := 1;
    gv_row_count := 0;
    gv_out_trans_values := NULL;
  END IF;

  -------------------
  -- PROCESSING END
  -------------------

-- EC2589
   commit;

  pk_edx_shared.pr_log
    (p_user_id,
     gv_dml_type,
     p_ip_address,
     p_call_source,
     gv_status,                              -- status: 0 = OK, 1 = BAD
     gv_module_id,
     gv_in_request_params,
     gv_out_trans_values,
     gv_row_count,
     gv_error_message
    );

EXCEPTION
  WHEN OTHERS THEN
    gv_error_message := SQLERRM;

    ROLLBACK;

    pk_edx_shared.pr_log
      (p_user_id,
       gv_dml_type,
       p_ip_address,
       p_call_source,
       1,                              -- status: 0 = OK, 1 = BAD
       gv_module_id,
       gv_in_request_params,
       gv_out_trans_values,
       0,
       gv_error_message
      );

    RAISE_APPLICATION_ERROR
      (-20000,
       gv_package_name||'.'||
       gv_module_name||', '||
       gv_error_message||', '||
       p_user_id||', '||
       p_ip_address||', '||
       p_call_source
      );

END pr_u4_upd_candidate_details;


-------------------------------------------------------------------------------
-- 1.21. Insert unit code and result for a candidate for a unit.
-------------------------------------------------------------------------------

PROCEDURE pr_iu4_add_candidate_unit
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no			IN  VARCHAR2,
   p_in_unit_code  		IN  VARCHAR2,
   p_in_unit_result		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2,
   -- EC4940 Summer 2021 Results Delivery Starts
   p_in_unit_ra_ind      IN VARCHAR2
   -- EC4940 Summer 2021 Results Delivery Ends
  )

IS

  v_valid_unit          VARCHAR2(1);
  v_unit_type		VARCHAR2(1);
  v_centre_id		VARCHAR2(6);
  v_app_from_date	DATE;
  v_app_to_date		DATE;
  v_course_id		VARCHAR2(5);
  v_reg_type 		STUDENTS.ST_REG_TYPE%TYPE;
  v_level		VARCHAR2(4);
  v_unit_already_exists VARCHAR2(1);
  v_unit_already_graded VARCHAR2(1);
  v_possible_grades	VARCHAR2(22);
  v_type		VARCHAR2(1);
  v_extass_type		VARCHAR2(3);
  v_qca_code            units.un_qca_code%type;
  v_aw_code_62_85       approval_awards.aw_award_code%type;

  CURSOR c10 IS
  SELECT st_centre_id,
         st_course_id,
         st_reg_type
    FROM students
   WHERE st_reg_no = p_in_reg_no
  ;

  -- From stv070
  -- NB unit type 3 is an externally assessed unit
  -- EC245 add QCA code
  CURSOR c11 IS
  SELECT 'Y',
         un_level,
         NVL(un_type, ' '),
         un_extass_type,
         un_qca_code
    FROM units
   WHERE un_unit_code = p_in_unit_code
  ;

  CURSOR c12 IS
  SELECT 'Y'
    FROM nvq_competence_units
   WHERE ncun_id = p_in_unit_code
  ;


PROCEDURE pr_process_btec
IS
  v_grade_1       	VARCHAR2(1);
  v_grade_2       	VARCHAR2(1);
  v_grade_3       	VARCHAR2(1);
  v_grade_4       	VARCHAR2(1);
  v_grade_5       	VARCHAR2(1);
  v_grades        	VARCHAR2(5);
  v_unit_part_of_course	VARCHAR2(1);
  v_yy			VARCHAR2(2);
  v_cert_end            date := null;
  v_diag_only		VARCHAR2(1);
  v_diag_type           NUMBER;
   -- EC4940 Summer 2021 Results Delivery Starts
  v_ra_ind_1       	VARCHAR2(1);
  v_ra_ind_2       	VARCHAR2(1);
  v_ra_ind_3       	VARCHAR2(1);
  v_ra_ind_4       	VARCHAR2(1);
  v_ra_ind_5       	VARCHAR2(1);
  v_ra_ind        	VARCHAR2(5);
   -- EC4940 Summer 2021 Results Delivery Ends

  CURSOR c1 IS
  SELECT 'Y',
         DECODE
           (su_grade_1||su_grade_2||su_grade_3||su_grade_4||su_grade_5,
            NULL, 'N',
            'Y'
           ),
         su_grade_1,
         su_grade_2,
         su_grade_3,
         su_grade_4,
         su_grade_5,
   -- EC4940 Summer 2021 Results Delivery Starts
         CASE WHEN su_grade_1 IS NOT NULL THEN NVL(su_ra_ind1,'N') ELSE su_ra_ind1 END AS su_ra_ind1,
         CASE WHEN su_grade_2 IS NOT NULL THEN NVL(su_ra_ind2,'N') ELSE su_ra_ind2 END AS su_ra_ind2,
         CASE WHEN su_grade_3 IS NOT NULL THEN NVL(su_ra_ind3,'N') ELSE su_ra_ind3 END AS su_ra_ind3,
         CASE WHEN su_grade_4 IS NOT NULL THEN NVL(su_ra_ind4,'N') ELSE su_ra_ind4 END AS su_ra_ind4,
         CASE WHEN su_grade_5 IS NOT NULL THEN NVL(su_ra_ind5,'N') ELSE su_ra_ind5 END AS su_ra_ind5
   -- EC4940 Summer 2021 Results Delivery Ends
    FROM student_units
   WHERE su_reg_no = p_in_reg_no
     AND su_unit_id = p_in_unit_code
  ;

  -- From stv060
  -- This cursor is very slow
  CURSOR c4 IS
  SELECT TO_DATE(cm_app_from_date, 'YYYYMMDD'),
         TO_DATE(cm_app_to_date, 'YYYYMMDD')
    FROM centre_modules_view
   WHERE cm_centre_id = SUBSTR(v_centre_id,1,5)
     AND UPPER(cm_module_code) = p_in_unit_code
  ;

  CURSOR c6 IS
  SELECT 'Y'
    FROM all_programme_units_view
  WHERE apuv_course_id = v_course_id
    AND apuv_unit_type = 'B'
    AND apuv_unit_code = p_in_unit_code
  ;

  cursor c7 is
    select MAX(AACC_CERTIFICATE_END_DATE)
     from  atac_accreditations
          ,award_codes
          ,atac_options
          ,atac_option_groups
          ,atac_option_group_units
     where aogu_unit_code = p_in_unit_code
      and  aogr_aopt_id   = aogu_aogr_aopt_id
      and  aogr_label     = aogu_aogr_label
      and  aopt_id        = aogr_aopt_id
      and  ac_code        = aopt_ac_code
      and  ac_nqf_ind     = 'Y'
      and  aacc_ac_code   = aopt_ac_code
      and  aacc_at_number = aopt_at_number;

  cursor c8 is
    SELECT ADD_MONTHS(MAX(AUAP_APP_TO_DATE),36)
     FROM  ALL_UNIT_APPROVALS
     WHERE AUAP_UNIT_CODE = p_in_unit_code
      AND  AUAP_CENTRE_ID = SUBSTR(v_centre_id,1,5);

BEGIN /* pr_process_btec */

  v_unit_already_exists := NULL;

  -- check to see if unit already exists for student
  v_unit_already_exists := 'N';
  v_unit_already_graded := 'N';
  OPEN c1;
  FETCH c1 INTO v_unit_already_exists,
                v_unit_already_graded,
                v_grade_1,
                v_grade_2,
                v_grade_3,
                v_grade_4,
                v_grade_5,
   -- EC4940 Summer 2021 Results Delivery Starts
                v_ra_ind_1,
                v_ra_ind_2,
                v_ra_ind_3,
                v_ra_ind_4,
                v_ra_ind_5;
   -- EC4940 Summer 2021 Results Delivery Ends
  CLOSE c1;
  OPEN c10;
  FETCH c10 INTO v_centre_id,
                 v_course_id,
                 v_reg_type;
  CLOSE c10;

  -- EC245
  pk_edx_eie_udi4.pr_get_award_code(v_course_id, v_aw_code_62_85, v_diag_only, v_diag_type);

  IF (v_reg_type='R' and v_unit_already_graded = 'N')
    OR (v_reg_type='I' and v_unit_already_EXISTS = 'N') THEN
  -- If unit is part of student's course then don't need to check if unit
    -- approved.
    v_unit_part_of_course := 'N';
    OPEN c6;
    FETCH c6 INTO v_unit_part_of_course;
    CLOSE c6;
    IF v_unit_part_of_course = 'N' THEN
      -- validate unit approved at centre
      v_app_from_date := NULL;
      OPEN c4;
      FETCH c4 INTO v_app_from_date, v_app_to_date;
      CLOSE c4;
      IF v_app_from_date IS NULL THEN
        gv_error_message := 'BTEC unit '||p_in_unit_code||
                            ' not approved at centre '||v_centre_id;
      END IF;
      IF gv_error_message IS NULL THEN
        IF SYSDATE NOT BETWEEN v_app_from_date AND v_app_to_date THEN
          gv_error_message := 'BTEC unit '||p_in_unit_code||
                              ' not currently approved';
        END IF;
      END IF;
    END IF;
  END IF;

  if gv_error_message is null and v_reg_type = 'I'
  then
  -- Check unit is on a certifiable programme.
    open c7;
    fetch c7 into v_cert_end;
    close c7;
    if v_cert_end is null
    then
      open c8;
      fetch c8 into v_cert_end;
      close c8;
    end if;
    if trunc(sysdate) > v_cert_end
    or v_cert_end is null
    then
      gv_error_message := 'Unit '||p_in_unit_code||
                          ' is not currently valid for certification.';
    end if;
  end if;

  IF gv_error_message IS NULL THEN
    -- validate_grade if Inserting or Updating
    v_possible_grades :=
      pk_edx_shared2.fn_get_valid_unit_grades
        (p_in_reg_no, p_in_unit_code);
    IF v_possible_grades is NULL
    THEN
         gv_error_message := 'Invalid grade '||p_in_unit_result||
                          ', there are no possible grades';
    ELSE
      IF INSTR(v_possible_grades, p_in_unit_result) = 0
      THEN
         gv_error_message := 'Invalid grade '||p_in_unit_result||
                          ', possible grades are:'||v_possible_grades;
      END IF;
    END IF;
  END IF;

  IF gv_error_message IS NULL THEN
    IF v_unit_already_exists = 'Y' THEN
      -- Update Unit
      v_grades :=
        v_grade_1||
        v_grade_2||
        v_grade_3||
        v_grade_4||
        v_grade_5||
        p_in_unit_result;
   -- EC4940 Summer 2021 Results Delivery Starts
      v_ra_ind :=
        v_ra_ind_1||
        v_ra_ind_2||
        v_ra_ind_3||
        v_ra_ind_4||
        v_ra_ind_5||
        p_in_unit_ra_ind;
   -- EC4940 Summer 2021 Results Delivery Ends

        -- EC245
        IF v_type = '5' AND
           v_qca_code IS NOT NULL AND
           v_aw_code_62_85 IN ('62','85')
        THEN
         SELECT TRANSLATE(v_grades,'DM','PPPPP')
         INTO   v_grades
         FROM DUAL;
        END IF;

      UPDATE student_units
         SET su_grade_1 = SUBSTR(v_grades,1,1),
             su_grade_2 = SUBSTR(v_grades,2,1),
             su_grade_3 = SUBSTR(v_grades,3,1),
             su_grade_4 = SUBSTR(v_grades,4,1),
             su_grade_5 = SUBSTR(v_grades,5,1),
             su_date = TO_CHAR(SYSDATE, 'YY'),
   -- EC4940 Summer 2021 Results Delivery Starts
             su_ra_ind1 = SUBSTR(v_ra_ind,1,1),
             su_ra_ind2 = SUBSTR(v_ra_ind,2,1),
             su_ra_ind3 = SUBSTR(v_ra_ind,3,1),
             su_ra_ind4 = SUBSTR(v_ra_ind,4,1),
             su_ra_ind5 = SUBSTR(v_ra_ind,5,1)
   -- EC4940 Summer 2021 Results Delivery Ends
       WHERE su_reg_no = p_in_reg_no
         AND su_unit_id = p_in_unit_code
      ;
      gv_out_trans_values := 'BTEC unit successfully updated';
    END IF;
    IF v_unit_already_exists = 'N' THEN
      -- insert unit
      v_yy :=
        CASE p_in_unit_result
          WHEN NULL THEN NULL
          ELSE TO_CHAR(SYSDATE, 'YY')
        END;
      pk_edx_shared3.pr_insert_student_unit
        (p_in_reg_no,
         p_in_unit_code,
         p_in_unit_result,
         v_yy,
         gv_error_message,
   -- EC4940 Summer 2021 Results Delivery Starts
         p_in_unit_ra_ind
   -- EC4940 Summer 2021 Results Delivery Ends
        );

      -- Update flags on students table causing overnight jobs
      -- to print out relevant paper work
      IF gv_error_message IS NULL THEN
        UPDATE students
           SET st_nops_reqd = DECODE(BTEC.FN_SUPPRESS_NOP_CAR(st_reg_no,'N'),'Y',NULL,'Y'),
               st_year =  2,
               st_car_reqd = DECODE(BTEC.FN_SUPPRESS_NOP_CAR(st_reg_no,'C'),'Y',NULL,'Y'),
	       st_result_origin        = 'W',
	       st_result_user          = p_user_id
         WHERE st_reg_no = p_in_reg_no
        ;
        gv_out_trans_values := 'BTEC unit successfully Inserted';
      END IF;
    END IF;
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    gv_error_message := 'pr_process_btec:'||SQLERRM;
    ROLLBACK;

END pr_process_btec;


PROCEDURE pr_process_nvq
IS
  v_achieved_year	VARCHAR2(2);
  v_expired_unit	VARCHAR2(1);
  v_expiry_date		VARCHAR2(11);
  v_cert_end            nvq_accreditations.nacc_certificate_end_date%type:=null;

  -- All cursors from STV070/STP150
  CURSOR c1 IS
  SELECT 'Y',
         nscu_achieved_year
    FROM nvq_student_competence_units
   WHERE nscu_st_reg_no = p_in_reg_no
     AND nscu_ncun_ncvq_code = p_in_unit_code
  ;

  CURSOR c3 IS
  SELECT 'Y'
    FROM nvq_centre_unit_approvals
   WHERE ncua_centre_id = SUBSTR(v_centre_id, 1, 5)
     AND ncua_ncun_ncvq_code = p_in_unit_code
  ;
  -- RSH 06/12/2005 (RfW 05/0186):
  -- Check whether NVQ unit certification has expired.
  CURSOR c5 IS
  SELECT 'Y',
         to_char(ncun_certificate_end_date,'DD-MON-YYYY')
    FROM nvq_competence_units
   WHERE ncun_ncvq_code = p_in_unit_code
     AND nvl(ncun_certificate_end_date,trunc(sysdate))
                        < trunc(sysdate)
  ;
  CURSOR c6 IS
    SELECT MAX(NACC_CERTIFICATE_END_DATE)
     FROM  NVQ_ACCREDITATIONS,
           NVQ_GROUP_UNITS
     WHERE NACC_NVQ_ID  = NGUN_NVQ_ID
      AND  NGUN_NCUN_ID = p_in_unit_code
  ;
BEGIN

  -- check to see if unit already exists for student
  v_unit_already_exists := 'N';
  OPEN c1;
  FETCH c1 INTO v_unit_already_exists,
                v_achieved_year;
  CLOSE c1;

  OPEN c10;
  FETCH c10 INTO v_centre_id,
                 v_course_id,
                 v_reg_type;
  CLOSE c10;
  IF v_unit_already_exists = 'N' THEN
    -- validate unit approved at centre
    v_valid_unit := 'N';
    OPEN c3;
    FETCH c3 INTO v_valid_unit;
    CLOSE c3;
    IF v_valid_unit <> 'Y' THEN
      gv_error_message := 'NVQ unit '||p_in_unit_code||
                          ' is not approved to run centre '||v_centre_id;
    END IF;
  END IF;
  -- RSH 06/12/2005 (RfW 05/0186):
  -- If unit certification has expired, populate the error message.
  IF gv_error_message IS NULL THEN
    IF p_in_unit_result = 'P' AND
       (
        v_unit_already_exists = 'N' OR
        (
         v_unit_already_exists = 'Y' AND
         v_achieved_year IS NULL
        )
       ) THEN
      v_expired_unit := 'N';
      OPEN c5;
      FETCH c5 INTO v_expired_unit,
                    v_expiry_date;
      CLOSE c5;
      IF v_expired_unit = 'Y' THEN
        gv_error_message := 'Certification of NVQ unit '||p_in_unit_code||
                            ' expired on '||v_expiry_date;
      END IF;
      -- And for I-types check if unit is part of a certifiable NVQ
      IF v_reg_type = 'I'
      THEN
        OPEN c6;
        FETCH c6 INTO v_cert_end;
        CLOSE c6;
        IF trunc(sysdate) > v_cert_end
        OR v_cert_end is null
        THEN
          gv_error_message := 'NVQ unit '||p_in_unit_code||
                              ' is not currently valid for certification';
        END IF;
      END IF;
    END IF;
  END IF;

  IF gv_error_message IS NULL THEN
    IF p_in_unit_result = 'P' OR
       p_in_unit_result IS NULL THEN
      IF p_in_unit_result = 'P' THEN
        v_achieved_year := TO_CHAR(SYSDATE, 'YY');
      ELSE
        v_achieved_year := NULL;
      END IF;
    ELSE
      gv_error_message := 'Invalid grade '||p_in_unit_result||
                          '. Enter P or NULL';
    END IF;
  END IF;

  IF gv_error_message IS NULL THEN
    IF v_unit_already_exists = 'Y' THEN
      UPDATE nvq_student_competence_units
         SET nscu_achieved_year = v_achieved_year
       WHERE nscu_st_reg_no = p_in_reg_no
         AND nscu_ncun_ncvq_code = p_in_unit_code
      ;
      gv_out_trans_values := 'NVQ unit successfully updated';
    END IF;
    IF v_unit_already_exists = 'N' THEN
      pk_edx_shared3.pr_insert_nvq_stud_comp_unit
        (p_in_reg_no,
         p_in_unit_code,
         v_achieved_year,
         gv_error_message
        );
      -- Update flags on students table causing overnight jobs
      -- to print out relevant paper work
      IF gv_error_message IS NULL THEN
        UPDATE students
           SET st_car_reqd = DECODE(BTEC.FN_SUPPRESS_NOP_CAR(st_reg_no,'C'),'Y',NULL,'Y'),
               st_nvq_roa_code = DECODE(v_achieved_year,
                                        NULL, st_nvq_roa_code,
                                        'Y'
                                       )
      	        , st_result_origin        = 'W'
  	        , st_result_user          = p_user_id
         WHERE st_reg_no = p_in_reg_no
        ;
        gv_out_trans_values := 'NVQ unit successfully inserted';
      END IF;
    END IF;
  END IF;

EXCEPTION
  WHEN OTHERS THEN
    gv_error_message := 'pr_process_nvq:'||SQLERRM;
    ROLLBACK;

END pr_process_nvq;


BEGIN /* pr_iu4_add_candidate_unit */

  -- Do initial stuff

  gv_module_name := 'pr_udi4_add_candidate_unit';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  gv_in_request_params :=
    'P1 = '||p_in_reg_no||' | '||
    'P2 = '||p_in_unit_code||' | '||
    'P3 = '||p_in_unit_result||' | '||
   -- EC4940 Summer 2021 Results Delivery Starts
    'P4 = '||p_in_unit_ra_ind;
   -- EC4940 Summer 2021 Results Delivery Ends


  ---------------------
  -- PROCESSING START
  ---------------------

  ------------------------
  -- Validate parameters
  ------------------------

  gv_error_message := NULL;

  pk_edx_eie_udi4.pr_validate_reg_no(p_in_reg_no);

  -- Determine type of unit
  -- Test if BTEC first
  v_valid_unit := 'N';
  OPEN c11;
  FETCH c11 INTO v_valid_unit,
                 v_level,
                 v_type,
                 v_extass_type,
                 v_qca_code;
  CLOSE c11;
  IF v_valid_unit = 'Y' THEN
    IF v_type = '3' THEN
      -- externally assessed, check type
      IF v_extass_type = 'IVA' THEN
        gv_error_message :=
          'Externally assessed BTEC (IVA), can not process unit '||
          p_in_unit_code;
      ELSE
        v_unit_type := 'B'; -- BTEC (valid externally assessed - non IVA)
      END IF;
    ELSE
      v_unit_type := 'B'; -- BTEC
    END IF;
  ELSE
    -- Test if NVQ
    OPEN c12;
    FETCH c12 INTO v_valid_unit;
    CLOSE c12;
    IF v_valid_unit = 'Y' THEN
      v_unit_type := 'N'; -- NVQ
    ELSE
      gv_error_message := 'Invalid BTEC/NVQ unit '||p_in_unit_code;
    END IF;
  END IF;

  ---------------------
  -- PROCESSING START
  ---------------------

  IF gv_error_message IS NULL THEN
    IF v_unit_type = 'B' THEN
      pr_process_btec;
    END IF;
    IF v_unit_type = 'N' THEN
      pr_process_nvq;
    END IF;
  END IF;

  IF gv_error_message IS NULL THEN
    -- RES(ults) apparently
    pr_eie_students_insert(p_in_reg_no, 'RES');
    gv_status := 0;
    p_out_error_message := NULL;
   -- EC2589 - was a call to pr_eo_student_transactions_nc ... No Commit - however .... so
   -- NO commit;
  ELSE
    ROLLBACK;
    gv_status := 1;
    p_out_error_message := gv_error_message;
  END IF;

  -------------------
  -- PROCESSING END
  -------------------

  -- AS this procedure is called multiple times followed possibly
  -- by a claim for a full award (1.22), it cannot be allowed to commit
  -- the transaction, as if any problem is found all database
  -- changes must be rolled back, including the successful ones.
  -- So local "no commit" versions of pr_log have been taken from
  -- pk_edx_shared2

  pk_edx_eie_udi4.pr_log_no_commit
    (p_user_id,
     gv_dml_type,
     p_ip_address,
     p_call_source,
     gv_status,                 -- status: 0 = OK, 1 = BAD
     gv_module_id,
     gv_in_request_params,
     gv_out_trans_values,
     gv_row_count,
     gv_error_message
    );

EXCEPTION
  WHEN OTHERS THEN
    gv_error_message := SQLERRM;

    ROLLBACK;

    pk_edx_shared.pr_log
      (p_user_id,
       gv_dml_type,
       p_ip_address,
       p_call_source,
       1,                              -- status: 0 = OK, 1 = BAD
       gv_module_id,
       gv_in_request_params,
       gv_out_trans_values,
       0,
       gv_error_message
      );

    RAISE_APPLICATION_ERROR
      (-20000,
       gv_package_name||'.'||
       gv_module_name||', '||
       gv_error_message||', '||
       p_user_id||', '||
       p_ip_address||', '||
       p_call_source
      );

END pr_iu4_add_candidate_unit;


-------------------------------------------------------------------------------
-- 1.22. Insert result for a full award/claim fallback/request reports
-------------------------------------------------------------------------------

PROCEDURE pr_u4_claim_candidate_prog
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_award_date		IN  VARCHAR2,  -- in format MM/YYYY
   p_in_overall_result		IN  VARCHAR2,
   p_in_claim_type		IN  VARCHAR2,
   p_in_withdraw		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  )

IS

  v_reg_type		VARCHAR2(1);
  v_cert_no		VARCHAR2(8);
  v_award_code		VARCHAR2(2);
  v_title               NUMBER(6);
  v_main_board		VARCHAR2(2);
  v_valid_combination	VARCHAR2(1);
  v_award_date		DATE;
  v_withdrawn_date	DATE;
  v_acty_code           VARCHAR2(2);
  v_trans_type		NUMBER(2);
  v_dumnum		NUMBER(1);
  v_award_month         VARCHAR2(6);
  v_sia_qual            award_titles.at_sia_qual%type;
  -----------------------------------
  -- Current Status for flag update
  -----------------------------------

  ---------------------------------------
  -- Potential Scenario's:
  --
  -- 1 = Award Claim
  -- 2 = Fallback Claim
  -- 3 = Interim Results
  -- 4 = Interim Results and Withdrawn
  -- 5 = Withdrawn Only
  -- 6 = Claim units and Withdraw (I-type)
  ---------------------------------------

  v_scenario		NUMBER(1);
  v_db_scenario		NUMBER(1);
  v_nvq_units_reported_ind
                        VARCHAR2(1);

  ------------------------------------
  -- Flag settings on students table
  ------------------------------------

  v_award_claim         VARCHAR2(3);
  v_award_elig		VARCHAR2(3);
  v_fallback		VARCHAR2(3);
  v_nops_reqd		VARCHAR2(3);
  v_nvq_roa_code	VARCHAR2(3);
  v_cred_trans_code	VARCHAR2(3);
  v_withdrawn_ind	VARCHAR2(3);
  v_car_reqd		VARCHAR2(3);
  v_date_award_claimed	VARCHAR2(3);
  v_nvq_roa_code2	VARCHAR2(3);
  v_fallback_cuc	VARCHAR2(3);
  v_course_id		VARCHAR2(5);
  v_reg_date		VARCHAR2(6);
  v_reg_date_full       students.st_reg_date%type;

  CURSOR check_fallback IS
     SELECT 1
       FROM award_codes,
	    approval_awards
      WHERE NVL(ac_fallback_ind,'N') = 'Y'
	AND ac_code = aw_award_code||''
	AND aw_course_number = v_course_id;

  CURSOR c1 IS
  SELECT st_reg_type,
         st_cert_no,
	 st_course_id,
         TO_CHAR(st_reg_date,'YYYYMM'),
         st_reg_date
    FROM students
   WHERE st_reg_no = p_in_reg_no
  ;

  -- From stv060 st_course_id post-change trigger
  CURSOR c2 IS
  SELECT aaw.aw_award_code,
         aap.aa_main_board,
         ac.ac_acty_code,
         TO_NUMBER(AA_BTEC_TITLE),
         nvl(at.at_sia_qual,'N')
    FROM award_codes ac,
         award_titles at,
         approval_application aap,
         approval_awards aaw,
         students st
   WHERE ac.ac_code = aaw.aw_award_code
     AND at.at_number = to_number(aap.aa_btec_title)
     AND aaw.aw_applicat_no = aap.aa_applicat_no
     AND aaw.aw_course_number = st.st_course_id
     AND st.st_reg_no = p_in_reg_no
  ;

  CURSOR c3 IS
  SELECT 'Y'
    FROM nvq_student_competence_units
   WHERE nscu_st_reg_no = p_in_reg_no
  ;

  CURSOR c10 IS
  SELECT fsfc_scenario,
         fsfc_award_claim,
         fsfc_award_elig,
         fsfc_fallback,
         fsfc_nops_reqd,
         fsfc_nvq_roa_code,
         fsfc_cred_trans_code,
         fsfc_withdrawn_ind,
         fsfc_car_reqd,
         fsfc_date_award_claimed,
         fsfc_nvq_roa_code2,
         fsfc_fallback_cuc
    FROM flag_settings_for_claims
   WHERE (v_reg_type <> 'I' AND
          fsfc_scenario = v_scenario AND
          fsfc_award_codes LIKE '%'||v_award_code||'%'
         )
          OR
         (v_reg_type = 'I' AND
          fsfc_award_codes = 'I-TYPE' AND
          fsfc_scenario = v_scenario
         )
  ;

BEGIN

  -- Do initial stuff

  gv_module_name := 'PR_U4_CLAIM_CANDIDATE_PROG';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  gv_in_request_params :=
    'P1 = '||p_in_reg_no||' | '||
    'P2 = '||p_in_award_date||' | '||
    'P3 = '||p_in_overall_result||' | '||
    'P4 = '||p_in_claim_type||' | '||
    'P5 = '||p_in_withdraw;

  ---------------------
  -- PROCESSING START
  ---------------------

  gv_error_message := NULL;

  ------------------------
  -- VALIDATE PARAMETERS
  ------------------------

  -----------------------------------------------------------------------
  -- Claim
  -- Type  Description
  -- ====  ============================================================
  --  A    Full award. Date parameter required, overall result not
  --       required as system will calculate after eligibility flag set.
  --       Award updated if required and paper work requested
  --  I    Interim Reporting. Paper work requested, no update.
  --  F    Fallback claim. Appropriate paper work requested, no update.
  -----------------------------------------------------------------------

  OPEN c1;
  FETCH c1 INTO v_reg_type,
                v_cert_no,
		v_course_id,
		v_reg_date,
                v_reg_date_full;
  CLOSE c1;

  ------------------------
  -- Validate award date
  ------------------------

  v_award_month := TO_CHAR(TO_DATE(p_in_award_date, 'MM/YYYY'),'YYYYMM');

  -- new rule states this should be based on month only.

  IF v_reg_date > v_award_month THEN
      gv_error_message :=
        'Award date must be later than registration date';
  END IF;

  IF p_in_claim_type = 'A' THEN
    IF p_in_award_date IS NULL THEN
      gv_error_message :=
        'Award date required for type A claim';
    END IF;
  END IF;

  IF p_in_claim_type = 'F' THEN

	OPEN check_fallback;
	FETCH check_fallback
	 INTO v_dumnum;
	IF check_fallback%NOTFOUND THEN
           gv_error_message := 'Error...fallback not allowed for this type of award.';
	END IF;
	CLOSE check_fallback;

    	IF p_in_overall_result IS NOT NULL THEN
      	   gv_error_message := 'For type F no result required';
    	END IF;
  END IF;

  IF p_in_claim_type = 'I' THEN
    IF p_in_overall_result IS NOT NULL THEN
      gv_error_message := 'For type I no result required';
    END IF;
  END IF;
  IF p_in_claim_type NOT IN ('A', 'F', 'I') THEN
    gv_error_message :=
      'Invalid claim type, valid types are A/F/I';
  END IF;

  ---------------------------
  -- Validate withdraw flag
  ---------------------------

  IF gv_error_message IS NULL THEN
    IF p_in_withdraw NOT IN ('R', 'W') THEN
      gv_error_message :=
        'Withdraw parameter can be W(ithdraw) or R(einstate) or space';
    END IF;
  END IF;

  ----------------------------
  -- Validate Certificate No
  ----------------------------

  IF p_in_claim_type = 'A' AND gv_error_message IS NULL THEN
    IF v_cert_no IS NOT NULL THEN
      gv_error_message := 'Overall grades only entered where certificate '||
                        'number is null';
    END IF;
  END IF;

  ----------------------------------------------------------------
  -- Validate award code, board and overall result, combinations
  ----------------------------------------------------------------

  IF gv_error_message IS NULL AND NVL(p_in_withdraw, ' ') <> 'R' THEN
    OPEN c2;
    FETCH c2 INTO v_award_code,
                  v_main_board,
                  v_acty_code,
                  v_title,
                  v_sia_qual;
    CLOSE c2;
  END IF;

  IF  p_in_claim_type  =  'A'
  AND v_reg_type       != 'I'
  AND v_sia_qual       =  'Y'
  AND gv_error_message IS NULL
  THEN
    -- 4-day rule for SIA quals
    IF v_reg_date_full > trunc(sysdate) - 4
    THEN
      gv_error_message := 'Award claim cannot be made within 4 days of registration';
    END IF;
  END IF;

  IF p_in_claim_type = 'A' AND
     v_reg_type <> 'I' AND
     gv_error_message IS NULL THEN

    v_valid_combination := 'N';

    IF v_award_code = 'EP' THEN
      IF p_in_overall_result IN ('A','B','C','D','E','U') OR
         p_in_overall_result IS NULL THEN
        v_valid_combination := 'Y';
      ELSE
        gv_error_message :=
'For award code EP only A, B, C, D, E or U are valid grades';
      END IF;
    END IF;

    IF v_award_code in ('F3','F4','3F','4F') and
       nvl(p_in_overall_result,'*') not in ('P','M','D')
    THEN
        gv_error_message :=
		'Overall grade P, M or D mandatory for QCF FAD';
    END IF;
    IF v_award_code in ('11','VV','F3','F4','3F','4F') or v_title = 8445 THEN
      IF p_in_overall_result IN ('P', 'M', 'D') OR
         p_in_overall_result IS NULL THEN
        v_valid_combination := 'Y';
      ELSE
        gv_error_message :=
		'For FAD only P/M/D are valid grades';
      END IF;
    END IF;
    IF v_award_code IN ('01', '02', '03', '04', '05', '06') AND
       v_main_board = '01' THEN
      IF p_in_overall_result IN ('D', 'C', 'P') OR
          p_in_overall_result IS NULL THEN
        v_valid_combination := 'Y';
      ELSE
        gv_error_message :=
          'For award codes 01-06, board 01 only grades P/C/D are valid ';
      END IF;
    END IF;
    IF p_in_overall_result IS NULL THEN
      v_valid_combination := 'Y';
    ELSE
      IF v_award_code IN ('3F','4F','01', '02', '03', '04', '05', '06', '11', 'EP','VV','F3','F4')  or v_title = 8445
      THEN
        NULL;
      ELSE
        gv_error_message :=
          'Overall grade not allowed for this award code '||v_award_code;
      END IF;
    END IF;
    IF gv_error_message IS NULL AND v_valid_combination = 'N' THEN
      gv_error_message :=
        'Invalid board/award code/result combination ('||
        v_main_board||'/'||
        v_award_code||'/'||
        p_in_overall_result||')';
    END IF;
  END IF; /* check valid combination */

  -------------------------------------
  -- Get flag_settings_for_claims
  -- associated with current scenario
  -------------------------------------

  v_scenario := NULL;
  v_db_scenario := NULL;

  IF p_in_claim_type = 'A' THEN
    v_scenario := '1';
  END IF;
  IF p_in_claim_type = 'F' THEN
    v_scenario := '2';
  END IF;
  IF p_in_claim_type = 'I' AND
     p_in_withdraw IS NULL THEN
    v_scenario := '3';
  END IF;
  IF p_in_claim_type = 'I' AND
     p_in_withdraw = 'W' THEN
    v_scenario := '4';
  END IF;
  IF p_in_withdraw = 'W' AND
     p_in_claim_type IS NULL THEN
    v_scenario := '5';
  END IF;
  IF p_in_withdraw = 'W' AND
     p_in_claim_type = 'A' THEN
    v_scenario := '6';
  END IF;

  IF v_scenario IS NOT NULL THEN
    v_db_scenario := NULL;
    v_award_claim := NULL;
    v_award_elig := NULL;
    v_fallback := NULL;
    v_nops_reqd := NULL;
    v_nvq_roa_code := NULL;
    v_cred_trans_code := NULL;
    v_withdrawn_ind := NULL;
    v_car_reqd := NULL;
    v_date_award_claimed := NULL;
    v_nvq_roa_code2 := NULL;
    v_fallback_cuc := NULL;
    OPEN c10;
    FETCH c10 INTO v_db_scenario,
                   v_award_claim,
                   v_award_elig,
                   v_fallback,
                   v_nops_reqd,
                   v_nvq_roa_code,
                   v_cred_trans_code,
                   v_withdrawn_ind,
                   v_car_reqd,
                   v_date_award_claimed,
                   v_nvq_roa_code2,
                   v_fallback_cuc;
    CLOSE c10;

  END IF;

  IF v_db_scenario IS NULL AND NVL(p_in_withdraw, ' ') <> 'R' THEN
    gv_error_message := 'Scenario unknown ('||
                        'Typ='||p_in_claim_type||
                        ' Wdn='||p_in_withdraw||
                        ' Awd='||v_award_code||
                        ' Scn='||TO_CHAR(v_scenario)||
                        '), contact support';
  END IF;


  --------------------
  -- Update Database
  --------------------

  IF gv_error_message IS NULL THEN
    IF p_in_withdraw = 'R' THEN
      UPDATE students
         SET st_withdrawn_ind = NULL,
             st_withdrawn_date = NULL
       WHERE st_reg_no = p_in_reg_no
      ;
    ELSE
      ----------------------------------------------
      -- Determine if any NVQ units to be reported
      ----------------------------------------------
      v_nvq_units_reported_ind := 'N';
      OPEN c3;
      FETCH c3 INTO v_nvq_units_reported_ind;
      CLOSE c3;

      IF v_date_award_claimed = 'Y' THEN
        v_award_date := TO_DATE(p_in_award_date, 'MM/YYYY');
      ELSE
        v_award_date := NULL;
      END IF;

      IF v_withdrawn_ind = 'Y' THEN
        v_withdrawn_date := SYSDATE;
      ELSE
        v_withdrawn_date := NULL;
      END IF;

      IF v_nvq_units_reported_ind = 'N' THEN
        UPDATE students
           SET st_award_claim = v_award_claim,
               st_award_elig = v_award_elig,
               st_fallback = v_fallback,
               st_nops_reqd = DECODE(BTEC.FN_SUPPRESS_NOP_CAR(p_in_reg_no,'N'),'Y',NULL,v_nops_reqd),
               st_nvq_roa_code = v_nvq_roa_code,
               st_cred_trans_code = v_cred_trans_code,
               st_withdrawn_ind = v_withdrawn_ind,
               st_withdrawn_date = v_withdrawn_date,
               st_car_reqd = DECODE(BTEC.FN_SUPPRESS_NOP_CAR(p_in_reg_no,'C'),'Y',NULL,v_car_reqd),
               st_award_printed = v_award_date,
               st_fallback_cuc = NULL,
               st_over_grade = p_in_overall_result,
               st_year = 2
         WHERE st_reg_no = p_in_reg_no
        ;
	if v_title = 6932 and
  	  (v_db_scenario = '3' or v_db_scenario = '4')
	then
  	  UPDATE students
   	  SET 	 st_nops_reqd = null,
       		 st_car_reqd = null
 	  WHERE  st_reg_no = p_in_reg_no;
	end if;
        if v_title = 6932 and
           v_db_scenario = '2'
        then
          UPDATE students
           SET st_fallback = NULL,
               st_car_reqd = NULL,
               st_award_printed = NULL,
               st_fallback_cuc = NULL
         WHERE st_reg_no = p_in_reg_no;
        end if;
      END IF;
      IF v_nvq_units_reported_ind = 'Y' THEN
        UPDATE students
           SET st_award_claim = v_award_claim,
               st_award_elig = v_award_elig,
               st_fallback = v_fallback,
               st_nops_reqd = DECODE(BTEC.FN_SUPPRESS_NOP_CAR(p_in_reg_no,'N'),'Y',NULL,v_nops_reqd),
               st_cred_trans_code = v_cred_trans_code,
               st_withdrawn_ind = v_withdrawn_ind,
               st_withdrawn_date = v_withdrawn_date,
               st_car_reqd = DECODE(BTEC.FN_SUPPRESS_NOP_CAR(p_in_reg_no,'C'),'Y',NULL,v_car_reqd),
               st_award_printed = v_award_date,
               st_nvq_roa_code =
                 DECODE(v_nvq_roa_code2,
                        'N/A', st_nvq_roa_code,
                        v_nvq_roa_code2
                       ),
               st_fallback_cuc = v_fallback_cuc,
               st_over_grade = p_in_overall_result,
	       st_year = 2
         WHERE st_reg_no = p_in_reg_no
        ;
      END IF;
    END IF;

    ----------------------
    -- save audit record
    ----------------------

    IF p_in_claim_type = 'A' THEN
      v_trans_type := 19;
    END IF;
    IF p_in_claim_type = 'I' THEN
      v_trans_type := 18;
    END IF;
    IF p_in_claim_type = 'F' THEN
      v_trans_type := 16;
    END IF;
    IF p_in_withdraw = 'W' THEN
      v_trans_type := 17;
    END IF;
    IF p_in_withdraw = 'R' THEN
      v_trans_type := 20;
    END IF;
   -- EC2589
      commit;
    IF p_in_withdraw = 'A' THEN
      pr_eie_students_insert(p_in_reg_no, 'RES');
    END IF;
  END IF;

  -------------------
  -- PROCESSING END
  -------------------

  IF gv_error_message = NULL THEN
    gv_status := 0;
    gv_row_count := 1;
  ELSE
    ROLLBACK;
    gv_status := 1;
    gv_row_count := 0;
  END IF;

  pk_edx_shared.pr_log
    (p_user_id,
     gv_dml_type,
     p_ip_address,
     p_call_source,
     gv_status,                              -- status: 0 = OK, 1 = BAD
     gv_module_id,
     gv_in_request_params,
     gv_out_trans_values,
     gv_row_count,
     gv_error_message
    );

  p_out_error_message := gv_error_message;

EXCEPTION
  WHEN OTHERS THEN
    gv_error_message := SQLERRM;

    ROLLBACK;

    pk_edx_shared.pr_log
      (p_user_id,
       gv_dml_type,
       p_ip_address,
       p_call_source,
       1,                              -- status: 0 = OK, 1 = BAD
       gv_module_id,
       gv_in_request_params,
       gv_out_trans_values,
       0,
       gv_error_message
      );

    RAISE_APPLICATION_ERROR
      (-20000,
       gv_package_name||'.'||
       gv_module_name||', '||
       gv_error_message||', '||
       p_user_id||', '||
       p_ip_address||', '||
       p_call_source
      );

    p_out_error_message := gv_error_message;

END pr_u4_claim_candidate_prog;

-------------------------------------------------------------------------------
-- 1.23. Update licensed student details.
-------------------------------------------------------------------------------

PROCEDURE pr_u4_upd_lcnsd_stud_details
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_full_qual_issued	IN  VARCHAR2,				-- VALUES 'Y','N' or NULL only
   p_in_unit_cert_issued        IN  VARCHAR2,				-- VALUES 'Y','N' or NULL only
   p_in_withdrawn               IN  VARCHAR2,				-- VALUES 'Y','N' or NULL only
   p_in_btec_award_date         IN  VARCHAR2,				-- Format MM/YY
   p_in_accred_service		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  )
IS

BEGIN

  -- Do initial stuff

  gv_module_name := 'PR_U4_UPD_LCNSD_STUD_DETAILS';

  -- Validate user

  pk_edx_shared.pr_validate_user(p_user_id, p_ip_address);

  -- validate module, and get dml type (I/Q) and module number (id)

  pk_edx_shared.pr_validate_module(gv_module_name,
                                   gv_dml_type,
                                   gv_module_id
                                  );

  pk_edx_shared.pr_validate_source(p_call_source);

  gv_in_request_params :=
    'P1 = '||p_in_reg_no||' | '||
    'P2 = '||p_in_full_qual_issued||' | '||
    'P3 = '||p_in_unit_cert_issued||' | '||
    'P4 = '||p_in_withdrawn||' | '||
    'P5 = '||p_in_btec_award_date||' | '||
    'P6 = '||p_in_accred_service;

  ---------------------
  -- PROCESSING START
  ---------------------

  ------------------------
  -- Validate parameters
  ------------------------

  gv_error_message := NULL;

  if nvl(p_in_full_qual_issued,'Y') not in ('Y','N') and
     gv_error_message is null
  then
    gv_error_message := 'Invalid Award Claim';
  end if;

  if nvl(p_in_unit_cert_issued,'Y') not in ('Y','N') and
     gv_error_message is null
  then
    gv_error_message := 'Invalid Unit Claim';
  end if;

  if nvl(p_in_withdrawn,'Y') not in ('Y','N') and
     gv_error_message is null
  then
    gv_error_message := 'Invalid Withdrawal';
  end if;

  if p_in_btec_award_date is not null and
     gv_error_message is null
  then
    if substr(p_in_btec_award_date,1,2) not between '01' and '12' or
       substr(p_in_btec_award_date,3,1) <> '/' 			  or
       substr(p_in_btec_award_date,4,2) not between '00' and '99'
    then
      gv_error_message := 'Invalid Award date';
    end if;
  end if;

  if gv_error_message is null
  then
    if nvl(p_in_full_qual_issued,'N') = 'Y' and p_in_btec_award_date is null or
       nvl(p_in_full_qual_issued,'N') = 'N' and p_in_btec_award_date is not null
    then
      gv_error_message := 'Invalid Award Claim / Award Date combination';
    end if;
  end if;

  -- if 2 or more parameters are set to 'Y' or 'N' then invalid combination...
  -- BUT allow unit claim + withdrawal together

  if gv_error_message is null
  then
    if instr(p_in_full_qual_issued||p_in_unit_cert_issued||p_in_withdrawn,'Y',
	 instr(p_in_full_qual_issued||p_in_unit_cert_issued||p_in_withdrawn,'Y',1)+1) > 0 or
       instr(p_in_full_qual_issued||p_in_unit_cert_issued||p_in_withdrawn,'N',
	 instr(p_in_full_qual_issued||p_in_unit_cert_issued||p_in_withdrawn,'N',1)+1) > 0
    then
      if p_in_unit_cert_issued||p_in_withdrawn = 'YY' and
	 p_in_full_qual_issued is null
      then
	null;
      else
        gv_error_message := 'Invalid Claim / Withdrawal combination';
      end if;
    end if;
  end if;

  IF gv_error_message IS NULL
  then
    if p_in_full_qual_issued is not null
    then
     if p_in_accred_service = 'Y'
     then
      if p_in_full_qual_issued = 'Y'
      then
        UPDATE students
           SET st_award_elig	= 'Y',
	       st_award_claim   = 'Y',
	       st_award_printed = to_date(p_in_btec_award_date,'MM/YY'),
	       st_result_origin = 'W'
         WHERE st_reg_no		  = p_in_reg_no;
      end if;
      if p_in_full_qual_issued = 'N'
      then
        UPDATE students
           SET st_award_elig	= null,
	       st_award_claim   = null,
	       st_award_printed = null
         WHERE st_reg_no		  = p_in_reg_no
	 and   st_cert_no		  is null;
      end if;
     else
      if p_in_full_qual_issued = 'Y'
      then
	-- dbms_output.put_line('Claim AWARD');
        UPDATE students
           SET st_award_elig	= 'Y',
	       st_award_printed = to_date(p_in_btec_award_date,'MM/YY'),
	       st_cert_no	= 'UNIVCERT',
	       st_result_origin = 'W'
         WHERE st_reg_no		  = p_in_reg_no
	   AND st_course_id	     IS NOT NULL
           AND st_delete                 IS NULL
           AND st_expired_ind            IS NULL
           AND nvl(st_licensed_ind,'N')   = 'Y'
           AND nvl(st_university_ind,'N') = 'Y';
      end if;
      if p_in_full_qual_issued = 'N'
      then
	-- dbms_output.put_line('Remove AWARD Claim');
        UPDATE students
           SET st_award_elig	= null,
	       st_award_printed = null,
	       st_cert_no	= null
         WHERE st_reg_no		  = p_in_reg_no
	   AND st_course_id	     IS NOT NULL
           AND st_delete                 IS NULL
           AND st_expired_ind            IS NULL
           AND nvl(st_licensed_ind,'N')   = 'Y'
           AND nvl(st_university_ind,'N') = 'Y';
      end if;
     end if;
    end if;

    if p_in_unit_cert_issued is not null
    then
      if p_in_unit_cert_issued = 'Y'
      then
	-- dbms_output.put_line('Claim UNIT');
        UPDATE students
           SET st_nops_reqd	= 'P',
	       st_result_origin = 'W'
         WHERE st_reg_no		    = p_in_reg_no
           AND st_delete                   IS NULL
           AND st_expired_ind              IS NULL
           AND nvl(st_licensed_ind,'N')     = 'Y'
           AND nvl(st_university_ind,'N')   = 'Y';
      end if;
      if p_in_unit_cert_issued = 'N'
      then
	-- dbms_output.put_line('Remove UNIT Claim');
        UPDATE students
           SET st_nops_reqd = null
         WHERE st_reg_no		    = p_in_reg_no
           AND st_delete                   IS NULL
           AND st_expired_ind              IS NULL
           AND nvl(st_licensed_ind,'N')     = 'Y'
           AND nvl(st_university_ind,'N')   = 'Y';
      end if;
    end if;

    if p_in_withdrawn is not null
    then
      if p_in_withdrawn = 'Y'
      then
	-- dbms_output.put_line('Withdraw Student');
       if p_in_accred_service = 'Y'
       then
        UPDATE students
           SET st_withdrawn_ind	 = 'Y',
	       st_withdrawn_date = trunc(sysdate)
         WHERE st_reg_no		    = p_in_reg_no;
       else
        UPDATE students
           SET st_withdrawn_ind	 = 'Y',
	       st_withdrawn_date = trunc(sysdate)
         WHERE st_reg_no		    = p_in_reg_no
           AND st_delete                   IS NULL
           AND st_expired_ind              IS NULL
           AND nvl(st_licensed_ind,'N')     = 'Y'
           AND nvl(st_university_ind,'N')   = 'Y';
       end if;
      end if;
      if p_in_withdrawn = 'N'
      then
	-- dbms_output.put_line('Un-withdraw Student');
       if p_in_accred_service = 'Y'
       then
        UPDATE students
           SET st_withdrawn_ind	 = null,
	       st_withdrawn_date = null
         WHERE st_reg_no		    = p_in_reg_no;
       else
        UPDATE students
           SET st_withdrawn_ind	 = null,
	       st_withdrawn_date = null
         WHERE st_reg_no		    = p_in_reg_no
           AND st_delete                   IS NULL
           AND st_expired_ind              IS NULL
           AND nvl(st_licensed_ind,'N')     = 'Y'
           AND nvl(st_university_ind,'N')   = 'Y';
       end if;
      end if;
    end if;
  END IF;

  IF gv_error_message IS NULL THEN
     gv_status 	 	 := 0;
     gv_row_count 	 := 1;
     gv_out_trans_values := 'Student updated';
  ELSE
     p_out_error_message := gv_error_message;
     gv_status	 	 := 1;
     gv_row_count 	 := 0;
     gv_out_trans_values := NULL;
  END IF;

  -------------------
  -- PROCESSING END
  -------------------

-- EC2589
   commit;

  pk_edx_shared.pr_log
    (p_user_id,
     gv_dml_type,
     p_ip_address,
     p_call_source,
     gv_status,                              -- status: 0 = OK, 1 = BAD
     gv_module_id,
     gv_in_request_params,
     gv_out_trans_values,
     gv_row_count,
     gv_error_message
    );

EXCEPTION
  WHEN OTHERS THEN
     gv_error_message := SQLERRM;

     ROLLBACK;

     pk_edx_shared.pr_log
      (p_user_id,
       gv_dml_type,
       p_ip_address,
       p_call_source,
       1,                              -- status: 0 = OK, 1 = BAD
       gv_module_id,
       gv_in_request_params,
       gv_out_trans_values,
       0,
       gv_error_message
      );

    RAISE_APPLICATION_ERROR
      (-20000,
       gv_package_name||'.'||
       gv_module_name||', '||
       gv_error_message||', '||
       p_user_id||', '||
       p_ip_address||', '||
       p_call_source
      );

END pr_u4_upd_lcnsd_stud_details;

-------------------------------------------------------------------------------
-- 1.99. Master procedure to handle calling of 1.21 multiple times followed
--       by 1.22 without committing data in the interim
-------------------------------------------------------------------------------

PROCEDURE pr_iu4_add_units_n_claim_prog
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_prog_code               IN  VARCHAR2,
   p_in_unit_codes_and_results  IN  VARCHAR2,  -- in format "code:result: ..."
   p_in_award_date		IN  VARCHAR2,  -- in format MM/YYYY
   p_in_overall_result		IN  VARCHAR2,
   p_in_claim_type		IN  VARCHAR2,
   p_in_withdraw		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  )

IS

  v_error_message	VARCHAR2(80);
  v_colon_posn          NUMBER;
  v_colon_posn2         NUMBER;
  v_start_posn          NUMBER;
  v_length              NUMBER;
  v_length_sub_string   NUMBER;
  v_sub_string		VARCHAR2(20);
  v_unit_code           VARCHAR2(8);
  v_unit_result		VARCHAR2(1);
  v_reg_no		VARCHAR2(7);
  v_count		NUMBER(3);

BEGIN

  gv_error_message := NULL;


  v_start_posn := 1;
  v_length := nvl(LENGTH(p_in_unit_codes_and_results),0);
  v_colon_posn := 0;
  v_count := 0;

  if	v_length = 0
  then
	v_reg_no := pk_edx_mas1.fn_scheme_reg_no(p_in_reg_no,'BTEC','*',upper(p_in_prog_code));
	if	v_reg_no = '???????'
	then	gv_error_message := 'Cannot Determine Scheme Component Reg. No';
		goto fini;
	end if;
  	pr_validate_student(v_reg_no,p_in_claim_type,p_in_withdraw);
  end if;

  WHILE gv_error_message IS NULL AND
        v_colon_posn < v_length LOOP

    -- Get unit

    v_count := v_count + 1;
    v_colon_posn := INSTR(p_in_unit_codes_and_results, ':', v_start_posn);
    IF v_colon_posn = 0 THEN
      v_colon_posn := v_length + 1;
    END IF;
    v_length_sub_string := v_colon_posn - v_start_posn;
    IF v_length_sub_string > 0 THEN
      v_unit_code :=
        SUBSTR(p_in_unit_codes_and_results,
               v_start_posn,
               v_length_sub_string
              );

	if	v_count = 1
	then
		v_reg_no := pk_edx_mas1.fn_scheme_reg_no(p_in_reg_no,'BTEC',v_unit_code,upper(p_in_prog_code));
		if	v_reg_no = '???????'
		then	gv_error_message := 'Cannot Determine Scheme Component Reg. No';
			goto fini;
		end if;
  		pr_validate_student(v_reg_no,p_in_claim_type,p_in_withdraw);
	end if;

    ELSE
      v_unit_code := NULL;
    END IF;

    -- Get result

    v_start_posn := v_colon_posn + 1;
    v_colon_posn := INSTR(p_in_unit_codes_and_results, ':', v_start_posn);
    IF v_colon_posn = 0 THEN
      v_colon_posn := v_length + 1;
    END IF;
    v_length_sub_string := v_colon_posn - v_start_posn;
    IF v_length_sub_string > 0 THEN
      v_unit_result :=
        SUBSTR(p_in_unit_codes_and_results,
               v_start_posn,
               v_length_sub_string
              );
    ELSE
      v_unit_result := NULL;
    END IF;


    IF v_unit_code IS NOT NULL
    THEN
      pk_edx_eie_udi4.pr_iu4_add_candidate_unit
        (p_user_id,
         p_ip_address,
         p_call_source,
         v_reg_no,
         v_unit_code,
         v_unit_result,
         gv_error_message
        )
      ;
    END IF;

    v_start_posn := v_colon_posn + 1;

  END LOOP; /* while */

  IF gv_error_message IS NULL THEN
    pk_edx_shared3.pr_update_audit_trail
      (v_reg_no,
       gv_error_message
      );
  END IF;

  IF gv_error_message IS NULL THEN
    IF p_in_award_date IS NOT NULL OR
       p_in_overall_result IS NOT NULL OR
       p_in_claim_type IS NOT NULL OR
       p_in_withdraw IS NOT NULL THEN
      pk_edx_eie_udi4.pr_u4_claim_candidate_prog
        (p_user_id,
         p_ip_address,
         p_call_source,
         v_reg_no,
         p_in_award_date,
         p_in_overall_result,
         p_in_claim_type,
         p_in_withdraw,
         p_out_error_message
       );
    END IF;
  END IF;

  <<fini>>
  p_out_error_message := gv_error_message;

  IF gv_error_message IS NOT NULL THEN
    ROLLBACK;
  END IF;

    -------------------
        -- PROCESSING END
        -------------------

        IF      p_out_error_message     is NULL
        THEN
                gv_status               := 0;
        ELSE
                gv_status               := 1;
        END IF;

        pk_edx_eie_udi4.pr_log_autonomous
                (
                        p_user_id
                ,       gv_dml_type
                ,       p_ip_address
                ,       p_call_source
                ,       gv_status               -- status: 0 = OK, 1 = BAD
                ,       gv_module_id
                ,       gv_in_request_params
                ,       gv_out_trans_values
                ,       gv_row_count
                ,       gv_error_message
                );

END pr_iu4_add_units_n_claim_prog;

PROCEDURE test_st_unit_exempt_claim
          (p_user_id               IN   VARCHAR2,
           p_ip_address            IN   VARCHAR2,
           p_call_source           IN   NUMBER,
           p_in_programme_code     IN  VARCHAR2,
           p_in_reg_no             IN  VARCHAR2,
           p_in_unit_code          IN  VARCHAR2,
           p_in_exempt_ind         IN  VARCHAR2,
           p_in_un_exempt_ind      IN  VARCHAR2,
           p_in_declaration_ind    IN  VARCHAR2)
IS
 p_out_error_message         varchar2(500);

BEGIN
  pk_edx_eie_udi4.pr_udi4_st_unit_exempt_claim
     (p_user_id              ,
           p_ip_address          ,
           p_call_source          ,
           p_in_programme_code    , -- SIA and SQA Course Ids
           p_in_reg_no            ,
           p_in_unit_code        ,
           p_in_exempt_ind        , -- NULL or 'YES'
           p_in_un_exempt_ind     ,
           p_in_declaration_ind  ,
           p_out_error_message    );


            DBMS_OUTPUT.PUT_LINE('** Input Parameters **');
            DBMS_OUTPUT.PUT_LINE(gv_in_request_params);
            DBMS_OUTPUT.PUT_LINE('** Output Parameters **');
            DBMS_OUTPUT.PUT_LINE(p_out_error_message);

END test_st_unit_exempt_claim;

PROCEDURE test_validate_ons_candidate
               (p_user_id               IN   VARCHAR2,
                p_ip_address            IN   VARCHAR2,
                p_call_source           IN   NUMBER,
                p_in_centre             IN  VARCHAR2,
                p_in_qual_code          IN  VARCHAR2,
                p_in_programme_code     IN  VARCHAR2,
                p_in_reg_no             IN  VARCHAR2,
                p_in_forenames          IN  VARCHAR2,
                p_in_lastname           IN  VARCHAR2,
                p_in_dob                IN  DATE,
                p_in_gender             IN  VARCHAR2)
IS
p_out_error_message         varchar2(500);
p_out_reg_no                students.st_reg_no%TYPE;
p_out_registration_required VARCHAR2(5) := 'No';

BEGIN

  pk_edx_eie_udi4.pr_iu4_validate_ons_candidate(p_user_id               ,
                p_ip_address           ,
                p_call_source          ,
                p_in_centre            ,
                p_in_qual_code         ,
                p_in_programme_code    ,
                p_in_reg_no            ,
                p_in_forenames         ,
                p_in_lastname          ,
                p_in_dob               ,
                p_in_gender            ,
                p_out_error_message    ,
                p_out_reg_no           ,
                p_out_registration_required);


            DBMS_OUTPUT.PUT_LINE('** Input Parameters **');
            DBMS_OUTPUT.PUT_LINE(gv_in_request_params);
            DBMS_OUTPUT.PUT_LINE('** Output Parameters **');
            DBMS_OUTPUT.PUT_LINE(p_out_error_message|| ','||p_out_reg_no|| ','||p_out_registration_required);

END test_validate_ons_candidate;

PROCEDURE test_sia_bulk_images
IS
	i			INTEGER			:=	0;

	p_user_id		VARCHAR2(12)			:=	'TESTER';
	p_ip_address		VARCHAR2(12)			:=	'1.1.1.1';
	p_call_source		NUMBER				:=	2;
	p_in_call_type		VARCHAR2(1)			:=	'V';
	p_in_reg_no		VARCHAR2(7)			:=	'AG14127';
	p_in_photo_image	BLOB;
	p_in_signature_image	BLOB;
	p_in_internal_user	VARCHAR2(1)			:=	'Y';
	p_out_student_forenames	STUDENTS.ST_FORENAMES%TYPE	:=	NULL;
	p_out_student_surname	STUDENTS.ST_SURNAME%TYPE	:=	NULL;
	p_out_centre_id		VARCHAR2(6)			:=	NULL;
	p_out_certificated_ind	VARCHAR2(1)			:=	NULL;
	p_out_deleted_ind	VARCHAR2(1)			:=	NULL;
	p_out_invalid_reg_ind	VARCHAR2(1)			:=	NULL;
	p_out_sia_ind		VARCHAR2(1)			:=	NULL;
	p_out_photo_ind		VARCHAR2(1)			:=	NULL;
	p_out_signature_ind	VARCHAR2(1)			:=	NULL;
	p_out_error_message	VARCHAR2(200)			:=	NULL;

BEGIN
	BEGIN
		select
			spho_face
		into
			p_in_photo_image
		from
			student_photos
		where
			spho_st_reg_no		=	'AZ45954';
	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			NULL;
	END;

	BEGIN
		select
			ssig_signature
		into
			p_in_signature_image
		from
			student_signatures
		where
			ssig_st_reg_no		=	'AZ45954';
	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			NULL;
	END;

	pk_edx_eie_udi4.pr_udi4_sia_bulk_images
		(
			p_user_id
		,	p_ip_address
		,	p_call_source
		,	p_in_call_type
		,	p_in_reg_no
		,	p_in_photo_image
		,	p_in_signature_image
		,	p_in_internal_user
		,	p_out_student_forenames
		,	p_out_student_surname
		,	p_out_centre_id
		,	p_out_sia_ind
		,	p_out_photo_ind
		,	p_out_signature_ind
		,	p_out_deleted_ind
		,	p_out_certificated_ind
		,	p_out_invalid_reg_ind
		,	p_out_error_message
		);

	DBMS_OUTPUT.PUT_LINE('** Input Parameters **');
	DBMS_OUTPUT.PUT_LINE('Call Type='||p_in_call_type);
	DBMS_OUTPUT.PUT_LINE('RegNo='||p_in_reg_no);
	DBMS_OUTPUT.PUT_LINE('IntUser='||p_in_internal_user);
	DBMS_OUTPUT.PUT_LINE('** Output Parameters **');
	DBMS_OUTPUT.PUT_LINE('Forenames='||p_out_student_forenames);
	DBMS_OUTPUT.PUT_LINE('Surname='||p_out_student_surname);
	DBMS_OUTPUT.PUT_LINE('Centre='||p_out_centre_id);
	DBMS_OUTPUT.PUT_LINE('SIA='||p_out_sia_ind);
	DBMS_OUTPUT.PUT_LINE('Photo='||p_out_photo_ind);
	DBMS_OUTPUT.PUT_LINE('Signature='||p_out_signature_ind);
	DBMS_OUTPUT.PUT_LINE('Deleted='||p_out_deleted_ind);
	DBMS_OUTPUT.PUT_LINE('Certificated='||p_out_certificated_ind);
	DBMS_OUTPUT.PUT_LINE('InvalidRegno='||p_out_invalid_reg_ind);
	DBMS_OUTPUT.PUT_LINE('Error Message='||rtrim(p_out_error_message));
END test_sia_bulk_images;

PROCEDURE test_sia_upd_upload_dets
IS
	i			INTEGER			:=	0;

	p_user_id		VARCHAR2(12)		:=	'TESTER';
	p_ip_address		VARCHAR2(12)		:=	'1.1.1.1';
	p_call_source		NUMBER			:=	2;
	p_in_batch_number	NUMBER			:=	1234;
	p_in_upload_date	DATE			:=	SYSDATE;
	p_out_error_message	VARCHAR2(200)		:=	NULL;

	p_in_reg_no		st_reg_no_tbl;
	p_out_error_reg_no	st_reg_no_tbl;

BEGIN
	p_in_reg_no(1)	:=	'X000001';
	p_in_reg_no(2)	:=	'X000002';
	p_in_reg_no(3)	:=	'X000003';

	pk_edx_eie_udi4.pr_udi4_sia_upd_upload_dets
		(
			p_user_id
		,	p_ip_address
		,	p_call_source
		,	p_in_batch_number
		,	p_in_upload_date
		,	p_in_reg_no
		,	p_out_error_reg_no
		,	p_out_error_message
		);

	DBMS_OUTPUT.PUT_LINE('** Input Parameters **');
	DBMS_OUTPUT.PUT_LINE('Batch Number='||to_char(p_in_batch_number));
	DBMS_OUTPUT.PUT_LINE('Upload Date='||to_char(p_in_upload_date,'dd/mm/yyyy hh24:mi:ss'));
	i := 1;
	BEGIN
		WHILE p_in_reg_no(i) is not NULL
		LOOP
			DBMS_OUTPUT.PUT_LINE	(
							'Reg No('||to_char(i)||')='||p_in_reg_no(i)
						);
			i := i+1;
		END LOOP;
	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			NULL;
	END;
	DBMS_OUTPUT.PUT_LINE('** Output Parameters **');
	i := 1;
	BEGIN
		WHILE p_out_error_reg_no(i) is not NULL
		LOOP
			DBMS_OUTPUT.PUT_LINE	(
							'Reg No('||to_char(i)||')='||p_out_error_reg_no(i)
						);
			i := i+1;
		END LOOP;
	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			NULL;
	END;
	DBMS_OUTPUT.PUT_LINE('Error Message='||rtrim(p_out_error_message));
END test_sia_upd_upload_dets;

PROCEDURE test_sia_update_status(p_in_reg_no st_reg_no_tbl)
IS
	i			INTEGER			:=	0;

	p_user_id		VARCHAR2(12)		:=	'TESTER';
	p_ip_address		VARCHAR2(12)		:=	'1.1.1.1';
	p_call_source		NUMBER			:=	2;
	p_out_error_message	VARCHAR2(200)		:=	NULL;

	p_out_error_reg_no	st_reg_no_tbl;

BEGIN
	pk_edx_eie_udi4.pr_udi4_sia_update_status
		(
			p_user_id
		,	p_ip_address
		,	p_call_source
		,	p_in_reg_no
		,	p_out_error_reg_no
		,	p_out_error_message
		);

	DBMS_OUTPUT.PUT_LINE('** Input Parameters **');
	BEGIN
		i := 1;
		WHILE p_in_reg_no(i) is not NULL
		LOOP
			DBMS_OUTPUT.PUT_LINE('Reg No('||to_char(i)||')='||p_in_reg_no(i));
			i := i + 1;
		END LOOP;
	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			NULL;
	END;

	DBMS_OUTPUT.PUT_LINE('** Output Parameters **');
	BEGIN
		i := 1;
		WHILE p_out_error_reg_no(i) is not NULL
		LOOP
			DBMS_OUTPUT.PUT_LINE('Reg No('||to_char(i)||')='||p_out_error_reg_no(i));
			i := i + 1;
		END LOOP;
	EXCEPTION
		WHEN	NO_DATA_FOUND
		THEN
			NULL;
	END;

	DBMS_OUTPUT.PUT_LINE('Error Message='||rtrim(p_out_error_message));
END test_sia_update_status;

PROCEDURE test_delete_registration ( p_in_reg_no                  IN  VARCHAR2,
                                     p_in_prog_code               IN  VARCHAR2)
IS
	i			INTEGER			:=	0;

   --	p_user_id		VARCHAR2(12)		:=	'TESTER';

        p_user_id               VARCHAR2(12)            :=      'TOMLIN_E';
	p_ip_address		VARCHAR2(12)		:=	'1.1.1.1';
	p_call_source		NUMBER			:=	1;
	p_out_error_message	VARCHAR2(500)		:=	NULL;

BEGIN
	pk_edx_eie_udi4.delete_registration
		(
			p_user_id
		,	p_ip_address
		,	p_call_source
		,	p_in_reg_no
		,	p_in_prog_code
		,	p_out_error_message
		);

    DBMS_OUTPUT.PUT_LINE('** Input Parameters **');
            DBMS_OUTPUT.PUT_LINE(gv_in_request_params);
            DBMS_OUTPUT.PUT_LINE('** Output Parameters **');
            DBMS_OUTPUT.PUT_LINE(p_out_error_message);

END test_delete_registration;


   -- EC4940 Summer 2021 Results Delivery Starts
PROCEDURE pr_iu4_add_units_n_claim_ra
  (p_user_id                    IN  VARCHAR2,
   p_ip_address                 IN  VARCHAR2,
   p_call_source                IN  NUMBER,
   p_in_reg_no                  IN  VARCHAR2,
   p_in_prog_code               IN  VARCHAR2,
   p_in_unit_codes_and_results  IN  VARCHAR2,  -- in format "code:result:Indicator ..."
   p_in_award_date		IN  VARCHAR2,  -- in format MM/YYYY
   p_in_overall_result		IN  VARCHAR2,
   p_in_claim_type		IN  VARCHAR2,
   p_in_withdraw		IN  VARCHAR2,
   p_out_error_message          OUT VARCHAR2
  )

IS

  v_error_message	VARCHAR2(80);
  v_colon_posn          NUMBER;
  v_colon_posn2         NUMBER;
  v_start_posn          NUMBER;
  v_length              NUMBER;
  v_length_sub_string   NUMBER;
  v_sub_string		VARCHAR2(20);
  v_unit_code           VARCHAR2(8);
  v_unit_result		VARCHAR2(1);
  v_reg_no		VARCHAR2(7);
  v_count		NUMBER(3);
  v_unit_ra_ind         VARCHAR2(1);

BEGIN

  gv_error_message := NULL;


  v_start_posn := 1;
  v_length := nvl(LENGTH(p_in_unit_codes_and_results),0);
  v_colon_posn := 0;
  v_count := 0;

  if	v_length = 0
  then
	v_reg_no := pk_edx_mas1.fn_scheme_reg_no(p_in_reg_no,'BTEC','*',upper(p_in_prog_code));
	if	v_reg_no = '???????'
	then	gv_error_message := 'Cannot Determine Scheme Component Reg. No';
		goto fini;
	end if;
  	pr_validate_student(v_reg_no,p_in_claim_type,p_in_withdraw);
  end if;

  WHILE gv_error_message IS NULL AND
        v_colon_posn < v_length LOOP

    -- Get unit

    v_count := v_count + 1;
    v_colon_posn := INSTR(p_in_unit_codes_and_results, ':', v_start_posn);
    IF v_colon_posn = 0 THEN
      v_colon_posn := v_length + 1;
    END IF;
    v_length_sub_string := v_colon_posn - v_start_posn;
    IF v_length_sub_string > 0 THEN
      v_unit_code :=
        SUBSTR(p_in_unit_codes_and_results,
               v_start_posn,
               v_length_sub_string
              );

	if	v_count = 1
	then
		v_reg_no := pk_edx_mas1.fn_scheme_reg_no(p_in_reg_no,'BTEC',v_unit_code,upper(p_in_prog_code));
		if	v_reg_no = '???????'
		then	gv_error_message := 'Cannot Determine Scheme Component Reg. No';
			goto fini;
		end if;
  		pr_validate_student(v_reg_no,p_in_claim_type,p_in_withdraw);
	end if;

    ELSE
      v_unit_code := NULL;
    END IF;

    -- Get result

    v_start_posn := v_colon_posn + 1;
    v_colon_posn := INSTR(p_in_unit_codes_and_results, ':', v_start_posn);
    IF v_colon_posn = 0 THEN
      v_colon_posn := v_length + 1;
    END IF;
    v_length_sub_string := v_colon_posn - v_start_posn;
    IF v_length_sub_string > 0 THEN
      v_unit_result :=
        SUBSTR(p_in_unit_codes_and_results,
               v_start_posn,
               v_length_sub_string
              );
    ELSE
      v_unit_result := NULL;
    END IF;
    v_start_posn := v_colon_posn+1;
    v_colon_posn := instr(p_in_unit_codes_and_results,':',v_start_posn);
    IF v_colon_posn = 0 THEN
      v_colon_posn := v_length + 1;
    END IF;
    v_length_sub_string := v_colon_posn - v_start_posn;
    IF v_length_sub_string > 0 THEN   
       v_unit_ra_ind := SUBSTR(p_in_unit_codes_and_results,
                                           v_start_posn,
                                           v_length_sub_string);
	ELSE
      v_unit_ra_ind := NULL;
    END IF;

    IF v_unit_code IS NOT NULL AND v_unit_ra_ind IS NOT NULL THEN
      pk_edx_eie_udi4.pr_iu4_add_candidate_unit
        (p_user_id,
         p_ip_address,
         p_call_source,
         v_reg_no,
         v_unit_code,
         v_unit_result,
         gv_error_message,
         v_unit_ra_ind
        )
      ;
    END IF;
    v_start_posn := v_colon_posn + 1;

  END LOOP; /* while */

  IF gv_error_message IS NULL THEN
    pk_edx_shared3.pr_update_audit_trail
      (v_reg_no,
       gv_error_message
      );
  END IF;

  IF gv_error_message IS NULL THEN
    IF p_in_award_date IS NOT NULL OR
       p_in_overall_result IS NOT NULL OR
       p_in_claim_type IS NOT NULL OR
       p_in_withdraw IS NOT NULL THEN
      pk_edx_eie_udi4.pr_u4_claim_candidate_prog
        (p_user_id,
         p_ip_address,
         p_call_source,
         v_reg_no,
         p_in_award_date,
         p_in_overall_result,
         p_in_claim_type,
         p_in_withdraw,
         p_out_error_message
       );
    END IF;
  END IF;

  <<fini>>
  p_out_error_message := gv_error_message;

  IF gv_error_message IS NOT NULL THEN
    ROLLBACK;
  END IF;

    -------------------
        -- PROCESSING END
        -------------------

        IF      p_out_error_message     is NULL
        THEN
                gv_status               := 0;
        ELSE
                gv_status               := 1;
        END IF;

        pk_edx_eie_udi4.pr_log_autonomous
                (
                        p_user_id
                ,       gv_dml_type
                ,       p_ip_address
                ,       p_call_source
                ,       gv_status               -- status: 0 = OK, 1 = BAD
                ,       gv_module_id
                ,       gv_in_request_params
                ,       gv_out_trans_values
                ,       gv_row_count
                ,       gv_error_message
                );

END pr_iu4_add_units_n_claim_ra;
   -- EC4940 Summer 2021 Results Delivery Ends

END pk_edx_eie_udi4;
/