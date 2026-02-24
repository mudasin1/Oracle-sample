create or replace PACKAGE     pk_edx_shared3 AS

-----------------------------------------------------------------------
--   PK_EDX_SHARED3
--   Package containing shared procedures for the EDX suite
-----------------------------------------------------------------------
--  WHO  WHEN    WHAT            IN/OUT PARAMETERS
--
--  GW   131004  Initiation
--  GW   141004  To provide routines for Edexcel Online to manage fee
--               maintenance
--  MB   111007  EC245 - change to pr_insert_student_unit
--
------------------------------------------------------------------------------

PROCEDURE pr_insert_nvq_stud_comp_unit
  (p_in_reg_no		        IN  VARCHAR2,
   p_in_unit_id			IN  VARCHAR2,
   p_in_achieved_year		IN  VARCHAR2,
   p_out_error_message		OUT VARCHAR2
  );

PROCEDURE pr_insert_student_unit
  (p_in_reg_no			IN  VARCHAR2,
   p_in_unit_id			IN  VARCHAR2,
   p_in_grade_1			IN  VARCHAR2,
   p_in_date			IN  VARCHAR2, -- YY
   p_out_error_message    	OUT VARCHAR2,
   -- EC4940 Summer 2021 Results Delivery Starts
   p_in_unit_ra_ind     IN  VARCHAR2 DEFAULT NULL
   -- EC4940 Summer 2021 Results Delivery Ends
);

PROCEDURE pr_update_audit_trail
  (p_in_reg_no			IN  VARCHAR2,
   p_out_error_message		OUT VARCHAR2
  );

END pk_edx_shared3;
/

create or replace PACKAGE BODY     pk_edx_shared3 AS

-------------------------------------------------------------------------------
-- Global Variables
-------------------------------------------------------------------------------

-- there aren't any global variables

-- NB only need to use following procedure to get invoice id if
-- unit being deleted
-- may not need to use

/*
procedure pr_get_unit_invo_id (p_unit_id    in     varchar2,
                               p_unit_type  in     varchar2,
                               p_invo_id    in out varchar2) is
  --
  -- Check whether a unit that has been deleted was added as an
  -- adjustment rather than as part of the students original
  -- registration.
  --
  cursor c1 is
    select ia.iatr_invo_id
    from   invoice_audit_student_units iu,
           invoice_audit_trails        ia
    where  ia.iatr_st_reg_no = :one.st_reg_no
    and    ia.iatr_adjustment_type = 'U'
    and    ia.iatr_invo_id is not null
    and    iu.iasu_iatr_id = ia.iatr_id
    and    nvl (iu.iasu_unit_code, '@') =
                 decode (p_unit_type, 'B', p_unit_id, '@')
    and    nvl (iu.iasu_guni_id, '@') =
                 decode (p_unit_type, 'G', p_unit_id, '@')
    and    nvl (iu.iasu_ncun_id, '@') =
                 decode (p_unit_type, 'N', p_unit_id, '@')
    and    ia.iatr_adjustment_date =
                 (select max (ia2.iatr_adjustment_date)
                  from   invoice_audit_student_units iu2,
                         invoice_audit_trails        ia2
                  where  ia2.iatr_st_reg_no = ia.iatr_st_reg_no
                  and    ia2.iatr_adjustment_type =
                                     ia.iatr_adjustment_type
                  and    ia2.iatr_invo_id is not null
                  and    iu2.iasu_iatr_id = ia2.iatr_id
                  and    nvl (iu2.iasu_unit_code, '@') =
                                  nvl (iu.iasu_unit_code, '@')
                  and    nvl (iu2.iasu_guni_id, '@') =
                                  nvl (iu.iasu_guni_id, '@')
                  and    nvl (iu2.iasu_ncun_id, '@') =
                                  nvl (iu.iasu_ncun_id, '@'));
  --
begin
  --
  open c1;
  fetch c1
  into  p_invo_id;
  if c1%notfound then
    p_invo_id := :one.st_invo_id;
  end if;
  close c1;
  --
end;
*/

PROCEDURE pr_maintain_fees
  (p_in_reg_no			IN  VARCHAR2,
   p_in_unit_id			IN  VARCHAR2,
   p_in_nvq_unit_id		IN  VARCHAR2,
   p_in_fm_type			IN  VARCHAR2,
   p_out_error_message		OUT VARCHAR2
  )
IS

  v_reg_type		VARCHAR2(1);
  v_scheme_reg_no	VARCHAR2(7);
  v_invoice		VARCHAR2(6);

  CURSOR c1 IS
  SELECT st_reg_type,
         st_scheme_reg_no,
         st_invoice
    FROM students
   WHERE st_reg_no = p_in_reg_no
  ;

BEGIN
  OPEN c1;
  FETCH c1 INTO v_reg_type,
                v_scheme_reg_no,
                v_invoice;
  CLOSE c1;

  IF v_scheme_reg_no IS NULL AND
     v_reg_type = 'I' THEN
    INSERT INTO fee_maint
      (fm_invoice,
       fm_invoice_system,
       fm_reg_no,
       fm_unit_id,
       fm_nvq_unit_id,
       fm_type,
       fm_date
      )
    VALUES
      (v_invoice,
       'B',
       p_in_reg_no,
       p_in_unit_id,
       p_in_nvq_unit_id,
       p_in_fm_type,  -- U=update/insert, X=delete
       TRUNC(SYSDATE)
      )
    ;
  END IF;
EXCEPTION
  WHEN OTHERS THEN
    p_out_error_message :=
      'pr_maintain_fees:'||
      SQLERRM;
END pr_maintain_fees;

PROCEDURE pr_insert_student_unit
  (p_in_reg_no			IN  VARCHAR2,
   p_in_unit_id			IN  VARCHAR2,
   p_in_grade_1			IN  VARCHAR2,
   p_in_date			IN  VARCHAR2, -- YY
   p_out_error_message    	OUT VARCHAR2,
   -- EC4940 Summer 2021 Results Delivery Starts
   p_in_unit_ra_ind     IN  VARCHAR2 DEFAULT NULL
   -- EC4940 Summer 2021 Results Delivery Ends
)
IS
  -- EC245
  v_type                VARCHAR2(1);
  v_qca_code            units.un_qca_code%type;
  v_aw_code_62_85       approval_awards.aw_award_code%type;
  v_course_id           students.st_course_id%TYPE;
  ac_code               VARCHAR2(2);

  CURSOR c_un IS
  SELECT NVL(un_type, ' '),
         un_qca_code
  FROM units
  WHERE un_unit_code = p_in_unit_id;

  CURSOR c_course IS
  SELECT st_course_id
  FROM students
  WHERE st_reg_no = p_in_reg_no;

  CURSOR get_ac (p_award_id varchar2) IS
  SELECT aw_award_code
  FROM approval_awards
  WHERE aw_course_number = p_award_id;

BEGIN
  -- EC245
  OPEN c_un;
  FETCH c_un INTO v_type,
                  v_qca_code;
  CLOSE c_un;

  OPEN c_course;
  FETCH c_course INTO v_course_id;
  CLOSE c_course;

  -- EC245
  FOR v_rec IN get_ac (v_course_id) LOOP
      ac_code := v_rec.aw_award_code;
  END LOOP;

  v_aw_code_62_85 := ac_code;

  -- EC245
  IF v_type = '5' AND
     v_qca_code IS NOT NULL AND
     v_aw_code_62_85 IN ('62','85')
  THEN
    IF p_in_grade_1 IN ('M','D')
    THEN
      INSERT INTO student_units
      (su_reg_no,
       su_unit_id,
       su_grade_1,
       su_date,
   -- EC4940 Summer 2021 Results Delivery Starts
       su_ra_ind1	
   -- EC4940 Summer 2021 Results Delivery Ends
      )
      VALUES
      (p_in_reg_no,
       p_in_unit_id,
       'P',
       p_in_date,
   -- EC4940 Summer 2021 Results Delivery Starts
       p_in_unit_ra_ind
   -- EC4940 Summer 2021 Results Delivery Ends
      );
    ELSE
      INSERT INTO student_units
      (su_reg_no,
       su_unit_id,
       su_grade_1,
       su_date,
   -- EC4940 Summer 2021 Results Delivery Starts
       su_ra_ind1
   -- EC4940 Summer 2021 Results Delivery Ends
      )
      VALUES
      (p_in_reg_no,
       p_in_unit_id,
       p_in_grade_1,
       p_in_date,
   -- EC4940 Summer 2021 Results Delivery Starts
       p_in_unit_ra_ind
   -- EC4940 Summer 2021 Results Delivery Ends
      );
    END IF;
  ELSE
      INSERT INTO student_units
      (su_reg_no,
       su_unit_id,
       su_grade_1,
       su_date,
   -- EC4940 Summer 2021 Results Delivery Starts
       su_ra_ind1
   -- EC4940 Summer 2021 Results Delivery Ends
      )
      VALUES
      (p_in_reg_no,
       p_in_unit_id,
       p_in_grade_1,
       p_in_date,
   -- EC4940 Summer 2021 Results Delivery Starts
       p_in_unit_ra_ind
   -- EC4940 Summer 2021 Results Delivery Ends
      );

  END IF;
  pr_maintain_fees
    (p_in_reg_no,
     p_in_unit_id,
     NULL,	-- nvq unit
     'U', 	-- fm_type
     p_out_error_message
    );

EXCEPTION
  WHEN OTHERS THEN
    p_out_error_message := 'pr_insert_student_unit '||
                           SQLERRM;
END pr_insert_student_unit;

PROCEDURE pr_insert_nvq_stud_comp_unit
  (p_in_reg_no		        IN  VARCHAR2,
   p_in_unit_id			IN  VARCHAR2,
   p_in_achieved_year		IN  VARCHAR2,
   p_out_error_message		OUT VARCHAR2
  )
IS
BEGIN
  INSERT INTO nvq_student_competence_units
    (nscu_st_reg_no,
     nscu_ncun_ncvq_code,
     nscu_achieved_year
    )
  VALUES
    (p_in_reg_no,
     p_in_unit_id,
     p_in_achieved_year
    )
  ;

  pr_maintain_fees
    (p_in_reg_no,
     NULL,		-- student unit id
     p_in_unit_id,
     'U', 		-- fm_type
     p_out_error_message
    );

EXCEPTION
  WHEN OTHERS THEN
    p_out_error_message :=
      'pr_insert_nvq_stud_comp_unit '||
      SQLERRM;
END pr_insert_nvq_stud_comp_unit;

PROCEDURE pr_update_audit_trail
  (p_in_reg_no		IN  VARCHAR2,
   p_out_error_message	OUT VARCHAR2
  )
IS
  -- This procedure taken directly from STV060.fmb

  v_adjustment_type	VARCHAR2(1);
  v_iatr_id          	NUMBER(8);
  v_check_worth      	NUMBER(8,4);
  v_new_worth        	NUMBER(8,4);
  v_old_worth        	NUMBER(8,4);
  v_adjustment_date  	DATE;
  v_invoice_id          VARCHAR2(6);
  v_adjustment_value 	NUMBER(8,4);
  v_dummy            	VARCHAR2(1);
  v_fm_type		VARCHAR2(1);
  v_invoice_state	VARCHAR2(1);

  CURSOR c1 IS
  SELECT DISTINCT
         fm_reg_no,
         fm_type,
         DECODE
           (st_invo_id,
            NULL, 'P',
            'G'
           ) 	invoice_state,
         st_student_worth
    FROM students,
         fee_maint
   WHERE fm_reg_no = st_reg_no
     AND fm_date IS NOT NULL
     AND fm_reg_no = p_in_reg_no
  ;

   CURSOR c2 IS
   SELECT fm_invoice          	invoice_id,
          SUM(fm_unit_value) 	adjustment_value
     FROM fee_maint
    WHERE fm_reg_no = P_in_reg_no
      AND fm_type = 'X' 	-- unit deletion
      AND fm_date IS NOT NULL
    GROUP BY fm_invoice
   ;

   CURSOR c3 IS
   SELECT fm_gnvq_unit_id,
          fm_nvq_unit_id,
          fm_unit_id
     FROM fee_maint
    WHERE fm_reg_no = p_in_reg_no
      AND fm_invoice = v_invoice_id
      AND fm_type = 'X'
      AND fm_date IS NOT NULL
   ;

  CURSOR c4 IS
  SELECT fm_gnvq_unit_id,
         fm_nvq_unit_id,
         fm_unit_id
    FROM fee_maint
   WHERE fm_reg_no = p_in_reg_no
     AND fm_type = v_fm_type
     AND fm_date IS NOT NULL;

  CURSOR c5 IS
  SELECT invoice_audit_trail_seq_no.nextval
    FROM DUAL
  ;

BEGIN

  FOR c1r IN c1 LOOP

    v_check_worth :=
      pk_invoices.fn_calculate_student_worth
        (p_in_reg_no, 'INTERACTIVE');
    v_fm_type := c1r.fm_type;
    v_invoice_state := c1r.invoice_state;

    UPDATE students
       SET st_student_worth = v_check_worth
     WHERE st_reg_no = p_in_reg_no
     ;

    IF v_fm_type = 'X' AND
       v_invoice_state = 'G' THEN

      -- Unit Deletion

      v_old_worth := c1r.st_student_worth;

      FOR c2r IN c2 LOOP

        v_new_worth := v_old_worth - c2r.adjustment_value;

        OPEN c5;
        FETCH c5 INTO v_iatr_id;
        CLOSE c5;

        INSERT INTO invoice_audit_trails
          (iatr_id,
           iatr_adjustment_type,
           iatr_st_reg_no,
           iatr_adjustment_date,
           iatr_invoice_state,
           iatr_new_student_worth,
           iatr_old_student_worth,
           iatr_credited_invo_id
          )
        VALUES
          (v_iatr_id,
           v_fm_type,
           p_in_reg_no,
           TRUNC(SYSDATE),
           v_invoice_state,
           v_new_worth,
           v_old_worth,
           c2r.invoice_id
          );

        FOR c3r IN c3 LOOP
           INSERT INTO invoice_audit_student_units
             (iasu_iatr_id,
              iasu_guni_id,
              iasu_ncun_id,
              iasu_unit_code
             )
           VALUES
             (v_iatr_id,
              c3r.fm_gnvq_unit_id,
              c3r.fm_nvq_unit_id,
              c3r.fm_unit_id
             );
         END LOOP;

         v_old_worth := v_new_worth;

      END LOOP;

    ELSE
       -- Inserts and Updates

       OPEN c5;
       FETCH c5 INTO v_iatr_id;
       CLOSE c5;

       INSERT INTO invoice_audit_trails
         (iatr_id,
          iatr_adjustment_type,
          iatr_st_reg_no,
          iatr_adjustment_date,
          iatr_invoice_state,
          iatr_new_student_worth,
          iatr_old_student_worth)
       VALUES
        (v_iatr_id,
         v_fm_type,
         p_in_reg_no,
         sysdate,
         v_invoice_state,
         v_check_worth,
         c1r.st_student_worth);
       --
       FOR c4r IN c4 LOOP
         INSERT INTO invoice_audit_student_units
           (iasu_iatr_id,
            iasu_guni_id,
            iasu_ncun_id,
            iasu_unit_code
           )
         VALUES
           (v_iatr_id,
            c4r.fm_gnvq_unit_id,
            c4r.fm_nvq_unit_id,
            c4r.fm_unit_id
           );
       END LOOP;

     END IF;

     DELETE FROM fee_maint
      WHERE fm_reg_no = p_in_reg_no
        AND fm_type = v_fm_type
        AND fm_date IS NOT NULL
     ;

   END LOOP;


EXCEPTION
  WHEN OTHERS THEN
    p_out_error_message :=
      'pr_update_audit_trail:'||
      SQLERRM;
END pr_update_audit_trail;

END pk_edx_shared3;
/