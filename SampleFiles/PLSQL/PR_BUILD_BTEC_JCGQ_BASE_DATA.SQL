set echo on
set termout on

connect ies@oralinux 

/*******************************************************************************
*
*   Module          : PR_BUILD_BTEC_JCGQ_BASE_DATA.SQL
*   Author          : CDS
*   Date            : August 2004
*
*   Description
*   -----------
*	Create BTEC base data for a centre, year and course. % for all courses.
*	Write data to EDI_DATA in syllabus/option/component/link file formats.
*
*   Amendment History
*   -----------------
*
*   Date         Author  Description
*   -----------  ------  ----------------------------------------------------
*   17-MAY-2005  CDS     Create BTEC ENTRY base data. New input parameter
*			 p_run_type determines whether to create BTEC
*			 REGISTRATION or BTEC ENTRY base data.
*   07-JUN-2006  CDS     RFC 06/0100
*			 Include programmes where certificate end date is
*			 current for the approved year (ENTRIES ONLY).
*   23-AUG-2006  RSH     RFC 06/0186
*			 Additional input parameter: p_eo_emls_id
*			 Only applicable when calling from EO procedure.
*   25-AUG-2006  RSH     Procedure PR_INSERT_EDI_DATA:
*                        Use V_EMLS_ID instead of EMLS_SEQ.CURRVAL.
*   07-SEP-2006  RSH     RFC 06/0186
*			 Additional input parameter: p_email
*			 Only applicable when calling from EO procedure.
*   23-JAN-2007  CDS     EMLS_ID NUMBER(7)
*   19-FEB-2007  CAG     LQ00033186 exclude Extended Project (EP).
*   11-MAY-2007  CDS     EC187 - Increase V_EMLO_ID to NUMBER(12).
*   16-JAN-2008  CDS     EC266 - Split gradesets for new (gradeset 3) and
*			 old (gradeset D) firsts (award codes 41 and 94).
*   24-SEP-2008  CDS     LQ00044811 - Version 11.
*   20-JUL-2009  CDS     LQ00052914 - Version 12.
*   26-MAY-2010  CDS     LQ00060810 - Version 13.
*   30-JUL-2010	 CDS	 LQ00060727 - New format parameter in fn_calc_btec_fee. 
*   14-JUN-2011  CDS     WI344- Version 14.
*   25-OCT-2016  CDS     EC2346 - Override JCQ Gradeset
*******************************************************************************/

-- "Set scan off" turns off substitution variables. 
Set scan off; 

CREATE OR REPLACE PROCEDURE IES.pr_build_btec_jcgq_base_data  (
          p_emap_id	    edi_message_approvals.emap_id%TYPE,
	  p_approval_year   approval_awards.aw_app_to%type,
	  p_course	    approval_awards.aw_course_number%type default '%',
	  p_acad_centre	out centres.cn_as400_centre_id%type,
	  p_base_data	out varchar2,
          p_s_emls_id   out edi_message_log_states.emls_id%type,
          p_o_emls_id   out edi_message_log_states.emls_id%type,
          p_c_emls_id   out edi_message_log_states.emls_id%type,
          p_l_emls_id   out edi_message_log_states.emls_id%type,
	  p_run_type	    varchar2	default 'REGISTRATION',
	  p_eo_emls_id	    edi_message_log_states.emls_id%type	default 0,
	  p_eo_email	    edi_message_logs.emlo_sender_email_address%type default null ) is


-- General variables...

p_email	edi_message_logs.emlo_sender_email_address%TYPE	:= p_eo_email;

v_btec_series			varchar2(1)	:= '9';
v_btec_series_entries		varchar2(1)	:= '7';
v_syllabus_file_type		varchar2(1)	:= 'S';
v_options_file_type		varchar2(1)	:= 'O';
v_link_file_type		varchar2(1)	:= 'L';
v_component_file_type		varchar2(1)	:= 'C';
v_component_suffix		varchar2(2)	:= '01';
v_language_indicator		varchar2(1)	:= '_';
v_filename			varchar2(8)	:= null;
v_btec_qual_id			varchar2(1)	:= 'B';
v_transmission_type		varchar2(1)	:= 'X';
v_row_count			number(8)	:= 0;
v_data_type			varchar2(1)	:= 'R';
v_uab				varchar2(2)	:= '13';
v_uab_entries			varchar2(2)	:= '15';
v_uab_output			varchar2(2)	:= null;
v_formats_version		varchar2(2)	:= null;
v_record_type_1			varchar2(1)	:= '1';
v_record_type_3			varchar2(1)	:= '3';
v_record_type_5			varchar2(1)	:= '5';
v_record_type_7			varchar2(1)	:= '7';
v_record_type_9			varchar2(1)	:= '9';
v_examination_series		varchar2(2)	:= null;
v_full_examination_details	varchar2(5)	:= null;
v_examination_year		varchar2(2)	:= null;
v_commence			date		:= null;
v_distribution_type		varchar2(1)	:= 'B';
v_centre_sequence		number(6)	:= 1;
v_software_package		varchar2(7)	:= 'EDEXCEL';
v_software_package_version	varchar2(3)	:= ' ';
v_no_of_recs_type7		number(7)	:= 0;
v_no_of_recs_type9		number(7)	:= 0;
v_no_of_centres			number(7)	:= 1;
v_voca_main_centre		varchar2(5)	:= null;
v_acad_centre			varchar2(5)	:= null;
v_syllabus_record_length        number(3)	:= 0;
v_options_record_length		number(3)	:= 0;
v_component_record_length       number(3)	:= 0;
v_link_record_length        	number(3)	:= 0;
v_err_ref			varchar2(50);
v_emlo_id	   		number(12);
v_emls_id	   		number(7);
v_s_emls_id	   		number(7);	-- Syllabus
v_o_emls_id	   		number(7);	-- Option
v_c_emls_id	   		number(7);	-- Component
v_l_emls_id	   		number(7);	-- Link
v_run_type			varchar2(12);
v_year				number(4);

v_tpar_id		trading_partners.tpar_id%type		  := null;
v_tpar_eovi_ref		trading_partners.tpar_eovi_ref%type	  := null;
v_voca_centre		trading_partners.tpar_eovi_ref%type	  := null;
v_buni_id		centres.cn_bure_buni_id%type		  := null;
v_aaco_pass2_ind	at_award_codes.aaco_pass2_ind%type	  := null;

-- Option File variables...

v_exam_item   			varchar2(1)	:= 'C';
v_exam_process			varchar2(1)	:= 'R';
v_exam_process_entries		varchar2(1)	:= 'E';
v_exam_process_output		varchar2(1)	:= null;
v_qca_classification_code	varchar2(4)	:= ' ';
v_qca_accreditation_no		varchar2(8)	:= ' ';
v_regn_fee_defined		varchar2(1)	:= null;
v_regn_fee			number(5,2)	:= 0;
v_first_fcast_grade_grdset	varchar2(1)	:= ' ';
v_second_fcast_grade_grdset	varchar2(1)	:= ' ';
v_second_grade_or_rslt_grdset	varchar2(1)	:= ' ';
v_endorsement_to_first		varchar2(1)	:= ' ';
v_endorsement_to_second		varchar2(1)	:= ' ';
v_max_mark_or_ums		varchar2(3)	:= ' ';
v_number_of_components		varchar2(2)	:= '01';

-- introduced for Version 12 

v_exam_type_qual_unit		varchar2(1)	:= ' ';
v_exam_type_level_unit		varchar2(1)	:= ' ';

-- Component File variables...

v_teacher_marks			varchar2(1)	:= 'N';
v_max_mark			varchar2(3)	:= ' ';
v_component_grdset		varchar2(4)	:= ' ';
v_due_date			varchar2(6)	:= ' ';
v_timetabled			varchar2(1)	:= 'N';
v_timetable_date		varchar2(6)	:= ' ';
v_timetable_session		varchar2(1)	:= 'X';
v_time_allowed			varchar2(3)	:= '000';

-- Link File variables...

v_space				varchar2(6)	:= ' ';

/***********************************************************************/

-- build examination series and year fields...

   cursor get_exam_details(p_btec_series varchar2,
			   p_year	 varchar2) is
     select p_btec_series||
		v_btec_qual_id,
	    substr(p_year,3,2),
	    '01-SEP-'||substr(p_approval_year,3,2),
	    p_btec_series||
		v_btec_qual_id||
		substr(p_year,3,2)||
		v_language_indicator
     from   dual;

-- get the trading partner, vocational and academic centres...
-- (union allows either a vocational or academic message approval id)...

   cursor get_tpar_id is
     select tpar_id,
            llcv_lcen_centre_ref                voca,
            substr(llcv_lcen_centre_ref,1,5)    main_voca,
            llcv_linked_lcen_centre_ref         acad,
	    to_char(emve_version_no)		version
     from   linked_legacy_centres_view,
            trading_partners,
	    edi_message_versions,
            edi_message_approvals
     where  llcv_lcen_centre_ref             = tpar_eovi_ref
     and    llcv_lcen_ltyp_id                = 1
     and    llcv_linked_lcen_centre_ref is not null
     and    tpar_id                          = emap_tpar_id
     and    emve_id			     = emap_emve_id
     and    emap_id                          = p_emap_id
     union
     select tpar_id,
            llcv_linked_lcen_centre_ref                voca,
            substr(llcv_linked_lcen_centre_ref,1,5)    main_voca,
            llcv_lcen_centre_ref         	       acad,
	    to_char(emve_version_no)		       version
     from   linked_legacy_centres_view,
            trading_partners,
	    edi_message_versions,
            edi_message_approvals
     where  llcv_lcen_centre_ref             = tpar_eovi_ref
     and    llcv_lcen_ltyp_id                = 2
     and    llcv_linked_lcen_centre_ref is not null
     and    tpar_id                          = emap_tpar_id
     and    emve_id			     = emap_emve_id
     and    emap_id                          = p_emap_id;

/***********************************************************************/

-- Get record lengths...

   cursor get_record_length(p_file_type varchar2,
                            p_version   varchar2) is
        select  jffc_rec_length  rec_length
        from    jcq_file_format_control
        where   jffc_file_type_id = p_file_type
        and     jffc_jcq_version  = p_version;

-- get the business unit...

   cursor get_buni_id is
     select cn_bure_buni_id
     from   centres
     where  cn_centre_id = v_voca_centre;

-- get filename

   cursor get_filename(p_file_type varchar2,
		       p_uab	   varchar2) is
     select	p_file_type||
		v_full_examination_details||
		p_uab
     from 	dual;

-- get file sequence

   cursor get_file_sequence(p_file_type   varchar2,
			    p_btec_series varchar2,
			    p_uab	  varchar2) is
     select  lpad((to_number(
			substr(emlo_file_extension,2))+1),3,'0') emlo_file_seq
     from    edi_message_logs emlo
     where   emlo_emap_id = p_emap_id
     and     emlo_file_name like p_file_type||
				 p_btec_series||
				 v_btec_qual_id||
				 '___'||
				 p_uab||'%'
     and     to_number(substr(emlo_file_extension,2)) =
			(select max(to_number(substr(emlo_file_extension,2)))
			 from   edi_message_logs
			 where  emlo_file_name like p_file_type||
				 		    p_btec_series||
				 		    v_btec_qual_id||
				 		    '___'||
				 		    p_uab||'%'
     			 and    emlo_emap_id = emlo.emlo_emap_id
			);

-- RSH 23/08/2006 RfC 06/0186:
-- get EO file sequence

   cursor get_eo_file_sequence(p_emls_id   number) is
     select  emls_id,
             emlo_id,
             substr(emlo_file_extension,2) emlo_file_seq
     from    edi_message_log_states,
             edi_message_logs
     where   emls_id = p_emls_id
     and     emlo_id = emls_emlo_id;

-- get QCA Accreditation Code

   cursor get_qca_code(p_at_number number,
		       p_ac_code   varchar2) is
     select nvl(aaco_qca_code,' '),
	    nvl(aaco_pass2_ind,'N')
     from   at_award_codes
     where  aaco_at_number = p_at_number
     and    aaco_ac_code   = p_ac_code;

-- File Header

   cursor fh
   is
        select
		v_data_type,
		v_record_type_1,
		v_tpar_eovi_ref,
		v_uab_output,
		v_examination_series,
		v_examination_year,
		v_distribution_type,
		v_software_package,
		v_software_package_version,
		v_formats_version,
	        tpar_eovi_ref,
        	emap_etty_id
        from
           	trading_partners,
           	edi_message_approvals
        where
		tpar_id      =  emap_tpar_id
        and     emap_id      =  p_emap_id;

-- Check if any base data for centre/course/year...

   cursor chk_base_data(p_centre		varchar2,
	     	   	p_course 		varchar2,
	     	   	p_approval_year 	varchar2)
   is
   	select  'Y'
	from	dual
	where exists (
		      select	null
		      from      award_codes,
                		approval_awards,
                		award_titles,
                		approval_application,
                		approval_joint
        	      where	ac_code			= aw_award_code
		      and	aj_centre_no		= substr(p_centre,1,5)
	              and	aa_applicat_no		= aj_applicat_no
	              and	aa_btec_title		= at_number
	              and	aw_applicat_no		= aa_applicat_no
		      and	aw_award_code <> 'EP'
	              and	aw_course_number     like p_course
	              and	aw_course_number   not in ('KSQ00','ABS')
		      and	ac_jcgq_type	       is not null
	              and	aw_app_to              >= p_approval_year
		      and	p_run_type		= 'REGISTRATION'
		      union
		      select	null
		      from      award_codes,
		                approval_awards,
		                award_titles,
		                approval_application,
		                approval_joint
		      where	ac_code			= aw_award_code
		      and	aj_centre_no		= substr(p_centre,1,5)
		      and	aa_applicat_no		= aj_applicat_no
		      and	aa_btec_title		= at_number
		      and	aw_applicat_no		= aa_applicat_no
		      and	aw_award_code <> 'EP'
		      and	aw_course_number     like p_course
		      and	aw_course_number   not in ('KSQ00','ABS')
		      and	ac_jcgq_type	       is not null
		      and	aw_app_to	       
				    + aw_course_length >= p_approval_year
		      and	not exists (select	null
					    from	atac_accreditations
					    where	aacc_at_number = at_number
					    and		aacc_ac_code   = ac_code)
		      and	p_run_type		= 'ENTRIES'
		      union
		      select	null
		      from
				atac_accreditations,
				award_codes,
		                approval_awards,
		                award_titles,
		                approval_application,
		                approval_joint
		      where	ac_code			= aw_award_code
		      and	aj_centre_no		= substr(p_centre,1,5)
		      and	aa_applicat_no		= aj_applicat_no
		      and	aa_btec_title		= at_number
		      and	aw_applicat_no		= aa_applicat_no
		      and	aw_award_code <> 'EP'
		      and	aw_course_number     like p_course
		      and	aw_course_number   not in ('KSQ00','ABS')
		      and	ac_jcgq_type	       is not null
		      and	aacc_at_number 		= at_number
		      and	aacc_ac_code   		= ac_code
		      and	aacc_certificate_end_date
				     		       >= to_date('31-AUG-'||to_char(p_approval_year+1),'DD-MON-YYYY')
		      and	p_run_type		= 'ENTRIES'
		     );

-- Get approved courses at main vocational centre...(syllabus detail)

   cursor syllabus(p_centre		varchar2,
	     	   p_course 		varchar2,
	     	   p_approval_year 	varchar2)
   is
	--
	--	This part of the union for BTEC REGISTRATIONS BASEDATA ONLY
	--					=============
       	select
                aw_applicat_no					app_no,
                decode(at_coa_syllabus_flag,
			'Y',aa_btec_title,aw_course_number) 	course,
		aw_course_number				btec_course,
                aw_award_code					award_code,
		nvl(ac_accl_code,'*')				award_code_type,
		nvl(ac_jcgq_type,' ')				jcgq_type,
		nvl(ac_jcgq_level,' ')				jcgq_level,
		nvl(ac_jcgq_result_type,' ')			jcgq_result_type,
		nvl(nvl(bogr_jcq_gradeset,
			ac_jcgq_result_gradeset),' ')		jcgq_result_gradeset,
                substr(nvl(at_short_name,at_name),1,36)		course_title,
                aw_app_to					app_to_year,
		at_number					at_number,
	   	decode(aw_award_code,'28','*','29','*','30','*',
	    	 decode(aw_bale_code,null,'*',
	      	  decode(least(2,abs(nvl(v_buni_id,9)-4)),2,
		   decode(sign(v_commence - to_date('01-sep-02')+1),1,
		     decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*')
		     ),
			decode(nvl(aw_revised_btec_award_ind,'N'),'Y',
			     decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*')),'*'
			)
		   ),
		       decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*'))
	      )
	    )
	   )							btec_award_level
     	from
		bnm_overall_grades,
		award_codes,
                approval_awards,
                award_titles,
                approval_application,
                approval_joint
        where	ac_code                 = nvl(bogr_award_code,aw_award_code)
	and	bogr_at_number(+)	= to_number(aa_btec_title)
	and	aj_centre_no		= substr(p_centre,1,5)
        and	aa_applicat_no		= aj_applicat_no
        and	aa_btec_title		= at_number
        and	aw_applicat_no		= aa_applicat_no
	and	aw_award_code <> 'EP'
        and	aw_course_number     like p_course
        and	aw_course_number   not in ('KSQ00','ABS')
	and	ac_jcgq_type	       is not null
        and     aw_app_to              >= p_approval_year
	and	p_run_type		= 'REGISTRATION'
	union
	--
	--	This part of the union for BTEC RESULT ENTRIES BASEDATA ONLY
	--                                      ==============
   	select
                aw_applicat_no					app_no,
                decode(at_coa_syllabus_flag,
			'Y',aa_btec_title,aw_course_number) 	course,
		aw_course_number				btec_course,
                aw_award_code					award_code,
		nvl(ac_accl_code,'*')				award_code_type,
		nvl(ac_jcgq_type,' ')				jcgq_type,
		nvl(ac_jcgq_level,' ')				jcgq_level,
		nvl(ac_jcgq_result_type,' ')			jcgq_result_type,
		nvl(nvl(bogr_jcq_gradeset,
			ac_jcgq_result_gradeset),' ')		jcgq_result_gradeset,
                substr(nvl(at_short_name,at_name),1,36)		course_title,
                aw_app_to					app_to_year,
		at_number					at_number,
	   	decode(aw_award_code,'28','*','29','*','30','*',
	    	 decode(aw_bale_code,null,'*',
	      	  decode(least(2,abs(nvl(v_buni_id,9)-4)),2,
		   decode(sign(v_commence - to_date('01-sep-02')+1),1,
		     decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*')
		     ),
			decode(nvl(aw_revised_btec_award_ind,'N'),'Y',
			     decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*')),'*'
			)
		   ),
		       decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*'))
	      )
	    )
	   )							btec_award_level
     	from
		bnm_overall_grades,
		award_codes,
                approval_awards,
                award_titles,
                approval_application,
                approval_joint
        where	ac_code                 = nvl(bogr_award_code,aw_award_code)
	and	bogr_at_number(+)	= to_number(aa_btec_title)
	and	aj_centre_no		= substr(p_centre,1,5)
        and	aa_applicat_no		= aj_applicat_no
        and	aa_btec_title		= at_number
        and	aw_applicat_no		= aa_applicat_no
	and	aw_award_code <> 'EP'
        and	aw_course_number     like p_course
        and	aw_course_number   not in ('KSQ00','ABS')
	and	ac_jcgq_type	       is not null
        and	aw_app_to	       
		    + aw_course_length >= p_approval_year
	and	not exists (select	null
			    from	atac_accreditations
			    where	aacc_at_number = at_number
			    and		aacc_ac_code   = ac_code)
	and	p_run_type		= 'ENTRIES'
	union
	--
	--	This part of the union ALSO for BTEC RESULT ENTRIES BASEDATA ONLY
	--                                           ==============
   	select
        	aw_applicat_no					app_no,
                decode(at_coa_syllabus_flag,
			'Y',aa_btec_title,aw_course_number) 	course,
		aw_course_number				btec_course,
                aw_award_code					award_code,
		nvl(ac_accl_code,'*')				award_code_type,
		nvl(ac_jcgq_type,' ')				jcgq_type,
		nvl(ac_jcgq_level,' ')				jcgq_level,
		nvl(ac_jcgq_result_type,' ')			jcgq_result_type,
		nvl(nvl(bogr_jcq_gradeset,
			ac_jcgq_result_gradeset),' ')		jcgq_result_gradeset,
                substr(nvl(at_short_name,at_name),1,36)		course_title,
                aw_app_to					app_to_year,
		at_number					at_number,
	   	decode(aw_award_code,'28','*','29','*','30','*',
	    	 decode(aw_bale_code,null,'*',
	      	  decode(least(2,abs(nvl(2,9)-4)),2,
		   decode(sign(to_date('01-SEP-05') - to_date('01-sep-02')+1),1,
		     decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*')
		     ),
			decode(nvl(aw_revised_btec_award_ind,'N'),'Y',
			     decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*')),'*'
			)
		   ),
		       decode(nvl(aw_bale_code,'*'),'4','6',nvl(aw_bale_code,'*'))
	      )
	    )
	   )							btec_award_level
     	from
		bnm_overall_grades,
		atac_accreditations,
		award_codes,
                approval_awards,
                award_titles,
                approval_application,
                approval_joint
        where	ac_code                 = nvl(bogr_award_code,aw_award_code)
	and	bogr_at_number(+)	= to_number(aa_btec_title)
	and	aj_centre_no		= substr(p_centre,1,5)
        and	aa_applicat_no		= aj_applicat_no
        and	aa_btec_title		= at_number
        and	aw_applicat_no		= aa_applicat_no
	and	aw_award_code <> 'EP'
        and	aw_course_number     like p_course
        and	aw_course_number   not in ('KSQ00','ABS')
	and	ac_jcgq_type	       is not null
	and	aacc_at_number 		= at_number
	and	aacc_ac_code   		= ac_code
        and	aacc_certificate_end_date
		     		       >= to_date('31-AUG-'||to_char(p_approval_year+1),'DD-MON-YYYY')
	and	p_run_type		= 'ENTRIES'
	order by 1;

-- Get course combinations...(option detail)

   cursor options(p_app_no	number,
	     	  p_award_code	varchar2)
   is
   	select
                ac_combination		combination
     	from
		approval_combination
        where	ac_award_code		= p_award_code
        and	ac_applicat_no		= p_app_no;

/***********************************************************************/

PROCEDURE pr_insert_edi_data ( p_data_in varchar2)
IS
BEGIN
        v_row_count := v_row_count + 1;
/*
	Use v_emls_id instead of emls_seq.currval.
*/
        INSERT into edi_data
        values (v_emls_id,
		v_row_count,
		p_data_in);
EXCEPTION
WHEN OTHERS THEN
        raise_application_error(-20000,'PR_BUILD_JCGQ_BASE_DATA : '||
				       'PR_INSERT_EDI_DATA '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_file_header ( p_file_type varchar2, p_record_length number)
IS
BEGIN

	pr_insert_edi_data
	       (rpad(
		p_file_type||
		v_record_type_1||
		v_full_examination_details||
		v_uab_output||
		v_examination_series||
		v_examination_year||
		v_distribution_type||
		rpad(v_software_package,10)||
		v_formats_version
		,p_record_length)
	       );


EXCEPTION
WHEN OTHERS THEN
        raise_application_error(-20000,'PR_BUILD_JCGQ_BASE_DATA : '||
				       'PR_FILE_HEADER '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_centre_header ( p_file_type varchar2, p_record_length number)
IS
BEGIN

    pr_insert_edi_data
	       (rpad(
		p_file_type||
		v_record_type_3||
		v_full_examination_details||
		v_uab_output||
		v_examination_series||
		v_examination_year||
	        lpad(v_centre_sequence,3,'0')
		,p_record_length)
	       );

EXCEPTION
WHEN OTHERS THEN
        raise_application_error(-20000,'PR_BUILD_JCGQ_BASE_DATA : '||
				       'PR_CENTRE_HEADER '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_centre_trailer ( p_file_type varchar2, p_record_length number)
IS
BEGIN

    pr_insert_edi_data
	       (rpad(
		p_file_type||
		v_record_type_7||
		v_full_examination_details||
		lpad(v_no_of_recs_type7,7,'0')||
		to_char(sysdate,'DDMMYY')
		,p_record_length)
	       );

EXCEPTION
WHEN OTHERS THEN
        raise_application_error(-20000,'PR_BUILD_JCGQ_BASE_DATA : '||
				       'PR_CENTRE_TRAILER '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_file_trailer ( p_file_type varchar2, p_record_length number)
IS
BEGIN

    pr_insert_edi_data
	       (rpad(
		p_file_type||
		v_record_type_9||
		v_full_examination_details||
		lpad(v_no_of_recs_type9,7,'0')||
		lpad(v_no_of_centres,7,'0')
		,p_record_length)
	       );

EXCEPTION
WHEN OTHERS THEN
        raise_application_error(-20000,'PR_BUILD_JCGQ_BASE_DATA : '||
				       'PR_FILE_TRAILER '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_insert_emlo
IS
BEGIN

      insert into edi_message_logs
       (EMLO_ID                       ,
	EMLO_EMAP_ID                  ,
	EMLO_ETTY_ID                  ,
	EMLO_FILE_NAME                ,
	EMLO_FILE_EXTENSION           ,
	EMLO_LOGGED_DATE              ,
	EMLO_LOG_ORIGIN_TYPE          ,
	EMLO_DELETE_IND               ,
	EMLO_RESUBMIT_IND             ,
	EMLO_REBATCH_IND              ,
	EMLO_ESER_ID                  ,
	EMLO_OVERRIDE_RELEASE_DATE    ,
	EMLO_SENDER_EMAIL_ADDRESS     ,
	EMLO_EPER_ID
       )
      values
       (emlo_seq.nextval,
        p_emap_id,
        v_transmission_type,
        v_filename,
        'X'||lpad(v_centre_sequence,3,'0'),
        sysdate,
        'A',
        'N',
        'N',
        'N',
        null,
        null,
        p_email,
        pk_cco.fn_eper_id
       );

	select emlo_seq.currval
	into   v_emlo_id
	from dual;

	-- dbms_output.put_line('EMLO ID = '||to_char(v_emlo_id));

EXCEPTION
  WHEN OTHERS
  THEN
        raise_application_error(-20000,'PR_BUILD_JCGQ_BASE_DATA : '||
				       'PR_INSERT_EMLO '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_insert_emls (p_file_type varchar2)
IS
BEGIN

  /*
    create the log state record for the output of this data.
    no state apart from FINAL is used, has to be created now
    because need to create edi_data rows
  */
      insert into edi_message_log_states
       (
	EMLS_ID,
	EMLS_EMLO_ID,
	EMLS_EDST_ID,
	EMLS_CREATED_DATE,
	EMLS_RECORD_COUNT
       )
      values
       (emls_seq.nextval,
	v_emlo_id,
        'F',
        sysdate,
        0
       );

	select emls_seq.currval
	into   v_emls_id
	from dual;

	if p_file_type = 'S'
	then
	  p_s_emls_id := v_emls_id;
	end if;

	if p_file_type = 'O'
	then
	  p_o_emls_id := v_emls_id;
	end if;

	if p_file_type = 'C'
	then
	  p_c_emls_id := v_emls_id;
	end if;

	if p_file_type = 'L'
	then
	  p_l_emls_id := v_emls_id;
	end if;

	-- dbms_output.put_line('EMLS ID = '||to_char(v_emls_id));

EXCEPTION
  WHEN OTHERS
  THEN
          raise_application_error(-20000,'pr_build_btec_jcgq_base_data : PR_INSERT_EMLS '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_insert_emla
IS
BEGIN

  /*
    create the action audit record
  */
      insert into edi_message_log_actions
       (
	emla_emlo_id,
	emla_eaty_id,
	emla_date,
	emla_qualifying_text
       )
      values
       (v_emlo_id,
        '7',
        sysdate,
        null
       );

EXCEPTION
  WHEN OTHERS
  THEN
          raise_application_error(-20000,'pr_build_btec_jcgq_base_data : PR_INSERT_EMLA '||sqlerrm);
END;

/***********************************************************************/

PROCEDURE pr_update_emls (p_file_type varchar2)
IS
BEGIN

	  UPDATE edi_message_log_states
	  set    emls_record_count = v_row_count
	  where  emls_id = decode(p_file_type,'S',p_s_emls_id,
					      'O',p_o_emls_id,
					      'C',p_c_emls_id,
					      'L',p_l_emls_id);

EXCEPTION
  WHEN OTHERS
  THEN
	    raise_application_error(-20000,'pr_build_btec_jcgq_base_data : PR_UPDATE_EMLS '||sqlerrm);
END;

/***********************************************************************/
/***********************************************************************/
/***********************************************************************/
/***********************************************************************/

BEGIN

  v_run_type := upper(p_run_type);

  if v_run_type = 'ENTRIES'
  then
    v_uab_output	  := v_uab_entries;
    v_exam_process_output := v_exam_process_entries;
    v_year		  := p_approval_year + 1;
  else
    v_uab_output	  := v_uab;
    v_exam_process_output := v_exam_process;
    v_year		  := p_approval_year;
  end if;

  /* get examination details */

  if v_run_type = 'ENTRIES'
  then
    open get_exam_details(v_btec_series_entries,v_year);
  else
    open get_exam_details(v_btec_series,v_year);
  end if;

  fetch get_exam_details
  into  v_examination_series,
	v_examination_year,
	v_commence,
	v_full_examination_details;
  if get_exam_details%notfound then
    --abort no error
    return;
  end if;

  close get_exam_details;

  -- dbms_output.put_line('TPAR = '||v_tpar_id||' EOVI REF = '||v_tpar_eovi_ref);

  /* obtain the trading partner id and centre identifier. */

  open get_tpar_id;

  fetch get_tpar_id
  into 	v_tpar_id,
	v_voca_centre,
	v_voca_main_centre,
	v_acad_centre,
	v_formats_version;
  if get_tpar_id%notfound then
    --abort no error
    return;
  end if;

  p_acad_centre := v_acad_centre;

  if v_formats_version in ('12','13','14')
  then
    v_component_grdset := rpad(v_component_grdset,4);
  else
    v_component_grdset := rpad(v_component_grdset,1);
  end if;

  close get_tpar_id;

  -- dbms_output.put_line('TPAR = '||v_tpar_id||' EOVI REF = '||v_tpar_eovi_ref);

  /* Check if any base data for specified parameters */

  p_base_data := 'N';

  for x in chk_base_data(v_voca_main_centre,p_course,p_approval_year) loop
    p_base_data := 'Y';
  end loop;

  if p_base_data = 'N' then
    -- No base data so do not produce files...
    return;
  end if;

  /* get business unit */

  open get_buni_id;

  fetch get_buni_id
  into 	v_buni_id;

  close get_buni_id;

  --  dbms_output.put_line('ACAD CENTRE = '||v_tpar_eovi_ref);

  /* get the next filename sequence */

/*
  RSH 23/08/2006 RfC 06/0186:
  If the procedure has been called from the Generate Vocational Basedata
  for EO module, the filename sequence has already been reserved, when the
  procedure was previously called from the Request Vocation Basedata for
  EO module, and stored in the EMLO record for the syllabus file.
*/
  if p_eo_emls_id > 0 then
    open get_eo_file_sequence(p_eo_emls_id);

    fetch get_eo_file_sequence
    into  v_emls_id,
          v_emlo_id,
          v_centre_sequence;
    if get_eo_file_sequence%notfound then
      --abort no error
      return;
    end if;

    close get_eo_file_sequence;
  else
    if v_run_type = 'ENTRIES'
    then
      open get_file_sequence(v_syllabus_file_type,
        		     v_btec_series_entries,
	  		     v_uab_entries);
    else
      open get_file_sequence(v_syllabus_file_type,
			     v_btec_series,
			     v_uab);
    end if;

    fetch get_file_sequence
    into  v_centre_sequence;

    --  dbms_output.put_line('SEQUENCE = '||v_centre_sequence);

    close get_file_sequence;

    /* Avoid having a file sequence of '00' */

    if substr(lpad(v_centre_sequence,3,'0'),2) = '00'
    then
      v_centre_sequence := v_centre_sequence + 1;
    end if;
  end if;

  -- dbms_output.put_line('SEQUENCE = '||v_centre_sequence);

  /* JCGQ BASE DATA version 10 */

  v_err_ref := 'Syllabus File';

  for r1_rec in get_record_length(v_syllabus_file_type,v_formats_version)
  loop
      v_syllabus_record_length := r1_rec.rec_length;        
  end loop;

  /* build the filename */

  if v_run_type = 'ENTRIES'
  then
    open get_filename(v_syllabus_file_type,v_uab_entries);
  else
    open get_filename(v_syllabus_file_type,v_uab);
  end if;

  fetch get_filename
  into  v_filename;
  if get_filename%notfound then
    --abort no error
    return;
  end if;

  close get_filename;

  -- dbms_output.put_line('Filename = '||v_filename);

/*
  RSH 23/08/2006 RfC 06/0186:
  If the procedure has been called from the Generate Vocational Basedata
  for EO module, do not insert the EMLO/EMLS/EMLA records for the syllabus
  file as these were done when the procedure was previously called from
  the Request Vocational Basedata for EO module.
*/
  if p_eo_emls_id > 0 then
    p_s_emls_id := p_eo_emls_id;
  else
    pr_insert_emlo;
    pr_insert_emls(v_syllabus_file_type);
    pr_insert_emla;
  end if;

/*
  RSH 23/08/2006 RfC 06/0186:
  If the procedure has been called from the Request Vocational Basedata
  for EO module, finish processing here as we only want to reserve the
  sequence for later processing by the Generate Vocational Basedata for
  EO module. 
*/
  if p_eo_emls_id < 0 then
    return;
  end if;

/* File Header */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_header(v_syllabus_file_type,v_syllabus_record_length);

/* Centre Header */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_header(v_syllabus_file_type,v_syllabus_record_length);

/* Detail Records */

   for x in syllabus(v_voca_main_centre,p_course,p_approval_year) loop

	-- dbms_output.put_line(x.course||' '||x.course_title);

	v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
	v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

	pr_insert_edi_data(v_syllabus_file_type||
			   v_record_type_5||
			   rpad(x.course,6)||
			   ' '||
			   rpad(substr(x.course_title,1,36),36)
			  );

   end loop;

/* Centre Trailer */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_trailer(v_syllabus_file_type,v_syllabus_record_length);

/* File Trailer */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_trailer(v_syllabus_file_type,v_syllabus_record_length);

    pr_update_emls(v_syllabus_file_type);

  v_err_ref := 'Options File';

  for r1_rec in get_record_length(v_options_file_type,v_formats_version)
  loop
      v_options_record_length := r1_rec.rec_length;        
  end loop;

  /* Reset counts */

  v_no_of_recs_type7 := 0;
  v_no_of_recs_type9 := 0;
  v_row_count	     := 0;

  /* build the filename */

  if v_run_type = 'ENTRIES'
  then
    open get_filename(v_options_file_type,v_uab_entries);
  else
    open get_filename(v_options_file_type,v_uab);
  end if;

  fetch get_filename
  into  v_filename;
  if get_filename%notfound then
    --abort no error
    return;
  end if;

  close get_filename;

  -- dbms_output.put_line('Filename = '||v_filename);

  pr_insert_emlo;
  pr_insert_emls(v_options_file_type);
  pr_insert_emla;

/* File Header */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_header(v_options_file_type,v_options_record_length);

/* Centre Header */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_header(v_options_file_type,v_options_record_length);

/* Detail Records */

   for x in syllabus(v_voca_main_centre,p_course,p_approval_year) loop

     for y in options(x.app_no,x.award_code) loop

	v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
	v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

	-- dbms_output.put_line(v_voca_centre);
	-- dbms_output.put_line(v_commence);
	-- dbms_output.put_line(v_buni_id);
	-- dbms_output.put_line(x.btec_course);
	-- dbms_output.put_line(x.award_code);
	-- dbms_output.put_line(x.award_code_type);
	-- dbms_output.put_line(x.app_no);
	-- dbms_output.put_line(x.btec_award_level);

        -- Get the BTEC registration fee...

	if v_run_type = 'REGISTRATION'
	then
	  v_regn_fee := to_number(fn_calc_btec_fee
				   (
				   v_voca_centre,
				   v_commence,
				   v_buni_id,
				   x.btec_course,
				   x.award_code,
				   x.award_code_type,
				   x.app_no,
				   x.btec_award_level,
				   1
				   )
				 );
	end if;

	-- dbms_output.put_line(v_regn_fee);

	if v_regn_fee <> 0
	then
	  v_regn_fee_defined := 'Y';
	else
	  v_regn_fee_defined := 'N';
	end if;

	v_qca_accreditation_no := ' ';
	v_aaco_pass2_ind       := 'N';

	open get_qca_code(x.at_number,x.award_code);

	fetch get_qca_code
	into  v_qca_accreditation_no,
	      v_aaco_pass2_ind;

	close get_qca_code;

	if x.award_code in ('41','94')	-- Firsts
	then
	  if v_aaco_pass2_ind = 'Y'	-- Old style (No D* grade)
	  then
	    x.jcgq_result_gradeset := 'D';
	  end if;
	end if;

	if v_formats_version = '12'
	then
	  pr_insert_edi_data(v_options_file_type||
			     v_record_type_5||
			     rpad(x.course||y.combination,6)||
			     rpad(x.course,6)||
			     rpad(x.jcgq_type,4)||
			     rpad(x.jcgq_level,3)||
			     rpad(v_exam_item,1)||
			     rpad(v_exam_type_qual_unit,4)||	
			     rpad(v_exam_type_level_unit,3)||	
			     rpad(v_exam_process_output,1)||
			     rpad(v_qca_classification_code,4)||
			     rpad(v_qca_accreditation_no,8)||
			     rpad(substr(x.course_title,1,36),36)||
			     v_regn_fee_defined||
			     lpad(v_regn_fee * 100,5,'0')||
			     rpad(v_first_fcast_grade_grdset,4)||
			     rpad(v_second_fcast_grade_grdset,4)||
			     x.jcgq_result_type||
			     rpad(x.jcgq_result_gradeset,4)||
			     rpad(v_second_grade_or_rslt_grdset,4)||
			     rpad(v_endorsement_to_first,4)||
			     rpad(v_endorsement_to_second,4)||
			     rpad(v_max_mark_or_ums,3)||
			     v_number_of_components
			    );
	elsif v_formats_version in ('13','14')
        then
	  pr_insert_edi_data(v_options_file_type||
			     v_record_type_5||
			     rpad(x.course||y.combination,6)||
			     rpad(x.course,6)||
			     rpad(x.jcgq_type,4)||
			     rpad(x.jcgq_level,3)||
			     rpad(v_exam_item,1)||
			     rpad(v_exam_type_qual_unit,4)||	
			     rpad(v_exam_type_level_unit,3)||	
			     rpad(v_exam_process_output,1)||
			     rpad(v_qca_classification_code,4)||
			     rpad(v_qca_accreditation_no,8)||
			     rpad(substr(x.course_title,1,36),36)||
			     v_regn_fee_defined||
			     lpad(v_regn_fee * 100,5,'0')||
			     rpad(v_first_fcast_grade_grdset,4)||
			     rpad(v_second_fcast_grade_grdset,4)||
			     x.jcgq_result_type||
			     rpad(x.jcgq_result_gradeset,4)||
			     rpad(v_second_grade_or_rslt_grdset,4)||
			     rpad(v_endorsement_to_first,4)||
			     rpad(v_endorsement_to_second,4)||
			     rpad(v_max_mark_or_ums,4)||			-- V13 change
			     v_number_of_components
			    );
	else
	  pr_insert_edi_data(v_options_file_type||
			     v_record_type_5||
			     rpad(x.course||y.combination,6)||
			     rpad(x.course,6)||
			     rpad(x.jcgq_type,4)||
			     rpad(x.jcgq_level,3)||
			     rpad(v_exam_item,1)||
			     rpad(v_exam_process_output,1)||
			     rpad(v_qca_classification_code,4)||
			     rpad(v_qca_accreditation_no,8)||
			     rpad(substr(x.course_title,1,36),36)||
			     v_regn_fee_defined||
			     lpad(v_regn_fee * 100,5,'0')||
			     v_first_fcast_grade_grdset||
			     v_second_fcast_grade_grdset||
			     x.jcgq_result_type||
			     x.jcgq_result_gradeset||
			     v_second_grade_or_rslt_grdset||
			     v_endorsement_to_first||
			     v_endorsement_to_second||
			     rpad(v_max_mark_or_ums,3)||
			     v_number_of_components
			    );
	end if;
     end loop;

   end loop;

/* Centre Trailer */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_trailer(v_options_file_type,v_options_record_length);

/* File Trailer */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_trailer(v_options_file_type,v_options_record_length);

    pr_update_emls(v_options_file_type);

  v_err_ref := 'Component File';

  for r1_rec in get_record_length(v_component_file_type,v_formats_version)
  loop
      v_component_record_length := r1_rec.rec_length;        
  end loop;

  /* Reset counts */

  v_no_of_recs_type7 := 0;
  v_no_of_recs_type9 := 0;
  v_row_count	     := 0;

  /* build the filename */

  if v_run_type = 'ENTRIES'
  then
    open get_filename(v_component_file_type,v_uab_entries);
  else
    open get_filename(v_component_file_type,v_uab);
  end if;

  fetch get_filename
  into  v_filename;
  if get_filename%notfound then
    --abort no error
    return;
  end if;

  close get_filename;

  -- dbms_output.put_line('Filename = '||v_filename);

  pr_insert_emlo;
  pr_insert_emls(v_component_file_type);
  pr_insert_emla;

/* File Header */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_header(v_component_file_type,v_component_record_length);

/* Centre Header */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_header(v_component_file_type,v_component_record_length);

/* Detail Records */

   for x in syllabus(v_voca_main_centre,p_course,p_approval_year) loop

     for y in options(x.app_no,x.award_code) loop

	v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
	v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

	pr_insert_edi_data(
			 v_component_file_type||
			 v_record_type_5||
			 rpad(x.course||y.combination||v_component_suffix,12)||
			 rpad(substr(x.course_title,1,36),36)||
			 v_teacher_marks||
			 rpad(v_max_mark,3)||
			 v_component_grdset||
			 rpad(v_due_date,6)||
			 v_timetabled||
			 rpad(v_timetable_date,6)||
			 v_timetable_session||
			 rpad(v_time_allowed,3,'0')
			);
     end loop;

   end loop;

/* Centre Trailer */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_trailer(v_component_file_type,v_component_record_length);

/* File Trailer */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_trailer(v_component_file_type,v_component_record_length);

    pr_update_emls(v_component_file_type);

  v_err_ref := 'Link File';

  for r1_rec in get_record_length(v_link_file_type,v_formats_version)
  loop
      v_link_record_length := r1_rec.rec_length;        
  end loop;

  /* Reset counts */

  v_no_of_recs_type7 := 0;
  v_no_of_recs_type9 := 0;
  v_row_count	     := 0;

  /* build the filename */

  if v_run_type = 'ENTRIES'
  then
    open get_filename(v_link_file_type,v_uab_entries);
  else
    open get_filename(v_link_file_type,v_uab);
  end if;

  fetch get_filename
  into  v_filename;
  if get_filename%notfound then
    --abort no error
    return;
  end if;

  close get_filename;

  -- dbms_output.put_line('Filename = '||v_filename);

  pr_insert_emlo;
  pr_insert_emls(v_link_file_type);
  pr_insert_emla;

/* File Header */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_header(v_link_file_type,v_link_record_length);

/* Centre Header */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_header(v_link_file_type,v_link_record_length);

/* Detail Records */

   for x in syllabus(v_voca_main_centre,p_course,p_approval_year) loop

     for y in options(x.app_no,x.award_code) loop

	v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
	v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

	pr_insert_edi_data(v_link_file_type||
			   v_record_type_5||
			   rpad(x.course||y.combination,6)||
			   rpad(x.course||y.combination||
					v_component_suffix,12)||
			   rpad(v_space,6)
			  );
     end loop;

   end loop;

/* Centre Trailer */

    v_no_of_recs_type7 := v_no_of_recs_type7 + 1;
    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_centre_trailer(v_link_file_type,v_link_record_length);

/* File Trailer */

    v_no_of_recs_type9 := v_no_of_recs_type9 + 1;

    pr_file_trailer(v_link_file_type,v_link_record_length);

    pr_update_emls(v_link_file_type);

    commit;

EXCEPTION
  WHEN OTHERS
  THEN
        raise_application_error(-20000,'pr_build_btec_jcgq_base_data '||
					v_err_ref||' '||sqlerrm);
END;
/

show errors

CREATE PUBLIC SYNONYM PR_BUILD_BTEC_JCGQ_BASE_DATA FOR IES.PR_BUILD_BTEC_JCGQ_BASE_DATA;


GRANT EXECUTE ON  IES.PR_BUILD_BTEC_JCGQ_BASE_DATA TO PUBLIC;

