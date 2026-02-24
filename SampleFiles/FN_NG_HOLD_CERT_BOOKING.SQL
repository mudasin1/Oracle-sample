create or replace function fn_ng_hold_cert_booking
(
	p_in_reg_no		in	varchar2
)
return	number
is
	v_booking_id	number	:= 0;

	cursor c1 ( c_reg_no varchar2 ) is
	select	pbaa_id
	from	assessments, award_title_rules, approval_application, approval_awards, prom_btec_ass_bookings, students
	where	st_reg_no = c_reg_no
	and	pbaa_st_reg_no = nvl(st_scheme_reg_no,st_reg_no)
	and	aw_course_number = st_course_id
	and	aa_applicat_no = aw_applicat_no
	and	atru_at_number = to_number(aa_btec_title)
	and	asse_atru_id = atru_id
	and	asse_catalogue_name = pbaa_catalogue_name
	and	(
			( asse_admo_id = 7 and asse_catalogue_name not in ('L6011967','R6011968') ) or
			( asse_admo_id in (8,9,10) ) or
			( asse_admo_id = 11 )
		)
	and	not exists
		(
			select	null
			from	student_assessment_results
			where	sare_st_reg_no = st_reg_no
			and	sare_asse_id = asse_id
			and	trunc(sare_datetime) = trunc(pbaa_booking_dtime)
			and	sare_aout_id <> 'W'
			union
			select	null
			from	student_assessment_results sa1,
				assessments as1,
				award_title_rules at1,
				approval_application aa1,
				approval_awards aw1,
				transfer_students ts1
			where	ts1.ts_reg_no = st_reg_no
			and	aw1.aw_course_number = ts1.ts_course_id
			and	aa1.aa_applicat_no = aw1.aw_applicat_no
			and	at1.atru_at_number = to_number(aa1.aa_btec_title)
			and	as1.asse_atru_id = at1.atru_id
			and	as1.asse_catalogue_name = pbaa_catalogue_name
			and	sa1.sare_st_reg_no = ts1.ts_reg_no
			and	sa1.sare_asse_id+0 = as1.asse_id
			and	trunc(sa1.sare_datetime) = trunc(pbaa_booking_dtime)
			and	sa1.sare_aout_id <> 'W'
		)
	order by pbaa_id;

begin
	open c1 ( p_in_reg_no );
	fetch c1 into v_booking_id;
	if c1%notfound
	or   p_in_reg_no in
( 'K547274',
 'K547276',
 'K547291',
 'K547301',
 'K547307',
 'K547314',
 'K547265' )
 then
		v_booking_id := 0;
	end if;
	close c1;

	return v_booking_id;
end fn_ng_hold_cert_booking;
 