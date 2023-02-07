c   SEED header information
c
c   Abbreviations
c
c   Format abbreviation
        parameter (nfaemx=2,ndkmx=10)
	integer form_abbr_lkup(nfaemx),fam_typ(nfaemx)
	character form_name(nfaemx)*50,ddl_key(ndkmx)*60
	common /formd/no_form_abbr,form_abbr_lkup,fam_typ,no_ddl_key,
     &  form_name,ddl_key
c   Comments
        parameter (ncemx=50)
	integer no_com,com_lkp(ncemx)
	character comment(ncemx)*70,com_code*1
	common /commd/no_com,com_lkp,com_code,comment
c   Instrument abbreviation
        parameter (niaemx=15)
	integer inst_abbr_lkup(niaemx)
	character inst_name(niaemx)*50
	common /instd/no_inst_abbr,inst_abbr_lkup,inst_name
c   Unit abbreviation
        parameter (nuaemx=5)
	integer unit_abbr_lkup(nuaemx)
	character unit_abbr(nuaemx)*20,unit_desc(nuaemx)*50
	common /unitd/no_unit_abbr,unit_abbr_lkup,unit_abbr,unit_desc
c   Poles & Zeros dictionary
        parameter (npzemx=15,npzzmx=5,npzpmx=9)
	integer paz_abbr_lkp(npzemx),no_zeros(npzemx),no_poles(npzemx),
     &  paz_inp_lkp(npzemx),paz_out_lkp(npzemx)
	character resp_paz_nam(npzemx)*25
	complex zeros(npzzmx,npzemx),poles(npzpmx,npzemx)
	real*4 norm_fac(npzemx),norm_freq(npzemx)
	real*4 gain_paz_fac(npzemx),gain_paz_freq(npzemx)
	common /respd/no_paz_abbr,paz_abbr_lkp,no_zeros,no_poles,
     &  norm_fac,norm_freq,gain_paz_fac,gain_paz_freq,zeros,poles,
     &  paz_inp_lkp,paz_out_lkp,resp_paz_nam
c   Digitizer dictionary
        parameter(mx_digit_abbr=10)
	integer digit_inp_unit_lkp(mx_digit_abbr)
	integer digit_outp_unit_lkp(mx_digit_abbr)
        integer resp_digit_pz(mx_digit_abbr)
	character resp_digit_nam(mx_digit_abbr)*25
	real*4 digit_rate(mx_digit_abbr)
	real*4 digit_gain_fac(mx_digit_abbr)
	real*4 digit_gain_freq(mx_digit_abbr)
	common /digitd/digit_inp_unit_lkp,digit_outp_unit_lkp
	common /digitd/digit_rate,digit_gain_fac,digit_gain_freq
	common /digitd/resp_digit_pz,resp_digit_nam
c   Coefficients dictionary
        parameter(mx_coeff_abbr=25, mx_digit_stage=11)
	integer coeff_abbr_lkp(mx_coeff_abbr)
	integer no_num(mx_coeff_abbr)
	integer no_denom(mx_coeff_abbr)
	integer inp_unit_lkp(mx_coeff_abbr)
	integer outp_unit_lkp(mx_coeff_abbr)
	character resp_coeff_nam(mx_coeff_abbr)*25,resp_coeff_pfx*128
	common /coeffd/ no_coeff_abbr,coeff_abbr_lkp,no_num,no_denom,
     &	inp_unit_lkp,outp_unit_lkp,resp_coeff_nam,resp_coeff_pfx
c   Decimation Dictionary
	integer deci_abbr_lkp(mx_coeff_abbr),deci_fac(mx_coeff_abbr)
	character resp_deci_nam(mx_coeff_abbr)*25
	real*4 irate(mx_coeff_abbr),deci_del(mx_coeff_abbr)
	real*4 deci_corr(mx_coeff_abbr)
	common /decid/ no_deci_abbr,deci_abbr_lkp,irate,deci_fac,
     &    deci_del,deci_corr,resp_deci_nam
c   Gain Dictionary
	integer gain_abbr_lkp(mx_coeff_abbr)
	character resp_gain_nam(mx_coeff_abbr)*25
	real*4 gain_fac(mx_coeff_abbr),gain_freq(mx_coeff_abbr)
	common /gaind/ no_gain_abbr,gain_abbr_lkp,gain_fac,gain_freq,
     &	resp_gain_nam
c
c   Fixed items for station description
	integer net_lookup(9),nst_net(9)
	integer no_stcom(100),stcom_lkp(10,100)
	real*4 alat(100),alon(100),elev(100)
	real*8 stcom_st_etim(10,100),stcom_end_etim(10,100)
	character net(9)*7,net_code(9)*2,stat_net(100)*5,
     &  site_name(100)*60,
     &	net_name(9)*50,stat_st_tim(100)*18,stat_end_tim(100)*18,dmc*80,
     &	label*80
	common /statd/no_net,net_lookup,nst_net,nst_tot,alat,alon,elev,
     &	no_stcom,stcom_lkp,stcom_st_etim,stcom_end_etim,net,net_name,
     &	net_code,stat_net,site_name,stat_st_tim,stat_end_tim,dmc,label
c
c   Fixed items for channel description
        parameter (mx_ch_sta=15)
	integer nch_tot(100),inst_lookup(mx_ch_sta,100)
	integer no_stag(mx_ch_sta,100)
	integer resp_lkp(mx_digit_stage,mx_ch_sta,100)
	character chan_net(mx_ch_sta,100)*3
	real*4 ldepth,angle(mx_ch_sta,100),dip(mx_ch_sta,100)
	real*4 sens_fac(mx_ch_sta,100)
        real*4 sens_freq(mx_ch_sta,100),orate(mx_ch_sta,100),drift
        real*4 stag_gain(mx_digit_stage,mx_ch_sta,100)
	common /chand/nch_tot,inst_lookup,resp_unit_lkp,cal_unit_lkp,
     &  ldepth,angle,dip,form_lookup,orate,drift,sens_fac,sens_freq,
     &  no_stag,resp_lkp,stag_gain,chan_net
c
c   Debugging
        parameter (mx_opt=4)
	logical optarr(mx_opt)
        logical optpar, optshd, opttim, optdat
	equivalence (optarr(1),optpar),(optarr(2),optshd)
	equivalence (optarr(3),opttim),(optarr(4),optdat)
	common /debug/optarr
