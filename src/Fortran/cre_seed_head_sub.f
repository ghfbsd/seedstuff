	function cre_head_seed(anf_tim,end_tim,no_stat,stat_lkup,
     &	no_chan,chan_lkup,rate,no_span,span_st_tim,span_end_tim,no_ser,
     &	ser_st_tim,ser_st_idx,ser_end_tim,ser_end_idx,id_short,upd_fl,
     &	ird_shd,idsk,idsk1,idsk2,odir,idir)
c
c   Main subroutine for creation of SEED headers
c
	implicit integer(a-z)
	parameter (numer_mx = 1500)
	parameter (unit2=12)
	integer stat_lkup(*),no_chan(*),chan_lkup(100,15),ser_st_idx
     &	(100,100,15),ser_end_idx(100,100,15),no_ser(100,15),num_sthd
     &	(100),ibuf(1024)
	real rate(100,15),numer(numer_mx)
	real*8 rnd_tim,anf_tim,end_tim,start_etim,end_etim,
     &  span_st_tim(*),span_end_tim(*),
     &  ser_st_tim(100,100,15),ser_end_tim(100,100,15)
	character string*4096,blockette*15000,upd_fl*(*),
     &	odir*(*),idir*(*),ofile*132,ifile*132,tfile*132,stat*7,stats*7
	real*8 dnull
	logical lexi
c
 	include 'head_seed.inc'
	data dnull/0.0/
c
	write(*,*) 'Start creating SEED headers...'
	cre_head_seed=0
c
	ifile=idir(1:lenc(idir))//'seed_header.'
	lni=lenc(ifile)
	ofile=odir(1:lenc(odir))//'seed_header.'
	ln=lenc(ofile)
	write(idsk1,9) ofile(1:ln)//'vol'
9	format(a)
c
	if(no_span.gt.0) then
	  idl=0
	  anf_tim=rnd_tim(1,anf_tim,1.,tcr)
	  end_tim=rnd_tim(1,end_tim,1.,tcr)
	  if(tcr.gt.0) end_tim=end_tim+1.
	  do n=1,no_span
	    span_st_tim(n)=rnd_tim(1,span_st_tim(n),1.,tcr)
	    span_end_tim(n)=rnd_tim(1,span_end_tim(n),1.,tcr)
	    if(tcr.gt.0) span_end_tim(n)=span_end_tim(n)+1.
	  enddo
	else
	  idl=1
	  if(anf_tim .eq. 0 .and. end_tim .eq. 0) then
	     call abstim(1,iltim,1999,365,0,0,0)
	     end_tim=dble(iltim)
	  endif
	endif
c
	if(ird_shd.eq.0) then
	  open(idsk,file=ofile(1:ln)//'abr',status='unknown',
     &    access='direct',
     &	  form='unformatted',recl=4096)
	  write(idsk1,9) ofile(1:ln)//'abr'
	else
	  tfile=ifile(1:lni)//'abr'
	  inquire(idsk,name=tfile,exist=lexi)
	  if(.not.lexi) then
	    write(*,*) 'Could not find ',ifile(1:lni)//'abr'
	    stop
	  endif
	  open(idsk2,file=tfile,status='old',access='direct',
     &	  form='unformatted',recl=4096)
c	  write(*,*) 'Opened ',ifile(1:lni)//'abr'
	  write(idsk1,9) ifile(1:lni)//'abr'
	endif
c
	nums=1
	num=0
c	num=1
	if(ird_shd.eq.0) then
     	  istat=cre_new_record(0,'A',num,ncnt,string)
	  call write_form_abbr(idsk,form_name,form_abbr_lkup,fam_typ,
     &	  no_ddl_key,ddl_key,ncbl,blockette,ncnt,string,num)
	  do n=1,no_net
	    istat=write_gen_abbr(idsk,net_lookup(n),net_name(n),ncbl,
     &	    blockette,ncnt,string,num)
	  enddo
	  do n=1,no_inst_abbr
	    istat=write_gen_abbr(idsk,inst_abbr_lkup(n)+no_net,
     &      inst_name(n),ncbl,blockette,ncnt,string,num)
	  enddo
	  do n=1,no_unit_abbr
	    istat=write_unit_abbr(idsk,unit_abbr_lkup(n),unit_abbr(n),
     &	    unit_desc(n),ncbl,blockette,ncnt,string,num)
	  enddo
	  do n=1,no_com
	    istat=write_comments(idsk,com_lkp(n),'S',comment(n),ncbl,
     &	    blockette,ncnt,string,num)
	  enddo
c	  do n=1,no_paz_abbr
c	    istat=write_paz_abbr(idsk,paz_abbr_lkp(n),'A',
c     &	    resp_paz_nam(n),unit_abbr_lkup(3),unit_abbr_lkup(4),norm_afac(n),
c     &	    norm_afreq(n),no_azeros(n),azeros(1,n),no_apoles(n),apoles(1,n),
c     &	    ncbl,blockette,ncnt,string,num)
c	  enddo
c	  do n=1,no_coeff_abbr
c	    istat=write_coeff_abbr(idsk,coeff_abbr_lkp(n),'D',
c     &      resp_coeff_nam(n),unit_abbr_lkup(inp_unit_lkp(n)),unit_abbr_lkup
c     &	    (outp_unit_lkp(n)),no_num(n),numer(1,n),no_denom(n),denom,
c     &	    ncbl,blockette,ncnt,string,num)
c	write(*,*) 'coeff',n,coeff_abbr_lkp(n),resp_coeff_nam(n),no_num(n),ncbl,
c     &	ncnt,num,istat
c	  enddo
c	  do n=1,no_deci_abbr
c	    istat=write_deci_abbr(idsk,deci_abbr_lkp(n),resp_deci_nam(n),
c     &	    irate(n),deci_fac(n),deci_del(n),deci_corr(n),ncbl,
c     &	    blockette,ncnt,string,num)
c	  enddo
c	  do n=1,no_gain_abbr
c	    istat=write_gain_abbr(idsk,gain_abbr_lkp(n),resp_gain_nam(n),
c     &	    gain_fac(n),gain_freq(n),ncbl,blockette,ncnt,string,num)
c	  enddo
	 call flush_record(idsk,0,num,ncnt,string)
c	write(*,*) 'end of record',num
	else
	  ios=0
	  do while(ios.eq.0)
	    num=num+1
	    read(idsk2,rec=num,iostat=ios) ibuf
c	    if(ios.eq.0) write(idsk,rec=num) ibuf
	  enddo
	  num=num-1
	  write(*,*) 'Include ',ifile(1:lni)//'abr',' with',
     &	  num,' records'
	endif
c
	  no_stat_eff=0
	  no_up=0
	  do n=1,no_stat
	    no_up=no_up+1
	    nn=stat_lkup(n)
c	    write(*,*) 'start',n,no_stat,no_up,nn,no_chan(nn)
	    nlk=get_netlkp(nn,no_net,nst_net)
	    call parse_time(stat_st_tim(nn),start_etim)
	    if(stat_end_tim(nn).ne.'0/0') then
	      call parse_time(stat_end_tim(nn),end_etim)
	    else
	      end_etim=0.0
	    endif
c	    write(*,*) 'up',no_stat,n,no_up,more,nn,nlk
c	    if(no_up.eq.1.or.idl.eq.0) then
c
	      stat=stat_net(nn)
c             Check whether this station appears more than once, which can
c                happen if instrumentation changes at a station but name does
c                not change.  This will lead to different file names for
c                intermediate seed data.
	      irev=0
	      do i=1,n-1
	        if(stat .eq. stat_net(stat_lkup(i))) irev=irev+1
	      enddo
 	      if(optshd) then
	        write(*,*) '**Write new station record'
 	        write(*,19) '**Write ',stat(1:lenc(stat)),no_up
19	        format(2a,i2.2)
              endif
	      if(ird_shd.eq.0) then
	        close(idsk)
		write(stats,'(a,i2.2)') stat(1:lenc(stat)),irev
		if('00' .eq. stats(lenc(stat)+1:lenc(stat)+3)) then
		  stats(lenc(stat)+1:lenc(stat)+3)='  '
		endif
	        open(idsk,file=ofile(1:ln)//stats(1:lenc(stats)),
     &          status='unknown',
     &	        access='direct',form='unformatted',recl=4096)
	        write(idsk1,9) ofile(1:ln)//stats(1:lenc(stats))
	      else
	        tfile=ifile(1:lni)//stat(1:lenc(stat))
	        inquire(idsk,name=tfile,exist=lexi)
	        if(.not.lexi) then
	          write(*,*) 'Could not find ',tfile(1:lenc(tfile))
	          stop
	        endif
	        open(idsk2,file=tfile,status='old',
     &	        access='direct',form='unformatted',recl=4096)
 	        if(optshd) write(*,*) '**Opened ',
     &              ifile(1:lni)//stat(1:lenc(stat))
	        write(idsk1,9) tfile(1:lenc(tfile))
	      endif
c
	      no_stat_eff=no_stat_eff+1
	      nums=nums+num
c	      num_sthd(n)=num+1
	      num_sthd(n)=nums+1
c	      write(*,*) 'stat nums',num,nums,n,num_sthd(n)
	      num=0
	      if(ird_shd.eq.0)
     &           istat=cre_new_record(0,'S',num,ncnt,string)
c	    endif
c
	    if(ird_shd.eq.0) then
	      istat=write_stat_id(idsk,stat_net(nn),alat(nn),alon(nn),
     &	      elev(nn),no_chan(nn),site_name(nn),net_lookup(nlk),
     &	      start_etim,end_etim,upd_fl,net_code(nlk),ncbl,blockette,
     &	      ncnt,string,num)
c
	      do i=1,no_stcom(nn)
	        if((anf_tim.gt.stcom_st_etim(i,nn).and.anf_tim.lt.
     &	        stcom_end_etim(i,nn)).or.
     &	        (end_tim.gt.stcom_st_etim(i,nn).and.end_tim.lt.
     &	        stcom_end_etim(i,nn))) then
	          istat=write_stat_com(idsk,stcom_lkp(i,nn),
     &              stcom_st_etim(i,nn),stcom_end_etim(i,nn),
     &              ncbl,blockette,ncnt,string,num)
	        endif
	      enddo
c
	      do m=1,no_chan(nn)
	        mm=chan_lkup(nn,m)
		if(optshd) then
 	           write(*,9101) '**Output channel ',n,nn,nlk,m,mm
		   write(*,9102) chan_net(mm,nn),
     &               inst_lookup(mm,nn),angle(mm,nn),dip(mm,nn),
     &               rate(nn,mm),drift
9101               format(a,5(1x,i3))
9102               format(' ** ..',a,1x,i2,4(1x,f8.2))
                endif
	        istat=write_chan_id(idsk,chan_net(mm,nn),
     &	        inst_lookup(mm,nn)+no_net,resp_unit_lkp,cal_unit_lkp,
     &	        form_lookup,alat(nn),alon(nn),elev(nn),0.0,
     &          angle(mm,nn),dip(mm,nn),
     &          rate(nn,mm),drift,'TG',start_etim,end_etim,
     &	        upd_fl,ncbl,blockette,ncnt,string,num)
c
	        pzlk=resp_lkp(1,mm,nn)
	        istat=write_resp_paz(idsk,'A',1,paz_inp_lkp(pzlk),
     &	        paz_out_lkp(pzlk),norm_fac(pzlk),norm_freq(pzlk),
     &	        no_zeros(pzlk),zeros(1,pzlk),no_poles(pzlk),poles
     &	        (1,pzlk),ncbl,blockette,ncnt,string,num)
                if(optshd)write(*,9103)
     &             1,stag_gain(1,mm,nn),gain_paz_freq(pzlk)
9103            format(' stage ',i2,': gain ',1pg12.4,' at ',0pf7.3)
	        istat=write_chan_sens_gain(idsk,1,stag_gain(1,mm,nn),
     &	        gain_paz_freq(pzlk),ncbl,blockette,ncnt,string,num)

c
	        dglk=resp_lkp(2,mm,nn)
                pzlk=resp_digit_pz(dglk)
                if (0 .eq. pzlk) then
c                 If no analog response for digitizer, write a x1 decimation
c                 stage description with the digitizer sensitivity in it.
	          istat=write_resp_coeff(idsk,'D',2,digit_inp_unit_lkp
     &	          (dglk),digit_outp_unit_lkp(dglk),0,0.0,0,0.0,ncbl,
     &	          blockette,ncnt,string,num)
	          istat=write_deci(idsk,2,digit_rate(dglk),1,0.0,0.0,
     &	          ncbl,blockette,ncnt,string,num)
                else
c                 Analog response for digitizer, write pole-zero and
c                 digitizer sensitivity for it.
	          istat=write_resp_paz(idsk,'A',2,paz_inp_lkp(pzlk),
     &	          paz_out_lkp(pzlk),norm_fac(pzlk),norm_freq(pzlk),
     &	          no_zeros(pzlk),zeros(1,pzlk),no_poles(pzlk),poles
     &	          (1,pzlk),ncbl,blockette,ncnt,string,num)
                endif
                if(optshd)write(*,9103)
     &            2,stag_gain(2,mm,nn),digit_gain_freq(dglk)
	        istat=write_chan_sens_gain(idsk,2,stag_gain(2,mm,nn),
     &	        digit_gain_freq(dglk),ncbl,blockette,ncnt,string,num)

c               Write FIR stages if present and requested
                   
	        if(no_stag(mm,nn).gt.2.and.id_short.ne.1) then
		  if(optshd) then
9104                format(2a,i2,a,2(1x,i2),a)
		    print 9104,'**Output long stage info ',
     &                'chan ',mm,' of sta',nn,no_stag(mm,nn),' stages'
                  endif
	          do k=3,no_stag(mm,nn)
	            colk=resp_lkp(k,mm,nn)
		    if(optshd) then
9105                  format(a,i2,a,i2,a,i3)
 	              print 9105,'**Stage ',k,', resp_lkp ',colk,
     &                  ', no_num ',no_num(colk)
                    endif
		    if (no_num(colk).gt.0) then
	              i=lenc(resp_coeff_pfx)
	              open(unit2,file=resp_coeff_pfx(1:i) // '/' //
     &                          resp_coeff_nam(colk),status='old')
                      ie=lenc(resp_coeff_nam(colk))
9106                  format('**Bad FIR coefficient file: ',a,'skip.')
                      nerr=0
                      do i=1,min(no_num(colk),numer_mx)
                        read(unit2,*,iostat=ios) ie,numer(i)
                        if((i.ne.ie .or. ios.ne.0) .and. nerr.eq.0)then
                          print 9106,resp_coeff_nam(colk)(1:ie)
                          nerr=nerr+1
                        endif
                      enddo
                      close(unit2)
                    endif
	            istat=write_resp_coeff(idsk,'D',k,
     &              inp_unit_lkp(colk),
     &	            outp_unit_lkp(colk),no_num(colk),numer,0,numer,
     &	            ncbl,blockette,ncnt,string,num)
	            istat=write_deci(idsk,k,irate(colk),deci_fac(colk),
     &	            deci_del(colk),deci_corr(colk),ncbl,blockette,ncnt,
     &	            string,num)
                    if(optshd)write(*,9103)
     &                 k,gain_fac(colk),gain_freq(colk)
	            istat=write_chan_sens_gain(idsk,k,gain_fac(colk),
     &	            gain_freq(colk),ncbl,blockette,ncnt,string,num)
	          enddo
	        endif
c
c	        istat=write_resp_ref(idsk,no_stag(mm,nn)-2,resp_lkp(3,mm,nn),
c     &	        no_coeff_abbr,no_deci_abbr,ncbl,blockette,ncnt,
c     &	        string,num)
c
                if(optshd)
     &            write(*,9103)0,sens_fac(mm,nn),sens_freq(mm,nn)
	        istat=write_chan_sens_gain(idsk,0,sens_fac(mm,nn),
     &	        sens_freq(mm,nn),ncbl,blockette,ncnt,string,num)
	      enddo
c	      call flush_record(idsk,0,num,ncnt,string)
c	write(*,*) 'flush of record',num,more
c	      if(more.eq.0) then
	        call flush_record(idsk,0,num,ncnt,string)
	        no_up=0
c	write(*,*) 'flush of record',num,more
c	      endif
	    else
	      ios=0
	      do while(ios.eq.0)
	        num=num+1
	        read(idsk2,rec=num,iostat=ios) ibuf
c	        if(ios.eq.0) write(idsk,rec=num) ibuf
	      enddo
	      num=num-1
	      no_up=0
	      write(*,*) 'Include ',ifile(1:lni)//stat(1:lenc(stat)),
     &        ' with',num,' records'
	    endif
	  enddo
c
	if(idl.eq.1) then
c
	  close(idsk)
	  open(idsk,file=ofile(1:ln)//'vol',status='unknown',
     &	  access='direct',form='unformatted',recl=4096)
c
	  num=0
	  istat=cre_new_record(0,'V',num,ncnt,string)
	    istat=write_vol_id(idsk,anf_tim,end_tim,dmc,label,ncbl,
     &	    blockette,ncnt,string,num)
	    istat=write_sthd_idx(idsk,no_stat,no_stat_eff,stat_lkup,
     &      stat_net,num_sthd,ncbl,blockette,ncnt,string,num)
	    istat=write_sphd_idx(idsk,no_span,no_stat,span_st_tim,
     &	    span_end_tim,num_sphd,ncbl,blockette,ncnt,string,num)
	  call flush_record(idsk,0,num,ncnt,string)
c	write(*,*) 'end of V1 record',num
	  return
	endif
c
c	num_sphd=num+1
	nums=nums+num
	num_sphd=nums+1
	iend=0
c	num_st=num
	num_st=nums
	nloop=0
c	write(*,*) 'num_sphd',num,nums,num_st,num_sphd
c
	close(idsk)
	if(optshd) then
	  write(*,*) '**Write time span records'
        endif
	open(idsk,file=ofile(1:ln)//'tim',status='unknown',
     &	access='direct',form='unformatted',recl=4096)
	write(idsk1,9) ofile(1:ln)//'tim'
c
1001	nloop=nloop+1
c
c	  num=num_st
	  nums=num_st
	  numss=num_st
c	  write(*,*) 'loop',nloop,num,nums,num_st,no_span
	  num=0
	  istat=cre_new_record(0,'T',num,ncnt,string)
	  do n=1,no_span
	    if(optshd) then
	      call tfix(span_st_tim(n),its,iths)
	      call abstim(0,its,iyrs,idys,ihs,ims,iss)
	      call datum(0,idys,iyrs,mons,itgs)
	      call tfix(span_end_tim(n),its,ithe)
	      call abstim(0,its,iyre,idye,ihe,ime,ise)
	      call datum(0,idye,iyre,mone,itge)
	      write(*,9001) ' **Span id ',n,num,
     &	          itgs,mons,iyrs,ihs,ims,iss,iths,
     &	          itge,mone,iyre,ihe,ime,ise,ithe
9001          format(a,2(1x,i3),1x,
     &            2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,' - ',
     &            2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4)
            endif
	    istat=write_span_id(idsk,'P',
     &        span_st_tim(n),span_end_tim(n),ncbl,
     &	      blockette,ncnt,string,num)
	  enddo
c
c	  no_fil=0
	  nrec_max=0
c
	  do n=1,no_stat
	    nn=stat_lkup(n)
c	    write(*,*) 'n',n,nn,no_chan(nn)
	    do m=1,no_chan(nn)
	      mm=chan_lkup(nn,m)
	      do k=1,no_ser(nn,mm)
	        nrec_max=max(nrec_max,ser_end_idx(k,nn,mm))
	      enddo
	    enddo
	  enddo
c	  write(*,*) 'nrec_max',nrec_max
c
c	  nb_off=num+nloop-1
	  nums=numss+num
	  nb_off=nums+nloop-1
	  nrec=0
	  istgt=1
	  do while (nrec.lt.nrec_max.and.istgt.eq.1)
	    istgt=get_next_rec(nrec,no_stat,stat_lkup,no_chan,
     &        chan_lkup,no_ser,ser_st_idx,ser_end_idx,nn,mm,k)
	    if(istgt.eq.1) then
	      nlk=get_netlkp(nn,no_net,nst_net)
	      if(optshd) then
	        write(*,9002) '**Span index',num,
     &            net_code(nlk),stat_net(nn),chan_net(mm,nn),
     &            ser_st_idx(k,nn,mm)+nb_off,
     &            ser_end_idx(k,nn,mm)+nb_off
9002            format(' ',a,i3,' for',3(1x,a),' at',2(1x,i5))
              endif
	      istat=write_timsr_idx(idsk,
     &          stat_net(nn),chan_net(mm,nn),
     &	        ser_st_tim(k,nn,mm),ser_st_idx(k,nn,mm)+nb_off,
     &	        ser_end_tim(k,nn,mm),ser_end_idx(k,nn,mm)+nb_off,
     &	        0,dnull,0,net_code(nlk),ncbl,blockette,ncnt,string,
     &	        num)
	      nrec=ser_end_idx(k,nn,mm)
	      nums=numss+num
	      if(nums.gt.num_st+nloop) then
	        write(*,*) 'Too few time span records - ',
     &            'add one more',nums,num_st,nloop
	        goto 1001
	      endif
	    endif
	  enddo
c
	call flush_record(idsk,1,num,ncnt,string)
c	write(*,*) 'end of record',num
c
c	write(*,*) 'Volume ID 2'
	num=0
c
	close(idsk)
	open(idsk,file=ofile(1:ln)//'vol',status='unknown',
     &	access='direct',form='unformatted',recl=4096)
c
	istat=cre_new_record(0,'V',num,ncnt,string)
	  istat=write_vol_id(idsk,anf_tim,end_tim,dmc,label,ncbl,
     &    blockette,ncnt,string,num)
	  istat=write_sthd_idx(idsk,no_stat,no_stat_eff,stat_lkup,
     &    stat_net,num_sthd,ncbl,blockette,ncnt,string,num)
	  istat=write_sphd_idx(idsk,no_span,no_stat,
     &    span_st_tim,span_end_tim,
     &	  num_sphd,ncbl,blockette,ncnt,string,num)
	call flush_record(idsk,0,num,ncnt,string)
c	write(*,*) 'end of record',num
	close(idsk)
c
	return
	end
	function get_next_rec(nrec,no_stat,stat_lkup,no_chan,chan_lkup,
     &	no_ser,ser_st_idx,ser_end_idx,nn,mm,k)
c
	implicit integer (a-z)
	integer stat_lkup(*),no_chan(*),chan_lkup(100,15),ser_st_idx
     &	(100,100,15),ser_end_idx(100,100,15),no_ser(100,15)	
c
	get_next_rec=0
	n=0
	do while (n.lt.no_stat.and.get_next_rec.eq.0)
	  n=n+1
	  nn=stat_lkup(n)
	  m=0
	  do while (no_chan(nn).gt.0.and.m.lt.no_chan(nn).and.
     &	  get_next_rec.eq.0)
	    m=m+1
	    mm=chan_lkup(nn,m)
c	write(*,*) 'noser',nn,mm,no_ser(nn,mm)
	    k=0
	    do while (no_ser(nn,mm).gt.0.and.k.lt.no_ser(nn,mm).and.
     &	    get_next_rec.eq.0)
	      k=k+1
c	write(*,*) n,nn,m,mm,k,nn,mm,ser_st_idx(k,nn,mm),ser_end_idx(k,nn,mm),nrec
	      if(ser_st_idx(k,nn,mm).eq.nrec+1) then
c	write(*,*) 'hurra',n,nn,m,mm,k,nn,mm,ser_st_idx(k,nn,mm),ser_end_idx(k,nn,mm)
c     &	,ser_st_tim(k,nn,mm),ser_end_tim(k,nn,mm)
	        get_next_rec=1
	        if(ser_end_idx(k,nn,mm).lt.ser_st_idx(k,nn,mm)) then
	          write(*,*) 'Time index error: ',
     &            'Last index smaller than first',
     &	          ser_st_idx(k,nn,mm),ser_end_idx(k,nn,mm)
	          get_next_rec=2
	        endif
	      endif
	    enddo
	  enddo
	enddo
c
	if(get_next_rec.eq.0) then
	  write(*,*) 'get_next_rec warning: ',
     &    'continuing record not found:',nrec+1
	endif  
c
	return
	end
	function write_vol_id(idsk,anf_tim,end_tim,dmc,label,
     &  block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	character string*(*),blockette*(*),dmc*(*),label*(*)
	real*8 anf_tim,end_tim,vol_tim
	real*4 vers
	external c_time
	data no_block/10/,vers/2.3/,rec_length/12/
c
	write_vol_id=0
	block_count=0
	blockette=' '
c
	ivol_tim=c_time()
c	ivol_tim=0
	vol_tim=ivol_tim
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_real(0,vers,4,1,block_count,blockette)
	istat=fill_int(0,rec_length,2,block_count,blockette)
	istat=fill_time(1,anf_tim,block_count,blockette)
	istat=fill_time(1,end_tim,block_count,blockette)
	istat=fill_time(1,vol_tim,block_count,blockette)
	istat=fill_char(1,dmc,len,block_count,blockette)
	istat=fill_char(1,label,len,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_sthd_idx(idsk,no_stat,no_stat_eff,nstat,stat,
     &  num_sthd,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	integer nstat(*),num_sthd(*)
	character string*(*),blockette*(*),stat(*)*(*)
	data no_block/11/
c
	write_sthd_idx=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,no_stat_eff,3,block_count,blockette)
	do n=1,no_stat
	  nn=nstat(n)
	  if(num_sthd(n).gt.0) then
	    istat=fill_char(0,stat(nn),5,block_count,blockette)
	    istat=fill_int(0,num_sthd(n),6,block_count,blockette)
	  endif
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_sphd_idx(idsk,no_span,no_stat,anf_tim,end_tim,
     &	num_sphd,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*8 anf_tim(*),end_tim(*)
	character string*(*),blockette*(*)
	data no_block/12/
c
	write_sphd_idx=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,no_span,4,block_count,blockette)
	do n=1,no_span
	  istat=fill_time(1,anf_tim(n),block_count,blockette)
	  istat=fill_time(1,end_tim(n),block_count,blockette)
	  istat=fill_int(0,num_sphd,6,block_count,blockette)
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	subroutine write_form_abbr(idsk,form_name,form_lookup,fam_typ,
     &	no_ddl_key,ddl_key,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	character string*(*),blockette*(*),form_name*(*),
     &	ddl_key(*)*(*)
	data no_block/30/
c
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_char(1,form_name,len,block_count,blockette)
	istat=fill_int(0,form_lookup,4,block_count,blockette)
	istat=fill_int(0,fam_typ,3,block_count,blockette)
	istat=fill_int(0,no_ddl_key,2,block_count,blockette)
	do n=1,no_ddl_key
	  istat=fill_char(1,ddl_key(n),len,block_count,blockette)
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_comments(idsk,com_lkp,com_code,comment,
     &	block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	character string*(*),blockette*(*),comment*(*),com_code*(*)
	data no_block/31/,com_lev/0/
c
	write_comments=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,com_lkp,4,block_count,blockette)
	istat=fill_char(0,com_code,1,block_count,blockette)
	istat=fill_char(1,comment,len,block_count,blockette)
	istat=fill_int(0,com_lev,3,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_gen_abbr(idsk,abbr_lookup,abbr_desc,block_count,
     &	blockette,ncnt,string,num)
c
	implicit integer (a-z)
	character string*(*),blockette*(*),abbr_desc*(*)
	data no_block/33/
c
	write_gen_abbr=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,abbr_lookup,3,block_count,blockette)
	istat=fill_char(1,abbr_desc,len,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_unit_abbr(idsk,unit_lookup,unit_abbr,unit_desc,
     &	block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	character string*(*),blockette*(*),unit_abbr*(*),
     *	unit_desc*(*)
	data no_block/34/
c
	write_unit_abbr=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,unit_lookup,3,block_count,blockette)
	istat=fill_char(1,unit_abbr,len,block_count,blockette)
	istat=fill_char(1,unit_desc,len,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_paz_abbr(idsk,resp_lkp,resp_typ,resp_nam,
     &	stag_inp_lookup,stag_out_lookup,norm_fac,norm_freq,no_zeros,
     &	zeros,no_poles,poles,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	complex zeros(*),poles(*)
	real*4 norm_fac,norm_freq
	character string*(*),blockette*(*),resp_typ*1,resp_nam*(*)
	data no_block/43/
c
	write_paz_abbr=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,resp_lkp,4,block_count,blockette)
	istat=fill_char(1,resp_nam,len,block_count,blockette)
	istat=fill_char(0,resp_typ,1,block_count,blockette)
	istat=fill_int(0,stag_inp_lookup,3,block_count,blockette)
	istat=fill_int(0,stag_out_lookup,3,block_count,blockette)
	istat=fill_real_exp(0,norm_fac,12,5,block_count,blockette)
	istat=fill_real_exp(0,norm_freq,12,5,block_count,blockette)
	istat=fill_int(0,no_zeros,3,block_count,blockette)
	do n=1,no_zeros
	  istat=fill_real_exp(0,real(zeros(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,aimag(zeros(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	enddo
	istat=fill_int(0,no_poles,3,block_count,blockette)
	do n=1,no_poles
	  istat=fill_real_exp(0,real(poles(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,aimag(poles(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_coeff_abbr(idsk,resp_lkp,resp_typ,resp_nam,
     &	stag_inp_lookup,stag_out_lookup,no_num,numer,
     &	no_denom,denom,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*4 numer(*),denom(*)
	character string*(*),blockette*(*),resp_typ*1,resp_nam*(*)
	data no_block/44/
c
	write_coeff_abbr=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,resp_lkp,4,block_count,blockette)
	istat=fill_char(1,resp_nam,len,block_count,blockette)
	istat=fill_char(0,resp_typ,1,block_count,blockette)
	istat=fill_int(0,stag_inp_lookup,3,block_count,blockette)
	istat=fill_int(0,stag_out_lookup,3,block_count,blockette)
	istat=fill_int(0,no_num,4,block_count,blockette)
	do n=1,no_num
	  istat=fill_real_exp(0,numer(n),12,5,block_count,blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	enddo
	istat=fill_int(0,no_denom,4,block_count,blockette)
	do n=1,no_denom
	  istat=fill_real_exp(0,denom(n),12,5,block_count,blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_deci_abbr(idsk,resp_lkp,resp_nam,rate,
     &	   dec_fac,dec_del,dec_corr,block_count,blockette,ncnt,string,
     &     num)
c
	implicit integer (a-z)
	real*4 rate,dec_del,dec_corr
	character string*(*),blockette*(*),resp_nam*(*)
	data no_block/47/,dec_off/0/
c
	write_deci_abbr=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,resp_lkp,4,block_count,blockette)
	istat=fill_char(1,resp_nam,len,block_count,blockette)
	istat=fill_real_exp(0,rate,10,4,block_count,blockette)
	istat=fill_int(0,dec_fac,5,block_count,blockette)
	istat=fill_int(0,dec_off,5,block_count,blockette)
	istat=fill_real_exp(0,dec_del,11,4,block_count,blockette)
	istat=fill_real_exp(0,dec_corr,11,4,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_gain_abbr(idsk,resp_lkp,resp_nam,sens_fac,sens_freq,
     &	block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*4 sens_fac,sens_freq
	character string*(*),blockette*(*),resp_nam*(*)
	data no_block/48/,no_hist/0/
c
	write_gain_abbr=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,resp_lkp,4,block_count,blockette)
	istat=fill_char(1,resp_nam,len,block_count,blockette)
	istat=fill_real_exp(0,sens_fac,12,5,block_count,blockette)
	istat=fill_real_exp(0,sens_freq,12,5,block_count,blockette)
	istat=fill_int(0,no_hist,2,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_stat_id(idsk,stat,lat,lon,elev,no_chan,
     &    site_name,net_lookup,st_eff_date,end_eff_date,upd_fl,
     &    net_id,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*4 lat,lon,elev
	real*8 st_eff_date,end_eff_date
	character string*(*),blockette*(*),stat*(*),upd_fl*(*),
     &	site_name*(*),net_id*(*)
	data no_block/50/,long_ord/3210/,word_ord/10/
c
	write_stat_id=0
	block_count=0
	blockette=' '
c	write(*,*) 'stat_id',stat,lat,lon,elev,no_chan,site_name,net_lookup
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_char(0,stat,5,block_count,blockette)
	istat=fill_real(0,lat,10,6,block_count,blockette)
	istat=fill_real(0,lon,11,6,block_count,blockette)
	istat=fill_real(0,elev,7,1,block_count,blockette)
	istat=fill_int(0,no_chan,4,block_count,blockette)
	istat=fill_int(0,0,3,block_count,blockette)	!no station comments
	istat=fill_char(1,site_name,len,block_count,blockette)
	istat=fill_int(0,net_lookup,3,block_count,blockette)
	istat=fill_int(0,long_ord,4,block_count,blockette)
	istat=fill_int(0,word_ord,2,block_count,blockette)
	istat=fill_time(2,st_eff_date,block_count,blockette)
	istat=fill_time(2,end_eff_date,block_count,blockette)
	istat=fill_char(0,upd_fl,1,block_count,blockette)
	istat=fill_char(0,net_id,2,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_stat_com(idsk,stat_com_lkp,
     &    st_eff_date,end_eff_date,
     &    block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*8 st_eff_date,end_eff_date
	character string*(*),blockette*(*)
	data no_block/51/,com_lev/0/
c
	write_stat_com=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_time(2,st_eff_date,block_count,blockette)
	istat=fill_time(2,end_eff_date,block_count,blockette)
	istat=fill_int(0,stat_com_lkp,4,block_count,blockette)
	istat=fill_int(0,com_lev,6,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_chan_id(idsk,chan_id,inst_lookup,resp_lookup,
     &  cal_lookup,form_lookup,lat,lon,elev,depth,angle,dip,rate,drift,
     &	chan_fl,st_eff_date,end_eff_date,upd_fl,block_count,blockette,
     &	ncnt,string,num)
c
	implicit integer (a-z)
	real*4 lat,lon,elev,depth,angle,dip,rate,drift
	real*8 st_eff_date,end_eff_date
	character string*(*),blockette*(*),chan_id*(*),chan_fl*(*),
     &	upd_fl*(*),loc_id*2
	data no_block/52/,loc_id/'__'/,sub_ch_lookup/0/,rec_length/12/
c
	write_chan_id=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_char(0,loc_id,2,block_count,blockette)
	istat=fill_char(0,chan_id,3,block_count,blockette)
	istat=fill_int(0,sub_ch_lookup,4,block_count,blockette)
	istat=fill_int(0,inst_lookup,3,block_count,blockette)
	istat=fill_char(0,'~',1,block_count,blockette)	!no opt. comment
	istat=fill_int(0,resp_lookup,3,block_count,blockette)
	istat=fill_int(0,cal_lookup,3,block_count,blockette)
	istat=fill_real(0,lat,10,6,block_count,blockette)
	istat=fill_real(0,lon,11,6,block_count,blockette)
	istat=fill_real(0,elev,7,1,block_count,blockette)
	istat=fill_real(0,depth,5,1,block_count,blockette)
	istat=fill_real(0,angle,5,1,block_count,blockette)
	istat=fill_real(0,dip,5,1,block_count,blockette)
	istat=fill_int(0,form_lookup,4,block_count,blockette)
	istat=fill_int(0,rec_length,2,block_count,blockette)
	istat=fill_real_exp(0,rate,10,4,block_count,blockette)
	istat=fill_real_exp(0,drift,10,4,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)	!no channel comments
	istat=fill_char(1,chan_fl,len,block_count,blockette)
	istat=fill_time(2,st_eff_date,block_count,blockette)
	istat=fill_time(2,end_eff_date,block_count,blockette)
	istat=fill_char(0,upd_fl,1,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_resp_paz(idsk,trans_typ,no_stag,stag_inp_lookup,
     &	stag_out_lookup,norm_fac,norm_freq,no_zeros,zeros,no_poles,
     &  poles,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	complex zeros(*),poles(*)
	real*4 norm_fac,norm_freq
	character string*(*),blockette*(*),trans_typ*1
	data no_block/53/
c
	write_resp_paz=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_char(0,trans_typ,1,block_count,blockette)
	istat=fill_int(0,no_stag,2,block_count,blockette)
	istat=fill_int(0,stag_inp_lookup,3,block_count,blockette)
	istat=fill_int(0,stag_out_lookup,3,block_count,blockette)
	istat=fill_real_exp(0,norm_fac,12,5,block_count,blockette)
	istat=fill_real_exp(0,norm_freq,12,5,block_count,blockette)
	istat=fill_int(0,no_zeros,3,block_count,blockette)
	do n=1,no_zeros
	  istat=fill_real_exp(0,real(zeros(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,aimag(zeros(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	enddo
	istat=fill_int(0,no_poles,3,block_count,blockette)
	do n=1,no_poles
	  istat=fill_real_exp(0,real(poles(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,aimag(poles(n)),12,5,block_count,
     &       blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	  istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_resp_coeff(idsk,resp_type,
     &  no_stag,stag_inp_lookup,stag_out_lookup,
     &  no_num,numer,no_denom,denom,block_count,
     &	blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*4 numer(*),denom(*)
	character string*(*),blockette*(*),resp_type*1
	logical force
	data no_block/54/
c
	write_resp_coeff=0
c
c       Watch out for long blockettes.  Split up into more than one if
c          too long.
        no_perblk=(9999 - (3+4+1+2+3+3+4+4))/24
	non=0
	nod=0
	force=.true.
c
        do while(non.lt.no_num .or. nod.lt.no_denom .or. force)
c         Fill next blockette
          force=.false.
	  block_count=0
	  blockette=' '
	  istat=fill_int(0,no_block,3,block_count,blockette)
	  istat=fill_int(0,0,4,block_count,blockette)
	  istat=fill_char(0,resp_type,1,block_count,blockette)
	  istat=fill_int(0,no_stag,2,block_count,blockette)
	  istat=fill_int(0,stag_inp_lookup,3,block_count,blockette)
	  istat=fill_int(0,stag_out_lookup,3,block_count,blockette)
	  if (non.ge.no_num) then
c           No more numerators to fill, report zero
	    istat=fill_int(0,0,4,block_count,blockette)
	    no_blk=0
	  else
c           More numerators to fill.  Insert as many as will fit.
	    no_blk=min(no_num-non,no_perblk)
	    istat=fill_int(0,no_blk,4,block_count,blockette)
	    do n=1,no_blk
	      istat=fill_real_exp(0,numer(n+non),12,5,block_count,
     &                            blockette)
	      istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	    enddo
	    non=non+no_blk
	  endif
c         Check if exceeded blockette size.  Flush if so after adding
c           a zero denom. count.
	  if(no_blk.ge.no_perblk) then
	    istat=fill_int(0,0,4,block_count,blockette)
	    call check_block(idsk,block_count,blockette,ncnt,string,
     &                        num)
	  else
	    if (nod.ge.no_denom) then
	      istat=fill_int(0,0,4,block_count,blockette)
	    else
	      no_blk=min(no_denom-nod,no_perblk)
	      istat=fill_int(0,no_blk,4,block_count,blockette)
	      do n=1,no_blk
	        istat=fill_real_exp(0,denom(n+nod),12,5,block_count,
     &                              blockette)
	        istat=fill_real_exp(0,0.0,12,5,block_count,blockette)
	      enddo
	      nod=nod+no_blk
	    endif
	    if(no_blk.ge.no_perblk) then
	      call check_block(idsk,block_count,blockette,ncnt,string,
     &                          num)
            endif
	  endif
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_deci(idsk,no_stag,rate,dec_fac,dec_del,dec_corr,
     &	block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*4 rate,dec_del,dec_corr
	character string*(*),blockette*(*)
	data no_block/57/,dec_off/0/
c
	write_deci=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,no_stag,2,block_count,blockette)
	istat=fill_real_exp(0,rate,10,4,block_count,blockette)
	istat=fill_int(0,dec_fac,5,block_count,blockette)
	istat=fill_int(0,dec_off,5,block_count,blockette)
	istat=fill_real_exp(0,dec_del,11,4,block_count,blockette)
	istat=fill_real_exp(0,dec_corr,11,4,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_chan_sens_gain(idsk,no_stag,sens_fac,sens_freq,
     &	block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*4 sens_fac,sens_freq
	character string*(*),blockette*(*)
	data no_block/58/,no_hist/0/
c
	write_chan_sens_gain=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,no_stag,2,block_count,blockette)
	istat=fill_real_exp(0,sens_fac,12,5,block_count,blockette)
	istat=fill_real_exp(0,sens_freq,12,5,block_count,blockette)
	istat=fill_int(0,no_hist,2,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_resp_ref(idsk,no_stag,resp_lkp,no_coeff_abbr,
     &	no_deci_abbr,block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	integer resp_lkp(*)
	character string*(*),blockette*(*)
	data no_block/60/stag_off/2/
c
	write_resp_ref=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_int(0,no_stag,2,block_count,blockette)
	do n=1,no_stag
	  istat=fill_int(0,n+stag_off,2,block_count,blockette)
	  no_resp=3
	  istat=fill_int(0,no_resp,2,block_count,blockette)
	  istat=fill_int(0,resp_lkp(n),4,block_count,blockette)
	  deci_lkp=resp_lkp(n)+no_coeff_abbr
	  gain_lkp=resp_lkp(n)+no_coeff_abbr+no_deci_abbr
	  istat=fill_int(0,deci_lkp,4,block_count,blockette)
	  istat=fill_int(0,gain_lkp,4,block_count,blockette)
	enddo
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_span_id(idsk,span_fl,anf_tim,end_tim,
     &     block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	real*8 anf_tim,end_tim
	character string*(*),blockette*(*),span_fl*(*)
	data no_block/70/
c
	write_span_id=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_char(0,span_fl,1,block_count,blockette)
	istat=fill_time(1,anf_tim,block_count,blockette)
	istat=fill_time(1,end_tim,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function write_timsr_idx(idsk,stat,chan_id,ser_st_tim,
     &  ser_st_idx,ser_end_tim,ser_end_idx,
     &  no_rep,rep_tim,rep_idx,net_id,
     &	block_count,blockette,ncnt,string,num)
c
	implicit integer (a-z)
	integer rep_idx(*)
	real*8 ser_st_tim,ser_end_tim,rep_tim(*)
	character string*(*),blockette*(*),stat*(*),chan_id*(*),
     &  net_id*(*),loc_id*2
	data no_block/74/,loc_id/'__'/
c
	write_timsr_idx=0
	block_count=0
	blockette=' '
c
	istat=fill_int(0,no_block,3,block_count,blockette)
	istat=fill_int(0,0,4,block_count,blockette)
	istat=fill_char(0,stat,5,block_count,blockette)
	istat=fill_char(0,loc_id,2,block_count,blockette)
	istat=fill_char(0,chan_id,3,block_count,blockette)
	istat=fill_time(1,ser_st_tim,block_count,blockette)
	istat=fill_int(0,ser_st_idx,6,block_count,blockette)
	istat=fill_int(0,1,2,block_count,blockette)	!no sub-sequence => first1
	istat=fill_time(1,ser_end_tim,block_count,blockette)
	istat=fill_int(0,ser_end_idx,6,block_count,blockette)
	istat=fill_int(0,1,2,block_count,blockette)	!no sub-sequence => last 1
c
	istat=fill_int(0,no_rep,3,block_count,blockette)
	if(no_rep.gt.0) then
	  do n=1,no_rep
	    istat=fill_time(1,rep_tim(n),block_count,blockette)
	    istat=fill_int(0,rep_idx(n),6,block_count,blockette)
	    istat=fill_int(0,0,2,block_count,blockette)	!no sub-sequence no
	  enddo
	endif
c
	istat=fill_char(0,net_id,2,block_count,blockette)
c
	call check_block(idsk,block_count,blockette,ncnt,string,num)
c
	return
	end
	function cre_new_record(mode,id,num,ncnt,string)
c
	implicit integer (a-z)
	character string*(*),id*(*)
c
	cre_new_record=0
c
	num=num+1
	string=' '
	ncnt=0
	istat=fill_int(0,num,6,ncnt,string)
	istat=fill_char(0,id,1,ncnt,string)
	if(mode.eq.0) then
	  istat=fill_char(0,' ',1,ncnt,string)
	else
	  istat=fill_char(0,'*',1,ncnt,string)
	endif
c
	return
	end
	subroutine flush_record(idsk,mode,num,ncnt,string)
c
	implicit integer (a-z)
	character string*(*)
c
	do i=1,4096
	  if(string(i:i).eq.'_') string(i:i)=' '
	enddo
	write(*,*) 'Write SEED header record',num,' of type ',
     &     string(7:8)
	write(idsk,rec=num) string(1:4096)
c
	return
	end
	subroutine check_block(idsk,block_count,blockette,ncnt,string,
     &     num)
c
	implicit integer (a-z)
	character rec_typ*1,string*(*),blockette*(*)
	data noff/7/
c
	mcbl=3
	istat=fill_int(0,block_count,4,mcbl,blockette)
c	write(*,*) 'flush:  ',num,ncnt,block_count,ncnt+block_count,' ',blockette(1:7),noff
c	if(ncnt+block_count.le.4097-noff) then
	if(ncnt+block_count.lt.4097-noff) then
	  string(ncnt+1:ncnt+block_count)=blockette(1:block_count)
	else
	  off=0
c	  do while (ncnt+block_count.gt.4097-noff)
	  do while (ncnt+block_count.ge.4097-noff)
	    nble=4096-ncnt
	    string(ncnt+1:4096)=blockette(off+1:off+nble)
	    call flush_record(idsk,0,num,ncnt,string)
	    rec_typ=string(7:7)
	    istat=cre_new_record(1,rec_typ,num,ncnt,string)
	    block_count=block_count-nble
	    off=off+nble
	  enddo
	  string(ncnt+1:ncnt+block_count)=
     &       blockette(off+1:off+block_count)
	endif
	ncnt=lenc(string)
c
	return
	end
	function fill_int(mode,int,len,ncnt,string)
c
	implicit integer (a-z)
	character string*(*),form*8
c
	fill_int=0
c
	if(mode.eq.0) then
	  write(form,'(2h(i,i2,1h.,i2,1h))') len,len
	elseif(mode.eq.1) then
	  nn=0
	  n=0
	  dowhile (n.lt.iabs(int))
	    nn=nn+1
	    n=10**nn-1
	  enddo
	  if(int.le.0) nn=nn+1
	  write(form,'(2h(i,i2,1h))') nn
	  len=nn
	endif
	ncnt=ncnt+1
	write(string(ncnt:ncnt+len-1),form) int
	ncnt=lenc(string)
c
	return
	end
	function fill_real(mode,vari,len1,len2,ncnt,string)
c
	implicit integer (a-z)
	character string*(*),form*7
	real*4 vari
c
	fill_real=0
c
	write(form,'(2h(f,i2.2,1h.,i1.1,1h))') len1,len2
c
	ncnt=ncnt+1
	nend=ncnt+len1-1
	write(string(ncnt:nend),form) vari
	do i=ncnt,nend
           if (0 .eq. index(' +-0',string(i:i))) go to 10
	   string(i:i) = '0'
        enddo
10      continue
        if (vari.lt.0) then
	   if (string(ncnt:ncnt) .eq. '0') then
	      string(ncnt:ncnt) = '-'
	   else
	      fill_real=1
	   endif
        endif
c
	ncnt=lenc(string)
c
	return
	end
	function fill_real_exp(mode,vari,len1,len2,ncnt,string)
c
	implicit integer (a-z)
	character string*(*),form*10
	real*4 vari
c
	fill_real_exp=0
c
	nn=len2+6
	if(vari.lt.0.) nn=nn+1
	no_null=len1-nn
	write(form,'(5h(1p e,i2.2,1h.,i1,1h))') len1,len2	
c
	ncnt=ncnt+1
	write(string(ncnt:ncnt+len1-1),form) vari
c9	format(1p e<len1>.<len2>)
c
	if(no_null.gt.0) then
	  do i=1,no_null
	    string(ncnt+i-1:ncnt+i-1)='_'
	  enddo
	endif
c	write(*,*) vari,len1,len2,nn,no_null,string(ncnt:ncnt+len1-1)
c
	ncnt=lenc(string)
c
	return
	end
	function fill_time(mode,time,ncnt,string)
c
	implicit integer (a-z)
	real*8 time,stime
	character string*(*)
c
	fill_time=0
c
	stime=time+0.00005
	itime=stime
	tsec=(stime-dble(itime))*10000.
	call abstim(0,itime,year,doy,hour,min,sec)
	ncnt=ncnt+1
	if(mode.eq.1) then
	  write(string(ncnt:ncnt+22),9) year,doy,hour,min,sec,tsec
9	format(i4.4,1h,,i3.3,1h,,2(i2.2,1h:),i2.2,1h.,i4.4,1h~)
	elseif(mode.eq.2) then
	  if(year.gt.1970) then
	    write(string(ncnt:ncnt+3),'(i4.4)') year
	    ncnt=ncnt+4
	    if(doy.gt.0) then
	      write(string(ncnt:ncnt+3),'(1h,,i3.3)') doy
	      ncnt=ncnt+4
	      if(hour.gt.0.or.min.gt.0.or.sec.gt.0) then
	        write(string(ncnt:ncnt+8),19) hour,min,sec
19	format(1h,,i2.2,2(1h:,i2.2))
	        ncnt=ncnt+9
	      endif
	    endif
	  endif
	  write(string(ncnt:ncnt+1),'(1h~)')
	endif
c
	ncnt=lenc(string)
c
	return
	end
	function fill_char(mode,char,len,ncnt,string)
c
	implicit integer (a-z)
	character string*(*),char*(*)
c
	fill_char=0
c
	if(mode.eq.1) len=lenc(char)
c	istat=str$upcase(char_1,char)
	do n=1,len
	  ncnt=ncnt+1
c	  string(ncnt:ncnt)=char_1(n:n)
	  string(ncnt:ncnt)=char(n:n)
	  if(string(ncnt:ncnt).eq.' ') string(ncnt:ncnt)='_'
	enddo
	ncnt=ncnt+1
	if(mode.eq.1) string(ncnt:ncnt)='~'
c
	ncnt=lenc(string)
c
	return
	end
        function get_netlkp(stlkp,no_net,nst_net)
c
        implicit integer(a-z)
        integer nst_net(*)
c
        ntlkp=0
        ianf=1
        do i=1,no_net
          if(stlkp.ge.ianf.and.stlkp.le.ianf+nst_net(i)-1) ntlkp=i
c       type *,i,no_net,stlkp,ianf,nst_net(i),ianf+nst_net(i)-1,ntlkp
          ianf=ianf+nst_net(i)
        enddo
        get_netlkp=ntlkp
c
        return
        end
