	function rd_options(nopt,opt,opt_val)
	implicit integer (a-z)
	character opt(*)*(*),opt_val(*)*(*),string*500
c
	narg=iargc()
	nopt=0
	nval=0
	if(narg.gt.0) then
	  do n=1,narg
	    call getarg(n,string)
	    lenstr=lenc(string)
	    if(index(string,'>').gt.0) lenstr=index(string,'>')-1
c	write(*,*) lenstr,index(string,'>')
	    if(string(1:1).eq.'-') then
	      nopt=nopt+1
	      opt(nopt)=string(2:lenstr)
	      if(nopt.gt.nval+1) then
	        nval=nopt-1
	        opt_val(nval)=' '
	      endif
c	write(*,*) 'opt',n,nopt,nval,string(1:lenstr)
	    else
	      nval=nval+1
	      opt_val(nval)=string(1:lenstr)
c             if(n.eq.1.and.nval.gt.nopt) then  !this appears to be a bug
	      if(nval.gt.nopt) then
	        nopt=nval
	        opt(nopt)=' '
	      endif
c	write(*,*) 'val',n,nopt,nval,string(1:lenstr)
	    endif
	  enddo
	  rd_options=1
	else
	  rd_options=0
	endif
c	write(*,*) 'options'
c	  do n=1,nopt
c	    write(*,*) n,opt(n),' ',opt_val(n)
c	  enddo
c
	return
	end
	function get_opt(opt,value,nopt,options,opt_val)
c
	implicit integer(a-z)
	character opt*(*),value*(*),options(*)*(*),opt_val(*)*(*)
c
	get_opt=0
	do n=1,nopt
	  if(options(n).eq.opt) then
	    get_opt=1
	    value=opt_val(n)
	  endif
	enddo
c	write(*,*) 'get_opt',get_opt,opt,' ',value
c
	return
	end
	function lenc(string)
c
	character string*(*)
c
	lnc=len(string)
	lnc1=index(string,' ')-1
	lnc2=index(string,char(0))-1
	if(lnc1.ge.0) lnc=min(lnc,lnc1)
	if(lnc2.ge.0) lnc=min(lnc,lnc2)
	lenc=lnc
c
	return
	end
	function len0(string)
c
	character string*(*)
c
	lnc=len(string)
	lnc2=lnc
	iend=0
	do while (lnc2.gt.0.and.iend.eq.0)
	  if(string(lnc2:lnc2).ne.' ') then
	    iend=1
	  else
	    lnc2=lnc2-1
	  endif
	enddo
	if(lnc2.gt.0) lnc=min(lnc,lnc2)
	len0=lnc
c
	return
	end
	subroutine parse_seed_db(unit1,unit2,cfgfile,stat_sel,net_sel)
c
c   reads and parses the SEED header information database file
c
	implicit integer (a-z)
	character string*4096,cfg_dir*132,cfg_env*132,dstring*80,
     &	char_tim*10,cfgfile*(*),stat_sel*(*),net_sel*(*),net_st*7,
     &	stat_st*5
        logical ex
	include 'head_seed.inc'
	data cfg_env/'SEED_STUFF_HOME'/
c
	intsel=0
	if(net_sel.ne.' ') intsel=1
	istsel=0
	if(stat_sel.ne.' ') istsel=1
	call getenv(cfg_env,cfg_dir)
	if(cfgfile.eq.' ') then
	  cfgfile=cfg_dir(1:lenc(cfg_dir))//'/copy_seed.cfg'
	endif
	open(unit1,file=cfgfile,status='old')
c     &	readonly)
	write(*,*) 'Loading SEED database from ',
     &    cfgfile(1:lenc(cfgfile))
c
c   Abbreviation header information
c
	no_form_abbr=0
	next=0
	istat=0
	do while (istat.eq.0)
	  istat=get_string(unit1,next,'Format_abbr',lens,string)
	  if(istat.eq.0) then
	    no_form_abbr=no_form_abbr+1
	    if (no_form_abbr.gt.nfaemx) then
	      write(0,9002) 'Format_abbr',nfaemx
	      stop
	    endif
	    istps=parse_int(string,1,form_abbr_lkup(no_form_abbr))
	    istps=parse_char(string,2,form_name(no_form_abbr))
	    istps=parse_int(string,3,fam_typ(no_form_abbr))
	    n=3
	    no_ddl_key=0
	    istps=0
	    do while(istps.eq.0)
	      no_ddl_key=no_ddl_key+1
	      if (no_ddl_key.gt.ndkmx) then
	        write(0,9002) 'Format_abbr (DDL key)',ndkmx
	        stop
	      endif
	      n=no_ddl_key+3
	      istps=parse_char(string,n,ddl_key(no_ddl_key))
            enddo
	    no_ddl_key=no_ddl_key-1
	  endif
	enddo
c
	istat=0
	no_com=0
	next=0
	do while (istat.eq.0)
	  istat=get_string(unit1,next,'Comment',lens,string)
	  if(istat.eq.0) then
	    no_com=no_com+1
	    if (no_com.gt.ncemx) then
	      write(0,9002) 'Comment',ncemx
	      stop
	    endif
	    istps=parse_int(string,1,com_lkp(no_com))
	    istps=parse_char(string,2,com_code)
	    istps=parse_char(string,3,comment(no_com))
	  endif
	enddo
c
	istat=0
	no_inst_abbr=0
	next=0
	do while (istat.eq.0)
	  istat=get_string(unit1,next,'Inst_abbr',lens,string)
	  if(istat.eq.0) then
	    no_inst_abbr=no_inst_abbr+1
	    if (no_inst_abbr.gt.niaemx) then
	      write(0,9002) 'Inst_abbr',nuaemx
	      stop
	    endif
	    istps=parse_int(string,1,inst_abbr_lkup(no_inst_abbr))
	    istps=parse_char(string,2,inst_name(no_inst_abbr))
	  endif
	enddo
c
	istat=0
	no_unit_abbr=0
	next=0
	do while (istat.eq.0)
	  istat=get_string(unit1,next,'Unit_abbr',lens,string)
	  if(istat.eq.0) then
	    no_unit_abbr=no_unit_abbr+1
	    if (no_unit_abbr.gt.nuaemx) then
	      write(0,9002) 'Unit_abbr',nuaemx
	      stop
	    endif
	    istps=parse_int(string,1,unit_abbr_lkup(no_unit_abbr))
	    istps=parse_char(string,2,unit_abbr(no_unit_abbr))
	    istps=parse_char(string,3,unit_desc(no_unit_abbr))
	  endif
	enddo
c
	istat=0
	no_paz_abbr=0
	next=0
	do while (istat.eq.0)
	  istat=get_string(unit1,next,'Resp_paz',lens,string)
	  if(istat.eq.0) then
	    no_paz_abbr=no_paz_abbr+1
	    if (no_paz_abbr.gt.npzemx) then
	      write(0,9002) 'Resp_paz',npzemx
	      stop
	    endif
	    istps=parse_int(string,1,paz_abbr_lkp(no_paz_abbr))
	    istps=parse_char(string,2,resp_paz_nam(no_paz_abbr))
	    istps=parse_int(string,3,no_zeros(no_paz_abbr))
	    istps=parse_int(string,4,no_poles(no_paz_abbr))
	    istps=parse_real(string,5,norm_fac(no_paz_abbr))
	    istps=parse_real(string,6,norm_freq(no_paz_abbr))
	    istps=parse_real(string,7,gain_paz_fac(no_paz_abbr))
	    istps=parse_real(string,8,gain_paz_freq(no_paz_abbr))
	    istps=parse_int(string,9,paz_inp_lkp(no_paz_abbr))
	    istps=parse_int(string,10,paz_out_lkp(no_paz_abbr))
 
9001        format('**Resp_paz - too many ',a,' in response ',i2,
     &        ' for ',a,'; max is ',i2)
9002        format('**',a,' - too many in cfg file; max is ',i2)
	    if (no_zeros(no_paz_abbr) .gt. npzzmx) then
              n=lenc(resp_paz_nam(no_paz_abbr))
	      write(0,9001) 'zeroes', paz_abbr_lkp(no_paz_abbr),
     &          resp_paz_nam(no_paz_abbr)(1:n),npzzmx
              stop
	    endif
	    if (no_poles(no_paz_abbr) .gt. npzpmx) then
              n=lenc(resp_paz_nam(no_paz_abbr))
	      write(0,9001) 'zeroes', paz_abbr_lkp(no_paz_abbr),
     &          resp_paz_nam(no_paz_abbr)(1:n),npzpmx
              stop
	    endif

	    nn=10
	    do n=1,no_zeros(no_paz_abbr)
	      nn=nn+1
	      istps=parse_cmplx(string,nn,zeros(n,no_paz_abbr))
	    enddo
	    do n=1,no_poles(no_paz_abbr)
	      nn=nn+1
	      istps=parse_cmplx(string,nn,poles(n,no_paz_abbr))
	    enddo
	  endif
	enddo
c
	next=0
        istat=get_string(unit1,next,'Resp_digit',lens,string)
	do while (istat.eq.0)
	  istps=parse_int(string,1,nn)
	  if (nn.gt.mx_digit_abbr.or.nn.le.0) then
	    write(0,*) '**Resp_digit - lookup key ',
     &        nn,' must be > 0 and <= ',mx_digit_abbr
          else
	    istps=parse_char(string,2,resp_digit_nam(nn))
	    istps=parse_int(string,3,digit_inp_unit_lkp(nn))
	    istps=parse_int(string,4,digit_outp_unit_lkp(nn))
	    istps=parse_real(string,5,digit_rate(nn))
	    istps=parse_real(string,6,digit_gain_fac(nn))
	    istps=parse_real(string,7,digit_gain_freq(nn))
	  endif
          istat=get_string(unit1,next,'Resp_digit',lens,string)
	enddo
c
	istat=0
	no_coeff_abbr=0
	no_deci_abbr=0
	no_gain_abbr=0
	next=0
	do while (istat.eq.0)
	  istat=get_string(unit1,next,'Resp_coeff',lens,string)
	  if(istat.eq.0) then
	    istps=parse_int(string,1,nn)
	    if (nn.gt.mx_coeff_abbr.or.nn.le.0) then
	      write(0,*) '**Resp_coeff - lookup key ',
     &          nn,' must be > 0 and <=',mx_coeff_abbr
            else
	      no_coeff_abbr=max(no_coeff_abbr,nn)
	      istps=parse_int(string,1,coeff_abbr_lkp(nn))
	      istps=parse_char(string,2,resp_coeff_nam(nn))
	      istps=parse_int(string,3,no_num(nn))
	      istps=parse_int(string,4,no_denom(nn))
	      istps=parse_int(string,5,inp_unit_lkp(nn))
	      istps=parse_int(string,6,outp_unit_lkp(nn))
	      no_deci_abbr=max(no_deci_abbr,nn)
	      istps=parse_real(string,7,irate(nn))
	      istps=parse_int(string,8,deci_fac(nn))
	      istps=parse_real(string,9,deci_del(nn))
	      istps=parse_real(string,10,deci_corr(nn))
	      no_gain_abbr=max(no_gain_abbr,nn)
	      istps=parse_real(string,11,gain_fac(nn))
	      istps=parse_real(string,12,gain_freq(nn))
c
	      resp_coeff_pfx=cfg_dir
	      if (resp_coeff_pfx .eq. ' ') resp_coeff_pfx = '.'
	      inquire(file=resp_coeff_pfx(1:lenc(resp_coeff_pfx))
     &           // '/' //
     &           resp_coeff_nam(nn)(1:lenc(resp_coeff_nam(nn))),
     &	         exist=ex)
              if(.not.ex) then
	        n=lenc(resp_coeff_nam(nn))
	        write(0,'(3a)') '**Resp_coeff - file bad:',
     &            resp_coeff_nam(nn)(1:n),', skipping.'
                no_num(nn)=0
	      endif
	    endif
	  endif
	enddo
c
c   Station header information
c
	istat=0
	no_net=0
	ns_net=0
	nst_tot=0
	nst_fnd=0
	next=0
	do while (istat.eq.0)
	  istat=get_string(unit1,next,'Network',lens,string)
	  if(istat.eq.0) then
	    istps=parse_char(string,1,net_st)
	    if(intsel.eq.0) then
	      no_net=no_net+1
	    elseif(intsel.eq.1.and.net_st.eq.net_sel) then
	      no_net=1
	      nst_tot=0
	      nst_fnd=0
	    else
	      no_net=2
	      nst_tot=50
	    endif
	    net(no_net)=net_st
	    net_lookup(no_net)=no_net
	    istps=parse_char(string,2,net_code(no_net))
	    istps=parse_char(string,3,net_name(no_net))
c
	    istfl=0
	    next1=2
	    nst_net(no_net)=0
	    do while (istfl.eq.0)
	      istfl=get_string(unit1,next1,'Station',lens,string)
	      if(istfl.eq.0) then
	        istps=parse_char(string,1,stat_st)
	        if(istsel.eq.0) then
	          nst_tot=nst_tot+1
	        elseif(istsel.eq.1.and.stat_st.eq.stat_sel) then
	          nst_fnd=nst_fnd+1
	          nst_tot=nst_fnd
	          ns_net=no_net
	        else
	          nst_tot=nst_fnd+1
	        endif
	        nst_net(no_net)=nst_net(no_net)+1
	        stat_net(nst_tot)=stat_st
	        istps=parse_char(string,2,site_name(nst_tot))
	        istps=parse_real(string,3,alat(nst_tot))
	        istps=parse_real(string,4,alon(nst_tot))
	        istps=parse_real(string,5,elev(nst_tot))
	        istps=parse_char(string,6,stat_st_tim(nst_tot))
	        istps=parse_char(string,7,stat_end_tim(nst_tot))
c	write(*,*) stat_st_tim(nst_tot),stat_end_tim(nst_tot)
c
	        no_stcom(nst_tot)=0
	        istfl1=0
	        next2=2
	        do while (istfl1.eq.0)
	          istfl1=get_string(unit1,next2,'Station_comment',lens,
     &              string)
	          if(istfl1.eq.0) then
	            no_stcom(nst_tot)=no_stcom(nst_tot)+1
	            istps=parse_int(
     &                string,1,stcom_lkp(no_stcom(nst_tot),nst_tot))
	            istps=parse_char(string,2,char_tim)
	            call parse_time(char_tim,
     &                stcom_st_etim(no_stcom(nst_tot),nst_tot))
	            istps=parse_char(string,3,char_tim)
	            call parse_time(char_tim,
     &                stcom_end_etim(no_stcom(nst_tot),nst_tot))
	          endif
	        enddo
c
		nch=0
	        next2=2
	        istfl1=get_string(unit1,next2,'Channel',lens,string)
	        do while (istfl1.eq.0)
	          nch=nch+1
		  if (nch.gt.mx_ch_sta) then
	            write(0,*) '**Channel - ',nch,' channels per ',
     &                'station too many, must be <=',mx_ch_sta,
     &                '; later channels omitted.'
                    go to 2000
		  endif

	          istps=parse_char(string,1,chan_net(nch,nst_tot))
	          istps=parse_int(string,2,inst_lookup(nch,nst_tot))
	          istps=parse_int(string,3,resp_unit_lkp)
	          istps=parse_int(string,4,cal_unit_lkp)
	          istps=parse_real(string,5,ldepth)
	          istps=parse_real(string,6,angle(nch,nst_tot))
	          istps=parse_real(string,7,dip(nch,nst_tot))
	          istps=parse_int(string,8,form_lookup)
	          istps=parse_real(string,9,orate(nch,nst_tot))
	          istps=parse_real(string,10,drift)

		  if (optpar) then
		     print*,'**Parse channel:',chan_net(nch,nst_tot)
		     print*,'..',ldepth,
     &                      angle(nch,nst_tot),dip(nch,nst_tot),
     &                      orate(nch,nst_tot)
		  endif
c
		  mm=0
	          do i=11,12
	            istps=parse_char(string,i,dstring)
	            idx=index(dstring,'/')
	            if(idx.gt.0) then
		      mm=mm+1
	              read(dstring(1:idx-1),*)
     &         		  resp_lkp(mm,nch,nst_tot)
	              read(dstring(idx+1:lenc(dstring)),*)
     &                    stag_gain(mm,nch,nst_tot)
	            endif
		    if (optpar) then
 		       write(*,*) '..resp_lkp(',mm,nch,nst_tot,') = ',
     &         		  resp_lkp(mm,nch,nst_tot)
                    endif
	          enddo
c
	          istps=parse_char(string,13,dstring)
	          if(dstring.ne.'0/0') then
c                   Parse strings of form "m", "m/n", "m-n"
		    nss=0
	            ldx=1
	            idx=1
	            lens=lenc(dstring)+1
		    dstring(lens:lens)='/'
	            do while (idx.le.lens)
	              if (0.eq.index('0123456789',
     &                               dstring(idx:idx))) then
			if(idx.gt.ldx) then
	                  read(dstring(ldx:idx-1),*) nsnew
			  if('/'.eq.dstring(idx:idx)) then
			    if(nss.eq.0) then
		              mm=mm+1
			      if (mm.le.mx_digit_stage) then
               		        resp_lkp(mm,nch,nst_tot)=nsnew
                                stag_gain(mm,nch,nst_tot)=0.0
			      endif
c		              write(*,*)
c    &                          'resp_lkp(',mm,nch,nst_tot,') = ',
c    &         		        resp_lkp(mm,nch,nst_tot)
			    else
			      do ldx=nss,nsnew
			        mm=mm+1
			        if (mm.le.mx_digit_stage) then
               		          resp_lkp(mm,nch,nst_tot)=ldx
                                  stag_gain(mm,nch,nst_tot)=0.0
			        endif
c		                write(*,*)
c    &                            'resp_lkp(',mm,nch,nst_tot,') = ',
c    &         		          resp_lkp(mm,nch,nst_tot)
			      enddo
			      nss=0
			    endif
			  elseif('-'.eq.dstring(idx:idx)) then
			    if(nss.eq.0) nss=nsnew
			  else
			    write(*,*) '**Channel:  Bad FIR coeff.',
     &                        ' key ',dstring(1:lens)
                            nss=0
                          endif
			endif
			ldx=idx+1
		      endif
		      idx=idx+1
	            enddo
	          endif
		  if (mm.gt.mx_digit_stage) then
	            write(0,*) '**Resp_coeff - ',mm,' decimation ',
     &                'stages too many, must be <=',mx_digit_stage,
     &                '; later stages omitted.'
                    mm=mx_digit_stage
		  endif
	          no_stag(nch,nst_tot)=mm
c		  write(*,*) 'no_stag(',nch,nst_tot,') = ',mm
c
	          istps=parse_real(string,14,sens_fac(nch,nst_tot))
	          istps=parse_real(string,15,sens_freq(nch,nst_tot))
		  if (optpar) then
		     print*,'..sens',
     &                  sens_fac(nch,nst_tot),
     &                  sens_freq(nch,nst_tot)
		  endif

2000              continue
	          istfl1=get_string(unit1,next2,'Channel',lens,string)
	        enddo
	        nch_tot(nst_tot)=min(nch,mx_ch_sta)
	      endif
	    enddo
	  endif
	enddo
c
	if(intsel.eq.1) then
	  no_net=1
	  nst_tot=nst_net(1)
	endif
c
	if(istsel.eq.1) then
	    no_net=1
	    nst_net(no_net)=nst_net(ns_net)
	    net(no_net)=net(ns_net)
	    net_lookup(no_net)=no_net
	    net_code(no_net)=net_code(ns_net)
	    net_name(no_net)=net_name(ns_net)
	    nst_net(no_net)=nst_net(ns_net)
	    nst_tot=nst_fnd
	endif
c
	next=0
	istat=get_string(unit1,next,'DMC',lens,string)
	istps=parse_char(string,1,dmc)
c
	close(unit1)
c
	return
	end
	function get_string(unit,next,ident,lens,string)
c
	implicit integer (a-z)
	character ident*(*),string*(*),buffer*132
c
	ind=0
	if(next.eq.0) rewind unit
	do while (ind.eq.0)
	  buffer='#'
	  ios=0
	  lenb=1
	  do while (buffer(1:1).eq.'#'.and.ios.eq.0)
	    read(unit,'(a)',iostat=ios) buffer
	    lenb=len0(buffer)
	  enddo
	  id_end=index(buffer,'>')-1
	  if(buffer(1:id_end).eq.ident) then
	    ind=1
	    start=id_end+3
	    string=buffer(start:lenb)
	    lens=lenb-start+1
	      do while (buffer(lenb:lenb).eq.'>')
	        read(unit,'(a)',iostat=ios) buffer
	        lenb=len0(buffer)
                start=index(buffer,'>')+2
	        lens=lens-2
	        string=string(1:lens)//buffer(start:lenb)
	        lens=lens+lenb-start+1
	      enddo
	  else
	    ind=0
	    if(ios.ne.0) ind=2
	    if(next.eq.2) ind=3
	  endif
	enddo
c
	if(ind.eq.1) then
	  get_string=0
	  if(next.eq.0) next=1
	else
	  get_string=2
	  if(ind.eq.3) backspace unit
	endif
c
	return
	end
	function get_sub_string(string,count,sub_string)
c
	implicit integer (a-z)
	character string*(*),sub_string*(*)
c
	get_sub_string=0
	lens=len(string)
	ia=1
	ie=1
	off=0
	do n=1,count
	  ia=ie+off
	  idx=index(string(ia:),' ')+ia-1
	  ie=idx-1
	  do off=1,lens-ie
	     if (string(ie+off:ie+off).ne.' ') exit
	  enddo
	  if(idx.le.ia) then
c	    write(*,*) 'error or end of string',ia,ie
	    ie=lens
	    get_sub_string=2
	  endif
	enddo
	sub_string=string(ia:ie)

	return
	end
	function parse_int(string,count,int)
c
	implicit integer (a-z)
	character string*(*),sub_string*132
c
	parse_int=0
	istat=get_sub_string(string,count,sub_string)
	if(istat.eq.0) read(sub_string,*) int
	parse_int=istat
c
	return
	end
	function parse_real(string,count,real)
c
	implicit integer (a-z)
	real*4 real
	character string*(*),sub_string*132
c
	parse_real=0
	istat=get_sub_string(string,count,sub_string)
	if(istat.eq.0) read(sub_string,*) real
	lens=lenc(string)
	lenss=lenc(sub_string)
	parse_real=istat
c
	return
	end
	function parse_cmplx(string,count,cmplx)
c
	implicit integer (a-z)
	complex*8 cmplx
	character string*(*),sub_string*132
c
	parse_cmplx=0
	istat=get_sub_string(string,count,sub_string)
	if(istat.eq.0) read(sub_string,*) cmplx
	parse_cmplx=istat
c
	return
	end
	function parse_char(string,count,sub_string)
c
	implicit integer (a-z)
	character string*(*),sub_string*(*)
c
	parse_char=0
	istat=get_sub_string(string,count,sub_string)
	parse_char=istat
c
	return
	end
	subroutine parse_time(char_tim,rtime)
c
c       Parse times in form yyyy/ddd/hh:mm:ss
	implicit integer(a-z)
	character char_tim*(*)
	integer idat(5)
	real*8 rtime
c
	idat(1) = 1970
	idat(2) = 1
        do i=3,5
	   idat(i) = 0
        enddo
        ixb = 1
        do i=1,5
	   idx=index(char_tim(ixb:),'/')
	   if (idx .eq. 0) idx=index(char_tim(ixb:),':')
	   if (idx .eq. 0) idx=lenc(char_tim)-ixb+2
	   ixs = ixb + idx-2
	   if (ixs .lt. ixb) go to 10
	   read(char_tim(ixb:ixs),*,iostat=ios) idat(i)
	   if (ios .ne. 0) then
	      write(0,*) '**Bad date in string: ',
     &               char_tim(1:lenc(char_tim)),', ignoring.'
              idat(i) = 0
	      go to 10
	   endif
	   ixb = ixs + 2
	enddo
10      continue
	call abstim(1,istim,idat(1),idat(2),idat(3),idat(4),idat(5))
	rtime=dble(istim)
c
	return
	end
	function parse_string(string,no_elem,elem)
c
c   Extraction of all substring elements in an input string (separator: ,)
c
	implicit integer(a-z)
	character string*(*),elem(*)*(*)
c
	parse_string=0
	iend=0
	no_elem=0
	lens=lenc(string)
c
	ia=0
	do while (iend.eq.0)
	  ind=index(string(ia+1:lens),',')
	  idx=ia+ind
	  if(ind.gt.0) then
	    ie=idx-1
	  else
	    iend=1
	    ie=lenc(string)
	  endif
c
	  no_elem=no_elem+1
	  ia=ia+1
	  istat=str_upcase(elem(no_elem),string(ia:ie))
	  ia=idx
	enddo
c	write(*,*) 'parse',no_elem,(elem(i),i=1,no_elem)
c
	return
	end
	function dec_fixhead(num,stat,chan,net_id,rate,begdat,istim,
     &    ttsec,timcor,nsamp,form_key,reclen,ibuf)
c
c   Decodes fixed section of SEED data header
c
	implicit integer(a-z)
	integer fixhead(16),ibuf(*)
	integer*2 iblk(12)
 	logical swp
	real*4 rate
	character stat*5,chan*3,net_id*2,buffer*56
	equivalence (fixhead(13),iblk)
	equivalence (buffer,fixhead)
c
c	structure /s_fixhead/
	  character*6 fixhead_seqnum
	  character*1 fixhead_indd
	  character*1 fixhead_resb1
	  character*5 fixhead_stat
	  character*2 fixhead_loca
	  character*3 fixhead_chan
	  character*2 fixhead_net_id
	  integer*2   fixhead_year
	  integer*2   fixhead_doy
	  byte   fixhead_hour
	  byte   fixhead_min
          byte   fixhead_sec
	  byte   fixhead_resb3
          integer*2   fixhead_ttsec
	  integer*2   fixhead_nsamp
	  integer*2   fixhead_fsamp
	  integer*2   fixhead_msamp
	  byte   fixhead_actfl
	  byte   fixhead_iofl
	  byte   fixhead_qualfl
	  byte   fixhead_numblk
	  integer*4   fixhead_timcor
	  integer*2   fixhead_begdat
	  integer*2   fixhead_begblk
	  integer*2   fixhead_blocktype1
	  integer*2   fixhead_nextblock1
	  byte   fixhead_encform
	  byte   fixhead_wordord
	  byte   fixhead_reclen
	  byte   fixhead_resb4
c	endstructure
	equivalence
     &	  (buffer(1:6),fixhead_seqnum),
     &	  (buffer(7:7),fixhead_indd),
     &	  (buffer(8:8),fixhead_resb1),
     &	  (buffer(9:13),fixhead_stat),
     &	  (buffer(14:15),fixhead_loca),
     &	  (buffer(16:18),fixhead_chan),
     &	  (buffer(19:20),fixhead_net_id),
     &	  (buffer(21:22),fixhead_year),
     &	  (buffer(23:24),fixhead_doy),
     &	  (buffer(25:25),fixhead_hour),
     &	  (buffer(26:26),fixhead_min),
     &	  (buffer(27:27),fixhead_sec),
     &	  (buffer(28:28),fixhead_resb3),
     &	  (buffer(29:30),fixhead_ttsec)
	equivalence
     &	  (buffer(31:32),fixhead_nsamp),
     &	  (buffer(33:34),fixhead_fsamp),
     &	  (buffer(35:36),fixhead_msamp),
     &	  (buffer(37:37),fixhead_actfl),
     &	  (buffer(38:38),fixhead_iofl),
     &	  (buffer(39:39),fixhead_qualfl),
     &	  (buffer(40:40),fixhead_numblk),
     &	  (buffer(41:44),fixhead_timcor),
     &	  (buffer(45:46),fixhead_begdat),
     &	  (buffer(47:48),fixhead_begblk),
     &	  (buffer(49:50),fixhead_blocktype1),
     &	  (buffer(51:52),fixhead_nextblock1),
     &	  (buffer(53:53),fixhead_encform),
     &	  (buffer(54:54),fixhead_wordord),
     &	  (buffer(55:55),fixhead_reclen),
     &	  (buffer(56:56),fixhead_resb4)
c	record /s_fixhead/ fixhead
c	equivalence (buffer,fixhead_seqnum)
c	data DEC/.true./
c
	do i=1,14
	 fixhead(i)=ibuf(i)
	enddo
c
	dec_fixhead=0
c
	usyear=fixhead_year
	usdoy=fixhead_doy
	swyear=iswap(fixhead_year)
	swdoy=iswap(fixhead_doy)
	swp=1900.le.swyear .and. 2500.ge.swyear .and.
     &      1.le.swdoy .and. 366.ge.swdoy
c
c	write(*,*) buffer(1:12)
c	write(*,*) fixhead_indd,fixhead_stat,fixhead_chan
	if(0.eq.index('DRMQB',fixhead_indd)) then
	  if(fixhead_indd.eq.'O') then
c	    write(*,*) 'Trailer record found'
	    dec_fixhead=2
	  else
c	    write(*,*) 'DEC_FIXHEAD error: No data record, identifier:',
c     &	    fixhead_indd
	    dec_fixhead=1
	  endif
	  return
	endif
	read(fixhead_seqnum,'(i6.6)') num
	stat=fixhead_stat
	if(stat.eq.'BRL') stat='BRNL'
	chan=fixhead_chan
	net_id=fixhead_net_id
        if (swp) then
	  iyear=swyear
	  iday=swdoy
        else
	  iyear=usyear
	  iday=usdoy
        endif
	ih=fixhead_hour
	im=fixhead_min
	is=fixhead_sec
c	write(*,*) num,stat,chan,iyear,iday,ih,im,is
	call abstim(1,istim,iyear,iday,ih,im,is)
	if(istim.lt.0) then
	  write(*,*) 'DEC_FIXHEAD error: time error'
	  dec_fixhead=1
	  return
	endif
	ttsec=i2val(swp,fixhead_ttsec)
	nsamp=i2val(swp,fixhead_nsamp)
	if(nsamp.eq.0.and.chan.ne.'ACE'.and.chan.ne.'LOG') then
	  write(*,*) 'DEC_FIXHEAD error: no of samples = 0',chan
c	  dec_fixhead=1
	endif
	fsamp=i2val(swp,fixhead_fsamp)
	msamp=i2val(swp,fixhead_msamp)
	if(fsamp.gt.0) then
	  if(msamp.gt.0) then
	    rate=float(msamp)*float(fsamp)
	  else
	    rate=-float(msamp)/float(fsamp)
	  endif
	elseif(fsamp.lt.0) then
	  if(msamp.gt.0) then
	    rate=float(msamp)/float(-fsamp)
	  else
	    rate=float(msamp)/float(fsamp)
	  endif
	elseif(fsamp.eq.0) then
	  rate=0.0
	  if(chan.ne.'ACE'.and.chan.ne.'LOG') then
	    write(*,*) 'DEC_FIXHEAD error: Sample rate not defined: ',
     &        chan
	  endif
	endif
c       Zero time correction if it already has been applied.
	timcor = i4val(swp,fixhead_timcor)
	if (1.eq.mod(fixhead_actfl/2,2)) timcor=0
	begdat=i2val(swp,fixhead_begdat)
	io = 1 + (i2val(swp,fixhead_begblk)-48)/2
	do i = 1,fixhead_numblk
	  if (i2val(swp,iblk(io)).ne.1000) then
	    io = 1 + (i2val(swp,iblk(1+io))-48)/2
	    cycle
	  endif
	  nextblock1=i2val(swp,iblk(1+io))
	  ix = 49 + (io-1)*2 + 4
	  blocktype1=i2val(swp,iblk(io))
	  encform=ichar(buffer(ix:ix))
	  wordord=ichar(buffer(ix+1:ix+1))
	  reclen=ichar(buffer(ix+2:ix+2))
	  form_key=encform+256*wordord
        enddo
c	form_key=encform
c	write(*,*) 'dec',num,istim,nsamp,fixhead_numblk,begdat,encform,wordord,
c     &	reclen
c
	return
	end
	function dec_fixhead_ful(num,stat,chan,net_id,rate,begdat,
     &  istim,ttsec,timcor,nsamp,form_key,wordord,reclen,timq,mysec,
     &  nfrm,ibuf)
c
c   Decodes fixed section of SEED data header
c
	implicit integer(a-z)
	integer fixhead(16),ibuf(*)
	integer*2 iblk(12)
 	logical swp
	real*4 rate
	character stat*5,chan*3,net_id*2,buffer*64
	equivalence (fixhead(13),iblk)
	equivalence (buffer,fixhead)
c
c	structure /s_fixhead/
	  character*6 fixhead_seqnum
	  character*1 fixhead_indd
	  character*1 fixhead_resb1
	  character*5 fixhead_stat
	  character*2 fixhead_loca
	  character*3 fixhead_chan
	  character*2 fixhead_net_id
	  integer*2   fixhead_year
	  integer*2   fixhead_doy
	  byte   fixhead_hour
	  byte   fixhead_min
          byte   fixhead_sec
	  byte   fixhead_resb3
          integer*2   fixhead_ttsec
	  integer*2   fixhead_nsamp
	  integer*2   fixhead_fsamp
	  integer*2   fixhead_msamp
	  byte   fixhead_actfl
	  byte   fixhead_iofl
	  byte   fixhead_qualfl
	  byte   fixhead_numblk
	  integer*4   fixhead_timcor
	  integer*2   fixhead_begdat
	  integer*2   fixhead_begblk
	  integer*2   fixhead_blocktype1
	  integer*2   fixhead_nextblock1
	  byte   fixhead_encform
	  byte   fixhead_wordord
	  byte   fixhead_reclen
	  byte   fixhead_resb4
	  integer*2   fixhead_blocktype2
	  integer*2   fixhead_nextblock2
	  byte   fixhead_timq
	  byte   fixhead_mysec
	  byte   fixhead_resb5
	  byte   fixhead_nfrm
c	endstructure
	equivalence
     &	  (buffer(1:6),fixhead_seqnum),
     &	  (buffer(7:7),fixhead_indd),
     &	  (buffer(8:8),fixhead_resb1),
     &	  (buffer(9:13),fixhead_stat),
     &	  (buffer(14:15),fixhead_loca),
     &	  (buffer(16:18),fixhead_chan),
     &	  (buffer(19:20),fixhead_net_id),
     &	  (buffer(21:22),fixhead_year),
     &	  (buffer(23:24),fixhead_doy),
     &	  (buffer(25:25),fixhead_hour),
     &	  (buffer(26:26),fixhead_min),
     &	  (buffer(27:27),fixhead_sec),
     &	  (buffer(28:28),fixhead_resb3),
     &	  (buffer(29:30),fixhead_ttsec)
	equivalence
     &	  (buffer(31:32),fixhead_nsamp),
     &	  (buffer(33:34),fixhead_fsamp),
     &	  (buffer(35:36),fixhead_msamp),
     &	  (buffer(37:37),fixhead_actfl),
     &	  (buffer(38:38),fixhead_iofl),
     &	  (buffer(39:39),fixhead_qualfl),
     &	  (buffer(40:40),fixhead_numblk),
     &	  (buffer(41:44),fixhead_timcor),
     &	  (buffer(45:46),fixhead_begdat),
     &	  (buffer(47:48),fixhead_begblk),
     &	  (buffer(49:50),fixhead_blocktype1),
     &	  (buffer(51:52),fixhead_nextblock1),
     &	  (buffer(53:53),fixhead_encform),
     &	  (buffer(54:54),fixhead_wordord),
     &	  (buffer(55:55),fixhead_reclen),
     &	  (buffer(56:56),fixhead_resb4),
     &	  (buffer(57:58),fixhead_blocktype2),
     &	  (buffer(59:60),fixhead_nextblock2)
 	equivalence
     &	  (buffer(61:61),fixhead_timq),
     &	  (buffer(62:62),fixhead_mysec),
     &	  (buffer(63:63),fixhead_resb5),
     &	  (buffer(64:64),fixhead_nfrm)
c	record /s_fixhead/ fixhead
c	equivalence (buffer,fixhead_seqnum)
c	data DEC/.true./
c
	do i=1,16
	 fixhead(i)=ibuf(i)
	enddo
c
	dec_fixhead_ful=0
c
c	write(*,*) buffer(1:12)
c	write(*,*) fixhead_indd,fixhead_stat,fixhead_chan
	if(0.eq.index('DRMQB',fixhead_indd)) then
	  if(fixhead_indd.eq.'O') then
c	    write(*,*) 'Trailer record found'
	    dec_fixhead_ful=2
	  else
c	    write(*,*) 'DEC_FIXHEAD error: No data record, identifier:',
c     &	    fixhead_indd
	    dec_fixhead_ful=1
	  endif
	  return
	endif
	read(fixhead_seqnum,'(i6.6)') num
	stat=fixhead_stat
	if(stat.eq.'BRL') stat='BRNL'
	chan=fixhead_chan
	net_id=fixhead_net_id
	usyear=fixhead_year
	usdoy=fixhead_doy
	swyear=iswap(fixhead_year)
	swdoy=iswap(fixhead_doy)
	swp=1900.le.swyear .and. 2500.ge.swyear .and.
     &      1.le.swdoy .and. 366.gt.swdoy
        if (swp) then
	  iyear=swyear
	  iday=swdoy
        else
	  iyear=usyear
	  iday=usdoy
        endif
	ih=fixhead_hour
	im=fixhead_min
	is=fixhead_sec
c	write(*,*) num,stat,chan,iyear,iday,ih,im,is
	call abstim(1,istim,iyear,iday,ih,im,is)
	if(istim.lt.0) then
	  write(*,*) 'DEC_FIXHEAD error: time error'
	  dec_fixhead_ful=1
	  return
	endif
	ttsec=i2val(swp,fixhead_ttsec)
	nsamp=i2val(swp,fixhead_nsamp)
	if(nsamp.eq.0.and.chan.ne.'ACE'.and.chan.ne.'LOG') then
	  write(*,*) 'DEC_FIXHEAD error: no of samples = 0',chan
c	  dec_fixhead=1
	endif
	fsamp=i2val(swp,fixhead_fsamp)
	msamp=i2val(swp,fixhead_msamp)
	if(fsamp.gt.0) then
	  if(msamp.gt.0) then
	    rate=float(msamp)*float(fsamp)
	  else
	    rate=-float(msamp)/float(fsamp)
	  endif
	elseif(fsamp.lt.0) then
	  if(msamp.gt.0) then
	    rate=float(msamp)/float(-fsamp)
	  else
	    rate=float(msamp)/float(fsamp)
	  endif
	elseif(fsamp.eq.0) then
	  rate=0.0
	  if(chan.ne.'ACE'.and.chan.ne.'LOG') then
	    write(*,*) 'DEC_FIXHEAD error: Sample rate not defined: ',
     &        chan
	  endif
	endif
c       Zero time correction if it already has been applied.
	timcor = i4val(swp,fixhead_timcor)
	if (1.eq.mod(fixhead_actfl/2,2)) timcor=0
	begdat=i2val(swp,fixhead_begdat)
	io = 1 + (i2val(swp,fixhead_begblk)-48)/2
	do i = 1,fixhead_numblk
	  if (i2val(swp,iblk(io)).ne.1000) then
	    blocktype2=i2val(swp,iblk(io))
	    nextblock2=i2val(swp,iblk(1+io))
	    io = 1 + (i2val(swp,iblk(1+io))-48)/2
	    cycle
	  endif
	  nextblock1=i2val(swp,iblk(1+io))
	  ix = 49 + (io-1)*2 + 4
	  blocktype1=i2val(swp,iblk(io))
	  encform=ichar(buffer(ix:ix))
	  wordord=ichar(buffer(ix+1:ix+1))
	  reclen=ichar(buffer(ix+2:ix+2))
	  form_key=encform+256*wordord
        enddo
c
c	form_key=encform
c
	timq=fixhead_timq
	mysec=fixhead_mysec
	nfrm=fixhead_nfrm
c
c	write(*,*) 'dec',num,istim,nsamp,fixhead_numblk,begdat,encform,wordord,
c     &	reclen,timq,mysec,nfrm
c
	return
	end
	subroutine tfix(time,itime,ttsec)
c
c  Converts double real UNIX time value in 2 longwords
c
	implicit integer (a-z)
	real*8 time,stim
c
	stim=time+0.00009
	itime=stim
	ttsec=(stim-dble(itime))*10000.
c
	return
	end
	function rnd_tim(ind,time,dt,tcor)
c
c   Roundoff of given time to next full sampling time and time correction
c
	implicit integer (a-z)
	real*8 time,rnd_tim
	real*4 dt
c
	call tfix(time,itime,ttsec)
	idt=dt*10000.0001
	if(idt.gt.0) then
	  if(ind.eq.0) then
	    nsamp=float(ttsec)/float(idt)+0.5
	  else
	    nsamp=ttsec/idt
	  endif
	else
	  nsamp=0
	endif
	tsc=nsamp*idt
	rnd_tim=dble(itime)+dble(tsc)*0.0001
	tcor=ttsec-tsc
c	
	return
	end
	subroutine datum(mode,lftag,iyear,monat,itag)
c
c   DATUM converts day of year into month and day and viceversa
c
c     Mode = 0 conversion lftag into itag, monat
c     Mode = 1 conversion itag, monat into lftag
c
	dimension mo(12)
	data mo/31,28,31,30,31,30,31,31,30,31,30,31/
c
	msum=0
	mo(2)=28
	if(mod(iyear,4).eq.0) mo(2)=29
	if(mode.eq.0) then
	  do k=1,12
	    msum=msum+mo(k)
	    if(msum.ge.lftag) goto 5
	  enddo
5	  monat=k
	  msum=msum-mo(k)
	  itag=lftag-msum
	else
	  do k=1,monat-1
	    msum=msum+mo(k)
	  enddo
	  lftag=msum+itag
	endif
c
	return
	end
	subroutine abstim(ind,itime,iyear,iday,ih,im,is)
c
c   ABSTIM converts dd/mm/yyyy hh:mm:ss times into absolute sec since
c   1.1.1970 and viceversa
c
c   ind  - (ip)
c          0 :  year, day of year, sec of day into abs. time
c          1 :  abs. time into year, day of year, sec of day
c   ichk - error status (iop)
c          0 - normal
c          1 - input data out of range
c
	double precision dtime
c
c   0 :  abs. time into year, day of year, sec of day
c
	if(ind.eq.0) then
	  iyear=itime/31536000+1970
10	  isecyr=(iyear-1970)*31536000+((iyear-1969)/4)*86400
	  if(isecyr.gt.itime) then
	    iyear=iyear-1
	    goto 10
	  endif
	  iday=(itime-isecyr)/86400+1
	  isecdy=(iday-1)*86400
	  ih=(itime-isecyr-isecdy)/3600
	  im=(itime-isecyr-isecdy-ih*3600)/60
	  is=itime-isecyr-isecdy-ih*3600-im*60
        else
c
c   1 :  year, day of year, sec of day into abs. time
c
   	  itsec=ih*3600+im*60+is
	  if(iyear.lt.1970.or.iyear.gt.2500.or.iday.lt.1.or.iday.gt.366
     &	    .or.itsec.lt.0.or.itsec.gt.86400) then
	    write(*,*) 'ABSTIM: wrong input time',itsec,iday,iyear
	    nerr=nerr+1
	    itime=-1
	    return
	  endif
	  dtime=(iyear-1970)*31536000d0+((iyear-1969)/4)*86400d0+
     &          (iday-1)*86400d0+dble(itsec)
          itime=min(dtime,2*dble(2**30-1))
        endif
	end

	function cyear(iyear)
	implicit integer (a-z)
	cyear=mod(iyear,100)
	end

	function iswap(int)
c
	integer*2 int,jint
	byte jbyt(2),sbyte
	equivalence (jint,jbyt)
c
	jint=int
	sbyte=jbyt(1)
	jbyt(1)=jbyt(2)
	jbyt(2)=sbyte
	iswap=jint
c
	return
	end
	subroutine fiswap(nx,ix,iy)
c
	integer*2 ix(*),iy(*)
c
	do n=1,nx
	  iy(n)=iswap(ix(n))
	enddo
c
	return
	end
	subroutine i4swap(nbuf,buf1,buf2)
c
	implicit integer (a-z)
	integer buf1(*),buf2(*)
	byte bbuf1(4),bbuf2(4)
	equivalence (bbuf1(1),long1),(bbuf2(1),long2)
c
	do n=1,nbuf
	  long1=buf1(n)
	  do m=1,4
	    bbuf2(5-m)=bbuf1(m)
	  enddo
	  buf2(n)=long2
	enddo
c
	return
	end
	function make_4096(mode,lenb,nb_rec,nb_tot,ibuf,num,stat,chan,
     &  net_id,rate,begdat,iftim,iftsc,timcor,nsamp,timq,
     &  mysec,nfrm)
c
	implicit integer (a-z)
	integer j4buf(1024)
	integer*2 j2buf(2048)
	byte ibuf(*),jbuf(4096)
	character stat*(*),chan*(*),net_id*(*),stat_st*5,chan_st*3
	real*4 rate,rate_st
	real*8 new_tim,next_tim
	equivalence (jbuf,j2buf,j4buf)
        save stat_st, chan_st, nsmp_tot, jbuf
	data nrec/0/,nn/0/,num_st/0/,next_tim/0.0/,rate_st/0.0/
	data iftim_st/0/,iftsc_st/0/,timcor_st/0/,timq_st/0/
	data mysec_st/0/,nfrm_st/0/
c
	make_4096=0
c
	new_tim=dble(iftim)+dble(iftsc)*0.0001
c	write(*,*) new_tim,next_tim,rate,nrec,chan,timq,mysec,nfrm
	if(nrec.gt.0 .and. (
     &     abs(next_tim-new_tim).gt.2./rate .or.
     &	   stat.ne.stat_st .or. chan.ne.chan_st .or. rate.ne.rate_st )
     &     .or. mode.eq.999) then
c         Flush record
 	  istat=put_int2(nsmp_tot,16,j2buf)
	  istat=put_int4(last_value,begdat/4+3,j4buf)
	  jbuf(55)=nint(log(float(lenb))/log(2.))
	  do n=1,nn
	    ibuf(n)=jbuf(n)
	  enddo
	  if(nn.lt.lenb) then
	    do n=nn+1,lenb
	      ibuf(n)=0
	    enddo
	  endif
	  nrec=0
	  num=num_st
	  stat=stat_st
	  chan=chan_st
	  iftim=iftim_st
	  iftsc=iftsc_st
	  timcor=timcor_st
	  rate=rate_st
	  nsamp=nsmp_tot
	  nb_tot=0
	  timq=timq_st
	  mysec=mysec_st
	  nfrm=nfrm_st
	  make_4096=1
	  return
	endif

c       Update time for continuity checks
	if(rate.gt.0.) then
	  next_tim=new_tim+float(nsamp)/rate
	else
	  next_tim=new_tim
	endif
	nrec=nrec+1

	if(nrec.eq.1) then
c         Start blocking
	  num_st=num
	  stat_st=stat
	  chan_st=chan
	  iftim_st=iftim
	  iftsc_st=iftsc
	  timcor_st=timcor
	  rate_st=rate
	  timq_st=timq
	  mysec_st=mysec
	  nsmp_tot=0
	  nb_tot=begdat
	  nfrm_st=0
	  do i=1,begdat
	    jbuf(i)=ibuf(i)
	  enddo
	  nn=begdat
	endif

c       Save integration constant in this block for use in aggregated block.
	last_value=get_int4(begdat/4+3,ibuf)

c       Calculate number of frames of data in block.  Copy data from input
c          into block; accumulate total sample count in block and bytes of
c          data in block.
	nfrm=(nb_rec*4-begdat)/64
	if (nfrm*64+nn .gt. lenb) nfrm = (lenb-nn)/64
	do n=1,nfrm*64
	  nn=nn+1
	  jbuf(nn)=ibuf(begdat+n)
	enddo
	nsfrm = nssamp(ibuf(1+begdat))
	do n=1,nfrm-1
	   nsfrm = nsfrm + nssamp(ibuf(1+begdat+n*64))
	enddo
	nsmp_tot=nsmp_tot+nsfrm
	nb_tot=nb_tot+nfrm*64
	nfrm_st=nfrm_st+nfrm
 
	if(nn.ge.lenb) then
c         Output block if filled
	  istat=put_int2(nsmp_tot,16,j2buf)
	  ind=begdat/4+3
	  istat=put_int4(last_value,ind,j4buf)
	  jbuf(55)=nint(log(float(lenb))/log(2.))
	  do n=1,nn
	    ibuf(n)=jbuf(n)
	  enddo
	  if(nn.lt.lenb) then
	    do n=nn+1,lenb
	      ibuf(n)=0
	    enddo
	  endif
	  nrec=0
	  num=num_st
	  stat=stat_st
	  chan=chan_st
	  iftim=iftim_st
	  iftsc=iftsc_st
	  timcor=timcor_st
	  rate=rate_st
	  nsamp=nsmp_tot
	  timq=timq_st
	  mysec=mysec_st
	  nfrm=nfrm_st
	endif
	end

	integer function get_int2(n,ibuf)
c
	integer*2 ibuf(*)
c
	get_int2=ibuf(n)
c
	return
	end
	integer function put_int2(int2,n,ibuf)
c
	integer*2 ibuf(*)
c
	ibuf(n)=int2
c
	put_int2=int2
	end
	integer function get_int4(n,ibuf)
c
	integer ibuf(*)
c
	get_int4=ibuf(n)
c
	return
	end
	integer function put_int4(int4,n,ibuf)
c
	integer ibuf(*)
c
	ibuf(n)=int4
c
	put_int4=int4
	end
cif defined (LINUX) || defined (HP)
	subroutine ieee_flags(cdum,dummy1,dummy2,dummy3)
c
	implicit integer(a-z)
	character cdum*(*),dummy1*(*),dummy2*(*),dummy3*(*)
	return
	end
cendif
	function open_next(mode,unit,idir,anf_tim,end_tim,chan_typ,
     &    ifile,nlgfl)
c
c   Looks and opens next data file in a SEED file ensemble
c
	implicit integer(a-z)
	character idir*(*),chan_typ*(*),ifile*(*)
	real*8 anf_tim,end_tim
c
c	write(*,*) 'open_next',mode,anf_tim,end_tim,chan_typ
	if(mode.eq.0) then
c	  idsc=0
c	  istat=open_next_dsc(mode,unit,idir,anf_tim,end_tim,ifile,nlgfl)
c	  if(istat.eq.0) then
c	    idsc=1
c	  else
	    istat=open_next_nodsc(mode,unit,idir,anf_tim,end_tim,
     &        chan_typ,ifile,nlgfl)
	    if(istat.eq.0) idsc=2
c	  endif
	else
c	  if(idsc.eq.1) then
c	    istat=open_next_dsc(mode,unit,idir,anf_tim,end_tim,ifile,nlgfl)
c	  elseif(idsc.eq.2) then
	    istat=open_next_nodsc(mode,unit,idir,anf_tim,end_tim,chan_typ,
     &	    ifile,nlgfl)
c	  else
c	    write(*,*) 'OPEN_NEXT error',idsc
c	    istat=2
c	  endif
	endif
c
	open_next=istat
	return
	end
	function open_next_nodsc(mode,unit,idir,anf_tim,end_tim,
     &    chan_typ,ifile,nlgfl)
c
c   Looks for next data file in the given input directory idir depending
c   on given time window for all stations included in the head_seed.inc file
c
	implicit integer(a-z)
	logical lexi
	character idir*(*),chan_typ*(*),ifile*(*),sr_stat*5,sr_chan*3 
        character bbuf(512),nfile*132,last_file*132,cyr*2
	real*4 rate
	real*8 anf_tim,end_tim,num_tima,num_time,sta_stim,sta_etim
	integer*2 blk1000
	equivalence (bbuf(49),blk1000)
c
 	include 'head_seed.inc'
	save no_chan,ne_chan,no_stat,ni_fil,last_file,sr_stat,sr_chan
c
	open_next_nodsc=0
c
	call tfix(anf_tim,iatim,iatsc)
	call abstim(0,iatim,iyra,idya,iha,ima,isa)
	call datum(0,idya,iyra,mona,itga)
c	write(*,*) 'open_next_nodsc',mode,anf_tim,itga,mona,iha,ima,isa,end_tim,chan_typ
	if(mode.eq.0) then
	  if(chan_typ.eq.'H') then
	    rate=80.
	  elseif(chan_typ.eq.'E') then
	    rate=80.
	  elseif(chan_typ.eq.'S') then
	    rate=40.
	  elseif(chan_typ.eq.'B') then
	    rate=20.
	  elseif(chan_typ.eq.'L') then
	    rate=1.
	  elseif(chan_typ.eq.'V') then
	    rate=0.1
	  elseif(chan_typ.eq.'U') then
	    rate=0.01
	  else
	    write(*,*) 'Channel type error: ',chan_typ
	    rate=0.01
	  endif
	  no_chan=0
	  ne_chan=0
	  no_stat=0
	endif
	inquire(unit,opened=lexi)
	if(lexi) close(unit)
c
	istfl=1
	iend=0
	do while (istfl.ne.0.and.iend.eq.0)
c	write(*,*) 'do',istfl,iend,mode,no_stat,stat_net(no_stat),no_chan,ns_chan,ne_chan
c
	  if(mode.ne.1) then
	    no_chan=no_chan+1
	    if(no_chan.gt.ne_chan) then
	      sr_stat=' '
	      do while (sr_stat(1:1).eq.' '.and.iend.eq.0)
	        lexi=.true.
		do while(lexi)
c                 Check if station operating in desired time range
	          no_stat=no_stat+1
		  if(no_stat.le.nst_tot) then
	            call parse_time(stat_st_tim(no_stat),sta_stim)
	            call parse_time(stat_end_tim(no_stat),sta_etim)
		    lexi = sta_etim.le.anf_tim .or.
     &                     sta_stim.ge.end_tim
                  else
		    lexi = .false.
		  endif
		  lexi = lexi .and. no_stat.le.nst_tot
	        enddo
	        if(no_stat.le.nst_tot) then
	          sr_stat=stat_net(no_stat)
c                 Doubtful whether this is correct - removed by GRH
c	          do while(stat_end_tim(no_stat).ne.'0/0')
c	            no_stat=no_stat+1
c	            sr_stat=stat_net(no_stat)
c	          enddo
	          do no_chan=1,nch_tot(no_stat)
	            if(chan_net(no_chan,no_stat)(1:1).eq.chan_typ) then
	              ns_chan=no_chan
	              ne_chan=no_chan+2
		      exit
	            endif
	          enddo
c		 enddo
	        else
	          iend=1
	          open_next_nodsc=2
	        endif
		if(mode.eq.2) mode=0
c	write(*,*) 'no_stat',no_stat,no_chan,stat_net(no_stat),chan_net(no_chan,
c     &	no_stat),iend
	      enddo
	    endif
	    if(iend.eq.0) then
	      ni_fil=0
	      sr_stat=stat_net(no_stat)
	      sr_chan=chan_net(no_chan,no_stat)
	      istfl=0
	    endif
	  else
	    ni_fil=ni_fil+1
	  endif
c	write(*,*) 'anf',no_stat,no_chan,sr_stat,sr_chan,iend
c
	  if(iend.eq.0) then
c	    if(iha.eq.0.and.ima.eq.0.and.isa.eq.0.and.iatsc.eq.0) then
	    ifile=' '
	    if(iha.eq.0.and.ima.eq.0.and.isa.eq.0.and.iabs(iatsc)
     &	    .lt.ifix(5000./rate)) then
	      write(ifile,'(2a,3i2.2,1h.,a)') idir(1:lenc(idir)),
     &        sr_stat(1:lenc(sr_stat)),cyear(iyra),mona,itga,sr_chan
	      inquire(file=ifile,recl=nbtfl,exist=lexi)
	    else
	      lexi=.false.
	    endif
c	write(*,*) 'lexi1',lexi,istfl,iha,ima,isa,iatsc,ifile
	    if(.not.lexi) then
	      write(ifile,'(2a,6i2.2,1h.,a)') idir(1:lenc(idir)),
     &	      sr_stat(1:lenc(sr_stat)),cyear(iyra),mona,itga,iha,ima,
     &	      isa,sr_chan
	      inquire(file=ifile,recl=nbtfl,exist=lexi)
c	write(*,*) 'lexi1a',lexi,istfl,ifile
	      if(.not.lexi) then
c	        num_tima=anf_tim-1.
	        num_tima=anf_tim
	        call tfix(num_tima,idtim,idtsc)
	        call abstim(0,idtim,iyrd,idyd,ihd,imd,isd)
	        call datum(0,idyd,iyrd,mond,itgd)
	        write(nfile,'(6i2.2)') cyear(iyrd),mond,itgd,ihd,imd,isd
 	        read(nfile(1:12),'(f12.0)') num_tima
		if(cyear(iyrd).lt.50) then
		   num_tima=num_tima+20D12
	        else
		   num_tima=num_tima+19D12
		endif
	        call tfix(end_tim,idtim,idtsc)
	        call abstim(0,idtim,iyrd,idyd,ihd,imd,isd)
	        call datum(0,idyd,iyrd,mond,itgd)
	        write(nfile,'(6i2.2)') cyear(iyrd),mond,itgd,ihd,imd,isd
	        read(nfile(1:12),'(f12.0)') num_time
		if(cyear(iyrd).lt.50) then
		   num_time=num_time+20D12
	        else
		   num_time=num_time+19D12
		endif
c		write(*,*) num_tima,num_time
c	        indxa=index(ifile,'/'//sr_stat(1:lenc(sr_stat)))+lenc
c     &	        (sr_stat)+1
c	        indxe=index(ifile,'.')-1
c	write(*,*) indxa,indxe,ifile(indxa:indxe)
c	        if(indxe.gt.indxa) then
c	          read(ifile(indxa:indxe),'(f12.0)') num_tima
c	          if(indxe-indxa.eq.11) then
c	          elseif(indxe-indxa.eq.5) then
c	            num_tima=num_tima*1.e6
c	          else
c	            write(*,*) 'next_open: name error',ifile(indxa:indxe)
c	          endif
c	        endif
	        if(ni_fil.eq.0) then
	          nfile=ifile
	        else
	          nfile=last_file
	        endif
c	write(*,*) 'ni_fil',ni_fil
c	    write(*,*) 'nfile',indxa,indxe,num_tima,ifile(indxa:indxe),nfile
	        modnx=mode
c	        if(mode.eq.2) modnx=0
c	write(*,*) 'call nextsf',modnx,idir,sr_stat,sr_chan,num_tima,num_time,nfile
	        istfl=next_sfile(modnx,idir,sr_stat,sr_chan,nfile,
     &            num_tima,num_time)
c	write(*,*) 'next_sfile: ',ifile,nfile,istfl
	        ifile=nfile
	        if(istfl.le.0) then
	          istfl=0
	          inquire(file=ifile,recl=nbtfl,exist=lexi)
c	write(*,*) 'lexi2',lexi,istfl,ifile
	        else
	          if(mode.eq.1) then
	            iend=1
	            if(no_stat.ge.nst_tot .and.
     &                 no_chan.ge.nch_tot(no_stat)) then
	              open_next_nodsc=2
	            else
	              open_next_nodsc=1
	            endif
	          endif
	        endif
	      else
	        istfl=0
c	write(*,*) 'set istfl=0'
	      endif
	    else
	      istfl=0
c	write(*,*) 'set istfl=0'
	    endif
c	write(*,*) 'endif',istfl,iend
	  endif
	  mode=2
	enddo
c
	if(istfl.eq.0.and.iend.eq.0) then
c-----	  nlgfl=nbtfl/4 -------------------------------------------------------
c	  nlgfl = 1024
c         nlgfl = 4096
c         Read first block of new file to determine block size.  Check there
c         is a type 1000 record available.  If so, extract blocking factor.
c         If not, assume 1024 block size.
	  open(unit,file=ifile,status='old',recl=512,access='direct',
     &    form='unformatted')
	  read(unit,rec=1) bbuf
	  if (blk1000 .eq. 1000 .or. iswap(blk1000) .eq. 1000) then
	     nlgfl=2**ichar(bbuf(55))
	     if (nlgfl .ne. 512) then
		close(unit)
	        open(unit,file=ifile,status='old',recl=nlgfl,
     &               access='direct',form='unformatted')
	     endif
	  else
	     ix = index(ifile,' ')
	     read(bbuf,'(i6)',iostat=ios) nlgfl
	     if (ios.ne.0 .or. bbuf(7) .ne. 'D') then
	        write(0,*) '**Not mseed data: ',ifile(1:ix-1)
	        open_next_nodsc=1
		mode=2
		close(unit)
	     else
		write(0,*) '**No type 1000 bkette in first block of'
		write(0,'(1x,a,a)') ifile(1:ix),'so using 4096'
	     endif
	     nlgfl=4096
	  endif
	  nlgfl=nlgfl/4
	  nfile=ifile
	  last_file=ifile
	  write(*,*) 'Opened next file: ',ifile(1:lenc(ifile))
	endif
c
	return
	end
	function check_infile(file_nam,no_stat,stat,no_chan,chan,ist,
     &    ich)
c
c   Checks if filename file_nam contains data of a valid station and channel
c   (string check only!)
c
	implicit integer(a-z)
	character file_nam*(*),stat(*)*(*),chan(*)*(*)
	character stat_fl*5,chan_fl*3
c
C-------------------- now unix dir format
	lf=lenc(file_nam)
c	write(*,*) 'check_infile:',file_nam(1:lf),no_stat,no_chan
	is = 0
	i = 1
	DO WHILE (i .LE. lf)
		IF( file_nam(lf-i:lf-i) .eq. '/' )THEN
			is = lf - i + 1
			i = lf+1
			
		ENDIF
		i = i + 1 
	END DO
C----------------------------------------
	if(is.gt.1) then
	  lnam= lf - is + 1
	  ntim=0
	  if(lnam.gt.12.and.lnam.lt.16) ntim=6
	  if(lnam.gt.18.and.lnam.lt.22) ntim=12
	  if(lnam.gt.0) then
	    ia=is
	    nc_stat=lf-is-ntim-3
	    ie=is+nc_stat-1
	    istat=str_upcase(stat_fl,file_nam(ia:ie))
	    stat_fl(nc_stat+1:nc_stat+1)=' '
	    istat=str_upcase(chan_fl,file_nam(lf-2:lf))
	  endif
c	  write(*,*) 'extract:',is,ie,stat_fl,ia,lf,chan_fl
	else
	  write(*,*) 'directory name error: ',file_nam
	  ia=1
	endif
c
	ist=0
c	ilen_stat =lenc(stat_fl)
c	if(ilen_stat.eq.4) stat_fl = stat_fl(1:4)//' ' 
c	if(ilen_stat.eq.3) stat_fl = stat_fl(1:3)//'  '
	do n=1,no_stat
	  ich = lenc(stat(n))
	  if(stat(n)(1:ich).eq.stat_fl(1:ich)) ist=n
	enddo
	ich=0
	do n=1,no_chan
	  if(chan(n).eq.chan_fl) ich=n
	enddo
c
	if(ist.gt.0.and.ich.gt.0) then
	  check_infile=0
	else
	  check_infile=1
	  write(*,*) 'Warning: Station and/or channel names not ',
     &      'consistent with list: ',stat_fl,' ',chan_fl,' ',
     &      file_nam(lf-2:lf),ist,ich
	endif
c
	return
	end
	function sr_start(unit,unit1,inp,icp,irew,nskip,nlgfl,irec,
     &    istim,sr_stat,sr_chan,iftim,iftsc,stat,chan,net_id,ismp_st,
     &    neof,sr_ltim,ired,ibuf)
c
c   Positioning of disk file or tape file at a certain time
c
c   Return values:
c       0 - OK
c       1 - desired time not in this file (gap)
c       2 - end-of-file before time desired, or read error during positioning
 
	implicit integer(a-z)
	integer ibuf(*),jbuf(256)
C       byte b14,b15,b17
	character label*13,cid*2,cid1*2,cid2*2,cbuf*1024,sr_stat*5,
     &	sr_chan*3,stat*5,chan*3,net_id*2
	real*4 rate,dt
	real*8 sr_tim,new_tim,next_tim,dum_tim,rnd_tim,sr_ltim
	equivalence (jbuf,cbuf)
C       equivalence (b14,cid(1:1)),(b15,cid1(1:1)),(b17,cid2(1:1))
C       data b14/14/,b15/15/,b17/17/
	data cid/'SS'/,cid1/'SS'/,cid2/'SS'/
c
	cid (1:1)=char(14)
	cid1(1:1)=char(15)
	cid2(1:1)=char(17)

	sr_start=0
	sr_tim=istim
	istat=0
	next_tim=0.0
	nrec=0
	num=0
	if(icp.eq.1) then
	  stat=' '
	  chan=' '
	else
	  stat='xyz'
	  chan='xyz'
	endif
c
	if(inp.eq.0) then
	  if(irew.eq.1) then
	    write(*,'(1x,a,/)') 'Rewind tape...'
	    call rew(unit)
	    call rd(unit,jbuf,512,ich,nbt)
	    if(cbuf(1:3).eq.'DRM') then
	      indx=index(cbuf,cid)+2
	      if(indx.eq.2) indx=index(cbuf,cid1)+2
	      if(indx.eq.2) indx=index(cbuf,cid2)+2
	      if(indx.gt.2) then
	        label=cbuf(indx:)
	        write(*,'(1x,3a,/)') 'Tape label found: ',label,
     &            ' - skip header file'
	      else
	        write(*,'(1x,a,/)') 'No correct tape label found'
	      endif
	      call spfil(unit,1,nsk)
	      neof=1
	    else
	      neof=0
	      write(*,'(1x,a,/)') 'No header file found'
	      call rew(unit)
	    endif
	    irew=0
	  endif
c
	  nskp=nskip-neof
	  do while(nskp.gt.0)
	    write(*,*) 'Try to skip',nskp,' file marks...'
	    call spfil(unit,nskp,mskip)
	    write(*,*) mskip,' files skipped'
	    nskp=nskp-mskip
	    if(nskp.gt.0) then
	      write(*,*) 'EOV found - try to read behind next file marks'
	      ich=5
	      meof=0
	      do while (ich.eq.5)
	        call rd(unit,ibuf,512,ich,nbt)
	        nskp=nskp-1
	        meof=meof+1
	      enddo
	      nskp=nskp+1
	      meof=meof-1
	      write(*,*) meof,' more file marks found'
	      if(nskp.lt.1) call back(unit,1,nbk)
	    endif
	  enddo
	  if(nskip.gt.0)
     &	  write(*,*) 'All file marks skipped - try to read data records'
	  neof=nskip
	endif
	irec=0
	anf=0
c
c	write(*,*) istat,stat,sr_stat,chan,sr_chan,sr_tim,next_tim
	do while (istat.ne.2.and.istat.ne.4.and.(stat.ne.sr_stat.or.
     &	chan.ne.sr_chan.or.sr_tim.gt.next_tim))
	  if(inp.eq.0) then
	    istat=read_rec_tap('QT',0,unit,0,1024,256,1024,ibuf,num,
     &        stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp,iof,
     &        nframe,neof)
	    new_tim=dble(iftim)+dble(iftsc+timcr)*0.0001
c	write(*,*) istat,stat,chan,iftim,iftsc,timcr
	  else
	    if(irec.eq.0) then
	      irec=irec+1
	      istat=read_rec_fil('QT',1,unit,nlgfl,nlgfl,ibuf,irec,
     &           num,frm,stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp,
     &           iof,nframe)
	      new_tim=dble(iftim)+dble(iftsc+timcr)*0.0001
c	write(*,*) istat,stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp
	    else
	      istim=sr_tim
	      istat=get_rec(1,unit,istim,nlgfl,nlgfl,ibuf,irec,num,
     &        stat,chan,net_id,rate,nsamp,iof,nframe,
     &        new_tim,dum_tim,anf)
	      call tfix(new_tim,iftim,iftsc)
c	write(*,*) 'get_rec',num,new_tim,nsamp,stat,chan,anf
	    endif
	    if(istat.ne.0) istat=4
	  endif
	  if(istat.ne.4) then
	    if(istat.ne.0) then
	      write(*,*) 'EOF found - try next record',istat
	      nrec=0
	      num=0
	    else
	      nrec=nrec+1
	      next_tim=new_tim+nsamp/rate
	      if(istat.eq.0.and.nrec.eq.1) then
	        if(icp.eq.-1) then
	          ichk=look_pos_file(unit1,stat,chan,new_tim,sr_stat,
     &              sr_chan,sr_tim,sr_ltim,ired)
	          if(ichk.eq.2) then
	            write(*,*) 'End of extraction file found'
	            sr_start=2
	            return
	          endif
	        endif
c
	        mskp=0
c	        if(rate.eq.1.) nsfil=2419200	!28 Tage
c	        if(rate.eq.80.) nsfil=86400	! 1 Tag
	        if(inp.eq.1) then
	          sr_stat=stat
	          sr_chan=chan
	        endif
	        if(inp.eq.0.and.stat.ne.sr_stat) then
	          stop 'Wrong station'
	        endif
c	        if(inp.eq.0.and.istat.eq.0.and.chan.ne.sr_chan.or.
c     &	        sr_tim-new_tim.gt.nsfil) then
C	write(*,*) istat,stat,chan,new_tim,sr_tim,nsfil
c	          write(*,*) 'Searched data not in present file - skip to next
c     & file'
c	          call spfil(unit,1,mskp)
c	          nrec=0
c	          num=0
c	        endif
	        if(mskp.eq.0) then
	          call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
                  call datum(0,idyf,iyrf,monf,itgf)
	          write(*,9) '  Search from:',
     &	            stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
9	            format(3(1x,a),f5.1,1x,2(i2.2,1h/),i4.4,
     &                     2x,2(i2.2,1h:),i2.2,1h.,i4.4)
	        endif
	      endif
	    endif
	  else
	    write(*,*) 'Read error during positioning'
	    sr_start=2
	    if(inp.eq.0) stop 'read error'
	  endif
	enddo
	call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	call datum(0,idyf,iyrf,monf,itgf)
c	write(*,*) nrec,istat,stat,sr_stat,chan,sr_chan,sr_tim,next_tim
	if(nrec.gt.0 .and. istat.eq.0 .and. stat.eq.sr_stat .and.
     &    chan.eq.sr_chan .and. sr_tim.lt.next_tim) then
	  write(*,9) ' Record found:',
     &	  stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
	  dt=1./rate
	  ismp_st=idint((sr_tim-new_tim)*rate+0.01)+1
	  dum_tim=new_tim+(ismp_st-1)*dt
c	  if(sr_tim-dum_tim.gt.dt*0.5) ismp_st=ismp_st+1
	  if(sr_tim-dum_tim.gt.0.0) ismp_st=ismp_st+1
	  dum_tim=new_tim+(ismp_st-1)*dt
	  dum_tim=rnd_tim(0,dum_tim,dt,tcor)
c	write(*,*) 'start',sr_tim,new_tim,ismp_st,dt,dum_tim,tcor
	  if(ismp_st.lt.1) then
	    ismp_st=1
	    call tfix(new_tim,iftim,iftsc)
	    write(*,*) 'Warning: time not found - take next time'
c	    sr_start=3
	  else
	    dum_tim=new_tim+(ismp_st-1)*dt
	    call tfix(dum_tim,iftim,iftsc)
	  endif
c	write(*,*) 'ismp_st',ismp_st,new_tim,sr_tim,timcr,dum_tim
	elseif(nrec.gt.0) then
	  write(*,9) '*****Record not found:',
     &	  stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
	  sr_start=1
	  ismp_st=0
	endif
	if(inp.eq.0.and.nrec.gt.0) then
	  call back(unit,4,nbk)
	elseif(inp.eq.1) then
	  irec=irec-1
	  if(irec.lt.0) irec=0
	endif
c
	return
	end
	function get_rec(nchan,unit,istim,nlong,lprec,ibuf,nrec,num,
     &    stat,chan,net_id,rate,nsamp,iof,nframe,anf_tim,start_tim,anf)
c
	implicit integer (a-z)
	integer ibuf(*)
	real*4 rate,dt,tmrec,diff
	real*8 start_tim,anf_tim,end_tim,sear_tim
	character stat*(*),chan*(*),net_id*(*)
	data nsmp_max/3770/
c
	ichk=4
c
	mrec=nrec
	sear_tim=istim
	dt=1./rate
	end_tim=anf_tim+dble(nsamp-1)*dt
	tmrec=float(nframe*60-10)/rate		!average time length of a record
	nsmp=nsmp_max
	if(nrec.eq.1) then
	  start_tim=anf_tim
	endif
c	write(*,*) 'get_rec',anf,nrec,sear_tim,start_tim,anf_tim,rate,nsamp,nlong,
c     &	lprec
	if(sear_tim.lt.start_tim-tmrec) then
	  nrec=1
	  get_rec=1
	  write(*,*) 'get_rec: time too early',nchan,sear_tim,start_tim
	  return
	endif
	iend=0
	ntry=0
c
	do while(iend.eq.0 .and.
     &             (sear_tim.lt.anf_tim .or. sear_tim.gt.end_tim .or.
     &              anf.eq.0 .or. ntry.eq.0)
     &    )
	  diff=(sear_tim-end_tim)/dt/nsmp
	  idiff=diff+0.5
	  if(idiff.gt.0) then
	    idiff=idiff+1
	  elseif(idiff.eq.0) then
	    idiff=-1
	  endif
	  mrec=mrec+idiff
	  if(mrec.lt.1) mrec=1
c100	write(*,*) 'get_rec 1:',nchan,unit,sear_tim,anf_tim,end_tim,mrec,
c     *	nrec,idiff,nsamp,dt
100	  istat=read_rec_fil('QT',nchan,unit,nlong,lprec,ibuf,mrec,
     &       num,frm,stat,chan,net_id,iatim,iatsc,timcor,rate,nsamp,
     &       iof,nframe)
	  ntry=ntry+1
	  nsmp=nsamp
c	write(*,*) 'get_rec 2:',nchan,istat,num,iatim,iatsc,nsamp,stat,chan,
c     &	ntry
	  if(ntry.gt.99) then
	    write(*,*) 'Get_rec retry error:',
     &        sear_tim,anf_tim,end_tim,mrec,nrec,idiff,nsmp,dt
	    istat=2
	  endif
	  if(istat.ne.0) then
	    if(istat.eq.1.and.ntry.lt.6) then
	      mrec=mrec*0.75
	      write(*,*) 'EOF - try 75% only',mrec,ntry
	      goto 100
	    else
	      ichk=istat
	      iend=1
	    endif
	  endif
	  ichk=istat
	  anf_tim=dble(iatim)+dble(iatsc+timcor)*0.0001
	  end_tim=anf_tim+dble(nsamp-1)*dt
	  if(sear_tim.lt.anf_tim.and.mrec.eq.1) then
	    iend=1
	    ichk=-1
	  endif
	  anf=1
	enddo
c
	if(ichk.gt.0) then
	  mrec=0
	  istat=0
	  end_tim=0.0
	  write(*,*) 'Direct access failed - try to read sequentially'
	  do while(istat.eq.0.and.sear_tim.gt.end_tim)
	    mrec=mrec+1
	    istat=read_rec_fil('QT',nchan,unit,lprec,1024,ibuf,mrec,
     &      num,frm,stat,chan,net_id,iatim,iatsc,timcor,rate,nsamp,iof,
     &      nframe)
c	write(*,*) istat,mrec,num,stat,chan,iatim,iatsc,timcor,rate,nsamp
	    anf_tim=dble(iatim)+dble(iatsc+timcor)*0.0001
	    end_tim=anf_tim+dble(nsamp-1)*dt
	  enddo
	  if(istat.eq.0) then
	    if(sear_tim.lt.end_tim) then
	      ichk=0
            else
	      ichk=3
	      write(*,*) 'GET_REC: time not found',sear_tim,nchan
	    endif
	  else
	    write(*,*) 'GET_REC: read error or EOF found',mrec,istat,
     &        nchan
	  endif
	endif
c
	nrec=mrec
	get_rec=ichk
c
	return
	end
	function look_pos_file(unit1,fd_stat,fd_chan,fd_tim,sr_stat,
     &    sr_chan,sr_tim,sr_ltim,ired)
c
c   Checks if found data belongs to searched event (input from unit1)
c
	implicit integer (a-z)
c	integer len_mon(12),rdy(1000,3)
	integer len_mon(12)
	character fd_stat*(*),fd_chan*(*),sr_stat*(*),sr_chan*(*),
     &  stat*5,chan*3,ans*1,all*3,alls*3
	real*8 fd_tim,fde_tim,sr_tim,anf_tim,end_tim,sr_ltim
	data len_mon/31,28,31,30,31,30,31,31,30,31,30,31/
c	data rdy/3000*0/
c
	look_pos_file=0
c
	if(fd_chan(1:1).eq.'H') nsfil=86400		!Sec pro Tag
	if(fd_chan(1:1).eq.'L') then
	  call tfix(fd_tim,iatim,iatsc)
	  call abstim(0,iatim,iyra,idya,iha,ima,isa)
	  call datum(0,idya,iyra,mona,itga)
	  nsfil=len_mon(mona)*86400			!Sec pro Monat
	  all='ALL'
	  alls='all'
	else
	  nsfil=86400
	  all='ALH'
	  alls='alh'
	endif
	fde_tim=fd_tim+nsfil
	rewind unit1
c	read(unit1,'(a)') proc_typ
	iend=0
	ncount=0
	ios=0
c
	do while (iend.eq.0.and.ios.eq.0)
	  read(unit1,'(2a,2(2(1x,i2),1x,i4,3(1x,i2)),1x,a)',iostat=ios)
     &	  stat,chan,tga,mna,yra,hha,mma,ssa,tge,mne,yre,hhe,mme,sse,ans
	  if(ios.eq.0) then
	    ncount=ncount+1
	    call datum(1,dya,yra,mna,tga)
	    call abstim(1,iatim,yra,dya,hha,mma,ssa)
	    anf_tim=dble(iatim)
	    call datum(1,dye,yre,mne,tge)
	    call abstim(1,ietim,yre,dye,hhe,mme,sse)
	    end_tim=dble(ietim)
	    if(chan(3:3).eq.'Z') nch=1
	    if(chan(3:3).eq.'N') nch=2
	    if(chan(3:3).eq.'E') nch=3
	    sr_stat=fd_stat
c	write(*,*) stat,chan,tga,mna,yra,hha,mma,ssa,tge,mne,yre,hhe,mme,sse,ans
c	write(*,*) stat,chan,fd_stat,fd_chan,all,alls
c	    if((stat.eq.fd_stat.or.stat.eq.'ALL'.or.stat.eq.'all') .and.
c     &	     (chan.eq.fd_chan.or.chan.eq.all.or.chan.eq.alls) .and.
c     &	     rdy(ncount,nch).ne.1) then
	    if( (stat.eq.fd_stat .or. stat.eq.'ALL' .or. stat.eq.'all')
     &          .and.
     &	        (chan.eq.fd_chan .or. chan.eq.all .or. chan.eq.alls)
     &        ) then
c	write(*,*) chan,fd_chan,anf_tim,end_tim,fd_tim,fde_tim
	      if((anf_tim.ge.fd_tim.and.anf_tim.le.fde_tim) .or.
     &	       (end_tim.gt.fd_tim.and.end_tim.lt.fde_tim) .or.
     &	       (fd_tim.ge.anf_tim.and.fd_tim.lt.end_tim)) then
	        sr_tim=anf_tim
	        sr_stat=fd_stat
	        sr_chan=fd_chan
	        sr_ltim=end_tim
	        ired=0
	        if(sr_chan(1:1).eq.'H'.and.ans.eq.'Y'.or.ans.eq.'y')
     &            ired=1
	        iend=1
c	        rdy(ncount,nch)=1
	      endif
	    endif
c	  else
c	    iend=2
c	    do n=1,ncount
c	      do nn=1,3
c	        if(rdy(n,nn).eq.0) iend=0
c	      enddo
c	    enddo
	  endif
	enddo
c
	if(iend.eq.0) then
	  look_pos_file=1
	  sr_tim=0.0
	  sr_chan='xyz'
	elseif(iend.eq.2) then
	  look_pos_file=2
	endif
c	write(*,*) 'look:',look_pos_end,fd_tim,fd_stat,fd_chan,sr_tim,sr_ltim,
c     &	sr_stat,sr_chan,ired
c
	return
	end
	subroutine change_time_string(string,itag,mon,iyear,ih,im,is)
c
	implicit integer (a-z)
	character string*(*)
c
	read(string,'(3i2,1x,3i2)') iyear,mon,itag,ih,im,is
	iyear=1900+iyear
	if(iyear.lt.1950) iyear=iyear+100
c
	return
	end
c	function get_next_event(unit,nst_tot,stat_net,no_stat,sr_stat,
c     &	no_chan,sr_chan,chan_typ,first_tim,last_tim,jred,more)
cc
cc   Reads in copy parameters from parameter file (already opened)
cc
c	implicit integer (a-z)
c	integer nstat(100),kan(300)
c	character sr_stat(*)*(*),sr_chan(*)*(*),chan_typ*(*),stat_net(*)*(*),
c     &	comp_net(3)*1,fd_stat*5,fd_chan*3,ans,char*132
c	real*8 first_tim,last_tim
c	data nch_tot,comp_net/3,'Z','N','E'/
c	data ianf/0/
cc
c	if(ianf.eq.0) then
c	  char=' '
c	  ios=0
c	  do while (char(12:12).ne.'/'.and.char(1:2).ne.'W1'.and.ios.eq.0)
c	    read(unit,'(a)',iostat=ios) char
c	  enddo
c	  if(ios.eq.0) then
c	    if(char(1:2).eq.'W1') then
c	      icp=0
c	      read(char,19,iostat=iosc) tga,mna,yra,hha,mms,ssa,nrc,
c     &	      nst,(nstat(i),i=1,nst),nch,(kan(i),i=1,nch)
c19	      format(3x,2(i2,1x),i4,3(1x,i2),i5,i4,<nst>i2,i4,<nch>i2)
c	      if(iosc.ne.0) then
c	        read(char,29) tga,mna,yra,hha,mms,ssa,nrc,nst
c29	        format(3x,2(i2,1x),i4,3(1x,i2),i5,i4)
c	      endif
c	      fd_stat='ALL'
c	      fd_chan='ALB'
c	      ans='N'
c	    else
c	      icp=1
c	      read(char,'(2a,2(2(1x,i2),1x,i4,3(1x,i2)),1x,a)',iostat=iosc)
c     &	      fd_stat,fd_chan,tga,mna,yra,hha,mma,ssa,tge,mne,yre,hhe,
c     &	      mme,sse,ans
c	      ianf=1
c	    endif
c	  endif
c	endif
cc
c	call datum(1,dya,yra,mna,tga)
c	call abstim(1,iatim,yra,dya,hha,mma,ssa)
c	first_tim=dble(iatim)
c	if(icp.eq.1) then
c	  call datum(1,dye,yre,mne,tge)
c	  call abstim(1,ietim,yre,dye,hhe,mme,sse)
c	else
c	  ietim=iatim+nrc*10
c	endif
c	last_tim=dble(ietim)
cc
c	if(fd_stat(1:3).eq.'ALL'.or.fd_stat(1:3).eq.'all') then
c	  no_stat=nst_tot
c	  do n=1,no_stat
c	    sr_stat(n)=stat_net(n)
c	  enddo
c	else
c	  no_stat=1
c	  ichk=str_upcase(sr_stat(1),fd_stat)
c	endif
c	if(fd_chan(1:2).eq.'AL'.or.fd_chan(1:2).eq.'al') then
c	  ichk=str_upcase(chan_typ,fd_chan(3:3))
c	  no_chan=nch_tot
c	  do n=1,no_chan
c	    sr_chan(n)=chan_typ//'H'//comp_net(n)
c	  enddo
c	else
c	  ichk=str_upcase(chan_typ,fd_chan(1:1))
c	  no_chan=1
c	  ichk=str_upcase(sr_chan(1),fd_chan)
c	endif
cc
c	jred=0
c	if((chan_typ.eq.'H'.or.chan_typ.eq.'h').and.(ans.eq.'Y'.or.ans.eq.'y'))
c     &	jred=1
cc
c	char=' '
c	ios=0
c	do while (char(12:12).ne.'/'.and.char(1:2).ne.'W1'.and.ios.eq.0)
c	  read(unit,'(a)',iostat=ios) char
c	enddo
c	if(ios.eq.0) then
c	  if(char(1:2).eq.'W1') then
c	    read(char,19,iostat=iosc) tga,mna,yra,hha,mms,ssa,nrc,
c     &	    nst,(nstat(i),i=1,nst),nch,(kan(i),i=1,nch)
c	    if(iosc.ne.0) then
c	      read(char,29) tga,mna,yra,hha,mms,ssa,nrc,nst
c	    endif
c	  else
c	    read(char,'(2a,2(2(1x,i2),1x,i4,3(1x,i2)),1x,a)',iostat=iosc)
c     &	    fd_stat,fd_chan,tga,mna,yra,hha,mma,ssa,tge,mne,yre,hhe,mme,
c     &	    sse,ans
c	  endif
c	endif
c	more=1
c	if(ios.ne.0) more=0
cc
c	return
c	end
	subroutine corr_tim(new_tim,tmcr,dt,numi,stat,chan,net_id,
     &                      iof,nsamp,form,reclen,ibuf)
c
c   Corrects the record start to the next full sampling time
c
	implicit integer(a-z)
	integer ibuf(*)
	character stat*(*),chan*(*),net_id*(*)
	real dt
	real*8 rnd_tim,new_tim,dum_tim
c
	dum_tim=rnd_tim(0,new_tim,dt,tmcr)
	call tfix(dum_tim,iftim,iftsc)
	ichk=def_fixhead_ful(
     &    numi,stat,chan,net_id,dt,iof*4,iftim,iftsc,tmcr,
     &	  nsamp,form,reclen,0,0,0,ibuf)
c	  write(*,*) 'time corrected',numi,iftim,iftsc,tmcr,new_tim,
c     &    dum_tim,dt
c
	new_tim=dum_tim
c
	return
	end
	subroutine uncorr_tim(ibuf,net_idw)
c
	implicit integer(a-z)
	integer ibuf(*)
	character stat*5,chan*3,net_id*2,net_idw*(*)
	real*4 rate
	real*8 dum_tim
c
	icdf=dec_fixhead(num,stat,chan,net_id,rate,iof,iftim,iftsc,
     &    timcr,isamp,form,reclen,ibuf)
c	write(*,*) 'dec',num,stat,chan,rate,iof,iftim,iftsc,timcr,isamp
	dum_tim=dble(iftim)+dble(iftsc+timcr)*0.0001
	call tfix(dum_tim,iftim,iftsc)
	icdf=def_fixhead_ful(
     &    num,stat,chan,net_idw,1./rate,iof,iftim,iftsc,0,
     &	  isamp,form,reclen,0,0,0,ibuf)
c
	return
	end
	subroutine del_overlap(unit,nrec,new_tim,next_tim,dt,isamp)
c
c   Corrects sample overlap occuring in concatenating files
c
	implicit integer(a-z)
	integer ibuf(2000),jbuf(4000)
c	logical DEC
	character stat*5,chan*3,net_id*2
	real*8 next_tim,new_tim
	real*4 rate,dt
c	data DEC/.true./
c
c	write(*,*) 'del_overlap: ',nrec,new_tim,next_tim,dt
	if(next_tim-new_tim.gt.0.99*dt) then
		write(*,*) 'data overlap: ignored',new_tim,next_tim,
     &	        abs(new_tim-next_tim)
		return
c	  if(nrec.eq.1) then
c	    ndsm=idint((new_tim-next_tim)/dt)
c	    if(ndsm.eq.-1) then
c	      write(*,*) 'Sample overlap in new file - correction'
cc	      backspace unit
c	      read(unit,rec=nrec) (ibuf(i),i=1,1024)
c	      icdc=dec_fixhead(num,stat,chan,net_id,rate,begdat,iftim,iftsc,
c     &	      timcor,nsamp,ibuf)
c	      iof=begdat/4
c	      nframe=(4096-begdat)/64
c	      ldat=1024-iof
cc	      if(DEC) call i4swap(ldat,ibuf(iof+1),ibuf(iof+1))
c	      ifchk=0
c	      idchk=decomp_steim(1,ifchk,num,nframe,nsamp,ibuf(iof+1),
c     &	      ni_dat,jbuf,last_val)
cc	                  write(*,*) 'in',sm_last,last_val,nsamp,ni_dat,num,iof,
cc     &	                  nfram,ldat,timcor
c	      jbuf(ni_dat)=0
c	      nsamp=ni_dat-1
c	      cmpmod=0
c	      icmp=compress_steim(cmpmod,1,nrec,nsamp,jbuf,iof,1024,ndat,mdat,
c     &	      ibuf,totsmp,totsmp_1)
c	      if(ndat.lt.1024) then
c	        icmp=compress_steim(999,1,nrec,0,jbuf,iof,1024,ndat,mdat,ibuf,
c     &	        totsmp,totsmp_1)
c	      elseif(ndat.gt.1024) then
c	        write(*,*) 'Compress error: too much data',nrec,ia,
c     &	        nsamp,ndat,mdat,icmp,icdf,iftim,iftsc,timcor
c	      endif
cc	                  write(*,*) 'out',ndat,mdat,totsmp,totsmp_1
c	      if(mdat.eq.0) then
c	        write(*,*) 'Compress error: zero counter',nrec,ia,
c     &	        nsamp,ndat,mdat,icmp,icdf,iftim,iftsc,timcor
c	      endif
cc	      if(DEC) call i4swap(ldat,ibuf(iof+1),ibuf(iof+1))
c	      icdf=def_fixhead(num,stat,chan,net_id,dt,iof*4,iftim,iftsc,
c     &	      timcor,mdat,ibuf)
cc	      backspace unit
c	      write(unit,rec=nrec) (ibuf(i),i=1,1024)
c	      new_tim=new_tim+dt
c	      isamp=isamp-1
c	    endif
c	  endif
	endif
c
	return
	end
	subroutine check_rec(mode,no_fil,nrec,statf,chanf,ratef,nsamp,
     &	anf_tim,timcr,nst,stat_lkup,nch,chan_lkup,rate,vol_anf,vol_end,
     &	nspn,spa_tim,spe_tim,nser,sera_tim,sere_tim,sera_idx,sere_idx)
c
c   Extracts all informations needed for the creation of descriptor files
c   and SEED headers
c
	implicit integer(a-z)
	logical found,stat_found
	integer stat_lkup(*),nch(*),chan_lkup(100,15),nser(100,15),
     &	sera_idx(100,100,15),sere_idx(100,100,15),last_rec(100,15)
	real*4 dt,ratef,rate(100,15)
	real*8 anf_tim,end_tim,next_tim(100,15),vol_anf,vol_end,
     &  spa_tim(*),spe_tim(*),
     &  sera_tim(100,100,15),sere_tim(100,100,15),anf_timc,
     &	end_timc,rnd_tim,spe_tmax,anf_etim,end_etim
	character statf*(*),chanf*(*)
c
 	include 'head_seed.inc'
c
	data ianf/0/, nrec_tot/0/, spe_tmax/0.0/, last_rec_tot/0/
	data nstff/0/, nchff/0/
c
	end_tim=anf_tim+dble(nsamp)/ratef
	anf_timc=anf_tim+dble(timcr)*0.0001
	end_timc=anf_timc+dble(nsamp-1)/ratef
	dt=1/ratef
c
c	write(*,*) 'check 1:',mode,nrec,no_fil,statf,chanf,ratef,nsamp
c	write(*,*) 'check 1:',mode,nrec,no_fil,statf,chanf,ratef,nsamp,anf_tim,
c     &	nst_tot,nch_tot
	if(mode.eq.0 .or.
     &     (mode.eq.999 .and. no_fil.eq.1 .and. ianf.eq.0)
     &    ) then
	  vol_anf=2.e9
	  vol_end=0.
	  nspn=0
	  nst=0
	  do n=1,nst_tot
	    nch(n)=0
	    stat_lkup(n)=0
	    do nn=1,nch_tot(n)
	      rate(n,nn)=0.0
	      chan_lkup(n,nn)=0
	      nser(n,nn)=0
	      do nnn=1,100
	        spa_tim(nnn)=0.0
	        spe_tim(nnn)=0.0
	        sera_tim(nnn,n,nn)=0.0
	        sere_tim(nnn,n,nn)=0.0
	        sera_idx(nnn,n,nn)=0
	        sere_idx(nnn,n,nn)=0
	      enddo
	    enddo
	  enddo
	  if(mode.ne.999) mode=2
	  ianf=1
	  nrec_tot=0
	  last_rec_tot=-1
	endif
c
c	if(nrec.ne.last_rec_tot.or.nrec.eq.1) nrec_tot=nrec_tot+1
	if(nrec.ne.last_rec_tot .or.
     &    (nrec.eq.1.and.mode.ne.999)) nrec_tot=nrec_tot+1
	do nn=1,nst_tot
	  if(statf.eq.stat_net(nn)) then
	    nstf=nn
	    stat_found=.false.
	    if(nst.gt.0) then
	      do nnn=1,nst
	        if(nstf.eq.stat_lkup(nnn)) stat_found=.true.
	      enddo
	    endif
c	    if(.not.found) then
	    call parse_time(stat_st_tim(nn),anf_etim)
	    if(stat_end_tim(nn).ne.'0/0') then
	      call parse_time(stat_end_tim(nn),end_etim)
	    else
	      end_etim=1.e10
	    endif
	    if(anf_tim.gt.anf_etim.and.anf_tim.lt.end_etim) then
	      if(.not.stat_found) then
	        nst=nst+1
	        stat_lkup(nst)=nstf
	      endif
	      stat_found=.true.
	      nstff=nstf
c	      write(*,*) 'time found',nst,stat_lkup(nst),nstf
	      if(nst.gt.nst_tot) then
	        write(*,*) 'Warning: too many stations',nst
	      endif
	    endif
c	    endif
c	    write(*,*) 'st',nstf,stat_found
	    if(stat_found) then
	     do mm=1,nch_tot(nstf)
c	      write(*,*) 'chan',mm,nstf,chanf,chan_net(mm,nstf)
	      if(chanf.eq.chan_net(mm,nstf)) then
	        nchf=mm
	        found=.false.
	        if(nch(nstf).gt.0) then
	          do mmm=1,nch(nstf)
	            if(nchf.eq.chan_lkup(nstf,mmm)) found=.true.
	          enddo
	        endif
	        if(.not.found) then
	          ncht=ncht+1
	          nch(nstf)=nch(nstf)+1
	          chan_lkup(nstf,nch(nstf))=nchf
	          rate(nstf,nchf)=ratef
	        endif
		nchff=nchf
	      endif
	     enddo
	    endif
	  endif
	enddo
c	write(*,*) 'cntr',statf,chanf,nstf,nstff,stat_lkup(nst),nchf,nst,
c     &	nch(nstf),nrec,nrec_tot,anf_timc
	nstf=nstff
	nchf=nchff
c
	if(nrec.eq.1.and.mode.ne.999) then
	  if(nser(nstf,nchf).gt.0) then
c	    sere_tim(nser(nstf,nchf),nstf,nchf)=rnd_tim(0,next_tim(nstf,
c     &	    nchf),dt,tcr)
	    sere_tim(nser(nstf,nchf),nstf,nchf)=next_tim(nstf,nchf)
	    sere_idx(nser(nstf,nchf),nstf,nchf)=last_rec(nstf,nchf)
	  endif
	  nser(nstf,nchf)=nser(nstf,nchf)+1
c	  sera_tim(nser(nstf,nchf),nstf,nchf)=anf_tim
	  sera_tim(nser(nstf,nchf),nstf,nchf)=anf_timc
c	  sera_idx(nser(nstf,nchf),nstf,nchf)=nrec
	  sera_idx(nser(nstf,nchf),nstf,nchf)=nrec_tot
	  if(sera_tim(nser(nstf,nchf),nstf,nchf).lt.vol_anf)
     &      vol_anf=sera_tim(nser(nstf,nchf),nstf,nchf)
c	write(*,*) 'ch_anf:',sera_idx(nser(nstf,nchf),nstf,nchf),sera_tim(nser(nstf,nchf),nstf,nchf)
	  next_tim(nstf,nchf)=anf_timc
c
	  if(no_fil.eq.1) then
	    if(nspn.gt.0) spe_tim(nspn)=spe_tmax
	    nspn=nspn+1
c	    spa_tim(nspn)=anf_tim
c	    spe_tmax=end_tim
	    spa_tim(nspn)=anf_timc
	    spe_tmax=end_timc
	  endif
c	  if(spa_tim(nspn).gt.anf_tim) spa_tim(nspn)=anf_tim
	  if(spa_tim(nspn).gt.anf_timc) spa_tim(nspn)=anf_timc
c	write(*,*) nst,nch(nstf),nstf,nchf,no_fil,nser(nstf,nchf)
	endif
c
	if(mode.ne.999) then
c	  if(abs(anf_timc-next_tim(nstf,nchf)).gt.0.99/ratef) then
	  if(abs(anf_timc-next_tim(nstf,nchf)).gt.1000./ratef) then
	    call tfix(anf_timc,iftim,iftsc)
	    call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	    call datum(0,idyf,iyrf,monf,itgf)
	    call tfix(next_tim(nstf,nchf),intim,intsc)
	    call abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	    call datum(0,idyn,iyrn,monn,itgn)
	    write(*,9) 'Time-gap:',itgf,monf,iyrf,ihf,imf,isf,iftsc,
     &	    'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc
9	format(2(1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,
     &         i4.4))
	    if(anf_timc-next_tim(nstf,nchf).gt.1000./ratef) then
	      sere_tim(nser(nstf,nchf),nstf,nchf)=
     &          rnd_tim(0,next_tim(nstf,nchf),dt,tcr)
cc	      sere_idx(nser(nstf,nchf),nstf,nchf)=nrec-1
	      sere_idx(nser(nstf,nchf),nstf,nchf)=nrec_tot-1
	      nser(nstf,nchf)=nser(nstf,nchf)+1
cc	      sera_tim(nser(nstf,nchf),nstf,nchf)=anf_tim
	      sera_tim(nser(nstf,nchf),nstf,nchf)=anf_timc
cc	      sera_idx(nser(nstf,nchf),nstf,nchf)=nrec
	      sera_idx(nser(nstf,nchf),nstf,nchf)=nrec_tot
c	write(*,*) 'gap',nser(nstf,nchf),nstf,nchf,anf_timc,nrec_tot
	    endif
	  endif
	endif
c
c	write(*,*) 'check 999',mode,no_fil,nser(nstf,nchf),nstf,nchf,end_tim,nrec,nst
	if(mode.eq.999) then
c	  sere_tim(nser(nstf,nchf),nstf,nchf)=end_tim
	  sere_tim(nser(nstf,nchf),nstf,nchf)=end_timc
c	  sere_idx(nser(nstf,nchf),nstf,nchf)=nrec
	  sere_idx(nser(nstf,nchf),nstf,nchf)=nrec_tot
c	write(*,*) 'end gap',nser(nstf,nchf),nstf,nchf,end_timc,nrec_tot
  	if(sere_tim(nser(nstf,nchf),nstf,nchf).gt.vol_end)
     &    vol_end=sere_tim(nser(nstf,nchf),nstf,nchf)
c	  if(spe_tmax.lt.end_tim) spe_tmax=end_tim
	  if(spe_tmax.lt.end_timc) spe_tmax=end_timc
	  spe_tim(nspn)=spe_tmax
c	write(*,*) 'check_rec',nrec,nrec_tot,nser(nstf,nchf),nstf,nchf,sera_idx(nser(nstf,
c     &	nchf),nstf,nchf),sere_idx(nser(nstf,nchf),nstf,nchf)
	  ianf=0
	endif
c
	last_file=no_fil
	next_tim(nstf,nchf)=end_timc+1./ratef
c	last_rec(nstf,nchf)=nrec
	last_rec(nstf,nchf)=nrec_tot
	last_rec_tot=nrec
c	if(spe_tmax.lt.end_tim) spe_tmax=end_tim
	if(spe_tmax.lt.end_timc) spe_tmax=end_timc
c
	return
	end
	function i4val(swp,ival)
	logical swp
	if (swp) then
	   call i4swap(1,ival,ires)
	   i4val = ires
        else
	   i4val = ival
        endif
	end
	function i2val(swp,ival)
	integer*2 ival
	logical swp
	if (swp) then
	   i2val = iswap(ival)
        else
	   i2val = ival
        endif
	end
	function decomp_steim(form,ifchk,nrec,nframe,nsamp,
     &    in_buf,num_dat,out_buf,last_val)
c
c   Decodes SEED data record in Steim compression algorithm
c
c   Return values:
c       0 - OK
c       -1 - start value wrong
c       -2 end value wrong
c       -3 sample count wrong
	implicit integer (a-z)
	logical init,swp
	integer in_buf(*),out_buf(*),diff(7)
	data lvc_fl/0/
9001    format(1x,'Warning: ',a,' value check failed in record',i5,
     &     ': got',i11,', expecting',i11)
9002    format(1x,'Warning: no of data too ',a,' in record',i5,
     &     ': got ',i5,', expecting ',i5)
9003    format(1x,'Warning: ',a,' value zero in record',i5)
9004    format(1x,'Internal logic error:  Frame ',a,
     &     ' sample count mismatch')
c
	ichk=0
	swp=form/256 .ne. byteorder()
c
	start_value=i4val(swp,in_buf(2))
	end_value=i4val(swp,in_buf(3))
	if(start_value.eq.0) write(*,9003) 'start',nrec
	if(end_value.eq.0)   write(*,9003) 'end',nrec
	num_dat=0
	init=nrec.eq.1
c	write(*,*) 'start,end',start_value,end_value,nsamp,ifchk
c
	n=0
	iend=0
	ncount=0
	do while (n.le.nframe.and.iend.eq.0)
	  n=n+1
	  ncount=ncount+1
	  nibbles=i4val(swp,in_buf(ncount))
C         The frame might be a reblocked one that was the beginning of a block
C         and contains begin/end values.  Refresh the end value if so.
C         Actually, some dataloggers insert these in data streams for no reason,
C         and expect any values after the first frame to be ignored.  So we do
C         too.
	  if (nibbles/2**26 .eq. 0) then
c           if (nibbles .ne. 0) then
c             start_value=i4val(swp,in_buf(ncount+1))
c             end_value=i4val(swp,in_buf(ncount+2))
c           endif
	    nn=2
	    ncount=ncount+2
	  else
	    nn=0
	  endif
	  nsfrm=0
	  do while (nn.lt.15.and.iend.eq.0)
	    nn=nn+1
	    ncount=ncount+1
	    nibble=get_nibble(nibbles,nn)
	    ndiff=extr_diff(form,in_buf(ncount),nibble,diff)
	    nsfrm=nsfrm+ndiff
	    if (num_dat.eq.0) then
C             if (init) then
	        value=start_value-diff(1)
C             else
C               value=last_val-diff(1)
C             endif
	    endif
c	write(*,*) 'extr_diff',ncount,in_buf(ncount),nibble,ndiff,iend,num_dat,nn
	    nnn=1
	    do while (nnn.le.ndiff.and.iend.eq.0)
	      num_dat=num_dat+1
c             if(num_dat.eq.1) then
c               if(dabs(dble(last_val)+dble(diff(nnn))).gt.2.1e9) then
c                 write(*,*) 'DECOMP warning: data overflow',nrec,num_dat,
c    &	          last_val,diff(nnn)
c                 last_val=0
c               endif
c               value=last_val+diff(nnn)
c	write(*,*) 'value1',num_dat,nnn,diff(nnn),value,start_value
c               if(ifchk.eq.1) then
c                 if(value.ne.start_value) then
c                   if(warn) then
c                     write(*,9001) 'first',nrec,value,start_value
c     	              write(*,*) 'Use new start value for decompressing'
c       	    endif
c                   ichk=-1
c                   value=start_value
c                 endif
c               else
c                 value=start_value
c               endif
c             elseif(num_dat.gt.nsamp) then
              if(num_dat.gt.nsamp) then
	        num_dat=num_dat-1
	        iend=2
	      else
c               if(iabs(value).lt.16777216) value=value+diff(nnn)
                value=value+diff(nnn)
c	write(*,*) 'value2',num_dat,nnn,diff(nnn),value,ndiff
	        if (num_dat.eq.1.and.value.ne.start_value) then
                  write(*,9001) 'first',nrec,last_val,start_value
                  ichk=-1
	        endif
	      endif
	      out_buf(num_dat)=value
c	write(*,*) 'out_buf',num_dat,out_buf(num_dat)
	      if(nnn.eq.ndiff.and.iend.ne.2) iend=1
	      nnn=nnn+1
	    enddo
	    if(nn.lt.15.and.iend.ne.2) iend=0
	  enddo
	  if(n.lt.nframe.and.iend.ne.2) iend=0
	  if (nssamp(nibbles).ne.nsfrm.and.iend.ne.2) write(*,9004) n
	enddo
	last_val=value
	if(value.ne.end_value) then
	  if(lvc_fl.le.5) then
	    write(*,9001) 'last',nrec,value,end_value
	    if(lvc_fl.eq.5) then
	      write(*,*) ' Limit exceeded - stopping warnings'
	    endif
	  endif
	  ichk=-2
	  lvc_fl=lvc_fl+1
	else
	  lvc_fl=0
	endif
	if(num_dat.lt.nsamp) then
	  write(*,9002) 'small',nrec,num_dat,nsamp
	  ichk=-3
	endif
	if(num_dat.gt.nsamp) then
	  write(*,9002) 'large',nrec,num_dat,nsamp
	  ichk=-3
	  num_dat=nsamp
	endif
c	write(*,*) 'decomp end:',nsamp,num_dat,(out_buf(i),i=1,num_dat)
c
	decomp_steim=ichk
	return
	end
	integer function get_nibble(nibbles,no_nib)
	get_nibble = mod(nibbles/4**(15-no_nib),4)
	end
	function extr_diff(code,value,nibble,diff)
c
c   Unpacks all differences contained in data longword value
c
	implicit integer(a-z)
c
 	integer*2 word(2)
 	byte byte(4)
	logical swp
 	equivalence (long,word,byte)
c
	integer diff(*),ldiff(7)
	data ldiff/30,15,10,8,6,5,4/
c
	form=mod(code,256)
	swp=code/256 .ne. byteorder()
	long=value
c	write(*,*) 'extr-diff',form,dnib
c	write(*,'(z8,1x,z8,1x,z8)') nibble,long,dnib
c
	if(nibble.eq.0) then
	  extr_diff=0
	elseif(nibble.eq.1) then
	  extr_diff=4
 	  do n=1,4
 	    diff(n)=byte(n)
 	  enddo
c	  call get_diff(long,ndiff,diff,8)
	elseif(nibble.eq.2) then
	  if(form.eq.11) then
	    if (swp) pause '**DECOMP_STEIM: BYTE SWAP UNIMPLEMENTED'
	    dnib=get_dnib(long)
	    if(dnib.eq.1) ndiff=1
	    if(dnib.eq.2) ndiff=2
	    if(dnib.eq.3) ndiff=3
	    if(dnib.eq.0) ndiff=5
	    call get_diff(long,ndiff,diff,ldiff(ndiff))
	  else
	    ndiff=2
	    do n=1,2
  	      diff(n)=i2val(swp,word(n))
	    enddo
	  endif
	  extr_diff=ndiff
	elseif(nibble.eq.3) then
	  if(form.eq.11) then
	    if (swp) pause '**DECOMP_STEIM: BYTE SWAP UNIMPLEMENTED'
	    dnib=get_dnib(long)
	    if(dnib.eq.0) ndiff=5
	    if(dnib.eq.1) ndiff=6
	    if(dnib.eq.2) ndiff=7
	    call get_diff(long,ndiff,diff,ldiff(ndiff))
	  else
	    ndiff=1
	    diff(1)=i4val(swp,long)
	  endif
	  extr_diff=ndiff
	else
	  write(*,*) '**EXTR_DIFF:  Bad nibble value ',nibble
	  extr_diff=0
	endif
c	write(*,'(z8,i4,7z8)') long,ndiff,(diff(i),i=1,ndiff)
c
	end
	function get_dnib(long)
	implicit integer (a-z)

c
	nibbles=long
	if(nibbles.lt.0) then
	  nib1=1
	else
	  nib1=0
	endif
	nibbles=nibbles*2
	if(nibbles.lt.0) then
	  nib2=1
	else
	  nib2=0
	endif
c
	get_dnib=nib1*2+nib2
c
	return
	end
	subroutine get_diff(long,ndiff,diff,ldiff)
c
	implicit integer (a-z)
	integer diff(*)
c
	shift=2**(32-ldiff)
c	write(*,'(1x,a,(1x,z8),2i6,2z10)') 'get_diff',long,ndiff,ldiff
	do n=1,ndiff
c	  rshift=2**((n-1)*ldiff)
c	  dummy=long/rshift
	  rshft=-(n-1)*ldiff
	  dummy=ishft(long,rshft)
c	  if(n.gt.1.and.long.lt.0.and..not.(ndiff.eq.3.and.n.eq.2.and.
c     &	  diff(3).eq.0)) dummy=dummy-1
c	  dummy1=dummy*shift
	  lshft=32-ldiff
	  dummy1=ishft(dummy,lshft)
          diff(ndiff-n+1)=dummy1/shift
c	write(*,'(i2,3(1x,z8),i8)') ndiff-n+1,dummy,dummy1,
c     &	diff(ndiff-n+1),diff(ndiff-n+1)
c	write(*,'(i2,1(1x,z8),i8)') ndiff-n+1,diff(ndiff-n+1),diff(ndiff-n+1)
 	enddo
c
	return
	end
	subroutine getsbe(ibuf,ifst,ilst)
C       GETSBE -- Get first and last data words in a Steim block
C
C       Assumes:
C          ibuf - header in native order Steim block in 4-byte words, data
C                 possibly swapped
C
C       Returns:
C          ifst - first sample value in block
C          ilst - last sample value in block
C
C       Routine assumes that all frames are in use.
        integer ibuf(*)
	integer*2 ihw(2)
	character iby(4)
	logical swp
	equivalence (iwd,iby,ihw)

	iwd=ibuf(1+52/4)
	nbt=2**ichar(iby(3))
	inw=nbt/4
	iwd=ibuf(1+44/4)
	ibd=ihw(1)
	nframe=(nbt-ibd)/64
	ix=1+ibd/4
	iwd=ibuf(ix)
	swp=iwd/4**13 .ne. 0
	ifst=i4val(swp,ibuf(ix+1))
	do i=1,nframe
	  ioff=16*(nframe-i)
	  iscw=i4val(swp,ibuf(ix+ioff))
	  if (iscw.ne.0 .and. iscw/4**13.eq.0) then
	    ilst=i4val(swp,ibuf(ix+2+ioff))
	    return
	  endif
        enddo
	write(*,*) '**GETSBE warning:  No frames found in use!'
	end
	function def_fixhead(num,stat,chan,net_id,dt,begdat,istim,
     &    ttsec,timcor,nsamp,form_key,reclen,fixhead)
c
c  Fills all information in fixed section of SEED data header
c
	implicit integer(a-z)
	integer fixhead(*),ibuf(16)
	integer*2 i2yr,i2dy,i2ns,i2fs,i2ms,i2bd,i2ts,i2c1,i2c48,i2c1000
 	logical swp
	real*4 dt
	character stat*(*),chan*(*),net_id*(*),buffer*64
c	,fixhead*(*)
c
c	structure /s_fixhead/
	  character*6 fixhead_seqnum
	  character*1 fixhead_indd
	  character*1 fixhead_resb1
	  character*5 fixhead_stat
	  character*2 fixhead_loca
	  character*3 fixhead_chan
	  character*2 fixhead_net_id
	  integer*2   fixhead_year
	  integer*2   fixhead_doy
	  byte   fixhead_hour
	  byte   fixhead_min
          byte   fixhead_sec
	  byte   fixhead_resb3
          integer*2   fixhead_ttsec
	  integer*2   fixhead_nsamp
	  integer*2   fixhead_fsamp
	  integer*2   fixhead_msamp
	  byte   fixhead_actfl
	  byte   fixhead_iofl
	  byte   fixhead_qualfl
	  byte   fixhead_numblk
	  integer*4   fixhead_timcor
	  integer*2   fixhead_begdat
	  integer*2   fixhead_begblk
	  integer*2   fixhead_blocktype1
	  integer*2   fixhead_nextblock1
	  byte   fixhead_encform
	  byte   fixhead_wordord
	  byte   fixhead_reclen
	  byte   fixhead_resb4
c	endstructure
	equivalence
     &	  (buffer(1:6),fixhead_seqnum),
     &	  (buffer(7:7),fixhead_indd),
     &	  (buffer(8:8),fixhead_resb1),
     &	  (buffer(9:13),fixhead_stat),
     &	  (buffer(14:15),fixhead_loca),
     &	  (buffer(16:18),fixhead_chan),
     &	  (buffer(19:20),fixhead_net_id),
     &	  (buffer(21:22),fixhead_year),
     &	  (buffer(23:24),fixhead_doy),
     &	  (buffer(25:25),fixhead_hour),
     &	  (buffer(26:26),fixhead_min),
     &	  (buffer(27:27),fixhead_sec),
     &	  (buffer(28:28),fixhead_resb3),
     &	  (buffer(29:30),fixhead_ttsec)
	equivalence
     &	  (buffer(31:32),fixhead_nsamp),
     &	  (buffer(33:34),fixhead_fsamp),
     &	  (buffer(35:36),fixhead_msamp),
     &	  (buffer(37:37),fixhead_actfl),
     &	  (buffer(38:38),fixhead_iofl),
     &	  (buffer(39:39),fixhead_qualfl),
     &	  (buffer(40:40),fixhead_numblk),
     &	  (buffer(41:44),fixhead_timcor),
     &	  (buffer(45:46),fixhead_begdat),
     &	  (buffer(47:48),fixhead_begblk),
     &	  (buffer(49:50),fixhead_blocktype1),
     &	  (buffer(51:52),fixhead_nextblock1),
     &	  (buffer(53:53),fixhead_encform),
     &	  (buffer(54:54),fixhead_wordord),
     &	  (buffer(55:55),fixhead_reclen),
     &	  (buffer(56:56),fixhead_resb4)
c	record /s_fixhead/ fixhead
	equivalence (buffer,ibuf)
	data i2c1/1/, i2c48/48/, i2c1000/1000/
c       swp = byteorder() .eq. 0
        swp = .false.
c
c	buffer=fixhead
c	write(*,*) 'def',num,stat,chan,net_id,dt,begdat,istim,ttsec,
c     &	timcor,nsamp,form_key
	do i=1,16
	  ibuf(i)=fixhead(i)
	enddo
c
	def_fixhead=0
c
	if(dt.gt.1.000001) then
	  fsamp=-(dt+0.1)
	  msamp=1
	elseif(dt.gt.0.00001) then
	  fsamp=1./dt+0.1
	  msamp=1
	else
	  fsamp=0
	  msamp=0
	endif
	call abstim(0,istim,iyear,iday,ih,im,is)
	i2yr=iyear
	i2dy=iday
	i2ns=nsamp
	i2fs=fsamp
	i2ms=msamp
	i2bd=begdat
	i2ts=ttsec
c	if(DEC) then
c	  call i4swap(1,timcor,tmcr)
c	  fixhead_year=iswap(fixhead_year)
c	  fixhead_doy=iswap(fixhead_doy)
c	  fixhead_ttsec=iswap(ttsec)
c	  fixhead_nsamp=iswap(fixhead_nsamp)
c	  fixhead_fsamp=iswap(fixhead_fsamp)
c	  fixhead_msamp=iswap(fixhead_msamp)
c	  fixhead_begdat=iswap(fixhead_begdat)
c	  fixhead_begblk=iswap(fixhead_begblk)
c	else
c	  tmcr=timcor
c	  fixhead_year=iyear
c	  fixhead_doy=iday
c	  fixhead_nsamp=nsamp
c	  fixhead_fsamp=fsamp
c	  fixhead_msamp=msamp
c	  fixhead_begdat=begdat
c	  fixhead_begblk=0
c	endif
c
	fixhead_resb1=' '
c	fixhead_resb2=' '
	fixhead_resb3=0
	write(fixhead_seqnum,'(i6.6)') num
	fixhead_indd='D'
	istat=str_upcase(stat,stat)
	fixhead_stat=stat
	fixhead_loca=' '
	istat=str_upcase(chan,chan)
	fixhead_chan=chan
	fixhead_net_id=net_id
	fixhead_year=i2val(swp,i2yr)
	fixhead_doy=i2val(swp,i2dy)
	fixhead_hour=ih
	fixhead_min=im
	fixhead_sec=is
	fixhead_ttsec=i2val(swp,i2ts)
	fixhead_nsamp=i2val(swp,i2ns)
	fixhead_fsamp=i2val(swp,i2fs)
	fixhead_msamp=i2val(swp,i2ms)
	fixhead_actfl=0
	fixhead_iofl=0
	fixhead_qualfl=0
c	fixhead_numblk=0
c	fixhead_timcor=tmcr
	fixhead_timcor=i4val(swp,timcor)
	fixhead_begdat=i2val(swp,i2bd)
	fixhead_begblk=i2val(swp,i2c48)
	fixhead_encform=mod(form_key,256)
	fixhead_reclen=reclen
c
c	write(*,*) 'def',num,istim,nsamp,fixhead_numblk
c       if(fixhead_numblk.eq.0.or.buffer(40:40).eq.' ') then
	  fixhead_numblk=i2val(swp,i2c1)
	  fixhead_blocktype1=i2val(swp,i2c1000)
	  fixhead_nextblock1=0
c         fixhead_wordord=1
	  fixhead_wordord=byteorder()
	  fixhead_resb4=0
c       endif
c
	do i=1,16
	   fixhead(i)=ibuf(i)
	enddo

c
	return
	end
	function def_fixhead_ful(num,stat,chan,net_id,dt,begdat,istim,
     &	ttsec,timcor,nsamp,form_key,reclen,timq,mysec,nfrm,fixhead)
c
c  Fills all information in fixed section of SEED data header
c
	implicit integer(a-z)
	integer fixhead(*),ibuf(16)
	integer*2 i2yr,i2dy,i2ns,i2fs,i2ms,i2bd,i2ts,i2c1,i2c48,i2c1000
 	logical swp
	real*4 dt
	character stat*(*),chan*(*),net_id*(*),buffer*64
c
c	structure /s_fixhead/
	  character*6 fixhead_seqnum
	  character*1 fixhead_indd
	  character*1 fixhead_resb1
	  character*5 fixhead_stat
	  character*2 fixhead_loca
	  character*3 fixhead_chan
	  character*2 fixhead_net_id
	  integer*2   fixhead_year
	  integer*2   fixhead_doy
	  byte   fixhead_hour
	  byte   fixhead_min
          byte   fixhead_sec
	  byte   fixhead_resb3
          integer*2   fixhead_ttsec
	  integer*2   fixhead_nsamp
	  integer*2   fixhead_fsamp
	  integer*2   fixhead_msamp
	  byte   fixhead_actfl
	  byte   fixhead_iofl
	  byte   fixhead_qualfl
	  byte   fixhead_numblk
	  integer*4   fixhead_timcor
	  integer*2   fixhead_begdat
	  integer*2   fixhead_begblk
	  integer*2   fixhead_blocktype1
	  integer*2   fixhead_nextblock1
	  byte   fixhead_encform
	  byte   fixhead_wordord
	  byte   fixhead_reclen
	  byte   fixhead_resb4
	  integer*2   fixhead_blocktype2
	  integer*2   fixhead_nextblock2
	  byte   fixhead_timq
	  byte   fixhead_mysec
	  byte   fixhead_resb5
	  byte   fixhead_nfrm
cc	endstructure
	equivalence
     &	  (buffer(1:6),fixhead_seqnum),
     &	  (buffer(7:7),fixhead_indd),
     &	  (buffer(8:8),fixhead_resb1),
     &	  (buffer(9:13),fixhead_stat),
     &	  (buffer(14:15),fixhead_loca),
     &	  (buffer(16:18),fixhead_chan),
     &	  (buffer(19:20),fixhead_net_id),
     &	  (buffer(21:22),fixhead_year),
     &	  (buffer(23:24),fixhead_doy),
     &	  (buffer(25:25),fixhead_hour),
     &	  (buffer(26:26),fixhead_min),
     &	  (buffer(27:27),fixhead_sec),
     &	  (buffer(28:28),fixhead_resb3),
     &	  (buffer(29:30),fixhead_ttsec)
	equivalence
     &	  (buffer(31:32),fixhead_nsamp),
     &	  (buffer(33:34),fixhead_fsamp),
     &	  (buffer(35:36),fixhead_msamp),
     &	  (buffer(37:37),fixhead_actfl),
     &	  (buffer(38:38),fixhead_iofl),
     &	  (buffer(39:39),fixhead_qualfl),
     &	  (buffer(40:40),fixhead_numblk),
     &	  (buffer(41:44),fixhead_timcor),
     &	  (buffer(45:46),fixhead_begdat),
     &	  (buffer(47:48),fixhead_begblk),
     &	  (buffer(49:50),fixhead_blocktype1),
     &	  (buffer(51:52),fixhead_nextblock1),
     &	  (buffer(53:53),fixhead_encform),
     &	  (buffer(54:54),fixhead_wordord),
     &	  (buffer(55:55),fixhead_reclen),
     &	  (buffer(56:56),fixhead_resb4),
     &	  (buffer(57:58),fixhead_blocktype2),
     &	  (buffer(59:60),fixhead_nextblock2)
 	equivalence
     &	  (buffer(61:61),fixhead_timq),
     &	  (buffer(62:62),fixhead_mysec),
     &	  (buffer(63:63),fixhead_resb5),
     &	  (buffer(64:64),fixhead_nfrm)
c	record /s_fixhead/ fixhead
	equivalence (buffer,ibuf)
	data i2c1/1/, i2c48/48/, i2c1000/1000/
c       swp = byteorder() .eq. 0
        swp = .false.
c
c	buffer=fixhead
c	write(*,*) 'def',num,stat,chan,net_id,dt,begdat,istim,ttsec,
c     &	timcor,nsamp,form_key,timq,mysec,nfrm
	do i=1,16
	  ibuf(i)=fixhead(i)
	enddo
c
	def_fixhead_ful=0
c
	if(dt.gt.1.000001) then
	  fsamp=-(dt+0.1)
	  msamp=1
	elseif(dt.gt.0.00001) then
	  fsamp=1./dt+0.1
	  msamp=1
	else
	  fsamp=0
	  msamp=0
	endif
	call abstim(0,istim,iyear,iday,ih,im,is)
	i2yr=iyear
	i2dy=iday
	i2ns=nsamp
	i2fs=fsamp
	i2ms=msamp
	i2bd=begdat
	i2ts=ttsec
c	if(DEC) then
c	  call i4swap(1,timcor,tmcr)
c	  fixhead_year=iswap(fixhead_year)
c	  fixhead_doy=iswap(fixhead_doy)
c	  fixhead_ttsec=iswap(ttsec)
c	  fixhead_nsamp=iswap(fixhead_nsamp)
c	  fixhead_fsamp=iswap(fixhead_fsamp)
c	  fixhead_msamp=iswap(fixhead_msamp)
c	  fixhead_begdat=iswap(fixhead_begdat)
c	  fixhead_begblk=iswap(fixhead_begblk)
c	else
c	  tmcr=timcor
c	  fixhead_year=iyear
c	  fixhead_doy=iday
c	  fixhead_nsamp=nsamp
c	  fixhead_fsamp=fsamp
c	  fixhead_msamp=msamp
c	  fixhead_begdat=begdat
c	  fixhead_begblk=0
c	endif
c
	fixhead_resb1=' '
c	fixhead_resb2=' '
	fixhead_resb3=0
	write(fixhead_seqnum,'(i6.6)') num
	fixhead_indd='D'
	istat=str_upcase(stat,stat)
	fixhead_stat=stat
	fixhead_loca=' '
	istat=str_upcase(chan,chan)
	fixhead_chan=chan
	fixhead_net_id=net_id
	fixhead_year=i2val(swp,i2yr)
	fixhead_doy=i2val(swp,i2dy)
	fixhead_hour=ih
	fixhead_min=im
	fixhead_sec=is
	fixhead_ttsec=i2val(swp,i2ts)
	fixhead_nsamp=i2val(swp,i2ns)
	fixhead_fsamp=i2val(swp,i2fs)
	fixhead_msamp=i2val(swp,i2ms)
	fixhead_actfl=0
	fixhead_iofl=0
	fixhead_qualfl=0
c	fixhead_numblk=0
c	fixhead_timcor=tmcr
	fixhead_timcor=i4val(swp,timcor)
	fixhead_begdat=i2val(swp,i2bd)
	fixhead_begblk=i2val(swp,i2c48)
	fixhead_encform=mod(form_key,256)
	fixhead_reclen=reclen
	fixhead_timq=timq
	fixhead_mysec=mysec
	fixhead_nfrm=nfrm
c
c	write(*,*) 'def',num,istim,nsamp,fixhead_numblk
c       if(fixhead_numblk.eq.0.or.buffer(40:40).eq.' ') then
	  fixhead_numblk=i2val(swp,i2c1)
	  fixhead_blocktype1=i2val(swp,i2c1000)
	  fixhead_nextblock1=0
c	  fixhead_wordord=1
	  fixhead_wordord=mod(form_key/256,2)
	  fixhead_resb4=0
c       endif
c
c	fixhead(1:64)=buffer(1:64)
	do i=1,16
	   fixhead(i)=ibuf(i)
	enddo

c
	return
	end
	function wr_file(mode,unit,ind,start_tim,last_tim,nch,nrec,
     &    lenb,ibuf)
c
c  ind=0 mseed original
c  ind=1 mseed Steim1 (decomp)
c  ind=2 Pitsa Ascii
c  ind=3 Steim1 (no decomp)
c
	implicit integer(a-z)
	integer ibuf(*),jbuf(7000),last_val(300)
	character stat*5,chan*3,net_id*2
	real*4 rate
	real*8 start_tim,last_tim,st_tim,end_tim,nstart_tim,nlast_tim
c	common /wrf/jbuf,last_val
	common /inf_dat/iftim,iftsc,timcr,rate,nsamp,stat,chan,net_id
	data last_val/300*0/, end_tim/0.0/, tstart,tend/2*0/
c
	wr_file=0
c
	if(ind.eq.0) then
	  if(mode.ne.999) then
	    if (byteorder() .eq. 0) call swap_mseed(-1,ibuf)
	    write(unit,rec=nrec) (ibuf(i),i=1,lenb)
c	    write(*,*) nrec,'. record written'
	  endif
	  return
	endif
c
	if(mode.ne.999.and.ind.ne.3) then
	  tstart=start_tim
	  tend=last_tim
	  istat=dec_fixhead(num,stat,chan,net_id,rate,begdat,iftim,
     &      iftsc,timcor,nsamp,form,reclen,ibuf)
c	write(*,*) num,nch,nrec,stat,chan,net_id,rate,begdat,iftim,nsamp,
c     &	nosmp,form,reclen
	  st_tim=dble(iftim)+dble(iftsc+timcor)*0.0001
	  end_tim=st_tim+dble(nsamp-1)/dble(rate)
	  iof=begdat/4+1
	  nbt=lenb*4
	  nframe=(nbt-begdat)/64
	  idchk=decomp_steim(form,1,nrec,nframe,nsamp,ibuf(iof),
     &	  nosmp,jbuf,last_val(nch))
c	write(*,*) 'decom',idchk,nosmp,last_val(nch)
	elseif(mode.ne.999.and.ind.eq.3) then
	  do i=1,nsamp
	    jbuf(i)=ibuf(i)
	  enddo
	  nosmp=nsamp
	  st_tim=dble(iftim)+dble(iftsc+timcor)*0.0001
	  end_tim=st_tim+dble(nsamp-1)/dble(rate)
	endif
c
	if(ind.eq.1.or.ind.eq.3) then
c	if(mode.eq.999) nosmp=0
c	write(*,*) 'wr_mseed',mode,unit,nch,nosmp,nrec,chan
	  call wr_mseed(mode,unit,nch,nosmp,jbuf,nrec,stat,chan,
     &	  net_id,rate,st_tim)
	endif
c
	if(ind.eq.2.and.mode.ne.999) then
	  istart=1
	  iosmp=nosmp
	  if((st_tim.le.tstart .and. end_tim.gt.tstart) .or.
     &       (nrec.eq.1 .and. st_tim.gt.tstart)) then
	    istart=dble(rate)*(dble(tstart)-st_tim)+1
	    if(istart.lt.1) istart=1
	    nstart_tim=st_tim+(dble(istart-1)/dble(rate))
	    ntot=0
c
	    call tfix(nstart_tim,iatim,iatsc)
	    call abstim(0,iatim,iyra,idya,iha,ima,isa)
	    call datum(0,idya,iyra,mona,itga)
	    write(unit,'(a)') '#SAMP_FREQ'
	    write(unit,*) rate
	    write(unit,'(a)') '#START_TIME'
	    write(unit,'(i4.4,5(1x,i2.2),1h.,i4.4)')
     &        iyra,mona,itga,iha,ima,isa,iatsc
	    write(unit,'(a)') '#STATION_CODE'
	    write(unit,'(a)') stat
	    write(unit,'(a)') '#STATION_CHANNEL'
	    write(unit,'(a)') chan
c
	  endif
	  if(st_tim.le.tend.and.end_tim.ge.tend) then
	    iosmp=dble(rate)*(dble(tend)-st_tim)
	    nlast_tim=st_tim+(dble(nosmp)/dble(rate))
	  endif
c	write(*,*) istart,iosmp,nosmp,st_tim,end_tim,tstart,tend,nstart_tim,
c     &	nlast_tim
c
	  n=0
	  do i=istart,iosmp
	    n=n+1
	    write(unit,*) jbuf(i)
	  enddo
	  ntot=ntot+n
	endif
c
	return
	end
	subroutine wr_mseed(wrmod,unit,nch,nosmp,jbuf,nrec,
     &                      stat,chan,net_id,rate,st_tim)
c
	implicit integer(a-z)
	integer ibuf(4000,20),jbuf(*),numo(20),ndat(20),mdat(20),
     &  totsmp(20),totsmp_1(20),itnext(20)
	character stat*5,chan*3,net_id*2
	real*4 rate,diff
	real*8 st_tim,stim_in(20),stim_out(20),stim_st(20),
     &    stim_in_next(20),stim_out_next(20)
c	common /wrs/ibuf,numo,ndat,mdat,totsmp,totsmp_1,itnext,stim_in,stim_out,
c     &	stim_st,stim_in_next,stim_out_next
c
	if(nrec.eq.1) then
	  mode=0
	  numo(nch)=0
	  stim_st(nch)=0.0
	  stim_out(nch)=0.0
	else
	  if(wrmod.ne.999.and.dabs(st_tim-stim_in_next(nch)) .gt.
     &      0.5/rate) then
	    call tfix(st_tim,iftim,iftsc)
	    call abstim(0,iftim,iyra,idya,iha,ima,isa)
	    call datum(0,idya,iyra,mona,itga)
	    call tfix(stim_in_next(nch),intim,intsc)
	    call abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	    call datum(0,idyn,iyrn,monn,itgn)
	    write(*,9) 
     &	    'Time-gap in input file: ',itga,mona,iyra,iha,ima,isa,iftsc,
     &	    'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc
9	format(2(1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4))
c
	    icmp=compress_steim(999,nch,nrec,0,jbuf,
     &	    16,1024,ndat(nch),mdat(nch),ist_out,ibuf(1,nch),totsmp(nch),
     &	    totsmp_1(nch))
	    numo(nch)=numo(nch)+1
	    call tfix(stim_st(nch),iatim,iatsc)
	    istat=def_fixhead(numo(nch),stat,chan,net_id,1./rate,64,
     &	    iatim,iatsc,0,mdat(nch),10,12,ibuf(1,nch))
c	write(*,*) 'write999',numo(nch),stim_st(nch),ndat(nch),mdat(nch)
	    if (byteorder() .eq. 0) call swap_mseed(-1,ibuf(1,nch))

	    write(unit,rec=numo(nch)) (ibuf(i,nch),i=1,1024)
c	    write(*,*) numo(nch),'. record written - 999'
	    stim_out_next(nch)=st_tim
	    stim_st(nch)=st_tim
	    mode=0
	  endif
	endif
	stim_in_next(nch)=st_tim+dble(nosmp)/dble(rate)
c
	if(wrmod.ne.999) then
	  nwr=2
	  if(nosmp.lt.100) nwr=1
	else
	  nwr=1
	  mode=999
	  nosmp=0
	endif
c
	do n=1,nwr
	  if(n.eq.1) then
	    ia=1
	    isamp=nosmp/2
	    if(nwr.eq.1) isamp=nosmp
	  else
	    ia=nosmp/2+1
	    isamp=nosmp-isamp
	  endif
c
	  nrec=nrec+1
	  stim_in(nch)=st_tim+dble((ia-1))/dble(rate)
c	write(*,*) 'cmpr',n,nch,nrec,nosmp,isamp,ndat(nch),mdat(nch),ia,ist_out
	  icmp=compress_steim(mode,nch,nrec,isamp,jbuf(ia),
     &	  16,1024,ndat(nch),mdat(nch),ist_out,ibuf(1,nch),totsmp(nch),
     &	  totsmp_1(nch))
	  mode=2
	  if(stim_st(nch).lt.0.1) stim_st(nch)=stim_out(nch)
	  stim_out(nch)=stim_in(nch)+dble(ist_out)/dble(rate)
	  if(itnext(nch).eq.1) then
	    stim_st(nch)=stim_out(nch)
c	    write(*,*) 'correct stim_st',nrec,numo(nch),ist_out,
c     &	    stim_st(nch)
	  endif
c	write(*,*) 'compress cps:',n,icmp,mode,nrec,nosmp,isamp,ndat(nch),
c     &	mdat(nch),stim_in(nch),ist_out,stim_out(nch),totsmp(nch),
c     &	totsmp_1(nch)
c	write(*,'(8(1x,z8))') (ibuf(i),i=1,128)
c
	  itnext(nch)=0
	  if(icmp.eq.1.or.icmp.eq.999) then
	    numo(nch)=numo(nch)+1
	    call tfix(stim_st(nch),iatim,iatsc)
	    istat=def_fixhead(numo(nch),stat,chan,net_id,1./rate,64,
     &	    iatim,iatsc,0,mdat(nch),10,12,ibuf(1,nch))
c	write(*,*) 'write',numo(nch),stim_st(nch),ndat(nch),mdat(nch)
	    if (byteorder() .eq. 0) call swap_mseed(-1,ibuf(1,nch))
	    write(unit,rec=numo(nch)) (ibuf(i,nch),i=1,1024)
c	    write(*,*) numo(nch),'. record written'
	    stim_out_next(nch)=stim_st(nch)+dble(mdat(nch))/dble(rate)
	    stim_st(nch)=stim_out(nch)
	    if(icmp.eq.1.and.dabs(stim_st(nch)-stim_out_next(nch))
     &	    .gt.0.5/dble(rate)) then
	      diff=dabs(stim_st(nch)-stim_out_next(nch))
c	write(*,*) ndat(nch),stim_out_next(nch),stim_st(nch),diff
	      if(ndat(nch).eq.1024.and.diff.gt.10./rate) then
	        itnext(nch)=1
	      else
	        write(*,*) 'Timing problem occured while compressing',
     &            nrec,nch,numo(nch),ndat(nch),stim_out_next(nch),
     &            stim_st(nch),diff
	      endif
	    endif
	  endif
	enddo
c
	return
	end
	function compress_steim(cmp_mode,no_chan,nrec,ndat_in,
     &	in_buf,len_head,len_buf,ndat_out,mdat_out,ist_out,out_buf,
     &	totsmp,totsmp_1)
c
c   Compresses integer type data in Steim compression frames
c
	implicit integer(a-z)
	integer in_buf(*),out_buf(*)
	logical word_dif(300),long_dif(300)
	integer diff(4,300),first(300),last(300),ncount(300)
        save word_dif, long_dif, diff, first, last, ncount

	compress_steim=0
	if(ndat_out.ge.len_buf) then
	  if(cmp_mode.ne.999) then
	    mode=1
	    if(cmp_mode.eq.0) mode=0
	  else
	    mode=999
	    mod999=1
	  endif
	else
	  mode=cmp_mode
	  if(mode.eq.1) mode=2
	  if(mode.eq.999) mod999=2
	endif
c
	buf_count=0
	if(cmp_mode.eq.0) then
	  ncount(no_chan)=0
	  last(no_chan)=0
	  mdat_out=0
	if(no_chan.eq.1) nnnn=0
	endif
c
c	if(mode.eq.999) write(*,*) 'compress:',ndat_out,ndat_in,ncount(no_chan),
c     &	word_dif(no_chan),long_dif(no_chan)
c	write(*,*) 'compress:',cmp_mode,mode,buf_count,ndat_in,ncount(no_chan),
c     &	totsmp,totsmp_1
	do while (buf_count.lt.ndat_in)
	  ncount(no_chan)=ncount(no_chan)+1
	  buf_count=buf_count+1
	  if(dabs(dble(in_buf(buf_count))-dble(last(no_chan))) .gt.
     &         2.1e9) then
	    write(*,*) 'WRSEED warning: too large difference',no_chan,
     &	    buf_count,in_buf(buf_count),last(no_chan)
	    diff(ncount(no_chan),no_chan)=0
	  else
	    diff(ncount(no_chan),no_chan) =
     &        in_buf(buf_count)-last(no_chan)
	  endif
	  if(ncount(no_chan).eq.1) first(no_chan)=in_buf(buf_count)
	  last(no_chan)=in_buf(buf_count)
	  call diff_compare(
     &       ncount(no_chan),diff(ncount(no_chan),no_chan),
     &	     word_dif(no_chan),long_dif(no_chan)
     &    )
c	write(*,*) 'diff_comp',buf_count,ncount(no_chan),in_buf(buf_count),
c     &	last(no_chan),diff(ncount(no_chan),no_chan),cmp_mode,ndat_in
	  if(cmp_mode.eq.0.and.ndat_in.eq.1) then
	write(*,*) 'Single value start problem - correct to long word'
	    long_dif(no_chan)=.true.
	  endif
	  if(mode.ne.999) then
	    istat=diff_store(
     &        mode,no_chan,nrec,buf_count,
     &        ncount(no_chan),diff(1,no_chan),word_dif(no_chan),
     &        long_dif(no_chan),first(no_chan),last(no_chan),
     &        len_buf,len_head,ndat_out,mdat_out,
     &	      ist_out,out_buf,totsmp,totsmp_1)
	  else
	    istat=diff_store(
     &        mod999,no_chan,nrec,buf_count,
     &        ncount(no_chan),diff(1,no_chan),word_dif(no_chan),
     &        long_dif(no_chan),first(no_chan),last(no_chan),
     &        len_buf,len_head,ndat_out,mdat_out,
     &        ist_out,out_buf,totsmp,totsmp_1)
	  endif
	enddo
c	if(mode.eq.999) 
c     &	write(*,*) 'compr1:',istat,ndat_out,mdat_out,mode
c
	if(mode.eq.999.and.(ndat_in.eq.0.or.ndat_out.lt.len_buf)) then
c     &	.or.(ndat_out.eq.len_buf.and.ncount(no_chan).eq.0))) then
	  istat=diff_store(
     &      mode,no_chan,nrec,buf_count,
     &      ncount(no_chan),diff(1,no_chan),word_dif(no_chan),
     &      long_dif(no_chan),first(no_chan),last(no_chan),
     &      len_buf,len_head,ndat_out,mdat_out,
     &	    ist_out,out_buf,totsmp,totsmp_1)
	  if(ndat_out.lt.len_buf) then
	    do i=ndat_out+1,len_buf
	      out_buf(i)=0
	    enddo
	  endif
	  compress_steim=999
	endif
	if(ndat_out.ge.len_buf.and.mode.ne.999) then
	  if(totsmp.ne.totsmp_1) then
c	    write(*,*) 'Warning: not all data flushed',nrec,
c     &	    totsmp,totsmp_1
	  endif
	  compress_steim=1
c	write(*,*) 'compress_steim=1'
	endif
c
c	cmp_mode=mode
c
	return
	end
	function diff_store(mode,no_chan,nrec,nct_in,ncount,diff,
     &	  word_dif,long_dif,first,last,len_buf,len_head,ndat_out,
     &    mdat_out,ist_out,out_buf,totsmp,totsmp_1)
c
c   Stores given differences either in longword, word or bytes
c
	implicit integer (a-z)
	integer diff(*),out_buf(*)
	logical word_dif,long_dif,ndswap
	integer*2 work_word(2)
	byte work_byte(4)
	equivalence (work_long,work_word,work_byte)
	data nerr/0/, ndswap/.false./
c
c       ndswap = byteorder() .eq. 0
	istat=0
c
	if(mode.eq.999) then
c	write(*,*) 'diff_store 999:',ncount,word_dif,long_dif
	  if(ncount.eq.0) then
	    long_dif=.false.
	    word_dif=.false.
	  endif
	  if(ncount.eq.1.or.word_dif) long_dif=.true.
	  if(ncount.eq.2.and..not.long_dif) word_dif=.true.
c	  if(ncount.eq.1) long_dif=.true.
c	  if(ncount.eq.2) word_dif=.true.
	  if(ncount.eq.3) work_byte(4)=0
	else
	  totsmp=totsmp+1
	  if(totsmp.eq.1) nerr=0
	endif

c	write(*,*) 'diff_store',mode,ncount,diff(ncount),long_dif,word_dif,first,
c     &	last,totsmp,totsmp_1

	if(long_dif .or.
     &     (word_dif .and. ncount.eq.2) .or.
     &     ncount.eq.4 .or.
     &     mode.eq.999) then
	  if(.not.word_dif.and..not.long_dif) then
	    if(ncount.ne.4.and.mode.ne.999)
     &        write(*,*) 'diff_store error 1:',ncount
	    if(ncount.gt.0) then
	      do n=1,ncount
c	        if(iabs(diff(n)).gt.127) then
c	          write(*,*) 'diff_store error: ',n,ncount,(diff(i),i=1,ncount)
c     &	          ,mode,word_dif,long_dif
c	          long_dif=.true.
c	        endif
                if (ndswap) then
	          work_byte(5-n)=diff(n)
                else
		  work_byte(n)=diff(n)
	        endif
	      enddo
	    endif
	    istat=fill_frame(mode,no_chan,nrec,nct_in,ncount,work_long,
     &	    '01',len_buf,len_head,first,last,ndat_out,mdat_out,
     &	    ist_out,out_buf,totsmp,totsmp_1)
	  elseif(word_dif.and..not.long_dif) then
	    if(mod(ncount,2).ne.0.or.ncount.eq.0) write(*,*)
     &	    'diff_store error 2:',ncount
	    nnn=0
	    dummy=first-diff(1)
	    if(ncount/2.gt.0) then
	      do n=1,ncount/2
	        do nn=1,2
	          nnn=nnn+1
                  if (ndswap) then
	            work_word(3-nn)=diff(nnn)
                  else
	            work_word(nn)=diff(nnn)
                  endif
	          dummy=dummy+diff(nnn)
	          if(nn.eq.1) first_st=dummy
	          if(nn.eq.2) last_st=dummy
	        enddo
	        istat=fill_frame(mode,no_chan,nrec,nct_in,2,work_long,
     &	        '10',len_buf,len_head,first_st,last_st,ndat_out,
     &          mdat_out,ist_out,out_buf,totsmp,totsmp_1)
	      enddo
	    endif
	  elseif(long_dif) then
	    na=1
	    mod1=1
	    dummy=first-diff(1)
	    if(ncount.gt.2) then
	      do nn=1,2
	        work_word(nn)=diff(nn)
	        dummy=dummy+diff(nn)
	        if(nn.eq.1) first_st=dummy
	        if(nn.eq.2) last_st=dummy
	      enddo
	      istat=fill_frame(mod1,no_chan,nrec,nct_in,2,work_long,
     &	      '10',len_buf,len_head,first_st,last_st,ndat_out,mdat_out,
     &	      ist_out,out_buf,totsmp,totsmp_1)
	      na=3
	    endif
	    if(na.le.ncount) then
	      do n=na,ncount
	        dummy=dummy+diff(n)
	        first_st=dummy
	        last_st=dummy
	        istat=fill_frame(mode,no_chan,nrec,nct_in,1,diff(n),'11',
     &	        len_buf,len_head,first_st,last_st,ndat_out,mdat_out,
     &	        ist_out,out_buf,totsmp,totsmp_1)
	      enddo
	    endif
	  endif
	  if(totsmp.ne.totsmp_1) then
	    nerr=nerr+1
	    if(nerr.lt.20)
     &	    write(*,*) 'totsmp error:',totsmp,totsmp_1,ncount,long_dif,
     &	    word_dif
	  endif
	  ncount=0
	endif
c
	diff_store=istat
c	write(*,*) 'diff_store end:',diff_store,ndat_out,mdat_out,totsmp,totsmp_1
c
	return
	end
	function fill_frame(mode,no_chan,irec,nct_in,ncount,work_long,
     &	nibble,len_buf,len_head,first_date,last_date,nlong,nsamp_out,
     &	ist_out,buffer,totsmp,totsmp_1)
c
c   Fills Steim frames with difference data values
c
c       Identifier usage:
c         fcount - 32-bit word index in current frame (0<=fcount<=16)

	implicit integer(a-z)
	integer buffer(*)
	integer fcount(300),frame(16,300),new_record(300)
        integer nframe(300),nibbles(300),first_value(300),nsamp(300)
        integer nsmp_fr(300)
	integer first_vlfr(300),first_vlst(300),last_vlfr(300)
	character nibble*2
	save fcount,frame,nframe,nibbles,nsamp,nsmp_fr
c	common /ffr/frame,new_record,fcount,nframe,nibbles,first_value,
c     &	nsamp,nsmp_fr,first_vlfr,last_vlfr
c	data ntr_max/300/
	data nrec/0/, nover/0/, nfr_max/0/
c
c       ndurch=ndurch+1
c	if(nrec.gt.24700.and.no_chan.eq.4)
c     &	write(*,*) 'last,first',mode,first_date,last_date,ncount,no_chan,ndurch,
c     &	nibble,work_long
c	if(lstfrst.eq.first_date) then
c	  write(*,*) 'Warning: first date ident.',ndurch,first_date,ncount,nibble
c	endif
c	if(lstlst.eq.last_date) then
c	  write(*,*) 'Warning: last date ident.',ndurch,last_date,ncount,nibble
c	endif
c	if(mode.ne.2) write(*,*) 'fill st',mode,ndurch
c	write(*,*) 'fill_frame',mode,ncount,work_long,nibble,nlong,nsamp_out,totsmp_1
	if(mode.eq.0) then
c	write(*,*) 'fill_frame 0:',no_chan,irec,nlong,ncount,nover,ndurch
	  nover=0
c         ndurch=0
	  new_record(no_chan)=1
 	  fcount(no_chan)=1
	  nframe(no_chan)=0
	  nibbles(no_chan)=0
	  nsmp_fr(no_chan)=0
	  nfr_max=(len_buf-len_head)/16
	endif
c
	fill_frame=0
	if(mode.eq.999) fill_frame=1
c
	if(new_record(no_chan).eq.1) then
	  new_record(no_chan)=2
c	  first_value(no_chan)=first_date
c	if(irec.eq.0)
c     &	write(*,*) 'new record',nrec+1,ndurch,nframe(no_chan),fcount(no_chan)
	  if(fcount(no_chan).le.3) then
	    fcount(no_chan)=3
	    nframe(no_chan)=0
	    first_value(no_chan)=first_date
c	    nibbles(no_chan)=0
	  else
	    nframe(no_chan)=nframe(no_chan)-nfr_max
	    first_value(no_chan)=first_vlst(no_chan)
c	write(*,*) 'irregular init',nrec+1,ndurch
	  endif
	  nsamp(no_chan)=0
	  ist_out=nct_in-totsmp+totsmp_1
c	write(*,*) 'fill',nct_in,ncount,totsmp,totsmp_1,ist_out
	endif
c
c	if(mode.eq.999) write(*,*) 'new_record',no_chan,new_record(no_chan)
	if(new_record(no_chan).eq.2 .and.
     &     (mode.eq.0 .or. mode.eq.1 .or. mode.eq.999)) then
c	  if(irec.eq.0)
c    &	    write(*,*) 'fill_frame 1',mode,nlong,nover,ndurch
	  if(mode.eq.0.or.mode.eq.1) mode=2
	  new_record(no_chan)=0
	  if(nlong.gt.0) nover=nlong-len_buf
	  nlong=len_head
	  nsamp_out=0
	  if(nover.gt.0) then
	    do i=1,nover
	      nlong=nlong+1
	      buffer(nlong)=buffer(len_buf+i)
	    enddo
	    if(nlong+16.gt.len_buf) then
	      fill_frame=1
c	if(irec.eq.0)
c     &	write(*,*) 'fill_frame 2:',nover,nsamp_out,nlong,fcount(no_chan),ndurch
	    endif
	  endif
	else
c	  if(nlong.ne.nlong_old) write(*,*) irec,ndurch,nlong,nlong_old,no_chan,
c     &	  ncount,fcount(no_chan)
	endif
c
	if(ncount.gt.0) then
	  fcount(no_chan)=fcount(no_chan)+1
	  frame(fcount(no_chan),no_chan)=work_long
	  if(nsmp_fr(no_chan).eq.0) then
c	if(nrec.gt.14)
c     &  write(*,*) 'firstvl set in frame',nframe(no_chan)+1,fcount(no_chan),
c     &	first_date,ndurch,lstfrst
	    first_vlfr(no_chan)=first_date
c	if(nframe(no_chan)+1.gt.nfr_max) write(*,*) 'Warning: max nframe exc',
c     &	ndurch
	  endif
	  call fill_nibbles(nibbles(no_chan),fcount(no_chan),nibble)
          nsmp_fr(no_chan)=nsmp_fr(no_chan)+ncount
          totsmp_1=totsmp_1+ncount
	endif
	if(fcount(no_chan).eq.16.or.fill_frame.eq.1) then
	  if(fcount(no_chan).eq.16.or.mode.eq.999) then
	    nframe(no_chan)=nframe(no_chan)+1
	    if(nframe(no_chan).eq.1) then
	      first_value(no_chan)=first_vlfr(no_chan)
c	if(nrec.gt.14)
c     &  write(*,*) 'firstvl set in record',nframe(no_chan),fcount(no_chan),
c     &	first_value(no_chan),ndurch
	    endif
	    if(nframe(no_chan).gt.nfr_max) then
	      write(*,*) 'Warning: too many frames generated in ',
     &          'compression scheme:',fill_frame,
     &          no_chan,nframe(no_chan),fcount(no_chan),ndurch
	      if(nframe(no_chan).eq.nfr_max)
     &          first_vlst(no_chan)=first_vlfr(no_chan)
	    endif
	    frame(1,no_chan)=nibbles(no_chan)
	    last_vlfr(no_chan)=last_date
c	if(nrec.gt.14)
c     &  write(*,*) 'lastvl set in record',nframe(no_chan),fcount(no_chan),
c     &	last_vlfr(no_chan),ndurch
	    nsamp(no_chan)=nsamp(no_chan)+nsmp_fr(no_chan)
c
	    if(mod(nlong,16).ne.0) then
	      write(*,*) 'Frame fill error: wrong start index',
     &	        fill_frame,nrec,nlong,nlong_old,no_chan,ndurch,
     &	        fcount(no_chan),nibbles(no_chan),
     &          (frame(n,no_chan),n=1,16)
	      write(*,'(10(1x,z8))')
     &          nibbles(no_chan),(frame(n,no_chan),n=1,16)
c             write(88) (buffer(i),i=1,nlong+16)
c	      stop 'fill error'
	      nlong=nlong-mod(nlong,16)
	      write(*,*) 'nlong corrected to',nlong
	    endif
	    do n=1,fcount(no_chan)
	      nlong=nlong+1
	      buffer(nlong)=frame(n,no_chan)
	    enddo
	    if(mod(nlong,16).ne.0.and.mode.ne.999) then
	      write(*,*) 'Frame fill error 0: wrong end index',
     &	        fill_frame,nrec,nlong,nlong_old,no_chan,ndurch,
     &	        fcount(no_chan),nibbles(no_chan),
     &          (frame(n,no_chan),n=1,16)
	      write(*,'(10(1x,z8))')
     &          nibbles(no_chan),(frame(n,no_chan),n=1,16)
c             write(88) (buffer(i),i=1,nlong)
	      stop 'fill error'
	    endif
	    if(nlong.gt.2048) then
	      write(*,*) 'nlong warning: array overflow',
     &          no_chan,nlong,ndurch
	    endif
c
	    fcount(no_chan)=1
	    if(nframe(no_chan).eq.nfr_max) then
	      fcount(no_chan)=3
c	write(*,*) 'Init first frame as no',nframe(no_chan)
	    endif
	    nibbles(no_chan)=0
	    nsmp_fr(no_chan)=0
c	if(nrec.gt.24700.and.no_chan.eq.4)
c     &	write(*,*) 'write frame:',fill_frame,nframe(no_chan),nlong+1,fcount
c     &	(no_chan),ncount,nibbles(no_chan),first_vlfr(no_chan),
c     &	last_vlfr(no_chan),ndurch
	  endif
	  if((nlong.le.len_buf.and.nlong+16.gt.len_buf) .or.
     &       fill_frame.eq.1) then
c	    fill_frame=1
	    buffer(len_head+2)=first_value(no_chan)
	    last_value=last_vlfr(no_chan)
	    buffer(len_head+3)=last_value
	    new_record(no_chan)=1
	    nsamp_out=nsamp(no_chan)
	    nrec=nrec+1
c	if(mode.eq.999) write(*,*) 'fill_frame:',nsamp_out,nlong,fcount(no_chan)
c	write(*,*) 'fill_frame 3:',nsamp_out,nlong,fcount(no_chan),mode,ndurch
c	write(*,*) 'first,last',nrec,first_value(no_chan),last_value,nsamp_out
	  endif
	endif
	nlong_old=nlong
c	if(nrec.gt.24700.and.no_chan.eq.4)
c     &	write(*,*) 'last',nrec,ndurch,nlong,no_chan,fcount(no_chan),
c     &	nibbles(no_chan),nframe(no_chan),frame(fcount(no_chan),no_chan),
c     &	fill_frame
c	lstfrst=first_date
c	lstlst=last_date
c
	return
	end
	subroutine fill_nibbles(nibbles,fcount,nibble)
c
c   Fills Steim 'nibbles' in first word in a compression frame
c
	implicit integer(a-z)
	character nibble*2
c
	if(fcount.gt.16) return
	if(fcount.lt.3) nibbles=0
c
	do n=1,2
	  nn=fcount*2
	  if(n.eq.1) nn=nn-1
	  if(nibble(n:n).eq.'1') nibbles=ipow(nibbles,32-nn)
	enddo
	end
	function ipow(int,nset)
c
	ipow=int+2**(nset)
c
	return
	end
	function nssamp(cword)
C       NSSAMP -- Return number of samples in a Steim frame given control
C                 word.
C       Assumes:
C          cword - 32 bit control word
C       Returns:
C          function result - number of samples that control word
C             represents.
C
        integer cword, cequiv
	character cchar(4)
	equivalence (cchar, cequiv)

	cequiv = cword
        n = 0
	do i=0,15
	   nib = mod(ichar(cchar(1+i/4))/4**(3-mod(i,4)),4)
	   if (nib.gt.0) n = n + 2**(3-nib)
	enddo
	nssamp = n
	end
	subroutine diff_compare(ncount,diff,word_dif,long_dif)
c
c   Checks for optimum data type for given difference value
c
	implicit integer(a-z)
	logical word_dif,long_dif
c
	if(ncount.eq.1) then
	  word_dif=.false.
	  long_dif=.false.
	endif
	if(iabs(diff).gt.127) then
	  word_dif=.true.
	  if(iabs(diff).gt.32767) then
	    word_dif=.false.
	    long_dif=.true.
	  endif
	endif
c
	return
	end
	function merge_seed(idsc,idsc1,idsc2,seed_lst,seed_out,ikeep)
c
	character buffer*4096
        character seed_shd*132,seed_lst*132,seed_in*132,seed_out*132
        logical lexi
c
	seed_shd=seed_lst(1:index(seed_lst,'.LST'))//'SHD'
        open(idsc,file=seed_shd,status='old',iostat=merge_seed)
        if (merge_seed .ne. 0) return
c
	write(*,*) 'merge_seed started, output file is ',
     &    seed_out(1:lenc(seed_out))
	open(idsc1,file=seed_out,status='unknown',form='unformatted',
     &	access='direct',recl=4096)
c     &	access='direct',recl=1024)
	num=0
	ifiles=0
	ihead=0
c
1000	read(idsc,'(a)',iostat=ios) seed_in
	if(ios.ne.0) then
	  if(ihead.eq.0) then
	    if(ikeep.eq.0) then
	      close(idsc,status='delete')
	    else
	      close(idsc)
	    endif
	    open(idsc,file=seed_lst,status='old')
	    ihead=1
	    read(idsc,'(a)',err=999,end=999) seed_in
	  else
	    goto 999
	  endif
	endif
c	inquire(file=seed_in,opened=lopen)
c	lenr=nbtin/4
c	nrep=1024/lenr
	nbtin=4096
c	lenr=1024
	lenr=4096
	nrep=1
	nfl=lenc(seed_in)
	open(idsc2,file=seed_in(1:nfl),status='old',form='unformatted',
     &	access='direct',recl=lenr)
	nfl=lenc(seed_in)
	write(*,'(1x,2a)') 'Opened next file: ',seed_in(1:nfl)
	    idel=1
	    idx=index(seed_in,'seed_header')-1
	    if(idx.ge.0) then
	      if(seed_in(1:idx).ne.seed_lst(1:idx)) idel=0
c	      write(*,*) seed_in(1:idx),'/',seed_lst(1:idx),':',idel
	    endif
	nrec=0
c
100	nrec=nrec+1
	do n=1,nrep
	  ia=(n-1)*nbtin+1
	  ie=n*nbtin
	  read(idsc2,rec=nrec,iostat=ios) buffer(ia:ie)
	  if(ios.ne.0) goto 900
c	  if(buffer(7:7).eq.'D') then
c     &	  istat=dec_fixhead(num1,stat,chan,net_id,rate,begdat,iftim,iftsc,
c     &	  timcor,nsamp,buffer)
c	  call abstim(0,iftim,iyra,idya,iha,ima,isa)
c	  call datum(0,idya,iyra,mona,itga)
c	  if(n.eq.1)
c     &	  write(*,*) num1,' ',stat,chan,rate,itga,mona,iyra,iha,ima,isa,iftsc,
c     &	  timcor,nsamp
	enddo
	num=num+1
	write(buffer(1:6),'(i6.6)') num
	write(idsc1,rec=num) buffer
	goto 100
c	
900	if(ikeep.eq.0.and.idel.eq.1) then
	  close(idsc2,status='delete')
	  write(*,'(1x,2a)') 'Delete file:      ',seed_in(1:nfl)
	else
	  close(idsc2)
	  write(*,'(1x,2a)') 'Closed file:      ',seed_in(1:nfl)
	endif
	goto 1000
c
999	if(ikeep.eq.0) then
	  close(idsc,status='delete')
	else
	  close(idsc)
	endif
	close(idsc1)
	end
	subroutine swap_mseed(ind,ibuf)
c
c  ind= 1 - swap for further processing:  header to native, data left alone
c  ind= 2 - swap for pql display
c  ind= 3 - swap data only (e.g. Reftek data records)
c  ind=-1 - swap back to big-endian order:  header to big-endian, data as req.
c
	implicit integer(a-z)
	integer ibuf(*),ihead(16)
	integer*2 idum(2),i2c64
	logical swp
	character buffer*64
	equivalence(buffer,ihead),(dummy,idum)
c
	  character*6 seqnum
	  character*1 indd
	  character*1 resb1
	  character*5 stat
	  character*2 loca
	  character*3 chan
	  character*2 net_id
	  integer*2   year
	  integer*2   doy
	  byte   hour
	  byte   min
          byte   sec
	  byte   resb3
          integer*2   ttsec
	  integer*2   nsamp
	  integer*2   fsamp
	  integer*2   msamp
	  byte   actfl
	  byte   iofl
	  byte   qualfl
	  byte   numblk
	  integer*4   timcor
	  integer*2   begdat
	  integer*2   begblk
	  integer*2   blocktype1
	  integer*2   nextblock1
	  byte   encform
	  byte   wordord
	  byte   reclen
	  byte   resb4
	  integer*2   blocktype2
	  integer*2   nextblock2
	  byte   timq
	  byte   mysec
	  byte   resb5
	  byte   nfrm
c
	equivalence
     &	  (buffer(1:6),seqnum),
     &	  (buffer(7:7),indd),
     &	  (buffer(8:8),resb1),
     &	  (buffer(9:13),stat),
     &	  (buffer(14:15),loca),
     &	  (buffer(16:18),chan),
     &	  (buffer(19:20),net_id),
     &	  (buffer(21:22),year),
     &	  (buffer(23:24),doy),
     &	  (buffer(25:25),hour),
     &	  (buffer(26:26),min),
     &	  (buffer(27:27),sec),
     &	  (buffer(28:28),resb3),
     &	  (buffer(29:30),ttsec)
	equivalence
     &	  (buffer(31:32),nsamp),
     &	  (buffer(33:34),fsamp),
     &	  (buffer(35:36),msamp),
     &	  (buffer(37:37),actfl),
     &	  (buffer(38:38),iofl),
     &	  (buffer(39:39),qualfl),
     &	  (buffer(40:40),numblk),
     &	  (buffer(41:44),timcor),
     &	  (buffer(45:46),begdat),
     &	  (buffer(47:48),begblk),
     &	  (buffer(49:50),blocktype1),
     &	  (buffer(51:52),nextblock1),
     &	  (buffer(53:53),encform),
     &	  (buffer(54:54),wordord),
     &	  (buffer(55:55),reclen),
     &	  (buffer(56:56),resb4),
     &	  (buffer(57:58),blocktype2),
     &	  (buffer(59:60),nextblock2)
        equivalence
     &	  (buffer(61:61),timq),
     &	  (buffer(62:62),mysec),
     &	  (buffer(63:63),resb5),
     &	  (buffer(64:64),nfrm)
        data i2c64/64/

c       Get start of data (st_dat) and first blockette (st_blk) fields
	dummy=ibuf(12)
	st_dat=idum(1)
	st_blk=idum(2)
	if(ind.ne.3)then
	  if(ind.ge.0) then
	    if(st_dat.gt.1.and.st_dat.lt.1000) return
	  else
	    if(st_dat.lt.0.or.st_dat.ge.1000) return
	  endif
        endif
c
	do i=1,16
	  ihead(i)=ibuf(i)
	enddo
c       Don't do 'data only'/Reftek swap if data looks like mseed
	if((ind.ne.3.or.buffer(1:2).ne.'DT').and.indd.ne.'D') return
c
	if(ind.ne.3) then
	  if(ind.ge.0) st_dat=iswap(idum(1))
	  if(st_dat.eq.0) then
c	    write(*,*) 'Mseed swap error: length of fixed header zero'
	    st_dat=64
	    begdat=iswap(i2c64)
	  endif
	  start=st_dat/4+1
	  lenr=reclen
	  ndat=2**lenr
	  nswap=(ndat-st_dat)/4
	  nframe=nswap/16
c	write(*,*) 'swap',ind,start,nswap,lenr,ndat,nframe
c
          if(ind.lt.0 .and. 1.ne.byteorder() .or. ind.gt.0) then
	    year=iswap(year)
	    doy=iswap(doy)
	    ttsec=iswap(ttsec)
	    nsamp=iswap(nsamp)
	    fsamp=iswap(fsamp)
	    msamp=iswap(msamp)
	    call i4swap(1,timcor,timcor)
	    begdat=iswap(begdat)
	    begblk=iswap(begblk)
	    blocktype1=iswap(blocktype1)
	    nextblock1=iswap(nextblock1)
	    blocktype2=iswap(blocktype2)
	    nextblock2=iswap(nextblock2)
	  endif
	  if(ind.gt.0 .and. (numblk.eq.0.or.buffer(40:40).eq.' ')) then
	    numblk=1
	    blocktype1=1000
	    nextblock1=0
	  endif
	  if(encform.ne.11) encform=10
	  svwo=wordord
	  if(ind.eq.-1) wordord=1
	  reclen=12
	  resb4=0
c
	  do i=1,16
	    ibuf(i)=ihead(i)
	  enddo
c
	  if(ind.ne.-1) return

c         For "Sun" (big-endian), only swap data if not already big-endian
	  if (svwo.eq.1) return
c
	  if(chan.eq.'LOG'.or.chan.eq.'ACE') return
c
	else
	  start=17
	  nframe=15
          pause '**SWAP_MSEED:  Untested for this case!'
	endif
c
        swp=byteorder() .ne. svwo
	nn=start-1
	do n=1,nframe
	  nn=nn+1
	  if (.not.swp) nibbles=ibuf(nn)
	  call i4swap(1,ibuf(nn),ibuf(nn))
          if (swp) nibbles=ibuf(nn)
	  do m=1,15
	    nn=nn+1
	    nibble=get_nibble(nibbles,m)
 	    if(nibble.eq.0.or.nibble.eq.3) then
	      call i4swap(1,ibuf(nn),ibuf(nn))
	    endif
	    if(nibble.eq.2) then
	      dummy=ibuf(nn)
	      do i=1,2
	        idum(i)=iswap(idum(i))
	      enddo
	      ibuf(nn)=dummy
	    endif
	  enddo
	enddo
	end
	function find_rec(rec_typ,unit,ind,lenb,start_tim,nchan)
c
c   ind - 0 conventional file
c         1 raw disk (for RF only)
c
	implicit integer(a-z)
	integer ibuf(1024)
	character rec_typ*(*),stat*5,chan*3,net_id*2
	real*4 rate
	real*8 start_tim,new_tim,next_tim
c
	irec=1
	iend=0
	ndurch=0
	istat=0
	new_tim=0.0
	irecs=0
	irecm=0
	irecst=irec
c	write(*,*) 'start find_rec',iend,ndurch,new_tim,istat
	do while(iend.eq.0)
	  nsamp=0
	  do while(nsamp.eq.0.and.istat.eq.0)
	    if(ind.eq.0) then
	      istat=read_rec_fil(rec_typ,1,unit,lenb,lenb,ibuf,irec,
     &          num,form,stat,chan,net_id,iftim,iftsc,timcr,rate,
     &          nsamp,iof,nframe)
c	write(*,*) istat,irec,num,stat,chan,iftim,iftsc,timcr,rate,nsamp,iend
	      irec=irec+1
cif defined (LINUX)
	    else
	      nbskip=(irec-irecst)*lenb*4
c	write(*,*) 'try to skip',irec,irecst,nbskip
	      if(irecst.gt.0) istat=c_doffset(nbskip)
c	write(*,*) 'status',istat
	      istat=read_rec_tap(rec_typ,0,unit,0,lenb,lenb,lenb,ibuf,
     &          num,stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp,iof,
     &          nframe,neof)
	      if(stat(1:4).eq.'0000') istat=1
c	write(*,*) istat,irec,num,stat,chan,iftim,iftsc,timcr,rate,nsamp,
c     &	iend,nbskip
	      irecst=irec
	      irec=irec+1
cendif
	    endif
	  enddo
	  new_tim=dble(iftim)+dble(iftsc+timcr)*0.0001
	  next_tim=new_tim+dble(nsamp)/dble(rate)
	  if(istat.eq.0.and.irec.gt.irecm.and.new_tim.lt.start_tim)
     &      irecm=irec
c	write(*,*) start_tim,new_tim,next_tim,irec,irecm,irecs
	  if(start_tim.gt.new_tim.and.start_tim.lt.next_tim) then
	    iend=1
c	write(*,*) 'iend',irec,length,new_tim,next_tim,start_tim
	    write(*,*) 'Found requested start time by random search',
     &        irec,ndurch
	  else
	    length=next_tim-new_tim
	    if(istat.eq.0) then
	      diff=(start_tim-new_tim)/length*nchan
	      irec=irec+diff
	      if(iabs(irec-irecs).lt.10) irec=irec-irec/10
	    else
c	      irec=irec/2
	      irec=irec-irec/10
	      istat=0
	    endif
	    ndurch=ndurch+1
	    if(ndurch.gt.9) then
c	      write(*,*) irec,irecs,irecm,length,new_tim,next_tim,start_tim,ndurch,iend
	      if(ndurch.eq.12.or.irec.eq.irecs) then
	        iend=1
c	         write(*,*) 'Did not find requested start time by ',
c    &             'random search',irecm,ndurch
	        irec=irecm
	      endif
	    endif
	    irecs=irec
	  endif
	enddo
c
	find_rec=irec-2*nchan
	if(find_rec.lt.1) find_rec=1
	write(*,*) 'Jump to record: ',find_rec
cif defined(LINUX)
	if(ind.eq.1) then
	  nbskip=(find_rec-irecst)*lenb*4
c	write(*,*) 'try to skip',irec,irecst,nbskip
	  if(irecst.gt.0) istat=c_doffset(nbskip)
	endif
cendif
c
	return
	end
	function read_rec_fil(rec_typ,nchan,unit,lenbi,lenbo,ibuf,
     &    irec,num,form,stat,chan,net_id,iftim,iftsc,timcor,rate,nsamp,
     &    iof,nframe)
c
c   Reads record no irec from open file (unit)
c
	implicit integer (a-z)
	integer ibuf(*)
	real*4 rate,rates,dt
	character stat*(*),chan*(*),net_id*(*),stats*5,chans*5,
     &     rec_typ*(*)
	data ieof_st/0/, nrec_last/0/, last_val/0/
c
	ichk=0
	if(lenbo.gt.lenbi.and.ieof_st.gt.0) then
	  read_rec_fil=ieof_st
	  ieof_st=0
	  return
	endif
c
	if(rec_typ.eq.'S2') then
	  rec_typ='QT'
	  iform=12
        else
	  iform=0
	endif
c
	nb_out=lenbo*4
	nb_rec=lenbi*4
	nb_tot=0
	nlong=lenbi
	ndat=lenbi
c
	rec_fac=nlong/ndat
	nrec=(irec-1)*rec_fac
c       Seems to be a bug; why should subst. next record?  GRH/UB
c       if(rec_typ.eq.'QT'.and.lenbo.gt.lenbi) then
c         if(nrec_last.gt.0) nrec=nrec_last
c       endif
	nchans=nchan
	nums=num
	stats=stat
	chans=chan
	rates=rate
	ieof_st=0
	ind_sh=0
c
100	if(nrec.ge.0) then
	  do n=1,rec_fac
	    ia=(n-1)*ndat+1
	    ie=n*ndat
	    nrec=nrec+1
c	write(*,*) 'try to read',nrd,unit,nrec,ia,ie
	    read(unit,rec=nrec,iostat=ios) (ibuf(i),i=ia,ie)
	  enddo
	  nrec_last=nrec
C         Assume that any read error is an end-of-file.  iostat= is not
C            standardized across compilers except that 0 is OK.
	  if (ios.gt.0) ios=-1
c
	  if(ios.eq.0) then
	    ichk=0
	    if(rec_typ.eq.'RF') then
	      istat=dec_refhead(num,stat,chan,net_id,rate,begdat,iftim,
     &	        iftsc,timcor,nsamp,form,reclen,ibuf)
c	write(*,*) istat,num,stat,chan,rate,iftim,nsamp,form,reclen
	      if(istat.eq.0) then
	        do i=2,16
	          ibuf(i)=0
	        enddo
                call swap_mseed(3,ibuf)
		form=form+256*byteorder()
	      endif
	      istat=def_fixhead(num,stat,chan,net_id,1./rate,begdat,
     &          iftim,iftsc,timcor,nsamp,form,reclen,ibuf)
	    else
	      istat=dec_fixhead_ful(
     &           num,stat,chan,net_id,rate,begdat,iftim,iftsc,timcor,
     &           nsamp,form,wordord,reclen,timq,mysec,nfrm,ibuf
     &        )
              call swap_mseed(1,ibuf)
	    endif
	    last_val=ibuf(3+begdat/4)
c
c           istat=dec_fixhead_ful(num,stat,chan,net_id,rate,begdat,iftim,
c    &	    iftsc,timcor,nsamp,form,wordord,reclen,timq,mysec,nfrm,ibuf)
c	write(*,*) '1:',istat,num,stat,chan,rate,iftim,nsamp,form,reclen,
c     &	timq,mysec,nfrm
	    if(istat.ne.0) ichk=3
	    if(iform.ne.0) form=form-mod(form,256)+iform
	    if(rate.gt.0.0) then
	      dt=1./rate
	    else
	      dt=0.0
	    endif
c
            if(rate.eq.40..and.chan(1:1).eq.'B') then
               if(ind_sh.eq.0) then
                  ind_sh=1
                  write(*,*) '*** Wrong channel naming for 40 Hz ',
     &              'data stream - correct to SH? ***',chan
               endif
               chan='S'//chan(2:3)
               istat=def_fixhead_ful(num,stat,chan,net_id,1./rate,
     &         begdat,iftim,iftsc,timcor,nsamp,form,reclen,timq,mysec,
     &         nfrm,ibuf)
            endif
c
c	write(*,*) 'lenbio',rec_typ,istat,ichk,lenbi,lenbo
	    if(istat.eq.0.and.rec_typ.eq.'QT'.and.lenbo.gt.lenbi) then
	      istat=make_4096(0,nb_out,nb_rec,nb_tot,ibuf,num,stat,
     &          chan,net_id,rate,begdat,iftim,iftsc,timcor,nsamp,timq,
     &          mysec,nframe)
	      if(istat.eq.0.and.nb_tot.lt.nb_out) goto 100
	      if(istat.eq.1) nrec_last=nrec-1
            else if (istat.eq.0.and.rec_typ.eq.'QT') then
	      nframe=(lenbo*4-begdat)/64
	    endif
c
	    if(ichk.eq.0)
     &	    istat=def_fixhead_ful(num,stat,chan,net_id,dt,begdat,iftim,
     &	    iftsc,timcor,nsamp,form,reclen,timq,mysec,nfrm,ibuf)
c	write(*,*) '2:',istat,num,stat,chan,rate,iftim1,nsamp,form,reclen
	    iof=begdat/4
	  elseif(ios.eq.-1) then
	    ichk=1
	    write(*,*) 'EOF ',stats,chans,nums+1
c	write(*,*) 'End',lenbo,lenbi,nb_out,nb_rec,nb_tot
	    if(rec_typ.eq.'QT' .and. lenbo.gt.lenbi .and.
     &         nb_tot.ne.4096) then
	      istat=make_4096(999,nb_out,nb_rec,nb_tot,ibuf,num,stat,
     &          chan,net_id,rate,begdat,iftim,iftsc,timcor,nsamp,
     &	        timq,mysec,nframe)
	      istat=def_fixhead_ful(num,stat,chan,net_id,dt,begdat,
     &          iftim,iftsc,timcor,nsamp,form,reclen,timq,mysec,nfrm,
     &          ibuf)
	      ieof_st=ichk
	      ichk=0
	    else
	      num=0
	    endif
	  else
	    ichk=4
	    write(*,*) 'Read_rec read error:',nchan,nrec,ios
	    if(rec_typ.eq.'QT'.and.lenbo.gt.lenbi) then
	      istat=make_4096(999,nb_out,nb_rec,nb_tot,ibuf,num,stat,
     &          chan,net_id,rate,begdat,iftim,iftsc,timcor,nsamp,
     &	        timq,mysec,nframe)
	      istat=def_fixhead_ful(num,stat,chan,net_id,dt,begdat,
     &          iftim,iftsc,timcor,nsamp,form,reclen,timq,mysec,nfrm,
     &          ibuf)
	      ieof_st=ichk
	      ichk=0
	    else
	      num=0
	    endif
	  endif
	  irec=nrec
	else
	  ichk=1
	  write(*,*) 'Illegal rec number :',nchan,nrec,irec,rec_fac
	endif
c
	read_rec_fil=ichk
	return
	end
	function read_rec_tap(rec_form,nchan,unit,unit1,lenbi,lenbr,
     &	lenbo,ibuf,num,stat,chan,net_id,iftim,iftsc,timcor,rate,nsamp,
     &	iof,nframe,neof)
c
c   Reads next SEED data record from tape
c
	implicit integer (a-z)
	integer ibuf(*),ibuf1(8192)
	real*4 rate,rates,dt
c	character rec_form*2,stat*5,chan*3,net_id*2,stats*5,
	character rec_form*(*),stat*(*),chan*(*),net_id*(*),stats*5,
     &	chans*3,rec_typ*1,rec_num*6,cbuf1*32768
	equivalence (ibuf1,cbuf1)
c	common /rtp/ibuf1
	data nrep/0/, ieof_st/0/, ird/0/, idoub/0/
c
	ichk=0
	if(rec_form.eq.'CS'.and.lenbo.gt.lenbi.and.ieof_st.gt.0) then
	  read_rec_tap=ieof_st
	  ieof_st=0
	  return
	endif
c
	nb_out=lenbo*4
	nb_rec=lenbi*4
	if(lenbr.le.lenbi) then
	  rec_fac=lenbi/lenbr
	  imult=0
	  nrep=0
	  ird=0
	else
	  rec_fac=lenbr/lenbi
	  imult=1
	endif
c	write(*,*)  rec_form,lenbi,lenbr,lenbo,imult,rec_fac
	nums=num
	stats=stat
	chans=chan
	rates=rate
	ieof_st=0
c
100	ios=0
	if(imult.eq.0.and.rec_form.ne.'CS') then
	  do while (nrep.lt.rec_fac.and.ios.eq.0)
	    nrep=nrep+1
	    ia=(nrep-1)*lenbr+1
	    ie=nrep*lenbr
	    call rd(unit,ibuf(ia),lenbr*2,ios,nbt)
c	write(*,*)  'rd1',ios,nbt,ia,ie
	    if(ios.ne.0) then
	      neof=neof+1
	      if(ios.eq.1)
     &          write(*,*) 'EOF: mark #',neof,' ',stats,chans,nums+1
	      if(ios.eq.1.and.unit1.gt.0) 
     &	        write(unit1,*) 'EOF: mark #',neof,' ',
     &            stats,chans,nums+1
	      if(ios.eq.2) write(*,*) 'EOT: mark #',neof
	      if(ios.eq.2.and.unit1.gt.0)
     &          write(unit1,*) 'EOT: mark #',neof
	      if(ios.eq.5) write(*,*) 'EOV: mark #',neof
	      if(ios.eq.5.and.unit1.gt.0)
     &          write(unit1,*) 'EOV: mark #',neof
	      if(ios.ne.1.and.ios.ne.2.and.ios.ne.5) write(*,*) 
     &	      'RD error:',ios,nbt,stats,chans,nums+1
	      if(ios.ne.1.and.ios.ne.2.and.ios.ne.5.and.unit1.gt.0) 
     &	      write(unit1,*) 'RD error:',ios,nbt,stats,chans,nums+1
	    endif
	  enddo
	else
c	write(*,*)  'try to read',rec_fac,nrep,lenbi,lenbr
1000	  nrep=nrep+1
	  if(nrep.gt.rec_fac) nrep=1
	  ia=(nrep-1)*lenbi
c	write(*,*)  ia,nrep,lenbi,ird,lenbr,ios,unit
	  if(nrep.eq.1.and.ird.eq.0) then
	    call rd(unit,ibuf1,lenbr*2,ios,nbt)
c	write(*,*)  'rd2',ios,nbt
	    if(ios.ne.0) then
	      neof=neof+1
	      if(ios.eq.1)
     &          write(*,*) 'EOF: mark #',neof,' ',stats,chans,nums+1
	      if(ios.eq.1.and.unit1.gt.0) 
     &	      write(unit1,*) 'EOF: mark #',neof,' ',stats,chans,nums+1
	      if(ios.eq.2)
     &          write(*,*) 'EOT: mark #',neof
	      if(ios.eq.2.and.unit1.gt.0)
     &          write(unit1,*) 'EOT: mark #',neof
	      if(ios.eq.5) write(*,*) 'EOV: mark #',neof
	      if(ios.eq.5.and.unit1.gt.0)
     &          write(unit1,*) 'EOV: mark #',neof
	      if(ios.ne.1.and.ios.ne.2.and.ios.ne.5) write(*,*) 
     &	      'RD error:',ios,nbt,stats,chans,nums+1
	      if(ios.ne.1.and.ios.ne.2.and.ios.ne.5.and.unit1.gt.0) 
     &	      write(unit1,*) 'RD error:',ios,nbt,stats,chans,nums+1
	      nrep=0
	    endif
	  endif
c
	  if(nrep.gt.0) then
	    ia=(nrep-1)*lenbi
	    if(rec_form.ne.'RF') then
	      ca=ia*4
	      rec_num=cbuf1(ca+1:ca+6)
	      rec_typ=cbuf1(ca+7:ca+7)
	    else
	      rec_typ='D'
	    endif
c	write(*,*)  'copy',ia,nrep,lenbi,rec_num,rec_form,rec_typ
	    if(rec_typ.eq.'D') then
	      ichks=0
c
	      read(rec_num,*) num
	      if(rec_form.eq.'CS'.and.
     &	      nums.gt.0.and.num.le.nums.and.nums-num.lt.100) then
	        if(idoub.eq.0)
     &            write(*,*) 'Start eliminating double data',num,nums
	        idoub=1
	        goto 1000
	      endif
	      if(idoub.eq.1)
     &          write(*,*) ' Stop eliminating double data',num
	      idoub=0
	      nums=num
c
	      do i=1,lenbi
	        ibuf(i)=ibuf1(ia+i)
c	type '(1x,2i8,z8)',i,ia+i,ibuf(i)
	      enddo
	      iend=1
	      ird=0
	    else
	      if(rec_typ.ne.' ') write(*,*) 'Record ',rec_num,
     &	      ' is of type ',rec_typ,', skip record'
c	      write(*,*) 'Record ',rec_num,
c     &	      ' is of type ',rec_typ,', skip record',nrep
	      iend=0
c	      nrep=nrep-1	!?????
	      goto 1000
	    endif
	  endif
	endif
c
	if(ios.eq.0) then
	  ichk=0
	  if(rec_form.eq.'RF') then
	    istat=dec_refhead(num,stat,chan,net_id,rate,begdat,iftim,
     &	    iftsc,timcor,nsamp,form,reclen,ibuf)
c	write(*,*) istat,num,stat,chan,rate,iftim,nsamp,form
	    if(istat.eq.0) then
	      do i=2,16
	        ibuf(i)=0
	      enddo
	      call swap_mseed(3,ibuf)
	      form=form+256*byteorder()
	    else
	      if(istat.eq.4) goto 100
	    endif
	    istat=def_fixhead(num,stat,chan,net_id,1./rate,begdat,
     &        iftim,iftsc,timcor,nsamp,form,reclen,ibuf)
            timq=0
            mysec=0
            nfrm=0
	  else
	    istat=dec_fixhead_ful(num,stat,chan,net_id,rate,begdat,
     &        iftim,iftsc,timcor,nsamp,form,wordord,reclen,timq,mysec,
     &        nfrm,ibuf)
	    call swap_mseed(1,ibuf)
	  endif
c
c	write(*,*) 'dec',istat,num,stat,chan,net_id,rate,begdat,iftim,
c     &	iftsc,timcor,nsamp,form,reclen,rec_typ
	  if(istat.ne.0) ichk=3
	  if(istat.eq.0.and.rec_form.eq.'CS'.and.nsamp.eq.0.and.
     &	     chan(3:3).eq.'Z') then
	    write(*,*) 'Null record found - skip ',num,' ',chan
	    goto 100
	  endif
	  if(rate.gt.0.0) then
	    dt=1./rate
	  else
	    dt=0.0
	  endif
c
	  if(rec_form.eq.'CS'.and.lenbo.gt.lenbi) then
	    istat=make_4096(0,nb_out,nb_rec,nb_tot,ibuf,num,stat,chan,
     &        net_id,rate,begdat,iftim,iftsc,timcor,nsamp,timq,mysec,
     &	      nfrm)
c	    write(*,*) 'make',nb_out,nb_rec,nb_tot,istat
	    if(lenbr.eq.128) nrep=0
	    if(istat.eq.0.and.nb_tot.lt.nb_out) goto 100
	    if(istat.eq.1) then
	      nrep=nrep-1
	      ird=0
	      if(nrep.eq.0) ird=1
c	      write(*,*) 'flush',nrep,ird
	    endif
	  endif
c
	  if(ichk.eq.0) then
	    istat=def_fixhead_ful(num,stat,chan,net_id,dt,begdat,
     &        iftim,iftsc,timcor,nsamp,form,reclen,timq,mysec,nfrm,
     &        ibuf)
	  endif
	  iof=begdat/4
	  nframe=(4096-begdat)/64
	else
	  ichk=ios
c	  write(*,*) 'Read_rec read error:',num,ios,nbt,nb_tot,nb_out,nb_rec
	  if(rec_form.eq.'CS'.and.lenbo.gt.lenbi.and.nb_tot.gt.0.and.
     &	  nb_tot.lt.nb_out) then
	    istat=make_4096(999,nb_out,nb_rec,nb_tot,ibuf,num,stat,
     &        chan,net_id,rate,begdat,iftim,iftsc,timcor,nsamp,timq,
     &        mysec,nfrm)
	    istat=def_fixhead_ful(num,stat,chan,net_id,dt,begdat,
     &        iftim,iftsc,timcor,nsamp,form,reclen,timq,mysec,nfrm,
     &        ibuf)
	    ieof_st=ichk
	    ichk=0
	  else
	    num=0
	  endif
	endif
c
	read_rec_tap=ichk
c	write(*,*) 'read_rec_tap',ichk,read_rec_tap
	return
	end
	function dec_refhead(num,stat,chan,net_id,rate,begdat,istim,
     &	ttsec,timcor,nsamp,form_key,reclen,data)
c
	implicit integer(a-z)
	integer ichn(8)
	character data*(*),rec_typ*2,head*44,stat*(*),chan*(*)
	character net_id*(*),cform*2,cr_chan*1
	real*4 rate,srate,cor_rate
	common /cor_year/cor_year,cor_rate
	data irate,cform/0,' '/
c
	dec_refhead=0
	rec_typ=data(1:2)
	write(head,'(14z2.2)') (data(i:i),i=3,16)
c	write(*,*) head(1:28)
	read(head,9,err=999) nexp,year,unitid,day,ih,im,is,its,nbyte,
     &    num
9	format(2i2.2,i4.2,i3.3,3i2.2,i3.3,2i4.4)
c	write(*,*) nexp,year,unitid,day,ih,im,is,its,nbyte,num
c
	write(stat,'(i4.4)') unitid
	net_id='YY'
	year=1900+year
	if(year.lt.1950) year=year+100
	if(cor_year.gt.0 .and. year.ne.cor_year .and.
     &     year.ne.cor_year-1) then
	  if(iprint.eq.0) write(*,*) '**** Wrong year found - ',
     &      'take defined one *** ',year,cor_year
	  year=cor_year
	  iprint=1
	endif
	call abstim(1,istim,year,day,ih,im,is)
	ttsec=its*10
c
	if(rec_typ.ne.'DT') then
c
	  if(rec_typ.eq.'DS') then
	    read(data(17:18),'(i2.2)') nstrm
	    read(data(43:48),'(8i1.1)') (ichn(i),i=1,8)
	    read(data(59:62),'(i4.4)') irate
	    read(data(63:64),'(a)') cform
c	write(*,*) nstrm,irate,cform,(ichn(i),i=1,8)
	    if(irate.gt.0) then
	      srate=irate
	    else
	      write(*,*) 'Sampling rate not defined - take 20 Hz'
	      srate=20.
	    endif
	    if(cform.ne.'CO') then
	      write(*,*) 'REFTEK format error - format not CO: ',cform
	      cform='CO'
	    endif
	  endif
c
	  chan=rec_typ
	  rate=0.0
	  begdat=0
	  timcor=0
	  form_key=0
	  dec_refhead=1
	  chan='LOG'
	  do n=1024,65,-1
	    nn=n-64
	    data(n:n)=data(nn:nn)
	  enddo
c
	  return
	endif
c
	if(rec_typ.eq.'DT'.and.srate.eq.0.0) then
	  write(*,*)
     &      '**** Missing definition headers - take defaults ****'
	  if(cor_rate.gt.0.) then
	    srate=cor_rate
	  else
	    srate=20.
	  endif
	  write(*,*) '**** Sampling rate',srate,'Hz ****'
	endif
c
	write(head,'(8z2.2)') (data(i:i),i=17,24)
c	write(*,*) head(1:16)
	read(head,'(i4.4,2i2.2,i4.4,i2.2,a)')
     &    nev,nstrm,nchn,nsamp,dum,form
c
	if(srate.gt.40.) cr_chan='H'
	if(srate.le.40.and.srate.gt.20.) cr_chan='S'
	if(srate.le.20.and.srate.gt.1.) cr_chan='B'
	if(srate.le.1.) cr_chan='L'
	if(nchn.eq.0) chan=cr_chan//'HZ'
	if(nchn.eq.1) chan=cr_chan//'HN'
	if(nchn.eq.2) chan=cr_chan//'HE'
	if(nchn.eq.3) chan=cr_chan//'LZ'
	if(nchn.eq.4) chan=cr_chan//'LN'
	if(nchn.eq.5) chan=cr_chan//'LE'
	rate=srate
	begdat=64
	timcor=0
	form_key=256+10
	reclen=10
c
	return
c
999	write(*,*) 'Warning: unexpected record ',head(1:28)
	dec_refhead=4
	return
	end
	subroutine change_stat(stat_new,buffer)
c
	implicit integer(a-z)
	character stat_new*(*),buffer*(*)
c
	buffer(9:13)=stat_new(1:5)
c
	return
	end
	integer function byteorder()
C       BYTEORDER -- Return byte order of machine
C
C       Called via:
C          n = byteorder()
C
C       Returns:
C          0 - VAX/Intel byte order (little-endian)
C          1 - m68k, Sun byte order (big-endian)
C
C       G. Helffrich - U. Bristol
	integer value
	character bytes*4
	equivalence (value,bytes)

C       This is 0xffffff00
	value = -256
	if (ichar(bytes(1:1)) .eq. 0) then
	   byteorder = 0
        else if (ichar(bytes(4:4)) .eq. 0) then
	   byteorder = 1
        else
	   pause '**Unknown behavior in byteorder.'
	   byteorder = 0
        endif
	end
