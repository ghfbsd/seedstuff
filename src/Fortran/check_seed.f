	program check_seed
c
	implicit integer(a-z)
	parameter (dmax=62*4*15+4*13)
	integer ibuf(1024),dbuf(dmax),ihdr(16)
	integer*2 ihds(32)
	character buffer*4096,seed_in*132,idir*32,fmt(2)*2,
     &	dummy1*80,dummy2*80,dummy3*80
	equivalence (buffer,ibuf),(ihdr,ihds)
	parameter (idsc2=1)
c
        logical ofix, swp, wrt
	character stat*5,chan*3,net_id*2,strng*1,stat_sr*5,chan_sr*3,
     &	opt(10)*1,opt_string(10)*80,string*132
	integer timcor
	real*4 rate
	real*8 dt,anf_tim,end_tim,next_tim,numta,numte
c
	character version*18
 	include 'statnet.inc'
 	include 'version.inc'

	data fmt /'LE','BE'/
	data ofix /.false./, lpsc/0/

	write(*,*) 'check_seed -',version
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call check_seed_use
	inarg=1
	istat=get_opt(' ',seed_in,nopt,opt,opt_string)
	isearch=0
	istar=index(seed_in,'*')
	if(istar.gt.0) then
	  isearch=1
	  if(istar.gt.1) then
	    nstat=1
	    stat_net(1)=seed_in(1:istar-1)
	  endif
	endif
	iall=0
	itim=0
	jrec=0
	nbtin=4096
	if(nopt.gt.0) then
	  istat=get_opt('B',string,nopt,opt,opt_string)
	  if(istat.eq.1) read(string,*) nbtin
	  istat=get_opt('a',string,nopt,opt,opt_string)
	  ls=lenc(string)
	  if(istat.eq.1) iall=1
	  if(iall.eq.1.and.ls.gt.0) then
	     if(string(1:1).ne." ") read(string,*) jrec
	  endif
	  if(iall.eq.1) then
	     istat=get_opt('t',strng,nopt,opt,opt_string)
	     if(istat.eq.1) itim=1
	  endif
	  istat=get_opt('o',strng,nopt,opt,opt_string)
	  if(istat.eq.1) iall=2
	  ofix=1 .eq. get_opt('f',strng,nopt,opt,opt_string)
	endif
c
	mode=0
	idir='./'
	nst=0
	nch=nchan
	istfl=2
c
1000	if(isearch.eq.1) then
	  if(istfl.eq.2) then
	    nch=nch+1
	    if(nch.gt.nchan) then
	      nst=nst+1
	      if(nst.gt.nstat) goto 999
	      nch=1
	      stat_sr=stat_net(nst)(1:lenc(stat_net(nst)))//'*'
	    endif
	    chan_sr=chan_net(nch)
	    mode=0
	    seed_in='*'
	    numta=19700101.
	    numte=20380101.
	  endif
c	write(*,*) mode,idir,stat_sr,chan_sr,seed_in,numta,numte
	  istfl=next_sfile(mode,idir,stat_sr,chan_sr,seed_in,numta,
     &	  numte)
c	  write(*,*) 'next_sfile: ',istfl,seed_in
	  if(istfl.eq.2) goto 1000
	  mode=1
	endif
c
	if(inarg.eq.0) then
	  write(*,'(1x,a,$)') 'Input file:_'
	  read(*,'(a)') seed_in
	endif
c
c	nbtin=4096
c	lenr=1024
	lenr=nbtin
	open(idsc2,file=seed_in,status='old',form='unformatted',access=
     &	'direct',recl=lenr,iostat=ios)
        if(ios .ne. 0) then
	  write(*,*) '**Unable to open data file.'
	  stop
        endif
	nfl=lenc(seed_in)
	write(*,'(1x,2a)') 'Opened file: ',seed_in(1:nfl)
	nrec=0
	irec=0
	diff=0
c
100	nrec=nrec+1
	if(jrec.gt.0) nrec=jrec
	read(idsc2,rec=nrec,iostat=ios) buffer(1:nbtin)
	if(ios.ne.0) goto 900
c       Save data format byte and block size across swap (stupid routine sets
c         it unconditionally).
        wrt = .false.
        do i=1,16
	   ihdr(i)=ibuf(i)
        enddo
        sv_form = ichar(buffer(53:53))
        sv_blkz = ichar(buffer(55:55))
	call swap_mseed(1,ibuf)
	if (ichar(buffer(53:53)).eq.10) then
	  buffer(53:53) = char(sv_form)
	  buffer(55:55) = char(sv_blkz)
        endif
c
	if(0 .eq. index('DRQMB',buffer(7:7))) then
	  write(*,*) 'Skip ',buffer(7:7),' record ',buffer(1:8)
	  goto 100
	endif
	istat=dec_fixhead_ful(num,stat,chan,net_id,rate,begdat,iftim,
     &	  iftsc,timcor,nsamp,form,wordord,reclen,timq,mysec,nfrm,ibuf)
c	write(*,*) istat,irec,num,stat,chan,iftim,iftsc,timcor,rate,nsamp,
c     &	form,ord,reclen,timq,mysec,nfrm
c	istat=def_fixhead(num,stat,chan,net_id,1./rate,begdat,iftim,iftsc,
c     &	timcor,nsamp,buffer)
	iftcor=iftsc+timcor
	anf_tim=dble(iftcor)*dble(0.0001)
	anf_tim=anf_tim+dble(iftim)
	if(rate.gt.0.0) then
	  dt=1./rate
	  end_tim=anf_tim+nsamp*dt
	else
	  dt=0.0
	  end_tim=anf_tim
	endif
c
        lflg = mod(ichar(buffer(38:38))/16,4)
        if (lflg .ne. 0) then
c         Adjust time for any flagged leap second in interval
	  if (lflg.eq.1) lpsc=+1
	  if (lflg.eq.2) lpsc=-1
	  end_tim=end_tim-lpsc
        endif
c
	call tfix(anf_tim,iftim,iftsc)
	call abstim(0,iftim,iyra,idya,iha,ima,isa)
	call datum(0,idya,iyra,mona,itga)
	call tfix(end_tim,ietim,ietsc)
	call abstim(0,ietim,iyre,idye,ihe,ime,ise)
	call datum(0,idye,iyre,mone,itge)
	irec=irec+1
c
	if(irec.gt.1) diff=(next_tim-anf_tim)*10000.
	if(iabs(diff).gt.9999) diff=sign(9999,diff)
c	write(*,*) next_tim,anf_tim,diff
	if(irec.gt.1 .and. abs(next_tim-anf_tim).gt.dt*0.5 .and.
     &    dt.gt.0.0) then
	  call tfix(next_tim,intim,intsc)
	  call abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	  call datum(0,idyn,iyrn,monn,itgn)
          if (diff.lt.0) then
            dummy1='Time-gap:'
          else
            dummy1='Time-overlap:'
          endif
          io=index(dummy1,' ')-1
      	  write(*,9) 
     &	  dummy1(1:io),itga,mona,iyra,iha,ima,isa,iftsc,
     &	  'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc
9	format(2(1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,
     &     i4.4))
	endif
c
	if(irec.le.1) then
	  sr_form=form
	  sr_recl=reclen
	else if(reclen.ne.sr_recl .or. form.ne.sr_form) then
	  write(*,'(1x,a,i6,a,i3,a,i2)')
     &       'Record ',num,': Incorrect record length ',reclen,
     &       ' or format',form
	endif
c
	if(iall.eq.1) then
	  write(*,19)
     &    nrec,stat,chan,net_id,rate,nsamp,itga,mona,iyra,iha,ima,isa,
     &	  iftsc,'to',itge,mone,iyre,ihe,ime,ise,ietsc,diff
19	  format(i6,3(1x,a),f6.2,i6,2x,2(i2.2,1h/),
     &         i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,2x,a,2x,2(i2.2,1h/),
     &         i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,i5)
	  if(itim.eq.1) write(*,*) 'Timing quality (%,mysec):',
     &      timq,mysec
	elseif(iall.eq.2) then
	  write(*,*) num,' ',stat,chan,' ',
     &      net_id,rate,itga,mona,iyra,iha,ima,isa,iftsc,timcor,nsamp
	elseif(irec.eq.1) then
	  write(*,29) 'Start of first record:',
     &	    stat,chan,net_id,itga,mona,iyra,iha,ima,isa,iftsc
29	    format(1x,a,2x,2a,1x,a,2x,2(i2.2,1h/),
     &             i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,i5)
	  write(*,'(1x,a,i2.2,1x,a,1x,a,f7.2)')
     &      'Format code: ',mod(form,256),fmt(1+mod(form/256,2)),
     &      'Rate:',rate
	endif

c       Validate match between samples in record header and samples in Steim
c          control words -- if data in Steim I format!
        if (mod(form,256).eq.10) then
	  swp = iyra .ne. ihds(11)
	  if (begdat.lt.64) then
	    write(*,59) nrec,begdat,64
	    begdat=64
59          format(1x,'*** Incorrect data start offset in record ',i6,
     &      ': ',i4,' trying with likely value ',i4)
	  endif
          io=1+begdat/4
          nfrm=(nbtin-begdat)/64
          nchk=0
          do i=0,nfrm-1
	    nchk=nchk+nssamp(i4val(swp,ibuf(io+i*16)))
	  enddo
	  if(nchk.ne.nsamp) write(*,49) nrec,nsamp,nchk
49        format(1x,'Incorrect # samples in record ',i6,
     &      ', in header ',i4,' in control words ',i4)
          if (ofix) then
	    i = decomp_steim(form,ifchk,nrec,nfrm,nchk,
     &          ibuf(io),nout,dbuf,last_val)
c           Problem with data - update # samples, x0, xn
            if (wrt .or. i.ne.0 .or. nchk.ne.nsamp) then
	      ibuf(io+2) = i4val(swp,last_val)
c             Update #samples in preserved header, copy back
	      ihds(16) = i2val(swp,nchk)
	      do i=1,16
		ibuf(i) = ihdr(i)
	      enddo
              write(idsc2,rec=nrec,iostat=ios) buffer(1:nbtin)
	      if (ios.ne.0)
     &          write(*,69) '*** Trouble fixing rec ',nrec
	    endif
	  endif
        else
	  nchk = nsamp
        endif
c
	next_tim=end_tim
c	write(idsc2,rec=nrec) buffer
	num=num+1
	if(jrec.eq.0) goto 100
c	
900	continue
        if(nchk.ne.nsamp) write(*,*)
     &    '(Sample # mismatch probably from unzeroed buffer when',
     &    ' writing original data)'
        if(iall.eq.0) then
	  write(*,39) '   End of last record:',
     &	  itge,mone,iyre,ihe,ime,ise,ietsc
39	format(1x,a,15x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,
     &     i5)
69	format(1x,a,i6)
	endif
c
	close(idsc2)
	if(isearch.eq.1) goto 1000
c
999	continue
	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)
	end
	subroutine check_seed_use
c
	write(*,*) 'Checks MiniSEED data files in Quanterra Steim1 or',
     &     ' Steim2 compression format'
	write(*,*) 'Usage:'
	write(*,*) 'check_seed <filename> -[act]'
	write(*,*) '  <filename> - full filename or "*" or "<statcode>*"'
	write(*,*) '  -a (<no_rec>) - print (a single or all) record(s)'
	write(*,*) '  -B <bsize> - blocksize in bytes'
	write(*,*) '  -f - check data and fix Steim xn words'
	write(*,*) '  -t - print timing parameters (along with -a)'
c
	stop
	end
