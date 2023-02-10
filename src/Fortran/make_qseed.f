	program make_qseed
c
	implicit integer(a-z)
	integer ibuf(2048),obuf(1024),cbuf(2048)
	integer*2 hbuf(32)
	byte obbuf(4096)
	character buffer*4096,seed_in*132,seed_out*256,idir*32,odir*64
        character dummy*80,nid*2,band*1,fmt(2)*2
	equivalence(ibuf,buffer),(obuf,obbuf,cbuf,hbuf)
	logical swp
	parameter (idsc1=1,idsc2=2)
c
	character stat*5,chan*3,net_id*2,strng*1,
     &  stat_net(14)*5,chan_net(9)*3,
     &	stat_sr*5,chan_sr*3,stat_new*5,
     &  stream*1,opt(10)*1,opt_string(10)*80,cstr*32,sesr*16
	integer timcor
	real*4 rate
	real*8 dt, anf_tim,end_tim,cut_tim,next_tim,last_next
	real*8 numta,numte
c
	character version*18
 	include 'version.inc'
	data fmt/'LE','BE'/, timq,mysc/2*0/, nbtin/4096/, swp/.false./
	write(*,*) 'make_qseed -',version
c
        bo=byteorder()

	do i=1,16
	  obuf(i)=0
        enddo

	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call make_qseed_use
	inarg=1
	istat=get_opt(' ',seed_in,nopt,opt,opt_string)
	if (0.eq.get_opt('a',stat_new,nopt,opt,opt_string)) then
	  iall=0
        else if (0.eq.get_opt('v',stat_new,nopt,opt,opt_string)) then
	  iall=2
        else
	  iall=1
        endif
	inst=get_opt('S',stat_new,nopt,opt,opt_string)
	iblk=get_opt('b',buffer,nopt,opt,opt_string)
	ilnm=get_opt('l',dummy,nopt,opt,opt_string)
	inet=get_opt('n',nid,nopt,opt,opt_string)
	iout=get_opt('o',odir,nopt,opt,opt_string)
	ibnd=get_opt('r',band,nopt,opt,opt_string)
	irss=get_opt('R',dummy,nopt,opt,opt_string)
	icut=get_opt('c',cstr,nopt,opt,opt_string)
	iesr=get_opt('s',sesr,nopt,opt,opt_string)
c
	mode=0
	idir='./'
	if (iout.ne.1) then
	  odir='./'
	else
	  ix=lenc(odir)
	  if (odir(ix:ix).ne.'/') odir(ix+1:ix+1)='/'
	endif
	if (icut.ne.0) then
	  read(cstr,*,iostat=ios) icyr,icmo,icdy,ichr,icmn,icss,icth
	  if (ios.ne.0) stop '**Cut time (-c) bad.'
	  call datum(1,icdoy,icyr,icmo,icdy)
	  call abstim(1,ictim,icyr,icdoy,ichr,icmn,icss)
	  if (ictim.lt.0) stop '**Cut time (-c) bad.'
	  cut_tim=dble(ictim)+dble(icth)*0.0001
	endif
	if (iblk.ne.0 .and. buffer .ne. ' ') then
	  read(buffer,*,iostat=ios) nbtin
	  if (ios.ne.0 .or. nbtin.lt.256 .or. nbtin.ne.ntwo(nbtin))
     &      stop '**Bad -b value'
        endif
	nst=0
	nch=9
	istfl=2
	nchan=0
	nstat=0
	nsblk=0
c
	open(idsc2,file=seed_in,status='old',form='unformatted',
     &       access='direct',recl=nbtin)
	nfl=lenc(seed_in)
	write(*,'(1x,2a)') 'Opened file: ',seed_in(1:nfl)
	nrec=0
	nreco=0
	diff=0
	nout=0
	ihead=0
c
100	nrec=nrec+1
        read(idsc2,rec=nrec,iostat=ios) buffer(1:nbtin)
        if(ios.ne.0) goto 900

	buffer(1:6)='000000'
	istat=dec_fixhead(num,stat,chan,net_id,rate,begdat,iftim,iftsc,
     &	  timcor,nsamp,form,reclen,buffer)
	swp=form/256 .ne. bo
c
	if(iesr.eq.1) then
	  read(sesr,*,iostat=ios) rate
	  if(ios.ne.0) stop '**ERROR:  Bad -s value'
        endif
	if(rate.lt.1.) rate=1./rate
 	if(inst.eq.1) stat=stat_new
	if(inet.eq.1) then
	  net_id=nid
	else
	  net_id='YY'
	endif
	if(ibnd.eq.1) then
	  chan(1:1)=band
	  stream=band
 	else
	  if(rate.ge.80.0) stream='H'
	  if(rate.lt.80.0) stream='B'
	  if(rate.lt.10.0) stream='M'
	  if(rate.le.1.00) stream='L'
 	endif
	if(chan(3:3).eq.'1'.or.chan(3:3).eq.'4') chan=stream//'HZ'
	if(chan(3:3).eq.'2'.or.chan(3:3).eq.'5') chan=stream//'HN'
	if(chan(3:3).eq.'3'.or.chan(3:3).eq.'6') chan=stream//'HE'
c
	if(nrec.eq.1) then
	  call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	  call datum(0,idyf,iyrf,monf,itgf)
	  if(ilnm.eq.0.and.
     &      ihf.eq.0.and.imf.eq.0.and.isf.eq.0.and.iftsc.eq.0) then
	    write(seed_out,'(2a,3i2.2,1h.,a)') odir(1:lenc(odir)),
     &	    stat(1:lenc(stat)),cyear(iyrf),monf,itgf,chan
	  else
	    write(seed_out,'(2a,6i2.2,1h.,a)') odir(1:lenc(odir)),
     &	    stat(1:lenc(stat)),cyear(iyrf),monf,itgf,ihf,imf,isf,
     &	    chan
	  endif
	  open(idsc1,file=seed_out,status='unknown',form='unformatted',
     &	  access='direct',recl=4096)
          ibtim=iftim
	  ibtsc=iftsc
	  ibcor=timcor
	endif
c
        if(irss.ne.0) then
	  ns=nssamp(ibuf(1+16))
	  do i=2,nbtin/64-1
	    ns=ns+nssamp(ibuf(1+16*i))
	  enddo
	  if(ns .ne. nsamp)then
	    write(*,*) '   **Sample count wrong in record ',nrec,
     &         ', is ',nsamp,' and should be ',ns
            nsamp=ns
	  endif
	endif
	nsblk = nsblk + nsamp
c
	anf_tim=dble(iftim)+dble(iftsc+timcor)*0.0001
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

c       Check if file should be truncated by explicit cut; change nsamp if so.
c         Also clear control words following frame with last sample in it.
	if (icut.ne.0 .and. cut_tim.lt.end_tim) then
	  if (dt.gt.0) nsamp=max(0,nsamp-nint((end_tim-cut_tim)/dt))
	  end_tim=cut_tim
	  ns=nssamp(ibuf(1+16))
	  do i=2,nbtin/64-1
	    if(ns.ge.nsamp) ibuf(1+16*i)=0
	    ns=ns+nssamp(ibuf(1+16*i))
	  enddo
	  if (ns.ne.nsamp) then
	    write(*,*) '  **Warning: cut point does not end frame.'
	  endif
        endif
	call tfix(anf_tim,iftim,iftsc)
	call abstim(0,iftim,iyra,idya,iha,ima,isa)
	call datum(0,idya,iyra,mona,itga)
	call tfix(end_tim,ietim,ietsc)
	call abstim(0,ietim,iyre,idye,ihe,ime,ise)
	call datum(0,idye,iyre,mone,itge)

	istat=def_fixhead_ful(nrec,stat,chan,net_id,real(dt),begdat,
     &    iftim,iftsc,timcor,nsamp,form,12,0,0,0,ibuf)
c
	if(nrec.gt.1) diff=(next_tim-anf_tim)*10000.
	if(iabs(diff).gt.9999) diff=9999
c	write(*,*) next_tim,anf_tim,diff
	if(nrec.gt.1 .and. abs(next_tim-anf_tim).gt.dt*0.5 .and.
     &     dt.gt.0.0) then
	  call tfix(next_tim,intim,intsc)
	  call abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	  call datum(0,idyn,iyrn,monn,itgn)
	  if(ihead.ne.1)
     &	  write(*,'(2(1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),
     &    i2.2,1h.,i4.4))') 
     &	  'Time-gap in input file: ',itga,mona,iyra,iha,ima,isa,iftsc,
     &	  'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc
c
	  if(nout.gt.0) then
c           Time discontinuity.  Zero all words to end of buffer.  Recalculate
c           number of samples in continuous data up to end of buffer and reset.
	    ib = begdat/4
	    obuf(1+ib+2) = ric
	    do i=1,1024-nout
	      obuf(nout+i) = 0
	    enddo
	    nsblk=nssamp(obuf(1+ib))
	    do i=1,(4096-begdat)/64-1
	      nsblk=nsblk+nssamp(obuf(1+ib+16*i))
	    enddo
	    nreco=nreco+1
            istat=def_fixhead_ful(nreco,stat,chan,net_id,real(dt),
     &                           begdat,ibtim,ibtsc,ibcor,nsblk,form,
     &                           12,0,0,0,obuf)
            if(swp) call swaphd(hbuf)
	    write(idsc1,rec=nreco) obuf
	    nout=0
	    nsblk=nsamp
	  endif
	  ibtim=iftim
	  ibtsc=iftsc
	  ibcor=timcor
	endif
c
	if(iall.eq.1) then
	  write(*,'(i6,3(1x,a),f6.2,i6,2x,2(i2.2,1h/),i4.4,2x,
     &    2(i2.2,1h:),i2.2,1h.,i4.4,2x,a,2x,2(i2.2,1h/),i4.4,2x,
     &    2(i2.2,1h:),i2.2,1h.,i4.4,i5)')
     &    num,stat,chan,net_id,rate,nsamp,itga,mona,iyra,iha,ima,isa,
     &	  iftsc,'to',itge,mone,iyre,ihe,ime,ise,ietsc,diff
	elseif(iall.eq.2) then
	  write(*,*) num,' ',stat,chan,' ',
     &      net_id,rate,itga,mona,iyra,iha,ima,isa,iftsc,timcor,nsamp
	elseif(nrec.eq.1) then
	  write(*,'(1x,a,2x,2a,1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),
     &      i2.2,1h.,i4.4)') 'Start of first record:',
     &	    stat,chan,net_id,itga,mona,iyra,iha,ima,isa,iftsc
	  write(*,'(1x,a,i2.2,1x,a,1x,a,f7.2)')
     &      'Format code: ',mod(form,256),fmt(1+mod(form/256,2)),
     &      'Rate:',rate
	endif
c
	last_next=next_tim
	next_tim=end_tim
c
C       Start of data is either start of block (if first block of group) or
C         it is the beginning of the Steim data (if subsequent block of group)
	if (nout.gt.0) then
	  ib = begdat/4
        else
	  ib = 0
        endif
	ric = ibuf(1+begdat/4+2)
	do n=1+ib,nbtin/4
	  nout=nout+1
	  cbuf(nout)=ibuf(n)
	enddo
c
	if(nout.ge.1024) then
C         Recalculate samples in all Steim frames.
	  ib = begdat/4
	  ns=nssamp(obuf(1+ib))
	  do i=1,(4096-begdat)/64-1
	    ns=ns+nssamp(obuf(1+ib+16*i))
	  enddo
	  obuf(1+ib+2) = ric
	  nreco=nreco+1
	  istat=def_fixhead_ful(nreco,stat,chan,net_id,real(dt),
     &                          begdat,ibtim,ibtsc,ibcor,ns,form,
     &                          12,0,0,0,obuf)
          if(swp) call swaphd(hbuf)
	  write(idsc1,rec=nreco) obuf
	  nout=nout-1024
	  if (nout.gt.0) then
	    do i=1,nout
	      obuf(ib+i) = cbuf(1024+i)
	    enddo
	    nout=nout+16
	    nfrm=(nout-ib)/16
	    nsblk=nssamp(obuf(1+ib))
	    do i=1,nfrm-1
	      nsblk=nsblk+nssamp(obuf(1+ib+16*i))
	    enddo
	  else
	    nsblk = 0
	  endif
	  anf_tim=dble(ibtim)+dt*ns+dble(ibtsc)*0.0001
	  call tfix(anf_tim,ibtim,ibtsc)
	endif
c
	goto 100
c	
900	if(iall.eq.0) then
	  write(*,'(1x,a,2x,2a,1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),
     &    i2.2,1h.,i4.4,i5)') '   End of last record:',
     &	  stat,chan,net_id,itge,mone,iyre,ihe,ime,ise,ietsc
	endif
c
	if(nout.gt.0) then
	  if(mod(nout,16) .ne. 0) then
	    pause '**BUFFERING LOGIC IS WRONG -- PARTIAL STEIM FRAME?'
	  endif
	  do i=nout,1024
	    obuf(i) = 0
	  enddo
	  ib = begdat/4
	  nfrm=(nout-ib)/16
          ns=nssamp(obuf(1+ib))
          do i=1,nfrm-1
            ns=ns+nssamp(obuf(1+ib+16*i))
          enddo
	  nreco=nreco+1
	  istat=def_fixhead_ful(nreco,stat,chan,net_id,real(dt),
     &                          begdat,ibtim,ibtsc,ibcor,ns,form,
     &                          12,0,0,0,obuf)
          if(swp) call swaphd(hbuf)
	  write(idsc1,rec=nreco) obuf
	endif
	close(idsc1)
	close(idsc2)
	stop
	end

	subroutine swaphd(ibuf)
c       SWAPHD -- Swap endian-related fields in mseed header before
c                 output to restore to endianness of data.  Unconditionally
c                 swaps.
	integer*2 ibuf(32)

c       Date (year)
	ibuf(11)=iswap(ibuf(11))
c       Date (doy)
	ibuf(12)=iswap(ibuf(12))
c       Date (decimicroseconds)
	ibuf(15)=iswap(ibuf(15))
c       # samples
	ibuf(16)=iswap(ibuf(16))
c       sample rate factor
	ibuf(17)=iswap(ibuf(17))
c       sample rate multiplier
	ibuf(18)=iswap(ibuf(18))
c       Time correction (4 byte)
	call i4swap(1,ibuf(21),ibuf(21))
c       data start
	ibuf(23)=iswap(ibuf(23))
c       first blockette
	ibuf(24)=iswap(ibuf(24))
c       blockette 1000 id, next
	ibuf(25)=iswap(ibuf(25))
	ibuf(26)=iswap(ibuf(26))
c       blockette 1001(?) id, next
	if (ibuf(26).ne.0) then
	   ibuf(27)=iswap(ibuf(27))
	   ibuf(28)=iswap(ibuf(28))
        endif
	end

	function ntwo(int)
	ntwo=2
	do i=1,31
	   if (ntwo .ge. int) return
	   ntwo = ntwo*2
        enddo
	write(0,*) '**NTWO:  Value too large'
	end

	subroutine make_qseed_use
c
	write(*,1000)
	stop
1000    format('Converts PASSCAL miniseed to Quanterra Steim1 format',
     &   /,'Usage:',
     &   /,'make_qseed <filename> [-a][-v][-R][-b size][-S code]',
     &                           '[-n net][-r id][-o dir]',
     &   /,'  <filename> - name of PASSCAL file',
     &   /,'  -a - output all block times',
     &   /,'  -b size - block size is size bytes (default 4096)',
     &   /,'  -c yyyy,mm,dd,hh,mm,ss,th - cut output at time',
     &   /,'  -l - force long names (always include hhmmss)',
     &   /,'  -n - assign network code (default YY)',
     &   /,'  -s x - set sample rate in sps (default from data)',
     &   /,'  -o - output directory (default: ./)',
     &   /,'  -r - set sample rate id (default rate-dependent)',
     &   /,'  -R - recalculate samples from Steim control words',
     &   /,'  -S <new_statcode> - change station code',
     &   /,'  -v - verbose block time output'
     &  )
	end
