	program swap_mseed_main	! Linux version only
c
	implicit integer(a-z)
	integer ibuf(1024)
	character buffer*4096,seed_in*132,seed_out*132,idir*132,odir*132
	parameter (idsc1=1,idsc2=2)
c
	character stat*5,chan*3,net_id*2,strng*1,stat_sr*5,chan_sr*3,
     &	opt(10)*1,opt_string(10)*80,string*132
	integer timcor
	real*4 rate,dtw
	real*8 dt, anf_tim,end_tim,next_tim,last_next,numta,numte
	equivalence(ibuf,buffer)
c
	character version*18
#	include "statnet.inc"
#	include "version.inc"
	write(*,*) 'swap_mseed -',version
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call swap_mseed_use
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
	nbtin=4096
	idir='./'
	odir='./'
c
	if(nopt.gt.1) then
	  istat=get_opt('i',idir,nopt,opt,opt_string)
	  ln=lenc(idir)
	  if(idir(ln:ln).ne.'/') idir(ln+1:ln+1)='/'
	  istat=get_opt('o',odir,nopt,opt,opt_string)
	  ln=lenc(odir)
	  if(odir(ln:ln).ne.'/') odir(ln+1:ln+1)='/'
	  istat=get_opt('B',string,nopt,opt,opt_string)
	  if(istat.eq.1) read(string,*) nbtin
	  istat=get_opt('a',strng,nopt,opt,opt_string)
	  if(istat.eq.1) iall=1
c	  if(strng.eq.'o'.or.strng.eq.'O') iall=2
	endif
c
	mode=0
	nst=0
	nch=9
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
c	write(*,*) mode,idir,stat,chan,seed_in,numta,numte
	  istfl=next_sfile(mode,idir,stat_sr,chan_sr,seed_in,numta,numte)
c	write(*,*) 'next_sfile: ',istfl,seed_in
	  if(istfl.eq.2) goto 1000
	  mode=1
	endif
c
c	if(inarg.eq.0) then
c	  write(*,'(1x,a,$)') 'Input file:_'
c	  read(*,'(a)') seed_in
c	endif
c
cc	nbtin=4096
cc	lenr=1024
	lenr=nbtin
	nrep=1
	open(idsc1,name=seed_in,status='old',form='unformatted',access=
     &	'direct',recl=lenr)
	seed_out=odir(1:lenc(odir))//seed_in(1:lenc(seed_in))//'.swap'
	open(idsc2,name=seed_out,status='unknown',form='unformatted',access=
     &	'direct',recl=lenr)
	nfl=lenc(seed_in)
	write(*,'(1x,2a)') 'Opened file: ',seed_in(1:nfl)
	nrec=0
	irec=0
	diff=0
c
100	nrec=nrec+1
	do n=1,nrep
	  ia=(n-1)*nbtin+1
	  ie=n*nbtin
	  read(idsc1,rec=nrec,iostat=ios) buffer(ia:ie)
	  if(ios.ne.0) goto 900
	enddo
	if(buffer(7:7).ne.'D') then
	  write(*,*) 'No data record',nrec,'  ',buffer(1:7)
	  goto 100
	endif
c	istat=make_4096(512,8,ibuf)
	istat=swap_mseed(2,ibuf)
	istat=dec_fixhead(num,stat,chan,net_id,rate,begdat,iftim,iftsc,
     &	timcor,nsamp,form,reclen,ibuf)
c	write(*,*) 'n dec',istat,irec,num,stat,chan,iftim,iftsc,timcor,rate,
c     &	nsamp,form
	dtw=0.0
	if(rate.gt.0.0) dtw=1./rate
	if(form.ne.11) form=10
	istat=def_fixhead(num,stat,chan,net_id,dtw,begdat,iftim,iftsc,
     &	timcor,nsamp,form,reclen,ibuf)
c	write(*,*) 'ndef',istat,irec,num,stat,chan,iftim,iftsc,timcor,rate,
c     &	nsamp,form
	anf_tim=iftsc+timcor
	anf_tim=anf_tim*dble(0.0001)
	anf_tim=anf_tim+dble(iftim)
c	write(*,*) 'anf_tim',anf_tim
	if(rate.gt.0.0) then
	  dt=1./rate
	  end_tim=anf_tim+nsamp*dt
	else
	  dt=0.0
	  end_tim=anf_tim
	endif
	itch=tfix(anf_tim,iftim,iftsc)
	icha=abstim(0,iftim,iyra,idya,iha,ima,isa)
	ichd=datum(0,idya,iyra,mona,itga)
	itch=tfix(end_tim,ietim,ietsc)
	icha=abstim(0,ietim,iyre,idye,ihe,ime,ise)
	ichd=datum(0,idye,iyre,mone,itge)
c
	irec=irec+1
	if(irec.gt.1) diff=(next_tim-anf_tim)*dble(10000.)
	if(iabs(diff).gt.9999) diff=9999
	if(nrec.gt.1.and.abs(next_tim-anf_tim).gt.dt*0.5.and.dt.gt.0.0) then
	  itch=tfix(next_tim,intim,intsc)
	  icha=abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	  ichd=datum(0,idyn,iyrn,monn,itgn)
	  if(ihead.ne.1)
     &	  write(*,9) 
     &	  'Time-gap in input file: ',itga,mona,iyra,iha,ima,isa,iftsc,
     &	  'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc
9	format(2(1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4))
	endif
c
	if(iall.eq.1) then
	  write(*,19)
     &    num,stat,chan,net_id,rate,nsamp,itga,mona,iyra,iha,ima,isa,
     &	  iftsc,'to',itge,mone,iyre,ihe,ime,ise,ietsc,diff
19	format(i6,3(1x,a),f6.2,i6,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,
     &	  i4.4,2x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,i5)
	elseif(iall.eq.2) then
	  write(*,*) num,' ',stat,chan,' ',net_id,rate,itga,mona,iyra,iha,ima,isa,
     &	  iftsc,timcor,nsamp
	elseif(nrec.eq.1) then
	  write(*,29) 'Start of first record:',
     &	  itga,mona,iyra,iha,ima,isa,iftsc
29	format(1x,a,9x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,i5)
	endif
c
	last_next=next_tim
	next_tim=end_tim
	write(idsc2,rec=nrec) buffer(1:lenr)
	num=num+1
	goto 100
c	
900	if(iall.eq.0) then
	  write(*,29) '   End of last record:',
     &	  itge,mone,iyre,ihe,ime,ise,ietsc
	endif
c
	close(idsc1)
	close(idsc2)
	if(isearch.eq.1) goto 1000
c
999	continue
c	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)
	stop
	end
	subroutine swap_mseed_use
c
	write(*,*) 'Swaps bytes for use of PASSCAL programs on Linux
     & systems'
	write(*,*) 'Usage:'
	write(*,*) 'swap_mseed <filename> -[a]'
	write(*,*) '  <filename> - full filename or "*" or "<statcode>*"'
	write(*,*) '  -a - prints all records'
	write(*,*) '  -B <bsize> - blocksize in bytes'
c
	stop
	end
