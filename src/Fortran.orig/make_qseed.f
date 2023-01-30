	program make_qseed
c
	implicit integer(a-z)
	integer ibuf(1024),obuf(1024)
	byte obbuf(4096)
	character buffer*4096,seed_in*132,seed_out*132,idir*32,odir*32,
     &	dummy1*80,dummy2*80,dummy3*80
	equivalence(ibuf,buffer),(obuf,obbuf)
	parameter (idsc1=1,idsc2=2)
c
	character stat*5,chan*3,net_id*2,strng*1,
     &  stat_net(14)*5,chan_net(9)*3,
     &	stat_sr*5,chan_sr*3,stat_new*5,
     &  stream*1,opt(10)*1,opt_string(10)*80
	integer timcor
	real*4 rate
	real*8 dt, anf_tim,end_tim,next_tim,last_next,numta,numte
c
	character version*18
 	include 'version.inc'
	write(*,*) 'make_qseed -',version
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call make_qseed_use
	inarg=1
	istat=get_opt(' ',seed_in,nopt,opt,opt_string)
	isearch=0
	iall=0
	stat_new='STAT'
	inst=get_opt('S',stat_new,nopt,opt,opt_string)
	iblk=get_opt('s',dummy1,nopt,opt,opt_string)
c
	mode=0
	idir='./'
	odir='./'
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
c	  write(*,*) 'next_sfile: ',istfl,seed_in
	  if(istfl.eq.2) goto 1000
	  mode=1
	endif
c
	nbtin=4096-3*1024*iblk
	open(idsc2,name=seed_in,status='old',form='unformatted',access=
     &	'direct',recl=nbtin)
	nfl=lenc(seed_in)
	write(*,'(1x,2a)') 'Opened file: ',seed_in(1:nfl)
	nrec=0
	nreco=0
	diff=0
	nout=0
c
100	nrec=nrec+1
        read(idsc2,rec=nrec,iostat=ios) buffer(1:nbtin)
        if(ios.ne.0) goto 900

	buffer(1:6)='000000'
	istat=dec_fixhead(num,stat,chan,net_id,rate,begdat,iftim,iftsc,
     &	timcor,nsamp,form,reclen,buffer)
c
	if(rate.lt.1.) rate=1./rate
	net_id='YY'
	if(stat_new.ne.'STAT') stat=stat_new
c	if(inst.eq.1) then
c	  stream='S'
c	else
	  if(rate.gt.20.0) stream='H'
	  if(rate.le.20.0) stream='B'
	  if(rate.lt.10.0) stream='L'
c	endif
	if(index('14',chan(3:3)).ne.0) chan=stream//'HZ'
	if(index('25',chan(3:3)).ne.0) chan=stream//'HN'
	if(index('36',chan(3:3)).ne.0) chan=stream//'HE'
c
	if(nrec.eq.1) then
	  icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	  ichd=datum(0,idyf,iyrf,monf,itgf)
	    if(ihf.eq.0.and.imf.eq.0.and.isf.eq.0.and.iftsc.eq.0) then
	    write(seed_out,'(2a,3i2.2,1h.,a)') odir(1:lenc(odir)),
     &	    stat(1:lenc(stat)),cyear(iyrf),monf,itgf,chan
	  else
	    write(seed_out,'(2a,6i2.2,1h.,a)') odir(1:lenc(odir)),
     &	    stat(1:lenc(stat)),cyear(iyrf),monf,itgf,ihf,imf,isf,
     &	    chan
	  endif
	  open(idsc1,name=seed_out,status='unknown',form='unformatted',
     &	  access='direct',recl=4096)
	endif
c
	istat=def_fixhead_ful(nrec,stat,chan,net_id,1./rate,begdat,iftim,iftsc,
     &	timcor,nsamp,form,12,timq,mysec,nfrm,ibuf)
	anf_tim=dble(iftim)+dble(iftsc+timcor)*0.0001
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
	if(nrec.gt.1) diff=(next_tim-anf_tim)*10000.
	if(iabs(diff).gt.9999) diff=9999
c	write(*,*) next_tim,anf_tim,diff
	if(nrec.gt.1.and.abs(next_tim-anf_tim).gt.dt*0.5.and.dt.gt.0.0) then
	  itch=tfix(next_tim,intim,intsc)
	  icha=abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	  ichd=datum(0,idyn,iyrn,monn,itgn)
	  if(ihead.ne.1)
     &	  write(*,'(2(1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),
     &    i2.2,1h.,i4.4))') 
     &	  'Time-gap in input file: ',itga,mona,iyra,iha,ima,isa,iftsc,
     &	  'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc
c
	  if(nout.gt.0) then
	    nbuf=nout/256
c	    istat=make_4096(1024,nbuf,obbuf)
            istat=make_4096(999,1024,nbuf,nb_tot,obbuf,
     &         nreco,stat,chan,net_id,rate,0,iftim,tftsc,timcor,
     &         nsamp,form,lrecl,timq,mysec,nfrm)
	    nreco=nreco+1
	    write(idsc1,rec=nreco) obuf
	    nout=0
	  endif
	endif
c
	if(iall.eq.1) then
	  write(*,'(i6,3(1x,a),f6.2,i6,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),
     &    i2.2,1h.,i4.4,2x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,
     &    i5)')
     &    num,stat,chan,net_id,rate,nsamp,itga,mona,iyra,iha,ima,isa,
     &	  iftsc,'to',itge,mone,iyre,ihe,ime,ise,ietsc,diff
	elseif(iall.eq.2) then
	  write(*,*) num,' ',stat,chan,' ',net_id,rate,itga,mona,iyra,iha,ima,isa,
     &	  iftsc,timcor,nsamp
	elseif(nrec.eq.1) then
	  write(*,'(1x,a,2x,2a,1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),
     &       i2.2,1h.,i4.4)') 'Start of first record:',
     &	  stat,chan,net_id,itga,mona,iyra,iha,ima,isa,iftsc
	  write(*,'(1x,a,i2.2)') 'Format code: ',form
	endif
c
	last_next=next_tim
	next_tim=end_tim
c
	do n=1,nbtin/4
	  nout=nout+1
	  obuf(nout)=ibuf(n)
	enddo
c
	if(nout.ge.1024) then
c	  istat=make_4096(1024,4,obbuf)
          istat=make_4096(0,1024,1024,nb_tot,obuf,
     &         nreco,stat,chan,net_id,rate,0,iftim,tftsc,timcor,
     &         nsamp,form,lrecl,timq,mysec,nfrm)
	  nreco=nreco+1
	  write(idsc1,rec=nreco) obuf
	  nout=0
	endif
c
	num=num+1
	goto 100
c	
900	if(iall.eq.0) then
	  write(*,'(1x,a,2x,2a,1x,a,2x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),
     &    i2.2,1h.,i4.4,i5)') '   End of last record:',
     &	  stat,chan,net_id,itge,mone,iyre,ihe,ime,ise,ietsc
	endif
c
	if(nout.gt.0) then
c	  nbuf=nout/256
c	  istat=make_4096(1024,nbuf,obbuf)
          istat=make_4096(999,1024,nout,nb_tot,obuf,
     &         nreco,stat,chan,net_id,rate,0,iftim,tftsc,timcor,
     &         nsamp,form,lrecl,timq,mysec,nfrm)
	  nreco=nreco+1
	  write(idsc1,rec=nreco) obuf
	endif
	close(idsc1)
	close(idsc2)
	if(isearch.eq.1) goto 1000
c
999	continue
c	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)
	stop
	end
	subroutine make_qseed_use
c
	write(*,*) 'Converts PASSCAL miniseed to Quanterra type Steim1 format'
	write(*,*) 'Usage:'
	write(*,*) 'make_qseed <filename> [-s] -[S code]'
	write(*,*) '  <filename> - name of PASSCAL file'
	write(*,*) '  -s - small (1024) block size (otherwise 4096)'
	write(*,*) '  -S <new_statcode> - change station code'
c
	stop
	end
