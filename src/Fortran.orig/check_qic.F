	program check_qic
c
	implicit integer(a-z)
	integer ibuf(1024), eof_marks, nrec(29),nchs(30)
	character cbuf*4096,stat*5,chan*3,tape*15,string1*80,rec_typ*2,
     &	ans*1,ofile*132,s_chan(29)*3,net_id*2,opt(10)*1,opt_string
     &	(10)*80,dummy1*80,dummy2*80,dummy3*80
	real*4 rate,dt,rates(30)
	real*8 new_tim,next_tim(29)
	character version*18
	COMMON /COUNTS/ eof_marks
	equivalence (ibuf,cbuf)
	parameter (unit=1,unit1=2)
c	data nch_tot,s_chan/29,'VHZ','VHN','VHE','LHZ','LHN','LHE','BHZ',
c     &	'BHN','BHE','BX1','HHZ','HHN','HHE','HX1','UHZ','UHN','UHE',
c     &	'UMZ','UMN','UME','UCI','UTI','UFC','UFP','UCD','ACU','AFU',
c     &	'ACE','LOG'/
c
#	include "version.inc"
	write(*,*) 'check_qic -',version
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call check_qic_use
	inarg=1
	istat=get_opt(' ',tape,nopt,opt,opt_string)
	nbtin=1024
	if(nopt.gt.1) then
	  istat=get_opt('B',string1,nopt,opt,opt_string)
	  if(istat.eq.1) read(string1,*) nbtin
	endif
	lenbi=1024
	lenbr=nbtin/4
	lenbo=lenbi
	rec_typ='QT'
c
	eof_marks = 0
	write(*,'(1x,a,/)') 'Rewind tape...'
	call dvasn(unit,tape)
	call rew(unit)
    	ofile='CHECK_QIC.CHK'
	open(unit1,name=ofile,status='unknown')
	write(*,'(/,1x,2a,/)') 'List file opened: ',ofile(1:lenc(ofile))
c
	idiv=1
	nfil=0
	nfils=0
	do while(istat.ne.4)
	  do n=1,nch_tot
	    nrec(n)=0
	  enddo
	  istat=0
	  num=0
	  do while (istat.eq.0)
	    istat=read_rec_tap(rec_typ,0,unit,unit1,lenbi,lenbr,lenbo,ibuf,
     &	    num,stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp,iof,nframe,
     &	    neof)
c	write(*,*) 'read',istat,num,stat,chan,iftim,iftsc,rate,nsamp
c
	    nch=0
	    n=0
	    do while (nch.eq.0.and.n.le.nch_tot)
	      n=n+1
	      if(chan.eq.s_chan(n)) nch=n
	    enddo
	    if(nch.eq.0) then
	      nch_tot=nch_tot+1
	      s_chan(nch_tot)=chan
	      nch=nch_tot
	      nrec(nch)=0
	    endif
	    if(rate.gt.0.0) then
	      dt=1./rate
	    else
	      dt=0.0
	    endif
c
	    if(istat.eq.0) then
	      nrec(nch)=nrec(nch)+1
	      new_tim=dble(iftim)+dble(iftsc)*0.0001
	      if(nrec(nch).eq.1) then
	        nfil=nfil+1
	        nchs(nfil)=nch
	        rates(nfil)=rate
	        icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	        ichd=datum(0,idyf,iyrf,monf,itgf)
	        write(*,9) 'Start of stream',nfil,':',
     &	        stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
	        write(unit1,9) 'Start of stream',nfil,':',
     &	        stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
9	format(1x,a,i3,3(1x,a),f5.1,1x,2(i2,1h/),i4,2x,2(i2,1h:),i2,1h.,i4)
	        nchgs=0
	        if(rday.lt.idint(new_tim)/86400) rday=idint(new_tim)/86400
	      else
	        if(abs(new_tim-next_tim(nch)).gt.dt.and.dt.gt.0.0) then
	        icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	        ichd=datum(0,idyf,iyrf,monf,itgf)
	        intim=next_tim(nch)
	        intsc=(next_tim(nch)-dble(intim))*10000.
	        icha=abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	        ichd=datum(0,idyn,iyrn,monn,itgn)
	          write(*,19) 'Time-gap:',itgf,monf,iyrf,ihf,imf,isf,
     &	          iftsc,'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc,
     &	          'in stream',stat,chan
	          write(unit1,19) 'Time-gap:',itgf,monf,iyrf,ihf,imf,isf,
     &	          iftsc,'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc,
     &	          'in stream',stat,chan
19	format(2(1x,a,2x,2(i2,1h/),i4,2x,2(i2,1h:),i2,1h.,i4),3(1x,a))
	        endif
	      endif
c
	      if(nsamp.gt.0.and.dt.gt.0.0) then
	         next_tim(nch)=new_tim+nsamp*dt
	      else
	         next_tim(nch)=new_tim
	      endif
c
	      if(idiv.eq.1.and.idint(next_tim(nch))/86400.gt.rday
     &	      .and.rate.gt.0.0) then
	        intim=next_tim(nch)
	        intsc=(next_tim(nch)-dble(intim))*10000.
	        icha=abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	        ichd=datum(0,idyn,iyrn,monn,itgn)
	        write(*,39) 'New day: ',itgn,monn,iyrn,idyn
	        write(unit1,39) 'New day:',itgn,monn,iyrn,idyn
39	format(1x,a,2x,2(i2,1h/),i4,i8)
	        rday=idint(next_tim(nch))/86400
	      endif
	    endif
c
	    if(istat.eq.2) then
c	      ichk=dec_trailer(unit,unit1)
	      istat=4
	    endif
	  enddo
	  if(nfil.ne.nfils) then
	    do nn=1,nfil
	      nch=nchs(nn)
	      ichk=tfix(next_tim(nch),iftim,iftsc)
	      icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	      ichd=datum(0,idyf,iyrf,monf,itgf)
	      write(*,29) ' End of stream',nn,':',
     &	      stat,s_chan(nch),rates(nn),itgf,monf,iyrf,ihf,imf,isf,iftsc,
     &	      istat
	      write(unit1,29) ' End of stream',nn,':',
     &	      stat,s_chan(nch),rates(nn),itgf,monf,iyrf,ihf,imf,isf,iftsc,
     &	      istat
29	format(1x,a,i3,3(1x,a),f5.1,1x,2(i2,1h/),i4,2x,2(i2,1h:),i2,1h.,2i4/)
            enddo
	    nfils=nfil
	  endif
	enddo
	close(unit1)
c
c	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)

	end
	subroutine check_qic_use
c
	write(*,*) 'Checks Quanterra station tapes'
	write(*,*) 'Usage:'
	write(*,*) 'check_qic <tape device> -[B]'
	write(*,*) '  -B <bsize> - blocksize in bytes (def=1024)'
c
	stop
	end
