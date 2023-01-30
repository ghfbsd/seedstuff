	program check_tape
c
	implicit integer(a-z)
	integer ibuf(1024), eof_marks, nrec(50,40),nchs(100),nch_tot(40)
	character cbuf*4096,stat*5,chan*3,tape*15,string*80,
     &	ans*1,ofile*132,s_chan(50,40)*3,s_stat(40)*5,net_id*2,rec_typ*2,
     &	opt(10)*1,opt_string(10)*80,dummy1*80,dummy2*80,dummy3*80
	real*4 rate,dt,rates(100)
	real*8 new_tim,next_tim(50,40)
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
	write(*,*) 'check_tape -',version
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call check_tape_use
	inarg=1
	istat=get_opt(' ',tape,nopt,opt,opt_string)
	istat=get_opt('d',rec_typ,nopt,opt,opt_string)
	if(rec_typ.ne.'RF'.and.rec_typ.ne.'CS'.and.
     &	rec_typ.ne.'SE') rec_typ='QT'
	if(rec_typ.eq.'RF') then
	  istat=get_opt('h',ans,nopt,opt,opt_string)
	  iskip=0
	  if(istat.eq.1) then
	    iskip=1
	    read(ans,*) nskip
	    if(nskip.eq.0) nskip=1
	  endif
	endif
c
	nbtin=1024
	ibt=0
	istat=get_opt('B',string,nopt,opt,opt_string)
	if(istat.eq.1) then
	  read(string,*) nbtin
	  ibt=1
	endif
	lenb=nbtin/4
c
	write(*,'(1x,a,/)') 'Rewind tape...'
	call dvasn(unit,tape)
	call rew(unit)
    
	ofile='CHECK_TAPE.CHK'
	open(unit1,name=ofile,status='unknown')
	write(*,'(/,1x,2a,/)') 'List file opened: ',ofile(1:lenc(ofile))
c
	if(rec_typ.eq.'RF') then
	  if(iskip.eq.1) then
	    nbyt=1024
	    do n=1,nskip
	      call rd(unit,ibuf,nbyt,ios,nbt)
	    enddo
	    write(*,*) 'Reftek tape or disk - first records skipped:',
     &	    nskip
	  endif
	  nlong=256
	  ndat=1024
	elseif(rec_typ.eq.'CS') then
	  nlong=128
	  if(ibt.eq.0) nbtin=4096
	  ndat=nbtin/4
	elseif(rec_typ.eq.'SE') then
	  nlong=1024
	  ndat=8192
	  rec_typ='QT'
	else
	  nlong=1024
	  ndat=nbtin/4
	endif
	lenbo=nlong
c
	nst_tot=0
	idiv=1
	nfil=0
	nfils=0
	do while(istat.ne.4)
	  do n=1,nch_tot(nst_tot)
	    nrec(n,nst_tot)=0
	  enddo
	  istat=0
	  num=0
	  do while (istat.eq.0)
1000	    istat=read_rec_tap(rec_typ,0,unit,unit1,nlong,ndat,lenbo,ibuf,
     &	    num,stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp,iof,nframe,
     &	    neof)
c	write(*,*) 'read',istat,num,stat,chan,iftim,iftsc,rate,nsamp
c
	    nst=0
	    n=0
	    do while (nst.eq.0.and.n.lt.nst_tot)
	      n=n+1
	      if(stat.eq.s_stat(n)) nst=n
	    enddo
	    if(nst.eq.0) then
	      nst_tot=nst_tot+1
	      s_stat(nst_tot)=stat
	      nst=nst_tot
	    endif
c
	    nch=0
	    n=0
	    do while (nch.eq.0.and.n.le.nch_tot(nst))
	      n=n+1
	      if(chan.eq.s_chan(n,nst)) nch=n
	    enddo
c	write(*,*) nst,nch,n,chan,s_chan(n,nst)
	    if(nch.eq.0) then
	      nch_tot(nst)=nch_tot(nst)+1
	      s_chan(nch_tot(nst),nst)=chan
	      nch=nch_tot(nst)
	      nrec(nch,nst)=0
	    endif
	    if(rate.gt.0.0) then
	      dt=1./rate
	    else
	      dt=0.0
	    endif
c	write(*,*) nst,nch,nst_tot,nch_tot(nst)
c
	    if(istat.eq.0) then
	      nrec(nch,nst)=nrec(nch,nst)+1
	      new_tim=dble(iftim)+dble(iftsc)*0.0001
	      if(nrec(nch,nst).eq.1) then
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
	        if(abs(new_tim-next_tim(nch,nst)).gt.dt.and.dt.gt.0.0) then
	        icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	        ichd=datum(0,idyf,iyrf,monf,itgf)
	        intim=next_tim(nch,nst)
	        intsc=(next_tim(nch,nst)-dble(intim))*10000.
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
	      if(nsamp.gt.0.and.dt.gt.0.0) then
	         next_tim(nch,nst)=new_tim+nsamp*dt
	      else
	         next_tim(nch,nst)=new_tim
	      endif
c
	      if(idiv.eq.1.and.idint(next_tim(nch,nst))/86400.gt.rday
     &	      .and.rate.gt.0.0) then
	        intim=next_tim(nch,nst)
	        intsc=(next_tim(nch,nst)-dble(intim))*10000.
	        icha=abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	        ichd=datum(0,idyn,iyrn,monn,itgn)
	        write(*,39) 'New day: ',itgn,monn,iyrn,idyn
	        write(unit1,39) 'New day:',itgn,monn,iyrn,idyn
39	format(1x,a,2x,2(i2,1h/),i4,i8)
	        rday=idint(next_tim(nch,nst))/86400
	      endif
	    endif
c
c	    if(istat.eq.2) then
c	      ichk=dec_trailer(unit,unit1)
c	      istat=4
c	    endif
	  enddo
	  if(nfil.ne.nfils) then
	    do nn=1,nfil
	      nch=nchs(nn)
	      ichk=tfix(next_tim(nch,nst),iftim,iftsc)
	      icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	      ichd=datum(0,idyf,iyrf,monf,itgf)
	      write(*,29) ' End of stream',nn,':',
     &	      stat,s_chan(nch,nst),rates(nn),itgf,monf,iyrf,ihf,imf,
     &	      isf,iftsc,istat
	      write(unit1,29) ' End of stream',nn,':',
     &	      stat,s_chan(nch,nst),rates(nn),itgf,monf,iyrf,ihf,imf,
     &	      isf,iftsc,istat
29	format(1x,a,i3,3(1x,a),f5.1,1x,2(i2,1h/),i4,2x,2(i2,1h:),i2,1h.,2i4/)
            enddo
c	    nfils=nfil
	    nfil=0
	  endif
	enddo
	close(unit1)
c
c	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)

	end
	subroutine check_tape_use
c
	write(*,*) 'Checks Quanterra station tapes and REFTEK disks/tapes'
	write(*,*) 'Usage:'
	write(*,*) 'check_tape <tape device> -[d,h,B]'
	write(*,*) '  -d <fcode> - input decoding format (def=QT)'
	write(*,*) '             QT - Quanterra MiniSEED (Steim1 or Steim2
     & compression)'
	write(*,*) '             CS - Comserv MiniSEED (Steim1 or Steim2
     & compression)'
	write(*,*) '             RF - REFTEK compression format (CO)'
	write(*,*) '             SE - Full SEED tapes (32k records)'
	write(*,*) '  -h <nkb>- skip tape header (<nkb> records, for RF only)'
	write(*,*) '  -B <bsize> - input blocksize in bytes (def=1024)'
c
	stop
	end
