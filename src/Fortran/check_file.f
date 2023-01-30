	program check_file
c
	implicit integer(a-z)
	integer ibuf(1024),nrec(50),nchs(50),idx(50)
	character cbuf*4096,stat*5,chan*3,seed_in*132,file_list*132,
     &	ans*1,ofile*132,s_chan(50)*3,net_id*2,rec_typ*2,opt(10)*1,
     &	opt_string(10)*80,string*132,stat_s(50)*5,chan_s(50)*3,
     &	dummy1*80,dummy2*80,dummy3*80
	real*4 rate,dt,rates(50)
	real*8 new_tim,next_tim(50),start_tim(10000,50),end_tim
     &	(10000,50)
	character version*18
	equivalence (ibuf,cbuf)
	parameter (unit=1,unit1=2)
c	data nch_tot,s_chan/32,'HHZ','HHN','HHE','HX1','BHZ','BHN','BHE',
c     &	'BX1','LHZ','LHN','LHE','VHZ','VHN','VHE','UHZ','UHN','UHE',
c     &	'UMZ','UMN','UME','UCI','UTI','UFC','UFP','UCD','ACD','ACE',
c     &	'ACU','AFC','AFP','AFU','LOG'/
c
 	include 'version.inc'
	write(*,*) 'check_file -',version
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call check_file_use
	inarg=1
	istat=get_opt(' ',seed_in,nopt,opt,opt_string)
	istat=get_opt('d',rec_typ,nopt,opt,opt_string)
	if(rec_typ.ne.'RF') rec_typ='QT'
	istat=get_opt('B',string,nopt,opt,opt_string)
	nbtin=4096
	if(istat.eq.1) read(string,*) nbtin
	istat=get_opt('F',string,nopt,opt,opt_string)
	imult=0
	if(istat.eq.1) then
	  imult=1
	  file_list=string(1:lenc(string))
	  write(*,*) 'Open list file: ',file_list(1:lenc(file_list))
	  open(unit1,file=file_list,status='old')
	endif
c
c
	if(rec_typ.eq.'QT') then
	  lenb=nbtin/4
	elseif(rec_typ.eq.'RF') then
	  lenb=256
	  nbtin=1024
	endif
c
	idiv=1
c
	nfil=0
	nfils=0
	istat=0
	nch_tot=0
c
	do while(istat.eq.0)
1000	  if(imult.eq.1) read(unit1,'(a)',end=900) seed_in
	  open(unit,file=seed_in,status='old',access='direct',form=
     &	  'unformatted',recl=nbtin)
	  write(*,*) 'Opened data file: ',seed_in(1:lenc(seed_in))
	  istat=0
	  irec=0
	  do n=1,nch_tot
	    nrec(n)=0
	  enddo
	  istat=0
	  num=0
	  do while (istat.eq.0)
1010	    irec=irec+1
	    istat=read_rec_fil(rec_typ,1,unit,lenb,lenb,ibuf,irec,num,
     &      form,stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp,iof,
     &      nframe)
c	write(*,*) 'read',istat,num,stat,chan,iftim,iftsc,rate,nsamp
	    if(istat.eq.3) goto 1010
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
	    if(nrec(nch).eq.0) then
	      inew=1
	      if(nfil.gt.0) then
	        do n=1,nfil
	          if(nchs(n).eq.nch) inew=0
	        enddo
	      endif
	    endif
c
	    if(istat.eq.0) then
	      nrec(nch)=nrec(nch)+1
	      new_tim=dble(iftim)+dble(iftsc)*0.0001
	      if(nrec(nch).eq.1.and.inew.eq.1) then
	        nfil=nfil+1
	        nchs(nfil)=nch
	        rates(nfil)=rate
	        call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	        call datum(0,idyf,iyrf,monf,itgf)
	        write(*,9) 'Start of stream',nfil,':',
     &	          stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
c	        write(unit1,9) 'Start of stream',nfil,':',
c     &	        stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
9	        format(1x,a,i3,3(1x,a),f5.1,1x,2(i2,1h/),
     &                 i4,2x,2(i2,1h:),i2,1h.,i4)
	        nchgs=0
	        if(rday.lt.idint(new_tim)/86400) rday=idint(new_tim)/86400
	        idx(nch)=idx(nch)+1
	        start_tim(idx(nch),nch)=new_tim
	        stat_s(nch)=stat
	        chan_s(nch)=chan
c	write(*,*) 'new channel',nch,idx(nch),start_tim(idx(nch),nch),
c     &	stat_s(nch),chan_s(nch)
	      else
	        if(abs(new_tim-next_tim(nch)).gt.dt.and.dt.gt.0.0) then
	        call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	        call datum(0,idyf,iyrf,monf,itgf)
	        intim=next_tim(nch)
	        intsc=(next_tim(nch)-dble(intim))*10000.
	        call abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	        call datum(0,idyn,iyrn,monn,itgn)
	          write(*,19) 'Time-gap:',itgf,monf,iyrf,ihf,imf,isf,
     &	          iftsc,'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc,
     &	          'in stream',stat,chan
c	          write(unit1,19) 'Time-gap:',itgf,monf,iyrf,ihf,imf,isf,
c     &	          iftsc,'instead of',itgn,monn,iyrn,ihn,imn,isn,intsc,
c     &	          'in stream',stat,chan
19	format(2(1x,a,2x,2(i2,1h/),i4,2x,2(i2,1h:),i2,1h.,i4),3(1x,a))
	        end_tim(idx(nch),nch)=next_tim(nch)
	        idx(nch)=idx(nch)+1
	        start_tim(idx(nch),nch)=new_tim
c	write(*,*) 'gap',nch,idx(nch),end_tim(idx(nch)-1,nch),start_tim
c     &	(idx(nch),nch),stat_s(nch),chan_s(nch)
	        endif
	      endif
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
	        call abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	        call datum(0,idyn,iyrn,monn,itgn)
	        write(*,39) 'New day: ',itgn,monn,iyrn,idyn
c	        write(unit1,39) 'New day:',itgn,monn,iyrn,idyn
39	format(1x,a,2x,2(i2,1h/),i4,i8)
	        rday=idint(next_tim(nch))/86400
	      endif
	    endif
	  enddo
c
	  if(imult.eq.1) then
	    close(unit)
	    goto 1000
	  endif
c
900	  if(nfil.ne.nfils) then
	    do nn=1,nfil
	      nch=nchs(nn)
	      call tfix(next_tim(nch),iftim,iftsc)
	      call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	      call datum(0,idyf,iyrf,monf,itgf)
	      write(*,29) ' End of stream',nn,':',
     &	        stat,s_chan(nch),rates(nn),itgf,monf,
     &          iyrf,ihf,imf,isf,iftsc,istat
c	      write(unit1,29) ' End of stream',nn,':',
c     &	        stat,s_chan(nch),rates(nn),itgf,monf,iyrf,ihf,imf,isf,iftsc,
c     &	        istat
29	      format(1x,a,i3,3(1x,a),f5.1,1x,2(i2,1h/),
     &               i4,2x,2(i2,1h:),i2,1h.,2i4/)
 	      end_tim(idx(nch),nch)=next_tim(nch)
c 	write(*,*) 'end',nch,idx(nch),end_tim(idx(nch),nch),
c     &	stat_s(nch),chan_s(nch)
          enddo
	    nfils=nfil
c
	    write(*,'(//,1x,a,/)') 'File summary:'
	    do nn=1,nch_tot
	      nch=0
	      if(chan_s(nn).eq.s_chan(nn)) nch=nn
c	      write(*,'(1x,3(1x,a))') 'Stream:',stat_s(nn),chan_s(nn)
	      if(nch.gt.0) then
	        do n=1,idx(nch)
	          iftim=start_tim(n,nch)
	          iftsc=(start_tim(n,nch)-dble(iftim))*10000.
	          call abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	          call datum(0,idyf,iyrf,monf,itgf)
	          intim=end_tim(n,nch)
	          intsc=(end_tim(n,nch)-dble(intim))*10000.
	          call abstim(0,intim,iyrn,idyn,ihn,imn,isn)
	          call datum(0,idyn,iyrn,monn,itgn)
	          write(*,49) stat_s(nch),chan_s(nch),itgf,monf,iyrf,ihf,
     &              imf,isf,iftsc,'-',itgn,monn,iyrn,ihn,imn,isn,intsc
49	          format(1x,2(1x,a),2x,2(i2.2,1h/),
     &              i4,2x,2(i2.2,1h:),i2.2,1h.,i4.4,2x,a,2x,
     &              2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,i4.4)
	        enddo
	      endif
	    enddo
	  endif
	enddo
	if(imult.eq.1) close(unit1)
c
	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)

	end
	subroutine check_file_use
c
	write(*,*) 'Checks Quanterra station files and REFTEK files'
	write(*,*) 'Usage:'
	write(*,*) 'check_file <filename> -[d,h]'
	write(*,*) '  -d <fcode> - input decoding format (def=QT)'
	write(*,*) '             QT - Quanterra MiniSEED (Steim1 or Steim2
     & compression)'
	write(*,*) '             RF - REFTEK compression format (CO)'
	write(*,*) '  -B <bsize> - blocksize in bytes'
	write(*,*) '  -F <filename> - list of input files'
c
	stop
	end
