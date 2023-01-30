	program extr_tape
c
	implicit integer(a-z)
	integer ibuf(4000),numw(300),nrec(30),rday(30),iopen(30)
	character stat*5,chan*3,chanw(30)*3,ans*1,sr_stat*5,sr_chan*3,
     &	net_id*2,ifile*132,ofile(30)*132,odir*32,cnum*6,tape*15,
     &	stats(30)*5,chans(30)*3,start_string*13,end_string*13,
     &	opt(10)*1,opt_string(10)*80,rec_typ*2,stat_new*5,string*80,
     &	times_file*132,dummy1*80,dummy2*80,dummy3*80
	real*4 rate,ratew(30),dt,dtw(30),rates(30)
	real*8 first_tim,dum_tim,new_tim(30),next_tim(30),new_end_tim(30),
     &	rnd_tim,start_tim,last_tim
	character version*18
c
	parameter (unit=1,unit1=10,unit2=2,unit3=3,unit4=4,unit5=5)
	equivalence (ibuf,cnum)
	data idt/4/,iend/0/
	data sr_stat,sr_chan/2*' '/
c
#	include "version.inc"
	write(*,*) 'extr_qic -',version
c
	jump_80=1
	irew=1
	icp=1
	jred=0
	idiv=0
	odir='./'
	stat_new=' '
c
	inarg=rd_options(nopt,opt,opt_string)
	if(inarg.eq.0) call extr_tape_use
	inarg=1
	start_tim=0.
	last_tim=3153600000.
	isel=0
	rec_typ='QT'
	ind_out=0
c
	istat=get_opt(' ',tape,nopt,opt,opt_string)
	istat=get_opt('d',rec_typ,nopt,opt,opt_string)
	if(rec_typ.ne.'RF'.and.rec_typ.ne.'CS') rec_typ='QT'
	istat=get_opt('b',start_string,nopt,opt,opt_string)
	if(istat.eq.1) then
	  isel=1
	  istat=change_time_string(start_string,itag,mon,iyear,ih,im,is)
	  ichd=datum(1,idy,iyear,mon,itag)
	  icha=abstim(1,istim,iyear,idy,ih,im,is)
	  start_tim=dble(istim)
	endif
	istat=get_opt('e',end_string,nopt,opt,opt_string)
        if(istat.eq.1) then
	  isel=1
	  istat=change_time_string(end_string,itagl,monl,iyearl,ihl,iml,isl)
          ichd=datum(1,idyl,iyearl,monl,itagl)
          icha=abstim(1,iltim,iyearl,idyl,ihl,iml,isl)
	  last_tim=dble(iltim)
	endif
	istat=get_opt('f',ans,nopt,opt,opt_string)
	if(ans.eq.'C'.or.ans.eq.'c') ind_out=1
	if(ans.eq.'A'.or.ans.eq.'a') ind_out=2
	if(ind_out.ne.0) isel=1
	if(rec_typ.eq.'RF') then
	  istat=get_opt('D',ans,nopt,opt,opt_string)
	  ittyp=0
	  if(istat.eq.1) ittyp=1
	  istat=get_opt('S',stat_new,nopt,opt,opt_string)
	  istat=get_opt('h',ans,nopt,opt,opt_string)
	  iskip=0
	  if(istat.eq.1) then
	    iskip=1
	    read(ans,*) nskip
	    if(nskip.eq.0) nskip=1
	  endif
	endif
	nbtin=1024
	ibt=0
	istat=get_opt('B',string,nopt,opt,opt_string)
	if(istat.eq.1) then
	  read(string,*) nbtin
	  ibt=1
	endif
	lenb=nbtin/4
	iselis=0
	istat=get_opt('T',times_file,nopt,opt,opt_string)
	if(istat.eq.1) then
	  iselis=1
	  open(unit2,file=times_file,status='old')
	endif
c
	call dvasn(unit,tape)
	if(rec_typ.eq.'RF') then
	  if(iskip.eq.1) then
	    nbyt=1024
	    do n=1,nskip
	      call rd(unit,ibuf,nbyt,ios,nbt)
	    enddo
	    write(*,*) 'Reftek tape or disk - first records skipped:',
     &	    nskip
	  endif
	  lenbi=256
	  lenbr=1024
	  if(ittyp.eq.1) lenbr=256
	  lenbo=256
	  if(ind_out.eq.1) lenbo=1024
	elseif(rec_typ.eq.'CS') then
	  lenbi=128
	  if(ibt.eq.0) nbtin=4096
	  lenbr=nbtin/4
	  lenbo=1024
	else
	  lenbi=1024
	  lenbr=nbtin/4
	  lenbo=1024
	endif
c
2000	if(iselis.eq.1) then
	  read(unit2,*,end=999) start_string,end_string
	  write(*,*) 'Extract event ',start_string(1:lenc(start_string)),' - ',
     &	  end_string(1:lenc(end_string))
	  istat=change_time_string(start_string,itag,mon,iyear,ih,im,is)
	  ichd=datum(1,idy,iyear,mon,itag)
	  icha=abstim(1,istim,iyear,idy,ih,im,is)
	  start_tim=dble(istim)
	  istat=change_time_string(end_string,itagl,monl,iyearl,ihl,iml,isl)
          ichd=datum(1,idyl,iyearl,monl,itagl)
          icha=abstim(1,iltim,iyearl,idyl,ihl,iml,isl)
	  last_tim=dble(iltim)
	  isel=1
	endif
c
100	nch=0
c
200	nch=1
	if(jred.eq.1) then
	  idiv=1
	else
	  idiv=0
	endif
	if(icp.eq.1) then
	  istat=0
	elseif(icp.eq.-1) then
	endif
	next_tim(nch)=0.
	iend=0
	iftim=0
c
#if defined(LINUX)
	if(isel.eq.1.and.rec_typ.eq.'RF'.and.ittyp.eq.1) then
	  irec=find_rec(rec_typ,unit,1,lenbi,start_tim,3)
	endif
#endif
c
1000	if(istat.eq.0) then
	  do n=1,30
	    nrec(n)=0
	    numw(n)=0
	    iopen(n)=0
	  enddo
	  num=0
	  nchs=0
	  chmod=0
c
	  do while (istat.eq.0)
1010	    if(iend.eq.0) then
	      istat=read_rec_tap(rec_typ,0,unit,0,lenbi,lenbr,lenbo,
     &	      ibuf,num,stat,chan,net_id,iftim,iftsc,timcr,rate,nsamp,
     &	      iof,nframe,neof)
c	write(*,*) 'main_read',istat,irec,num,stat,chan,iftim,iftsc,timcr,rate,nsamp,iend
	      if(istat.ne.4.and.(istat.eq.3.or.
     &	      (isel.eq.1.and.(chan(2:2).ne.'H'.or.chan(1:1).eq.'U'))))
     &	      goto 1010
	      timcr=0
	    endif
	    if(istat.eq.4) iend=1
c
	    ind=0
	    nch=0
	    do n=1,nchs
	      if(chan.eq.chans(n)) then
	        ind=1
	        nch=n
	      endif
	    enddo
	    if(ind.eq.0) then
	      nchs=nchs+1
	      stats(nchs)=stat
	      chans(nchs)=chan
	      rates(nchs)=rate
	      nch=nchs
	    endif
c
c	write(*,*) 'nch',nch,nchs
c	type '(8(1x,z8))',(ibuf(i),i=1,1024)
c	    if(istat.eq.4.or.istat.eq.2) iend=1
c
	    if(istat.eq.0) then
	      if(rec_typ.eq.'RF'.and.stat_new.ne.' ') then
	        istat=change_stat(stat_new,ibuf)
	        stat=stat_new
	      endif
	      iend=0
	      new_tim(nch)=dble(iftim)+dble(iftsc+timcr)*0.0001
	      if(rate.gt.0.0) then
	        dt=1./rate
	      else
	        dt=0.0
	      endif
	      if(nsamp.gt.0) then
	         new_end_tim(nch)=new_tim(nch)+nsamp*dt
	      else
	         new_end_tim(nch)=new_tim(nch)
	      endif
c	write(*,*) new_tim(nch),new_end_tim(nch),start_tim,last_tim,chan
	      if(new_end_tim(nch).lt.start_tim) goto 1010
	      if(iopen(nch).eq.0) then
	        iopen(nch)=1
	      endif
	      if(new_tim(nch).gt.last_tim.and.iopen(nch).ne.2) then
	        iopen(nch)=2
	        iend=1
	        do n=1,nchs
	          if(iopen(n).ne.2) iend=0
	        enddo
	        if(iend.eq.1) istat=2
	        goto 1010
	      else
	        if(iopen(nch).eq.2) goto 1010
	      endif
c
	      nrec(nch)=nrec(nch)+1
c
	      if(nrec(nch).gt.1.and.new_tim(nch)-next_tim(nch)
     &	      .gt.1000.*dtw(nch).and.dtw(nch).gt.0.0) then
c	write(*,*) nch,nrec(nch),new_tim(nch),next_tim(nch),dtw(nch)
	        itch=tfix(next_tim(nch),ietim,ietsc)
	        icha=abstim(0,ietim,iyrf,idyf,ihf,imf,isf)
	        ichd=datum(0,idyf,iyrf,monf,itgf)
	        write(*,9) '  End of input volume:',
     &	        stats(nch),chans(nch),rates(nch),itgf,monf,iyrf,ihf,imf,isf,
     &	        ietsc,istat
9	format(3(1x,a),f5.1,1x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,
     &	i4.4,i4/)
	        leno=lenc(ofile(nch))
	        write(*,'(1x,i5,2a)') nrec(nch),' records written on file ',
     &	        ofile(nch)(1:leno)
	        close (unit1+nch)
	        nrec(nch)=1
	      endif
c
	      if(nrec(nch).eq.1) then
	        nfil=nfil+1
	        ldat=1024-iof
	        if(icp.lt.1) istart=1
	        icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
                ichd=datum(0,idyf,iyrf,monf,itgf)
	        write(*,19) 'Start of input volume:',
     &	        stat,chan,rate,itgf,monf,iyrf,ihf,imf,isf,iftsc
19	format(3(1x,a),f5.1,1x,2(i2.2,1h/),i4.4,2x,2(i2.2,1h:),i2.2,1h.,
     &	i4.4)
	        if(jred.eq.1.and.rate.gt.40.) then
	          ratew(nch)=rate/float(idt)
	          dtw(nch)=1./ratew(nch)
	          chanw(nch)='BH'//chan(3:3)
	          ired=1
	        else
	          ratew(nch)=rate
	          if(rate.gt.0.0) then
	            dtw(nch)=1./ratew(nch)
	          else
	            dtw(nch)=0.0
	          endif
	          chanw(nch)=chan
	          ired=0
	        endif
c	        rday(nch)=idint(new_tim(nch))/86400
c	        dum_tim=new_tim(nch)+(ismp_st-1)*dt
	        dum_tim=new_tim(nch)
	        dum_tim=rnd_tim(0,dum_tim,dtw(nch),tcor_st)
	        icht=tfix(dum_tim,iftim,iftsc)
	        icha=abstim(0,iftim,iyrf,idyf,ihf,imf,isf)
	        ichd=datum(0,idyf,iyrf,monf,itgf)
		icyrf=cyear(iyrf)
	        if(ihf.eq.0.and.imf.eq.0.and.isf.eq.0.and.iftsc.eq.0) then
	          write(ofile(nch),'(2a,3i2.2,1h.,a)') odir(1:lenc(odir)),
     &	          stat(1:lenc(stat)),cyear(iyrf),monf,itgf,chanw(nch)
	        else
	          write(ofile(nch),'(2a,6i2.2,1h.,a)') odir(1:lenc(odir)),
     &	          stat(1:lenc(stat)),cyear(iyrf),monf,itgf,ihf,imf,isf,
     &	          chanw(nch)
	        endif
c
	        if(ind_out.lt.2) then
c		  lenbo = 1024 ! only with out the -xl flag (1024*4)
	          open(unit1+nch,name=ofile(nch),status='new',recl=lenbo*4,
     &	          access='direct',form='unformatted',iostat=ios)
	          if(ios.ne.0) then	! ios=128 file exists
	            write(*,*) 'Outfile exists - use extension .ext'
	            ofile(nch)=ofile(nch)(1:lenc(ofile(nch)))//'.ext'
	            open(unit1+nch,name=ofile(nch),status='new',recl=lenbo*4,
     &	            access='direct',form='unformatted')
	          endif
	        endif
	        if(ind_out.eq.2) then
	          write(ofile(nch),'(2a,1h.,a)') odir(1:lenc(odir)),
     &	          stat(1:lenc(stat)),chanw(nch)
	          ofile(nch)=ofile(nch)(1:lenc(ofile(nch)))//'.asc'
	          open(unit1+nch,name=ofile(nch),status='new')
	        endif
	      else
	        istart=0
	        ismp_st=1
	      endif
c
	      istat=wr_file(0,unit1+nch,ind_out,start_tim,last_tim,nch,
     &	      nrec(nch),lenbo,ibuf)
c
	      if(nsamp.gt.0) then
	         next_tim(nch)=new_tim(nch)+nsamp*dtw(nch)
	      else
	         next_tim(nch)=new_tim(nch)
	      endif
c	write(*,*) 'next',stat,chan,nch,nrec(nch),new_tim(nch),next_tim(nch),
c     &	rday(nch),idint(next_tim(nch))/86400
	      if(nrec(nch).gt.1.and.idint(next_tim(nch))/86400.gt.
     &	      rday(nch).and.dtw(nch).gt.0.0.and.isel.eq.0) then
	        itch=tfix(next_tim(nch),ietim,ietsc)
	        icha=abstim(0,ietim,iyrf,idyf,ihf,imf,isf)
	        ichd=datum(0,idyf,iyrf,monf,itgf)
	        write(*,9) '  End of input volume:',
     &	        stats(nch),chans(nch),rates(nch),itgf,monf,iyrf,ihf,imf,isf,
     &	        ietsc,istat
	        leno=lenc(ofile(nch))
	        write(*,'(1x,i5,2a)') nrec(nch),' records written on file ',
     &	        ofile(nch)(1:leno)
	        close (unit1+nch)
	        nrec(nch)=0
	      endif
	      rday(nch)=idint(next_tim(nch))/86400
	    endif
	    if(istat.eq.3) istat=0
	  enddo
c
c	  nrec=nrec+1
c	write(*,*) 'enddo',istat,nrec(nch),nch,ired,iend,idiv
	  do n=1,nchs
	  if(istat.ne.5.and.nrec(n).gt.0) then
c
	    istat=wr_file(999,unit1+n,ind_out,start_tim,last_tim,n,
     &	    nrec(n),lenbo,ibuf)
	    itch=tfix(next_tim(n),ietim,ietsc)
	    icha=abstim(0,ietim,iyrf,idyf,ihf,imf,isf)
	    ichd=datum(0,idyf,iyrf,monf,itgf)
	    write(*,9) '  End of input volume:',
     &	    stats(n),chans(n),rates(n),itgf,monf,iyrf,ihf,imf,isf,
     &	    ietsc,istat
	    leno=lenc(ofile(n))
	    write(*,'(1x,i5,2a)') nrec(n),' records written on file ',
     &	    ofile(n)(1:leno)
	    write(*,*)
	    close(unit1+n)
	  endif
	  enddo
	endif
	istat=0
c	if(idiv.eq.1.and.iend.ne.1) then
	if(iend.ne.1) then
	  if(icp.eq.1) goto 1000
	endif
c
900	if(icp.eq.0) then
	  if(more.eq.1) goto 100
	elseif(icp.eq.-1) then
	  goto 100
	endif
	if(iselis.eq.1) goto 2000
c
999	continue
c	istat=ieee_flags('clearall',dummy1,dummy2,dummy3)
	end
	subroutine extr_tape_use
c
	write(*,*) 'Extracts Quanterra station tapes and Reftek disks/tapes'
	write(*,*) 'Usage:'
	write(*,*) 'extr_tape <tape device> -[b,d,e,f,h,B,S]'
	write(*,*) '  -b <yymmdd_hhmmss> - start time (def=first rec)'
	write(*,*) '  -d <fcode> - input decoding format (def=QT)'
	write(*,*) '             QT - Quanterra MiniSEED (Steim1 or Steim2
     & compression)'
	write(*,*) '             CS - Comserv MiniSEED (Steim1 or Steim2
     & compression)'
	write(*,*) '             RF - REFTEK compression format (CO)'
	write(*,*) '  -e <yymmdd_hhmmss> - end time   (def=last rec)'
	write(*,*) '  -f <fcode> - output format code (def=input format)'
	write(*,*) '             c : Steim1 compression (4K)'
	write(*,*) '             a : PITSA ASCII'
	write(*,*) '  -h <nkb>- skip tape header (<nkb> records, for RF only)'
	write(*,*) '  -B <bsize> - blocksize in bytes (def=1024)'
	write(*,*) '  -D - Reftek disk drive (for random jump - Linux version
     &  only)'
	write(*,*) '  -S <new_statcode> - change station code (for RF only)'
c
	stop
	end
