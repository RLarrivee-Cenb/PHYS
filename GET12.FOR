C PR1769 Modified scratch program to help view data in binary file
	structure /bozo2/
c
		character*4	key		! yymm	(primary key)
		real		msil(3,31)	! mine ind. sio2 by shift,day
		real		cr12(3,31)	! crusher +1/2 by shift,day
		real 		flfr(3,31)	! flf davis tube recovery
		real		flfs(3,31)	! flf davis tube sio2
		real		fls(3,31)	! flf sio2
		real		flts(3,31)	! flot tails sio2
		real		aeff(4,3,31)	! amine efficiency by cell
c
c
	end structure
c
	record /bozo2/ hsd
c
	character yr*2/'05'/,mo*2,keyx*4
c
	open (unit=30,file='dvj12.txt',status='unknown')
c
	OPEN (UNIT=91,FILE='MET_P:HALFSIL2.DAT',STATUS='OLD',
	1	ACCESS='KEYED',SHARED)
C
	do 50 i=1,12
	write (mo,'(i2)') i
	if (mo(1:1) .eq. ' ') mo(1:1)='0'
	keyx=yr//mo	
	READ (91,KEY=keyx) HSD
c
301	format (1h ,a4)
	do 40 jd=1,31
	do 39 js=1,3
	write (30,302) keyx,jd,js,hsd.flts(js,jd)
302	format (1h ,a4,',',i2,',',i1,',',f7.2)
39	continue
40	continue
50	continue
c
	stop
	end
