C    This file is part of WATROUTE.
C
C    WATROUTE is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    WATROUTE is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with WATROUTE.  If not, see <http://www.gnu.org/licenses/>.

!***********************************************************************
!       copyright (c) by Nick Kouwen and Dave Watson 2007
!***********************************************************************

! Dan Princz, Month Year : added FIND_DAY and FIND_MONTH subroutines
! Craig Thompson, July 2008 : bugfix in FIND_DAY


	MODULE EF_ParseUtilities
		
	CONTAINS

C***********************************************************************
	integer FUNCTION SplitLine(line, keyword, value)
C***********************************************************************
C Extract the first white space delimited substring from line and place
C it into keyword. Put the rest of line into value.
C
C Do NOT change the incoming line
C
C Return:	0 = the line is empty
C		1 = there is ONLY a keyword
C		2 = there is a keyword and a value
C***********************************************************************

	character*(*) line
	character*(*) keyword, value
	character*10000 localLine
	integer I, DQ, lineLen
	logical rStat

	SplitLine = 0
	localLine = line			! work on local copy of the string
					
	rStat = Detab(localLine)				! change any tabs to spaces
	localLine = ADJUSTL(localLine)			! get rid of leading spaces
	lineLen = LEN_TRIM(localLine)	! find the new length, excluding trailing spaces

	if(lineLen .eq. 0) then ! we have an empty line
		SplitLine = 0
		return
	end if
			

	I = INDEX(localLine(1:lineLen),' ')	! Find the first space character
	
C Deal with possibility of quotes (strings)
	if(localLine(1:1) .eq. '"')then
		DQ = INDEX(localLine(2:lineLen),'"')+1
		localLine(1:1) = CHAR(32) !replace first occurrence of quote with a space
		if(DQ.GT.1) then
			localLine(DQ:DQ) = CHAR(32) !replace 2nd occurrence of quote with a space
				
C			localLine = ADJUSTL(localLine)			! get rid of leading spaces
C			lineLen = LEN_TRIM(localLine)
			if(DQ.EQ.lineLen) then
				keyword = localLine(2:lineLen-1)
				value = ''
				SplitLine = 1
				return
			else 
				keyword = localLine(2:DQ-1)
				value = localLine(DQ+1:lineLen)
				value = ADJUSTL(value)
				SplitLine = 2
			end if
			return
		end if
	end if


	if((I.EQ.0) .AND. (lineLen.GT.0))then	! Single word only
		keyword = localLine(1:lineLen)
		value = ''
		SplitLine = 1
	else
		keyword = localLine(1:I)
		value = localLine(I+1:lineLen)
		value = ADJUSTL(value)
		SplitLine = 2
	end if
	
	return
	end FUNCTION

C***********************************************************************
	integer FUNCTION CountWords(line)
C***********************************************************************
C
C Return:	the number of words in the line
C***********************************************************************

	character*(*) line
	character*10000 localLine
	integer I, DQ, lineLen
	logical rStat

	localLine = line			! work on local copy of the string
					
	rStat = Detab(localLine)				! change any tabs to spaces
	localLine = ADJUSTL(localLine)			! get rid of leading spaces
	lineLen = LEN_TRIM(localLine)	! find the new length, excluding trailing spaces

	CountWords = 0

	if(lineLen .eq. 0) 	return

	I = 1
	do while(I.ne.0)
		CountWords = CountWords+1
		I = INDEX(localLine(1:lineLen),' ')	! Find the first space character
		localLine = localLine(I+1:lineLen)
		localLine = ADJUSTL(localLine)
		lineLen = LEN_TRIM(localLine)
	end do
	
	return
	end FUNCTION




C***********************************************************************
	logical FUNCTION Detab(line)
C***********************************************************************
C Replace all tabs in line with space characters.
C
C Return:	= .false. no change occured
C		= .true. the string was changed
C***********************************************************************

	character*(*) line
	character*1 tab, space
	integer I

	tab = CHAR(9)		! What we are looking for
	space = CHAR(32)	! the substitution value

	Detab = .false.
	do I=1,LEN_TRIM(line)
		if(line(I:I) .eq. tab)then
			line(I:I) = space
			Detab = .true.
		end if
	end do

	return
	end FUNCTION

C***********************************************************************
	logical FUNCTION ToLowerCase(line)
C***********************************************************************
C Replace all uppercase character in line with lowercase characters.
C
C Return:	= .false. no change occured
C		= .true. the string was changed
C***********************************************************************

	character*(*) line
	integer I

	ToLowerCase = .false.
	do I=1,LEN_TRIM(line)
		if((line(I:I) .GE. 'A') .AND. (line(I:I) .LE. 'Z'))then
			line(I:I) = CHAR(ICHAR(line(I:I)) + 32)
			ToLowerCase = .true.
		end if
	end do

	return
	end FUNCTION


  
C***********************************************************************
	logical FUNCTION IsNumber(str)
C***********************************************************************
C Check if the string argument is a number
C
C Return:	= .false. not a number
C		= .true. is a number
C***********************************************************************

	character *(*) str 
	integer i  

      IsNumber=.true.
	
      do i=1,LEN_TRIM(str)
		if((str(i:i).lt.'0'.or.str(i:i).gt.'9').and.
     $		str(i:i).ne.'.'.and.str(i:i).ne.'-'.and.
     $		str(i:i).ne.'+')then
			IsNumber=.false.
			return
		end if
      end do

      return  
      end FUNCTION




C***********************************************************************
	INTEGER FUNCTION JDATE (YEAR,MONTH,DAY)
C***********************************************************************
C Conversion from a Gregorian calendar date to a Julian date. Valid for
C any Gregorian calendar date producing a Julian date greater than zero
C
C COMPUTES THE JULIAN DATE (JDATE) GIVEN A GREGORIAN CALENDAR DATE (YR,MTH,DAY).
C***********************************************************************
      INTEGER YEAR,MONTH,DAY,I,J,K
C
      I= YEAR
      J= MONTH
      K= DAY
C
      JDATE= K-32075+1461*(I+4800+(J-14)/12)/4+367*(J-2-(J-14)/12*12)
     &    /12-3*((I+4900+(J-14)/12)/100)/4
C
      RETURN 
      END FUNCTION

 

C***********************************************************************
	SUBROUTINE GDATE (JD, YEAR,MONTH,DAY)
C***********************************************************************
C Conversion from a Julian date to a Gregorian calendar date.
C
C COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY) GIVEN THE JULIAN DATE (JD).
C***********************************************************************
      INTEGER JD,YEAR,MONTH,DAY,I,J,K,L,N
C
      L= JD+68569
      N= 4*L/146097
      L= L-(146097*N+3)/4
      I= 4000*(L+1)/1461001
      L= L-1461*I/4+31
      J= 80*L/2447
      K= L-2447*J/80
      L= J/11
      J= J+2-12*L
      I= 100*(N-49)+I+L
C
      YEAR= I
      MONTH= J
      DAY= K
C
      RETURN
      END SUBROUTINE

!Dan Princz added this subroutine, FIND_MONTH
CDAN  ******************************************************************
CDAN  FUNCTION DETERMINES MONTH OF YEAR BY JULIAN DAY.
CDAN  ******************************************************************

      SUBROUTINE FIND_MONTH(IDAY, IYEAR, MONTH_NOW)

      INTEGER :: IDAY, IYEAR, MONTH_NOW, I = 0

      !IF IF STATEMENT FAILS SET FIND_MONTH = 0
      MONTH_NOW = 0

      !WILL ADD A DAY TO CERTAIN MONTHS IF IS LEAP YEAR
      !USING EQUATION FROM MESH
      IF (MOD(IYEAR,400) == 0) THEN !LEAP YEAR
        I = 1
      ELSE IF (MOD(IYEAR,100) == 0) THEN !NOT A LEAP YEAR
        I = 0
      ELSE IF (MOD(IYEAR,4) == 0) THEN !LEAP YEAR
        I = 1
      ELSE !NOT A LEAP YEAR
        I = 0
      ENDIF

      !SOLVE AND RETURN MONTH OF YEAR
      IF(IDAY .LT. 32) THEN
        MONTH_NOW = 1
      ELSEIF(IDAY .GE. 32 .AND. IDAY .LT. (60 + I)) THEN
        MONTH_NOW = 2
      ELSEIF(IDAY .GE. (60 + I) .AND. IDAY .LT. (91 + I)) THEN
        MONTH_NOW = 3
      ELSEIF(IDAY .GE. (91 + I) .AND. IDAY .LT. (121 + I)) THEN
        MONTH_NOW = 4
      ELSEIF(IDAY .GE. (121 + I) .AND. IDAY .LT. (152 + I)) THEN
        MONTH_NOW = 5
      ELSEIF(IDAY .GE. (152 + I) .AND. IDAY .LT. (182 + I)) THEN
        MONTH_NOW = 6
      ELSEIF(IDAY .GE. (182 + I) .AND. IDAY .LT. (213 + I)) THEN
        MONTH_NOW = 7
      ELSEIF(IDAY .GE. (213 + I) .AND. IDAY .LT. (244 + I)) THEN
        MONTH_NOW = 8
      ELSEIF(IDAY .GE. (244 + I) .AND. IDAY .LT. (274 + I)) THEN
        MONTH_NOW = 9
      ELSEIF(IDAY .GE. (274 + I) .AND. IDAY .LT. (305 + I)) THEN
        MONTH_NOW = 10
      ELSEIF(IDAY .GE. (305 + I) .AND. IDAY .LT. (335 + I)) THEN
        MONTH_NOW = 11
      ELSEIF(IDAY .GE. (335 + I)) THEN
        MONTH_NOW = 12
      END IF

      RETURN

      END SUBROUTINE FIND_MONTH

!Dan Princz added this subroutine, FIND_DAY
CDAN  ******************************************************************
CDAN  FUNCTION DETERMINES DAY OF MONTH BY JULIAN DAY.
CDAN  ******************************************************************

      SUBROUTINE FIND_DAY(IDAY, IYEAR, DAY_NOW)

      INTEGER :: IDAY, IYEAR, DAY_NOW, I = 0

      !IF IF STATEMENT FAILS SET FIND_DAY = 0
      DAY_NOW = 0

      !WILL ADD A DAY TO CERTAIN MONTHS IF IS LEAP YEAR
      !USING EQUATION FROM MESH
      IF (MOD(IYEAR,400) == 0) THEN !LEAP YEAR
        I = 1
      ELSE IF (MOD(IYEAR,100) == 0) THEN !NOT A LEAP YEAR
        I = 0
      ELSE IF (MOD(IYEAR,4) == 0) THEN !LEAP YEAR
        I = 1
      ELSE !NOT A LEAP YEAR
        I = 0
      ENDIF

      !SOLVE AND RETURN DAY OF MONTH
      IF(IDAY .LT. 32) THEN
        DAY_NOW = IDAY
      ELSEIF(IDAY .GE. 32 .AND. IDAY .LT. (60 + I)) THEN
        DAY_NOW = IDAY - 31
      ELSEIF(IDAY .GE. (60 + I) .AND. IDAY .LT. (91 + I)) THEN
        DAY_NOW = IDAY - 59 - I
      ELSEIF(IDAY .GE. (91 + I) .AND. IDAY .LT. (121 + I)) THEN
        DAY_NOW = IDAY - 90 - I
      ELSEIF(IDAY .GE. (121 + I) .AND. IDAY .LT. (152 + I)) THEN
        DAY_NOW = IDAY - 120 - I
      ELSEIF(IDAY .GE. (152 + I) .AND. IDAY .LT. (182 + I)) THEN
        DAY_NOW = IDAY - 151 - I
      ELSEIF(IDAY .GE. (182 + I) .AND. IDAY .LT. (213 + I)) THEN
        DAY_NOW = IDAY - 181 - I
      ELSEIF(IDAY .GE. (213 + I) .AND. IDAY .LT. (244 + I)) THEN
        DAY_NOW = IDAY - 212 - I
      ELSEIF(IDAY .GE. (244 + I) .AND. IDAY .LT. (274 + I)) THEN
        DAY_NOW = IDAY - 243 - I
      ELSEIF(IDAY .GE. (274 + I) .AND. IDAY .LT. (305 + I)) THEN
        DAY_NOW = IDAY - 273 - I
      ELSEIF(IDAY .GE. (305 + I) .AND. IDAY .LT. (335 + I)) THEN
        DAY_NOW = IDAY - 304 - I
      ELSEIF(IDAY .GE. (335 + I)) THEN
        DAY_NOW = IDAY - 334 - I
      END IF

      RETURN

      END SUBROUTINE FIND_DAY


	END MODULE



