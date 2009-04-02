// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

/*
Name:            RandomDefs.h
Description:     Commonly used definitions and numbers
Library:         random
Original Author: Sven Thommesen
Date:            1997-01-15 (v. 0.6)
Modified by:     Sven Thommesen
Date:		 1997-09-01 (v. 0.7)
Modified by:	 Sven Thommesen
Date:		 1997-12-08 (v. 0.75)
Modified by:	 Sven Thommesen
Date:		 1998-10-08 (v. 0.8)
Modified by:	 Sven Thommesen
Date:		 2000-02-21 (v. 0.81)
Modified by:	 Sven Thommesen
Date:		 2001-07-17
*/

/*
123456789|123456789|123456789|123456789|123456789|123456789|123456789|123456789|
*/

#include <Swarm/misc.h> // clock, time, getpid, sleep, gettimeofday,

// System calls used:

// clock()        is in <time.h>                (under Linux, at least)
// time(0)        is in <time.h>		(under Linux, at least)
// getpid()       is in <unistd.h>		(under Linux, at least)
// sleep(1)       is in <unistd.h>		(under Linux, at least)
// gettimeofday() is in <sys/time.h>		(under Linux, at least)
// tempusFugit()  is in <random/random.m>	(in this directory)
// nextSeed()     is in <random/random.m>       (in this directory)

// clock() measures CPU time in steps of 1/CLOCKS_PER_SECOND seconds
// clock() is not influenced by sleep()

// time(0) measures calendar time in whole seconds since 1970/01/01
// time(0) is influenced by sleep()
// time(0) returns a long int

// getpid() returns the unix process id of the program, a value > 0
// getpid() returns an int

// i = sleep(j) sleeps for j seconds, and returns the number of seconds
// left to sleep if interrupted (normally 0)

// tempusFugit() returns the  program's runtime in microseconds

// nextSeed() provides successive 'random' seed values using an inline LCG 

// 
// -----------------------------
// 

// Unused macros:

// Like OLDRANDSEED, but sleep() ensures successive values are different:
#define SLEEPYSEED   ( ( unsigned int) ( getpid() * time(0) + sleep(1) ) )

// Depends on the program's runtime:
#define CHICKENFEED  ( ( unsigned int) ( getpid() * tempusFugit() ) )

// Not usable, as sometimes clock() = 0:
#define BIRDSEED     ( ( unsigned int) ( getpid() * tempusFugit() * clock() ) )

// 
// -----------------------------
// 

// These values were used in v. 0.7:

// #define RANDOMSEED   ( BIRDSEED )
// #define STARTSEED ( (_useFixedSeed) ? DEFAULTSEED : RANDOMSEED )

// 
// -----------------------------
// 

// Fixed seed values:

#define DEFAULTSEED   321654789U
#define DEFAULTSEED1 1023456789U
#define DEFAULTSEED2  123456789U
#define DEFAULTSEED3 2468013579U
#define DEFAULTSEED4  864297531U

// Variable seed values:

#define PIDTIMESEED  ( ( unsigned int) ( getpid() * time(0) ) )
#define PIDRANDSEED  ( ( unsigned int) ( getpid() * time(0) * tempusFugit() ) )

// This seed depends on pid and absolute time,
// and will vary between program invocations:
#define FIRSTSEED ( _firstSeed )

// This seed depends on the program's running time,
// and hence is less predictable:
#define RANDOMSEED ( PIDRANDSEED )

// This seed follows an inline LCG, an is thus
// perfectly predictable and repeatable:
#define NEXTSEED  ( nextSeed() )

// This one works just fine (evaluated at runtime):
#define STARTSEED ( (_useFixedSeed) ? NEXTSEED : RANDOMSEED )

// 
// -----------------------------
// 

// This number is used as a test value
// by generator objects:
#define TESTCASE             99U

// This number is used to limit the number of
// virtual generators for split generators,
// and is used as a test case in distributions:
#define MAXVGEN       (1U << 30)

#define GENNAMESIZE          12
#define DISTNAMESIZE         30

// The 'magic' numbers below are completely arbitrary!
// They are used by the getState/setState methods.
// (They do, however, follow a pattern of sorts.)

// This scheme was adopted for the second
// revision (v. 0.7) for greater flexibility.

/*
#define CATEGORY             x000000U

#define GENMAJORTYPE         0xx0000U
#define GENSUBTYPE           000xx00U
#define GENREVISION          00000xxU

#define DISTOUTPUT           0x00000U
#define DISTPARAMS           00x0000U
#define DISTMAJORTYPE        000xx00U
#define DISTREVISION         00000xxU
*/

#define GENERATORMASK        1000000U

#define LAGFIBMASK            100000U

#define ACGMAGIC             1110000U
#define SCGMAGIC             1120000U
#define SWBMAGIC             1130000U
#define PSWBMAGIC            1140000U

#define LCGGENMASK            200000U

#define LCGMAGIC             1210000U
#define PMMLCGMAGIC          1220000U
#define C2LCGXMAGIC          1230000U
#define C4LCGXMAGIC          1240000U

#define TGFSRMAGIC           1310000U
#define MT19937MAGIC         1320000U
#define C2TAUSMAGIC          1330000U
#define MRGMAGIC             1340000U
#define C2MRGMAGIC           1350000U
#define MWCMAGIC             1360000U
#define C3MWCMAGIC           1370000U
#define RWC2MAGIC            1380000U
#define RWC8MAGIC            1390000U

#define GENSUBMASK               100U

#define DISTRIBUTIONMASK     2000000U

/*
#define BOOLDISTMASK          100000U
#define INTDISTMASK           200000U
#define UNSIGNEDDISTMASK      300000U
#define FLOATDISTMASK         400000U
#define DOUBLEDISTMASK        500000U
#define LDOUBLEDISTMASK       600000U

#define ONEPARAMDISTMASK       10000U
#define TWOPARAMDISTMASK       20000U
#define THREEPARAMDISTMASK     30000U
#define UNIFORMDISTMASK        80000U
#define ZEROPARAMDISTMASK      90000U
*/

#define UNIFORMINTEGERMAGIC  2280100U
#define UNIFORMUNSIGNEDMAGIC 2380100U
#define UNIFORMDOUBLEMAGIC   2480100U

#define RANDOMBITMAGIC       2190100U
#define BERNOULLIDISTMAGIC   2110100U
#define EXPONENTIALDISTMAGIC 2410100U
#define NORMALDISTMAGIC      2420100U
#define LOGNORMALDISTMAGIC   2420200U
#define GAMMADISTMAGIC       2420300U
#define POISSONDISTMAGIC     2420400U

// REVISION NUMBERS for objects in this release:

#define LCGREVISION                2U
#define PMMLCGREVISION             2U
#define ACGREVISION                2U
#define SCGREVISION                2U
#define SWBREVISION                2U
#define PSWBREVISION               1U
#define TGFSRREVISION              1U
#define MT19937REVISION            1U
#define C2TAUSREVISION             1U
#define C2LCGXREVISION             1U
#define C4LCGXREVISION             1U
#define MRGREVISION                1U
#define C2MRGREVISION              1U
#define MWCREVISION                1U
#define C3MWCREVISION              1U
#define RWC2REVISION               1U
#define RWC8REVISION               1U

#define UNIFORMINTEGERREVISION     2U
#define UNIFORMUNSIGNEDREVISION    2U
#define UNIFORMDOUBLEREVISION      2U
#define RANDOMBITREVISION          2U
#define BERNOULLIDISTREVISION      1U
#define NORMALDISTREVISION         2U
#define LOGNORMALDISTREVISION      2U
#define EXPONENTIALDISTREVISION    2U
#define GAMMADISTREVISION          2U
#define POISSONDISTREVISION        2U

