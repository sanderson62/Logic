/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/* Reverse the bytes for shorts and ints on reverse-byte platforms */
#define COBHALF(a) ((((a)&0xff)<<8) | \
		    ((((unsigned)(a)&0xff00))>>8))
#define COBWORD(a) ((((a)&0xff)<<24)    | \
		    (((a)&0xff00)<<8)   | \
		    (((a)&0xff0000)>>8) | \
		    ((((unsigned)(a)&0xff000000))>>24))
