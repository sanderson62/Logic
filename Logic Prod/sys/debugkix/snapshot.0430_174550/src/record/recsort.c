/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


#ifdef INTEL
#define COBHALF(a)  ((((a)&0xff)<<8) | \
		     ((((unsigned)(a)&0xff00))>>8))
#define COBWORD(a)  ((((a)&0xff)<<24)    | \
		     (((a)&0xff00)<<8)   | \
		     (((a)&0xff0000)>>8) | \
		     ((((unsigned)(a)&0xff000000))>>24))
#else
#define COBHALF (a) (a)
#define COBWORD (a) (a)
#endif
#ifdef SUN
#include <varargs.h>
#else
#include "stdarg.h"
#endif
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>


#define  SKIP_2_BYTES   2
#define  SKIP_4_BYTES   4
#define  READ_2_BYTES   2
#define  READ_4_BYTES   4
#define  MAX_BUFF       10240
#define  MAX_KEY_SIZE   80
#define  HEADER         4

unsigned int hex_read();
void quick();
void qs();
int  xstrncmp();
int  xstrmcpy();

int  fd_in;
int  fd_out;

unsigned char buff[MAX_BUFF];
unsigned char wkbuff[MAX_BUFF];
unsigned char tmpbuff[MAX_KEY_SIZE];
unsigned char headbuff[HEADER];
long num_records = 0;
unsigned char *rec_inuse;
int  *rec_size;
int keyoff = 0;
int keylngth = 0;
int fixrecords = 0;
long fixlength;


struct REC_STRUC {
    long rec_off;
    short rec_size;
    unsigned char rec_key[MAX_KEY_SIZE];
} ;

struct REC_STRUC *rec_pointer;
struct REC_STRUC rec_data;

#ifdef __STDC__
main(int argc, unsigned char *argv[])
{
#else
main(argc, argv)
   int argc;
   unsigned char *argv[];
{
#endif

   int i;

	if (argc < 5)
   {
      fprintf(stderr,"Usage: %s keyoff keylength in_filename out_filename <recsize if fixed>\n",argv[0]);
      exit(-1);
   }

   keyoff = atoi(argv[1]);
   keylngth = atoi(argv[2]);

   if ((fd_in = open(argv[3], O_NDELAY | O_RDONLY)) == -1)
   {
      fprintf(stderr,"Could not open %s for read.\n",argv[3]);
      exit(-2);
   }

   if ((fd_out = open(argv[4], O_CREAT | O_WRONLY | O_TRUNC)) == -1)
   {
      fprintf(stderr,"Could not open %s for write.\n",argv[4]);
      exit(-3);
   }

   if (argc == 6)
   {
      fixrecords = 1;
      fixlength = atoi(argv[5]);
   }

   if( get_num_recs())
   {
      fprintf(stderr,"Error getting number of records.\n");
      exit(-4);
   }

   if(!num_records)
   {
      fprintf(stderr,"Input file empty.\n");
      exit(-6);
   }

   rec_pointer = (struct REC_STRUC *) calloc(num_records, sizeof(struct REC_STRUC));
   
   if (rec_pointer == NULL)
   {
   fprintf(stderr,"Couldn't malloc  %d bytes\n",(num_records*sizeof(struct REC_STRUC)));
   fprintf(stderr,"Size of REC_STRUC = %d\n",sizeof(struct REC_STRUC));
   exit(1);
   }
   lseek(fd_in, 0, SEEK_SET);

   if( get_rec_size(keyoff, keylngth) )
   {
      fprintf(stderr,"Error getting input records.\n");
      exit(-5);
   }

   fprintf(stderr,"call sort\n");
   quick((unsigned char *)rec_pointer, num_records);       /* sort the keys */

   lseek(fd_in, rec_pointer[0].rec_off, SEEK_SET);
   if(fixrecords)
   {
        read(fd_in, wkbuff, rec_pointer[0].rec_size);
        write(fd_out, wkbuff, rec_pointer[0].rec_size);
        memcpy(tmpbuff,wkbuff+keyoff,keylngth);
   }
   else
   {
        read(fd_in, wkbuff, rec_pointer[0].rec_size + 4);
        write(fd_out, wkbuff, rec_pointer[0].rec_size +4);
        memcpy(tmpbuff,wkbuff+keyoff+4,keylngth);
   }
   for(i=1; i < num_records; i++)
   {
        lseek(fd_in, rec_pointer[i].rec_off, SEEK_SET);
        if(fixrecords)
        {
           read(fd_in, wkbuff, rec_pointer[i].rec_size);
	   if(memcmp(tmpbuff, wkbuff+keyoff, keylngth) != 0)
		 {
                 write(fd_out, wkbuff, rec_pointer[i].rec_size);
	         memcpy(tmpbuff,wkbuff+keyoff,keylngth);
		 }
	   else
	         fprintf(stderr,"Duplicate Key = %s\n",tmpbuff);
        }
        else
        {
           read(fd_in, wkbuff, rec_pointer[i].rec_size + 4);
	   if(memcmp(tmpbuff, wkbuff+keyoff+4, keylngth) != 0)
		 {
                 write(fd_out, wkbuff, rec_pointer[i].rec_size +4);
	         memcpy(tmpbuff,wkbuff+keyoff+4,keylngth);
		 }
	   else
	         fprintf(stderr,"Duplicate Key = %s\n",tmpbuff);
        }
   }
   fprintf(stdout,"\nSort Completed\n");
   fchmod(fd_out,00644);

   exit(0);

}



#ifdef __STDC__
int
get_rec_size(int keyoff, int keylngth)
{
#else
int 
get_rec_size(keyoff, keylngth)
   int keyoff;
   int keylngth;
{
#endif

   unsigned int vreclen;
   unsigned int byte_count;
   long cummoff = 0;
   int i;
   unsigned char dum_buff[MAX_BUFF];
 
   switch (fixrecords)
   {
      case 0:
         for(i=0; i < num_records; i++)
         {
         vreclen = hex_read(fd_in, READ_4_BYTES);
      
             switch (vreclen)
             {
                 case -1:   /* Error Reading File */
                         fprintf(stderr,"error, num_records = %d\n",i);
                         return(1);
                 case 0:    /* EOF this should not happen */
                         fprintf(stderr,"2num_records = %d\n",i);
                         return(2);  
                 default:
                         rec_pointer[i].rec_size = vreclen;
                         rec_pointer[i].rec_off = cummoff;
                         cummoff = cummoff + vreclen + 4;
                         read(fd_in, dum_buff, vreclen);
                         xstrncpy(rec_pointer[i].rec_key, &dum_buff[keyoff],  keylngth);
                         break; 
             }
         }
         return(0);
      case 1:
         for(i=0; i < num_records; i++)
         {
            byte_count = read(fd_in, dum_buff, fixlength);
            if (byte_count != fixlength)
            {
               fprintf(stderr,"error, fixed read %d bytes",fixlength);
               return(3);
            }
            rec_pointer[i].rec_size = fixlength;
            rec_pointer[i].rec_off = cummoff;
            cummoff = cummoff + fixlength;
            xstrncpy(rec_pointer[i].rec_key, &dum_buff[keyoff], keylngth);
         }
         return(0);
    }
}
   

   
#ifdef __STDC__
unsigned int
hex_read(int fd, int nbytes)
{
#else
unsigned int
hex_read( fd, nbytes)
   int fd;
   int nbytes;
{
#endif

   int i;
   unsigned int hex_value;

   switch (read(fd,headbuff,nbytes))
   {
      case -1:
         fprintf(stderr,"\nCould not read in %d hex bytes.\n",nbytes);   
         return(-1);
      case 0:  /* EOF */
         return(0);
      default:
	 memcpy(&hex_value,headbuff,4);
	 /*
	 hex_value = (unsigned int)*headbuff;
	 fprintf(stdout,"hex_value = %x\n",hex_value);
         for(i=1; i < nbytes; i++)
         {
            hex_value = hex_value<<8;
            hex_value |= (unsigned int) headbuff[i];
         }
	 */
         break;
   }
   return(hex_value);
}


int
get_num_recs()
{
   long vreclen;
   int i = 0;
   int byte_count;
   unsigned char dum_buff[MAX_BUFF];
   struct stat stbuf;

   switch (fixrecords)
   {
      case 0:    /* Variable Length Records  */
         for(;;)
         {
         vreclen = hex_read(fd_in, READ_4_BYTES);
      
             switch (vreclen)
             {
                 case -1:   /* Error Reading File */
                         return(1);
                 case 0:    /* EOF */
                         fprintf(stdout,"%ld records on file\n",num_records);
                         return(0);
                 default:
                         num_records++;
                         byte_count = read(fd_in, dum_buff, vreclen);
                         if (byte_count != vreclen)
                         {
                             fprintf(stderr,"Error reading record %d\n",num_records);
                             return(1);
                         }
                         break;
             }
          }
          break;
      case 1:  /* Fixed Length Records */
	 fstat(fd_in,&stbuf);
	 if ((stbuf.st_size % fixlength) != 0)
	    {
	    fprintf(stderr,"File size %ld not divisible by fixed length %d\n",(int)stbuf.st_size,fixlength);
	    exit(1);
	    }
	 num_records = stbuf.st_size / fixlength;
         fprintf(stdout,"%ld records on file\n",num_records);
         return(0);
         break;
   
   }
}

#ifdef __STDC__
void
quick(unsigned char *item, int count)
{
#else
void
quick(item, count)
    unsigned char *item;
    int count;
{
#endif

    qs(item, 0, count-1);
}

#ifdef __STDC__
void
qs(struct REC_STRUC *item, int left, int right)
{
#else
void
qs(item, left, right)
    struct REC_STRUC *item;
    int left;
    int right;
{
#endif
    register int i, j , k;
    unsigned char x[MAX_KEY_SIZE];
    struct REC_STRUC y;

    i = left;  
    j = right; 
    k = 0;

    xstrncpy(x, item[(left + right)/2].rec_key, keylngth);

    do {
       while((xstrncmp(item[i].rec_key, x, keylngth) < 0) && (i<right)) i++;
       while((xstrncmp(item[j].rec_key, x, keylngth) > 0) && (j>left))  j--;

       if(i<=j) 
       {
           y = item[i];
           item[i] = item[j];
           item[j] = y;
           i++;
           j--; 
       }
     } while(i<=j);
    
       if(left < j) qs(item, left, j);
       if(i < right) qs(item, i, right);
       if(k == 100)
       {
          k = 0;
          fprintf(stdout,".");
          fflush(stdout);
       }
}
	
#ifdef __STDC__
int
xstrncpy(unsigned char *dest, unsigned char *orig, int num_bytes)
{
#else
int
xstrncpy(dest, orig, num_bytes)
    unsigned char *dest;
    unsigned char *orig;
    int num_bytes;
{
#endif

    int i;
    for(i=0; i < num_bytes; i++)
    {
       dest[i] = orig[i];
    }
    return(0);
}

#ifdef __STDC__
int
xstrncmp(unsigned char *dest, unsigned char *orig, int num_bytes)
{
#else
int
xstrncmp(dest, orig, num_bytes)
    unsigned char *dest;
    unsigned char *orig;
    int  num_bytes;
{
#endif

    int i;
    for(i=0; i < num_bytes; i++)
    {
       if(dest[i] < orig[i])        /* s1 less than s2    */
          return(-1);
       if(dest[i] > orig[i])        /* s1 greater than s2 */
          return(1);
    }
    return(0);                      /* they must be equal */
}
       
