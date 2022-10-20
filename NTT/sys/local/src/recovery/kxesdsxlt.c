/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/****************************************************************************
**
** FUNCTION:    kxesds_backout_user_exit
**
** DESCRIPTION: User exit module for Dynamic Transaction Backout (DTB)
**              of VSAM ESDS files during WRITE command operation.
**              With VSAM -ESDS data sets, no delete function exists and
**              records cannot be physically deleted from the dataset.
**              An application program may choose to delete a record
**              logically by performing a get-for-update followed by a
**              write-update for the same record but with a marked-for-
**              deletion flag.
**
**              This exit allows for DTB (for syncpoint-rollback,
**              transaction-abend or UniKix-abend) following a WRITE
**              operation on an ESDS data.
**              You are given the opportunity to "mark" the record just
**              written on the file as deleted according to application-
**              dependant logic. A sample is presented here for marking
**              the record as deleted.
**
**              No EXEC CICS calls can be coded in this routine.
**
**
** INPUTS:      record_buffer  - pointer to record to be marked as deleted
**              record_length  - length of record
**              record_dataset - name of the dataset
**              return_code    - return code to be passed back to UniKix.
**                               Initially set to 0.
**
** OUTPUTS:     return_code   - return code passed back to UniKix, which must
**                              be on of:
**                              0 - record not marked as deleted (default)
**                              4 - record has been marked as deleted
**                              X - any other return codes, will be taken
**                                  to mean record not marked for deletion
**
**
******************************************************************************/

/*
** NOTE.   The record_buffer address must not be modified by this exit
**         otherwise the record marked for deletion will not be written
**         correctly to the ESDS data set.
**
*/

#include "kxrcvinclude.h"

#define LOW_VALUES  0x00
#define HIGH_VALUES 0xFF


#ifdef __STDC__
void kxesds_backout_user_exit(char *record_buffer,
                              int   record_length,
                              char *record_dataset,
                              int  *return_code)
#else
void kxesds_backout_user_exit(record_buffer, record_length, record_dataset, return_code)
char *record_buffer;
int   record_length;
char *record_dataset;
int  *return_code;
#endif
{
   /* int buffer_position;
   **
   **
   ** It is not possible to modify the record length during dynamic
   ** transaction backout.
   ** When positioning in the record_buffer to write your "marker"
   ** flag, take care not to modify the record_buffer address. That is,
   ** avoid using the following type of code which updates the
   ** record_buffer address.
   **
   ** eg. position, to write our marker "DELETED" in the last 7 bytes
   **     of record:
   **
   **     record_buffer=(char *)record_buffer+record_length-7;
   **
   ** Instead, focus on copying into the record_buffer using the
   ** record_length as a displacement into the buffer.
   **
   **     record_length -= 7;
   **     memcpy((char *)record_buffer+record_length,"DELETED",7);
   **
   **
   **
   **
   **
   ** Example 1: Write marker x"FF" (high values) at byte 1 in record.
   **
   **     *record_buffer = HIGH_VALUES;
   **     *return_code=4;
   **
   ** Example 2: Write marker x"FF" (high values) at last byte in record.
   **
   **     *((char *)record_buffer+record_length-1) = HIGH_VALUES;
   **     *return_code=4;
   **
   ** Example 3: Write deletion marker "DELETED" from byte 4 in record.
   **
   ** The following example assumes we have a record_length of 100,
   ** and we want to write the marker "DELETED" from position 4 in
   ** the record_buffer.
   **
   **     buffer_position = record_length - 96;
   **     memcpy((char *)record_buffer+buffer_position,"DELETED",7);
   **     *return_code=4;
   **
   **
   ** Example 4: Mark the record as deleted only if the file name is "LOGFILE"
   **
   ** if (!(memcmp(record_dataset,"LOGFILE",7)))
   **      {
   **      buffer_position = record_length - 96;
   **      memcpy((char *)record_buffer+buffer_position,"DELETED",7);
   **      *return_code=4;
   **      return;
   **      }
   **
   */

   *return_code = 0; /* This is the default case, just return with */
   /* no "marker" set. Replace with code from    */
   /* above examples.                            */





   return;
} /* end of kxesds_backout_user_exit */
