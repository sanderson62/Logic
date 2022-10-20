      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id.                 CCFextfh.
       environment division.
       data division.
       linkage section.
       01 paction pic x(2) comp-x.
       01 parmblk pic x(100).
       procedure division using paction parmblk.
       sole section.
       entry-point.
           call "EXTFH" using paction parmblk.
       exit-point.
           exit program.
