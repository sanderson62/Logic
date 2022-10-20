/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/* 
This include will be a no-op for Non-NT platforms
but for the NT (WINTEL) environment it provides the 
definition for DLL_EXPORT and an include for <windows.h>
*/
#define DLL_EXPORT
#define DLL_IMPORT
