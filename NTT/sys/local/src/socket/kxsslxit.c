/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

#include <seccomon.h>
#include <certt.h>

/*
 * Return SECSuccess to accept the certificate.
 * Return SECFailure to reject the certificate.
 * PR_LogPrint statements go to the unikixssl trace file
 * if unikixssl trace is on.
 */
SECStatus Exit_VerifyCertificate(CERTCertificate *cert)
{
   PR_LogPrint("%d\t-> Exit_VerifyCertificate", __LINE__);
   PR_LogPrint("%d\tIn certificate verification user exit", __LINE__);
   PR_LogPrint("%d\t<- Exit_VerifyCertificate", __LINE__);
   return SECSuccess;
}
