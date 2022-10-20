#ifndef __DFHWBTDH__
#define __DFHWBTDH__
/*-------------------------------------------------------------------*/
/* This header file defines the parameter lists which are passed to  */
/* the of the user replaceable analyzer program.                     */
/*-------------------------------------------------------------------*/

typedef struct {
   char wbra_eyecatcher[8];
   unsigned long int wbra_function;
   unsigned long int wbra_response;
   unsigned long int wbra_reason;
   char filler__001[4];
   char wbra_server_program[8];
   char wbra_converter_program[8];
   char wbra_userid[8];
   char wbra_alias_tranid[4];
   char wbra_alias_termid[4];
   char wbra_user_token[8];
   char wbra_dfhcnv_key[8];
   unsigned long int wbra_client_ip_address;
   unsigned long int wbra_server_ip_address;
   unsigned long int *wbra_method_ptr;
   unsigned long int *wbra_http_version_ptr;
   unsigned long int *wbra_resource_ptr;
   unsigned long int *wbra_request_header_ptr;
   unsigned long int *wbra_user_data_ptr;
   signed short int wbra_method_length;
   signed short int wbra_http_version_length;
   signed short int wbra_resource_length;
   signed short int wbra_request_header_length;
   signed short int wbra_user_data_length;
   unsigned char wbra_request_type;
   char wbra_unescape;
   unsigned long int wbra_content_length;
} wbra_parms;
#endif
