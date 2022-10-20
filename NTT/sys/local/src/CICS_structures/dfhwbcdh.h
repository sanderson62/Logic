#ifndef __DFHWBCDH__
#define __DFHWBCDH__
/*-------------------------------------------------------------------*/
/* This header file defines the parameter lists which are passed to  */
/* the 2 functions (DECODE and ENCODE) of the user replaceable       */
/* converter program.                                                */
/*-------------------------------------------------------------------*/

typedef struct {
   char comm_parmlist[1];
} dfhcommarea;

/*-------------------------------------------------------------------*/
/* The fields at the start of the converter                          */
/* commarea must be accessible                                       */
/* independent of the converter function being called.               */
/* These declarations provide a definition of the                    */
/* commarea in terms of these common fields.                         */
/*-------------------------------------------------------------------*/

typedef struct {
   char converter_eyecatcher[8];
   char converter_version;
   char converter_volatile;
   signed short int converter_function;
   unsigned long int converter_response;
   unsigned long int converter_reason;
   char converter_parmlist[1];
} converter_parms;

/*-------------------------------------------------------------------*/
/* These declarations define the parameter list which                */
/* is passed to the ENCODE function of the user replaceable          */
/* Converter program. It is called by the alias program              */
/* if data mapping of the remote procedure's output is required.     */
/*-------------------------------------------------------------------*/

typedef struct {
   char encode_eyecatcher[8];
   char encode_version;
   char encode_volatile;
   signed short int encode_function;
   unsigned long int encode_response;
   unsigned long int encode_reason;
   unsigned long int *encode_data_ptr;
   signed long int encode_input_data_len;
   char encode_user_token[8];
   signed long int encode_entry_count;
} encode_parms;

/*-------------------------------------------------------------------*/
/* These declarations define the parameter list which                */
/* is passed to the DECODE function of the converter program.        */
/*-------------------------------------------------------------------*/

typedef struct {
   char decode_eyecatcher[8];
   char decode_version;
   char decode_volatile;
   signed short int decode_function;
   unsigned long int decode_response;
   unsigned long int decode_reason;
   unsigned long int decode_client_address;
   char decode_client_address_string[15];
   char filler__001;
   unsigned long int *decode_data_ptr;
   unsigned long int *decode_method_ptr;
   unsigned long int *decode_http_version_ptr;
   unsigned long int *decode_resource_ptr;
   unsigned long int *decode_request_header_ptr;
   unsigned long int *decode_user_data_ptr;
   signed short int decode_method_length;
   signed short int decode_http_version_length;
   signed short int decode_resource_length;
   signed short int decode_request_header_length;
   signed long int decode_input_data_len;
   signed short int decode_user_data_length;
   char filler__002[2];
   signed long int decode_output_data_len;
   char decode_server_program[8];
   char decode_user_token[8];
   signed long int decode_entry_count;
} decode_parms;

#endif
