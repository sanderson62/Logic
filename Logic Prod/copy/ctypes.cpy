       77  char                   pic s9(2)  comp-5 is typedef.
       77  uns-char               pic  9(2)  comp-5 is typedef.
       77  short                  pic s9(4)  comp-5 is typedef.
       77  uns-short              pic  9(4)  comp-5 is typedef.
       77  int                    pic s9(9)  comp-5 is typedef.
       77  uns-int                pic  9(9)  comp-5 is typedef.
      $IF P64 set
       77  long                   pic s9(18) comp-5 is typedef.
       77  uns-long               pic  9(18) comp-5 is typedef.
      $ELSE
       77  long                   pic s9(9)  comp-5 is typedef.
       77  uns-long               pic  9(9)  comp-5 is typedef.
      $END
       77  l-long                 pic s9(18) comp-5 is typedef.
       77  uns-l-long             pic  9(18) comp-5 is typedef.
       77  d-l-float                         comp-2 is typedef.
       77  d-float                           comp-2 is typedef.
       77  float                             comp-1 is typedef.
       77  proc-pointer           procedure-pointer is typedef.
       77  data-pointer                     pointer is typedef.
       77  void                   pic  9(2)  comp-5 is typedef.
       78  default-convention-val value   0.
       78  cdecl-convention-val   value   0.
       78  pascal-convention-val  value  11.
       78  fast-convention-val    value   2.
       78  std-convention-val     value  74.
       78  sys-convention-val     value  16.
       78  opt-convention-val     value   0.
       78  pasc16-convention-val  value  35.
       78  cdec16-convention-val  value  32.
