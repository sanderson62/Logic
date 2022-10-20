#!/usr/bin/ksh     
compprogs=`find . -type f -name "*.cbl" -maxdepth 1 -mtime +8 | cut -d/ -f2`
for file in ${compprogs}
do 
#    file2=echo ${file} | cut -d/ -f2
#   echo "file = ${file}"
    program_name=${file%%.*}
    echo "compiling = ${program_name}"        
    compone ${program_name}
done

#find . -type f -name "*.cbl" -maxdepth 1 -mtime +7
