#!/usr/bin/bash
case_name=`grep -i case_name $1 | awk '{print $3}' | sed $'s/\'//g'`
echo $case_name
redis_node=`grep -i redis $1 | awk '{print $3}' |  sed $'s/\'//g' | cut -d : -f 1`
echo $redis_node
redis_port=`grep -i redis $1 | awk '{print $3}' |  sed $'s/\'//g' | cut -d : -f 2 | cut -d , -f 1`
echo $redis_port
#######################
IFS=$'\n'
for conf in `grep '=' ./$1`
do
    redis-cli -c -h $redis_node -p $redis_port hset $case_name \
        `echo $conf | cut -d '=' -f 1 | awk '{gsub(/^\s+|\s+$/, "");print}'` \
        `echo $conf | cut -d '=' -f 2 | awk '{gsub(/^\s+|\s+$/, "");print}'` \
         >/dev/null 2>&1
done
