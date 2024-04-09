#!/bin/bash

dir=`mktemp -d temp.XXX`
total=0
success=0

for f in *.lam
do
    /bin/echo -n "${f}... "
    total=$(($total+1))
    out="${f%.lam}.out"
    if [[ ${f} == poly-* ]]; then
	../lam -letpoly ${f} &> ${dir}/${out} & pid=$!
    else
	../lam ${f} &> ${dir}/${out} & pid=$!
    fi
    disown
    sleep 0.2 && kill -9 ${pid} &> /dev/null
    diff ${out} ${dir}/${out} &> /dev/null
    if [ $? -ne 0 ]; then
	echo "failed"
    else
	echo "ok"
	success=$(($success+1))
    fi
done

echo "=============================="
echo "${success}/${total} tests passed."

rm -rf ${dir}
