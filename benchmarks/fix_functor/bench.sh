#!/bin/sh

filesize () {
    size=$(wc -c < $1)
    echo $size '1000.0' | awk '{printf "%f", $1 / $2}'
}

rm result.txt

for n in `seq 0 10`
do
    # prepare result file
    RESULT_FILE=result$n.txt
    rm $RESULT_FILE
    echo -n '' > $RESULT_FILE
    # 1. num of functor apllication
    # 2. time of code generation [s]
    # 3. execution time of generated code [s]
    # 4. memory usage [KB]
    # 5. code size [KB]

    # compile translated code
    make codegen

    for i in `seq 0 100`
    do
        echo $n "-" $i
        # run bench
        echo $i | tr '\n' '\t' >> $RESULT_FILE
        /usr/bin/time -f '%M' ./bench.out $i 2>&1 | tr '\n' '\t' >> $RESULT_FILE
        # measure size of code
        ./print_code.out $i | sed -e '1d' > code.dump
        filesize code.dump >> $RESULT_FILE
        rm code.dump
        echo '' >> $RESULT_FILE
    done

    # clean
    make clean
done

# calculate the average and output as result.txt
python3 avg.py