#!/bin/sh

set -e

for TRY in `seq 1 4` ; do 
    echo "Try $TRY"
    for COUNT in `seq 2 2 6` ; do
        echo "Running $COUNT"
        ANON_ID=7980225 REDUCE_TASKS=$COUNT make search &> data/search_${COUNT}_${TRY}
        REDUCE_TASKS=$COUNT make summary &> data/summary_${COUNT}_${TRY}
    done
done

# transforms data into pairs
# grep -i -r -E 'running in|successfully' data | paste -d " " - - | cut -d' ' -f1,2,14 | sed -e 's/data\///' | sed -r 's/:[0-9]+\/[0-9]+\/[0-9]+//' | sed -r 's/_([0-9])/,\1/g' | sed -e 's/ /,/g' > data.csv
