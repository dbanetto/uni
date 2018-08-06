#!/bin/bash

urls=(
    'https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_128'
    'https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_256'
    'https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_512'
    'https://5de7xws1p8.execute-api.us-west-2.amazonaws.com/prod/prime_1024'
)

( cd lambda_hit && cargo build --release )

for uri in ${urls[@]} ; do
    base_name=`echo $uri | sed -e 's/^.*_\([0-9]\+\)$/\1/'`
    for l in $(seq 2 5) ; do 
        name="logs/${base_name}.${l}.a.csv"
        uri="${uri}?max=10000&loops=${l}"

        echo "Running $name for $uri"
        ./lambda_hit/target/release/lambda_hit "$uri" -t 50 --async > ${name}

        sleep 10
    done
done


# create summary stats
# clistats can be built from https://github.com/dpmcmlxxvi/clistats
for csv in ./logs/*.csv ; do 
    cat "$csv" | sed '$ d' | clistats -fc 2 > "$csv.stat" 
done
