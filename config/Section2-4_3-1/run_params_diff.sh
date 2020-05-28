#!/bin/sh

diff=(0.00 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00)

id=(klandsp klandnp kland keq kocean)
param=(KLAND_SP KLAND_NP KLAND KEQ KOCEAN)

len1=${#diff[@]}
len2=${#id[@]}

klandsp_ref=0.14
klandnp_ref=0.45
kland_ref=0.65
keq_ref=0.8
kocean_ref=0.40

for ((i=0; i<$len2; i++))
do
    cd config
    #Setting all diff params back to default
    sed -i s/${param[0]}=.*/${param[0]}=$klandsp_ref/ params_diff.conf
    sed -i s/${param[1]}=.*/${param[1]}=$klandnp_ref/ params_diff.conf
    sed -i s/${param[2]}=.*/${param[2]}=$kland_ref/ params_diff.conf
    sed -i s/${param[3]}=.*/${param[3]}=$keq_ref/ params_diff.conf
    sed -i s/${param[4]}=.*/${param[4]}=$kocean_ref/ params_diff.conf
    cd ..

    echo Starting ${id[$i]} runs
    
    for ((j=0; j<$len1; j++))
    do
        echo Current ${id[$i]} is ${diff[$j]}
        cd config
        sed -i s/${param[$i]}=.*/${param[$i]}=${diff[$j]}/ params_diff.conf
        sed -i s/ID=.*/ID=\"${id[$i]}${diff[$j]}\"/ params_diff.conf
        cd ..
        bash run_ebm.sh config/params_diff.conf
    done
done