#!/bin/sh

albedo=(0.00 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00)

id=(aland alandlat aseaice asnow aocean aoceanlat)
param=(A_LAND A_LAND_LAT A_SEAICE A_LANDICE A_OCEAN A_OCEAN_LAT)

len1=${#albedo[@]}
len2=${#id[@]}

aland_ref=0.32
alandlat_ref=0.05
aseaice_ref=0.60 
asnow_ref=0.70
aocean_ref=0.289 
aoceanlat_ref=0.09

for ((i=0; i<$len2; i++))
do
    cd config
    #Setting all albedo params back to default
    sed -i s/${param[0]}=.*/${param[0]}=$aland_ref/ params_albedo.conf
    sed -i s/${param[1]}=.*/${param[1]}=$alandlat_ref/ params_albedo.conf
    sed -i s/${param[2]}=.*/${param[2]}=$aseaice_ref/ params_albedo.conf
    sed -i s/${param[3]}=.*/${param[3]}=$asnow_ref/ params_albedo.conf
    sed -i s/${param[4]}=.*/${param[4]}=$aocean_ref/ params_albedo.conf
    sed -i s/${param[5]}=.*/${param[5]}=$aoceanlat_ref/ params_albedo.conf
    cd ..

    echo Starting ${id[$i]} runs
    
    for ((j=0; j<$len1; j++))
    do
        echo Current ${id[$i]} is ${albedo[$j]}
        cd config
        sed -i s/${param[$i]}=.*/${param[$i]}=${albedo[$j]}/ params_albedo.conf
        sed -i s/ID=.*/ID=\"${id[$i]}${albedo[$j]}\"/ params_albedo.conf
        cd ..
        bash run_ebm.sh config/params_albedo.conf
    done
done