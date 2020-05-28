#!/bin/sh

#heatCap=(00.00 00.01 00.02 00.03 00.04 00.05 00.06 00.07 00.08 00.09 00.10 00.11 00.12 00.13 00.14 00.15 00.16 00.17 00.18 00.19 00.20 00.25 00.30 00.35 00.40 00.45 00.50 00.55 00.60 00.65 00.70 00.75 00.80 00.85 00.90 00.95 01.00 01.50 02.00 02.50 03.00 03.50 04.00 04.50 05.00 05.50 06.00 06.50 07.00 07.50 08.00 08.50 09.00 09.50 10.00 10.50 11.00 11.50 12.00 12.50 13.00 13.50 14.00 14.50 15.00 15.50 16.00 16.50 17.00 17.50 18.00 18.50 19.00 19.50 20.00)
heatCap=(00.00 00.02 00.04 00.06 00.08 00.10 00.1200.14 00.16 00.18 00.20 00.30 00.40 00.50 01.00 02.00 04.00 06.00 08.00 08.50 09.00 09.50 10.00 10.50 11.00 11.50 12.00 12.50 13.00 13.50 14.00 14.50 15.00)

id=(catmos csoil cseaice csnow cmixed)
param=(C_ATMOS C_SOIL C_SEAICE C_SNOW C_MIXED)

len1=${#heatCap[@]}
len2=${#id[@]}

catmos_ref=0.150833026
csoil_ref=0.07
cseaice_ref=0.152990252
csnow_ref=4.81658950E-02
cmixed_ref=9.28524303

for ((i=0; i<$len2; i++))
do
    cd config
    #Setting all heatCap params back to default
    sed -i s/${param[0]}=.*/${param[0]}=$catmos_ref/ params_heatCap.conf
    sed -i s/${param[1]}=.*/${param[1]}=$csoil_ref/ params_heatCap.conf
    sed -i s/${param[2]}=.*/${param[2]}=$cseaice_ref/ params_heatCap.conf
    sed -i s/${param[3]}=.*/${param[3]}=$csnow_ref/ params_heatCap.conf
    sed -i s/${param[4]}=.*/${param[4]}=$cmixed_ref/ params_heatCap.conf
    cd ..

    echo Starting ${id[$i]} runs
    
    for ((j=0; j<$len1; j++))
    do
        echo Current ${id[$i]} is ${heatCap[$j]}
        cd config
        sed -i s/${param[$i]}=.*/${param[$i]}=${heatCap[$j]}/ params_heatCap.conf
        sed -i s/ID=.*/ID=\"${id[$i]}${heatCap[$j]}\"/ params_heatCap.conf
        cd ..
        bash run_ebm.sh config/params_heatCap.conf
    done
done