#!/bin/sh

#outRad_cob=(100.0 110.0 120.0 130.0 140.0 150.0 160.0 170.0 180.0 190.0 200.0 210.0 220.0 230.0 240.0 250.0 260.0 270.0 280.0 290.0 300.0 310.0 320.0 330.0 340.0 350.0 360.0 370.0 380.0 390.0 400.0 410.0 420.0 430.0 440.0 450.0 460.0 470.0 480.0 490.0 500.0)
#outRad_A=(150.0 160.0 170.0 180.0 190.0 200.0 202.0 204.0 206.0 208.0 209.0 210.0 211.0 212.0 213.0 214.0 216.0 218.0 220.0 230.0 240.0 250.0 260.0 270.0 280.0 290.0 300.0)
#outRad_cos=(00.25 00.50 00.75 01.00 02.00 03.00 04.00 05.00 06.00 07.00 08.00 09.00 10.00 11.00 12.00 13.00 14.00 15.00 16.00 17.00 18.00 19.00 20.00 30.00)
#outRad_B=(0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00 1.10 1.20 1.30 1.40 1.50 1.60 1.70 1.80 1.90 2.00 2.10 2.20 2.25 2.30 2.35 2.40 2.50 2.60 2.70 2.80 2.90 3.00 3.10 3.20 3.30 3.40 3.50 3.60 3.70 3.80 3.90 4.00 4.10 4.20 4.30 4.40 4.50 4.60 4.70 4.80 4.90 5.00)
outRad_A=(209.5 209.6 209.7 209.8 209.8 210.0 210.1 210.2 210.3 210.4 210.5 210.6 210.7 210.8 210.9 211.0)
outRad_cos=(05.20 5.25 5.30 5.31 5.32 5.33 5.34 5.35 5.36 5.37 5.38 5.39 5.40 5.45 5.50)
outRad_B=(2.00 2.05 2.10 2.11 2.12 2.13 2.14 2.15 2.20)

#id=(co2base abase co2scaling b)
#param=(CO2_BASE A_BASE CO2_SCALING B)
id=(abase co2scaling b)
param=(A_BASE CO2_SCALING B)

len1=${#outRad[@]}
len2=${#id[@]}

#co2base_ref=315.0
abase_ref=210.2
co2scale_ref=5.35
b_ref=2.13

i=0
cd config
#Setting all outRad params back to default
#sed -i s/${param[0]}=.*/${param[0]}=$co2base_ref/ params_outRad.conf
#sed -i s/${param[1]}=.*/${param[1]}=$abase_ref/ params_outRad.conf
#sed -i s/${param[2]}=.*/${param[2]}=$co2scale_ref/ params_outRad.conf
#sed -i s/${param[3]}=.*/${param[3]}=$b_ref/ params_outRad.conf
sed -i s/${param[0]}=.*/${param[0]}=$abase_ref/ params_outRad.conf
sed -i s/${param[1]}=.*/${param[1]}=$co2scale_ref/ params_outRad.conf
sed -i s/${param[2]}=.*/${param[2]}=$b_ref/ params_outRad.conf
cd ..

echo Starting ${id[$i]} runs

#for ((j=0; j<${#outRad_cob[@]}; j++))
#do
#    echo Current ${id[$i]} is ${outRad_cob[$j]}
#    cd config
#    sed -i s/${param[$i]}=.*/${param[$i]}=${outRad_cob[$j]}/ params_outRad.conf
#    sed -i s/ID=.*/ID=\"${id[$i]}${outRad_cob[$j]}\"/ params_outRad.conf
#    sed -i s/OB=.*/OB=0.409253/ params_outRad.conf
#    cd ..
#    bash run_ebm.sh config/params_outRad.conf
#done

#sed -i s/${param[0]}=.*/${param[0]}=$co2base_ref/ config/params_outRad.conf
#i=1
for ((j=0; j<${#outRad_A[@]}; j++))
do
    echo Current ${id[$i]} is ${outRad_A[$j]}
    cd config
    sed -i s/${param[$i]}=.*/${param[$i]}=${outRad_A[$j]}/ params_outRad.conf
    sed -i s/ID=.*/ID=\"${id[$i]}${outRad_A[$j]}\"/ params_outRad.conf
    sed -i s/OB=.*/OB=0.409253/ params_outRad.conf

    cd ..
    bash run_ebm.sh config/params_outRad.conf
done

sed -i s/${param[1]}=.*/${param[1]}=$abase_ref/ config/params_outRad.conf
#i=2
i=1
for ((j=0; j<${#outRad_cos[@]}; j++))
do
    echo Current ${id[$i]} is ${outRad_cos[$j]} with param ${param[$i]}
    cd config
    sed -i s/${param[$i]}=.*/${param[$i]}=${outRad_cos[$j]}/ params_outRad.conf
    sed -i s/ID=.*/ID=\"${id[$i]}${outRad_cos[$j]}\"/ params_outRad.conf
    sed -i s/OB=.*/OB=0.409253/ params_outRad.conf
    cd ..
    bash run_ebm.sh config/params_outRad.conf
done

sed -i s/${param[2]}=.*/${param[2]}=$co2scale_ref/ config/params_outRad.conf
#i=3
i=2
for ((j=0; j<${#outRad_B[@]}; j++))
do
    echo Current ${id[$i]} is ${outRad_B[$j]}
    cd config
    sed -i s/${param[$i]}=.*/${param[$i]}=${outRad_B[$j]}/ params_outRad.conf
    sed -i s/ID=.*/ID=\"${id[$i]}${outRad_B[$j]}\"/ params_outRad.conf
   sed -i s/OB=.*/OB=0.409253/ params_outRad.conf
    cd ..
    bash run_ebm.sh config/params_outRad.conf
done
