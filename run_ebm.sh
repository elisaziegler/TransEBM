#!/bin/sh

cd config
bash config_module.sh

conf=$1

pattern_out=" OUT_DIR="
pattern_id=" ID="
pattern_wrk=" WRK_DIR="

cd ..
tmp=$(grep -w "$conf" -e "$pattern_out")
tmp=${tmp#$pattern_out} #subtract pattern from the beginning of tmp
tmp=${tmp#\"}
tmp=${tmp#\'}
tmp=${tmp%\"}
tmp=${tmp%\",}
EBM_out_dir=${tmp%\'}

tmp=$(grep -w "$conf" -e "$pattern_id")
tmp=${tmp#$pattern_id}
tmp=${tmp#\"} #remove '..' surrounding the string 
tmp=${tmp#\'}
tmp=${tmp%\"}
tmp=${tmp%\",}
EBM_runid=${tmp%\'}

tmp=$(grep -w "$conf" -e "$pattern_wrk")
tmp=${tmp#$pattern_wrk}
tmp=${tmp#\"}
tmp=${tmp%\"}
tmp=${tmp#\'}
tmp=${tmp%\'}
tmp=${tmp%\",}
EBM_wrk_dir="$tmp$EBM_runid/"

cd preprocess
# .* makes it into a wildcard that contains more than the single preceding character!
# \. makes it register the full-stop!
# only with "" (and not with '') will the variable $conf be expanded!
sed -i "s|config/.*\.conf|$conf|" preprocessing.f90
bash preprocess.sh
echo _______________________________________________
echo Finished preprocessing, start running the model.
echo _______________________________________________
cd ../src
sed -i "s|config/.*\.conf|$conf|" EBM.f90
bash ebm.sh

echo _______________________________________________
echo Finished running the model, start postprocessing.
echo _______________________________________________

echo EBM_out_dir=$EBM_out_dir
echo EBM_runid=$EBM_runid
echo EBM_wrk_dir=$EBM_wrk_dir

#cd ../postprocess
#bash postprocess_cdo.sh $EBM_wrk_dir
mkdir -p $EBM_out_dir/$EBM_runid/
mv $EBM_wrk_dir/* $EBM_out_dir/$EBM_runid/
rmdir $EBM_wrk_dir
