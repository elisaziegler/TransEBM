#!/bin/sh

# moves input albedo & geography and copies config file to the output folder of each run that was done
echo /stacycode/albus/EBM_output/*/*/ | xargs -n 1 mv ../input/*.nc
echo /stacycode/albus/EBM_output/*/*/ | xargs -n 1 cp ../config/config.conf

# moves the output to the place for data storage
mv /stacycode/albus/EBM_output/*/ /stacywork/albus/EBM
