#!/bin/bash
# Get new QC data from DB and copy to apropriate locations

cd ~/Projects/secondaries/ && ~/bin/getqcdata && cp ~/Projects/secondaries/qcData.rda /srv/shiny-server/
