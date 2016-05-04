#!/bin/sh

#R -f getQCData.R

cp ./secexplorer/server.R ./secexplorer/ui.R ./secexplorer/global.R /srv/shiny-server/qcserver
chmod 755 /srv/shiny-server/qcserver/*

cp ./secshiny/PlotSecondariesshiny.Rmd /srv/shiny-server/secondaries/index.Rmd
chmod 755 /srv/shiny-server/secondaries/*

cp ./showsinglesec/server.R ./showsinglesec/ui.R /srv/shiny-server/secserver
chmod 755 /srv/shiny-server/secserver/*

cp ./qcData.rda /srv/shiny-server/
chmod 755 /srv/shiny-server/qcData.rda
