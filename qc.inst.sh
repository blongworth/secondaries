#!/bin/sh

#R -f getQCData.R

cp ./qcserver/server.R ui.R global.R /srv/shiny-server/qcserver
chmod 755 /srv/shiny-server/qcserver/*

cp PlotSecondariesshiny.Rmd /srv/shiny-server/secondaries/index.Rmd
chmod 755 /srv/shiny-server/secondaries/*

cp ./showsinglesec/server.R ui.R /srv/shiny-server/secserver
chmod 755 /srv/shiny-server/secserver/*

cp ./qcData.rda /srv/shiny-server/
chmod 755 /srv/shiny-server/qcData.rda
