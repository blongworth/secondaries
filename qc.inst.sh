#!/bin/sh

#R -f getQCData.R

cp server.R ui.R global.R qcData.rda /srv/shiny-server/qcserver
chmod 755 /srv/shiny-server/qcserver/*

cp PlotSecondariesshiny.Rmd /srv/shiny-server/secondaries/index.Rmd
cp qcData.rda /srv/shiny-server/secondaries
chmod 755 /srv/shiny-server/secondaries/*
