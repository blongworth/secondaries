#!/bin/sh

#R -f getQCData.R

cp PlotSecondariesshiny.Rmd /srv/shiny-server/secondaries/index.Rmd
chmod 755 /srv/shiny-server/secondaries/*
