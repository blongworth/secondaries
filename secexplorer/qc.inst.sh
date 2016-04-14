#!/bin/sh

#R -f getQCData.R

cp server.R ui.R global.R /srv/shiny-server/qcserver
chmod 755 /srv/shiny-server/qcserver/*
