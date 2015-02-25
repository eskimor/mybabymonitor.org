#!/bin/bash
cabal clean
cabal configure
cabal build
rm static/tmp/*
scp -r config mybabymonitor.org:
scp -r static mybabymonitor.org:
cd dist/build/mybabymonitor.org
strip mybabymonitor.org
gzip mybabymonitor.org
scp mybabymonitor.org.gz mybabymonitor.org:
ssh mybabymonitor.org "gunzip -f mybabymonitor.org.gz"
#ssh mybabymonitor.org "killall mybabymonitor.org"
#ssh mybabymonitor.org "(APPROOT=\"http://mybabymonitor.org\" nohup ./mybabymonitor.org &); disown -a"
