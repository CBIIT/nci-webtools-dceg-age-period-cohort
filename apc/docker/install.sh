#!/usr/bin/bash

sudo yum install -y python python-devel readline-devel httpd-devel python-pip R git httpd
sudo pip install --upgrade pip
sudo pip install rpy2 mod_wsgi flask
sudo R -e "install.packages(c('jsonlite', 'xlsx'), repos='http://cran.rstudio.com/')"
sudo ln -s /usr/lib/jvm/jre/lib/amd64/server/libjvm.so /usr/lib64/libjvm.so

sudo mkdir -p /analysistools/public_html/apps/apc
sudo mkdir -p /analysistools/public_html/wsgi
sudo chown -R apache:apache /analysistools
sudo chmod -R 777 /analysistools

mkdir ~/.checkouts && cd ~/.checkouts
git clone https://github.com/CBIIT/nci-webtools-dceg-age-period-cohort.git

cp -R ~/.checkouts/nci-webtools-dceg-age-period-cohort/apc/* /analysistools/public_html/apps/apc
sudo cp -f ~/.checkouts/nci-webtools-dceg-age-period-cohort/apc/docker/httpd.conf /etc/httpd/conf/httpd.conf

## Starting Application
cd /analysistools/public_html/apps/apc/
sudo httpd -k start
nohup python apc.py -p 8040 &
#sudo httpd -k stop
