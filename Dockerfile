from rocker/hadleyverse


ENV http_proxy http://172.17.42.1:3128
ENV https_proxy http://172.17.42.1:3128


RUN install2.r --error pacman

ADD /media/sda3/Dropbox/sources/efsagis_0.0.1.tar.gz /tmp

RUN R CMD INSTALL /tmp/efsagis_0.0.1.tar.gz 