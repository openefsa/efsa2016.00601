from r-base:3.2.3


#ENV http_proxy http://172.17.42.1:3128
#ENV https_proxy http://172.17.42.1:3128
#ENV JAVA_TOOL_OPTIONS="-Dhttps.proxyHost=172.17.42.1 -Dhttps.proxyPort=3128 -Dhttp.proxyHost=172.17.42.1 -Dhttp.proxyPort=3128"

ENV SPARK_MASTER_OPTS "-Dspark.driver.port=7001 -Dspark.fileserver.port=7002 -Dspark.broadcast.port=7003 -Dspark.replClassServer.port=7004 -Dspark.blockManager.port=7005 -Dspark.executor.port=7006 -Dspark.ui.port=4040 -Dspark.broadcast.factory=org.apache.spark.broadcast.HttpBroadcastFactory"
ENV SPARK_WORKER_OPTS="-Dspark.driver.port=7001 -Dspark.fileserver.port=7002 -Dspark.broadcast.port=7003 -Dspark.replClassServer.port=7004 -Dspark.blockManager.port=7005 -Dspark.executor.port=7006 -Dspark.ui.port=4040 -Dspark.broadcast.factory=org.apache.spark.broadcast.HttpBroadcastFactory"
ENV SPARK_MASTER_PORT=7077
ENV SPARK_MASTER_WEBUI_PORT=8080
ENV SPARK_WORKER_PORT=8888
ENV SPARK_WORKER_WEBUI_PORT=8081

EXPOSE 8080 7077 8888 8081 4040 7001 7002 7003 7004 7005 7006 


RUN apt-get update
RUN apt-get install -y libgdal-dev libproj-dev curl librdf0-dev openjdk-8-jdk openssh-server


RUN install2.r tmap raster dplyr tidyr gplots pander uuid network devtools redland readr readxl reshape2 maptools ggplot2 knitr rmarkdown


RUN curl http://mirror.nohup.it/apache/spark/spark-1.6.1/spark-1.6.1-bin-hadoop2.6.tgz >  spark-1.6.1-bin-hadoop2.6.tgz
RUN tar -xzf spark-1.6.1-bin-hadoop2.6.tgz -C /usr/local/
RUN cd /usr/local && ln -s spark-1.6.1-bin-hadoop2.6 spark
ENV SPARK_HOME /usr/local/spark
RUN ln -s /usr/local/spark/R/lib/SparkR/ /usr/local/lib/R/site-library/


RUN Rscript -e "devtools::install_github('ropensci/datapackage')"
RUN Rscript -e "devtools::install_github('openefsa/efsagis')"
RUN Rscript -e "devtools::install_github('openefsa/efsa2016.00601')"



## Add pandoc from rstudio
ENV PATH /usr/lib/rstudio-server/bin/:$PATH

## Download and install RStudio server & dependencies
## Attempts to get detect latest version, otherwise falls back to version given in $VER
 ## Symlink pandoc, pandoc-citeproc so they are available system-wide
RUN rm -rf /var/lib/apt/lists/ \
  && apt-get update \
  && apt-get install -y -t unstable --no-install-recommends \
    ca-certificates \
    file \
    git \
    libapparmor1 \
    libedit2 \
    libcurl4-openssl-dev \
    libssl1.0.0 \
    libssl-dev \
    psmisc \
    python-setuptools \
    sudo \
  && VER=$(wget --no-check-certificate -qO- https://s3.amazonaws.com/rstudio-server/current.ver) \
  && wget -q http://download2.rstudio.org/rstudio-server-${VER}-amd64.deb \
  && dpkg -i rstudio-server-${VER}-amd64.deb \
  && rm rstudio-server-*-amd64.deb \
  && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc /usr/local/bin \
  && ln -s /usr/lib/rstudio-server/bin/pandoc/pandoc-citeproc /usr/local/bin \
  && wget https://github.com/jgm/pandoc-templates/archive/1.15.0.6.tar.gz \
  && mkdir -p /opt/pandoc/templates && tar zxf 1.15.0.6.tar.gz \
  && cp -r pandoc-templates*/* /opt/pandoc/templates && rm -rf pandoc-templates* \
  && mkdir /root/.pandoc && ln -s /opt/pandoc/templates /root/.pandoc/templates \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/


RUN Rscript -e "devtools::install_github('hrbrmstr/ggalt')"


COPY /Makefile /tmp/
COPY /analysis/mapProposals.Rmd /tmp/analysis/
RUN mkdir -p /tmp/analysis/output
RUN make -C /tmp

RUN mkdir /var/run/sshd 
RUN echo 'root:screencast' |chpasswd
EXPOSE 22
CMD    /usr/sbin/sshd -D
