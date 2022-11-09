FROM rocker/r-devel


RUN apt-get --allow-releaseinfo-change update
RUN apt-get remove -y libxml2 libxml2-dev
RUN apt-get install -y libxml2 libxml2-dev libssl-dev

RUN RDscript -e 'install.packages("devtools")'

COPY . /home/docker
WORKDIR /home/docker

RUN RDscript -e 'devtools::install_deps()'

CMD ["RDscript", "-e", "devtools::test()"]
