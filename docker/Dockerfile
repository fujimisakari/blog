FROM node:latest
MAINTAINER fujimisakari@gmail.com

# install hexo
RUN npm install hexo-cli -g

# init
WORKDIR /blog
RUN hexo init && npm install && npm install hexo-deployer-aws-s3 hexo-deployer-rsync hexo-generator-feed --save

# create data volume
VOLUME /blog

# hexo default port
EXPOSE 4000

# set entrypoint
ADD entrypoint.sh /root/

# set credentials
ADD ./aws /root/.aws

# run hexo server
ENTRYPOINT ["/root/entrypoint.sh"]
