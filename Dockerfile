FROM clojure:openjdk-16-tools-deps-alpine
COPY . /usr/src/app
WORKDIR /usr/src/app
CMD ["clojure", "-M:server"]

