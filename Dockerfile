# syntax=docker/dockerfile:1

FROM sbtscala/scala-sbt:eclipse-temurin-alpine-22_36_1.10.1_3.3.3 AS builder
WORKDIR /app

COPY build.sbt ./
COPY project ./project
RUN sbt --batch update

COPY . .
RUN sbt --batch assembly

FROM eclipse-temurin:22-jre-alpine

ENV ESMETA_HOME=/opt/esmeta
WORKDIR $ESMETA_HOME

COPY --from=builder /app $ESMETA_HOME

RUN chmod +x $ESMETA_HOME/bin/esmeta
ENV PATH=$ESMETA_HOME/bin:$PATH

ENTRYPOINT ["java", "-jar", "bin/esmeta"]
CMD ["help"]
