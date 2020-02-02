FROM alpine:edge
RUN apk add --no-cache llvm8-dev llvm8-static zlib-static libxml2-dev gcc g++ cargo rust && \
    mkdir work
ENV LLVM_SYS_80_PREFIX=/usr/lib/llvm8
WORKDIR /work
ENTRYPOINT ["cargo"]