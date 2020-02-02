FROM ubuntu:latest
RUN apt-get update && \
    apt install -y llvm-8-dev zlib1g-dev gcc g++ cargo rustc && \
    cd /tmp && \
    mkdir work
ENV LLVM_SYS_80_PREFIX=/usr/lib/llvm-8
WORKDIR /tmp/work
ENTRYPOINT ["cargo"]

# docker build -t rust-llvm-ubuntu .
# docker run --rm --mount="type=bind,source=`pwd`,target=/tmp/work"  rust-llvm-ubuntu test