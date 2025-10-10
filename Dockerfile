FROM debian:bookworm-slim AS build

RUN apt-get update && apt-get install -y \
    curl ca-certificates build-essential pkg-config \
    libssl-dev clang \
    && rm -rf /var/lib/apt/lists/*

# Install rustup
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN curl -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /app
COPY . .

ENV CC=gcc
ENV Cxx=g++
ENV PKG_CONFIG_PATH=/usr/lib/pkgconfig

RUN cargo fetch

CMD ["/bin/bash"]
