ARG UBUNTU_VERSION=noble
FROM ubuntu:${UBUNTU_VERSION}

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        software-properties-common \
    && add-apt-repository -y universe \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        git \
        sudo \
        xz-utils \
        locales \
        neovim \
        ripgrep \
        fd-find \
        fzf \
    && locale-gen en_US.UTF-8 \
    && ln -sf /usr/bin/fdfind /usr/local/bin/fd \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=${USER_UID}

RUN groupadd --gid ${USER_GID} ${USERNAME} 2>/dev/null || true \
    && useradd --uid ${USER_UID} --gid ${USER_GID} -m -s /bin/bash ${USERNAME} 2>/dev/null || true \
    && echo "${USERNAME} ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/${USERNAME} \
    && chmod 0440 /etc/sudoers.d/${USERNAME}

USER ${USERNAME}
WORKDIR /home/${USERNAME}
