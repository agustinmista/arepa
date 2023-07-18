from haskell:9.2

SHELL ["/bin/bash", "-c"]

# ----------------------------------------
# Install global dependencies

ENV TZ=Europe/Stockholm
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update && \
    apt-get -y install --no-install-suggests --no-install-recommends \
    sudo \
    vim \
    git \
    clang \
    g++

# ----------------------------------------
# Run the rest as non-root

# Setup a new user
ARG USER_NAME=arepa
ARG UID=1000
ARG GID=1000

RUN groupadd -g $GID -o $USER_NAME
RUN useradd -m -u $UID -g $GID -G sudo -o -s /bin/bash -d /home/$USER_NAME $USER_NAME
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Move into userland
USER $USER_NAME

# Prepare the user's workspace
RUN sudo mkdir -p /workspace && \
    sudo chown $USER_NAME:$USER_NAME /workspace

WORKDIR /workspace

# ----------------------------------------
# Copy the source files into the container

COPY --chown=$UID:$GID ./ /workspace

# ----------------------------------------
# Build the project

RUN cabal update && \
    cabal build

# ----------------------------------------
# Set up the entrypoint

ENTRYPOINT ["/bin/bash"]
