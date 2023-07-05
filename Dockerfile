FROM debian:bookworm-20230703

ARG username=minesweeper-bot
ARG tz='US/Pacific'

RUN apt-get update
RUN apt-get upgrade -y

RUN apt-get install -y git git-doc git-man
RUN apt-get install -y tigervnc-standalone-server openbox

RUN apt-get install -y nix
RUN apt-get install -y xterm

ENTRYPOINT ["/bin/bash"]
CMD ["-l"]
# docker build -t mine .
# docker run -v nix:/nix -v $(pwd):/root/project -it -p 5901:5901 --security-opt seccomp:unconfined mine
# vncserver -xstartup /usr/bin/openbox-session -localhost no -geometry 1024x768
