#/usr/local/bin/start_synergy &

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/* ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi


SSH_AGENT=/usr/bin/ssh-agent
## Run ssh-agent only if not already running, and available
if [ -x "${SSH_AGENT}" ] ; then

  if [ -z "${SSH_AGENT_PID}" ]; then
    eval "$(${SSH_AGENT} -s)"
  fi

fi

exec i3
