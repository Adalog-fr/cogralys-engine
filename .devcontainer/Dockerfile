FROM gnatpro-asis:24.0

ENV OS=unix

# Persist bash history
RUN SNIPPET="export PROMPT_COMMAND='history -a' && export HISTFILE=/commandhistory/.bash_history" \
    && echo $SNIPPET >> "/root/.bashrc"
