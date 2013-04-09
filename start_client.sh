#! /bin/bash

# +K true: enable kernel poll
# -boot start_sasl: start sasl
erl -pa apps/*/ebin -boot start_sasl +K true -s proxy_client
