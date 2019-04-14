# 50pm - Print fifty simple plus minus questions for kids 

This is the server part.

In local development
- Start development docker: tools/dev0.sh
- Once in the container:
  - mkdir -p /deploy/data/fiftypm_api (run once as long as stays in container)
  - rebar3 shell --name svr --setcookie mycookie 
  - or, rebar3 as cohost_stg shell --name svr --setcookie mycookie

