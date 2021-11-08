#!/bin/sh

# bbc search

# base url and query string
baseurl='https://www.bbc.co.uk/iplayer/search?'
query="${QUERY_STRING}"
url="${baseurl}${query}"

# css selector
css='div.list.search-list'

# css exclude
search='search-list__header'

# outfile
outfile='/tmp/bbc-search.html'

# hxselect and sed
hxnormalize -x "${url}" \
| hxselect -s '\n' -c "${css}" \
| hxprune -c "${search}" \
| sed -e 's#/iplayer/#https://www.bbc.co.uk/iplayer/#g' \
-e "/<a/ { /href/ s/.*href=['\"]https:\/\/www.bbc.co.uk\/iplayer\/episode\/.*['\"]\([^<]*\)/&play/g }" \
-e 's#?q=#https://www.bbc.co.uk/iplayer/search?q=#g' \
> "${outfile}"

# W3m-control
printf "%s\r\n" "W3m-control: GOTO ${outfile}";
# delete previous buffer
printf "%s\r\n" "W3m-control: DELETE_PREVBUF";

# clear screen
printf "\033c"
