#!/bin/sh

# bbc episodes

# current link under cursor in w3m
url="${W3M_CURRENT_LINK}"   

# if the current link contains a url pipe it into grep,
# remove the google redirect and decode the url
#if the current link is empty set the url to the page url
if [ ! -z "${url}" ]; then
   url="${url}"
else
    url="${W3M_URL}"
fi

# css selector
css='div.tleo-list'

# css exclude
secondary='content-item__info__secondary'

# outfile
outfile='/tmp/bbc-episodes.html'

# hxselect and sed
hxnormalize -x "${url}" \
| hxselect -s '\n' -c "${css}" \
| hxprune -c "${secondary}" \
| sed -e 's#/iplayer/#https://www.bbc.co.uk/iplayer/#g' \
-e "/<a/ { /href/ s/.*href=['\"]https:\/\/www.bbc.co.uk\/iplayer\/episode\/.*['\"]\([^<]*\)/&play/g }" \
> "${outfile}"

# W3m-control
printf "%s\r\n" "W3m-control: GOTO ${outfile}";
# delete previous buffer
printf "%s\r\n" "W3m-control: DELETE_PREVBUF";

# clear screen
printf "\033c"
