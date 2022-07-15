#!/bin/sh
#
# shell function for uploading issues (and setting the state of closed bugs)
#
# usage:
#       upload [open | closed] <file> [ <labels> ]
#

upload_usage() {
  echo "usage: upload [open | closed] <file> [ -a <users> ] [ -l <labels> ]"
  exit 1
}

upload() {
  if [[ $# < 2 ]] ; then
    upload_usage
  fi

  # process issue state
  case $1 in
    open) state="open" ;;
    closed) state="closed" ;;
    *) upload_usage ;;
  esac
  shift

  # get the issue file
  file=$1; shift
  if [ ! -r $file ] ; then
    echo "missing issue file $file"
    exit 1
  fi

  # extract the original bug ID; we assume the file name is either
  # "bug-xxx.md" or "feature-xxx.bd"
  base=$(basename $file ".md")
  bugno=$(echo $base | sed -E 's/[a-z-]+0*//')

  users=""
  labels=""
  while [ "$#" != "0" ] ; do
    if [[ $# < 2 ]] ; then
      usage
    fi
    case $1 in
      -l) shift; labels="-l $1"; shift ;;
      -a) shift; users="-a $1"; shift ;;
      *) usage ;;
    esac
  done

  echo "hub issue create -F $file $users $labels"
#  url=$(hub issue create -F $file $users $labels)
url="https://github.com/smlnj/bug-form-test/issues/5"

  # extract the issue number from the issue URL
  issueno=$(basename $url)

  if [ x$state = xclosed ] ; then
    echo "hub issue update $issueno -s closed"
#    hub issue update $issueno -s closed
  fi

  echo "* bug **$bugno** maps to [issue $issueno]($url) [$state]"
}
