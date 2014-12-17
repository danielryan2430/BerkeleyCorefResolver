#!/bin/bash

function usage {
cat <<EOF



----------------------------------------------------------------------------------------------------
Usage:
-----

${0##*/}  -D <ontonotes-release-data-directory> <top-level-directory>


Description:
-----------

<ontonotes-release-data-directory>: Location of the data directory under the OntoNotes 4.0 release
             <top-level-directory>: The directory inside which the *_skel files exist and need to
                                    be convered to .conll files

----------------------------------------------------------------------------------------------------




EOF
exit;
}


function message
{
  echo "----------------------------------------------------------------------------------------------------"
  echo
  echo $* 1>&2
  echo
  echo "----------------------------------------------------------------------------------------------------"

}



function r { echo ${1%.*}; }
function t { echo ${1##*/}; }
function e { echo $(t ${1##*.}); }
function h { echo ${1%/*}; }

# define helper function: run a command and print its exit code
function erun () {
  debug=0
  if [[ $1 == "-d" ]]; then
    debug=1
    shift;
  fi


  if [[ $DEBUG -eq 1 ]]; then
      debug=1
  fi




  verbose=0
  if [[ $1 == "-v" ]]; then
    verbose=1
    shift;
  fi


  if [[ $VERBOSE -eq 1 ]]; then
      verbose=1
  fi






  if [[ $debug -eq 1 ]]; then
    echo "debug mode ..."
    echo "eval $1"
  else
    echo "normal mode ..."
    if [[ $verbose -eq 1 ]]; then
      echo -e "\nrun: $1\n-------------"
    fi

    eval $1
  fi


  local code=$?
  if [ $code -ne 0 ]; then
	  echo "Exit code: $code"
	  exit $code
  fi
}




# handle the valid command line options
DEBUG=0
VERBOSE=0
DEBUG_OPTION=""
while getopts D:dh opt
do
  case "$opt" in
    v)
      VERBOSE=1;;

    d)
      DEBUG=1;;

    D)
      ON_DATA_DIR="$OPTARG";;

    \?)
      usage
      exit 1;;

    h)
      usage
      exit 0;;

    :)
      echo "option -$OPTARG requires an argument"
      usage
      exit 1;;

    esac
done
shift `expr $OPTIND - 1`


# at this point $* contains the arguments after interpreting the options

d=$1

# if no arguments are specified, then just print usage
if [[ $# -eq 0 ]]; then
    usage
fi



if [[ -z $ON_DATA_DIR ]]; then
  usage
fi



# debugging
if [[ $DEBUG -eq 1 ]]; then
    echo "debugging mode is on ..." 1>&2
    DEBUG_OPTION="-d"
fi




for skel in $(find $d -name "*_skel"); do
  gp=$ON_DATA_DIR/$(r ${skel/*data/}).parse

  if [[ ! -e $gp ]]; then
    echo "could not find the gold parse in the ontonotes release ... exiting ..."
    exit
  fi

  conll=${skel/_skel/_conll}
  echo python skeleton2conll.py $gp $skel $conll -edited --text
  python skeleton2conll.py $gp $skel $conll -edited --text
done









# complain if the exit status of the last command executed is non-zero
if [[ $? != 0 ]]; then echo "the last command exited with a non-zero status" 1>&2; fi



