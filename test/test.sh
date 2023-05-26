#!/bin/bash

stop_on_failure="${1:-t}"
debug="${2:-nil}"
print="${3:-(quote try:unexpected)}"
describe="${4:-(quote try:unexpected)}"
num_passes=
num_failures=

function run_test_case {
  local test_case_name="\"$1\""
  shift
  echo "SHTEST: Running ${test_case_name} $@"
  $@
  local retval=$?
  if ((retval == 22)); then
    echo
    echo "SHTEST: ${test_case_name} PASS"
    num_passes=$((num_passes+1))
  else
    echo
    echo "SHTEST: ${test_case_name} FAIL"
    num_failures=$((num_failures+1))
  fi
}

function lisp_tests {
  local lisp_name="$1"
  shift

  run_test_case "lisp test suite on ${lisp_name}" $@ <<EOF
(quicklisp:quickload :journal/test)
(when (try:passedp (journal-test::test :debug ${debug} :print ${print}
                                       :describe ${describe}))
  (uiop/image:quit 22))
EOF
}

function run_tests {
  local test_suite="$1"
  local lisp="$2"
  shift; shift
  echo
  echo "SHTEST: running test suite ${test_suite} with ${lisp} $@"
  num_failures=0
  num_passes=0
  ${test_suite} ${lisp} ros --lisp ${lisp} run -- $@
  if ((num_failures > 0)); then
    if [ "${stop_on_failure}" = "t" ]; then
      echo "SHTEST: Aborting with ${num_failures} failures,"\
           "${num_passes} passes."
      exit 1
    fi
  fi
}

run_tests lisp_tests sbcl --noinform
# Runs out of heap in the Express version.
# run_tests lisp_tests allegro --batch --backtrace-on-error
run_tests lisp_tests ccl-bin
run_tests lisp_tests cmu-bin -batch
run_tests lisp_tests ecl
# Some encoding related problems with CLISP unders Roswell. Seems to
# work fine with the system binary.
# run_tests lisp_tests clisp -on-error exit
run_tests lisp_tests abcl-bin

echo "SHTEST: ${num_failures} failures, ${num_passes} passes."
