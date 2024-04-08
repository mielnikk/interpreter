#!/usr/bin/env bash

interpreter=../generated/TestTortex
examples_path=.

echo -e "================================================================="
echo -e "Running correct examples. All should execute properly"

for example in $examples_path/good/*.tx; do
  echo -e "-----------------------------------------------------------------"
  echo -e "Running $example:\n"
  echo -e "-----"
  cat "$example" | ./$interpreter
done

echo -e "================================================================="
echo -e "Running incorrect examples. All should fail"

for example in $examples_path/bad/*.tx; do
  echo -e "-----------------------------------------------------------------"
  echo -e "Running $example:\n"
  echo -e "-----"
  cat "$example" | ./$interpreter
done