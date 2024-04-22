#!/bin/bash

LIB="Quizlet"
LIB2="Cards"

if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  open _build/default/_doc/_html/$LIB/$LIB2/
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    # WSL
    DOCPATH=$(wslpath -w ./_build/default/_doc/_html/$LIB/$LIB2/)
    explorer.exe ${DOCPATH} || true
  else
    nautilus _build/default/_doc/_html/$LIB/$LIB2/
  fi
fi
