#!/bin/bash

BASE_COMMAND="esmeta parse"
TARGET_FILES=("$ESMETA_HOME/tests/sample1.js" "$ESMETA_HOME/tests/sample2.js")
FILES=("$ESMETA_HOME/tests/output_1.txt" "$ESMETA_HOME/tests/output_2.txt")

echo "[LOG] Trying to test ${#TARGET_FILES[@]} files"

MATCH_COUNT=0

for i in "${!TARGET_FILES[@]}"; do
    COMMAND_OUTPUT=$($BASE_COMMAND ${TARGET_FILES[$i]})
    FILE_CONTENT=$(<"${FILES[$i]}")

    if [ "$COMMAND_OUTPUT" == "$FILE_CONTENT" ]
        then ((MATCH_COUNT++)); 
    else 
        echo "FAILED: ${TARGET_FILES[$i]}"
    fi
done

echo "[LOG] Test Completed, $MATCH_COUNT / ${#TARGET_FILES[@]} files passed."