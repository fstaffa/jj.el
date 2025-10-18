#!/bin/bash
# This script helps identify which test commands need updating

echo "=== Commands that need converting to lists ==="
echo ""
echo "In jj.el, commands are now:"
echo "  '(\"log\" \"--revisions\" \"immutable_heads()..@\" \"-T\" \"change_id ++ ' | ' ++ description.first_line() ++ ' | ' ++ bookmarks\")"
echo "  '(\"status\")"
echo "  '(\"bookmark\" \"list\" \"-T\" \"name ++ \\\"\\\\n\\\"\")"
echo ""
echo "Tests still use strings - THIS IS THE MISMATCH!"
echo ""
grep -n "cmd.*\"bookmark\|cmd.*\"log\|cmd.*\"status\|cmd.*\"describe\|cmd.*\"abandon\|cmd.*\"new" test-jj*.el | head -50
