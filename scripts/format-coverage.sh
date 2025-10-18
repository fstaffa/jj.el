#!/usr/bin/env bash
# format-coverage.sh - Format undercover.el coverage output as GitHub Flavored Markdown
#
# This script parses SimpleCov JSON output from undercover.el and formats it
# as a markdown table suitable for GitHub Actions Job Summary and PR comments.
#
# Usage:
#   ./format-coverage.sh [coverage-file.json]
#
# If no coverage file is provided, looks for coverage/.resultset.json
# Outputs markdown to stdout

set -euo pipefail

# Determine coverage file location
COVERAGE_FILE="${1:-coverage/.resultset.json}"

# Check if coverage file exists
if [[ ! -f "$COVERAGE_FILE" ]]; then
    echo "## Coverage Report"
    echo ""
    echo "**Status:** Coverage data not available"
    echo ""
    echo "Coverage collection may have failed or no coverage data was generated."
    exit 0
fi

# Parse coverage data using jq
# undercover.el JSON format: .["undercover.el"].coverage contains file paths as keys
# Each file has an array of line coverage (null = not executable, 0 = not covered, N = covered N times)

# Extract overall coverage percentage and per-file breakdown
OUTPUT=$(jq -r '
  # Get the undercover.el coverage data (try both undercover.el and RSpec keys for compatibility)
  (.["undercover.el"].coverage // .RSpec.coverage) as $coverage |

  # Calculate per-file statistics
  [
    $coverage | to_entries[] |
    {
      file: .key,
      lines: .value,
      total: ([.value[] | select(. != null)] | length),
      covered: ([.value[] | select(. != null and . > 0)] | length),
      percentage: (
        ([.value[] | select(. != null)] | length) as $total |
        if $total == 0 then 0 else
          (([.value[] | select(. != null and . > 0)] | length) / $total * 100)
        end
      )
    }
  ] as $files |

  # Calculate overall statistics
  ($files | map(.total) | add) as $total_lines |
  ($files | map(.covered) | add) as $covered_lines |
  (if $total_lines == 0 then 0 else ($covered_lines / $total_lines * 100) end) as $overall |

  # Format as markdown
  "## Coverage Report\n\n",
  "**Overall Coverage:** \($overall | floor)%\n\n",
  "| File | Coverage | Lines Covered | Total Lines |\n",
  "| ---- | -------- | ------------- | ----------- |\n",
  (
    $files[] |
    "| \(.file | split("/") | last) | \(.percentage | floor)% | \(.covered) | \(.total) |"
  ),
  "\n"
' "$COVERAGE_FILE" 2>/dev/null)

# Check if jq parsing succeeded
if [[ -z "$OUTPUT" ]]; then
    echo "## Coverage Report"
    echo ""
    echo "**Status:** Failed to parse coverage data"
    echo ""
    echo "The coverage file format may be invalid or incompatible."
    exit 0
fi

# Output the formatted markdown
echo "$OUTPUT"
