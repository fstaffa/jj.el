# Specification: CI/CD Pipeline Enhancement

## Goal
Enhance the existing GitHub Actions workflow with comprehensive linting, testing, and coverage reporting to ensure code quality and consistency across the jj.el Emacs Lisp package.

## User Stories
- As a contributor, I want automated linting to catch package structure and documentation issues before merge so that the package maintains high quality standards
- As a maintainer, I want to see test coverage metrics displayed in GitHub so that I can identify untested code paths without external services
- As a reviewer, I want all CI checks to run independently so that I can see all issues at once rather than fixing them one at a time
- As a developer, I want the CI pipeline to test across multiple platforms and Emacs versions so that compatibility issues are caught early

## Core Requirements

### Functional Requirements
- Add three linting jobs: package-lint (validates package metadata), checkdoc (validates documentation strings), and byte-compilation warnings (catches code issues)
- All linting jobs must fail on warnings using default configurations
- Linting jobs run with fail-fast: false to show all issues simultaneously
- Integrate undercover.el for coverage reporting during test execution
- Display coverage in GitHub Actions Job Summary as markdown tables
- Post coverage summary as PR comments for easy review
- Rename aggregation job from `required-checks` to `all-checks-pass`
- Aggregation job depends on test matrix and all three linting jobs
- Maintain existing test matrix: Ubuntu, macOS, Windows with Emacs 29.4, 30.1, snapshot

### Non-Functional Requirements
- Linting jobs run on ubuntu-latest with Emacs 30.1 (stable version)
- Linting jobs execute in parallel with test matrix for faster feedback
- Coverage collection must not cause test failures if undercover.el encounters issues
- PR comment action requires GITHUB_TOKEN with write permissions
- Total CI runtime should not significantly increase (linting is fast, runs parallel to tests)
- Use eask commands for linting where available for consistency with existing setup

## Visual Design
No visual mockups provided - this is a CI/CD backend feature with text-based output in GitHub Actions interface.

## Reusable Components

### Existing Code to Leverage
- **Workflow file**: `.github/workflows/test.yml` provides the foundation structure
- **Aggregation pattern**: Current `required-checks` job pattern using jq can be extended to include linting jobs
- **Setup steps**: Existing jcs090218/setup-emacs and emacs-eask/setup-eask actions reusable for linting jobs
- **Test execution**: Current buttercup test setup via eask provides coverage integration point
- **Package management**: Eask already configured in `Eask` file with buttercup dependency

### New Components Required
- **Linting jobs**: Three new jobs (package-lint, checkdoc, byte-compilation) - no similar jobs exist currently
- **Coverage integration**: undercover.el integration with test job - not currently implemented
- **Coverage reporting**: Scripts to format coverage data as markdown and post to GitHub - new functionality
- **Dependency addition**: undercover.el must be added as development dependency in Eask file

## Technical Approach

### Database
Not applicable - CI/CD pipeline operates on GitHub Actions infrastructure.

### API
- GitHub Actions Job Summary API via `GITHUB_STEP_SUMMARY` environment variable
- GitHub REST API via GITHUB_TOKEN for posting PR comments
- Use actions like peter-evans/create-or-update-comment for PR comment automation

### Frontend
Not applicable - CI/CD backend feature.

### Testing

**Linting Job Structure (3 jobs):**
- Job: `package-lint`
  - Runs on: ubuntu-latest
  - Emacs: 30.1
  - Steps: checkout, setup-emacs, setup-eask, eask package-lint command
  - Exit code: non-zero on warnings

- Job: `checkdoc`
  - Runs on: ubuntu-latest
  - Emacs: 30.1
  - Steps: checkout, setup-emacs, setup-eask, eask checkdoc command
  - Exit code: non-zero on warnings

- Job: `byte-compile`
  - Runs on: ubuntu-latest
  - Emacs: 30.1
  - Steps: checkout, setup-emacs, setup-eask, eask compile with warnings-as-errors
  - Exit code: non-zero on warnings

**Coverage Integration:**
- Add undercover.el to development dependencies in Eask file
- Modify test execution to load undercover.el before running tests
- Configure undercover.el to track coverage for jj.el source file
- Generate coverage report after test completion
- Format as markdown table with per-file breakdown and overall percentage
- Write to `GITHUB_STEP_SUMMARY` for persistent display
- Conditionally post to PR if pull_request context available

**Aggregation Job:**
- Rename from `required-checks` to `all-checks-pass`
- Add `needs: [test, package-lint, checkdoc, byte-compile]`
- Keep `if: ${{ always() }}` to run even if some jobs fail
- Use jq to verify all dependent jobs succeeded (handle experimental: true for snapshot tests)

## Out of Scope
- Caching mechanisms for faster CI runs (not requested)
- Deployment automation or package publishing
- Quality gates or coverage thresholds (e.g., fail if coverage below X%)
- Documentation generation or publishing
- Automated changelog or release generation
- Performance benchmarking
- Security scanning tools
- External coverage service integration (Coveralls, Codecov)
- Custom linter configurations or selective warning suppression
- Changes to existing test matrix platforms or Emacs versions
- Coverage badges in README (can be added later manually)

## Success Criteria
- All three linting jobs execute successfully and fail on warnings
- Coverage metrics display in GitHub Actions Job Summary for every test run
- Coverage summary posts as comment on all pull requests
- `all-checks-pass` job correctly aggregates status of all 4 dependent jobs (test + 3 linting)
- Existing test matrix continues to work unchanged (9 test combinations: 3 platforms x 3 Emacs versions)
- Total CI pipeline execution time remains under 10 minutes
- No false failures introduced by coverage collection
