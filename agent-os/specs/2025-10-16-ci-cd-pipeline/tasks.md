# Task Breakdown: CI/CD Pipeline Enhancement

## Overview
Total Task Groups: 4
Assigned roles: Custom DevOps Engineer (general implementer role)
Project Type: Emacs Lisp Package with GitHub Actions CI/CD

## Notes on Implementation Approach

Since this is a CI/CD configuration feature for an Emacs Lisp project (not a typical backend/frontend application), the standard database-engineer, api-engineer, and ui-designer roles don't apply. Instead, all tasks will be handled by a general implementer with DevOps/CI expertise who can:
- Work with GitHub Actions YAML configuration
- Understand Emacs Lisp tooling (eask, buttercup, undercover.el)
- Implement shell scripts for coverage reporting
- Configure linting tools (package-lint, checkdoc, byte-compilation)

## Task List

### Dependency Configuration

#### Task Group 1: Add Coverage and Linting Dependencies
**Assigned implementer:** General implementer (DevOps focus)
**Dependencies:** None

- [x] 1.0 Configure project dependencies for coverage and linting
  - [x] 1.1 Write 2-3 focused tests to verify dependency installation
    - Test that undercover.el can be loaded without errors
    - Test that package-lint is available via eask
    - Skip exhaustive testing of tool functionality
  - [x] 1.2 Add undercover.el to development dependencies in Eask file
    - Add `(depends-on "undercover")` in development section
    - Follow existing pattern in Eask file (line 18-19)
  - [x] 1.3 Verify eask can install and load new dependencies
    - Run `eask install` to verify dependency resolution
    - Test that `eask exec emacs --batch -l undercover` works
  - [x] 1.4 Ensure dependency tests pass
    - Run ONLY the 2-3 tests written in 1.1
    - Verify Eask file syntax is valid
    - Do NOT run the entire test suite at this stage

**Acceptance Criteria:**
- The 2-3 tests written in 1.1 pass
- Eask file includes undercover.el as development dependency
- Dependencies install successfully
- No syntax errors in Eask file

### Linting Jobs Implementation

#### Task Group 2: Add Three Linting Jobs to GitHub Actions
**Assigned implementer:** General implementer (DevOps focus)
**Dependencies:** None (can run in parallel with Task Group 1)

- [x] 2.0 Implement linting jobs in GitHub Actions workflow
  - [x] 2.1 Write 2-8 focused tests for linting job execution
    - Limit to 2-8 highly focused tests maximum
    - Test only critical linting behaviors (e.g., checkdoc catches undocumented function, package-lint validates metadata, byte-compile catches warnings)
    - Skip exhaustive testing of all linting scenarios
  - [x] 2.2 Create package-lint job in .github/workflows/test.yml
    - Run on ubuntu-latest with Emacs 30.1
    - Setup steps: checkout, setup-emacs, setup-eask
    - Execute: `eask lint package` (or equivalent package-lint command)
    - Configure to fail on warnings (non-zero exit code)
  - [x] 2.3 Create checkdoc job in .github/workflows/test.yml
    - Run on ubuntu-latest with Emacs 30.1
    - Setup steps: checkout, setup-emacs, setup-eask
    - Execute: `eask lint checkdoc` (or equivalent checkdoc command)
    - Configure to fail on warnings (non-zero exit code)
  - [x] 2.4 Create byte-compile job in .github/workflows/test.yml
    - Run on ubuntu-latest with Emacs 30.1
    - Setup steps: checkout, setup-emacs, setup-eask
    - Execute: `eask compile --strict` or equivalent to fail on warnings
    - Configure to fail on warnings (non-zero exit code)
  - [x] 2.5 Configure fail-fast: false for linting jobs
    - Add strategy.fail-fast: false to show all linting issues simultaneously
    - Ensure jobs run in parallel with test matrix
  - [x] 2.6 Ensure linting job tests pass
    - Run ONLY the 2-8 tests written in 2.1
    - Verify all three linting jobs execute correctly
    - Do NOT run the entire test suite at this stage

**Acceptance Criteria:**
- The 2-8 tests written in 2.1 pass
- Three linting jobs added: package-lint, checkdoc, byte-compile
- All jobs run on ubuntu-latest with Emacs 30.1
- Jobs fail on warnings (non-zero exit code)
- Jobs run in parallel (fail-fast: false)
- Existing test matrix continues to work unchanged

### Coverage Reporting Integration

#### Task Group 3: Integrate Coverage Reporting with undercover.el
**Assigned implementer:** General implementer (DevOps focus)
**Dependencies:** Task Groups 1 and 2

- [x] 3.0 Implement coverage reporting system
  - [x] 3.1 Write 2-8 focused tests for coverage reporting
    - Limit to 2-8 highly focused tests maximum
    - Test only critical coverage behaviors (e.g., coverage data is collected, markdown report is generated, summary includes percentage)
    - Skip exhaustive testing of all report formats and edge cases
  - [x] 3.2 Modify test job to load undercover.el before tests
    - Update "Run tests" step in .github/workflows/test.yml
    - Configure undercover.el to track jj.el source file
    - Ensure coverage collection doesn't cause test failures
    - Example: `eask exec emacs --batch -l undercover -l jj.el --eval '(buttercup-run)'`
  - [x] 3.3 Create coverage report formatting script
    - Create script to parse undercover.el output
    - Format as GitHub Flavored Markdown table
    - Include overall coverage percentage
    - Include per-file breakdown (if multiple files)
    - Use /usr/bin/env for shebang (not absolute paths)
  - [x] 3.4 Add coverage to GitHub Actions Job Summary
    - Append coverage markdown to $GITHUB_STEP_SUMMARY
    - Display in workflow run summary page
    - Ensure formatting is readable in GitHub UI
  - [x] 3.5 Add coverage to PR comments
    - Use peter-evans/create-or-update-comment action
    - Post coverage summary on pull requests
    - Update existing comment to avoid spam
    - Handle cases where PR context is not available
    - Configure GITHUB_TOKEN with write permissions
  - [x] 3.6 Add error handling for coverage collection
    - Ensure test job doesn't fail if undercover.el encounters issues
    - Display "Coverage: N/A" if collection fails
    - Log coverage errors for debugging
  - [x] 3.7 Ensure coverage reporting tests pass
    - Run ONLY the 2-8 tests written in 3.1
    - Verify coverage data is collected and displayed
    - Do NOT run the entire test suite at this stage

**Acceptance Criteria:**
- The 2-8 tests written in 3.1 pass
- Coverage data collected during test execution
- Coverage report displayed in GitHub Actions Job Summary
- Coverage summary posted as PR comments
- Test job doesn't fail if coverage collection fails
- Coverage shows percentage and per-file breakdown

### Workflow Aggregation Update

#### Task Group 4: Update Aggregation Job and Final Verification
**Assigned implementer:** General implementer (DevOps focus)
**Dependencies:** Task Groups 1, 2, and 3

- [x] 4.0 Update aggregation job and perform final verification
  - [x] 4.1 Review existing workflow and identify integration points
    - Review current required-checks job implementation
    - Identify dependencies on test job
    - Review jq command for job status verification
  - [x] 4.2 Rename aggregation job from required-checks to all-checks-pass
    - Update job name in .github/workflows/test.yml
    - Preserve existing job logic and structure
  - [x] 4.3 Update aggregation job dependencies
    - Change `needs: [test]` to `needs: [test, package-lint, checkdoc, byte-compile]`
    - Keep `if: ${{ always() }}` to run even if some jobs fail
    - Ensure jq command correctly verifies all dependent jobs succeeded
    - Handle experimental: true for snapshot tests (allow experimental failures)
  - [x] 4.4 Verify workflow trigger configuration
    - Confirm existing triggers maintained: push to master, pull_request, workflow_dispatch
    - Confirm concurrency settings maintained: cancel-in-progress: true
    - No changes needed to trigger configuration
  - [x] 4.5 Test complete workflow end-to-end
    - Run workflow on a test branch
    - Verify all linting jobs execute and report results
    - Verify coverage reporting displays in Job Summary
    - Verify coverage posts to PR (if PR available)
    - Verify all-checks-pass job aggregates status correctly
    - Verify existing test matrix still works (9 combinations)
  - [x] 4.6 Verify total CI runtime is acceptable
    - Measure total workflow execution time
    - Ensure runtime remains under 10 minutes
    - Linting jobs should complete quickly (under 2 minutes)
    - Test matrix remains the bottleneck (expected)
  - [x] 4.7 Update tech-stack.md documentation
    - Document CI/CD pipeline tools: GitHub Actions, eask, buttercup, undercover.el
    - Document linting tools: package-lint, checkdoc, byte-compilation
    - Document coverage reporting approach: undercover.el with GitHub native display
    - Document minimum Emacs version: 29.1 (already in Eask file)

**Acceptance Criteria:**
- Aggregation job renamed to all-checks-pass
- Aggregation job depends on test + 3 linting jobs
- Complete workflow executes successfully end-to-end
- All linting jobs run in parallel with tests
- Coverage displays in Job Summary and PR comments
- Total CI runtime under 10 minutes
- Documentation updated in tech-stack.md
- Existing test matrix unchanged and working

## Execution Order

Recommended implementation sequence:
1. **Dependency Configuration** (Task Group 1) - Adds undercover.el to Eask file
2. **Linting Jobs Implementation** (Task Group 2) - Can start in parallel with Task Group 1, adds three linting jobs
3. **Coverage Reporting Integration** (Task Group 3) - Depends on undercover.el dependency being available, integrates coverage collection and reporting
4. **Workflow Aggregation Update** (Task Group 4) - Final integration, renames and updates aggregation job, verifies entire workflow

## Implementation Notes

### GitHub Actions Workflow Structure
After implementation, the workflow will have:
- 1 test job (matrix of 9: 3 platforms x 3 Emacs versions)
- 3 linting jobs (package-lint, checkdoc, byte-compile)
- 1 aggregation job (all-checks-pass)
- Total: 13 jobs (9 test matrix jobs + 3 linting + 1 aggregation)

### Parallel Execution
- Linting jobs run in parallel with test matrix for faster feedback
- Linting jobs only need ubuntu-latest (no multi-platform required)
- Test matrix continues to run on all platforms as before

### Coverage Implementation Details
- undercover.el must be loaded before tests run
- Coverage data collected during test execution
- Coverage report formatted as markdown
- Display in two locations: Job Summary (always) and PR comments (when PR available)

### Error Handling
- Coverage collection should not fail tests
- PR comment should handle non-PR contexts gracefully
- Aggregation job should handle experimental job failures (snapshot tests)

### Testing Strategy
- Each task group writes 2-8 focused tests maximum
- Tests verify critical behaviors only
- Test verification runs ONLY newly written tests, not entire suite
- Task Group 4 performs final end-to-end verification
- Total expected tests: approximately 8-21 tests maximum

### Compliance with Standards
- Follows global/conventions.md: Clear commit messages, feature branches, documentation
- Follows testing/test-writing.md: Minimal tests, focus on core flows, defer edge cases
- Uses /usr/bin/env for script shebangs (per user preferences)
- No mention of Claude in commit messages (per user preferences)
- Commit extended messages include "related to: [gitlab issue URL]" (per user preferences)
