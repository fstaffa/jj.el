# Specification: Testing Framework Setup

## Goal
Establish a Buttercup-based testing infrastructure for jj.el that supports isolated unit tests with mocked jj CLI interactions, integrated with Eask for local and CI test execution.

## User Stories
- As a developer, I want to run tests locally with a simple command so that I can verify changes before committing
- As a developer, I want tests to run in isolation without requiring jj installation so that the barrier to contribution is low
- As a developer, I want to mock jj command outputs so that tests are fast and reproducible
- As a contributor, I want clear test patterns to follow so that I can add tests for new features consistently

## Core Requirements

### Functional Requirements
- Set up Buttercup test framework integrated with Eask build system
- Create test directory structure following Emacs Lisp conventions
- Implement test helper utilities for mocking jj CLI command execution (using `cl-letf`)
- Add `eask test` command that runs all Buttercup tests
- Ensure tests run in complete isolation using temporary directories
- Mock all external command calls (`shell-command-to-string`, `call-process`, etc.)
- Create abstraction layer for setting up test repository scenarios and test data
- Provide initial test coverage for core functions: command parsing, log output processing, bookmark list parsing

### Non-Functional Requirements
- Tests must execute quickly (under 1 second total for initial suite)
- Tests must not require jj binary to be installed on the system
- Test output should be clear and descriptive for debugging failures
- Test infrastructure should be extensible for future test additions

## Visual Design
Not applicable - no visual components for test infrastructure.

## Reusable Components

### Existing Code to Leverage
- Buttercup is already listed as development dependency in Eask file
- Existing test file structure at `tests/test-jj.el` provides basic template
- Core functions in `jj.el` that need testing:
  - `jj--run-command`: Command execution entry point (line 47)
  - `jj--bookmarks-get`: Parses bookmark list output (line 96)
  - `jj--log-count-revs`: Parses log output to count revisions (line 81)
  - `jj--get-project-folder`: Locates .jj directory (line 26)
  - `jj--get-project-name`: Extracts project name from path (line 29)

### New Components Required
- Test helper module for mocking command execution
- Test fixture utilities for generating sample jj output
- Temporary directory setup/teardown utilities
- Test data abstraction for repository state scenarios

## Technical Approach

### Test Framework Configuration
- Use Buttercup as the testing framework (already in development dependencies)
- Configure Eask to run Buttercup with proper load paths
- Update `Eask` file to replace placeholder test script with actual Buttercup invocation

### Test Directory Structure
```
tests/
  test-jj.el           # Main test suite (already exists, needs expansion)
  test-helper.el       # Test utilities and mocking helpers (new)
  fixtures/            # Sample jj command outputs (new)
    sample-log.txt
    sample-bookmarks.txt
    sample-status.txt
```

### Mocking Strategy
- Use `cl-letf` to mock functions that execute external commands:
  - `shell-command-to-string` (used by `jj--run-command`)
  - `call-process` and related functions if needed
- Create test helper `jj-test-with-mocked-command` that:
  - Accepts a command-output mapping
  - Wraps test body in `cl-letf` to intercept command calls
  - Returns predefined output based on command string matching

### Test Isolation
- Each test suite (`describe` block) should set up its own temporary directory
- Use `make-temp-file` with `DIRECTORY` argument to create isolated test environments
- Mock `jj--get-project-folder` to return test-specific temporary directory
- Clean up temporary directories in `after-each` hooks

### Test Data Abstraction
- Create helper functions to generate common test scenarios:
  - `jj-test-setup-empty-repo`: Provides minimal jj repository context
  - `jj-test-setup-with-bookmarks`: Provides bookmark list output
  - `jj-test-setup-with-log`: Provides log output with specified number of commits
- Store fixture data in `tests/fixtures/` for complex outputs

### Initial Test Coverage
Focus on core parsing and utility functions:
- `jj--get-project-name`: Test path extraction logic
- `jj--bookmarks-get`: Test bookmark list parsing (handles newlines, empty lists)
- `jj--log-count-revs`: Test log output counting
- `jj--run-command`: Test command construction and mocking integration

### Eask Integration
- Modify Eask file script section to replace placeholder with:
  ```elisp
  (script "test" "eask exec buttercup -L . -L tests tests/")
  ```
- This runs Buttercup with proper load paths for both source and test files

### Testing Approach
- Use Buttercup's `describe`/`it` BDD-style syntax for readability
- Group related tests in `describe` blocks by function or feature area
- Use clear, descriptive test names following pattern: "should [expected behavior] when [condition]"
- Use `expect` matchers: `:to-equal`, `:to-be`, `:to-match`, `:to-throw`

## Out of Scope
- Error handling test patterns (deferred to future work after error handling is implemented)
- CI/CD workflow modifications (being handled in separate task)
- Test coverage reporting tools and metrics
- Performance testing and benchmarking
- Snapshot testing or visual regression testing
- Property-based testing frameworks
- Integration tests requiring actual jj repository interaction
- Comprehensive fixture library (start with minimal fixtures only)
- Advanced test configuration or environment variables
- Mocking of Emacs UI components (buffers, windows) for now

## Success Criteria
- `eask test` command successfully runs all Buttercup tests
- All tests pass without requiring jj binary installation
- Tests execute in under 1 second total
- At least 4 core functions have basic unit test coverage
- Test helper utilities are in place for future test additions
- Tests run in complete isolation (can run in parallel without conflicts)
- New developers can run tests immediately after cloning the repository
- Test output clearly indicates pass/fail status and failure reasons
