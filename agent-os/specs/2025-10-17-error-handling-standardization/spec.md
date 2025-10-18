# Specification: Error Handling Standardization

## Goal

Establish a robust, user-friendly error handling foundation for jj.el that properly captures command failures, provides clear error messages, and enables debugging through comprehensive logging.

## User Stories

- As a user, I want clear error messages when something goes wrong so that I understand what happened and how to fix it
- As a user, I want to know immediately if I'm not in a jj repository so that I don't waste time trying to run commands
- As a developer, I want debug logging available so that I can troubleshoot issues when they occur
- As a developer, I want detailed error context preserved so that I can investigate command failures effectively
- As a user, I want appropriate error severity levels so that minor issues don't panic me while serious problems get my attention

## Core Requirements

### Functional Requirements

- Implement three-tier error categorization:
  - User errors: Invalid input, no .jj repository found, invalid revsets
  - Command failures: jj command exits with non-zero status, Git remote errors, conflict states
  - System errors: jj binary not found, file system issues
- Use appropriate Emacs error signaling:
  - `message` for informational/minor issues
  - `user-error` for user-actionable problems
  - `error` for serious system-level failures
- Validate repository context before executing commands (check for .jj folder)
- Capture exit codes and separate stdout/stderr from command execution
- Preserve comprehensive error context (command, exit code, stderr) in dedicated buffer
- Implement debug mode with configurable logging to *Messages* buffer

### Non-Functional Requirements

- Command execution must be synchronous to maintain predictable control flow
- Error handling must not introduce significant performance overhead
- Debug logging must be disabled by default to avoid cluttering *Messages*
- Error buffer must be accessible but not intrusive to normal workflow
- Breaking changes to function signatures are acceptable (v0.0.1)

## Visual Design

No visual assets provided - this is a backend infrastructure feature.

## Reusable Components

### Existing Code to Leverage

- `jj--get-project-folder`: Already implements .jj folder detection via `locate-dominating-file`
- `jj--run-command`: Current command execution function to be replaced
- Test infrastructure: Buttercup tests with comprehensive mocking patterns in `tests/test-jj.el`
- Buffer management patterns: Existing buffer creation in `jj-status` and `jj--log-show`

### New Components Required

- **Command execution wrapper**: Replace `shell-command-to-string` with `call-process` to capture exit codes and stderr
  - Why: Current implementation cannot distinguish success from failure or separate error output
- **Error categorization functions**: Helper functions to classify errors into user/command/system categories
  - Why: No existing error classification system
- **Debug logging utility**: Conditional logging function that respects debug flag
  - Why: No existing logging infrastructure
- **Repository validation wrapper**: Function to check .jj presence before command execution
  - Why: Current code doesn't validate repository context before running commands
- **Error buffer management**: Functions to write and display detailed error context
  - Why: No existing error context preservation mechanism

## Technical Approach

### Database

Not applicable - no database in this Emacs package.

### API

**Current State Analysis:**
- `jj--run-command` uses `shell-command-to-string` which:
  - Cannot capture exit codes
  - Combines stdout and stderr
  - Provides no error context
  - Returns empty string on failure
- Functions assume commands succeed (no error handling)
- No repository validation before command execution

**Proposed Command Execution:**

Replace `jj--run-command` implementation:
- Use `call-process` with temporary buffers for stdout and stderr
- Return structured result: `(success . (stdout . stderr . exit-code))`
- Execute from project root via `default-directory` binding
- Log command execution when debug mode enabled

**New Error Handling Functions:**

- `jj--validate-repository`: Check for .jj folder, signal user-error if missing
- `jj--debug-log`: Conditional logging to *Messages* when `jj-debug-mode` is non-nil
- `jj--handle-command-error`: Classify and signal appropriate error type
- `jj--write-error-buffer`: Write detailed error context to *jj-errors* buffer

**Function Signature Changes:**

- `jj--run-command`: Change return value from string to structured result
- All command wrappers (status, log, abandon, etc.): Add error handling and validation
- Update callers to handle new return format and potential errors

**Configuration Variables:**

- `jj-debug-mode`: Boolean flag to enable debug logging (default: nil)
- `jj-error-buffer-name`: Name of error context buffer (default: "*jj-errors*")

### Frontend

Not applicable - no web frontend in this Emacs package.

### Testing

**Test Coverage Requirements:**

- Repository validation: Test with and without .jj folder
- Command success: Verify successful execution with exit code 0
- Command failure: Test non-zero exit codes trigger appropriate errors
- Error categorization: Test each error type signals correct Emacs error
- Debug logging: Verify logging only occurs when flag enabled
- Error buffer: Test error context is written correctly
- Backward compatibility: Update all existing tests for new function signatures

**Testing Pattern:**

Continue using Buttercup with mocking:
- Mock `call-process` to control exit codes and output
- Mock `locate-dominating-file` for repository detection scenarios
- Verify error signals using `condition-case`
- Test debug logging by checking *Messages* buffer content

## Out of Scope

- Automatic retry or recovery mechanisms for failed commands
- Exponential backoff for transient failures
- Network-specific error handling
- Dedicated error display buffer for routine errors (only detailed context in *jj-errors*)
- Backward compatibility with existing function signatures
- Asynchronous command execution
- Error aggregation or error history tracking
- User-configurable error message templates

## Success Criteria

- All jj commands validate repository context before execution
- Failed commands (non-zero exit) signal appropriate error type
- Exit codes, stderr, and command details preserved in *jj-errors* buffer
- Debug mode logs command execution details to *Messages*
- User errors use `user-error`, system errors use `error`, info uses `message`
- No test regressions - all existing tests updated and passing
- Repository validation prevents confusing error messages when not in .jj project
- Error messages are clear and actionable for users
