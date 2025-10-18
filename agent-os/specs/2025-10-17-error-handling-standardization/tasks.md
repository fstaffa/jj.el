# Task Breakdown: Error Handling Standardization

## Overview
Total Tasks: 47 sub-tasks across 5 major task groups
Assigned roles: api-engineer (core infrastructure), testing-engineer (test coverage)
Estimated Completion: 6-8 hours

## Task List

### Core Infrastructure Layer

#### Task Group 1: Command Execution Infrastructure
**Assigned implementer:** api-engineer
**Dependencies:** None
**Rationale:** This group establishes the foundation by replacing `shell-command-to-string` with `call-process` and implementing the new command execution wrapper. All other work depends on this foundation.

- [x] 1.0 Complete command execution infrastructure
  - [x] 1.1 Write 4-6 focused tests for new command execution wrapper
    - Test successful command execution (exit code 0)
    - Test failed command execution (non-zero exit)
    - Test stdout/stderr separation
    - Test execution from correct project directory
    - Skip exhaustive edge case testing
  - [x] 1.2 Implement new `jj--run-command` with `call-process`
    - Replace `shell-command-to-string` with `call-process`
    - Use temporary buffers for stdout and stderr capture
    - Return structured result: `(success-flag stdout stderr exit-code)`
    - Execute from project root via `default-directory` binding
    - Preserve existing command construction logic (--no-pager, --color never)
    - Reference: Lines 48-54 in jj.el for current implementation
  - [x] 1.3 Add configuration variables
    - Define `jj-debug-mode` (boolean, default: nil)
    - Define `jj-error-buffer-name` (string, default: "*jj-errors*")
    - Add defcustom declarations with proper documentation
  - [x] 1.4 Ensure command execution tests pass
    - Run ONLY the 4-6 tests written in 1.1
    - Verify structured return format works correctly
    - Do NOT run the entire test suite at this stage

**Acceptance Criteria:**
- The 4-6 tests written in 1.1 pass
- `jj--run-command` returns structured result with exit code
- stdout and stderr are captured separately
- Commands execute from project root directory
- Configuration variables are properly defined

### Error Categorization and Handling

#### Task Group 2: Error Handling Functions
**Assigned implementer:** api-engineer
**Dependencies:** Task Group 1 (COMPLETED)
**Rationale:** With the command execution infrastructure in place, we can now build the error handling system that uses the exit codes and stderr output.

- [x] 2.0 Complete error handling functions
  - [x] 2.1 Write 5-7 focused tests for error handling
    - Test user-error signals for user errors (invalid input, no .jj repo)
    - Test error signals for system errors (jj binary not found)
    - Test message for informational issues
    - Test error categorization logic
    - Test error buffer writing
    - Limit to critical error handling paths only
  - [x] 2.2 Implement `jj--validate-repository`
    - Use existing `jj--get-project-folder` to check for .jj
    - Signal `user-error` with message "Not in a jj repository" if missing
    - Return project folder path if valid
    - Keep function simple and focused
  - [x] 2.3 Implement `jj--handle-command-error`
    - Accept parameters: command, exit-code, stderr, stdout
    - Categorize errors into three types:
      - User errors: Exit codes 1-2, "invalid" in stderr -> `user-error`
      - Command failures: Exit codes 1-255 -> `error` with command context
      - System errors: Binary not found, file system issues -> `error`
    - Use appropriate Emacs error signaling (message/user-error/error)
    - Call `jj--write-error-buffer` before signaling
  - [x] 2.4 Implement `jj--write-error-buffer`
    - Create or get buffer named by `jj-error-buffer-name`
    - Write timestamp, command, exit code, stderr, stdout
    - Format for readability with clear section headers
    - Keep buffer accessible but don't auto-display
    - Use `with-current-buffer` and `insert` for writing
  - [x] 2.5 Ensure error handling tests pass
    - Run ONLY the 5-7 tests written in 2.1
    - Verify error categorization works correctly
    - Verify error buffer contains expected content
    - Do NOT run the entire test suite at this stage

**Acceptance Criteria:**
- The 5-7 tests written in 2.1 pass
- Repository validation signals user-error when not in .jj repo
- Errors are categorized into user/command/system types
- Error context is written to *jj-errors* buffer
- Appropriate error signaling used for each category

### Debug Logging Infrastructure

#### Task Group 3: Debug Logging System
**Assigned implementer:** api-engineer
**Dependencies:** Task Groups 1-2 (BOTH COMPLETED)
**Rationale:** With command execution and error handling established, add debug logging to provide visibility when troubleshooting.

- [x] 3.0 Complete debug logging system
  - [x] 3.1 Write 3-4 focused tests for debug logging
    - Test logging occurs when `jj-debug-mode` is t
    - Test no logging when `jj-debug-mode` is nil
    - Test log message format includes command and result
    - Skip testing message buffer internals
  - [x] 3.2 Implement `jj--debug-log`
    - Accept format string and arguments (like `message`)
    - Check `jj-debug-mode` flag before logging
    - Log to *Messages* buffer using `message`
    - Prefix messages with "[jj-debug]" for easy filtering
    - Keep function simple, no complex formatting
  - [x] 3.3 Integrate debug logging into `jj--run-command`
    - Log command before execution
    - Log exit code and success/failure after execution
    - Log stderr if present and non-empty
    - Use `jj--debug-log` function
  - [x] 3.4 Integrate debug logging into error handlers
    - Log when `jj--validate-repository` fails
    - Log when `jj--handle-command-error` is called
    - Include relevant context (command, exit code, error type)
  - [x] 3.5 Ensure debug logging tests pass
    - Run ONLY the 3-4 tests written in 3.1
    - Verify conditional logging based on debug flag
    - Verify log message format is correct
    - Do NOT run the entire test suite at this stage

**Acceptance Criteria:**
- The 3-4 tests written in 3.1 pass
- Debug logging respects `jj-debug-mode` flag
- Command execution is logged when debug mode enabled
- Error handling events are logged with context
- Log messages are clearly prefixed

### Function Migration and Integration

#### Task Group 4: Update Existing Functions
**Assigned implementer:** api-engineer
**Dependencies:** Task Groups 1-3 (ALL COMPLETED)
**Rationale:** With all infrastructure in place, migrate existing functions to use the new command execution and error handling system.

- [x] 4.0 Complete function migration
  - [x] 4.1 Write 4-6 focused tests for migrated functions
    - Test `jj-status` with repository validation
    - Test `jj--bookmarks-get` with error handling
    - Test `jj--log-count-revs` with new return format
    - Test one transient command wrapper (e.g., `jj-status-describe`)
    - Limit to critical user-facing functions only
  - [x] 4.2 Update `jj-status` function
    - Add `jj--validate-repository` call at start
    - Update to handle new `jj--run-command` return format
    - Extract stdout from structured result
    - Add error handling for command failures
    - Reference: Lines 56-65 in jj.el for current implementation
  - [x] 4.3 Update `jj--bookmarks-get` function
    - Add `jj--validate-repository` call at start
    - Update to handle new `jj--run-command` return format
    - Extract stdout and parse bookmarks
    - Add error handling for command failures
    - Reference: Lines 97-99 in jj.el for current implementation
  - [x] 4.4 Update `jj--log-count-revs` function
    - Add `jj--validate-repository` call at start
    - Update to handle new `jj--run-command` return format
    - Extract stdout and count revisions
    - Add error handling for command failures
    - Reference: Lines 82-85 in jj.el for current implementation
  - [x] 4.5 Update `jj--log-show` function
    - Add `jj--validate-repository` call at start
    - Update to handle new `jj--run-command` return format
    - Extract stdout and display in buffer
    - Add error handling for command failures
    - Reference: Lines 124-133 in jj.el for current implementation
  - [x] 4.6 Update command wrapper functions
    - Update `jj-status-describe` (lines 68-73)
    - Update `jj-status-abandon` (lines 106-111)
    - Update `jj--new` (lines 184-189)
    - Update `jj--fetch` (lines 210-216)
    - Update `jj--push` (lines 229-234)
    - Add repository validation to each
    - Handle new return format from `jj--run-command`
    - Preserve existing transient argument handling
  - [x] 4.7 Update `jj--status-abandon-revset-from-trunk` function
    - Add repository validation
    - Update to handle new return formats
    - Ensure error handling works with user confirmation flow
    - Reference: Lines 87-95 in jj.el for current implementation
  - [x] 4.8 Ensure function migration tests pass
    - Run ONLY the 4-6 tests written in 4.1
    - Verify repository validation works for key functions
    - Verify functions handle command failures gracefully
    - Do NOT run the entire test suite at this stage

**Acceptance Criteria:**
- The 4-6 tests written in 4.1 pass
- All user-facing functions validate repository context
- Functions handle new structured command results
- Error handling is consistent across all functions
- Breaking changes are documented (v0.0.1 acceptable)

### Comprehensive Testing and Verification

#### Task Group 5: Test Suite Update and Verification
**Assigned implementer:** testing-engineer
**Dependencies:** Task Groups 1-4
**Rationale:** After all implementation is complete, update existing tests for compatibility and add strategic integration tests.

- [x] 5.0 Complete test suite update and verification
  - [x] 5.1 Review existing test suite for compatibility issues
    - Review all 33 existing tests in test-jj.el
    - Identify tests that mock `shell-command-to-string` (need updating)
    - Identify tests that expect old return format (need updating)
    - Document breaking changes needed for each test file
    - Focus on test infrastructure, not implementation details
  - [x] 5.2 Update existing test mocking infrastructure
    - Update `jj-test-with-mocked-command` helper in test-helper.el
    - Change mocking from `shell-command-to-string` to `call-process`
    - Update mock to return structured result format
    - Ensure mock supports exit code and stderr simulation
    - Verify mock helper works with new command infrastructure
  - [x] 5.3 Update existing unit tests
    - Update tests in "jj--run-command" suite for new return format
    - Update tests in "jj-status" suite for repository validation
    - Update tests in "jj--bookmarks-get" suite for new format
    - Update tests in "jj--log-count-revs" suite for new format
    - Update tests in "jj--log-show" suite for repository validation
    - Update tests in "Error Handling Tests" suite for new infrastructure
    - Preserve data-driven test pattern (plist-based test cases)
    - Maintain existing test coverage metrics (80%+)
  - [x] 5.4 Write up to 8 additional integration tests maximum
    - Test end-to-end error flow: bad command -> error buffer -> user-error
    - Test repository validation across multiple functions
    - Test debug logging in realistic workflow
    - Test error categorization with realistic jj command failures
    - Test error buffer accumulates multiple errors
    - Test command success path with debug mode enabled
    - Limit to critical integration paths only
    - Skip edge cases, performance tests, stress tests
  - [x] 5.5 Run complete test suite
    - Run ALL tests (existing 33 + new ~20-30 from task groups + 8 integration)
    - Expected total: approximately 61-71 tests
    - Verify all tests pass
    - Verify no regressions in existing functionality
    - Generate coverage report (target: maintain 80%+ coverage)
  - [x] 5.6 Document test changes and coverage
    - Update test file header commentary with new test count
    - Document breaking changes in test infrastructure
    - Update coverage metrics in test-jj.el header
    - Add brief notes about new test suites added

**Acceptance Criteria:**
- All tests pass (approximately 61-71 tests total)
- Test coverage remains at 80%+ for critical functions
- No more than 8 additional integration tests added
- Existing test infrastructure updated for new command format
- Test documentation reflects current state
- No regressions in existing functionality

## Execution Order

Recommended implementation sequence:

1. **Task Group 1: Command Execution Infrastructure** (2-3 hours)
   - Foundation for all other work
   - Enables exit code capture and stderr separation
   - Must be complete before error handling can be implemented

2. **Task Group 2: Error Handling Functions** (1-2 hours)
   - Depends on command execution infrastructure
   - Enables proper error categorization and reporting
   - Must be complete before function migration

3. **Task Group 3: Debug Logging System** (1 hour)
   - Depends on command execution and error handling
   - Can be developed in parallel with Task Group 2
   - Provides visibility for troubleshooting

4. **Task Group 4: Update Existing Functions** (2-3 hours)
   - Depends on all infrastructure being in place
   - Largest scope: 10+ functions to update
   - Breaking changes acceptable (v0.0.1)

5. **Task Group 5: Test Suite Update and Verification** (1-2 hours)
   - Depends on all implementation being complete
   - Ensures no regressions
   - Validates entire feature works end-to-end

## Important Constraints

- **Base implementer assignments** on available implementers in implementers.yml
  - api-engineer: Core infrastructure, error handling, function migration
  - testing-engineer: Test coverage, test suite updates
  - No database-engineer or ui-designer needed (Emacs Lisp package)
- **Maintain data-driven test pattern** from existing test suite
  - Use plist-based test cases with `dolist`
  - Keep test fixtures in tests/fixtures/ directory
  - Preserve existing test helper functions
- **Breaking changes are acceptable** (v0.0.1 project)
  - Function signatures can change
  - Return values can change
  - No backward compatibility required
- **Use focused test-driven approach**
  - Each task group writes 3-8 tests maximum during development
  - Tests focus on critical behaviors only
  - Full test suite verification happens in Task Group 5
- **Follow Emacs Lisp conventions**
  - Use double-dash prefix for internal functions (jj--function)
  - Use defcustom for user-configurable variables
  - Use appropriate error signaling (message, user-error, error)
  - Maintain lexical-binding: t in file headers
- **Preserve existing patterns**
  - Keep `jj--get-project-folder` for repository detection
  - Maintain transient popup command structure
  - Preserve buffer naming conventions (e.g., "jj: project-name")
  - Keep command construction format (--no-pager --color never)

## Technical Notes

### Command Execution Changes

**Current Implementation (lines 48-54 in jj.el):**
```elisp
(defun jj--run-command (command)
  "Run a jj COMMAND from the project root"
  (let* ((default-directory (jj--get-project-folder))
         (jj-cmds (list "jj" "--no-pager" "--color" "never" command))
         (cmd-string (s-join " " jj-cmds)))
    (message cmd-string)
    (shell-command-to-string cmd-string)))
```

**New Implementation Pattern:**
```elisp
(defun jj--run-command (command)
  "Run a jj COMMAND from the project root.
Returns (success-flag stdout stderr exit-code)."
  (let* ((default-directory (jj--get-project-folder))
         (stdout-buffer (generate-new-buffer " *jj-stdout*"))
         (stderr-buffer (generate-new-buffer " *jj-stderr*"))
         (exit-code (call-process "jj" nil (list stdout-buffer stderr-buffer) nil
                                 "--no-pager" "--color" "never" command))
         (stdout (with-current-buffer stdout-buffer (buffer-string)))
         (stderr (with-current-buffer stderr-buffer (buffer-string))))
    (jj--debug-log "Command: jj %s (exit: %d)" command exit-code)
    (when (not (string-empty-p stderr))
      (jj--debug-log "Stderr: %s" stderr))
    (kill-buffer stdout-buffer)
    (kill-buffer stderr-buffer)
    (list (zerop exit-code) stdout stderr exit-code)))
```

### Error Categorization Logic

**User Errors (user-error):**
- Not in a jj repository (.jj folder missing)
- Invalid revset syntax
- Invalid command arguments
- Exit codes 1-2 with "invalid" or "not found" in stderr

**Command Failures (error with context):**
- Command exits with non-zero status
- Git remote errors
- Conflict states
- Exit codes 1-255 (excluding user errors)

**System Errors (error):**
- jj binary not found (call-process returns nil)
- File system permission errors
- Unrecoverable system-level failures

### Debug Logging Format

```
[jj-debug] Command: jj status
[jj-debug] Exit code: 0
[jj-debug] Stderr: <empty>
```

### Error Buffer Format

```
===============================================================================
Error at: 2025-10-17 14:32:45
===============================================================================

Command: jj status
Exit Code: 128
Error Type: command-failure

--- Stderr ---
fatal: not a jj repository

--- Stdout ---
<empty>

===============================================================================
```

## Standards Compliance

This tasks list is aligned with:
- **Global coding style**: Small focused functions, meaningful names, DRY principle
- **Global conventions**: Clear documentation, version control best practices
- **Global error handling**: User-friendly messages, fail fast, specific error types
- **Testing standards**: Minimal tests during development, focus on core flows, defer edge cases

## Related Files

- Implementation file: /home/mathematician314/data/personal/jj.el/jj.el
- Test file: /home/mathematician314/data/personal/jj.el/tests/test-jj.el
- Test helper: /home/mathematician314/data/personal/jj.el/tests/test-helper.el
- Spec document: /home/mathematician314/data/personal/jj.el/agent-os/specs/2025-10-17-error-handling-standardization/spec.md
- Requirements: /home/mathematician314/data/personal/jj.el/agent-os/specs/2025-10-17-error-handling-standardization/planning/requirements.md
