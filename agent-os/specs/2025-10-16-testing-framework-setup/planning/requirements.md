# Spec Requirements: Testing Framework Setup

## Initial Description
Establishing a Buttercup-based test suite with fixtures, mocks for jj CLI interactions, and test runners integrated into Eask.

## Requirements Discussion

### First Round Questions

**Q1:** I assume tests should be organized in a `tests/` directory at the project root, mirroring the source code structure. Is that correct, or would you prefer a different organization?
**Answer:** OK - use the proposed structure with tests mirroring source code

**Q2:** For mocking jj command outputs, I'm thinking we should create a test helper that can stub `call-process` or similar functions. Should we use inline mocking (cl-letf) or a separate mocking library like el-mock?
**Answer:** OK - mock jj command outputs, inline mocking is fine for now

**Q3:** For test runner integration with Eask, should we add a specific Easkfile command like `eask test` or `eask buttercup`? Are there any environment variables or test-specific configurations we need to set?
**Answer:** No additional variables for now - just basic test command

**Q4:** I'm assuming we should focus initial test coverage on core functions like git log parsing, commit formatting, and basic jj.el command flows. Should we also include integration tests, or stick to unit tests for now?
**Answer:** OK - focus on core functions and basic flows as proposed

**Q5:** Do you need fixture files for test data (e.g., sample jj log output), or should we generate test data programmatically in the test setup?
**Answer:** Fixtures should be minimal for now

**Q6:** Should tests run in isolation (each test in a clean environment), or are there shared setups we should maintain across test suites? For example, do we need to initialize a test jj repository?
**Answer:** Tests should run in isolation, use temp directories, not require jj installed, mock all external commands

**Q7:** For error handling tests, should we verify specific error messages, or just that errors are raised appropriately?
**Answer:** Error handling should NOT be tested yet - just infrastructure

**Q8:** Should the CI/CD pipeline automatically run tests on every commit/PR? If so, are there specific test commands or coverage thresholds we need to configure?
**Answer:** Pipeline running in separate task - don't modify CI workflow

**Q9:** For testing functions that interact with repository state, should we build an abstraction for creating test repository scenarios, or mock the repository state directly?
**Answer:** Build the abstraction for test data/repository tests

**Q10:** What should we NOT include in this initial testing setup? (e.g., coverage reporting, performance tests, snapshot testing, etc.)
**Answer:** Ignore all points mentioned (coverage reporting, performance tests, snapshot testing, property-based testing, visual regression)

### Existing Code to Reference

**Similar Features Identified:**
No tests exist yet, set them up based on common Elisp practice

### Follow-up Questions
None required - all necessary information gathered.

## Visual Assets

### Files Provided:
No visual assets provided.

### Visual Insights:
Not applicable - no visual files found.

## Requirements Summary

### Functional Requirements
- Set up Buttercup testing framework integrated with Eask
- Create `tests/` directory structure mirroring source code organization
- Implement test helpers for mocking jj CLI command outputs using inline mocking (cl-letf)
- Add basic Eask test command (no additional environment variables)
- Focus test coverage on core functions: git log parsing, commit formatting, basic jj.el command flows
- Tests must run in isolation using temporary directories
- All external commands (especially jj) must be mocked - no jj installation required
- Create abstraction for test repository scenarios and test data
- Minimal fixture files for now
- Infrastructure setup only - error handling tests deferred

### Reusability Opportunities
- Follow common Emacs Lisp testing practices and conventions
- Use standard Buttercup patterns for test organization
- Leverage Eask's built-in test runner capabilities

### Scope Boundaries

**In Scope:**
- Buttercup framework setup and configuration
- Test directory structure creation
- Basic Eask test command integration
- Test helper utilities for mocking jj CLI interactions
- Isolation mechanisms (temp directories, mocked external commands)
- Abstraction layer for test data and repository state scenarios
- Initial test infrastructure and scaffolding
- Unit tests for core parsing and formatting functions

**Out of Scope:**
- Error handling test patterns (deferred to future work)
- CI/CD workflow modifications (separate task)
- Test coverage reporting tools
- Performance testing
- Snapshot testing
- Property-based testing
- Visual regression testing
- Integration tests (focus on unit tests initially)
- Comprehensive fixture library (minimal fixtures only)
- Additional test environment variables or complex configurations

### Technical Considerations
- Use Buttercup as the testing framework
- Integrate with Eask build/task system
- Tests must not depend on jj being installed on the system
- Use `cl-letf` for inline function mocking
- Create temporary directories for test isolation
- Mock all `call-process`, `shell-command`, and similar external command invocations
- Follow Emacs Lisp naming conventions for test files (test-*.el or *-test.el)
- Ensure tests can run both locally and in CI environments (though CI setup is separate task)
