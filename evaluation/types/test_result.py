"""Contains definition of a test results."""

class TestResult:
    """Definition of a test result."""

    def __init__(self, name, description, passed, fail_reason=None):
        """
        Create a new test result.

        :type name: str
        :type description: str
        :type passed: bool
        :type fail_reason: str
        """
        self.name = name
        self.description = description
        self.passed = passed
        self.passed_binary = int(self.passed)
        self.fail_reason = fail_reason
