"""Contains definition of an evaluation results."""

class EvaluationResult:
    """Definition of an evaluation result."""

    def __init__(self, name, test_results):
        """
        Create a new evaluation result.

        :type name: str
        :type test_results: list of evaluation.types.test_result.TestResult
        """
        self.name = name
        self.test_results = test_results
        self.total = len(test_results)
        self.passed = list(filter(self._passed, test_results))
        self.failed = list(filter(self._failed, test_results))
        self.number_of_passed = len(self.passed)
        self.number_of_failed = len(self.failed)
        if self.total == 0:
            self.accuracy = 0.0
        else:
            self.accuracy = self.number_of_passed / self.total

    def _failed(self, test_result):
        """
        Return true when test failed.

        :type test_result: evaluation.types.test_result.TestResult
        """
        return not test_result.passed

    def _passed(self, test_result):
        """
        Return true when test passed.

        :type test_result: evaluation.types.test_result.TestResult
        """
        return test_result.passed
