"""Contains definition of response to user."""

class Response:
    """Definition of response to user."""

    def __init__(self, message, valid):
        """
        Create a response.

        :type message: str
        :type valid: bool
        """
        self.message = message
        self.valid = valid
