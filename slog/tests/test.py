# Base class for tests

import sys

from yaspin import yaspin

from slog.common.client import SlogClient

class DynText:
    def __init__(self, base_text):
        self.text = base_text
        self.extra = None

    def __str__(self):
        # if self.extra:
        #     # TODO: why dis dont works
        #     # return f'{self.text} - {self.extra}'
        #     return self.text
        # else:
        #     return self.text
        return self.text


class Test:
    """
    Base Test class
    """
    def __init__(self, server, txt):
        self.test_text = txt
        self.spin_text = DynText(self.test_text)
        self.client = SlogClient()

    def success(self):
        """
        On test success call this
        """
        print('\033[32m ✔ Success \033[0m')
        sys.exit(0)

    def fail(self, msg=""):
        """
        On test failure call this
        """
        if msg != "":
            msg = ": " + msg
        print('\033[31;1m 💥 Failure{} \033[0m'.format(msg))
        sys.exit(1)

    def run_test(self, writer):
        """
        Override this
        """
        return True

    def test(self):
        """
        Starts the test
        """
        with yaspin(text=self.spin_text) as spinner:
            if self.run_test(spinner):
                spinner.ok("✔")
            else:
                spinner.fail("💥")
