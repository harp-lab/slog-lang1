# Base class for tests

import sys
from typing import Tuple

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
        self.results: list[Tuple[str, str]] = []
        self.spin_text = DynText(self.test_text)
        self.client = SlogClient()

    def success(self, msg):
        """
        On test success call this
        """
        print('\033[32m ✔ Success \033[0m')
        self.results.append(("Success", msg))
        # sys.exit(0)

    def fail(self, msg=""):
        """
        On test failure call this
        """
        if msg != "":
            msg = ": " + msg
        print('\033[31;1m 💥 Failure{} \033[0m'.format(msg))
        self.results.append(("Failure", msg))
        # sys.exit(1)

    def run_test(self, writer):
        """
        Override this
        """
        return True

    def test(self):
        """
        Starts the test
        """

        yp = yaspin(text=self.spin_text)
        if not sys.stdout.isatty(): yp.hide()
        with yp as spinner:
            if self.run_test(spinner):
                spinner.ok("✔")
            else:
                spinner.fail("💥")
        print("raw results:")
        for res in self.results:
            print(res)
        failures = [msg for (res, msg) in self.results if res == "Failure"]
        successes = [msg for (res, msg) in self.results if res == "Success"]
        end_punc = "." if len(failures) == 0 else ":"
        print(f"{len(failures)} errors{end_punc}")
        for msg in failures:
            print(msg)
        sys.exit(len(failures))