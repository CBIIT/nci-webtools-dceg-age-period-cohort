from html.parser import HTMLParser
from pathlib import Path
import unittest


CROSSTALK_DIR = Path(__file__).resolve().parents[1]
EXPECTED_SCRIPT_INTEGRITY = {
    "index.html": {
        "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.2.4/jquery.js":
            "sha384-TlQc6091kl7Au04dPgLW7WK3iey+qO8dAi/LdwxaGBbszLxnizZ4xjPyNrEf+aQt",
        "https://cdnjs.cloudflare.com/ajax/libs/babel-polyfill/6.13.0/polyfill.min.js":
            "sha384-vqdIZ/QLRwPCEuhZYvYR9a3IQPsfnOMePPc6ODEOeIZTqsZh3ox3KIp1osNKjcAw",
        "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/js/bootstrap.js":
            "sha384-OkuKCCwNNAv3fnqHH7lwPY3m5kkvCIUnsHbjdU7sN022wAYaQUfXkqyIZLlL0xQ/",
        "https://cdnjs.cloudflare.com/ajax/libs/datatables/1.10.12/js/jquery.dataTables.min.js":
            "sha384-89aj/hOsfOyfD0Ll+7f2dobA15hDyiNb8m1dJ+rJuqgrGR+PVqNU8pybx4pbF3Cc",
        "https://cdnjs.cloudflare.com/ajax/libs/d3/4.2.2/d3.min.js":
            "sha384-fzAgVEj4uxSOqciY/YFRkurGhwjQK9jW65CVu8lPRZvv6yzQ9XCmGpmwdVLtIf4k",
        "https://cdnjs.cloudflare.com/ajax/libs/dompurify/0.8.2/purify.min.js":
            "sha384-EjLJRXVo2obWHJEDs/ygDlB4clkSJYVwRch1mnhCiKA8lJb19T9D/aJuWhxGZfTi",
        "https://cdnjs.cloudflare.com/ajax/libs/excel-builder/2.0.2/excel-builder.compiled.js":
            "sha384-aBX4SjPd42+VCPqb1S/+7eB/TXSDoIU891lR0sF+xWOI6AHNa28tS2JvPsG/zLFw",
        "https://cdnjs.cloudflare.com/ajax/libs/sprintf/1.0.3/sprintf.min.js":
            "sha384-qCEjHcUl7HOFnyRWL6HhE2udgBvicaWchvyQD9i40Bx3bBPclswqb7eKg/4fG2DH",
    },
    "tabledisplay.html": {
        "https://cdnjs.cloudflare.com/ajax/libs/dompurify/0.7.3/purify.min.js":
            "sha384-c/x4jMRblQaXi4vPR73CSNiiFC4rzVkwAAgunmFm95vz90xyfNV5dzEhYu4Y8xyI",
    },
}


class ScriptParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.scripts = {}

    def handle_starttag(self, tag, attrs):
        if tag != "script":
            return
        attributes = dict(attrs)
        if "src" in attributes:
            self.scripts[attributes["src"]] = attributes


class SubresourceIntegrityTests(unittest.TestCase):
    def test_ticket_scripts_have_expected_integrity_and_cors(self):
        for filename, expected_scripts in EXPECTED_SCRIPT_INTEGRITY.items():
            with self.subTest(filename=filename):
                parser = ScriptParser()
                parser.feed((CROSSTALK_DIR / filename).read_text(encoding="utf-8"))

                for src, expected_integrity in expected_scripts.items():
                    with self.subTest(filename=filename, src=src):
                        self.assertIn(src, parser.scripts)
                        self.assertEqual(
                            parser.scripts[src].get("integrity"),
                            expected_integrity,
                        )
                        self.assertEqual(
                            parser.scripts[src].get("crossorigin"),
                            "anonymous",
                        )


if __name__ == "__main__":
    unittest.main()
