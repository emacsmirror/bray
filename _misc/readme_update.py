#!/usr/bin/env python3
# GPL License, Version 3.0 or later

import os
import re
import subprocess
import sys

BASE_DIR = os.path.join(os.path.dirname(__file__), "..")

ELISP_NAME = "bray.el"
EMACS_NAME = "emacs"


def patch_help_test(emacs_output: str) -> str:

    # Replace unicode quotes to double back-ticks.
    def key_replace_quote_to_rst(m: re.Match[str]) -> str:
        return "``{:s}``".format(m.group(2))

    emacs_output = re.sub(
        "(\u2018)([\\w\\-*]+)(\u2019)",
        key_replace_quote_to_rst, emacs_output,
    )

    # Add double back-ticks to lisp keywords,
    # because they are used to represent "code".
    def key_replace_property(m: re.Match[str]) -> str:
        return "{:s}``{:s}``".format(m.group(1), m.group(2))

    emacs_output = re.sub(
        "([\\s\\(\\)])(:[\\w\\-*]+)\\b",
        key_replace_property, emacs_output,
    )

    # Replace lisp convention for single quote escaping
    # with a single quote.
    emacs_output = emacs_output.replace("\\='", "'")

    return emacs_output


def main() -> int:

    cmd = [
        EMACS_NAME,
        "--batch",
        "--load",
        os.path.join(BASE_DIR, ELISP_NAME),
        "--load",
        os.path.join(BASE_DIR, "_misc", "readme_update.el"),

        "--eval", (
            """(readme_update-printf \""""
            """\nCustom Variables"""
            """\n----------------\n\n")"""
        ),
        "--eval", (
            """(readme_update """
            """"^bray-[a-z]" 'var-custom """
            """(list 'bray-mode-hook))"""
        ),

        "--eval", (
            """(readme_update-printf \""""
            """\nOther Variables"""
            """\n---------------\n\n")"""
        ),
        "--eval", (
            """(readme_update """
            """"^bray-[a-z]" 'var """
            """(list 'bray-mode-off-hook """
            """      'bray-mode-on-hook """
            """      'bray-mode-hook """
            """      'bray-mode-map))"""
        ),

        "--eval", (
            """(readme_update-printf \""""
            """\nFunctions"""
            """\n---------\n\n")"""
        ),
        "--eval", (
            """(readme_update """
            """"^bray-[a-z]" 'fun """
            """(list 'bray-mode))"""
        ),
    ]

    p = subprocess.run(
        cmd,
        stdout=subprocess.PIPE,
    )

    emacs_output = (
        p.stdout.decode('utf-8').rstrip() +
        '\n\n'
    )
    del p

    # strip trailing space
    emacs_output = re.sub(r'[ \t]+(\n|\Z)', r'\1', emacs_output)
    emacs_output = patch_help_test(emacs_output)

    # Try write reStructuredText directly!
    with open('readme.rst', 'r', encoding='utf-8') as f:
        data = f.read()

    help_begin_text = '.. BEGIN VARIABLES'
    help_end_text = '.. END VARIABLES'
    help_begin_index = data.find(help_begin_text)
    help_end_index = data.find(help_end_text, help_begin_index)

    if help_begin_index == -1:
        print('Error: {!r} not found'.format(help_begin_text))
        return 1
    if help_end_index == -1:
        print('Error: {!r} not found'.format(help_end_text))
        return 1

    help_begin_index += len(help_begin_text) + 1

    data_update = data[:help_begin_index] + emacs_output + data[help_end_index:]

    with open('readme.rst', 'w', encoding='utf-8') as f:
        f.write(data_update)
    return 0


if __name__ == "__main__":
    sys.exit(main())
