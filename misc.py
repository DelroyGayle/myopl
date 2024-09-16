"""
misc.py
In order to refactor basic.py
This module will contain miscellaneous code needed by basic.py
"""


def string_with_arrows(text, pos_start, pos_end):
    result = ''

    # Calculate indices - start of by searching from the right
    index_start = max(text.rfind('\n', 0, pos_start.theindex), 0)
    index_end = text.find('\n', index_start + 1)
    if index_end < 0:
        index_end = len(text)

    # Generate each line
    line_count = pos_end.linenum - pos_start.linenum + 1
    for i in range(line_count):
        # Calculate line columns
        line = text[index_start:index_end]
        col_start = pos_start.column if i == 0 else 0
        col_end = pos_end.column if i == line_count - 1 else len(line) - 1

        # Append to result
        result += line + '\n'
        result += ' ' * col_start + '^' * (col_end - col_start)

        # Re-calculate indices
        index_start = index_end
        index_end = text.find('\n', index_start + 1)
        if index_end < 0:
            index_end = len(text)

    return result.replace('\t', '')
