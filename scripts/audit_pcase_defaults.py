#!/usr/bin/env python3
"""Audit cljel pcase forms; optionally rewrite only bare-wildcard forms.

This lightweight source scanner respects quoted strings and line comments. Its
rewrite preserves every clause body, comment and unrelated pcase byte for byte.
"""
from pathlib import Path
import argparse


def scan(source):
    stack, forms = [], []
    string = escape = comment = False
    for i, char in enumerate(source):
        if comment:
            if char == '\n':
                comment = False
            continue
        if string:
            if escape:
                escape = False
            elif char == '\\':
                escape = True
            elif char == '"':
                string = False
            continue
        if char == ';':
            comment = True
        elif char == '"':
            string = True
        elif char in '([{':
            stack.append([char, i, []])
        elif char in ')]}':
            if not stack or '([{'[')]}'.index(char)] != stack[-1][0]:
                raise ValueError(f'unbalanced delimiter at {i}')
            _, start, children = stack.pop()
            node = (start, i + 1, children)
            forms.append(node)
            if stack:
                stack[-1][2].append(node)
    if stack or string:
        raise ValueError('unclosed delimiter or string')
    return forms


def skip(source, i, end):
    while i < end:
        if source[i].isspace() or source[i] == ',':
            i += 1
        elif source[i] == ';':
            newline = source.find('\n', i)
            i = end if newline < 0 else newline + 1
        else:
            break
    return i


def parts(source, node):
    start, end, children = node
    i = skip(source, start + 1, end - 1)
    result = []
    while i < end - 1:
        sub = next((child for child in children if child[0] == i), None)
        if sub:
            result.append((i, sub[1], sub))
            i = sub[1]
        else:
            j = i + (source[i] == "'")
            if j < end - 1 and source[j] == '"':
                j += 1
                escaped = False
                while j < end - 1:
                    if escaped:
                        escaped = False
                    elif source[j] == '\\':
                        escaped = True
                    elif source[j] == '"':
                        j += 1
                        break
                    j += 1
            else:
                while j < end - 1 and not source[j].isspace() and source[j] not in '()[]{};,':
                    j += 1
            if j == i:
                raise ValueError(f'unexpected token at {i}')
            result.append((i, j, None))
            i = j
        i = skip(source, i, end - 1)
    return result


def text(source, part):
    return source[part[0]:part[1]]


def pcase_forms(source):
    for node in scan(source):
        if source[node[0]] != '(':
            continue
        elements = parts(source, node)
        if elements and text(source, elements[0]) == 'pcase':
            clauses = elements[2:]
            heads = [text(source, parts(source, clause[2])[0]) for clause in clauses]
            yield node, elements, heads


def rewrite(source):
    edits = []
    converted = []
    for node, elements, heads in pcase_forms(source):
        if not heads or heads[-1] != '_':
            continue
        if '_' in heads[:-1]:
            raise ValueError('non-final wildcard')
        line = source.count('\n', 0, node[0]) + 1
        converted.append(line)
        # One binding per pcase: dispatch expressions must be evaluated only once.
        name = f'pcase-dispatch-value-{line}'
        expr = text(source, elements[1])
        indent = source[source.rfind('\n', 0, node[0]) + 1:node[0]]
        if indent.strip():
            indent = ' ' * (node[0] - source.rfind('\n', 0, node[0]) - 1)
        edits.append((node[0], elements[1][1], f'(let [{name} {expr}]\n{indent}(elisp-cond'))
        for clause, head in zip(elements[2:], heads):
            pattern = parts(source, clause[2])[0]
            if head == '_':
                replacement = 't'
            elif head.startswith('(or ') and pattern[2]:
                choices = parts(source, pattern[2])[1:]
                replacement = '(or ' + ' '.join(f'(equal {name} {text(source, choice)})' for choice in choices) + ')'
            elif head.startswith("'") or head.startswith('"') or head.lstrip('-').isdigit():
                replacement = f'(equal {name} {head})'
            else:
                raise ValueError(f'unsupported pcase pattern at line {line}: {head}')
            edits.append((pattern[0], pattern[1], replacement))
        edits.append((node[1], node[1], ')'))
    for start, end, value in sorted(edits, reverse=True):
        source = source[:start] + value + source[end:]
    return source, converted


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('root', type=Path)
    parser.add_argument('--rewrite', action='store_true')
    args = parser.parse_args()
    total = 0
    for path in sorted(args.root.glob('src/cljel/**/*.cljel')):
        before = path.read_text()
        after, lines = rewrite(before)
        if lines:
            total += len(lines)
            print(f'{path.relative_to(args.root)}: {lines}')
            if args.rewrite:
                path.write_text(after)
                # Scan the edited file too; guard against malformed output.
                if any('_' in heads for _, _, heads in pcase_forms(after)):
                    raise ValueError(f'wildcard remains in {path}')
    print(f'{total} bare-wildcard defaults {"converted" if args.rewrite else "found"}')


if __name__ == '__main__':
    main()
