#!/usr/bin/env python3
from difflib import Differ
import argparse
import subprocess


#with open("docs/fourthstyle", 'r') as file1:
#    data1 = file1.read()


with open("um_tempcolor", 'r') as file1:
    data1 = file1.read()


#result = subprocess.run([
#    'git', 
#    'log',
#    '--graph',
#    #'--pretty="format:\\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset"',
#    #'--pretty=format:\%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset',
#    '--pretty=format:%h -%d %s (%cr) <%an>',
#    '--abbrev-commit',
#    '--color'
#    ],
#    stdout=subprocess.PIPE)
#data1 = result.stdout.decode("utf-8")


#prefix = ">>>>>>> "
#branch = raw_conflict_line[len(prefix):]
#if 'HEAD' in branch:
#print(branches)
#head_branch = next(line for line in branches if line.startswith("* "))
#head_branch = head_branch[2:]
#branch = head_branch
#return branch
#

#    start = line.find("\x1B[")
#    end = line.find("\x1B[m", start)

def parse_line(line):
    escapes = []
    #escapes_for_char = []
    escapes_so_far = ""
    clean_line = []

    index = 0
    clean_index = 0
    while index < len(line):
        if line[index] == "\x1b":
            start = line.find("\x1b[", index)
            if start != 1:
                end = line.find("m", start)
                if (end - start) <= 2:
                    #print(line[start:end + 1])
                    escapes[len(escapes)-1][1] = line[start:end + 1]
                else:
                    escapes_so_far += line[start:end + 1]
                index = end
        else:
            escapes.append([escapes_so_far, ""])
            escapes_so_far = ""
            clean_line.append(line[index])
            clean_index += 1
        index = index + 1
    return (escapes, "".join(clean_line))

#from shutil import get_terminal_size
#print(get_terminal_size())

import re

# not sure if rgx is the fastest way
first_non_graph_rgx = re.compile(r'[^*|\\/ _]')

import copy

style = []
second = []
for raw_line in data1.splitlines():
    #print(parse_line(line))
    #print(parse_line(line))
    (escapes, line) = parse_line(raw_line)
    match = first_non_graph_rgx.search(line)
    graph = line

    last_graph_idx = None
    if match:
        last_graph_idx = match.start(0)
        graph = line[:last_graph_idx]

    if '/' in graph or '\\' in graph:
        extended = graph.replace("*", "|")

        extended = extended.replace("_", " ")
        #style.append(escapes[:len(extended)])
        #style.append(escapes.copy())
        style.append(copy.deepcopy(escapes))
        second.append(extended)

    #print("=================================")
    #print(raw_line.replace("\x1B", "\\x1B") + "=====")
    #print("=".join([ str(escape) for escape in escapes]).replace("\x1B", "\\x1B") + "=======")
    #print(line + "==")

    #colorlin = ""
    #for idx, column in enumerate(line):
    #    colorlin += escapes[idx][0].replace("\x1B", "\\x1B") + column + escapes[idx][1].replace("\x1B", "\\x1B")
    #print(colorlin)

    #colorlin = ""
    #for idx, column in enumerate(line):
    #    colorlin += escapes[idx][0] + column + escapes[idx][1]
    ##if len(escapes) > len(line):
    ##    colorlin += escapes[len(line)]

    #print(colorlin)
    #print("=================================")

    second.append(line.replace("|_", "|─"))
    style.append(escapes)

#final = second
#for line_number, columns in enumerate(final):
#    line = ""
#    for idx, column in enumerate(columns):
#        #print(idx + column)
#        #print(idx + column)
#        #line += style[line_number][idx] + column
#        #line += column
#        line += style[line_number][idx][0] + column + style[line_number][idx][1]
#    #not_empty_line = len(line.replace("|", "").replace("*", "").replace(" ", "")) > 0
#    not_empty_line = True
#    if not_empty_line:
#        #line = line.replace('|', '│')
#        print(line)


previous = None
for lno, line in enumerate(second):
    if previous is not None:
        for idx, column in enumerate(line):
            if column == '/' or column == '\\':
                up_column = previous[idx] if idx < len(previous) else None
                if up_column and up_column == column:
                    if column == '/':
                        top__char = '╭'
                        down_char = '╯'
                    if column == '\\':
                        top__char = '╮'
                        down_char = '╰'

                    second[lno-1] = second[lno-1][:idx] + top__char + second[lno-1][idx+1:]
                    second[lno] = second[lno][:idx] + down_char + second[lno][idx+1:]
    previous = line


#final = second
#for line_number, columns in enumerate(final):
#    line = ""
#    for idx, column in enumerate(columns):
#        #print(idx + column)
#        #print(idx + column)
#        #line += style[line_number][idx] + column
#        #line += column
#        line += style[line_number][idx][0] + column + style[line_number][idx][1]
#    #not_empty_line = len(line.replace("|", "").replace("*", "").replace(" ", "")) > 0
#    not_empty_line = True
#    if not_empty_line:
#        #line = line.replace('|', '│')
#        print(line)

previous = None
for lno, line in enumerate(second):
    if previous is not None:
        for idx, column in enumerate(line):
            up_column = previous[idx] if idx < len(previous) else None

            if up_column and up_column == '╰' and column == '╭':
                top__char = '|'
                down_char = '|'

                second[lno-1] = second[lno-1][:idx] + top__char + second[lno-1][idx+1:]
                second[lno] = second[lno][:idx] + down_char + second[lno][idx+1:]
    previous = line


for lno, line in enumerate(second):
    second[lno] = list(line)


def get_matrix_size(matrix):
    return len(matrix), len(matrix[0])


def get_sub_matrix_idx(target, start_pos, size):
    start_x, start_y = start_pos
    height, width = size
    end_y = start_y + width

    target_height = len(target)

    if start_x + height > target_height:
        return None

    for offset_x in range(height):
        x = start_x + offset_x

        if end_y > len(target[x]):
            return None

    return (start_pos, size)


def get_sub_matrix(target, start_pos, size):
    start_x, start_y = start_pos
    height, width = size
    end_y = start_y + width

    target_height = len(target)

    if start_x + height > target_height:
        return None

    window = []
    #window = [None]*height

    for offset_x in range(height):
        x = start_x + offset_x

        if end_y > len(target[x]):
            return None

        window.append(tuple(target[x][start_y:end_y]))
        #window[offset_x] = target[x][start_y:end_y]

    return tuple(window)


def equals_matrix(expected, target, start_pos):
    start_x, start_y = start_pos

    for i in range(len(expected)):
        for j in range(len(expected[i])):
            if target[start_x + i][start_y + j] != expected[i][j]:
                return False

    return True


def replace_matrix(replacement, target, start_pos):
    start_x, start_y = start_pos
    #print(start_x, start_y)

    for i in range(len(replacement)):
        for j in range(len(replacement[i])):
            target[start_x + i][start_y + j] = replacement[i][j]

    return True


def print_matrix(matrix):
    for i in range(len(matrix)):
        print(matrix[i])


def replace(target, expected, replacement, max_column=None, paint=None):
    expected = tuple([tuple(line) for line in expected])
    expected_size = get_matrix_size(expected)
    for lidx in range(len(target)):
        for ridx in range(len(target[lidx])):
            if max_column is not None and ridx > max_column:
                continue
            start_pos = (lidx, ridx)
            window = get_sub_matrix(target, start_pos, expected_size)
            # window = get_sub_matrix_idx(target, start_pos, expected_size)
            if window is not None:
                # print_matrix(window)
                # print()
                if expected == window:
                # if equals_matrix(expected, target, start_pos):
                    replace_matrix(replacement, target, start_pos)

                    if paint is None:
                        paint = []

                    for source, dest in paint:
                        source_line = lidx + source[0]
                        source_column = ridx + source[1]
                        dest_line = lidx + dest[0]
                        dest_column = ridx + dest[1]
                        style[dest_line][dest_column] = style[source_line][source_column].copy()


def replace_list(target, substitutions, max_column=None):
    expecteds_set = set(substitutions.keys())
    expected_size = get_matrix_size(list(substitutions.values())[0][0])
    #print("expected_size")
    #print(expected_size)
    #print(substitutions)
    for lidx in range(len(target)):
        for ridx in range(len(target[lidx])):
            if max_column is not None and ridx > max_column:
                continue
            start_pos = (lidx, ridx)
            window = get_sub_matrix(target, start_pos, expected_size)
            #if expected_size[1] > 3:
            #    print_matrix(window)
            #    print()
            while window is not None and window in expecteds_set:
                # window = get_sub_matrix_idx(target, start_pos, expected_size)
                if window is not None:
                    if window in expecteds_set:  # expected == window:
                    # if equals_matrix(expected, target, start_pos):
                        replacement = substitutions[window][0]
                        #print_matrix(replacement)
                        replace_matrix(replacement, target, start_pos)


                        paint = substitutions[window][1]
                        if paint is None:
                            paint = []

                        for source, dest in paint:
                            source_line = lidx + source[0]
                            source_column = ridx + source[1]
                            dest_line = lidx + dest[0]
                            dest_column = ridx + dest[1]
                            style[dest_line][dest_column] = style[source_line][source_column].copy()

                    window = get_sub_matrix(target, start_pos, expected_size)


def old_replace(target, expected, replacement, max_column=None, paint=None):
    for lidx, line in enumerate(target):
        for ridx, column in enumerate(line):
            if max_column is not None and ridx > max_column:
                continue
            top_left = column
            top_right = line[ridx + 1] if ridx + 1 < len(line) else None

            bottom_left = None
            bottom_right = None
            if lidx + 1 < len(target):
                if ridx < len(target[lidx+1]):
                    bottom_left = target[lidx + 1][ridx]

                if ridx + 1 < len(target[lidx+1]):
                    bottom_right = target[lidx + 1][ridx + 1]

                    found = [
                        [top_left, top_right],
                        [bottom_left, bottom_right]
                    ]

                    if expected == found:
                        target[lidx][ridx] = replacement[0][0]
                        target[lidx][ridx+1] = replacement[0][1]
                        target[lidx+1][ridx] = replacement[1][0]
                        target[lidx+1][ridx+1] = replacement[1][1]

                        if paint is None:
                            paint = []

                        for source, dest in paint:
                            source_line = lidx + source[0]
                            source_column = ridx + source[1]
                            dest_line = lidx + dest[0]
                            dest_column = ridx + dest[1]
                            style[dest_line][dest_column] = style[source_line][source_column].copy()


def replace_2(target, expected, replacement, paint=None):
    for lidx, line in enumerate(target):
        for ridx, column in enumerate(line):
            top_left = column
            top_right = None
            top_middle = None
            bottom_left = None
            bottom_middle = None
            bottom_right = None
            if lidx + 1 < len(target):
                if ridx + 2 < len(line):
                    top_middle = line[ridx + 1]
                    top_right = line[ridx + 2]

                if ridx < len(target[lidx+1]):
                    bottom_left = target[lidx + 1][ridx]

                if ridx + 2 < len(target[lidx+1]):
                    bottom_middle = target[lidx + 1][ridx + 1]
                    bottom_right = target[lidx + 1][ridx + 2]

                    found = [
                        [top_left, top_middle, top_right],
                        [bottom_left, bottom_middle, bottom_right]
                    ]

                    if expected == found:
                        target[lidx][ridx] = replacement[0][0]
                        target[lidx][ridx+1] = replacement[0][1]
                        target[lidx][ridx+2] = replacement[0][2]
                        target[lidx+1][ridx] = replacement[1][0]
                        target[lidx+1][ridx+1] = replacement[1][1]
                        target[lidx+1][ridx+2] = replacement[1][2]

                        if paint is None:
                            paint = []

                        for source, dest in paint:
                            source_line = lidx + source[0]
                            source_column = ridx + source[1]
                            dest_line = lidx + dest[0]
                            dest_column = ridx + dest[1]
                            style[dest_line][dest_column] = style[source_line][source_column].copy()


def get_paint(*args):
    if len(args) == 0:
        return None
    else:
        p = []
        for i, line in enumerate(args):
            for j, _ in enumerate(line):
                if args[i][j] == 'S':
                    p.append((i, j))

        for i, line in enumerate(args):
            for j, _ in enumerate(line):
                if args[i][j] == 'D':
                    p.append((i, j))

        return [tuple(p)]


class GitLines():
    def __init__(self, lines):
        self.lines = lines
        self._paint = None
        self.substitutions = {}

    def replace(self, *args):
        self.needle = args
        return self

    def by(self, *args):
        replacement = tuple([tuple(list(line)) for line in args])
        expected = tuple([tuple(list(line)) for line in self.needle])

        self.substitutions[expected] = (replacement, self._paint)

    def run(self):
        replace_list(self.lines, self.substitutions)
        self._paint = None
        self.needle = None
        self.substitutions = {}

    def paint(self, *args):
        if len(args) == 0:
            self._paint = None
        else:
            p = []
            for i, line in enumerate(args):
                for j, _ in enumerate(line):
                    if args[i][j] == 'S':
                        p.append((i, j))

            for i, line in enumerate(args):
                for j, _ in enumerate(line):
                    if args[i][j] == 'D':
                        p.append((i, j))

            self._paint = [tuple(p)]



lines = GitLines(second)


lines.paint('S ',
            'D ')
lines.replace('| ',
              ' ╮').by('| ',
                       '╰╮')
#                       'L╮')

lines.paint('SD',
            '  ')
lines.replace('╰ ',
              ' |').by('╰╮',
                       ' |')

lines.paint('SD',
            '  ')
lines.replace('╰ ',
              ' *').by('╰╮',
                       ' *')

lines.paint('  ',
            'SD')

lines.replace(' *',
              '╭ ').by(' *',
                       '╭╯')

lines.paint('DS',
            '  ')

lines.replace(' ╯',
              '* ').by('╭╯',
                       '* ')

lines.paint('  ',
            'SD')

lines.replace(' |',
              '╭ ').by(' |',
                       '╭╯')

lines.paint('DS')
lines.replace(' ╯',
              '| ').by('╭╯',
                       '| ')

lines.paint(' D',
            ' S')
lines.replace('* ',
              '|╮').by('*╮',
                       '||')

lines.paint()
lines.replace('|╯',
              '* ').by('├╯',
                       '*')

#????
#lines.paint('  ',
#            'SD')
#lines.replace('|╯',
#              '╭|').by('||',
#                       '╭╯')

lines.paint('  ',
            'SD')
lines.replace(' ╯',
              '╭ ').by(' |',
                       '╭╯')

lines.paint('SD',
            '  ')
lines.replace('╰ ',
              ' ╮').by('╰╮',
                       ' |')

lines.run()



#============================

paint = get_paint('D S',
                  '   ')
#print(paint)
#expected = [
#    [' ', '|', '╯'],
#    ['╭', '|', ' ']
#]
#replacement = [
#    ['╭', '|', '╯'],
#    ['|', '|', ' ']
#]
#
#replace(second, expected, replacement, paint=paint)
#replace_2(second, expected, replacement, paint)

lines.paint('D S',
            '   ')
lines.replace(' |╯',
              '╭| ').by('╭|╯',
                        '|| ')

lines.paint('D S',
            '   ')
lines.replace(' |─',
              '╭| ').by('╭|─',
                        '|| ')

### new strategy

lines.paint()
lines.replace(' |╯',
              ' | ').by(' ├╯',
                        ' | ')

lines.paint()
lines.replace(' |╯',
              '╮| ').by(' ├╯',
                        '╮| ')


####paint = get_paint('D S',
####                  '   ')
####
#####print(paint)
####expected = [
####    [' ', '|', '─'],
####    ['╭', '|', ' ']
####]
####replacement = [
####    ['╭', '|', '─'],
####    ['|', '|', ' ']
####]
####
####replace_2(second, expected, replacement, paint)

#lines.paint()
#lines.replace(' |╭',
#              ' |╯').by(' |╭',
#                        ' ├╯')

#lines.paint()
#lines.replace(' |╭',
#              ' |╯').by(' |╭',
#                        ' ├╯')


lines.run()

####################expected = [
####################    [' ', '|', '|'],
####################    [' ', '|', '╯']
####################]
####################replacement = [
####################    [' ', '|', '|'],
####################    [' ', '├', '╯']
####################]
####################
####################replace_2(second, expected, replacement)
####################
####################
####################expected = [
####################    [' ', '|', '╭'],
####################    [' ', '|', '╯']
####################]
####################replacement = [
####################    [' ', '|', '╭'],
####################    [' ', '├', '╯']
####################]
####################
####################replace_2(second, expected, replacement)



# MUST IMPLEMENT MAX_COLUMN FIRST
#lines.replace('|╭',
#              '|╯').by('|╭',
#                       '├╯')


expected = [
    ['|', '╭'],
    ['|', '╯']
]
replacement = [
    ['|', '╭'],
    ['├', '╯']
]

replace(second, expected, replacement, max_column=0)

expected = [
    ['|', '|'],
    ['|', '╯']
]

replacement = [
    ['|', '|'],
    ['├', '╯']
]

replace(second, expected, replacement, max_column=0)




def compress_style(line_number, line):
    """
        Too many escape codes since to make the line break before end of
        line is reached.
        With this function we try to compress style be reusing style of
        consecutive chars
    """
    previous_idx = None
    for idx, column in enumerate(line):
        if previous_idx is not None:
            if (style[line_number][previous_idx][0] == style[line_number][idx][0] and
                    style[line_number][previous_idx][1] == style[line_number][idx][1]):
                style[line_number][previous_idx][1] = ""
                style[line_number][idx][0] = ""

        previous_idx = idx


def compress_escapes(line):
    #return re.sub(r'(\x1b[[^m]*m)\x1bm(\x1b[[^m]*m)',
    #              r'\1\2',
    #              line)
    final_line = line
    #final_line = re.sub('\x1b\\[m(\x1b\\[[^m]+m)',
    #                    r'\1',
    #                    final_line)
    #final_line = re.sub('\x1b\\[m(\x1b\\[[^m]+m)',
    #                    r'\1',
    #                    final_line)
    final_line = re.sub('\x1b\\[m *\x1b\\[m',
                        '\x1b\\[m',
                        final_line)
    final_line = re.sub('\x1b\\[m \\*\x1b\\[m',
                        '\x1b\\[m \\*',
                        final_line)

    return final_line


final = second
for line_number, columns in enumerate(final):
    compress_style(line_number, columns)
    line = ""
    unstyled_line = ""
    for idx, column in enumerate(columns):
        if idx > 80:
            continue
        #print(idx + column)
        #print(idx + column)
        #line += style[line_number][idx] + column
        unstyled_line += column
        #if column == " ":
        #    #line += column
        #    line += style[line_number][idx][0] + column
        #else:
        #    #line += style[line_number][idx][0] + column + style
        line += style[line_number][idx][0] + column + style[line_number][idx][1]

    not_empty_line = len(unstyled_line.replace("|", "").replace("*", "").replace(" ", "")) > 0
    #not_empty_line = True
    if not_empty_line:
        line = line.replace('|', '│')
        #line = line.replace('|', 'H')
        line = compress_escapes(line)
        #line = line.replace('\x1b', '\\x1b')
        #print(line + "<<" + str(len(line)), end='')
        #print(line, end='')
        #print()
        #print(line + " <<" + str(len(line)) +"-"+ str(len(unstyled_line)))
        print(line)
        #print(line[:40])


# good_
# good chars for dot:
# ┿
# ╪
# ┯
# ╿
# ┃


#prefix = ">>>>>>> "
#branch = raw_conflict_line[len(prefix):]
#if 'HEAD' in branch:
#result = subprocess.run(['git', 'branch', '--no-color'], stdout=subprocess.PIPE)
#branches = result.stdout.decode("utf-8").splitlines()
#print(branches)
#head_branch = next(line for line in branches if line.startswith("* "))
#head_branch = head_branch[2:]
#branch = head_branch
#return branch
#
#
#conflict_last_line = next(line for line in merged.splitlines() if line.startswith(">>>>>>>"))
#remote_branch = get_normalized_branch(conflict_last_line)
#
#conflict_first_line = next(line for line in merged.splitlines() if line.startswith("<<<<<<<"))
#local_branch = get_normalized_branch(conflict_first_line)
#
#
