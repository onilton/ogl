
cdef (int,int) get_matrix_size(tuple matrix):
    return len(matrix), len(matrix[0])


def get_sub_matrix_idx(list target, tuple start_pos, tuple size):
    start_x, start_y = start_pos
    height, width = size
    end_y = start_y + width

    target_height = len(target)

    if start_x + height > target_height:
        return None

    cdef int offset_x = 0 
    for offset_x in range(height):
        x = start_x + offset_x

        if end_y > len(target[x]):
            return None

    return (start_pos, size)


cdef tuple get_sub_matrix(list target, (int, int) start_pos, (int, int) size):
    cdef int start_x = start_pos[0]
    cdef int start_y = start_pos[1]
    cdef int height = size[0]
    cdef int width = size[1]
    cdef int end_y = start_y + width

    cdef int target_height = len(target)

    if start_x + height > target_height:
        return None

    cdef list window = []
    #window = [None]*height

    cdef int offset_x = 0 
    cdef int x = 0 

    for offset_x in range(height):
        x = start_x + offset_x

        if end_y > len(target[x]):
            return None

        window.append(tuple(target[x][start_y:end_y]))
        #window[offset_x] = target[x][start_y:end_y]

    return tuple(window)

def equals_matrix(tuple expected, list target, tuple start_pos):
    start_x, start_y = start_pos

    for i in range(len(expected)):
        for j in range(len(expected[i])):
            if target[start_x + i][start_y + j] != expected[i][j]:
                return False

    return True


cdef void replace_matrix(tuple replacement, list target, (int, int) start_pos):
    cdef int start_x = start_pos[0]
    cdef int start_y = start_pos[1]
    cdef int replacement_size  = len(replacement)

    #for i in range(len(replacement)):
    #print(start_x, start_y)


    cdef int i = 0 
    cdef int j = 0 
    for i in range(replacement_size):
        for j in range(len(replacement[i])):
            target[start_x + i][start_y + j] = replacement[i][j]


def print_matrix(matrix):
    for i in range(len(matrix)):
        print(matrix[i])


def replace_list(list target, dict substitutions, max_column=None, list style=None):
    #cdef int[:] view = target
    cdef int target_size = len(target)
    expecteds_set = set(substitutions.keys())
    cdef (int,int) expected_size = get_matrix_size(list(substitutions.values())[0][0])
    cdef int lidx = 0 
    cdef int ridx = 0
    cdef (int, int) start_pos
    for lidx in range(target_size):
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



