from collections import OrderedDict
from os import path
import os

# Flag to control if errors are raise or prints and returns
RAISE_ERRORS = False

def add_vectors(vector_1, vector_2):
    """
    Returns a list object representing the result of adding two vectors together.

    Arguments:
    vector_1 -- list representing first vector
    vector_2 -- list representing second vector

    Error checking:
    Both arguments must be of type list.
    Both vectors must of the same length.
    Only vector elements of type int can be added together.
    """
    if type(vector_1) is not list:
        if RAISE_ERRORS:
            raise TypeError('vector_1 must be of type list, got {}'
                            .format(type(vector_1)))
        else:
            print('Error: first argument is not a list')
            return

    if type(vector_2) is not list:
        if RAISE_ERRORS:
            raise TypeError('vector_2 must be of type list, got {}'
                            .format(type(vector_2)))
        else:
            print('Error: second argument is not a list')
            return

    if len(vector_1) != len(vector_2):
        if RAISE_ERRORS:
            raise ValueError('the lengths of vector_1 and vector_2 must be equal')
        else:
            print('Error: lengths of the two vectors are different')

    added_vec = []
    for i in range(len(vector_1)):
        if type(vector_1[i]) is not int:
            if RAISE_ERRORS:
                raise TypeError('vector_1[{}] was expected to be of type int but was {}'
                                .format(i, type(vector_1[i])))
            else:
                print('attempted to add incompatible {} to {}'.format(vector_1[i], vector_2[i]))
                return

        if type(vector_2[i]) is not int:
            if RAISE_ERRORS:
                raise TypeError('vector_2[{}] was expected to be of type int but was {}'
                                .format(i, type(vector_2[i])))
            else:
                print('attempted to add incompatible {} to {}'.format(vector_1[i], vector_2[i]))
                return
        added_vec.append(vector_1[i] + vector_2[i])

    return added_vec


def print_frequency(some_text):
    """
    Prints a table of letter frequencies within a string.

    Non-letter characters are ignored.
    Table is sorted alphabetically.
    Letter case is ignored.
    Two blank spaces will separate the letter from its count.

    Returns None in all cases.

    Argument:
    some_text -- string containing the text to be analysed.

    Error checking:
    The argument must be a string object.
    """
    if type(some_text) is not str:
        if RAISE_ERRORS:
            raise TypeError('some_text must be of type str, got {}'
                            .format(type(some_text)))
        else:
            print('Error: only accepts strings')
            return

    char_count = OrderedDict()
    for char in list(some_text.lower()):
        if not char.isalpha():
            continue

        if char in char_count:
            char_count[char] += 1
        else:
            char_count[char] = 1

    to_return = None
    if len(char_count) != 0:
        to_return = ""
        for char in char_count:
            to_return += "{}  {}\n".format(char, char_count[char])

    return to_return

def verbing(some_text):
    """
    Returns a string where the each word has ing added to it if it is 3
    or more characters or length and ly to shorter words.

    Argument:
    some_text -- string containing the text to be analysed.

    Error checking:
    The argument must be a string object.
    """
    if type(some_text) is not str:
        if RAISE_ERRORS:
            raise TypeError('some_text must be of type str, got {}'
                            .format(type(some_text)))
        else:
            print('Error: only accepts strings')
            return

    words = some_text.split()
    for i in range(len(words)):
        if len(words[i]) >= 3:
            words[i] += 'ing'
        else:
            words[i] += 'ly'

    return " ".join(words)

def verbing_file(file_name):
    """
    Returns the contents of a given file after applying the verbing function to each
    line in the file.

    Argument:
    file_name -- name of the file (assumed to exist in same directory from where the
                python script is executed.

    Error checking:
    The argument must be a string object.
    File must exist and be readable (note no need to distinguish these cases).
    """
    if type(file_name) is not str:
        if RAISE_ERRORS:
            raise TypeError('file_name must be of type str, got {}'
                            .format(type(file_name)))
        else:
            print('Error: only accepts strings')
            return

    # these raises are not hidden by RAISE_ERRORS because they are undefined from the assignemnt
    if not path.exists(file_name):
        raise ValueError('File {} does not exist')

    if not path.isfile(file_name):
        raise ValueError('{} is not a file')

    if not path.isfile(file_name):
        raise ValueError('{} is not a file')

    if not os.access(file_name, os.R_OK):
        raise ValueError('Cannot read from {}')

    lines = ""
    with open(file_name, 'r') as file:
        for line in file.readlines():
            lines += verbing(line) + '\n'

    return lines

