#!/usr/bin/python3
# This Python file uses the following encoding: utf-8

"""
This module contains the functions that relate to the first three
lectures of the course (basic training and lists).
"""


def distance(t, accel):
    """
    Print the distance travelled.

    Calculates the distance travelling in a given time (t) at a given
    acceleration (accel).

    You should round the result to ONE decimal places (use the round
    method, for example round(10.01,1) creates the output 10.0). Why
    do this? To hide any problems related to floating point precision.

    Args:
       t is an integer
       accel is a float

    """
    dist = round(accel * t**2 * 0.5, 1)
    print(dist)


def pythagoras(a, b):
    """
    Find the length of a third side of a triangle.

    Uses pythagoras' theorem to calculate the length, that is the sum
    of the squares of two legs (a and b) is equal to the sum of the
    squares of the other leg.

    If a and b are both equal or greater than zero, print
    the length of the leg.

    If a is zero or negative print "First leg is invalid"

    If b is zero or negative print "Second leg is invalid"

    Again you should round the output to one decimal place (hide
    precision problems).

    Args:
       a is an integer or float
       b is an integer or float

    """
    if a <= 0:
        print('First leg is invalid')
        return

    if b <= 0:
        print('Second leg is invalid')
        return

    c = round((a**2 + b**2)**0.5, 1)
    print(c)


def grade(mark, mcr):
    """
    Assign a letter grade based on a mark

    Implement this grading scheme.

    A between 80 and 100
    B between 65 and 79
    C between 50 and 65
    D between 40 and 49
    E between 0 and 39

    K Fail due to not satisfying mandatory course requirements, even
      though the student’s numerical course mark reached the level
      specified for a pass, usually 50%. A student whose course mark
      is below 50 should be given a D (40–49) or E (0 –39), regardless
      of whether they met the mandatory course requirements.

    You should sanity check the types of the inputs.

    Args:
       mark is an integer mark, if not integer print "Invalid mark"

       mcr is a boolean (True if met mandatory requirements, False
        if have not met them), if not boolean print "Invalid mcr"

    """
    if type(mark) is not int or mark > 100 or mark < 0:
        print("Invalid mark")
        return

    if type(mcr) is not bool:
        print("Invalid mcr")
        return

    if not mcr:
        print("K")
    elif 0 <= mark < 40:
        print('E')
    elif 40 <= mark < 50:
        print('D')
    elif 50 <= mark < 65:
        print('C')
    elif 65 <= mark < 80:
        print('B')
    elif 80 <= mark <= 100:
        print('A')


def print_before(text, marker):
    """
    Count how many words occur in a given piece of text up to a marker

    Given a piece of text and a given marker word, display all the
    words from the beginning of the text through to (and including the
    marker word).

    Print the entire list of words in the text should the marker not
    be found (use list operators to implement this functionality).

    You should check the arguments and print an error message if the
    types of the argument are different from expected.

    For example, if the text is not a list display "Expected a list of
    words" and if the marker is not string display "Expected a string".

    Args:
      text is a list of words
      marker is a string
    """
    if type(text) is not list:
        print("Expected a list of words")
        return
    if type(marker) is not str:
        print("Expected a string")
        return

    to_print = []
    for word in text:
        # Lazy validation of list
        if type(word) is not str:
            print("Expected list of words")
            return

        if word == marker:
            break
        to_print.append(word)

    print(to_print)
