#!/usr/bin/env python3

import unittest
from assignment2 import add_vectors, print_frequency, verbing, verbing_file

class TestAssignment2(unittest.TestCase):

    def test_add_vectors(self):
        for a, b , c in [
            ([1,2,3], [4,5,6,], [5,7,9]),
            ]:
            with self.subTest(Vec1=a, Vec2=b, Result=c):
                self.assertEqual(add_vectors(a,b), c)

    def test_add_vectors_invalid_type(self):
        for a, b  in [
            ("vec", [4,5,6,]),
            ([1,2,3], "vec"),
            ("vec", "vec"),
            ]:
            with self.subTest(Vec1=a, Vec2=b):
                with self.assertRaises(TypeError):
                    add_vectors(a,b)

    def test_add_vectors_invalid_len(self):
        for a, b in [
            ([1,2,3], [4,5,6,4]),
            ([1,2,3,4], [5,6,4]),
        ]:
            with self.subTest(Vec1=a, Vec2=b):
                with self.assertRaises(ValueError):
                    add_vectors(a,b)

    def test_add_vectors_invalid_lsit_contents(self):
        for a, b in [
            ([1,2,3], [5,"String",4]),
            ([1,2,3], [5,6,[[],[],[]]]),
            ([1,{},3], [5,6,3]),
        ]:
            with self.subTest(Vec1=a, Vec2=b):
                with self.assertRaises(TypeError):
                    add_vectors(a,b)

    def test_print_frequency(self):
        for some, table in [
            ("aabb", "a  2\nb  2\n"),
            ("aAbb", "a  2\nb  2\n"),
            (" ", None),
            ("", None),
            ("AbAZBAZbZABA!","a  5\nb  4\nz  3\n"),
            ("AbAZBA*****98328504ZbZABA!","a  5\nb  4\nz  3\n"),
            ]:
            with self.subTest(some_text=some, table=table):
                self.assertEqual(print_frequency(some), table)

    def test_print_frequency_not_str(self):
        for some in [
            (None,),
            (0.0,),
            (1,),
            (['a'],),
            ]:
            with self.subTest(some_text=some):
                with self.assertRaises(TypeError):
                    print_frequency(some)

    def test_verbing(self):
        for some, result in [
            ("a b c", "aly bly cly"),
            ("aaa b c", "aaaing bly cly"),
            ("fdsfds 432432 fdsfdsf", "fdsfdsing 432432ing fdsfdsfing"),
            ]:
            with self.subTest(some_text=some, result=result):
                self.assertEqual(verbing(some), result)

    def test_verbing_not_str(self):
        for some in [
            (None,),
            (0.0,),
            (1,),
            (['a'],),
            ]:
            with self.subTest(some_text=some):
                with self.assertRaises(TypeError):
                    verbing(some)

    def test_verbing_file(self):
        self.assertEqual(verbing_file('monty-python.txt'),
"""Adrianing Wapcaplet:ing Aah,ing comeing in,ing comeing in,ing Mr....Simpson.ing Aaah,ing welcomeing toly
Mousebat,ing Follicle,ing Goosecreature,ing Ampersand,ing Spong,ing Wapcaplet,ing Looseliver,ing
Vendettaing anding Prang!ing
Mr.ing Simpson:ing Thanking you.ing
Wapcaplet:ing Doly siting down--mying name'sing Wapcaplet,ing Adrianing Wapcaplet...ing
Mr.ing Simpson:ing how'd'y'do.ing
Wapcaplet:ing Now,ing Mr.ing Simpson...ing Simpson,ing Simpson...ing French,ing isly it?ing
S:ly No.ing
W:ly Aah.ing Now,ing Ily understanding youing wanting usly toly advertiseing youring washinging powder.ing
S:ly String.ing
W:ly String,ing washinging powder,ing what'sing theing difference.ing Wely caning selling *anything*.ing
S:ly Good.ing Welling Ily haveing thising largeing quantitying ofly string,ing aly hundreding anding twenty-twoing
thousanding *miles*ing ofly itly toly bely exact,ing whiching Ily inherited,ing anding Ily thoughting ifly Ily
advertiseding it...ing
W:ly Ofly course!ing Aly nationaling campaign.ing Usefuling stuff,ing string,ing noly troubleing there.ing
S:ly Ah,ing buting there'sing aly snag,ing youing see.ing Dueing toly bading planning,ing theing hundreding anding
twenty-twoing thousanding milesing isly inly threeing inching lengths.ing Soly it'sing noting verying
useful.ing
W:ly Well,ing that'sing ouring sellinging point!ing
\"SIMPSON'Sing INDIVIDUALing STRINGETTES!\"ing
""")

    def test_verbing_file_errors(self):
        for path in [
                    '/',
                    '/root/.xinitrc',
                ]:
            with self.subTest(path=path):
                with self.assertRaises(ValueError):
                    verbing_file(path)

    def test_verbing_file_not_str(self):
        for path in [
                    None,
                    1,
                    0.0
                ]:
            with self.subTest(path=path):
                with self.assertRaises(TypeError):
                    verbing_file(path)

if __name__ == '__main__':
    unittest.main()
