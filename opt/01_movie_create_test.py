#!/usr/bin/env python
from common import *
import unittest

class MovieRegisterTestCase(unittest.TestCase):
    def setUp(self):
        self.conn = get_connection()

    def tearDown(self):
        self.conn.close()

    def test_01_can_register_movie(self):
        data = movie_register_req()
        response, body = do_request(self.conn, "/movie/store", data)
        self.assertEqual(response.status, 201)

    def test_02_can_get_bad_request_on_long_imdb_id(self):
        data = movie_register_req(imdb_id=generate_random_string(129))
        response, body = do_request(self.conn, "/movie/store", data)
        self.assertEqual(response.status, 400)

    def test_03_can_get_bad_request_on_long_screen_id(self):
        data = movie_register_req(screen_id=generate_random_string(129))
        response, body = do_request(self.conn, "/movie/store", data)
        self.assertEqual(response.status, 400)

suite = unittest.TestLoader().loadTestsFromTestCase(MovieRegisterTestCase)
unittest.TextTestRunner(verbosity=2).run(suite)
