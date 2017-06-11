#!/usr/bin/env python
from common import *
import unittest

class MovieRetrieveTestCase(unittest.TestCase):
    def setUp(self):
        self.imdb_id = generate_random_string(16)
        self.screen_id = generate_random_string(16)
        self.available_seats = 8
        self.conn = get_connection()
        data = movie_register_req(imdb_id=self.imdb_id, screen_id=self.screen_id, available_seats=self.available_seats)
        response, body = do_request(self.conn, "/movie/store", data)
        self.assertEqual(response.status, 201)

    def tearDown(self):
        self.conn.close()

    def test_01_can_retrieve_movie(self):
        data = movie_retrieve_req(imdb_id=self.imdb_id, screen_id=self.screen_id)
        response, body = do_request(self.conn, "/movie/lookup", data)
        self.assertEqual(response.status, 200)
        self.assertEqual(body["imdbId"], self.imdb_id)
        self.assertEqual(body["screenId"], self.screen_id)
        self.assertEqual(body["availableSeats"], self.available_seats)
        self.assertEqual(body["reservedSeats"], 0)

    def test_02_cannot_get_movie_with_invalid_imdb_id(self):
        data = movie_retrieve_req(imdb_id=generate_random_string(16), screen_id=self.screen_id)
        response, body = do_request(self.conn, "/movie/lookup", data)
        self.assertEqual(response.status, 404)

    def test_03_cannot_get_movie_with_invalid_screen_id(self):
        data = movie_retrieve_req(imdb_id=self.imdb_id, screen_id=generate_random_string(16))
        response, body = do_request(self.conn, "/movie/lookup", data)
        self.assertEqual(response.status, 404)

suite = unittest.TestLoader().loadTestsFromTestCase(MovieRetrieveTestCase)
unittest.TextTestRunner(verbosity=2).run(suite)
