#!/usr/bin/env python
from common import *
import unittest

class ReservationAddTestCase(unittest.TestCase):
    def setUp(self):
        self.imdb_id = generate_random_string(16)
        self.screen_id = generate_random_string(16)
        self.available_seats = 1
        self.conn = get_connection()
        data = movie_register_req(imdb_id=self.imdb_id, screen_id=self.screen_id, available_seats=self.available_seats)
        response, body = do_request(self.conn, "/movie/store", data)
        self.assertEqual(response.status, 201)

    def tearDown(self):
        self.conn.close()

    def test_01_can_reserve(self):
        data = movie_reserve_req(imdb_id=self.imdb_id, screen_id=self.screen_id)
        response, body = do_request(self.conn, "/reserve/add", data)
        self.assertEqual(response.status, 201)
        self.assertEqual(body["imdbId"], self.imdb_id)
        self.assertEqual(body["screenId"], self.screen_id)
        self.assertTrue("reservationId" in body.keys())

    def test_02_can_get_conflict_on_reservations_exceeded(self):
        data = movie_reserve_req(imdb_id=self.imdb_id, screen_id=self.screen_id)
        response, body = do_request(self.conn, "/reserve/add", data)
        self.assertEqual(response.status, 201)
        response, body = do_request(self.conn, "/reserve/add", data)
        self.assertEqual(response.status, 409)

    def test_03_cannot_reserve_seat_with_invalid_imdb_id(self):
        data = movie_reserve_req(imdb_id=generate_random_string(16), screen_id=self.screen_id)
        response, body = do_request(self.conn, "/reserve/add", data)
        self.assertEqual(response.status, 404)

    def test_04_cannot_reserve_seat_with_invalid_screen_id(self):
        data = movie_reserve_req(imdb_id=self.imdb_id, screen_id=generate_random_string(16))
        response, body = do_request(self.conn, "/reserve/add", data)
        self.assertEqual(response.status, 404)

suite = unittest.TestLoader().loadTestsFromTestCase(ReservationAddTestCase)
unittest.TextTestRunner(verbosity=2).run(suite)
