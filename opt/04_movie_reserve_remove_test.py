#!/usr/bin/env python
from common import *
import unittest

class ReservationRemovalTestCase(unittest.TestCase):
    def setUp(self):
        self.imdb_id = generate_random_string(16)
        self.screen_id = generate_random_string(16)
        self.available_seats = 1
        self.conn = get_connection()
        data = movie_register_req(imdb_id=self.imdb_id, screen_id=self.screen_id, available_seats=self.available_seats)
        response, body = do_request(self.conn, "/movie/store", data)
        self.assertEqual(response.status, 201)
        data = movie_reserve_req(imdb_id=self.imdb_id, screen_id=self.screen_id)
        response, body = do_request(self.conn, "/reserve/add", data)
        self.assertEqual(response.status, 201)
        self.reservation_id = body["reservationId"]

    def tearDown(self):
        self.conn.close()

    def test_01_can_remove_reserve(self):
        data = movie_reserve_remove_req(imdb_id=self.imdb_id, screen_id=self.screen_id, reservation_id=self.reservation_id)
        response, body = do_request(self.conn, "/reserve/remove", data)
        self.assertEqual(response.status, 200)

    def test_02_cannot_remove_reserve_with_invalid_reservation_id(self):
        data = movie_reserve_remove_req(imdb_id=self.imdb_id, screen_id=self.screen_id, reservation_id=generate_random_string(16))
        response, body = do_request(self.conn, "/reserve/remove", data)
        self.assertEqual(response.status, 404)

    def test_03_cannot_remove_reserve_with_invalid_imdb_id(self):
        data = movie_reserve_remove_req(imdb_id=generate_random_string(16), screen_id=self.screen_id, reservation_id=self.reservation_id)
        response, body = do_request(self.conn, "/reserve/remove", data)
        self.assertEqual(response.status, 404)

    def test_04_cannot_remove_reserve_with_invalid_screen_id(self):
        data = movie_reserve_remove_req(imdb_id=self.imdb_id, screen_id=generate_random_string(16), reservation_id=self.reservation_id)
        response, body = do_request(self.conn, "/reserve/remove", data)
        self.assertEqual(response.status, 404)

suite = unittest.TestLoader().loadTestsFromTestCase(ReservationRemovalTestCase)
unittest.TextTestRunner(verbosity=2).run(suite)
