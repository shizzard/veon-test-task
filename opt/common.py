import random
import string
import httplib
import json

def generate_random_string(len):
    return ''.join(
        random.choice(
            string.ascii_uppercase + string.digits
        ) for _ in range(len)
    )

def movie_register_req(imdb_id=None, screen_id=None, available_seats=None):
    if imdb_id is None:
        imdb_id = generate_random_string(16)
    if screen_id is None:
        screen_id = generate_random_string(16)
    if available_seats is None:
        available_seats = 3
    return json.dumps({
        "imdbId": imdb_id,
        "screenId": screen_id,
        "availableSeats": available_seats
    })

def movie_retrieve_req(imdb_id=None, screen_id=None):
    if imdb_id is None:
        imdb_id = generate_random_string(16)
    if screen_id is None:
        screen_id = generate_random_string(16)
    return json.dumps({
        "imdbId": imdb_id,
        "screenId": screen_id
    })

def movie_reserve_req(imdb_id=None, screen_id=None):
    if imdb_id is None:
        imdb_id = generate_random_string(16)
    if screen_id is None:
        screen_id = generate_random_string(16)
    return json.dumps({
        "imdbId": imdb_id,
        "screenId": screen_id
    })

def movie_reserve_remove_req(imdb_id=None, screen_id=None, reservation_id=None):
    if imdb_id is None:
        imdb_id = generate_random_string(16)
    if screen_id is None:
        screen_id = generate_random_string(16)
    if reservation_id is None:
        reservation_id = generate_random_string(16)
    return json.dumps({
        "imdbId": imdb_id,
        "screenId": screen_id,
        "reservationId": reservation_id
    })

def get_connection():
    return httplib.HTTPConnection("localhost:10080")

def do_request(conn, url, data):
    conn.request("POST", url, data)
    response = conn.getresponse()
    body = response.read().decode('utf-8')
    return response, json.loads(body)

