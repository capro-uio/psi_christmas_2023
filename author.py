"""
Author module
"""

import httpx
client = httpx.Client(timeout=10)

from typing import List

APIURL = 'https://api.cristin.no/v2'


class Author:
    def __init__(self, firstname: str, surname: str, cristinid: str):
        """
        Initialize an Author object.

        :param firstname: The first name of the author.
        :param surname: The surname of the author.
        :param cristinid: The Cristin ID of the author.
        """
        self.firstname = firstname
        self.surname = surname
        self.cristinid = cristinid
        self.pubids: tuple[int, ...] = ()
        self._add_pubids()

    def __str__(self):
        return f"{self.firstname} {self.surname} ({self.cristinid})"

    def __repr__(self):
        return f"{self.firstname} {self.surname} ({self.cristinid})"

    def __eq__(self, other):
        return self.cristinid == other.cristinid

    def __hash__(self):
        """
        Return the hash value of the Author object based on its Cristin ID.
        """
        return hash(self.cristinid)

    def _add_pubids(self):
        """
        Queries Cristin for the given researcher and returns a list of publication ids.

        :param cristinid: The cristinid of the researcher to query.
        """
        url = f"{APIURL}/persons/{self.cristinid}/results"
        headers = {'Accept': 'application/json'}
        response = client.get(url, headers=headers, timeout=10)
        if response.status_code == 200:
            self.pubids = tuple(id['cristin_result_id'] for id in response.json())

    def to_json(self):
        return {
            "firstname": self.firstname,
            "surname": self.surname,
            "cristinid": self.cristinid,
            "pubids": self.pubids
        }