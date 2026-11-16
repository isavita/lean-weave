#!/usr/bin/env python3
"""Deterministic contracts for the health wire harness's owner selection."""
import json
import unittest

from health import OwnerProbes


class Reply:
    def __init__(self, connection):
        self.status = connection.status
        payload = json.dumps({'probe': connection.path.rsplit('/', 1)[-1], 'ok': self.status == 200}).encode('utf-8')
        self.headers = [('x-health-owner', connection.owner), ('x-health-probe-pending', '1'),
            ('content-type', 'application/json'), ('content-length', str(len(payload)))]
        self.body = b'' if connection.method == 'HEAD' else payload

    def getheaders(self): return self.headers
    def read(self): return self.body


class Connection:
    def __init__(self, owner):
        self.owner = owner
        self.status = 200
        self.closed = False
        self.requests = []

    def request(self, method, path):
        assert not self.closed
        self.method, self.path = method, path
        self.requests.append((method, path))

    def getresponse(self): return Reply(self)
    def close(self): self.closed = True


class Factory:
    def __init__(self, owners):
        self.sequence = iter(owners)
        self.created = []

    def __call__(self):
        connection = Connection(next(self.sequence, '0'))
        self.created.append(connection)
        return connection


class HealthOwnerContracts(unittest.TestCase):
    def test_skewed_accepts_still_check_each_retained_owner(self):
        # The old four-connection sample returned false for four healthy owner-0
        # replies on SO_REUSEPORT. Discovery must tolerate that exact sequence.
        factory = Factory(['0', '0', '0', '0', '1'])
        probes = OwnerProbes(0, connection_factory=factory)
        try:
            probes.discover()
            self.assertEqual(probes.discovery_requests, 5)
            self.assertTrue(probes.all_statuses('startup', 200))
            self.assertEqual(len(factory.created), 5, 'assertions must reuse verified connections')
            self.assertEqual([entry['expected_owner'] for entry in list(probes.history)[-2:]], ['0', '1'])
            self.assertTrue(all(connection.closed for connection in factory.created[1:4]))
        finally: probes.close()
        self.assertTrue(all(connection.closed for connection in factory.created))

    def test_bad_status_is_recorded_and_not_retried(self):
        probes = OwnerProbes(0, connection_factory=Factory(['0', '1']))
        try:
            probes.discover()
            probes.owners['0'].status = 503
            self.assertFalse(probes.all_statuses('startup', 200))
            samples = list(probes.history)[-2:]
            self.assertEqual([entry['status'] for entry in samples], [503, 200])
            self.assertEqual(samples[0]['method'], 'GET')
            self.assertEqual(json.loads(samples[0]['body']), {'probe': 'startup', 'ok': False})
            self.assertEqual(samples[0]['headers']['content-type'], 'application/json')
            self.assertEqual(probes.sequence, 4)
        finally: probes.close()

    def test_head_and_get_have_separate_full_observations(self):
        probes = OwnerProbes(0, connection_factory=Factory(['0', '1']))
        try:
            probes.discover()
            probes.request('startup', owner='0')
            probes.request('startup', 'HEAD', owner='0')
            get, head = list(probes.history)[-2:]
            self.assertEqual((get['method'], head['method']), ('GET', 'HEAD'))
            self.assertNotEqual(get['body'], '')
            self.assertEqual(head['body'], '')
            self.assertEqual(get['headers'], head['headers'])
        finally: probes.close()

    def test_missing_owner_fails_bounded_discovery(self):
        factory = Factory(['0'])
        probes = OwnerProbes(0, connection_factory=factory)
        try:
            with self.assertRaisesRegex(AssertionError, 'bounded owner discovery failed'):
                probes.discover(max_connections=8)
            self.assertEqual(len(factory.created), 8)
        finally: probes.close()
        self.assertTrue(all(connection.closed for connection in factory.created))

    def test_retained_connection_owner_change_fails_with_reply(self):
        probes = OwnerProbes(0, connection_factory=Factory(['0', '1']))
        try:
            probes.discover()
            probes.owners['0'].owner = '1'
            with self.assertRaisesRegex(AssertionError, 'retained connection changed owner'):
                probes.all_statuses('startup', 200)
            observation = probes.history[-1]
            self.assertEqual(observation['expected_owner'], '0')
            self.assertEqual(observation['headers']['x-health-owner'], '1')
            self.assertEqual(observation['status'], 200)
        finally: probes.close()


if __name__ == '__main__': unittest.main()
