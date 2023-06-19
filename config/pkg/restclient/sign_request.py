#!/usr/bin/python3

import json
import os
import re
import traceback
from datetime import datetime
from pathlib import Path

import boto3
from botocore.auth import SigV4Auth
from botocore.awsrequest import AWSRequest

AWS_URL_REGEX = re.compile(r".+\.([^.]+)\.([^.]+)\.amazonaws\..+")


class Request:

    profile: str
    region: str
    service: str

    method: str
    url: str
    headers: dict
    body: bytes

    @classmethod
    def read(cls):
        return cls(input())

    def __init__(self, data):
        self._fields = json.loads(data)

        self.profile = self._fields["profile"]
        if not self.profile:
            raise ValueError("No profile provided")

        self.region = self._fields.get("region")
        self.service = self._fields.get("service")

        self.method = self._fields["method"]
        self.url = self._fields["url"]
        self.headers = self._fields.get("headers") or {}

        self.body = self._fields.get("body") or b""
        if isinstance(self.body, str):
            self.body = self.body.encode()

        if m := AWS_URL_REGEX.match(self.url):
            if not self.service:
                self.service = m.group(1)
            if not self.region:
                self.region = m.group(2)
        if not self.region:
            raise ValueError("No region provided")
        if not self.service:
            self.headers["-default-service"] = self.service = "execute-api"


def main():
    creds_cache = {}
    while True:
        try:
            req = Request.read()
            signed_headers = _sign_request(req, creds_cache)
            print(json.dumps(signed_headers))
        except EOFError:
            break
        except Exception as e:
            exc = f"{type(e).__name__} ({e})"
            _log(f"{exc}: {traceback.format_exc()}")
            print(json.dumps({"error": exc}))


def _sign_request(req: Request, creds_cache: dict) -> dict:
    aws_request = AWSRequest(method=req.method, url=req.url, data=req.body)
    creds = _get_creds(req, creds_cache)
    SigV4Auth(creds, req.service, req.region).add_auth(aws_request)
    return req.headers | dict(aws_request.headers.items())


def _get_creds(req: Request, creds_cache: dict):
    creds_id = f"{req.profile}:{req.region}"
    creds = creds_cache.get(creds_id)
    if not creds:
        _log(f"Creds {creds_id} not found in cache, creating")
    elif creds.refresh_needed():
        _log(f"Creds {creds_id} expired, refreshing")
    else:
        _log(f"Returning cached creds {creds_id}")
        return creds

    session = boto3.Session(profile_name=req.profile, region_name=req.region)
    creds = creds_cache[creds_id] = session.get_credentials()
    return creds


def _log(msg):
    p = Path(__file__)
    logfile = f"{p.parent}/{p.stem}.log"
    with open(logfile, "a") as f:
        time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        print(f"{time}: {os.getpid()}: {msg}", file=f, flush=True)


if __name__ == "__main__":
    main()
