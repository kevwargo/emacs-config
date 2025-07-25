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
    url: str
    method: str
    headers: dict
    body: bytes

    profile: str
    region: str | None
    service: str | None

    @classmethod
    def read(cls):
        return cls(input())

    def __init__(self, data):
        fields = json.loads(data)

        self.url = fields["url"]
        self.method = fields["method"]
        self.headers = fields.get("headers") or {}
        self.body = (fields.get("body") or "").encode()

        self.profile = fields.get("profile")
        if not self.profile:
            raise ValueError("No profile provided")

        self.region = fields.get("region")
        self.service = fields.get("service")

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
