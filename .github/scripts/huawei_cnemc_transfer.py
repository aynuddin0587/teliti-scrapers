#!/usr/bin/env python3
"""Invoke the existing Phase 4 Huawei function and write a validated raw bundle.

No CNEMC HTTP call from GitHub. No repository writes. Use a fresh --output
folder under RUNNER_TEMP, and pass its manifest.json to the patched R script.
Do not print source response bytes or credentials to logs.
"""

import argparse
import base64
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import sys
import zlib

MAX_PAGE_BYTES = 20 * 1024 * 1024
MAX_PAGES = 50
REGION = "cn-east-3"


def fail(message):
    raise ValueError(message)


def unwrap(value):
    for _ in range(6):
        if isinstance(value, str):
            try:
                value = json.loads(value)
            except json.JSONDecodeError:
                return value
        elif isinstance(value, dict) and "statusCode" in value and "body" in value:
            value = value["body"]
        elif isinstance(value, dict) and "result" in value and "ok" not in value:
            value = value["result"]
        else:
            break
    return value


def safe_diagnostic(value, maximum=240):
    """Show a bounded, single-line diagnostic without logging source payloads."""
    if value is None:
        return "<not reported>"
    if not isinstance(value, (str, int, float, bool)):
        return f"<{type(value).__name__}; content withheld>"
    text = " ".join(str(value).split())
    return text[:maximum]


def check_function_success(obj):
    """Distinguish remote collection errors from malformed SDK responses."""
    if not isinstance(obj, dict):
        fail(
            "Huawei invocation returned no JSON result object "
            f"(result_type={type(obj).__name__}); check FunctionGraph execution logs"
        )
    if obj.get("ok") is not True:
        # The function catches source exceptions and puts the reason in
        # its 'error' field; the old helper accidentally discarded it.
        remote_error = obj.get("error", obj.get("errorMessage", obj.get("message")))
        fail(
            "Huawei function did not report a complete collection: "
            f"purpose={safe_diagnostic(obj.get('purpose'))}; "
            f"ok={safe_diagnostic(obj.get('ok'))}; "
            f"remote_error={safe_diagnostic(remote_error)}; "
            f"reported_rows={safe_diagnostic(obj.get('total_rows'))}; "
            f"reported_pages={safe_diagnostic(obj.get('total_pages'))}. "
            "No source files or backup files were changed."
        )


def decode_raw(entry):
    if not isinstance(entry, dict) or entry.get("encoding") != "zlib+base64":
        fail("Invalid transport encoding")
    compressed = base64.b64decode(entry["body_zlib_b64"], validate=True)
    decoder = zlib.decompressobj()
    raw = decoder.decompress(compressed, MAX_PAGE_BYTES + 1)
    if len(raw) > MAX_PAGE_BYTES or decoder.unconsumed_tail:
        fail("Compressed source exceeded decompression limit")
    raw += decoder.flush()
    if len(raw) > MAX_PAGE_BYTES or not decoder.eof or decoder.unused_data:
        fail("Compressed source had invalid stream boundaries")
    if len(raw) != entry.get("raw_bytes"):
        fail("Transport byte count mismatch")
    if hashlib.md5(raw).hexdigest() != entry.get("raw_md5"):
        fail("Transport MD5 mismatch")
    if hashlib.sha256(raw).hexdigest() != entry.get("raw_sha256"):
        fail("Transport SHA256 mismatch")
    return raw


def decode_page(raw, expected_index, expected_total):
    doc = json.loads(raw.decode("utf-8-sig"))
    if not isinstance(doc, dict):
        fail("CNEMC returned a non-object JSON response")
    result = doc.get("result")
    if result is None or str(result).lower() in ("0", "false", "no", "none", ""):
        fail("CNEMC reported an unsuccessful page")
    headers, rows = doc.get("thead"), doc.get("tbody")
    if not isinstance(headers, list) or not headers:
        fail("CNEMC page missing headers")
    if not isinstance(rows, list) or len(rows) > 2000:
        fail("CNEMC page has missing or excessive rows")
    if int(doc.get("total", 1)) != expected_total:
        fail(f"Page {expected_index} changed total page count")
    return headers, rows


def validate_and_write(obj, out_dir):
    check_function_success(obj)
    if obj.get("purpose") != "export_raw_snapshot_v1":
        fail("Huawei function is not running Phase 4 export mode")
    filters = obj.get("request_filters")
    if filters != {"AreaID": "", "RiverID": "", "MNName": "", "PageSize": 2000}:
        fail("Huawei response was not an unfiltered PageSize=2000 collection")
    transport = obj.get("transport")
    if not isinstance(transport, dict) or transport.get("protocol") != "cnemc-source-bytes-v1":
        fail("Unexpected or missing transfer protocol")
    total_pages = obj.get("total_pages")
    pages = transport.get("pages")
    metadata = obj.get("pages")
    if (type(total_pages) is not int or not 1 <= total_pages <= MAX_PAGES or
            not isinstance(pages, list) or len(pages) != total_pages or
            not isinstance(metadata, list) or len(metadata) != total_pages):
        fail("Incomplete page transfer")
    time_text = obj.get("checked_at_utc")
    if not isinstance(time_text, str):
        fail("Missing original Huawei retrieval timestamp")
    source_time = datetime.fromisoformat(time_text.replace("Z", "+00:00"))
    if source_time.tzinfo is None:
        fail("Original retrieval timestamp has no timezone")
    source_time = source_time.astimezone(timezone.utc)
    age = (datetime.now(timezone.utc) - source_time).total_seconds()
    if not -120 <= age <= 900:
        fail(f"Huawei source response is stale or future-dated (age_seconds={age:.1f})")

    if out_dir.exists():
        fail("Output path already exists; refusing to overwrite prior transfer")
    out_dir.mkdir(mode=0o700, parents=True)
    entries, hashes, header_first, row_total, raw_total = [], [], None, 0, 0
    for i, (page, meta) in enumerate(zip(pages, metadata), 1):
        if (not isinstance(page, dict) or not isinstance(meta, dict) or
                page.get("page_index") != i or meta.get("page_index") != i or
                meta.get("http_status") != 200):
            fail("Page index or HTTP status mismatch")
        raw = decode_raw(page)
        headers, rows = decode_page(raw, i, total_pages)
        if header_first is None:
            header_first = headers
        if headers != header_first:
            fail("Source table headers changed between pages")
        if len(rows) != meta.get("row_count") or len(raw) != meta.get("response_bytes"):
            fail("Source page count or size mismatch")
        md5 = hashlib.md5(raw).hexdigest()
        if md5 != meta.get("source_body_md5"):
            fail("Page MD5 differs from Huawei response metadata")
        filename = f"page_{i:03d}_raw.json"
        path = out_dir / filename
        path.write_bytes(raw)
        if path.read_bytes() != raw:
            fail("Transferred source bytes failed disk round-trip")
        hashes.append(md5)
        row_total += len(rows)
        raw_total += len(raw)
        entries.append({
            "page_index": i,
            "file": filename,
            "raw_bytes": len(raw),
            "raw_md5": md5,
            "raw_sha256": hashlib.sha256(raw).hexdigest(),
            "row_count": len(rows),
            "http_status": meta["http_status"],
            "content_type": meta.get("content_type", ""),
            "server_date": meta.get("server_date", ""),
        })
    snapshot_md5 = hashlib.md5("|".join(hashes).encode("utf-8")).hexdigest()
    if (snapshot_md5 != obj.get("snapshot_md5_r_compatible") or
            row_total != obj.get("total_rows") or row_total <= 0 or
            len(header_first) != obj.get("header_columns")):
        fail("Nationwide row count, header count, or snapshot checksum mismatch")

    dictionary = transport.get("area_river")
    if dictionary is None or not isinstance(obj.get("dictionary"), dict):
        fail("Area-river dictionary missing: dry-run backup requires it")
    dict_raw = decode_raw(dictionary)
    dict_sha = hashlib.sha256(dict_raw).hexdigest()
    if dict_sha != obj["dictionary"].get("sha256"):
        fail("Area-river dictionary SHA256 mismatch")
    dict_name = "area_river_raw.bin"
    (out_dir / dict_name).write_bytes(dict_raw)
    dict_info = {
        "file": dict_name,
        "raw_bytes": len(dict_raw),
        "raw_md5": hashlib.md5(dict_raw).hexdigest(),
        "raw_sha256": dict_sha,
    }

    manifest = {
        "protocol": "cnemc-source-files-v1",
        "source_collector": "huawei_functiongraph",
        "source_region": REGION,
        "source_checked_at_utc": source_time.isoformat(),
        "source_checked_at_unix": source_time.timestamp(),
        "github_received_at_utc": datetime.now(timezone.utc).isoformat(),
        "main_url": "https://szzdjc.cnemc.cn:8070/GJZ/Business/Publish/Main.html",
        "endpoint": "https://szzdjc.cnemc.cn:8070/GJZ/Ajax/Publish.ashx",
        "request_filters": filters,
        "total_pages": total_pages,
        "total_rows": row_total,
        "header_columns": len(header_first),
        "snapshot_md5": snapshot_md5,
        "pages": entries,
        "area_river": dict_info,
    }
    # Manifest written LAST: R should never see an incomplete transfer as ready.
    manifest_path = out_dir / "manifest.json"
    manifest_path.write_text(json.dumps(manifest, ensure_ascii=False, indent=2), encoding="utf-8")
    print(json.dumps({
        "result": "PASS: byte-exact nationwide CNEMC response staged for R",
        "rows": row_total,
        "pages": total_pages,
        "raw_page_bytes": raw_total,
        "source_checked_at_utc": manifest["source_checked_at_utc"],
        "snapshot_md5": snapshot_md5,
    }, indent=2))
    return manifest


def invoke_huawei():
    from huaweicloudsdkcore.auth.credentials import BasicCredentials
    from huaweicloudsdkcore.exceptions import exceptions
    from huaweicloudsdkfunctiongraph.v2 import FunctionGraphClient, InvokeFunctionRequest
    from huaweicloudsdkfunctiongraph.v2.region.functiongraph_region import FunctionGraphRegion

    region = os.environ.get("HUAWEI_REGION", REGION)
    ak = os.environ.get("HUAWEI_AK", "")
    sk = os.environ.get("HUAWEI_SK", "")
    project = os.environ.get("HUAWEI_PROJECT_ID", "")
    urn = os.environ.get("HUAWEI_FUNCTION_URN", "")
    if not all((ak, sk, project, urn)):
        fail("Huawei GitHub secrets or repository variables are missing")
    if region != REGION or len(project) != 32 or not project.isalnum():
        fail("Huawei region or project ID is invalid")
    if not urn.startswith(f"urn:fss:{REGION}:{project}:"):
        fail("Function URN does not match the Shanghai 1 project")

    client = (FunctionGraphClient.new_builder()
              .with_credentials(BasicCredentials(ak, sk, project))
              .with_region(FunctionGraphRegion.value_of(REGION))
              .build())
    req = InvokeFunctionRequest()
    req.function_urn = urn
    req.body = {"purpose": "export_raw_snapshot_v1"}
    req.x_cff_request_version = "v1"
    try:
        response = client.invoke_function(req)
    except exceptions.ClientRequestException as err:
        fail(f"Huawei HTTP {err.status_code}: {err.error_code}; {str(err.error_msg)[:180]}")
    status = getattr(response, "status", None)
    if status is not None and str(status) != "200":
        fail(f"FunctionGraph invocation status was {status}")
    function_error = getattr(response, "function_error", None)
    if function_error:
        fail(f"FunctionGraph reported execution error: {safe_diagnostic(function_error)}")
    return unwrap(getattr(response, "result", None))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = invoke_huawei()
    validate_and_write(result, args.output)


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(f"CNEMC Huawei transfer failed: {type(exc).__name__}: {str(exc)[:250]}", file=sys.stderr)
        sys.exit(1)
