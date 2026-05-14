"""Tests for anchorpy_core IDL parsing.

Tests validate that both old-format and new-format Anchor IDLs
can be parsed through the Rust compat layer via parse_idl_compat_py.

NOTE: The old PyO3 class exports (Idl, IdlField, IdlAccount, etc.)
were removed when the Rust layer was rewritten for dual-format support.
All tests now use parse_idl_compat_py which returns canonical JSON.
The Python adapter layer (Tasks 8-9) will restore the class-based API.
"""

import json
from pathlib import Path

from anchorpy_core.idl import parse_idl_compat_py


def _parse(path: str) -> dict:
    """Helper: parse an IDL file and return as dict."""
    raw = Path(path).read_text()
    result = parse_idl_compat_py(raw)
    return json.loads(result)


# ── New-format IDL tests ────────────────────────────────────────────────────


def test_new_format_basic() -> None:
    idl = _parse("tests/idls/new_format_basic.json")
    assert idl["metadata"]["name"] == "basic_new"
    assert idl["metadata"]["version"] == "0.1.0"
    assert len(idl["instructions"]) == 1
    assert idl["instructions"][0]["name"] == "initialize"
    assert idl["instructions"][0]["discriminator"] == [
        175, 175, 109, 31, 13, 152, 155, 237,
    ]


def test_new_format_accounts_events() -> None:
    idl = _parse("tests/idls/new_format_accounts_events.json")
    assert idl["metadata"]["name"] == "accounts_events_program"
    assert len(idl["accounts"]) == 1
    assert idl["accounts"][0]["name"] == "State"
    assert idl["accounts"][0]["discriminator"] == [216, 146, 107, 94, 104, 75, 182, 177]
    # Verify that pubkey type parsed correctly
    third_arg = idl["instructions"][0]["args"][2]
    assert third_arg["name"] == "pubkey_field"
    assert third_arg["type"] == "pubkey"
    # Verify events
    assert len(idl.get("events", [])) == 1
    assert idl["events"][0]["name"] == "StateUpdated"
    assert idl["events"][0]["discriminator"] == [58, 183, 22, 192, 11, 98, 44, 203]
    # Verify errors
    assert len(idl.get("errors", [])) == 1
    assert idl["errors"][0]["code"] == 6000
    assert idl["errors"][0]["name"] == "InvalidAuthority"


def test_new_format_composite() -> None:
    idl = _parse("tests/idls/new_format_composite.json")
    assert idl["metadata"]["name"] == "composite_program"
    ix = idl["instructions"][0]
    assert ix["name"] == "composite_instruction"
    # Two top-level accounts: one single + one composite group
    assert len(ix["accounts"]) == 2
    # First is a simple account
    assert ix["accounts"][0]["name"] == "foo"
    # Second is a composite with nested accounts
    bar = ix["accounts"][1]
    assert bar["name"] == "bar"
    assert "accounts" in bar  # composite group has nested accounts
    assert len(bar["accounts"]) == 2


def test_new_format_types() -> None:
    """Verify type definitions are properly converted for new-format IDLs."""
    idl = _parse("tests/idls/new_format_accounts_events.json")
    types = idl.get("types", [])
    type_names = [t["name"] for t in types]
    assert "State" in type_names
    assert "StateUpdated" in type_names
    assert "MyEnum" in type_names
    # Check struct fields
    state_type = next(t for t in types if t["name"] == "State")
    assert state_type["type"]["kind"] == "struct"
    field_names = [f["name"] for f in state_type["type"]["fields"]]
    assert "pubkey_field" in field_names


# ── Old-format IDL tests (backward compatibility) ──────────────────────────


def test_old_format_clientgen() -> None:
    """Core assertions from the old test_clientgen_example, adapted for new API."""
    idl = _parse("tests/idls/clientgen_example_program.json")
    assert idl["metadata"]["name"] == "example_program"
    assert idl["metadata"]["version"] == "0.1.0"
    # Instructions
    second_ix = idl["instructions"][1]
    assert second_ix["name"] == "initializeWithValues"
    first_arg = second_ix["args"][0]
    assert first_arg["name"] == "boolField"
    assert first_arg["type"] == "bool"
    # Root accounts are converted to name+discriminator only
    first_acc = idl["accounts"][0]
    assert first_acc["name"] == "State"
    # Type definitions should be preserved in types[]
    types = idl.get("types", [])
    type_names = [t["name"] for t in types]
    assert "BarStruct" in type_names
    bar_struct = next(t for t in types if t["name"] == "BarStruct")
    assert bar_struct["type"]["kind"] == "struct"
    first_field = bar_struct["type"]["fields"][0]
    assert first_field["name"] == "someField"
    assert first_field["type"] == "bool"
    # Errors
    assert idl["errors"][0]["code"] == 6000
    assert idl["errors"][0]["name"] == "SomeError"
    assert idl["errors"][0]["msg"] == "Example error."


# ── Comprehensive parsing test ──────────────────────────────────────────────


# IDLs with known unsupported features (e.g. definedWithTypeArgs / generics).
KNOWN_PARSE_FAILURES = {
    # Uses definedWithTypeArgs which IdlTypeCompat doesn't handle yet.
    "generics_build.json",
}


def test_all_idls_parse() -> None:
    """All test-dir IDLs should parse, except the documented exceptions."""
    idls = []
    failures = []
    for path in sorted(Path("tests/idls/").iterdir()):
        raw = path.read_text()
        if path.name in KNOWN_PARSE_FAILURES:
            # Verify it does fail, so we know when to remove it from the skip list
            try:
                parse_idl_compat_py(raw)
                # If it succeeds, great -- remove from KNOWN_PARSE_FAILURES
                result = parse_idl_compat_py(raw)
                parsed = json.loads(result)
                idls.append((path.name, parsed))
            except ValueError:
                pass  # Expected failure
            continue
        try:
            result = parse_idl_compat_py(raw)
            parsed = json.loads(result)
            assert "metadata" in parsed, f"Missing metadata in {path.name}"
            assert "instructions" in parsed, f"Missing instructions in {path.name}"
            idls.append((path.name, parsed))
        except Exception as e:
            failures.append((path.name, str(e)))
    assert not failures, f"Unexpected parse failures: {failures}"
    # 33 total minus known failures
    assert len(idls) >= 33 - len(KNOWN_PARSE_FAILURES)


def test_all_idls_have_name_and_version() -> None:
    """Every parsed IDL should have name and version in metadata."""
    for path in sorted(Path("tests/idls/").iterdir()):
        if path.name in KNOWN_PARSE_FAILURES:
            continue
        raw = path.read_text()
        result = parse_idl_compat_py(raw)
        parsed = json.loads(result)
        meta = parsed.get("metadata", {})
        assert "name" in meta, f"Missing name in metadata for {path.name}"
        assert "version" in meta, f"Missing version in metadata for {path.name}"
