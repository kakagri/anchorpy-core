use anchor_lang_idl as anchor_idl;
use pyo3::{
    exceptions::PyValueError,
    prelude::*,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;

// ── Metadata compat (Change 1) ──────────────────────────────────────────────

/// Structured metadata as it appears in new-format (0.30+) IDLs.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlMetadataStructured {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub spec: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub repository: Option<String>,
}

/// Accepts both structured metadata (new format) and raw JSON (old format).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IdlMetadataCompat {
    Structured(IdlMetadataStructured),
    Raw(Value),
}

impl Default for IdlMetadataCompat {
    fn default() -> Self {
        IdlMetadataCompat::Raw(Value::Null)
    }
}

// ── Account definition compat (Change 3) ────────────────────────────────────

/// Handles both old-format (full type def) and new-format (discriminator only)
/// root-level `accounts` entries.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlAccountDefCompat {
    pub name: String,
    #[serde(default)]
    pub discriminator: Option<Vec<u8>>,
    /// Present in old-format IDLs where the struct definition lives here.
    #[serde(rename = "type", default)]
    pub ty: Option<IdlTypeDefinitionTyCompat>,
}

// ── Composite instruction accounts (Change 4) ───────────────────────────────

/// An instruction account that is either a single account or a composite group.
/// Composite MUST come first so the `accounts` field disambiguates before
/// the greedy `Single` variant matches.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IdlAccountItemCompat {
    Composite(IdlCompositeAccountsCompat),
    Single(IdlAccountCompat),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlCompositeAccountsCompat {
    pub name: String,
    pub accounts: Vec<IdlAccountItemCompat>,
}

impl From<IdlAccountItemCompat> for anchor_idl::types::IdlInstructionAccountItem {
    fn from(item: IdlAccountItemCompat) -> Self {
        match item {
            IdlAccountItemCompat::Composite(c) => {
                Self::Composite(anchor_idl::types::IdlInstructionAccounts {
                    name: c.name,
                    accounts: c.accounts.into_iter().map(|a| a.into()).collect(),
                })
            }
            IdlAccountItemCompat::Single(s) => Self::Single(s.into()),
        }
    }
}

// ── Single instruction account compat ───────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlAccountCompat {
    pub name: String,
    #[serde(default)]
    pub is_mut: bool,
    #[serde(default)]
    pub is_signer: bool,
    #[serde(default)]
    pub is_optional: Option<bool>,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    #[serde(default)]
    pub pda: Option<IdlPdaCompat>,
    #[serde(default)]
    pub relations: Vec<String>,
    // New format fields
    #[serde(default)]
    pub writable: bool,
    #[serde(default)]
    pub signer: bool,
    #[serde(default)]
    pub optional: bool,
    #[serde(default)]
    pub discriminator: Option<Vec<u8>>,
}

impl From<IdlAccountCompat> for anchor_idl::types::IdlInstructionAccount {
    fn from(account: IdlAccountCompat) -> Self {
        Self {
            name: account.name,
            docs: account.docs.unwrap_or_default(),
            writable: account.writable || account.is_mut,
            signer: account.signer || account.is_signer,
            optional: account.optional || account.is_optional.unwrap_or(false),
            address: None,
            pda: account.pda.map(|p| p.into()),
            relations: account.relations,
        }
    }
}

// ── PDA compat ──────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlPdaCompat {
    pub seeds: Vec<IdlSeedCompat>,
    #[serde(default)]
    pub program: Option<IdlSeedCompat>,
    // Old format field
    #[serde(default)]
    pub program_id: Option<String>,
}

impl From<IdlPdaCompat> for anchor_idl::types::IdlPda {
    fn from(pda: IdlPdaCompat) -> Self {
        Self {
            seeds: pda.seeds.into_iter().map(|s| s.into()).collect(),
            program: pda.program.map(|p| p.into()),
        }
    }
}

// ── Seeds compat (Change 5) ─────────────────────────────────────────────────

/// Accepts both new-format tagged seeds (`{"kind":"const","value":[…]}`) and
/// old-format untagged seeds (`{"value":"str","ty":"…"}`).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IdlSeedCompat {
    Tagged(IdlSeedTagged),
    // Old untagged variants – order matters: Const has `value`+`ty`, Arg has
    // `path`+optional `ty`, Account has `path`+optional `account`+optional `ty`.
    OldConst(IdlSeedConstCompat),
    OldArg(IdlSeedArgCompat),
    OldAccount(IdlSeedAccountCompat),
}

/// New-format tagged seed enum.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum IdlSeedTagged {
    Const {
        value: SeedConstValue,
    },
    Arg {
        path: String,
    },
    Account {
        path: String,
        #[serde(default)]
        account: Option<String>,
    },
}

/// The `value` of a const seed can be a byte array (new format) or a string
/// (old format passed through the tagged path, unlikely but handled).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SeedConstValue {
    Bytes(Vec<u8>),
    Str(String),
}

impl From<IdlSeedCompat> for anchor_idl::types::IdlSeed {
    fn from(seed: IdlSeedCompat) -> Self {
        match seed {
            IdlSeedCompat::Tagged(t) => match t {
                IdlSeedTagged::Const { value } => {
                    let bytes = match value {
                        SeedConstValue::Bytes(b) => b,
                        SeedConstValue::Str(s) => s.into_bytes(),
                    };
                    Self::Const(anchor_idl::types::IdlSeedConst { value: bytes })
                }
                IdlSeedTagged::Arg { path } => {
                    Self::Arg(anchor_idl::types::IdlSeedArg { path })
                }
                IdlSeedTagged::Account { path, account } => {
                    Self::Account(anchor_idl::types::IdlSeedAccount { path, account })
                }
            },
            IdlSeedCompat::OldConst(c) => Self::Const(c.into()),
            IdlSeedCompat::OldArg(a) => Self::Arg(a.into()),
            IdlSeedCompat::OldAccount(a) => Self::Account(a.into()),
        }
    }
}

// Old-format seed structs (kept for backward compat)

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlSeedConstCompat {
    pub value: String,
    #[serde(default)]
    pub ty: Option<String>,
}

impl From<IdlSeedConstCompat> for anchor_idl::types::IdlSeedConst {
    fn from(seed: IdlSeedConstCompat) -> Self {
        Self {
            value: seed.value.into_bytes(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlSeedArgCompat {
    pub path: String,
    #[serde(default)]
    pub ty: Option<String>,
}

impl From<IdlSeedArgCompat> for anchor_idl::types::IdlSeedArg {
    fn from(seed: IdlSeedArgCompat) -> Self {
        Self {
            path: seed.path,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlSeedAccountCompat {
    pub path: String,
    #[serde(default)]
    pub account: Option<String>,
    #[serde(default)]
    pub ty: Option<String>,
}

impl From<IdlSeedAccountCompat> for anchor_idl::types::IdlSeedAccount {
    fn from(seed: IdlSeedAccountCompat) -> Self {
        Self {
            path: seed.path,
            account: seed.account,
        }
    }
}

// ── Main IDL compat struct ──────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlCompat {
    #[serde(default)]
    pub version: String,
    #[serde(default)]
    pub name: String,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    #[serde(default)]
    pub constants: Vec<IdlConstCompat>,
    pub instructions: Vec<IdlInstructionCompat>,
    #[serde(default)]
    pub accounts: Vec<IdlAccountDefCompat>,
    #[serde(default)]
    pub types: Vec<IdlTypeDefinitionCompat>,
    #[serde(default)]
    pub events: Option<Vec<IdlEventCompat>>,
    #[serde(default)]
    pub errors: Option<Vec<IdlErrorCodeCompat>>,
    #[serde(default)]
    pub metadata: IdlMetadataCompat,
    #[serde(default)]
    pub address: Option<String>,
}

impl From<IdlCompat> for anchor_idl::types::Idl {
    fn from(idl: IdlCompat) -> Self {
        // Extract name/version: prefer structured metadata, fall back to root.
        let (meta_name, meta_version, spec, description, repository) = match &idl.metadata {
            IdlMetadataCompat::Structured(s) => (
                s.name.clone(),
                s.version.clone(),
                s.spec.clone().unwrap_or_else(|| "0.1.0".to_string()),
                s.description.clone(),
                s.repository.clone(),
            ),
            IdlMetadataCompat::Raw(_) => (
                idl.name.clone(),
                idl.version.clone(),
                "0.1.0".to_string(),
                None,
                None,
            ),
        };

        // Build the types array.  Start with explicit `types`.
        let mut types: Vec<anchor_idl::types::IdlTypeDef> =
            idl.types.into_iter().map(|t| t.into()).collect();

        // For old-format accounts that carry an inline type definition,
        // inject those into `types` so both formats end up with struct defs
        // in the types array.
        let mut accounts = Vec::new();
        for acct in idl.accounts {
            let disc = acct.discriminator.clone().unwrap_or_default();
            accounts.push(anchor_idl::types::IdlAccount {
                name: acct.name.clone(),
                discriminator: disc,
            });
            // If this account carries an inline type definition AND there
            // isn't already a type with the same name, inject it.
            if let Some(ty_def) = acct.ty {
                let already_exists = types.iter().any(|t| t.name == acct.name);
                if !already_exists {
                    types.push(anchor_idl::types::IdlTypeDef {
                        name: acct.name,
                        docs: vec![],
                        serialization: anchor_idl::types::IdlSerialization::Borsh,
                        repr: None,
                        generics: vec![],
                        ty: ty_def.into(),
                    });
                }
            }
        }

        Self {
            address: idl.address.unwrap_or_default(),
            metadata: anchor_idl::types::IdlMetadata {
                name: meta_name,
                version: meta_version,
                spec,
                description,
                repository,
                dependencies: vec![],
                contact: None,
                deployments: None,
            },
            docs: idl.docs.unwrap_or_default(),
            instructions: idl.instructions.into_iter().map(|i| i.into()).collect(),
            accounts,
            events: idl.events.unwrap_or_default().into_iter().map(|e| e.into()).collect(),
            errors: idl.errors.unwrap_or_default().into_iter().map(|e| e.into()).collect(),
            types,
            constants: idl.constants.into_iter().map(|c| c.into()).collect(),
        }
    }
}

// ── Instruction compat ──────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlInstructionCompat {
    pub name: String,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    pub accounts: Vec<IdlAccountItemCompat>,
    pub args: Vec<IdlFieldCompat>,
    #[serde(default)]
    pub returns: Option<IdlTypeCompat>,
    #[serde(default)]
    pub discriminator: Option<Vec<u8>>,
}

impl From<IdlInstructionCompat> for anchor_idl::types::IdlInstruction {
    fn from(instruction: IdlInstructionCompat) -> Self {
        Self {
            name: instruction.name,
            docs: instruction.docs.unwrap_or_default(),
            discriminator: instruction.discriminator.unwrap_or_default(),
            accounts: instruction.accounts.into_iter().map(|a| a.into()).collect(),
            args: instruction.args.into_iter().map(|a| a.into()).collect(),
            returns: instruction.returns.map(|r| r.into()),
        }
    }
}

// ── Field compat ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlFieldCompat {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: IdlTypeCompat,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
}

impl From<IdlFieldCompat> for anchor_idl::types::IdlField {
    fn from(field: IdlFieldCompat) -> Self {
        Self {
            name: field.name,
            ty: field.ty.into(),
            docs: field.docs.unwrap_or_default(),
        }
    }
}

// ── Type compat (Change 2) ──────────────────────────────────────────────────

/// Helper for `{ "defined": "X" }` (string) vs `{ "defined": { "name": "X", ... } }` (object).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DefinedValue {
    Object {
        name: String,
        #[serde(default)]
        generics: Vec<IdlTypeCompat>,
    },
    Simple(String),
}

/// Serde-friendly untagged enum that handles all IDL type representations.
///
/// Variant order is critical for untagged deserialization:
/// struct-like variants must come before the catch-all `Simple(String)`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IdlTypeCompat {
    OptionWrapped { option: Box<IdlTypeCompat> },
    VecWrapped { vec: Box<IdlTypeCompat> },
    ArrayWrapped { array: (Box<IdlTypeCompat>, usize) },
    GenericLenArrayWrapped {
        #[serde(rename = "genericLenArray")]
        array: (Box<IdlTypeCompat>, String),
    },
    DefinedWrapped { defined: DefinedValue },
    DefinedWithTypeArgsWrapped {
        #[serde(rename = "definedWithTypeArgs")]
        defined_with_type_args: DefinedWithTypeArgs,
    },
    GenericWrapped { generic: String },
    Simple(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DefinedWithTypeArgs {
    pub name: String,
    #[serde(default)]
    pub args: Vec<GenericArgCompat>,
}

/// Generic argument — either `{"type": "u32"}` or `{"value": "10"}`
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum GenericArgCompat {
    Type {
        #[serde(rename = "type")]
        ty: IdlTypeCompat,
    },
    Const {
        value: String,
    },
}

impl From<IdlTypeCompat> for anchor_idl::types::IdlType {
    fn from(ty: IdlTypeCompat) -> Self {
        match ty {
            IdlTypeCompat::Simple(s) => match s.as_str() {
                "bool" => Self::Bool,
                "u8" => Self::U8,
                "i8" => Self::I8,
                "u16" => Self::U16,
                "i16" => Self::I16,
                "u32" => Self::U32,
                "i32" => Self::I32,
                "f32" => Self::F32,
                "u64" => Self::U64,
                "i64" => Self::I64,
                "f64" => Self::F64,
                "u128" => Self::U128,
                "i128" => Self::I128,
                "u256" => Self::U256,
                "i256" => Self::I256,
                "bytes" => Self::Bytes,
                "string" => Self::String,
                "publicKey" | "pubkey" => Self::Pubkey,
                _ => Self::Defined { name: s, generics: vec![] },
            },
            IdlTypeCompat::DefinedWrapped { defined } => match defined {
                DefinedValue::Simple(name) => Self::Defined { name, generics: vec![] },
                DefinedValue::Object { name, generics } => Self::Defined {
                    name,
                    generics: generics.into_iter().map(|a| match a {
                        IdlTypeCompat::Simple(s) => anchor_idl::types::IdlGenericArg::Const { value: s },
                        _ => anchor_idl::types::IdlGenericArg::Type { ty: a.into() },
                    }).collect(),
                },
            },
            IdlTypeCompat::OptionWrapped { option } => Self::Option(Box::new((*option).into())),
            IdlTypeCompat::VecWrapped { vec } => Self::Vec(Box::new((*vec).into())),
            IdlTypeCompat::ArrayWrapped { array } => {
                Self::Array(Box::new((*array.0).into()), anchor_idl::types::IdlArrayLen::Value(array.1))
            }
            IdlTypeCompat::GenericLenArrayWrapped { array } => {
                Self::Array(Box::new((*array.0).into()), anchor_idl::types::IdlArrayLen::Generic(array.1))
            }
            IdlTypeCompat::DefinedWithTypeArgsWrapped { defined_with_type_args } => {
                Self::Defined {
                    name: defined_with_type_args.name,
                    generics: defined_with_type_args.args.into_iter().map(|a| match a {
                        GenericArgCompat::Type { ty } => anchor_idl::types::IdlGenericArg::Type { ty: ty.into() },
                        GenericArgCompat::Const { value } => anchor_idl::types::IdlGenericArg::Const { value },
                    }).collect(),
                }
            }
            IdlTypeCompat::GenericWrapped { generic } => Self::Generic(generic),
        }
    }
}

// ── Const compat ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlConstCompat {
    pub name: String,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    #[serde(rename = "type")]
    pub ty: IdlTypeCompat,
    pub value: String,
}

impl From<IdlConstCompat> for anchor_idl::types::IdlConst {
    fn from(constant: IdlConstCompat) -> Self {
        Self {
            name: constant.name,
            docs: constant.docs.unwrap_or_default(),
            ty: constant.ty.into(),
            value: constant.value,
        }
    }
}

// ── Type definition compat ──────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlTypeDefinitionCompat {
    pub name: String,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    #[serde(default)]
    pub generics: Vec<String>,
    #[serde(rename = "type")]
    pub ty: IdlTypeDefinitionTyCompat,
}

impl From<IdlTypeDefinitionCompat> for anchor_idl::types::IdlTypeDef {
    fn from(def: IdlTypeDefinitionCompat) -> Self {
        Self {
            name: def.name,
            docs: def.docs.unwrap_or_default(),
            serialization: anchor_idl::types::IdlSerialization::Borsh,
            repr: None,
            generics: def.generics.into_iter().map(|g| {
                anchor_idl::types::IdlTypeDefGeneric::Type { name: g }
            }).collect(),
            ty: def.ty.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum IdlTypeDefinitionTyCompat {
    Struct { fields: Vec<IdlFieldCompat> },
    Enum { variants: Vec<IdlEnumVariantCompat> },
    Alias { value: IdlTypeCompat },
}

impl From<IdlTypeDefinitionTyCompat> for anchor_idl::types::IdlTypeDefTy {
    fn from(ty: IdlTypeDefinitionTyCompat) -> Self {
        match ty {
            IdlTypeDefinitionTyCompat::Struct { fields } => Self::Struct {
                fields: Some(anchor_idl::types::IdlDefinedFields::Named(fields.into_iter().map(|f| f.into()).collect()))
            },
            IdlTypeDefinitionTyCompat::Enum { variants } => Self::Enum {
                variants: variants.into_iter().map(|v| v.into()).collect()
            },
            IdlTypeDefinitionTyCompat::Alias { value } => Self::Type {
                alias: value.into()
            },
        }
    }
}

// ── Enum variant compat ─────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlEnumVariantCompat {
    pub name: String,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    #[serde(default)]
    pub fields: Option<EnumFieldsCompat>,
}

impl From<IdlEnumVariantCompat> for anchor_idl::types::IdlEnumVariant {
    fn from(variant: IdlEnumVariantCompat) -> Self {
        Self {
            name: variant.name,
            fields: variant.fields.map(|f| f.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum EnumFieldsCompat {
    Named(Vec<IdlFieldCompat>),
    Tuple(Vec<IdlTypeCompat>),
}

impl From<EnumFieldsCompat> for anchor_idl::types::IdlDefinedFields {
    fn from(fields: EnumFieldsCompat) -> Self {
        match fields {
            EnumFieldsCompat::Named(fields) => Self::Named(fields.into_iter().map(|f| f.into()).collect()),
            EnumFieldsCompat::Tuple(types) => Self::Tuple(types.into_iter().map(|t| t.into()).collect()),
        }
    }
}

// ── Event compat ────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlEventCompat {
    pub name: String,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    #[serde(default)]
    pub fields: Option<Vec<IdlEventFieldCompat>>,
    #[serde(default)]
    pub discriminator: Option<Vec<u8>>,
}

impl From<IdlEventCompat> for anchor_idl::types::IdlEvent {
    fn from(event: IdlEventCompat) -> Self {
        Self {
            name: event.name,
            discriminator: event.discriminator.unwrap_or_default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlEventFieldCompat {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: IdlTypeCompat,
    #[serde(default)]
    pub docs: Option<Vec<String>>,
    #[serde(default)]
    pub index: bool,
}

impl From<IdlEventFieldCompat> for anchor_idl::types::IdlField {
    fn from(field: IdlEventFieldCompat) -> Self {
        Self {
            name: field.name,
            ty: field.ty.into(),
            docs: field.docs.unwrap_or_default(),
        }
    }
}

// ── Error code compat ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IdlErrorCodeCompat {
    pub code: u32,
    pub name: String,
    #[serde(default)]
    pub msg: Option<String>,
}

impl From<IdlErrorCodeCompat> for anchor_idl::types::IdlErrorCode {
    fn from(error: IdlErrorCodeCompat) -> Self {
        Self {
            code: error.code,
            name: error.name,
            msg: error.msg,
        }
    }
}

// ── Public API ──────────────────────────────────────────────────────────────

pub fn parse_idl_compat(json: &str) -> Result<anchor_idl::types::Idl, serde_json::Error> {
    let idl_compat: IdlCompat = serde_json::from_str(json)?;
    Ok(idl_compat.into())
}

// Python module creation function
pub(crate) fn create_idl_mod(py: Python) -> PyResult<&PyModule> {
    let m = PyModule::new(py, "idl")?;
    m.add_function(wrap_pyfunction!(parse_idl_compat_py, m)?)?;
    Ok(m)
}

#[pyfunction]
fn parse_idl_compat_py(json: &str) -> PyResult<String> {
    let idl = parse_idl_compat(json).map_err(|e| PyValueError::new_err(e.to_string()))?;
    let json_string = serde_json::to_string(&idl).map_err(|e| PyValueError::new_err(e.to_string()))?;
    Ok(json_string)
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backward_compatibility() {
        // Test old format IDL
        let old_format_json = r#"{
            "version": "0.1.0",
            "name": "test",
            "instructions": [
                {
                    "name": "test_instruction",
                    "accounts": [
                        {
                            "name": "test_account",
                            "is_mut": true,
                            "is_signer": false
                        }
                    ],
                    "args": []
                }
            ]
        }"#;

        let result = parse_idl_compat(old_format_json);
        assert!(result.is_ok());

        // Test new format IDL
        let new_format_json = r#"{
            "address": "test_address",
            "metadata": {
                "name": "test",
                "version": "0.1.0",
                "spec": "0.1.0"
            },
            "instructions": [
                {
                    "name": "test_instruction",
                    "discriminator": [1, 2, 3, 4],
                    "accounts": [
                        {
                            "name": "test_account",
                            "writable": true,
                            "signer": false
                        }
                    ],
                    "args": []
                }
            ]
        }"#;

        let result = parse_idl_compat(new_format_json);
        assert!(result.is_ok());
    }

    #[test]
    fn test_new_format_metadata() {
        let json = r#"{
            "address": "test_address",
            "metadata": {
                "name": "my_program",
                "version": "1.2.3",
                "spec": "0.1.0"
            },
            "instructions": []
        }"#;
        let result = parse_idl_compat(json).unwrap();
        assert_eq!(result.metadata.name, "my_program");
        assert_eq!(result.metadata.version, "1.2.3");
        assert_eq!(result.address, "test_address");
    }

    #[test]
    fn test_old_format_metadata_still_works() {
        let json = r#"{
            "version": "0.1.0",
            "name": "old_program",
            "instructions": []
        }"#;
        let result = parse_idl_compat(json).unwrap();
        assert_eq!(result.metadata.name, "old_program");
        assert_eq!(result.metadata.version, "0.1.0");
    }

    #[test]
    fn test_pubkey_type() {
        let json = r#"{
            "address": "test",
            "metadata": {"name": "test", "version": "0.1.0", "spec": "0.1.0"},
            "instructions": [{
                "name": "test_ix",
                "discriminator": [1,2,3,4,5,6,7,8],
                "accounts": [],
                "args": [{"name": "key", "type": "pubkey"}]
            }]
        }"#;
        let result = parse_idl_compat(json);
        assert!(result.is_ok(), "Failed to parse pubkey type: {:?}", result.err());
    }

    #[test]
    fn test_defined_type_as_object() {
        let json = r#"{
            "address": "test",
            "metadata": {"name": "test", "version": "0.1.0", "spec": "0.1.0"},
            "instructions": [{
                "name": "test_ix",
                "discriminator": [1,2,3,4,5,6,7,8],
                "accounts": [],
                "args": [{"name": "val", "type": {"defined": {"name": "MyStruct"}}}]
            }],
            "types": [{
                "name": "MyStruct",
                "type": {"kind": "struct", "fields": [{"name": "x", "type": "u64"}]}
            }]
        }"#;
        let result = parse_idl_compat(json);
        assert!(result.is_ok(), "Failed to parse defined-as-object: {:?}", result.err());
    }

    #[test]
    fn test_new_format_account_discriminator_only() {
        let json = r#"{
            "address": "test",
            "metadata": {"name": "test", "version": "0.1.0", "spec": "0.1.0"},
            "instructions": [],
            "accounts": [
                {"name": "MyAccount", "discriminator": [1,2,3,4,5,6,7,8]}
            ],
            "types": [
                {"name": "MyAccount", "type": {"kind": "struct", "fields": [{"name": "data", "type": "u64"}]}}
            ]
        }"#;
        let result = parse_idl_compat(json).unwrap();
        assert_eq!(result.accounts.len(), 1);
        assert_eq!(result.accounts[0].name, "MyAccount");
        assert_eq!(result.accounts[0].discriminator, vec![1u8,2,3,4,5,6,7,8]);
    }

    #[test]
    fn test_composite_instruction_accounts() {
        let json = r#"{
            "address": "test",
            "metadata": {"name": "test", "version": "0.1.0", "spec": "0.1.0"},
            "instructions": [{
                "name": "composite_ix",
                "discriminator": [1,2,3,4,5,6,7,8],
                "accounts": [
                    {"name": "single_acc", "writable": true, "signer": false},
                    {
                        "name": "nested_group",
                        "accounts": [
                            {"name": "authority", "signer": true},
                            {"name": "token", "writable": true}
                        ]
                    }
                ],
                "args": []
            }]
        }"#;
        let result = parse_idl_compat(json).unwrap();
        assert_eq!(result.instructions[0].accounts.len(), 2);
    }

    #[test]
    fn test_new_format_pda_seeds() {
        let json = r#"{
            "address": "test",
            "metadata": {"name": "test", "version": "0.1.0", "spec": "0.1.0"},
            "instructions": [{
                "name": "test_ix",
                "discriminator": [1,2,3,4,5,6,7,8],
                "accounts": [{
                    "name": "pda_account",
                    "writable": true,
                    "pda": {
                        "seeds": [
                            {"kind": "const", "value": [109, 121, 115, 101, 101, 100]},
                            {"kind": "account", "path": "authority"},
                            {"kind": "arg", "path": "bump"}
                        ]
                    }
                }],
                "args": [{"name": "bump", "type": "u8"}]
            }]
        }"#;
        let result = parse_idl_compat(json);
        assert!(result.is_ok(), "New format PDA seeds failed: {:?}", result.err());
    }
}
