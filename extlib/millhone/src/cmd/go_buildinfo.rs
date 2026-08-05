//! Extract embedded Go build information ("buildinfo") from compiled Go binaries.
//!
//! Go binaries built with module support embed the module list of everything
//! linked into the binary (the same data `go version -m` prints). For Go >= 1.18
//! the data is stored inline as varint-length-prefixed strings following a
//! 16-byte-aligned magic header; this module supports only that inline format.
//! Older binaries (Go < 1.18) use a pointer-based encoding and are skipped.

use serde::Serialize;

/// The 14-byte magic marking the start of the buildinfo header.
const BUILDINFO_MAGIC: &[u8] = b"\xff Go buildinf:";

/// The buildinfo header is always aligned to 16 bytes.
const BUILDINFO_ALIGN: usize = 16;

/// Size of the buildinfo header; inline string data begins at this offset.
const BUILDINFO_HEADER_SIZE: usize = 32;

/// Flag bit: version and modinfo are stored inline (Go >= 1.18).
const FLAG_INLINE_STRINGS: u8 = 0x2;

/// A single Go module (path + version) recorded in a binary's buildinfo.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GoModule {
    pub path: String,
    pub version: String,
}

/// Build information parsed from a Go binary.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct GoBuildInfo {
    /// The Go toolchain version that built the binary, e.g. "go1.25.6".
    pub go_version: String,

    /// The main module ("mod" line). Usually versioned "(devel)" for
    /// locally-built binaries, but carries a real version for binaries
    /// built via `go install module@version`.
    pub main_module: Option<GoModule>,

    /// Dependency modules ("dep" lines), with "=>" replacements applied.
    pub modules: Vec<GoModule>,
}

/// Cheaply decide whether a file's leading bytes look like a binary format
/// that can carry Go buildinfo: ELF, Mach-O (thin or fat, either endianness),
/// or PE. 4 bytes are sufficient; callers may pass any prefix length.
pub fn is_candidate_binary(header: &[u8]) -> bool {
    if header.len() < 4 {
        return false;
    }
    match &header[..4] {
        b"\x7fELF" => true,
        // Mach-O thin: feedface / feedfacf, both byte orders.
        [0xfe, 0xed, 0xfa, 0xce | 0xcf] | [0xce | 0xcf, 0xfa, 0xed, 0xfe] => true,
        // Mach-O fat (universal): cafebabe / bebafeca.
        [0xca, 0xfe, 0xba, 0xbe] | [0xbe, 0xba, 0xfe, 0xca] => true,
        // PE starts with the DOS "MZ" stub.
        [b'M', b'Z', _, _] => true,
        _ => false,
    }
}

/// Scan a whole binary for the Go buildinfo header and parse the inline
/// (Go >= 1.18) representation. Returns `None` when no buildinfo is present,
/// or when the binary uses the older pointer-based encoding.
pub fn parse_go_buildinfo(buf: &[u8]) -> Option<GoBuildInfo> {
    let offset = find_aligned_magic(buf)?;
    let header = &buf[offset..];
    let flags = *header.get(BUILDINFO_MAGIC.len() + 1)?;

    if flags & FLAG_INLINE_STRINGS == 0 {
        // Go < 1.18 pointer-based encoding: out of scope.
        tracing::debug!(
            offset,
            "buildinfo present but not inline format (go < 1.18); skipping"
        );
        return None;
    }

    let rest = header.get(BUILDINFO_HEADER_SIZE..)?;
    let (go_version, rest) = read_varint_string(rest)?;
    let (modinfo, _) = read_varint_string(rest)?;
    let modinfo = strip_modinfo_sentinels(modinfo);

    let mut info = GoBuildInfo {
        go_version: String::from_utf8_lossy(go_version).into_owned(),
        main_module: None,
        modules: Vec::new(),
    };
    if info.go_version.is_empty() {
        return None;
    }

    if let Some(modinfo) = modinfo {
        parse_modinfo(&String::from_utf8_lossy(modinfo), &mut info);
    }
    Some(info)
}

/// Find the byte offset of the buildinfo magic, which the linker places at a
/// 16-byte-aligned offset. Alignment lets us step by 16 rather than 1.
fn find_aligned_magic(buf: &[u8]) -> Option<usize> {
    let mut offset = 0;
    while offset + BUILDINFO_HEADER_SIZE <= buf.len() {
        if buf[offset..].starts_with(BUILDINFO_MAGIC) {
            return Some(offset);
        }
        offset += BUILDINFO_ALIGN;
    }
    None
}

/// Read a Go "inline string": uvarint length followed by that many bytes.
/// Returns the string bytes and the remaining buffer.
fn read_varint_string(buf: &[u8]) -> Option<(&[u8], &[u8])> {
    let (len, consumed) = read_uvarint(buf)?;
    let len = usize::try_from(len).ok()?;
    let start = consumed;
    let end = start.checked_add(len)?;
    if end > buf.len() {
        return None;
    }
    Some((&buf[start..end], &buf[end..]))
}

/// Decode an unsigned LEB128 varint. Returns the value and bytes consumed.
fn read_uvarint(buf: &[u8]) -> Option<(u64, usize)> {
    let mut value: u64 = 0;
    for (i, byte) in buf.iter().take(10).enumerate() {
        value |= u64::from(byte & 0x7f) << (7 * i);
        if byte & 0x80 == 0 {
            return Some((value, i + 1));
        }
    }
    None
}

/// The linker wraps modinfo in 16-byte sentinels. Valid modinfo is at least
/// 33 bytes with a trailing newline just before the closing sentinel; strip
/// 16 bytes from each end. Anything else is treated as absent.
fn strip_modinfo_sentinels(modinfo: &[u8]) -> Option<&[u8]> {
    let len = modinfo.len();
    if len >= 33 && modinfo[len - 17] == b'\n' {
        Some(&modinfo[16..len - 16])
    } else {
        None
    }
}

/// Parse the tab-separated modinfo lines:
///   mod\t<path>\t<version>\t<hash?>   -> main module
///   dep\t<path>\t<version>\t<hash?>   -> dependency
///   =>\t<path>\t<version>\t<hash?>    -> replacement for the preceding dep
///   path\t... / build\t...            -> ignored
fn parse_modinfo(modinfo: &str, info: &mut GoBuildInfo) {
    for line in modinfo.lines() {
        let mut fields = line.split('\t');
        let (directive, path, version) = match (fields.next(), fields.next(), fields.next()) {
            (Some(d), Some(p), v) => (d, p, v.unwrap_or_default()),
            _ => continue,
        };
        let module = GoModule {
            path: path.to_string(),
            version: version.to_string(),
        };
        match directive {
            "mod" => info.main_module = Some(module),
            "dep" => info.modules.push(module),
            // A replacement line overrides the immediately preceding dep:
            // the replacement is what was actually linked into the binary.
            "=>" => {
                if let Some(last) = info.modules.last_mut() {
                    *last = module;
                }
            }
            _ => continue,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a synthetic buildinfo blob: padding to 16-byte alignment, magic,
    /// ptrSize+flags, then inline go version + sentinel-wrapped modinfo.
    fn synthetic_buildinfo(pad: usize, flags: u8, go_version: &str, modinfo_body: &str) -> Vec<u8> {
        let mut buf = vec![0u8; pad];
        buf.extend_from_slice(BUILDINFO_MAGIC);
        buf.push(8); // ptrSize
        buf.push(flags);
        buf.resize(pad + BUILDINFO_HEADER_SIZE, 0);
        push_varint_string(&mut buf, go_version.as_bytes());
        let mut modinfo = vec![0xAAu8; 16];
        modinfo.extend_from_slice(modinfo_body.as_bytes());
        modinfo.extend_from_slice(&[0xBBu8; 16]);
        push_varint_string(&mut buf, &modinfo);
        buf
    }

    fn push_varint_string(buf: &mut Vec<u8>, s: &[u8]) {
        let mut len = s.len() as u64;
        loop {
            let byte = (len & 0x7f) as u8;
            len >>= 7;
            if len == 0 {
                buf.push(byte);
                break;
            }
            buf.push(byte | 0x80);
        }
        buf.extend_from_slice(s);
    }

    const MODINFO: &str = "path\texample.com/app\nmod\texample.com/app\t(devel)\t\ndep\tgithub.com/google/uuid\tv1.6.0\th1:abc=\ndep\tgolang.org/x/sys\tv0.0.0-20220715151400-c0bba94af5f8\th1:def=\n";

    #[test]
    fn parses_inline_buildinfo() {
        let buf = synthetic_buildinfo(64, FLAG_INLINE_STRINGS, "go1.25.6", MODINFO);
        let info = parse_go_buildinfo(&buf).expect("should parse");
        assert_eq!(info.go_version, "go1.25.6");
        assert_eq!(
            info.main_module,
            Some(GoModule {
                path: "example.com/app".into(),
                version: "(devel)".into()
            })
        );
        assert_eq!(
            info.modules,
            vec![
                GoModule {
                    path: "github.com/google/uuid".into(),
                    version: "v1.6.0".into()
                },
                GoModule {
                    path: "golang.org/x/sys".into(),
                    version: "v0.0.0-20220715151400-c0bba94af5f8".into()
                },
            ]
        );
    }

    #[test]
    fn applies_replacement_to_preceding_dep() {
        let modinfo = "mod\texample.com/app\t(devel)\t\ndep\tgithub.com/old/lib\tv1.0.0\th1:x=\n=>\tgithub.com/new/lib\tv2.0.0\th1:y=\ndep\tgithub.com/other\tv3.0.0\th1:z=\n";
        let buf = synthetic_buildinfo(0, FLAG_INLINE_STRINGS, "go1.22.0", modinfo);
        let info = parse_go_buildinfo(&buf).expect("should parse");
        assert_eq!(
            info.modules,
            vec![
                GoModule {
                    path: "github.com/new/lib".into(),
                    version: "v2.0.0".into()
                },
                GoModule {
                    path: "github.com/other".into(),
                    version: "v3.0.0".into()
                },
            ]
        );
    }

    #[test]
    fn keeps_incompatible_versions_verbatim() {
        let modinfo = "mod\texample.com/app\t(devel)\t\ndep\tgithub.com/legacy/big\tv2.0.0+incompatible\th1:x=\n";
        let buf = synthetic_buildinfo(16, FLAG_INLINE_STRINGS, "go1.21.0", modinfo);
        let info = parse_go_buildinfo(&buf).expect("should parse");
        assert_eq!(info.modules[0].version, "v2.0.0+incompatible");
    }

    #[test]
    fn skips_pointer_format() {
        let buf = synthetic_buildinfo(32, 0x0, "go1.17.0", MODINFO);
        assert!(parse_go_buildinfo(&buf).is_none());
    }

    #[test]
    fn rejects_missing_magic() {
        assert!(parse_go_buildinfo(&vec![0u8; 4096]).is_none());
    }

    #[test]
    fn rejects_unaligned_magic() {
        // Magic placed at a non-16-byte-aligned offset must not be found.
        let mut buf = vec![0u8; 8];
        buf.extend_from_slice(&synthetic_buildinfo(
            0,
            FLAG_INLINE_STRINGS,
            "go1.22.0",
            MODINFO,
        ));
        assert!(parse_go_buildinfo(&buf).is_none());
    }

    #[test]
    fn tolerates_invalid_sentinels() {
        // Sentinel validation failure drops modinfo but keeps the go version.
        let mut buf = vec![0u8; 0];
        buf.extend_from_slice(BUILDINFO_MAGIC);
        buf.push(8);
        buf.push(FLAG_INLINE_STRINGS);
        buf.resize(BUILDINFO_HEADER_SIZE, 0);
        push_varint_string(&mut buf, b"go1.22.0");
        push_varint_string(&mut buf, b"too-short");
        let info = parse_go_buildinfo(&buf).expect("should parse");
        assert_eq!(info.go_version, "go1.22.0");
        assert!(info.main_module.is_none());
        assert!(info.modules.is_empty());
    }

    #[test]
    fn detects_candidate_binaries() {
        assert!(is_candidate_binary(b"\x7fELF\x02\x01\x01"));
        assert!(is_candidate_binary(&[0xfe, 0xed, 0xfa, 0xcf]));
        assert!(is_candidate_binary(&[0xcf, 0xfa, 0xed, 0xfe]));
        assert!(is_candidate_binary(&[0xca, 0xfe, 0xba, 0xbe]));
        assert!(is_candidate_binary(b"MZ\x90\x00"));
        assert!(!is_candidate_binary(b"#!/b"));
        assert!(!is_candidate_binary(b"MZ"));
    }

    #[test]
    fn truncated_varint_or_string_is_rejected() {
        let mut buf = Vec::new();
        buf.extend_from_slice(BUILDINFO_MAGIC);
        buf.push(8);
        buf.push(FLAG_INLINE_STRINGS);
        buf.resize(BUILDINFO_HEADER_SIZE, 0);
        // Length claims 100 bytes but only 3 follow.
        buf.push(100);
        buf.extend_from_slice(b"abc");
        assert!(parse_go_buildinfo(&buf).is_none());
    }
}
