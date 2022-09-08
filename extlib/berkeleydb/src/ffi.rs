use std::{cell::RefCell, ptr, slice};

use libc::{c_char, c_int, size_t};
use stable_eyre::{eyre::Context, Report};

use crate::BerkeleyDB;

thread_local! {
    static LAST_ERROR: RefCell<Option<String>> = RefCell::new(None);
    static PARSED_RESULTS: RefCell<Option<String>> = RefCell::new(None);
}

/// Given a `Result`, either extract its `Ok` value or set it as an error, returning the provided code.
macro_rules! check {
    ($code:expr, $result:expr) => {
        match $result {
            Ok(a) => a,
            Err(e) => return set_error($code, e),
        }
    };
}

#[no_mangle]
pub extern "C" fn hello_from_rust() {
    println!("Hello from Rust!");
}

/// Read the provided buffer, which must be the content of a BerkeleyDB formatted file.
///
/// The buffer is copied and parsed as a database.
/// The results are then rendered to a JSON encoded array of Base64 items in UTF-8 encoding and stored in thread local storage.
///
/// Returns the length of the results buffer, or a negative value if an error is encountered.
/// If the provided buffer is invalid, returns `-1`.
/// If there is a parse error, returns `-2`.
/// If the returned length cannot fit in a signed 32-bit integer, returns `-3`.
///
/// Use `error_len` and `take_error` to read the error message.
/// Use `take_result` to read the parsed results.
///
/// # Safety
///
/// This function exhibits undefined behavior if the provided buffer is not sized correctly.
#[no_mangle]
pub unsafe extern "C" fn bdb_parse_buf(buf: *mut u8, len: size_t) -> c_int {
    if buf.is_null() {
        return -1;
    }

    let buf = slice::from_raw_parts(buf, len).to_vec();
    let mut db = check!(-2, BerkeleyDB::from(buf).context("parse db"));
    let values = check!(-2, db.read().context("read db"));
    let encoded = check!(-2, serde_json::to_string(&values).context("encode results"));
    let len = check!(-3, i32::try_from(encoded.len() + 1).context("convert len"));

    PARSED_RESULTS.with(|prev| {
        *prev.borrow_mut() = Some(encoded);
    });

    len
}

/// Write the parsed results of calling `parse_buf` into a caller-provided buffer,
/// returning the number of bytes written or a negative error code.
///
/// Writes a UTF-8 string into the buffer regardless of platform.
///
/// After calling this function and successfully getting the results,
/// the results are cleared.
///
/// # Safety
///
/// This function exhibits undefined behavior if the provided buffer is not actually the size passed.
/// If a null pointer is passed, returns `-1`.
/// If the size is too small, returns `-2`; use the return value of [`parse_buf`] to size the buffer and avoid this case.
/// If `parse_buf` has not been called, returns `-3`.
/// If `parse_buf` was called but had an error, returns `-4`. Use [`error_len`] and [`take_error`] to extract the error.
///
/// Error numbers returned from this function do not affect the error returned from [`take_error`].
#[no_mangle]
pub unsafe extern "C" fn bdb_take_result(buf: *mut c_char, len: size_t) -> c_int {
    if buf.is_null() {
        return -1;
    }

    if LAST_ERROR.with(|prev| prev.borrow().is_some()) {
        return -4;
    }

    if let Some(msg) = PARSED_RESULTS.with(|prev| prev.borrow_mut().take()) {
        let buf = slice::from_raw_parts_mut(buf as *mut u8, len as usize);
        if msg.len() >= buf.len() {
            return -2;
        }

        // Copy as a null-terminated string.
        ptr::copy_nonoverlapping(msg.as_ptr(), buf.as_mut_ptr(), msg.len());
        buf[msg.len()] = 0;

        // Return the length.
        return msg.len() as c_int;
    }

    // No error and no result? `parse_buf` was never called.
    -3
}

/// Update the most recent error, clearing whatever may have been there before.
fn set_error(code: impl Into<i32>, err: Report) -> c_int {
    LAST_ERROR.with(|prev| {
        *prev.borrow_mut() = Some(format!("{err:#}"));
    });

    code.into()
}

/// Return the number of bytes in the last error message, or `0` if no error.
#[no_mangle]
pub extern "C" fn bdb_error_len() -> c_int {
    LAST_ERROR.with(|prev| match *prev.borrow() {
        Some(ref err) => err.len() as c_int + 1,
        None => 0,
    })
}

/// Write the most recent error message into a caller-provided buffer,
/// returning the number of bytes written or `0` if no error.
///
/// Writes a UTF-8 string into the buffer regardless of platform.
///
/// # Safety
///
/// This function exhibits undefined behavior if the provided buffer is not actually the size passed.
/// If a null pointer is passed, returns `-1`.
/// If the size is too small, returns `-2`; use [`error_len`] to avoid this case.
///
/// Error numbers returned from this function do not affect the error that this function would have taken.
#[no_mangle]
pub unsafe extern "C" fn bdb_take_error(buf: *mut c_char, len: size_t) -> c_int {
    if buf.is_null() {
        return -1;
    }

    let err = match LAST_ERROR.with(|prev| prev.borrow_mut().take()) {
        Some(err) => err,
        None => return 0,
    };

    let buf = slice::from_raw_parts_mut(buf as *mut u8, len as usize);
    if err.len() >= buf.len() {
        return -2;
    }

    // Copy as a null-terminated string.
    ptr::copy_nonoverlapping(err.as_ptr(), buf.as_mut_ptr(), err.len());
    buf[err.len()] = 0;

    // Return the length.
    err.len() as c_int
}
