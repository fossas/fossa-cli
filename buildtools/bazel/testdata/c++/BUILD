load("@//build_tools/cc:cc.bzl", "dbx_thirdparty_cc_library")

dbx_thirdparty_cc_library(
    name = "textual-includes",
    tags = ["dbx-shelflife-version:libffi=3.2.1-4"],
    textual_hdrs = ["src/dlmalloc.c"],
)

dbx_thirdparty_cc_library(
    name = "ffi",
    srcs = [
        "include/ffi_common.h",
        "include/fficonfig.h",
        "src/closures.c",
        "src/java_raw_api.c",
        "src/prep_cif.c",
        "src/raw_api.c",
        "src/types.c",
        "src/x86/ffi.c",
        "src/x86/ffi64.c",
        "src/x86/sysv.S",
        "src/x86/unix64.S",
    ],
    hdrs = [
        "include/ffi.h",
        "include/ffitarget.h",
    ],
    copts = [
        "-DHAVE_CONFIG_H",
        "-I$(GENDIR)/external/ubuntu_libffi/include",
        "-Iexternal/ubuntu_libffi/include",
        "-Wno-unused-result",
        # Upstream likes to put -Ofast-math in CFLAGS, but we censor
        # that for safety.
    ],
    strip_include_prefix = "include",
    tags = ["dbx-shelflife-version:libffi=3.2.1-4"],
    visibility = ["//visibility:public"],
    deps = [
        ":textual-includes",
    ],
)

genrule(
    name = "configured-headers",
    srcs = ["src/x86/ffitarget.h"],
    outs = [
        "include/ffi.h",
        "include/ffitarget.h",
        "include/fficonfig.h",
    ],
    cmd = """
     ...
   """
)
