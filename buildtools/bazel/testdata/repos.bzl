# the go_repository rules below have hand written attributes, auto-generated go_repository rules
# are defined in third_party/automatic_repos.bzl

load("@bazel_gazelle//:deps.bzl", "go_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def go_repositories():
    # rules_go deps
    go_repository(
        name = "com_github_gogo_protobuf",
        build_extra_args = ["-exclude=vendor"],
        build_file_generation = "on",
        build_file_proto_mode = "legacy",
        importpath = "github.com/test/one",
        patch_args = ["-p1"],
        patches = [
            "//patches:gogo-protobuf-fieldmask-json.patch",  
        ],
        sum = "h1:DqDEcV5aeaTmdFBePNpYsp3FlcVH/2ISVVM9Qf8PSls=",
        version = "v1.0.0",
    )

    go_repository(
        name = "com_github_gogo_protobuf",
        build_extra_args = ["-exclude=vendor"],
        build_file_generation = "on",
        build_file_proto_mode = "legacy",
        importpath = "github.com/test/two",
        patch_args = ["-p1"],
        patches = [
            "//patches:gogo-protobuf-fieldmask-json.patch",  
        ],
        sum = "h1:DqDEcV5aeaTmdFBePNpYsp3FlcVH/2ISVVM9Qf8PSls=",
        version = "v2.0.0",
    )