package buckaudit

import (
	"fmt"
	"testing"

	"github.com/fossas/fossa-cli/graph"
)

func TestBuck(t *testing.T) {
	pack := "buck"
	depPaths, _ := deps(pack)

	revisionMap, _ := uploadDeps(depPaths)
	depGraphJSON, _ := transDepsJSON(pack)

	transDeps, _ := depGraph(depGraphJSON, revisionMap)
	imports, _ := directDeps(pack, revisionMap)
	out := graph.Deps{
		Direct:     imports,
		Transitive: transDeps,
	}
	fmt.Printf("\n%+v\n", out)
}

var inputs = '''{"//third-party/java/zstd-jni:zstd-jni": [
	"third-party/java/zstd-jni/LICENSE",
	"third-party/java/zstd-jni/zstd-jni-1.3.5-2.jar"
    ],
    "//third-party/py/pathlib:pathlib.py": [
	"third-party/py/pathlib/pathlib.py"
    ],
    "//third-party/py/pywatchman:pywatchman-archive": [
	"third-party/py/pywatchman/LICENSE",
	"third-party/py/pywatchman/pywatchman/__init__.py",
	"third-party/py/pywatchman/pywatchman/capabilities.py",
	"third-party/py/pywatchman/pywatchman/compat.py",
	"third-party/py/pywatchman/pywatchman/encoding.py",
	"third-party/py/pywatchman/pywatchman/load.py",
	"third-party/py/pywatchman/pywatchman/pybser.py"
    ],
    "//third-party/py/six:six.py": [
	"third-party/py/six/six.py"
    ],
    "//third-party/py:typing-archive": [],
    "//third-party/py:typing.py": [
	"third-party/py/typing/python2/typing.py"
    ]
  }'''
