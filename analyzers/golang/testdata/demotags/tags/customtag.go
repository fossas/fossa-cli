// +build customtag

package tags

import "crypto/sha256"

func Foo() {
	_ = sha256.New()
}
