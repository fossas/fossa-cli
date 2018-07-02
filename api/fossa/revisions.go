package fossa

import (
	"net/url"

	"github.com/fossas/fossa-cli/pkg"
)

type License struct {
	ID             int64  `json:"id"`
	LicenseID      string `json:"licenseId"`
	RevisionID     string `json:"revisionId"`
	LicenseGroupID int64  `json:"licenseGroupId"`
	Ignored        bool   `json:"ignored"`
	Title          string `json:"title"`
	URL            string `json:"url"`
	FullText       string `json:"text"`
	Copyright      string `json:"copyright"`
}

type Revision struct {
	Locator  *Locator `json:"loc"`
	Licenses Licenses `json:"licenses"`
	Project  *Project `json:"project"`
}

type Project struct {
	Title   string   `json:"title"`
	URL     string   `json:"url"`
	Public  bool     `json:"public"`
	Authors []string `json:"authors"`
}

type Licenses = []*License
type Revisions = []*Revision

func FetchRevisionForPackage(p pkg.Package) (rev *Revision, err error) {
	locator := LocatorOf(p.ID)
	ep := serverURL
	ep, _ = ep.Parse("/api/revisions/" + locator.QueryString())
	_, err = GetJSON(ep.String(), &rev)

	if err != nil {
		return rev, err
	}

	if rev.Locator == nil {
		rev.Locator = locator
	}
	if len(rev.Licenses) == 0 {
		rev.Licenses = append(rev.Licenses, &License{
			LicenseID: "UNKNOWN",
		})
	}

	if rev.Project == nil {
		rev.Project = &Project{
			Title: rev.Locator.Project,
			URL:   "UNKNOWN",
		}
	}
	return rev, err
}

func FetchRevisionForDeps(deps map[pkg.ID]pkg.Package) (revs Revisions, err error) {
	pkgs := make([]string, 0, len(deps))
	for pkgID := range deps {
		qs, _ := url.QueryUnescape(LocatorOf(pkgID).QueryString())
		pkgs = append(pkgs, qs)
	}

	// Split pkgs into chunks of 20 for performance reasons
	chunks := make([][]string, 0)
	chunkSize := 20
	for i := 0; i < len(pkgs); i += chunkSize {
		end := i + chunkSize

		if end > len(pkgs) {
			end = len(pkgs)
		}

		chunks = append(chunks, pkgs[i:end])
	}

	ch := make(chan Revisions, len(chunks)) // buffered
	for _, chunk := range chunks {
		qs := url.Values{}
		for _, q := range chunk {
			qs.Add("locator", q)
		}
		ep := serverURL
		ep, err = ep.Parse("/api/revisions?" + qs.Encode())

		if err != nil {
			return revs, err
		}

		go func(url string) {
			var ret Revisions
			_, err = GetJSON(ep.String(), &ret)
			if err != nil {
				close(ch)
			}
			ch <- ret
		}(ep.String())
	}

	revs = make(Revisions, 0)
	for range chunks {
		select {
		case ret := <-ch:
			revs = append(revs, ret...)
		}
	}

	return revs, err
}
