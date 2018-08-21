package fossa

import (
	"fmt"
	"net/url"
	"strconv"
)

var UsersAPI = "/api/users/%s"

var CachedOrganizationID = -1

type User struct {
	OrganizationID int
}

func GetOrganizationID() (string, error) {
	if CachedOrganizationID != -1 {
		return strconv.Itoa(CachedOrganizationID), nil
	}

	q := url.Values{}
	q.Add("count", "1")

	var users []User
	_, err := GetJSON(fmt.Sprintf(UsersAPI, "?"+q.Encode()), &users)
	if err != nil {
		return "", err
	}

	CachedOrganizationID = users[0].OrganizationID
	return strconv.Itoa(CachedOrganizationID), nil
}
