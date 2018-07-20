package fossa

import (
	"fmt"
	"net/url"
	"strconv"
)

var UsersAPI = "/api/users/%s"

type User struct {
	OrganizationID int
}

func GetOrganizationID() (string, error) {
	q := url.Values{}
	q.Add("count", "1")

	var users []User
	_, err := GetJSON(fmt.Sprintf(UsersAPI, "?"+q.Encode()), &users)
	if err != nil {
		return "", err
	}

	return strconv.Itoa(users[0].OrganizationID), nil
}
