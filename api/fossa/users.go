package fossa

import (
	"errors"
	"fmt"
	"strconv"
)

const OrganizationAPI = "/api/cli/organization"

var (
	cachedOrganizationID = -1
	MockOrgID            string
)

type Organization struct {
	OrganizationID int
}

func GetOrganizationID() (string, error) {
	fmt.Println("GETTTING ORG IDD")
	if MockOrgID != "" {
		return MockOrgID, nil
	}

	if cachedOrganizationID != -1 {
		return strconv.Itoa(cachedOrganizationID), nil
	}

	var organization Organization
	_, err := GetJSON(OrganizationAPI, &organization)
	if err != nil {
		return "", err
	}
	if organization.OrganizationID == 0 {
		return "", errors.New("could not get organization for api key")
	}

	cachedOrganizationID = organization.OrganizationID
	return strconv.Itoa(cachedOrganizationID), nil
}
