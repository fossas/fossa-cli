package fossa

import (
	"errors"
	"strconv"
)

const OrganizationAPI = "/api/cli/organization"

var CachedOrganizationID = -1

type Organization struct {
	OrganizationID int
}

func GetOrganizationID() (string, error) {
	if CachedOrganizationID != -1 {
		return strconv.Itoa(CachedOrganizationID), nil
	}

	var organization Organization
	_, err := GetJSON(OrganizationAPI, &organization)
	if err != nil {
		return "", err
	}
	if organization.OrganizationID == 0 {
		return "", errors.New("could not get organization for api key")
	}

	CachedOrganizationID = organization.OrganizationID
	return strconv.Itoa(CachedOrganizationID), nil
}
