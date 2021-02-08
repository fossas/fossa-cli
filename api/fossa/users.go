package fossa

import (
	"errors"
	"fmt"
	"strconv"
)

const OrganizationAPI = "/api/cli/organization"

var (
	cachedOrganization = Organization{-1, false}
	MockOrgID            string
	MockOrgSAML			 bool
)

type Organization struct {
	OrganizationID int
	UsesSAML       bool `json:"usesSAML,omitEmpty"`
}

func mockOrg() (Organization, error) {
	mockID, err := strconv.Atoi(MockOrgID)
	if err != nil {
		return Organization{}, fmt.Errorf("Invalid mocked Org ID, expected integer string, found: '%v'", MockOrgID)
	}
	return Organization{mockID, MockOrgSAML}, nil
}

func GetOrganizationID() (string, error) {
	org, err := GetOrganization()
	if err != nil {
		return "", err
	}
	return strconv.Itoa(org.OrganizationID), nil
}

func GetOrganization() (Organization, error) {
	if MockOrgID != "" {
		return mockOrg()
	}

	if cachedOrganization.OrganizationID != -1 {
		return cachedOrganization, nil
	}

	var organization Organization
	_, err := GetJSON(OrganizationAPI, &organization)
	if err != nil {
		return Organization{}, err
	}
	if organization.OrganizationID == 0 {
		return Organization{}, errors.New("could not get organization for api key")
	}

	cachedOrganization = organization
	return cachedOrganization, nil
}
