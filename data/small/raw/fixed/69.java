public gov.ca.cwds.cals.persistence.model.calsns.rfa.RFA1aApplicant deleteApplicant(java.lang.Long formId, java.lang.Long applicantId) {
    gov.ca.cwds.cals.persistence.model.calsns.rfa.RFA1aApplicant applicant = findApplicantByFormIdAndApplicantId(formId, applicantId);
    if (applicant != null) {
        applicant = delete(applicant.getId());
    }
    return applicant;
}