@cucumber.api.java.en.And(value = "^the status of the proposal titled \"([^\"]*)\" is \"([^\"]*)\"$")
public void theStatusOfTheProposalTitledIs(java.lang.String title, cat.udl.eps.entsoftarch.thesismarket.Proposal.Status status) throws java.lang.Throwable {
    cat.udl.eps.entsoftarch.thesismarket.Proposal proposal = proposalRepository.findByTitleContaining(title).get(0);
    assertThat(proposal.getStatus(), org.hamcrest.CoreMatchers.is(status));
}