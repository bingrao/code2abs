@org.junit.Test
public void homeResource_RedirectToLogin() throws java.lang.Exception {
    mvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get("/")).andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers.status().is3xxRedirection());
}