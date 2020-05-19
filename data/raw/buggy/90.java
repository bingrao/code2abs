@org.junit.Test
public void homeResource_RedirectToLogin() throws java.lang.Exception {
    mvc.perform(org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get("/"))
        .andDo(org.springframework.test.web.servlet.result.MockMvcResultHandlers.print())
        .andExpect(org.springframework.test.web.servlet.result().MockMvcResultMatchers.status().is3xxRedirection());
}