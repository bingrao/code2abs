@org.springframework.web.bind.annotation.RequestMapping(value = "/welcome")
public java.lang.String welcome() {
    java.lang.System.out.println("in annotation controller");
    org.springframework.http.HttpHeaders headers = new org.springframework.http.HttpHeaders();
    headers.add("Content-Type", "application/json");
    return "welcome";
}