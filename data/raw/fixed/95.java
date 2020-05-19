@org.springframework.web.bind.annotation.RequestMapping(value = "welcome")
public java.lang.String welcome() {
    java.lang.System.out.println("in annotation controller");
    return "welcome";
}