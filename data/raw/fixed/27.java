@org.springframework.web.bind.annotation.RequestMapping(value = "/users/search/patientDemographic", method = org.springframework.web.bind.annotation.RequestMethod.GET)
gov.samhsa.c2s.provideruiapi.infrastructure.dto.PageableDto<gov.samhsa.c2s.provideruiapi.infrastructure.dto.UmsUserDto> searchUsersByDemographic(@org.springframework.web.bind.annotation.RequestParam(value = "firstName", required = false)
java.lang.String firstName, @org.springframework.web.bind.annotation.RequestParam(value = "lastName", required = false)
java.lang.String lastName, @org.springframework.web.bind.annotation.RequestParam(value = "genderCode", required = false)
java.lang.String genderCode, @org.springframework.web.bind.annotation.RequestParam(value = "birthDate", required = false)
java.lang.String birthDate, @org.springframework.web.bind.annotation.RequestParam(value = "page", required = false)
java.lang.Integer page, @org.springframework.web.bind.annotation.RequestParam(value = "size", required = false)
java.lang.Integer size);