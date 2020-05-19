public void setBirthday(java.lang.String birthday, java.lang.String pattern) {
    this.birthday = org.joda.time.format.DateTimeFormat.forPattern(pattern).withZone(DateTimeZone.UTC).parseDateTime(birthday);
}