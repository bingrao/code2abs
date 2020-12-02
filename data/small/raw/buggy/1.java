public void mockSpec(@mockit.Mocked(value = "getDescription")
mockit.MockedEnumsTest.MyEnum unused ) {
    new mockit.Expectations() {{
        mockit.MockedEnumsTest.MyEnum unused = 10;
        mockit.MockedEnumsTest.MyEnum.First.getDescription(unused);
        mockit.MockedEnumsTest.MyEnum.Second.getDescription(unused);
    }};
    mockit.MockedEnumsTest.MyEnum.Second.getDescription(unused);
}