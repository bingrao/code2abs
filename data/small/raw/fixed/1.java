@mockit.Test(expected = mockit.UnexpectedInvocation.class)
public void mockSpec(@mockit.Mocked(value = "getDescription")
mockit.MockedEnumsTest.MyEnum unused) {
    new mockit.Expectations() {{
        mockit.MockedEnumsTest.MyEnum unused = 10;
        onInstance(mockit.MockedEnumsTest.MyEnum.First).getDescription(unused);
        onInstance(mockit.MockedEnumsTest.MyEnum.Second).getDescription(unused);
    }};
    mockit.MockedEnumsTest.MyEnum.Second.getDescription(unused);
}