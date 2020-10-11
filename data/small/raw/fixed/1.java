@mockit.Test(expected = mockit.UnexpectedInvocation.class)
public void mockSpecificEnumElementsEvenWhenUsingASingleStrictMockInstance(@mockit.Mocked(value = "getDescription")
mockit.MockedEnumsTest.MyEnum unused) {
        new mockit.Expectations() {
        {
        onInstance(mockit.MockedEnumsTest.MyEnum.First).getDescription();
        onInstance(mockit.MockedEnumsTest.MyEnum.Second).getDescription();
        }
        };
        mockit.MockedEnumsTest.MyEnum.Second.getDescription();
        }