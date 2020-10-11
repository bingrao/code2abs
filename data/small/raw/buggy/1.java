public void mockSpecificEnumElementsEvenWhenUsingASingleStrictMockInstance(@mockit.Mocked(value = "getDescription")
mockit.MockedEnumsTest.MyEnum unused) {
        new mockit.Expectations() {
        {
        mockit.MockedEnumsTest.MyEnum.First.getDescription();
        mockit.MockedEnumsTest.MyEnum.Second.getDescription();
        }
        };
        mockit.MockedEnumsTest.MyEnum.Second.getDescription();
        }