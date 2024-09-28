module Tests exposing (tests)

import Review
import StringSpellsCompanyNameCorrectly
import Test exposing (Test)


tests : Test
tests =
    Test.describe "elm-review-mini"
        [ Test.describe "StringSpellsCompanyNameCorrectly"
            [ Test.test "fruits.com in string is accepted"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is fruits.com"
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors = []
                    }
                        |> Review.test
                )
            , Test.test "(should pass) frits.com in string is reported"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is obviously frits.com"
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors =
                        [ { path = "src/A.elm"
                          , message = "misspelled fruits.com"
                          , details = [ "The typo of using frits.com instead of fruits.com has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                          , range = Review.ExpectUnder "frits.com"
                          , fixedFiles =
                                [ { path = "src/A.elm"
                                  , source = """
                                        module A exposing (a)
                                        
                                        a =
                                            "the name is obviously fruits.com"
                                        """
                                  }
                                ]
                          }
                        ]
                    }
                        |> Review.test
                )
            , Test.test "(should fail because module source doesn't parse) fruits.com in string is accepted"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is obviously fruits.com
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors =
                        []
                    }
                        |> Review.test
                )
            , Test.test "(should fail because missing errors) frits.com in string is reported"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is obviously frits.com"
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors =
                        []
                    }
                        |> Review.test
                )
            , Test.test "(should fail because missing fixes) frits.com in string is reported"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is obviously frits.com"
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors =
                        [ { path = "src/A.elm"
                          , message = "misspelled fruits.com"
                          , details = [ "The typo of using frits.com instead of fruits.com has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                          , range = Review.ExpectUnder "frits.com"
                          , fixedFiles = []
                          }
                        ]
                    }
                        |> Review.test
                )
            , Test.test "(should fail because incorrect range) frits.com in string is reported"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is obviously frits.com"
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors =
                        [ { path = "src/A.elm"
                          , message = "misspelled fruits.com"
                          , details = [ "The typo of using frits.com instead of fruits.com has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                          , range = Review.ExpectUnder "\"the name is obviously frits.com\""
                          , fixedFiles =
                                [ { path = "src/A.elm"
                                  , source = """
                                        module A exposing (a)
                                        
                                        a =
                                            "the name is obviously fruits.com"
                                        """
                                  }
                                ]
                          }
                        ]
                    }
                        |> Review.test
                )
            , Test.test "(should fail because fixed module source doesn't parse) frits.com in string is reported"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is obviously frits.com"
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors =
                        [ { path = "src/A.elm"
                          , message = "misspelled fruits.com"
                          , details = [ "The typo of using frits.com instead of fruits.com has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                          , range = Review.ExpectUnder "frits.com"
                          , fixedFiles =
                                [ { path = "src/A.elm"
                                  , source = """
                                        module A exposing (a)
                                        
                                        a =
                                            "the name is obviously fruits.com
                                        """
                                  }
                                ]
                          }
                        ]
                    }
                        |> Review.test
                )
            , Test.test "(should fail because fixed module source doesn't match) frits.com in string is reported"
                (\() ->
                    { projectConfiguration = Review.applicationConfigurationMinimal
                    , files =
                        [ { path = "src/A.elm"
                          , source = """
                                module A exposing (a)
                                
                                a =
                                    "the name is obviously frits.com"
                                """
                          }
                        ]
                    , review = StringSpellsCompanyNameCorrectly.review
                    , expectedErrors =
                        [ { path = "src/A.elm"
                          , message = "misspelled fruits.com"
                          , details = [ "The typo of using frits.com instead of fruits.com has been made and noticed by users too many times. Our company is `fruits.com`, not `frits.com`." ]
                          , range = Review.ExpectUnder "frits.com"
                          , fixedFiles =
                                [ { path = "src/A.elm"
                                  , source = """
                                        module A exposing (a)
                                        
                                        a =
                                            "the name is obviously fruity.com"
                                        """
                                  }
                                ]
                          }
                        ]
                    }
                        |> Review.test
                )
            ]
        ]
