{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.GraphQL.Parser where

import           Data.Char
import           Data.Either
import           Test.Hspec
import           Test.QuickCheck

import           Data.ByteString  (ByteString)
import qualified Data.Text        as T
import qualified Data.Text.Encoding as T
import           GraphQL.AST
import           GraphQL.Lexer
import           GraphQL.Parser
import           Test.GraphQL.Gen

toBs :: Show a => a -> ByteString
toBs = T.encodeUtf8 . T.pack . show

parserSpec :: Spec
parserSpec = do
  describe "Should parse a Value" $ do
    it "Should parse a float into a ValueFloat" $ property $ \x ->
      value (toBs x) `shouldBe` Right (ValueFloat x)
    it "Should parse a float into a ValueInt" $ property $ \x ->
      value (toBs x) `shouldBe` Right (ValueInt x)
    it "Should parse a bool into a ValueBool" $ property $ \x -> do
      let lower = T.encodeUtf8 . T.pack . map toLower . show
      value (lower x) `shouldBe` Right (ValueBoolean x)
    it "Should parse a float into a ValueNull" $
      value "null" `shouldBe` Right ValueNull
    it "Should parse a String into a ValueString" $
      value "\"hey\"" `shouldBe` Right (ValueString "hey")
    it "Should parse a float into a ValueVariable" $ property $ do
       name@(Name nameValue) <- generate genName
       value ("$" <> T.encodeUtf8 nameValue) `shouldBe`
         Right (ValueVariable (Variable name))
    it "Should parse a Name into a ValueEnum" $ property $ do
       name@(Name nameValue) <- generate genName
       value (T.encodeUtf8 nameValue) `shouldBe`
         Right (ValueEnum (EnumValue name))
    it "Should parse an ObjectList into a ValueObject" $
       value "{ hey : 1, boo : 2 }" `shouldBe`
         Right (ValueObject [ ObjectField (Name "hey") (ValueInt 1)
                            , ObjectField (Name "boo") (ValueInt 2)
                            ])
    it "Should parse a list into a ValueList" $
      value "[2, [], [3, [4 5 []]], { hey : { foo : \"there\" } }, {}]" `shouldBe`
        Right (ValueList [ ValueInt 2
                         , ValueList []
                         , ValueList [ValueInt 3, ValueList [ValueInt 4,ValueInt 5, ValueList []]]
                         , ValueObject [ObjectField (Name "hey")
                             (ValueObject [ObjectField (Name "foo") (ValueString "there")])]
                         , ValueObject []
                         ])

  describe "Should parse a SelectionSet" $ do
    it "Should parse a Selection Set w/o Aliases" $
      selSet "{building {floorCount}}" `shouldBe`
        Right [ SelectionField (Field Nothing (Name "building") [] [] [
          SelectionField (Field Nothing (Name "floorCount") [] [] []) ])
        ]

    it "Should parse a Selection Set w/ Aliases" $
      selSet "{foo:building {okedoke: floorCount}}" `shouldBe` Right
        [ SelectionField (Field (Just (Alias (Name "foo"))) (Name "building") [] [] [
           SelectionField (Field (Just (Alias (Name "okedoke"))) (Name "floorCount") [] [] []) ]) ]

    it "Should parse a Selection Set w/ Arguments" $
      selSet "{building {floorCount (id: 3, foo: \"bar\")}}" `shouldBe` Right
        [ SelectionField (Field Nothing (Name "building") [] [] [
           SelectionField (Field Nothing (Name "floorCount") [
             Argument (Name "id") (ValueInt 3)
           , Argument (Name "foo") (ValueString "bar")
           ] [] []) ]) ]

    it "Should parse a Selection Set w/ Directives" $
      selSet "{ building @height(foo: 4) }" `shouldBe` Right
        [ SelectionField (Field Nothing (Name "building") []
          [ Directive (Name "height") [ Argument (Name "foo") (ValueInt 4) ]] []) ]

    it "Should parse a Selection Set w/ argumentless Directives " $
      selSet "{ building @height }" `shouldBe` Right
        [ SelectionField (Field Nothing (Name "building") []
          [ Directive (Name "height") mempty] []) ]

  describe "Should parse an OperationDefinition" $ do
    it "Should parse an OperationDefinition with VariableDefinition" $
      opDef "{building {floorCount}}" `shouldBe` Right (AnonymousQuery
        [ SelectionField (Field Nothing (Name "building") [] [] [
          SelectionField (Field Nothing (Name "floorCount") [] [] []) ]) ])

    it "Should parse a Named OperationDefinition" $
      opDef "query Building {building {floorCount}}" `shouldBe` Right
        (OperationDefinition Query (Just $ Name "Building") [] []
         [ SelectionField (Field Nothing (Name "building") [] [] [
           SelectionField (Field Nothing (Name "floorCount") [] [] []) ]) ])

    it "Should parse a Named OperationDefinition with VariableDefinitions" $ do
      let varDef = VariableDefinition (Variable (Name "id")) typ Nothing
          typ = TypeNamed (NamedType (Name "Int"))
      opDef "query Building ($id: Int) {building {floorCount}}" `shouldBe` Right
        (OperationDefinition Query (Just $ Name "Building") [varDef] []
         [ SelectionField (Field Nothing (Name "building") [] [] [
           SelectionField (Field Nothing (Name "floorCount") [] [] []) ]) ])

    it "Should parse a Named OperationDefinition with a DefaultValue" $ do
      let varDef = VariableDefinition (Variable (Name "id")) typ defVal
          typ = TypeNamed (NamedType (Name "Int"))
          defVal = Just (DefaultValue (ValueInt 4))
      opDef "query Building ($id: Int = 4) {building {floorCount}}" `shouldBe` Right
        (OperationDefinition Query (Just $ Name "Building") [varDef] []
         [ SelectionField (Field Nothing (Name "building") [] [] [
           SelectionField (Field Nothing (Name "floorCount") [] [] []) ]) ])

  describe "Should parse Fragments" $ do
    it "Should parse a Named FragmentDefinition" $ do
       let frag = "fragment friendFields on User { profilePic(size: 50) }"
       gDef frag `shouldBe` Right
         (DefinitionExecutable (DefinitionFragment
           (FragmentDefinition (Name "friendFields")
             (TypeCondition (NamedType (Name "User"))) [] [
               SelectionField (Field Nothing (Name "profilePic") [
                 Argument (Name "size") (ValueInt 50)] [] [])])))

    it "Should parse a FragmentSpread" $ do
      let spread = "query withFragments { user(id: 4)"
                <> "{ friends(first: 10){ ...friendFields } } }"
      gDef spread `shouldBe` Right
        (DefinitionExecutable (DefinitionOperation (OperationDefinition Query
          (Just (Name "withFragments")) [] [] [
            SelectionField (Field Nothing (Name "user") [
              Argument (Name "id") (ValueInt 4)] [] [
                SelectionField (Field Nothing (Name "friends") [
                  Argument (Name "first") (ValueInt 10)] [] [
                    SelectionFragmentSpread (FragmentSpread (Name "friendFields")
                      [])])])])))

    it "Should parse an InlineFragment" $ do
      let inlineFrag = "query inlineFragmentTyping { profiles(handles:"
                    <> "[\"zuck\", \"cocacola\"]) { ... "
                    <> "on User{friends{count}}}}"
      gDef inlineFrag `shouldBe` Right
        (DefinitionExecutable (DefinitionOperation (OperationDefinition Query
          (Just (Name "inlineFragmentTyping")) [] [] [
            SelectionField (Field Nothing (Name "profiles") [
              Argument (Name "handles") (ValueList [
                ValueString "zuck",ValueString "cocacola"])] [] [
                  SelectionInlineFragment (InlineFragment (Just (TypeCondition
                    (NamedType (Name "User")))) [] [
                      SelectionField (Field Nothing (Name "friends") [] [] [
                         SelectionField (Field Nothing (Name "count") [] [] [])]
                           )])])])))

  describe "Should parse a Definition" $
    it "Should parse an ExecutableDefinition" $ do
      let varDefs = VariableDefinition (Variable (Name "id")) typ' defVal'
          typ' = TypeNamed (NamedType (Name "Int"))
          defVal' = Just (DefaultValue (ValueInt 4))
          oDef' = OperationDefinition Query (Just $ Name "Building") [varDefs] []
             [ SelectionField (Field Nothing (Name "building") [] [] [
               SelectionField (Field Nothing (Name "floorCount") [] [] []) ])
             ]
      gDef "query Building ($id: Int = 4) {building {floorCount}}" `shouldBe`
        Right (DefinitionExecutable (DefinitionOperation oDef'))

  describe "Should parse anonymous queries" $
    it "Should parse two consecutive fields" $ do
      let oDef = AnonymousQuery sels
          sels = [ SelectionField (Field Nothing (Name "dog") [] [] [])
                 , SelectionField (Field Nothing (Name "cat") [] [] [])
                 ]
      gDef "{ dog, cat }" `shouldBe` Right
        (DefinitionExecutable (DefinitionOperation oDef))
  describe "Should fail to parse a Definition purely" $
    it "Should fail to parse" $
      gDef "{{building {floorCount}}" `shouldSatisfy` isLeft

  describe "Should parse Type System Extensions" $ do
    it "Should parse Scalar Type Extensions" $
      gDef "extend scalar Foo @lol" `shouldBe` Right
        (ExtensionTypeSystem
           $ ExtensionType
           $ ExtensionScalarType
           $ ScalarTypeExtension (Name "Foo")
           [ Directive (Name "lol") [] ])
    it "Should parse Scalar Type Extensions w/ Arguments" $
      gDef "extend scalar Foo @lol(hey: 234)" `shouldBe` Right
        (ExtensionTypeSystem
           $ ExtensionType
           $ ExtensionScalarType
           $ ScalarTypeExtension (Name "Foo")
           [ Directive (Name "lol") [Argument (Name "hey") (ValueInt 234)]])

  describe "Should parse Type Synstem Definitions" $
    it "Should parse Schema Definitions" $
      gDef "schema { query : Query, mutation: Mutation }" `shouldBe` Right
        (DefinitionTypeSystem
           $ DefinitionSchema
           $ SchemaDefinition []
           [ OperationTypeDefinition Query (NamedType (Name "Query"))
           , OperationTypeDefinition Mutation (NamedType (Name "Mutation"))
           ])

