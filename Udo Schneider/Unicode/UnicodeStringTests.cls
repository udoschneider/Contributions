"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #UnicodeStringTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UnicodeStringTests guid: (GUID fromString: '{7d6b19d1-da04-4c52-87ed-8fb39f85201b}')!
UnicodeStringTests comment: ''!
!UnicodeStringTests categoriesForClass!Unclassified! !
!UnicodeStringTests methodsFor!

testAsByteArray
	| string |
	string := 'ABCD'.
	self assert: string asByteArray = #[65 66 67 68].
	self assert: string asUtf16String asByteArray = #[65 0 66 0 67 0 68 0]!

testAsSymbol
	| string |
	string := 'Test'.
	self assert: string asSymbol = string asUtf16String asSymbol!

testByteStringEndsWithNull
| string bytes expectedResult |
	string := 'A'.
bytes := ByteArray fromAddress: string yourAddress length:2.
expectedResult := #[65 0].
self assert: bytes = expectedResult description: 'ByteString does not end with null char'!

testConcatenation
	| string1 string2 result |
	string1 := 'Udo'.
	string2 := 'Schneider'.
	result := 'UdoSchneider'.
	self assert: (string1 , string2) = result.
	self assert: (string1 asUtf16String , string2) = result.
	self assert: (string1 , string2 asUtf16String) = result.
	self assert: (string1 asUtf16String , string2 asUtf16String) = result.
	self assert: (string1 , string2) = result asUtf16String.
	self assert: (string1 asUtf16String , string2) = result asUtf16String.
	self assert: (string1 , string2 asUtf16String) = result asUtf16String.
	self assert: (string1 asUtf16String , string2 asUtf16String) = result asUtf16String!

testCopiedUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUtf16String copy.
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[65 0 0 0].
	self assert: bytes = expectedResult
		description: 'Copied UnicodeString does not end with double null char'!

testDeepCopiedUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUtf16String deepCopy.
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[65 0 0 0].
	self assert: bytes = expectedResult
		description: 'Deep Copied UnicodeString does not end with double null char'!

testEquality
	| string ustring |
	string := 'abc123'.
	ustring := string asUtf16String.
	self assert: string = string.
	self assert: ustring = ustring.
	self assert: string = ustring.
	self assert: ustring = ustring.
	self deny: string = 'def446'.
	self deny: ustring = 'def446'!

testHash
	| string ustring |
	string := 'abc123'.
	ustring := string asUtf16String.
	self assert: string hash = ustring hash!

testShallowCopiedUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUtf16String shallowCopy.
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[65 0 0 0].
	self assert: bytes = expectedResult
		description: 'Shallow Copied UnicodeString does not end with double null char'!

testUnicodeStringEndsWithNullNull
	| string bytes expectedResult |
	string := 'A' asUtf16String.
	bytes := ByteArray fromAddress: string yourAddress length: 4.
	expectedResult := #[65 0 0 0].
	self assert: bytes = expectedResult description: 'UnicodeString does not end with double null char'! !
!UnicodeStringTests categoriesFor: #testAsByteArray!public! !
!UnicodeStringTests categoriesFor: #testAsSymbol!public! !
!UnicodeStringTests categoriesFor: #testByteStringEndsWithNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testConcatenation!public!tests! !
!UnicodeStringTests categoriesFor: #testCopiedUnicodeStringEndsWithNullNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testDeepCopiedUnicodeStringEndsWithNullNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testEquality!public!tests! !
!UnicodeStringTests categoriesFor: #testHash!public!tests! !
!UnicodeStringTests categoriesFor: #testShallowCopiedUnicodeStringEndsWithNullNull!public!testing!tests! !
!UnicodeStringTests categoriesFor: #testUnicodeStringEndsWithNullNull!public!testing!tests! !

