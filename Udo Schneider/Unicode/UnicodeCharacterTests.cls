"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #UnicodeCharacterTests
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
UnicodeCharacterTests guid: (GUID fromString: '{99ef6a3b-a219-4257-bb5e-3c3d786c4dbe}')!
UnicodeCharacterTests comment: ''!
!UnicodeCharacterTests categoriesForClass!Unclassified! !
!UnicodeCharacterTests methodsFor!

testLargeCodePoints
	self shouldnt: [Character value: 256] raise: BoundsError.
	self assert: (Character value: 65536) codePoint = 65536.
	! !
!UnicodeCharacterTests categoriesFor: #testLargeCodePoints!public!tests! !

