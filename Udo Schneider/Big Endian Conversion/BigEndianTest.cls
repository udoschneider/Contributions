"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #BigEndianTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
BigEndianTest guid: (GUID fromString: '{2928339b-5e1d-47d0-9100-845b67198122}')!
BigEndianTest comment: 'SUnitBrowser openOnTestCase: BigEndianTest'!
!BigEndianTest categoriesForClass!Unclassified! !
!BigEndianTest methodsFor!

assertBigEndianDouble: aDouble equals: aByteArray 
	self
		assert: (aByteArray bigEndianDoubleAtOffset: 0) = aDouble;
		assert: ((ByteArray new: 8)
					bigEndianDoubleAtOffset: 0 put: aDouble;
					yourself) = aByteArray!

assertBigEndianDword: anInteger equals: aByteArray 
	self
		assert: (aByteArray bigEndianDwordAtOffset: 0) = anInteger;
		assert: ((ByteArray new: 4)
					bigEndianDwordAtOffset: 0 put: anInteger;
					yourself) = aByteArray!

assertBigEndianFloat: aFloat equals: aByteArray 
	self
		assert: (aByteArray bigEndianFloatAtOffset: 0) = aFloat;
		assert: ((ByteArray new: 4)
					bigEndianFloatAtOffset: 0 put: aFloat;
					yourself) = aByteArray!

assertBigEndianQword: anInteger equals: aByteArray 
	self
		assert: (aByteArray bigEndianQwordAtOffset: 0) = anInteger;
		assert: ((ByteArray new: 8)
					bigEndianQwordAtOffset: 0 put: anInteger;
					yourself) = aByteArray!

assertBigEndianWord: anInteger equals: aByteArray 
	self
		assert: (aByteArray bigEndianWordAtOffset: 0) = anInteger;
		assert: ((ByteArray new: 2)
					bigEndianWordAtOffset: 0 put: anInteger;
					yourself) = aByteArray!

assertDouble: aDouble equals: aByteArray 
	self
		assert: (aByteArray doubleAtOffset: 0) = aDouble;
		assert: ((ByteArray new: 8)
					doubleAtOffset: 0 put: aDouble;
					yourself) = aByteArray!

assertDword: anInteger equals: aByteArray 
	self 

assert: (aByteArray dwordAtOffset: 0) = anInteger;

		assert: ((ByteArray new: 4)
				dwordAtOffset: 0 put: anInteger;
				yourself) = aByteArray!

assertFloat: aFloat equals: aByteArray 
	self
		assert: (aByteArray floatAtOffset: 0) = aFloat;
		assert: ((ByteArray new: 4)
					floatAtOffset: 0 put: aFloat;
					yourself) = aByteArray!

assertQword: anInteger equals: aByteArray 
	self
		assert: (aByteArray qwordAtOffset: 0) = anInteger;
		assert: ((ByteArray new: 8)
					qwordAtOffset: 0 put: anInteger;
					yourself) = aByteArray!

assertWord: anInteger equals: aByteArray 
	self
		assert: (aByteArray wordAtOffset: 0) = anInteger;
		assert: ((ByteArray new: 2)
					wordAtOffset: 0 put: anInteger;
					yourself) = aByteArray!

testDouble
	self assertDouble: Float pi equals: #[24 45 68 84 251 33 9 64].

	self assertBigEndianDouble: Float pi equals: #[64 9 33 251 84 68 45 24]!

testFloat

self assertFloat: 3.5 equals: #[0 0 96 64].
self assertBigEndianFloat: 3.5 equals: #[64 96 0 0].
!

testUnsignedDword
	self
		assertDword: 16r67452301  equals: #[16r01 16r23 16r45 16r67] ;
		assertDword: 16rEFCDAB89    equals: #[16r89 16rAB 16rCD 16rEF] .
	self
		assertBigEndianDword:  16r01234567   equals: #[16r01 16r23 16r45 16r67];
		assertBigEndianDword: 16r89ABCDEF   equals: #[16r89 16rAB 16rCD 16rEF] !

testUnsignedQword
	self assertQword: 16rEFCDAB8967452301
		equals: #[16r01 16r23 16r45 16r67 16r89 16rAB 16rCD 16rEF].

self assertBigEndianQword: 16r0123456789ABCDEF
		equals: #[16r01 16r23 16r45 16r67 16r89 16rAB 16rCD 16rEF].



!

testUnsignedWord
	self
		assertWord: 16r2301 equals: #[16r01 16r23];
		assertWord: 16r6745 equals: #[16r45 16r67];
		assertWord: 16rAB89 equals: #[16r89 16rAB];
		assertWord: 16rEFCD equals: #[16rCD 16rEF].
	self
		assertBigEndianWord: 16r0123 equals: #[16r01 16r23];
		assertBigEndianWord: 16r4567 equals: #[16r45 16r67];
		assertBigEndianWord: 16r89AB equals: #[16r89 16rAB];
		assertBigEndianWord: 16rCDEF equals: #[16rCD 16rEF]! !
!BigEndianTest categoriesFor: #assertBigEndianDouble:equals:!public! !
!BigEndianTest categoriesFor: #assertBigEndianDword:equals:!public! !
!BigEndianTest categoriesFor: #assertBigEndianFloat:equals:!public! !
!BigEndianTest categoriesFor: #assertBigEndianQword:equals:!public! !
!BigEndianTest categoriesFor: #assertBigEndianWord:equals:!public! !
!BigEndianTest categoriesFor: #assertDouble:equals:!public! !
!BigEndianTest categoriesFor: #assertDword:equals:!public! !
!BigEndianTest categoriesFor: #assertFloat:equals:!public! !
!BigEndianTest categoriesFor: #assertQword:equals:!public! !
!BigEndianTest categoriesFor: #assertWord:equals:!public! !
!BigEndianTest categoriesFor: #testDouble!public! !
!BigEndianTest categoriesFor: #testFloat!public! !
!BigEndianTest categoriesFor: #testUnsignedDword!public! !
!BigEndianTest categoriesFor: #testUnsignedQword!public! !
!BigEndianTest categoriesFor: #testUnsignedWord!public! !

