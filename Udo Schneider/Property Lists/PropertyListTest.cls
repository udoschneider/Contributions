"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #PropertyListTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'PropertyListConstants'
	classInstanceVariableNames: ''!
PropertyListTest guid: (GUID fromString: '{0e394b5d-289d-487f-8043-bb5a591aa7d4}')!
PropertyListTest comment: 'SUnitBrowser openOnTestCase: PropertyListTest'!
!PropertyListTest categoriesForClass!Unclassified! !
!PropertyListTest methodsFor!

assertPlist: aByteArray equals: anObject 
	self 
		assert: ((NSPropertyListSerialization propertyListFromData: aByteArray
				format: self class propertyListTypeConstant)  =anObject)!

testArray
	self assertPlist: self class plistArray equals: self class testArray!

testBoolean
	self
		assertPlist: self class plistFalse equals: false;
		assertPlist: self class plistTrue equals: true!

testData
	self assertPlist: self class plistData equals: self class testData!

testDate
	self assertPlist: self class plistDate equals: self class testTimeStamp!

testDictionary
	self assertPlist: self class plistDictionary equals: self class testDictionary!

testInteger
	self
		assertPlist: self class plistInteger equals: self class testInteger!

testNestedArray
	self assertPlist: self class plistNestedArray equals: self class testNestedArray!

testNestedDictionary
	self assertPlist: self class plistNestedDictionary equals: self class testNestedDictionary!

testNotAPlist
	self should: 
			[NSPropertyListSerialization propertyListFromData: self class plistNotAPlist
				format: self class propertyListTypeConstant]
		raise: Exception!

testReal
	self
	
		assertPlist: self class plistReal equals: self class testReal!

testString
	self
		assertPlist: self class plistString equals: self class testString;
		assertPlist: self class plistStringWithSingleQuotes
			equals: self class testStringWithSingleQuotes;
 assertPlist: self class plistStringWithDoubleQuotes
			equals: self class testStringWithDoubleQuotes!

testWrongPlistVersion

	self should: 
			[NSPropertyListSerialization propertyListFromData: self class plistWrongVersion
				format: self class propertyListTypeConstant]
		raise: Exception! !
!PropertyListTest categoriesFor: #assertPlist:equals:!public! !
!PropertyListTest categoriesFor: #testArray!public!test! !
!PropertyListTest categoriesFor: #testBoolean!*-unreferenced selectors!public!test! !
!PropertyListTest categoriesFor: #testData!public!test! !
!PropertyListTest categoriesFor: #testDate!*-unreferenced selectors!public!test! !
!PropertyListTest categoriesFor: #testDictionary!public!test! !
!PropertyListTest categoriesFor: #testInteger!public!test! !
!PropertyListTest categoriesFor: #testNestedArray!public!test! !
!PropertyListTest categoriesFor: #testNestedDictionary!public!test! !
!PropertyListTest categoriesFor: #testNotAPlist!*-unreferenced selectors!public! !
!PropertyListTest categoriesFor: #testReal!public!test! !
!PropertyListTest categoriesFor: #testString!public!test! !
!PropertyListTest categoriesFor: #testWrongPlistVersion!*-unreferenced selectors!public! !

!PropertyListTest class methodsFor!

createAllPlistMethods
	"
		BinaryPropertyListTest createAllPlistMethods.
		XMLPropertyListTest createAllPlistMethods.
"

	self filesAndMethods 
		keysAndValuesDo: [:key :value | self createPlistMethod: value fromFile: key]!

createPlistMethod: methodName fromFile: aFilename 
	| fileStream stream |
	fileStream := FileStream 
				read: 'C:\Dokumente und Einstellungen\Udo Schneider\Desktop\plists\plists\' , aFilename
				text: false.
	stream := ReadWriteStream on: String new.
	stream
		nextPutAll: methodName;
		cr;
		tab;
		nextPut: $";
		cr;
		tab;
		tab;
		nextPutAll: 'self createPlistMethod: ';
		nextPut: $';
		nextPutAll: methodName;
		nextPut: $';
		nextPutAll: ' fromFile: ';
		nextPut: $';
		nextPutAll: aFilename;
		nextPut: $';
		cr;
		tab;
		nextPut: $";
		cr;
		tab;
		nextPut: $^;
		nextPutAll: '##('.
	fileStream contents base64StoreOn: stream.
	fileStream close.
	stream nextPutAll: ').'.
	^self class compile: stream contents!

isAbstract
	^self name = #TestCase or: [self name = #PropertyListTest]!

propertyListTypeConstant
self subclassResponsibility!

testArray
	| stream |
	stream := ReadWriteStream on: Array new.
	stream
		nextPut: self testString;
		nextPut: self testInteger;
		nextPut: self testReal;
		nextPut: true;
		nextPut: false;
		nextPut: self testTimeStamp;
		nextPut: self testData.
	^stream contents!

testData
	^#[16r01 16r23 16r45 16r67 16r89 16rAB 16rCD 16rEF]!

testDictionary
	^(Dictionary new)
		at: 'data' put: self testData;
		at: 'date' put: self testTimeStamp;
		at: 'false' put: false;
		at: 'true' put: true;
		at: 'number' put: self testInteger;
		at: 'real' put: self testReal;
		at: 'string' put: self testString;
		yourself!

testInteger
	^815!

testNestedArray
	| dic stream |
	dic := self testDictionary.
	dic at: 'array' put: self testArray.
	^self testArray copyWith: dic!

testNestedDictionary
	^(self testDictionary)
		at: 'array' put: (self testArray copyWith: self testDictionary);
		yourself!

testReal
	^3.141592!

testString
	^'The quick brown fox jumps over the lazy dog''s back'!

testStringWithDoubleQuotes
	^'The "quick brown" fox jumps over the lazy dog''s back'!

testStringWithSingleQuotes
	^'The ''quick brown'' fox jumps over the lazy dog''s back'!

testTimeStamp
	^TimeStamp date: (Date 
				newDay: 25
				monthIndex: 7
				year: 2005)
		time: (Time 
				fromUtcHours: 8
				minutes: 29
				seconds: 9
				milliseconds: 0)! !
!PropertyListTest class categoriesFor: #createAllPlistMethods!*-unreferenced selectors!public! !
!PropertyListTest class categoriesFor: #createPlistMethod:fromFile:!public! !
!PropertyListTest class categoriesFor: #isAbstract!public! !
!PropertyListTest class categoriesFor: #propertyListTypeConstant!public! !
!PropertyListTest class categoriesFor: #testArray!public! !
!PropertyListTest class categoriesFor: #testData!public! !
!PropertyListTest class categoriesFor: #testDictionary!public! !
!PropertyListTest class categoriesFor: #testInteger!public! !
!PropertyListTest class categoriesFor: #testNestedArray!public! !
!PropertyListTest class categoriesFor: #testNestedDictionary!public! !
!PropertyListTest class categoriesFor: #testReal!public! !
!PropertyListTest class categoriesFor: #testString!public! !
!PropertyListTest class categoriesFor: #testStringWithDoubleQuotes!public! !
!PropertyListTest class categoriesFor: #testStringWithSingleQuotes!public! !
!PropertyListTest class categoriesFor: #testTimeStamp!public! !

