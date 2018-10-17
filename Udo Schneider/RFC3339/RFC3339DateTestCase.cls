"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #RFC3339DateTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RFC3339DateTestCase guid: (GUID fromString: '{32d302d9-2eb8-4067-b0bc-5e9bfad1a8b7}')!
RFC3339DateTestCase comment: ''!
!RFC3339DateTestCase categoriesForClass!Unclassified! !
!RFC3339DateTestCase methodsFor!

assertRfc3339String: aString equalsYear: year month: month day: day 
	self 
		assert: (Date fromRfc3339String: aString) = (Date 
						newDay: day
						monthIndex: month
						year: year)!

assertYear: year month: month day: day equalsRfc3339String: aString 
	self 
		assert: (Date 
				newDay: day
				monthIndex: month
				year: year) rfc3339String 
				= aString!

testFromRfc3339Date
	self
		assertRfc3339String: '1985-04-12'
			equalsYear: 1985
			month: 4
			day: 12;
		assertRfc3339String: '1996-12-19'
			equalsYear: 1996
			month: 12
			day: 19;
		assertRfc3339String: '1990-12-31'
			equalsYear: 1990
			month: 12
			day: 31;
		assertRfc3339String: '1937-01-01'
			equalsYear: 1937
			month: 1
			day: 1!

testToRfc3339Date
	self
		assertYear: 1985
			month: 4
			day: 12
			equalsRfc3339String: '1985-04-12';
		assertYear: 1996
			month: 12
			day: 19
			equalsRfc3339String: '1996-12-19';
		assertYear: 1990
			month: 12
			day: 31
			equalsRfc3339String: '1990-12-31';
		assertYear: 1937
			month: 1
			day: 1
			equalsRfc3339String: '1937-01-01'! !
!RFC3339DateTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:!helpers!private! !
!RFC3339DateTestCase categoriesFor: #assertYear:month:day:equalsRfc3339String:!helpers!private! !
!RFC3339DateTestCase categoriesFor: #testFromRfc3339Date!*-unreferenced selectors!public!test! !
!RFC3339DateTestCase categoriesFor: #testToRfc3339Date!*-unreferenced selectors!public!test! !

