"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #RFC3339TimeTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RFC3339TimeTestCase guid: (GUID fromString: '{4c25013e-2270-49df-8887-9d5e46e510c9}')!
RFC3339TimeTestCase comment: ''!
!RFC3339TimeTestCase categoriesForClass!Unclassified! !
!RFC3339TimeTestCase methodsFor!

assertHour: hour minute: minute second: second millisecondFraction: millisecondFraction equalsRfc3339String: aString 
	self 
		assert: (Time 
				fromUtcHours: hour
				minutes: minute
				seconds: second
				millisecondsFraction: millisecondFraction) rfc3339String 
				= (Time fromRfc3339String: aString) rfc3339String!

assertHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: anOffset equalsRfc3339String: aString 
	self 
		assert: ((Time 
				fromUtcHours: hour
				minutes: minute
				seconds: second
				millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: anOffset)) 
				rfc3339String = (Time fromRfc3339String: aString) rfc3339String!

assertHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: anOffset equalsRfc3339String: aString 
	self 
		assert: ((Time 
				fromUtcHours: hour
				minutes: minute
				seconds: second
				millisecondsFraction: millisecondFraction) addTime: (Time fromString: anOffset)) 
				rfc3339String = (Time fromRfc3339String: aString) rfc3339String!

assertRfc3339String: aString equalsHour: hour minute: minute second: second millisecondFraction: millisecondFraction 
	self 
		assert: (Time fromRfc3339String: aString) = (Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction)!

assertRfc3339String: aString equalsHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: offset 
	self assert: (Time fromRfc3339String: aString) 
				= ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: offset))!

assertRfc3339String: aString equalsHour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: offset 
	self assert: (Time fromRfc3339String: aString) 
				= ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) addTime: (Time fromString: offset))!

testFromRfc3339Time
	self
		assertRfc3339String: '23:20:50.52Z'
			equalsHour: 23
			minute: 20
			second: 50
			millisecondFraction: 52;
		assertRfc3339String: '16:39:57-08:00'
			equalsHour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00';
		assertRfc3339String: '12:00:27.87+00:20'
			equalsHour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'!

testToRfc3339Time
	self
		assertHour: 23
			minute: 20
			second: 50
			millisecondFraction: 52
			equalsRfc3339String: '23:20:50.52Z';
		assertHour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00'
			equalsRfc3339String: '16:39:57-08:00';
		assertHour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'
			equalsRfc3339String: '12:00:27.87+00:20'! !
!RFC3339TimeTestCase categoriesFor: #assertHour:minute:second:millisecondFraction:equalsRfc3339String:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertHour:minute:second:millisecondFraction:offsetAdd:equalsRfc3339String:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertHour:minute:second:millisecondFraction:offsetSubtract:equalsRfc3339String:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertRfc3339String:equalsHour:minute:second:millisecondFraction:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertRfc3339String:equalsHour:minute:second:millisecondFraction:offsetAdd:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #assertRfc3339String:equalsHour:minute:second:millisecondFraction:offsetSubtract:!helpers!private! !
!RFC3339TimeTestCase categoriesFor: #testFromRfc3339Time!*-unreferenced selectors!public!test! !
!RFC3339TimeTestCase categoriesFor: #testToRfc3339Time!*-unreferenced selectors!public!test! !

