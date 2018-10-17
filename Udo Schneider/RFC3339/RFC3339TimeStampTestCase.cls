"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #RFC3339TimeStampTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
RFC3339TimeStampTestCase guid: (GUID fromString: '{3340ff13-c3b5-40a6-a66d-836625c4cada}')!
RFC3339TimeStampTestCase comment: ''!
!RFC3339TimeStampTestCase categoriesForClass!Unclassified! !
!RFC3339TimeStampTestCase methodsFor!

assertRfc3339String: aString equalsYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction 	self assert: (TimeStamp fromRfc3339String: aString) 
				= (TimeStamp date: (Date 
								newDay: day
								monthIndex: month
								year: year)
						time: ((Time 
								fromUtcHours: hour
								minutes: minute
								seconds: second
								millisecondsFraction: millisecondFraction) ))!

assertRfc3339String: aString equalsYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: offset 
	self assert: (TimeStamp fromRfc3339String: aString) 
				= (TimeStamp date: (Date 
								newDay: day
								monthIndex: month
								year: year)
						time: ((Time 
								fromUtcHours: hour
								minutes: minute
								seconds: second
								millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: offset)))!

assertRfc3339String: aString equalsYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: offset 
	self assert: (TimeStamp fromRfc3339String: aString) 
				= (TimeStamp date: (Date 
								newDay: day
								monthIndex: month
								year: year)
						time: ((Time 
								fromUtcHours: hour
								minutes: minute
								seconds: second
								millisecondsFraction: millisecondFraction) addTime: (Time fromString: offset)))!

assertYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction  equalsRfc3339String: string 
	self 
		assert: (TimeStamp date: (Date 
						newDay: day
						monthIndex: month
						year: year)
				time: ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) )) 
					rfc3339String = (TimeStamp fromRfc3339String: string) rfc3339String!

assertYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetAdd: offset equalsRfc3339String: string 
	self 
		assert: (TimeStamp date: (Date 
						newDay: day
						monthIndex: month
						year: year)
				time: ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) subtractTime: (Time fromString: offset))) 
					rfc3339String = (TimeStamp fromRfc3339String: string) rfc3339String!

assertYear: year month: month day: day hour: hour minute: minute second: second millisecondFraction: millisecondFraction offsetSubtract: offset equalsRfc3339String: string 
	self 
		assert: (TimeStamp date: (Date 
						newDay: day
						monthIndex: month
						year: year)
				time: ((Time 
						fromUtcHours: hour
						minutes: minute
						seconds: second
						millisecondsFraction: millisecondFraction) addTime: (Time fromString: offset))) 
					rfc3339String = (TimeStamp fromRfc3339String: string) rfc3339String!

testFromRfc3339TimeStamp
	self
		assertRfc3339String: '1985-04-12T23:20:50.52Z'
			equalsYear: 1985
			month: 4
			day: 12
			hour: 23
			minute: 20
			second: 50
			millisecondFraction: 52;
		assertRfc3339String: '1996-12-19T16:39:57-08:00'
			equalsYear: 1996
			month: 12
			day: 19
			hour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00';
		assertRfc3339String: '1937-01-01T12:00:27.87+00:20'
			equalsYear: 1937
			month: 1
			day: 1
			hour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'!

testToRfc3339TimeStamp
	self
		assertYear: 1985
			month: 4
			day: 12
			hour: 23
			minute: 20
			second: 50
			millisecondFraction: 52
			equalsRfc3339String: '1985-04-12T23:20:50.52Z';
		assertYear: 1996
			month: 12
			day: 19
			hour: 16
			minute: 39
			second: 57
			millisecondFraction: 0
			offsetSubtract: '08:00'
			equalsRfc3339String: '1996-12-19T16:39:57-08:00';
		assertYear: 1937
			month: 1
			day: 1
			hour: 12
			minute: 0
			second: 27
			millisecondFraction: 87
			offsetAdd: '00:20'
			equalsRfc3339String: '1937-01-01T12:00:27.87+00:20'! !
!RFC3339TimeStampTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:hour:minute:second:millisecondFraction:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:hour:minute:second:millisecondFraction:offsetAdd:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertRfc3339String:equalsYear:month:day:hour:minute:second:millisecondFraction:offsetSubtract:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertYear:month:day:hour:minute:second:millisecondFraction:equalsRfc3339String:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertYear:month:day:hour:minute:second:millisecondFraction:offsetAdd:equalsRfc3339String:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #assertYear:month:day:hour:minute:second:millisecondFraction:offsetSubtract:equalsRfc3339String:!helpers!private! !
!RFC3339TimeStampTestCase categoriesFor: #testFromRfc3339TimeStamp!*-unreferenced selectors!public!test! !
!RFC3339TimeStampTestCase categoriesFor: #testToRfc3339TimeStamp!*-unreferenced selectors!public!test! !

