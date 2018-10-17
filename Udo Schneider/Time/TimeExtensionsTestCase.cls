"Filed out from Dolphin Smalltalk 7"!

TestCase subclass: #TimeExtensionsTestCase
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TimeExtensionsTestCase guid: (GUID fromString: '{8d6c42b2-9ab8-4de5-9ca2-31e160fa9573}')!
TimeExtensionsTestCase comment: ''!
!TimeExtensionsTestCase categoriesForClass!Unclassified! !
!TimeExtensionsTestCase methodsFor!

testMillisecondsFraction
	self 
		assert: ((Time 
				fromHours: 0
				minutes: 0
				seconds: 0
				milliseconds: 500) addTime: (Time 
							fromHours: 0
							minutes: 0
							seconds: 0
							milliseconds: 500)) 
				= (Time 
						fromHours: 0
						minutes: 0
						seconds: 1
						milliseconds: 0).
	self 
		assert: ((Time 
				fromHours: 0
				minutes: 0
				seconds: 0
				millisecondsFraction: 5) addTime: (Time 
							fromHours: 0
							minutes: 0
							seconds: 0
							millisecondsFraction: 5)) 
				= (Time 
						fromHours: 0
						minutes: 0
						seconds: 1
						milliseconds: 0)! !
!TimeExtensionsTestCase categoriesFor: #testMillisecondsFraction!*-unreferenced selectors!public! !

