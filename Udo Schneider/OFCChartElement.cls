"Filed out from Dolphin Smalltalk 7"!

OFCBrush subclass: #OFCChartElement
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartElement guid: (GUID fromString: '{a3e6de0e-6fb5-43c6-a3c7-67df26d4c39d}')!
OFCChartElement comment: ''!
!OFCChartElement categoriesForClass!Unclassified! !
!OFCChartElement methodsFor!

elementType
	self subclassResponsibility!

properties
	properties isNil ifTrue: 
		[ | elements |
		(ofcCanvas arrayAt: #elements) add: (properties := Dictionary new).
		properties 
			at: #type
			put: self elementType ].
	^ properties!

values: anArray
	self propertiesAt: #values put: anArray! !
!OFCChartElement categoriesFor: #elementType!constants!public! !
!OFCChartElement categoriesFor: #properties!accessing!public! !
!OFCChartElement categoriesFor: #values:!accessing!public! !

