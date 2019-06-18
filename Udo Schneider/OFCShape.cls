"Filed out from Dolphin Smalltalk 7"!

OFCChartElement subclass: #OFCShape
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCShape guid: (GUID fromString: '{5421b2b7-440a-4561-bfcc-ee1a8313c351}')!
OFCShape comment: ''!
!OFCShape categoriesForClass!Unclassified! !
!OFCShape methodsFor!

elementType
	^#shape!

fillColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

values: aCollectionOfPoints 
	self 
		propertiesAt: #values
		put: aCollectionOfPoints! !
!OFCShape categoriesFor: #elementType!constants!public! !
!OFCShape categoriesFor: #fillColor:!accessing!public! !
!OFCShape categoriesFor: #values:!accessing!public! !

