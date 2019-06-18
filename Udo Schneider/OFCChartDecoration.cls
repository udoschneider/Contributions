"Filed out from Dolphin Smalltalk 7"!

OFCBrush subclass: #OFCChartDecoration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartDecoration guid: (GUID fromString: '{c281dffd-2a55-4405-865f-67f66e43860b}')!
OFCChartDecoration comment: ''!
!OFCChartDecoration categoriesForClass!Unclassified! !
!OFCChartDecoration methodsFor!

elementType
	self subclassResponsibility!

properties
	properties isNil ifTrue: [ properties := ofcCanvas dictionaryAt: self elementType ].
	^ properties! !
!OFCChartDecoration categoriesFor: #elementType!constants!public! !
!OFCChartDecoration categoriesFor: #properties!accessing!public! !

