"Filed out from Dolphin Smalltalk 7"!

OFCBarChart subclass: #OFCOutlinedBarChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCOutlinedBarChart guid: (GUID fromString: '{9a320b8e-5a83-4f7a-ac68-e79e8d5cd60c}')!
OFCOutlinedBarChart comment: ''!
!OFCOutlinedBarChart categoriesForClass!Unclassified! !
!OFCOutlinedBarChart methodsFor!

elementType
	^ #'bar_filled'!

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'outline-colour'
		put: aColorOrHTMLHexString! !
!OFCOutlinedBarChart categoriesFor: #elementType!constants!public! !
!OFCOutlinedBarChart categoriesFor: #lineColor:!accessing!public! !

