"Filed out from Dolphin Smalltalk 7"!

OFCChartValue subclass: #OFCPieValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCPieValue guid: (GUID fromString: '{e7621d58-56ec-49b2-9fd9-c26afa6d72b0}')!
OFCPieValue comment: ''!
!OFCPieValue categoriesForClass!Unclassified! !
!OFCPieValue methodsFor!

label: aString 
	self 
		propertiesAt: #label
		put: aString!

labelColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'label-colour'
		put: aColorOrHTMLHexString!

labelSize: aNumber 
	self 
		propertiesAt: #'font-size'
		put: aNumber!

value: anInteger
	self propertiesAt: #value put: anInteger ! !
!OFCPieValue categoriesFor: #label:!accessing!public! !
!OFCPieValue categoriesFor: #labelColor:!accessing!public! !
!OFCPieValue categoriesFor: #labelSize:!accessing!public! !
!OFCPieValue categoriesFor: #value:!accessing!public! !

