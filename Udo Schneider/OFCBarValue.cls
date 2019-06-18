"Filed out from Dolphin Smalltalk 7"!

OFCChartValue subclass: #OFCBarValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCBarValue guid: (GUID fromString: '{5f586141-4b3a-461e-921e-a5c821ede1c3}')!
OFCBarValue comment: ''!
!OFCBarValue categoriesForClass!Unclassified! !
!OFCBarValue methodsFor!

bottom: aNumber
	self propertiesAt: #bottom put: aNumber !

left: aNumber
	self propertiesAt: #left put: aNumber !

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #'outline-colour'
		put: aColorOrHTMLHexString!

right: aNumber 
	self 
		propertiesAt: #right
		put: aNumber!

top: aNumber 
	self 
		propertiesAt: #top
		put: aNumber!

value: aNumber 
	self
		bottom: 0;
		top: aNumber;
		left: 0;
		right: aNumber! !
!OFCBarValue categoriesFor: #bottom:!accessing!public! !
!OFCBarValue categoriesFor: #left:!accessing!public! !
!OFCBarValue categoriesFor: #lineColor:!accessing!public! !
!OFCBarValue categoriesFor: #right:!accessing!public! !
!OFCBarValue categoriesFor: #top:!accessing!public! !
!OFCBarValue categoriesFor: #value:!accessing!public! !

