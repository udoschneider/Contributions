"Filed out from Dolphin Smalltalk 7"!

OFCChartDecoration subclass: #OFCTooltipStyle
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCTooltipStyle guid: (GUID fromString: '{d54cdcf1-de19-4674-9a37-a149eaba1de4}')!
OFCTooltipStyle comment: ''!
!OFCTooltipStyle categoriesForClass!Unclassified! !
!OFCTooltipStyle methodsFor!

backgroundColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #background
		put: aColorOrHTMLHexString!

bodyStyle: aCssString 
	self 
		propertiesAt: #body
		put: aCssString!

disabled
	self propertiesAt: #mouse put: 4!

elementType
	^ #tooltip!

hoover
	self propertiesAt: #mouse put: 2!

lineColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

lineWidth: anInteger 
	self 
		propertiesAt: #stroke
		put: anInteger!

proximity
	self propertiesAt: #mouse put: 1!

shadow: aBoolean
	self propertiesAt: #shadow put: aBoolean !

titleStyle: aByteString 
	self propertiesAt: #title put: aByteString! !
!OFCTooltipStyle categoriesFor: #backgroundColor:!accessing!public! !
!OFCTooltipStyle categoriesFor: #bodyStyle:!accessing!public! !
!OFCTooltipStyle categoriesFor: #disabled!accessing!public! !
!OFCTooltipStyle categoriesFor: #elementType!constants!public! !
!OFCTooltipStyle categoriesFor: #hoover!accessing!public! !
!OFCTooltipStyle categoriesFor: #lineColor:!accessing!public! !
!OFCTooltipStyle categoriesFor: #lineWidth:!accessing!public! !
!OFCTooltipStyle categoriesFor: #proximity!accessing!public! !
!OFCTooltipStyle categoriesFor: #shadow:!accessing!public! !
!OFCTooltipStyle categoriesFor: #titleStyle:!accessing!public! !

!OFCTooltipStyle class methodsFor!

icon
^Tooltip icon! !
!OFCTooltipStyle class categoriesFor: #icon!public! !

