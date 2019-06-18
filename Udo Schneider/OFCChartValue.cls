"Filed out from Dolphin Smalltalk 7"!

OFCObject subclass: #OFCChartValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChartValue guid: (GUID fromString: '{2d2db168-d81c-4721-ba2a-29488ff03dc0}')!
OFCChartValue comment: ''!
!OFCChartValue categoriesForClass!Unclassified! !
!OFCChartValue methodsFor!

color: aColorOrHTMLHexString 
	self 
		propertiesAt: #colour
		put: aColorOrHTMLHexString!

fillColor: aColorOrHTMLHexString 
	self color: aColorOrHTMLHexString!

jsonSaveOn: aStream 
	self properties jsonSaveOn: aStream!

properties
	properties isNil ifTrue: [properties := Dictionary new].
	^properties!

tooltip
	^ self 
		propertiesAt: #tip
		put: OFCTooltip new!

tooltipTitle: title body: body 
	self tooltip 
		title: title
		body: body!

value: anInteger
	self subclassResponsibility! !
!OFCChartValue categoriesFor: #color:!accessing!public! !
!OFCChartValue categoriesFor: #fillColor:!accessing!public! !
!OFCChartValue categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCChartValue categoriesFor: #properties!accessing!public! !
!OFCChartValue categoriesFor: #tooltip!accessing!public! !
!OFCChartValue categoriesFor: #tooltipTitle:body:!accessing!public! !
!OFCChartValue categoriesFor: #value:!accessing!public! !

!OFCChartValue class methodsFor!

icon ^ValueModel icon! !
!OFCChartValue class categoriesFor: #icon!public! !

