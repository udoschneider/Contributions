"Filed out from Dolphin Smalltalk 7"!

OFCNamedChart subclass: #OFCLineChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCLineChart guid: (GUID fromString: '{8fb6bfec-e526-4c8b-afe3-7ffbf7275c97}')!
OFCLineChart comment: ''!
!OFCLineChart categoriesForClass!Unclassified! !
!OFCLineChart methodsFor!

closeLoop: aBoolean 
	"Should only be used for Radar Charts (Radar Axis)"
	self 
		propertiesAt: #loop
		put: aBoolean!

elementType
	^ #line!

lineColor: aColor
	self propertiesAt: #'colour' put:  (aColor)!

lineWidth: anInteger 
	self propertiesAt: #width put: anInteger! !
!OFCLineChart categoriesFor: #closeLoop:!accessing!public! !
!OFCLineChart categoriesFor: #elementType!constants!public! !
!OFCLineChart categoriesFor: #lineColor:!accessing!public! !
!OFCLineChart categoriesFor: #lineWidth:!accessing!public! !

!OFCLineChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCLineChart.ico'! !
!OFCLineChart class categoriesFor: #icon!public! !

