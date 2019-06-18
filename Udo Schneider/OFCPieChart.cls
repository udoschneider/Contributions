"Filed out from Dolphin Smalltalk 7"!

OFCChart subclass: #OFCPieChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCPieChart guid: (GUID fromString: '{a2de5566-c2c4-44ec-a693-89cd52863b47}')!
OFCPieChart comment: ''!
!OFCPieChart categoriesForClass!Unclassified! !
!OFCPieChart methodsFor!

animate: aBoolean 
	self 
		propertiesAt: #animate
		put: aBoolean!

elementType
	^ #pie!

fillColors: anArrayOfColorsOrStrings 
	self propertiesAt: #'colours' put: anArrayOfColorsOrStrings!

gradientFill: aBoolean 
	self 
		propertiesAt: #'gradient-fill'
		put: aBoolean!

hideLabels
	self showLabels: false!

showLabels
	self showLabels: true!

showLabels: aBoolean 
	self 
		propertiesAt: #'no-labels'
		put: aBoolean not!

startAngle: aNumber 
	self 
		propertiesAt: #'start-angle'
		put: aNumber! !
!OFCPieChart categoriesFor: #animate:!accessing!public! !
!OFCPieChart categoriesFor: #elementType!constants!public! !
!OFCPieChart categoriesFor: #fillColors:!accessing!public! !
!OFCPieChart categoriesFor: #gradientFill:!accessing!public! !
!OFCPieChart categoriesFor: #hideLabels!accessing!public! !
!OFCPieChart categoriesFor: #showLabels!accessing!public! !
!OFCPieChart categoriesFor: #showLabels:!accessing!public! !
!OFCPieChart categoriesFor: #startAngle:!accessing!public! !

!OFCPieChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCPieChart.ico'! !
!OFCPieChart class categoriesFor: #icon!public! !

