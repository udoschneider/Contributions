"Filed out from Dolphin Smalltalk 7"!

OFCNamedChart subclass: #OFCAreaChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCAreaChart guid: (GUID fromString: '{f3b2258a-8c22-4fd8-8193-f63119bfd941}')!
OFCAreaChart comment: ''!
!OFCAreaChart categoriesForClass!Unclassified! !
!OFCAreaChart methodsFor!

closeLoop: aBoolean
	"Should only be used for Radar Charts (Radar Axis)"
	self propertiesAt: #'loop' put: aBoolean!

dotHaloSize: aNumber 
	self 
		propertiesAt: #'halo-size'
		put: aNumber!

dotSize: aNumber 
	self 
		propertiesAt: #'dot-size'
		put: aNumber!

fillAlpha: aNumber 
	self 
		propertiesAt: #'fill-alpha'
		put: aNumber!

fillColor: aColorOrHTMLHexString 
	self 
		propertiesAt: #fill
		put: aColorOrHTMLHexString!

lineColor: aColor
	self propertiesAt: #'colour' put:  (aColor)!

lineWidth: anInteger 
	self propertiesAt: #width put: anInteger! !
!OFCAreaChart categoriesFor: #closeLoop:!accessing!public! !
!OFCAreaChart categoriesFor: #dotHaloSize:!accessing!public! !
!OFCAreaChart categoriesFor: #dotSize:!accessing!public! !
!OFCAreaChart categoriesFor: #fillAlpha:!accessing!public! !
!OFCAreaChart categoriesFor: #fillColor:!accessing!public! !
!OFCAreaChart categoriesFor: #lineColor:!accessing!public! !
!OFCAreaChart categoriesFor: #lineWidth:!accessing!public! !

!OFCAreaChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCAreaChart.ico'! !
!OFCAreaChart class categoriesFor: #icon!public! !

