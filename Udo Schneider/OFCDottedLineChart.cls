"Filed out from Dolphin Smalltalk 7"!

OFCLineChart subclass: #OFCDottedLineChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCDottedLineChart guid: (GUID fromString: '{42e5f745-d91a-4b40-b476-832a8981df5b}')!
OFCDottedLineChart comment: ''!
!OFCDottedLineChart categoriesForClass!Unclassified! !
!OFCDottedLineChart methodsFor!

dotHaloSize: aNumber 
	self 
		propertiesAt: #'halo-size'
		put: aNumber!

dotSize: aNumber 
	self 
		propertiesAt: #'dot-size'
		put: aNumber!

elementType
	^ #'line_dot'! !
!OFCDottedLineChart categoriesFor: #dotHaloSize:!accessing!public! !
!OFCDottedLineChart categoriesFor: #dotSize:!accessing!public! !
!OFCDottedLineChart categoriesFor: #elementType!constants!public! !

!OFCDottedLineChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCDottedLineChart.ico'! !
!OFCDottedLineChart class categoriesFor: #icon!public! !

