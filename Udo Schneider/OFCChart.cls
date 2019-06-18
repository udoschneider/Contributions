"Filed out from Dolphin Smalltalk 7"!

OFCChartElement subclass: #OFCChart
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCChart guid: (GUID fromString: '{b8d94817-1911-43d4-b72d-19b349c2fbfc}')!
OFCChart comment: ''!
!OFCChart categoriesForClass!Unclassified! !
!OFCChart methodsFor!

ofcOnClick: anURLorJsFunction 
	self 
		propertiesAt: #'on-click'
		put: anURLorJsFunction!

tooltip
	^ self 
		propertiesAt: #tip
		put: OFCTooltip new!

tooltip: aBlock 
	^ self tooltip
		with: aBlock;
		yourself!

url: anURLString 
	"A click on any of this charts datapoint calls this URL in a NEW WINDOW!!"
	self ofcOnClick: anURLString! !
!OFCChart categoriesFor: #ofcOnClick:!private! !
!OFCChart categoriesFor: #tooltip!accessing!public! !
!OFCChart categoriesFor: #tooltip:!accessing!public! !
!OFCChart categoriesFor: #url:!accessing!public! !

!OFCChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCCanvas.ico'! !
!OFCChart class categoriesFor: #icon!public! !

