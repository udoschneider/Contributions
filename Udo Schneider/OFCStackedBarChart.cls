"Filed out from Dolphin Smalltalk 7"!

OFCNamedChart subclass: #OFCStackedBarChart
	instanceVariableNames: 'datasets'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OFCStackedBarChart guid: (GUID fromString: '{ae8bcb28-fda4-4eaa-afb8-8b567880426f}')!
OFCStackedBarChart comment: ''!
!OFCStackedBarChart categoriesForClass!Unclassified! !
!OFCStackedBarChart methodsFor!

addDataset
	self properties.
	^ self datasets add: OFCStackedBarDataset new!

datasets
	datasets isNil ifTrue: [ datasets := OrderedCollection new ].
	^ datasets!

elementType
	^#'bar_stack'!

jsonSaveOn: aStream 
	| values colors keys |
	values := OrderedCollection new.
	colors := OrderedCollection new.
	keys := OrderedCollection new.
	datasets do: 
			[:eachSet | 
			colors add: eachSet color.
			keys add: eachSet key].
	1 to: datasets first values size
		do: [:valueIndex | values add: (datasets collect: [:eachSet | eachSet values at: valueIndex])].
	(properties
		copy;
		at: #values put: values;
		at: #colours put: colors;
		at: #keys put: keys;
		yourself) jsonSaveOn: aStream!

properties
	properties isNil ifTrue: 
		[ | elements |
		(ofcCanvas arrayAt: #elements) add: self.
		properties := Dictionary new.
		properties 
			at: #type
			put: self elementType ].
	^ properties!

values: anArray 
	"Use #addDataset instead!!"
	self shouldNotImplement! !
!OFCStackedBarChart categoriesFor: #addDataset!accessing!public! !
!OFCStackedBarChart categoriesFor: #datasets!accessing!public! !
!OFCStackedBarChart categoriesFor: #elementType!constants!public! !
!OFCStackedBarChart categoriesFor: #jsonSaveOn:!JSON-writing!public! !
!OFCStackedBarChart categoriesFor: #properties!accessing!public! !
!OFCStackedBarChart categoriesFor: #values:!accessing!public! !

!OFCStackedBarChart class methodsFor!

icon
	^Icon fromFile: 'Udo Schneider\Goodies\Resources\OFCBarChart.ico'! !
!OFCStackedBarChart class categoriesFor: #icon!public! !

