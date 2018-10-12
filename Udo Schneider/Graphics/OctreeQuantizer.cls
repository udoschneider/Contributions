"Filed out from Dolphin Smalltalk 7"!

Object subclass: #OctreeQuantizer
	instanceVariableNames: 'octree maxColors'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OctreeQuantizer guid: (GUID fromString: '{80823c20-b759-4550-96d7-138ff7de7fd2}')!
OctreeQuantizer comment: ''!
!OctreeQuantizer categoriesForClass!Unclassified! !
!OctreeQuantizer methodsFor!

quantizeColor: aColor 
	self quantizeInteger: aColor asRGB asParameter!

quantizedPalette
	^(octree palletize: maxColors)  asSortedCollection: [ :x :y | x luminance <= y luminance ].!

quantizeImage: aDIBSection 
	aDIBSection pixelValueDo: [:anInteger | self quantizeInteger: anInteger].
	^self quantizedPalette!

quantizeInteger: anInteger 
	octree addInteger: anInteger!

setMaxColors: anInteger maxBits: maxBits 
	maxColors := anInteger.
	octree := Octree bits: maxBits! !
!OctreeQuantizer categoriesFor: #quantizeColor:!public! !
!OctreeQuantizer categoriesFor: #quantizedPalette!public! !
!OctreeQuantizer categoriesFor: #quantizeImage:!public! !
!OctreeQuantizer categoriesFor: #quantizeInteger:!public! !
!OctreeQuantizer categoriesFor: #setMaxColors:maxBits:!private! !

!OctreeQuantizer class methodsFor!

maxColors: maxColors maxBits: maxBits 
	^(self basicNew )
		setMaxColors: maxColors maxBits: maxBits;
		yourself!

new
^self maxColors: 256 maxBits: 8! !
!OctreeQuantizer class categoriesFor: #maxColors:maxBits:!public! !
!OctreeQuantizer class categoriesFor: #new!public! !

