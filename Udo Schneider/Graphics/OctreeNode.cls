"Filed out from Dolphin Smalltalk 7"!

Object subclass: #OctreeNode
	instanceVariableNames: 'leaf red green blue pixelCount nextReducible children paletteIndex'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OctreeNode guid: (GUID fromString: '{e5d0ee7a-599b-4236-9736-98046249bb30}')!
OctreeNode comment: ''!
!OctreeNode categoriesForClass!Unclassified! !
!OctreeNode methodsFor!

addInteger: anInteger colorBits: colorBits level: level octree: octree 
	| index child |
	leaf 
		ifTrue: 
			[self increment: anInteger.
			octree trackPrevious: self]
		ifFalse: 
			[| shifted |
			shifted := anInteger bitShift: level - 8.
			index := ((shifted bitAnd: 16r010000) bitShift: -14) + ((shifted bitAnd: 16r0100) bitShift: -7) 
						+ (shifted bitAnd: 16r1).
			child := self childrenAt: index.
			child := child 
				ifNil: 
					[
					self childrenAt: index put: (OctreeNode 
								level: level + 1
								colorBits: colorBits
								octree: octree)].
			child 
				addInteger: anInteger
				colorBits: colorBits
				level: level + 1
				octree: octree]!

blue
	^blue!

children 
^children!

children: anArray 
	children := anArray!

childrenAt: anInteger 
	^children at: anInteger +1!

childrenAt: anInteger put: anOctreeNode 
	^children at: anInteger +1 put: anOctreeNode!

childrenDo: aMonadicValuable 
	children do: aMonadicValuable!

constructPalette: palette paletteIndex: anInteger 
	leaf 
		ifTrue: 
			[paletteIndex := anInteger + 1.
			palette add: (Color 
						red: red // pixelCount
						green: green // pixelCount
						blue: blue // pixelCount)]
		ifFalse: 
			[self childrenDo: [:eachChild | eachChild ifNotNil: [:value | value constructPalette: palette paletteIndex: anInteger]]]. ^palette!

green
	^green!

increment: anInteger 
	pixelCount := pixelCount + 1.
	red := red + ((anInteger bitAnd: 16rFF0000) >> 16).
	green := green + ((anInteger bitAnd: 16rFF00) >> 8).
	blue := blue + (anInteger bitAnd: 16rFF)!

maskAt: level
	^#(16r80 16r40 16r20 16r10 16r8 16r4 16r2 16r1) at: level +1!

nextReducible
	^nextReducible!

nextReducible: anObject
	nextReducible := anObject!

pixelCount
	^pixelCount!

red
	^red!

reduce
	| childrenC |

	childrenC := 0.
	red := 0.
	green := 0.
	blue := 0.
	self children do: 
			[:eachChild | 
			eachChild 
				ifNotNil: 
					[:value | 
					red := red + value red.
					green := green + value green.
					blue := blue + value blue.
					pixelCount := pixelCount + value pixelCount.
					childrenC := childrenC + 1]].
	self children: (Array new: 8).
	leaf := true.
	^childrenC - 1!

setLevel: level colorBits: colorBits octree: octree 
	leaf := level = colorBits.
	red := 0.
	green := 0.
	blue := 0.
	pixelCount := 0.
	leaf 
		ifTrue: 
			[octree leaves: octree leaves + 1.
			nextReducible := nil.
			children := nil]
		ifFalse: 
			[nextReducible := octree reducibleNodesAt: level.
			octree reducibleNodesAt: level put: self.
			children := Array new: 8]! !
!OctreeNode categoriesFor: #addInteger:colorBits:level:octree:!public! !
!OctreeNode categoriesFor: #blue!public! !
!OctreeNode categoriesFor: #children!public! !
!OctreeNode categoriesFor: #children:!public! !
!OctreeNode categoriesFor: #childrenAt:!public! !
!OctreeNode categoriesFor: #childrenAt:put:!public! !
!OctreeNode categoriesFor: #childrenDo:!public! !
!OctreeNode categoriesFor: #constructPalette:paletteIndex:!public! !
!OctreeNode categoriesFor: #green!public! !
!OctreeNode categoriesFor: #increment:!public! !
!OctreeNode categoriesFor: #maskAt:!public! !
!OctreeNode categoriesFor: #nextReducible!public! !
!OctreeNode categoriesFor: #nextReducible:!public! !
!OctreeNode categoriesFor: #pixelCount!public! !
!OctreeNode categoriesFor: #red!public! !
!OctreeNode categoriesFor: #reduce!public! !
!OctreeNode categoriesFor: #setLevel:colorBits:octree:!public! !

!OctreeNode class methodsFor!

level: level
				colorBits: colorBits
				octree: octree
				^self new setLevel: level
				colorBits: colorBits
				octree: octree! !
!OctreeNode class categoriesFor: #level:colorBits:octree:!public! !

