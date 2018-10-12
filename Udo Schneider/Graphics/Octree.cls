"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Octree
	instanceVariableNames: 'leafCount reducibleNodes previousColor previousNode maxColorBits root palette paletteTable'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Octree guid: (GUID fromString: '{d52d0007-93cb-44bd-b727-ab7ca366a93f}')!
Octree comment: ''!
!Octree categoriesForClass!Unclassified! !
!Octree methodsFor!

addInteger: anInteger 
	previousColor = anInteger 
		ifTrue: 
			[previousNode isNil 
				ifTrue: 
					[previousColor := anInteger.
					root 
						addInteger: anInteger
						colorBits: maxColorBits
						level: 0
						octree: self]
				ifFalse: [previousNode increment: anInteger]]
		ifFalse: 
			[previousColor := anInteger.
			root 
				addInteger: anInteger
				colorBits: maxColorBits
				level: 0
				octree: self]!

initialize
	leafCount := 0.
	reducibleNodes := Array new: 9.
	previousColor := 0.
	previousNode := nil!

leaves
^leafCount!

leaves: anInteger
leafCount := anInteger!

palletize: colorCount 
	[self leaves > colorCount] whileTrue: [self reduce].
	
	^root constructPalette: OrderedCollection new paletteIndex: 0!

reduce
	| index node |
	index := maxColorBits - 1.
	#USToDo.	"Is thie whileFalse or whileTrue"
	[index > 0 and: [(self reducibleNodesAt: index) notNil]] whileFalse: [index := index - 1].
	node := self reducibleNodesAt: index.
	self reducibleNodesAt: index put: node nextReducible.
	leafCount := leafCount - node reduce.
	previousNode := nil!

reducibleNodes
^reducibleNodes!

reducibleNodesAt: level
	^reducibleNodes at: level +1!

reducibleNodesAt: level put: octreeNode
	^reducibleNodes at: level + 1 put: octreeNode!

setBits: bits 
	maxColorBits := bits.
	root := OctreeNode 
				level: 0
				colorBits: maxColorBits
				octree: self!

trackPrevious: node
previousNode := node! !
!Octree categoriesFor: #addInteger:!public! !
!Octree categoriesFor: #initialize!public! !
!Octree categoriesFor: #leaves!public! !
!Octree categoriesFor: #leaves:!public! !
!Octree categoriesFor: #palletize:!public! !
!Octree categoriesFor: #reduce!public! !
!Octree categoriesFor: #reducibleNodes!public! !
!Octree categoriesFor: #reducibleNodesAt:!public! !
!Octree categoriesFor: #reducibleNodesAt:put:!public! !
!Octree categoriesFor: #setBits:!public! !
!Octree categoriesFor: #trackPrevious:!public! !

!Octree class methodsFor!

bits: bits
^self new setBits: bits
!

new
^super new initialize! !
!Octree class categoriesFor: #bits:!public! !
!Octree class categoriesFor: #new!public! !

