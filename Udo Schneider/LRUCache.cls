"Filed out from Dolphin Smalltalk 7"!

Object subclass: #LRUCache
	instanceVariableNames: 'maxSize entries'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LRUCache guid: (GUID fromString: '{8515b7a2-8df1-4b90-9cfc-09f81b093c6b}')!
LRUCache comment: ''!
!LRUCache categoriesForClass!Kernel-Objects! !
!LRUCache methodsFor!

add: anEntry 
	entries addFirst: anEntry.
	self
		truncateAndDeDupEntries;
		 entriesUpdated.
	^anEntry!

do: operation
	^entries do: operation!

entries
	^entries!

entriesUpdated
	self trigger: #entriesUpdated!

initialize
	entries := OrderedCollection new.
	^super initialize!

maxSize
	^maxSize!

maxSize: anObject
	maxSize := anObject!

size
	^entries size!

truncateAndDeDupEntries
	entries := entries copyWithoutDuplicates.
	entries size > maxSize ifTrue: [entries := entries copyFrom: 1 to: maxSize].
	! !
!LRUCache categoriesFor: #add:!accessing!public! !
!LRUCache categoriesFor: #do:!accessing!public! !
!LRUCache categoriesFor: #entries!accessing!public! !
!LRUCache categoriesFor: #entriesUpdated!accessing!public! !
!LRUCache categoriesFor: #initialize!initialize/release!private! !
!LRUCache categoriesFor: #maxSize!accessing!public! !
!LRUCache categoriesFor: #maxSize:!accessing!private! !
!LRUCache categoriesFor: #size!accessing!private! !
!LRUCache categoriesFor: #truncateAndDeDupEntries!accessing!helpers!private! !

!LRUCache class methodsFor!

icon
	^Icon fromId: 'HISTORYBACK.ICO'!

new
	^self new: 4!

new: size
	^super new
		initialize;
		maxSize: size;
		yourself! !
!LRUCache class categoriesFor: #icon!public! !
!LRUCache class categoriesFor: #new!public! !
!LRUCache class categoriesFor: #new:!public! !

