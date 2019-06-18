"Filed out from Dolphin Smalltalk 7"!

LRUCache subclass: #LRURegistryCache
	instanceVariableNames: 'regKey'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
LRURegistryCache guid: (GUID fromString: '{db0db731-3f2a-400f-9fbd-4840320b11a4}')!
LRURegistryCache comment: ''!
!LRURegistryCache categoriesForClass!Kernel-Objects! !
!LRURegistryCache methodsFor!

entriesUpdated
	entries
		keysAndValuesDo: [:eachKey :eachValue | regKey valueAt: 'lru' , eachKey displayString put: eachValue].
	^super entriesUpdated!

regKey
	^regKey!

regKey: aRegKey
	regKey := aRegKey.
	self updateFromRegistry!

updateFromRegistry
	| keys |
	keys := regKey subValues keys select: [:each | each beginsWith: 'lru'].
	keys asSortedCollection do: [:each | self add: (regKey valueAt: each)].
	self truncateAndDeDupEntries! !
!LRURegistryCache categoriesFor: #entriesUpdated!accessing!public! !
!LRURegistryCache categoriesFor: #regKey!public! !
!LRURegistryCache categoriesFor: #regKey:!public! !
!LRURegistryCache categoriesFor: #updateFromRegistry!public! !

