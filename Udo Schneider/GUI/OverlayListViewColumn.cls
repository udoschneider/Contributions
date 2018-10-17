"Filed out from Dolphin Smalltalk 7"!

ListViewColumn subclass: #OverlayListViewColumn
	instanceVariableNames: 'getOverlayImageBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
OverlayListViewColumn guid: (GUID fromString: '{2cf01934-d4d0-494e-bf13-26638dbac664}')!
OverlayListViewColumn comment: ''!
!OverlayListViewColumn categoriesForClass!Unclassified! !
!OverlayListViewColumn methodsFor!

defaultGetImageBlock
	"Private - Answer a monadic valuable to use to find out the image index of an object 
	that will be placed in the receiver."

	^nil!

getOverlayBlock
	^getOverlayImageBlock!

getOverlayBlock: anObject 
	getOverlayImageBlock := anObject!

initialize
	"Private - Initialize the receiver"

	super initialize.
	getOverlayImageBlock := nil.
!

overlayFromRow: item 


	^getOverlayImageBlock isNil ifFalse: [getOverlayImageBlock value: (self contentFromRow: item)]! !
!OverlayListViewColumn categoriesFor: #defaultGetImageBlock!adapters!constants!private! !
!OverlayListViewColumn categoriesFor: #getOverlayBlock!accessing!public! !
!OverlayListViewColumn categoriesFor: #getOverlayBlock:!accessing!public! !
!OverlayListViewColumn categoriesFor: #initialize!initializing!private! !
!OverlayListViewColumn categoriesFor: #overlayFromRow:!private! !

!OverlayListViewColumn class methodsFor!

publishedAspectsOfInstances
	"Answer a <LookupTable> of the <Aspect>s published by instances of the receiver."

	^(super publishedAspectsOfInstances)
		add: (Aspect block: #getOverlayBlock);
		yourself! !
!OverlayListViewColumn class categoriesFor: #publishedAspectsOfInstances!constants!development!must strip!public! !

