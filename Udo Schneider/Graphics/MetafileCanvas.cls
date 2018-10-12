"Filed out from Dolphin Smalltalk 7"!

Canvas subclass: #MetafileCanvas
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
MetafileCanvas guid: (GUID fromString: '{1fba093d-24aa-4b20-997d-1af6506f151e}')!
MetafileCanvas comment: ''!
!MetafileCanvas categoriesForClass!Unclassified! !
!MetafileCanvas methodsFor!

basicFree
	"Private - Free the external resources associated with the receiver."

	super basicFree
.

	GDILibrary default closeMetaFile: self asParameter.! !
!MetafileCanvas categoriesFor: #basicFree!private!realizing/unrealizing! !

!MetafileCanvas class methodsFor!

onFile: aFilename

|hDC|
	^(hDC := GDILibrary default createMetaFile: aFilename) isNil ifFalse: [self withOwnedDC: hDC]
! !
!MetafileCanvas class categoriesFor: #onFile:!*-unreferenced selectors!public! !

