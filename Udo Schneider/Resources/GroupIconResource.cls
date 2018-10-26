"Filed out from Dolphin Smalltalk 7"!

AbstractWindowsResource subclass: #GroupIconResource
	instanceVariableNames: 'entries offset'
	classVariableNames: ''
	poolDictionaries: 'Win32Constants'
	classInstanceVariableNames: ''!
GroupIconResource guid: (GUID fromString: '{67d915dd-a91b-4af1-9201-e95b6536a222}')!
GroupIconResource comment: ''!
!GroupIconResource categoriesForClass!Unclassified! !
!GroupIconResource methodsFor!

addEntry: aRESDIR image: anIconOrCursorResource 
	entries add: aRESDIR -> anIconOrCursorResource!

entriesAndImagesDo: aBlock 
	
	
	(entries asSortedCollection: [ :x :y | x key <= y key]) do: [:each | aBlock value: each key value: each value]!

initialize
	entries := OrderedCollection new.
	^super initialize!

newheaderEntryType 
^RES_ICON
!

offset
	^offset!

offset: anInteger 
	offset := anInteger!

packedResourceBytes
	self shouldNotImplement!

updateEntryResources: hUpdate 
	| bytes  |
	bytes := (NEWHEADER new)
				ResType: self newheaderEntryType;
				ResCount: entries size;
				bytes.
	self entriesAndImagesDo: 
			[:entry :icon | 
			icon
				identifier: offset;
				updateResource: hUpdate.
			bytes := bytes , ((entry asBINRESDIR)
								IconCursorId: offset;
								bytes).
			offset := offset + 1].
	^bytes!

updateGroupResourceIn: hUpdate withGroup: groupBytes 
	(KernelLibrary default 
		updateResource: hUpdate
		lpType: self class resourceType asParameter
		lpName: (self identifier isString ifTrue: [self identifier asUppercase] ifFalse: [self identifier]) asResourceId
		wLanguage: self locale asParameter
		lpData: groupBytes asParameter
		cbData: groupBytes size) ifFalse: [KernelLibrary default systemError]!

updateResource: hUpdate 
	| group |
	group := self updateEntryResources: hUpdate.
	self updateGroupResourceIn: hUpdate withGroup: group! !
!GroupIconResource categoriesFor: #addEntry:image:!public! !
!GroupIconResource categoriesFor: #entriesAndImagesDo:!public! !
!GroupIconResource categoriesFor: #initialize!public! !
!GroupIconResource categoriesFor: #newheaderEntryType!*-in class package!private! !
!GroupIconResource categoriesFor: #offset!accessing!public! !
!GroupIconResource categoriesFor: #offset:!accessing!public! !
!GroupIconResource categoriesFor: #packedResourceBytes!private! !
!GroupIconResource categoriesFor: #updateEntryResources:!*-in class package!private! !
!GroupIconResource categoriesFor: #updateGroupResourceIn:withGroup:!*-in class package!private! !
!GroupIconResource categoriesFor: #updateResource:!*-in class package!public! !

!GroupIconResource class methodsFor!

fromResourceBytes: bytes id: aResourceId locale: aLocale in: anExternalLibraryOrHandle 
	| header resource |
	header := NEWHEADER fromAddress: bytes yourAddress.
	resource := self id: aResourceId locale: aLocale.
	header binresdirEntries do: 
			[:entry | 
			resource addEntry: entry copy
				image: (self resourceEntryClass 
						fromId: entry IconCursorId
						locale: aLocale
						in: anExternalLibraryOrHandle)].
	^resource!

fromResourceFileBytes: bytes in: aFilename 
	| header resource |
	header := NEWHEADER fromAddress: bytes yourAddress.
	resource := self id: (File splitFilenameFrom: aFilename) locale: nil.
	header fileresdirEntries do: 
			[:entry | 
			| resourceBytes |
			resourceBytes := header resEntryBytes: entry.
			resource addEntry: entry copy
				image: (self resourceEntryClass fromResourceFileBytes: resourceBytes in: aFilename)].
	^resource!

resourceEntryClass 
^IconResource 
							!

resourceType
	^RT_GROUP_ICON! !
!GroupIconResource class categoriesFor: #fromResourceBytes:id:locale:in:!*-in class package!private! !
!GroupIconResource class categoriesFor: #fromResourceFileBytes:in:!*-in class package!private! !
!GroupIconResource class categoriesFor: #resourceEntryClass!*-in class package!constants!private! !
!GroupIconResource class categoriesFor: #resourceType!*-in class package!private! !

