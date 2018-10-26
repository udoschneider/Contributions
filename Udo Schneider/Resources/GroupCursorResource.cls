"Filed out from Dolphin Smalltalk 7"!

GroupIconResource subclass: #GroupCursorResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GroupCursorResource guid: (GUID fromString: '{c0647204-4a42-4a46-b26a-ae6a461294f0}')!
GroupCursorResource comment: ''!
!GroupCursorResource categoriesForClass!Unclassified! !
!GroupCursorResource methodsFor!

newheaderEntryType
	^RES_CURSOR! !
!GroupCursorResource categoriesFor: #newheaderEntryType!*-in class package!private! !

!GroupCursorResource class methodsFor!

fixedEntry: aFILERESDIR withFileBytes: aByteArray 
^aFILERESDIR fixedUsing: aByteArray!

fixedFileBytes: aByteArray withEntry: aFILERESDIR 
	| fixedBytes |
	
	fixedBytes := #[0 0 0 0] , aByteArray.
	fixedBytes
		wordAtOffset: 0 put: aFILERESDIR xHotSpot;
		wordAtOffset: 2 put: aFILERESDIR yHotSpot.
	^fixedBytes!

fromResourceFileBytes: bytes in: aFilename 
	| header resource |
	header := NEWHEADER fromAddress: bytes yourAddress.
	resource := self id: (File splitFilenameFrom: aFilename) locale: nil.
	header fileresdirEntries do: 
			[:entry | 
			| resourceBytes fixedEntry fixedResourceBytes |
			resourceBytes := header resEntryBytes: entry.
			fixedResourceBytes := self fixedFileBytes: resourceBytes withEntry: entry.
			fixedEntry := self fixedEntry: entry withFileBytes: resourceBytes.
			resource addEntry: fixedEntry
				image: (self resourceEntryClass fromResourceFileBytes: fixedResourceBytes in: aFilename)].
	^resource!

resourceEntryClass
	^CursorResource!

resourceType
	^RT_GROUP_CURSOR! !
!GroupCursorResource class categoriesFor: #fixedEntry:withFileBytes:!private! !
!GroupCursorResource class categoriesFor: #fixedFileBytes:withEntry:!*-in class package!private! !
!GroupCursorResource class categoriesFor: #fromResourceFileBytes:in:!*-in class package!private! !
!GroupCursorResource class categoriesFor: #resourceEntryClass!*-in class package!constants!private! !
!GroupCursorResource class categoriesFor: #resourceType!*-in class package!private! !

