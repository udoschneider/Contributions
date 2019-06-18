"Filed out from Dolphin Smalltalk 7"!

RelativeFileLocator subclass: #SpecialFolderRelativeFileLocator
	instanceVariableNames: 'csidl'
	classVariableNames: 'Current'
	poolDictionaries: 'ShellCSIDLConstants'
	classInstanceVariableNames: ''!
SpecialFolderRelativeFileLocator guid: (GUID fromString: '{0d08efb6-61c6-4d37-a3f4-a3fd19766948}')!
SpecialFolderRelativeFileLocator comment: ''!
!SpecialFolderRelativeFileLocator categoriesForClass!Unclassified! !
!SpecialFolderRelativeFileLocator methodsFor!

basePath
^ShellFolderLibrary default pathFromCSIDL: csidl!

setCSIDL: anInteger
csidl := anInteger! !
!SpecialFolderRelativeFileLocator categoriesFor: #basePath!public! !
!SpecialFolderRelativeFileLocator categoriesFor: #setCSIDL:!private! !

!SpecialFolderRelativeFileLocator class methodsFor!

csidl: anInteger 
"See http://msdn2.microsoft.com/en-us/library/bb762494.aspx for a list of CSIDLs"
	^(self new)
		setCSIDL: anInteger;
		yourself!

desktop

^self csidl: CSIDL_DESKTOP!

favorites
	^self csidl: CSIDL_FAVORITES!

myDocuments
	^self csidl: CSIDL_PERSONAL!

myPictures
	^self csidl: CSIDL_MYPICTURES!

programFiles
	^self csidl: CSIDL_PROGRAM_FILES!

windows
	^self csidl: CSIDL_WINDOWS! !
!SpecialFolderRelativeFileLocator class categoriesFor: #csidl:!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #desktop!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #favorites!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #myDocuments!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #myPictures!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #programFiles!*-in class package!instance creation!public! !
!SpecialFolderRelativeFileLocator class categoriesFor: #windows!*-in class package!instance creation!public! !

