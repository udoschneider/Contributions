"Filed out from Dolphin Smalltalk 7"!

RelativeFileLocator subclass: #PackageFolderRelativeFileLocator
	instanceVariableNames: 'packageName folder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
PackageFolderRelativeFileLocator guid: (GUID fromString: '{2ff3f6a0-964d-42b5-b8ba-21759a24c93c}')!
PackageFolderRelativeFileLocator comment: ''!
!PackageFolderRelativeFileLocator categoriesForClass!Unclassified! !
!PackageFolderRelativeFileLocator methodsFor!

basePath
	^(self package ifNil: [super basePath] ifNotNil: [:pkg | pkg path]) , folder
	
	
!

package
	"Answer the <Package> who's folder is used as the base path, or nil if the package is not loaded."

	^Smalltalk at: #Package ifPresent: [:class | class manager packageNamed: packageName ifNone: []]!

packageName
	"Answer the <readableString> name of the Package who's folder is used as the base path."

	^packageName!

setFolder: aFolder
folder := aFolder!

setPackageName: aString 
	packageName := aString! !
!PackageFolderRelativeFileLocator categoriesFor: #basePath!accessing!public! !
!PackageFolderRelativeFileLocator categoriesFor: #package!accessing!public! !
!PackageFolderRelativeFileLocator categoriesFor: #packageName!accessing!public! !
!PackageFolderRelativeFileLocator categoriesFor: #setFolder:!private! !
!PackageFolderRelativeFileLocator categoriesFor: #setPackageName:!initializing!private! !

!PackageFolderRelativeFileLocator class methodsFor!

package: aPackage folder: aFolder
	"Answer a new instance of the receiver for locating files relative to the folder containing
	the specified <Package>. Should the package be unloaded subsequently, then the instance will
	behave as if it were an ,ImageRelativeFileLocator>."

	^self packageNamed: aPackage name folder: aFolder!

packageNamed: aString folder: aFolder 
	"Answer a new instance of the receiver for locating files relative to the folder containing
	the named package. If the package is not loaded, then the instance will behave as if it were
	an <ImageRelativeFileLocator>."

	^(super new)
		setPackageName: aString;
		setFolder: aFolder;
		yourself! !
!PackageFolderRelativeFileLocator class categoriesFor: #package:folder:!instance creation!public! !
!PackageFolderRelativeFileLocator class categoriesFor: #packageNamed:folder:!instance creation!public! !

